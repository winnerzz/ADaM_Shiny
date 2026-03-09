# =============================================================================
# data_utils.R
# 功能：为 Shiny 应用提供数据读取与预处理的基础工具函数
# 依赖包：readr, dplyr, stringr, jsonlite
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(jsonlite)

# -----------------------------------------------------------------------------
# 辅助函数 1：strip_excel_apos()
# 说明：Excel 有时在日期字符串前自动加上单引号（'）以防止格式转换，
#       例如将 2023-01-15 存为 '2023-01-15。
#       本函数统一清除这个前置单引号，确保后续日期解析正常。
# 参数：x — 字符型向量
# 返回：清除前置单引号后的字符型向量
# -----------------------------------------------------------------------------
strip_excel_apos <- function(x) {
  ifelse(is.na(x), NA_character_, str_replace(x, "^'", ""))
}

# -----------------------------------------------------------------------------
# 辅助函数 2：dy_char()
# 说明：按 CDISC 标准计算研究日（Study Day）。
#       第1天 = 首次给药日（而非第0天），跨越参考日当天的 AE 算作第1天。
#       公式：Study Day = (日期 - 参考日) + 1
# 参数：d   — Date 类型，目标日期
#       ref — Date 类型，参考日期（通常为首次给药日 TRTSDT）
# 返回：字符型的研究日编号，任一参数为 NA 时返回 NA_character_
# -----------------------------------------------------------------------------
dy_char <- function(d, ref) {
  if (is.na(d) || is.na(ref)) return(NA_character_)
  as.character(as.integer(d - ref) + 1L)
}

# -----------------------------------------------------------------------------
# 主函数：load_sdtm_data()
# 说明：统一读取任意数量的 SDTM 域 CSV 文件，并执行标准化预处理：
#         1. 所有列以字符型读入（避免 readr 自动类型推断造成的日期/数值错误）
#         2. 自动识别并清除所有 *DTC 日期列中的 Excel 前置单引号
#         3. 返回命名列表，方便后续函数按名称取用
# 参数：
#   domain_paths — 命名字符向量，如 c(dm="/tmp/dm.csv", vs="/tmp/vs.csv")
#                  键名为 SDTM 域 ID（小写），值为对应 CSV 文件路径
# 返回：命名列表，键名与 domain_paths 一一对应
# -----------------------------------------------------------------------------
load_sdtm_data <- function(domain_paths) {

  # --- 内部辅助：清除所有 *DTC 日期列中的 Excel 前置单引号 ---
  clean_dtc_cols <- function(df) {
    dtc_cols <- names(df)[str_ends(names(df), "DTC")]
    if (length(dtc_cols) > 0) {
      message("[data_utils]   清除单引号的列: ", paste(dtc_cols, collapse = ", "))
      df <- df %>%
        mutate(across(all_of(dtc_cols), strip_excel_apos))
    }
    df
  }

  # --- 1. 逐域读取，全部以字符型读入 ---
  result <- lapply(names(domain_paths), function(sid) {
    path <- domain_paths[[sid]]
    message("[data_utils] 正在读取 ", toupper(sid), ": ", path)
    df <- read_csv(path, col_types = cols(.default = col_character()),
                   show_col_types = FALSE)

    # --- 2. 清除日期列前置单引号 ---
    df <- clean_dtc_cols(df)

    # --- 3. 非阻断性质量检查 ---
    if (!"USUBJID" %in% names(df)) {
      warning("[data_utils] 警告：", toupper(sid),
              " 中未找到 USUBJID 列，请检查文件格式！")
    }
    df
  })
  names(result) <- names(domain_paths)

  # --- 4. 输出读取摘要 ---
  summary_parts <- sapply(names(result),
    function(sid) paste0(toupper(sid), "=", nrow(result[[sid]]), "行"))
  message("[data_utils] 读取完成：", paste(summary_parts, collapse = ", "))

  result
}

# -----------------------------------------------------------------------------
# 辅助函数：infer_required_domains_from_spec()
# 说明：从已解析的 Spec 列表中提取被引用的 SDTM 域名称，
#       用于在调用 btn_generate 前自动推断需要哪些上传文件。
# 参数：specs_list — rv$specs 的命名列表，每项包含 $parsed$variables$source
# 返回：字符向量，SDTM_DOMAIN_REGISTRY 中已知的域 ID（小写）
# -----------------------------------------------------------------------------
infer_required_domains_from_spec <- function(specs_list) {
  all_sources <- unlist(lapply(specs_list, function(s) {
    if (is.null(s$parsed)) return(character(0))
    s$parsed$variables$source
  }))
  domains <- unique(na.omit(tolower(stringr::str_extract(all_sources, "^[A-Za-z]+"))))
  intersect(domains, names(SDTM_DOMAIN_REGISTRY))
}

# -----------------------------------------------------------------------------
# 主函数：load_spec_json()
# 说明：读取 ADaM 变量规格的 JSON 文件。
#       该 JSON 文件描述目标数据集（如 ADSL/ADAE）中每个变量的：
#         - 变量名、标签、类型（Char/Num）
#         - 来源域（Source）和派生逻辑（Derivation）
#       LLM 将使用这份规格作为生成 R 代码的"蓝图"。
#
# 预期 JSON 结构示例（单个变量条目）：
# {
#   "dataset": "ADSL",
#   "variables": [
#     {
#       "variable": "USUBJID",
#       "label":    "Unique Subject Identifier",
#       "type":     "Char",
#       "source":   "DM.USUBJID",
#       "derivation": "Direct mapping from DM"
#     },
#     {
#       "variable": "SAFFL",
#       "label":    "Safety Population Flag",
#       "type":     "Char",
#       "source":   "EX",
#       "derivation": "Y if subject has at least one EX record, else N"
#     }
#   ]
# }
#
# 参数：spec_path — JSON 规格文件路径，默认 "spec.json"
# 返回：解析后的 R 列表对象（由 jsonlite::fromJSON 生成）
# -----------------------------------------------------------------------------
load_spec_json <- function(spec_path = "spec.json") {

  # 检查文件是否存在
  if (!file.exists(spec_path)) {
    stop("[data_utils] 规格文件不存在：", spec_path,
         "\n  请确认路径正确，或先运行 generate_spec_template() 创建模板。")
  }

  message("[data_utils] 正在读取规格文件: ", spec_path)

  # 使用 jsonlite 解析 JSON，simplifyVector = FALSE 保持列表结构
  # （便于后续按变量名精确查找）
  spec <- fromJSON(spec_path, simplifyVector = TRUE)

  # 基础结构验证
  if (is.null(spec$dataset)) {
    warning("[data_utils] 规格文件中未找到 'dataset' 字段，请检查 JSON 结构。")
  }
  if (is.null(spec$variables)) {
    warning("[data_utils] 规格文件中未找到 'variables' 字段，LLM 将无变量定义可用。")
  } else {
    message("[data_utils] 规格加载完成：数据集=", spec$dataset,
            "，变量数=", nrow(spec$variables))
  }

  spec
}

# -----------------------------------------------------------------------------
# 辅助函数：summarize_sdtm()
# 说明：将读取的 SDTM 列表转换为适合传给 LLM 的"数据摘要"字符串。
#       包含：每个数据集的行数、列名列表，以及前3行的预览。
#       目的是在不超出 LLM Token 限制的前提下，提供足够的上下文。
# 参数：sdtm_list — load_sdtm_data() 返回的命名列表
# 返回：格式化的字符型摘要字符串
# -----------------------------------------------------------------------------
summarize_sdtm <- function(sdtm_list) {

  parts <- lapply(names(sdtm_list), function(domain) {
    df <- sdtm_list[[domain]]
    header <- paste0(
      "=== ", toupper(domain), " ===\n",
      "行数: ", nrow(df), "\n",
      "列名: ", paste(names(df), collapse = ", "), "\n",
      "前3行预览:\n"
    )
    # 将前3行转为简单的字符矩阵格式
    preview <- capture.output(print(as.data.frame(head(df, 3)), row.names = FALSE))
    paste0(header, paste(preview, collapse = "\n"))
  })

  paste(parts, collapse = "\n\n")
}

# -----------------------------------------------------------------------------
# 辅助函数：generate_spec_template()
# 说明：如果尚无 JSON 规格文件，此函数可根据目标数据集名称自动生成一个
#       包含常用 ADSL 变量的模板文件，供用户修改后使用。
# 参数：output_path — 输出路径，默认 "spec.json"
#       dataset     — 目标数据集名称，默认 "ADSL"
# 返回：无（将文件写入磁盘并输出提示）
# -----------------------------------------------------------------------------
generate_spec_template <- function(output_path = "spec.json",
                                   dataset = "ADSL") {

  # 内置 ADSL 常用变量模板
  template <- list(
    dataset = dataset,
    variables = data.frame(
      variable   = c("STUDYID", "USUBJID", "SUBJID",  "SITEID",
                     "TRT01P",  "TRT01PN", "TRT01A",  "TRT01AN",
                     "TRTSDT",  "TRTEDT",  "TRTEDY",
                     "SAFFL",   "ITTFL"),
      label      = c("Study Identifier", "Unique Subject Identifier",
                     "Subject Identifier", "Study Site Identifier",
                     "Planned Treatment for Period 01",
                     "Planned Treatment for Period 01 (N)",
                     "Actual Treatment for Period 01",
                     "Actual Treatment for Period 01 (N)",
                     "Date of First Exposure to Treatment",
                     "Date of Last Exposure to Treatment",
                     "Treatment Duration (Days)",
                     "Safety Population Flag",
                     "Intent-to-Treat Population Flag"),
      type       = c("Char","Char","Char","Char",
                     "Char","Num","Char","Num",
                     "Char","Char","Num",
                     "Char","Char"),
      source     = c("DM","DM","DM","DM",
                     "DM.ARM","DM.ARM","DM.ACTARM","DM.ACTARM",
                     "DM.RFXSTDTC","DM.RFXENDTC","Derived",
                     "EX","DM"),
      derivation = c(
        "Direct mapping",
        "Direct mapping",
        "Direct mapping",
        "Direct mapping",
        "Direct mapping from DM.ARM",
        "Numeric code: Placebo=0, Test Drug=1",
        "Direct mapping from DM.ACTARM",
        "Numeric code: Placebo=0, Test Drug=1",
        "Convert DM.RFXSTDTC to date",
        "Convert DM.RFXENDTC to date",
        "(TRTEDT - TRTSDT) + 1 per CDISC convention",
        "Y if subject has >=1 EX record, else N",
        "Y for all subjects in DM"
      ),
      stringsAsFactors = FALSE
    )
  )

  write(toJSON(template, pretty = TRUE, auto_unbox = TRUE), output_path)
  message("[data_utils] 规格模板已写入: ", output_path)
}
