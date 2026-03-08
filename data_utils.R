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
# 说明：统一读取 SDTM 源数据（DM、EX、AE 三个 CSV 文件）并执行标准化预处理：
#         1. 所有列以字符型读入（避免 readr 自动类型推断造成的日期/数值错误）
#         2. 自动识别并清除所有 *DTC 日期列中的 Excel 前置单引号
#         3. 返回命名列表，方便后续函数按名称取用
# 参数：
#   dm_path — DM（人口统计）CSV 文件路径，默认 "demo/dm.csv"
#   ex_path — EX（用药暴露）CSV 文件路径，默认 "demo/ex.csv"
#   ae_path — AE（不良事件）CSV 文件路径，默认 "demo/ae.csv"
# 返回：命名列表 list(dm = ..., ex = ..., ae = ...)
# -----------------------------------------------------------------------------
load_sdtm_data <- function(dm_path = "demo/dm.csv",
                           ex_path = "demo/ex.csv",
                           ae_path = "demo/ae.csv") {

  # --- 1. 读取三个 SDTM CSV 文件，全部以字符型读入 ---
  # 使用 col_types = cols(.default = col_character()) 是关键：
  # 避免 readr 将日期自动解析为 Date 对象，保持 YYYY-MM-DD 字符串格式
  message("[data_utils] 正在读取 DM: ", dm_path)
  dm <- read_csv(dm_path, col_types = cols(.default = col_character()),
                 show_col_types = FALSE)

  message("[data_utils] 正在读取 EX: ", ex_path)
  ex <- read_csv(ex_path, col_types = cols(.default = col_character()),
                 show_col_types = FALSE)

  message("[data_utils] 正在读取 AE: ", ae_path)
  ae <- read_csv(ae_path, col_types = cols(.default = col_character()),
                 show_col_types = FALSE)

  # --- 2. 清除所有 *DTC 日期列中的 Excel 前置单引号 ---
  # 遍历每个数据集的列名，对以 "DTC" 结尾的列应用 strip_excel_apos()
  # 例如：RFSTDTC、RFXSTDTC、AESTDTC、AEENDTC 等
  clean_dtc_cols <- function(df) {
    dtc_cols <- names(df)[str_ends(names(df), "DTC")]
    if (length(dtc_cols) > 0) {
      message("[data_utils]   清除单引号的列: ", paste(dtc_cols, collapse = ", "))
      df <- df %>%
        mutate(across(all_of(dtc_cols), strip_excel_apos))
    }
    df
  }

  dm <- clean_dtc_cols(dm)
  ex <- clean_dtc_cols(ex)
  ae <- clean_dtc_cols(ae)

  # --- 3. 基础数据质量检查（非阻断，仅输出警告信息）---
  # 检查关键字段 USUBJID 是否存在
  for (domain_name in c("dm", "ex", "ae")) {
    df <- get(domain_name)
    if (!"USUBJID" %in% names(df)) {
      warning("[data_utils] 警告：", toupper(domain_name),
              " 中未找到 USUBJID 列，请检查文件格式！")
    }
  }

  # 输出读取摘要
  message("[data_utils] 读取完成：",
          "DM=", nrow(dm), "行, ",
          "EX=", nrow(ex), "行, ",
          "AE=", nrow(ae), "行")

  # --- 4. 返回命名列表 ---
  list(dm = dm, ex = ex, ae = ae)
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
