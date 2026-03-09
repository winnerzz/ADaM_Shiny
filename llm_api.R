# =============================================================================
# llm_api.R
# ADaM 自动化生成平台 — LLM API 调用引擎
#
# 本次重构要点：
#   1. ProviderFactory：通过 provider_registry.R 统一注册 7 个提供商
#   2. 单一调用路径 .call_real_api()：消除 OpenAI/Anthropic 双分支
#   3. call_llm_engine_with_failover()：支持多提供商故障转移链
#   4. call_llm_engine() 保持原签名，兼容现有调用
#   5. 本地推理连接失败给出友好提示
#
# 依赖包：httr2, jsonlite, stringr
# =============================================================================

library(httr2)
library(jsonlite)
library(stringr)

source("provider_registry.R", local = TRUE)

# =============================================================================
# 全局配置
# MOCK_MODE = TRUE  → 返回硬编码响应（开发/演示用）
# MOCK_MODE = FALSE → 发起真实 HTTP 请求
# =============================================================================
MOCK_MODE <- FALSE   # ← 接入真实 API 时保持 FALSE

# =============================================================================
# 提供商推断：.infer_provider()
# 从模型名前缀推断对应提供商 key
# 特殊 sentinel 值："ollama:local" / "vllm:local"
# =============================================================================
.infer_provider <- function(model) {
  model_lc <- tolower(trimws(model))

  # 处理本地推理 sentinel 值
  if (str_detect(model_lc, "^(ollama|vllm):")) {
    return(str_extract(model_lc, "^[^:]+"))
  }

  # 按前缀匹配
  for (prefix in names(MODEL_PREFIX_MAP)) {
    if (str_starts(model_lc, prefix)) return(MODEL_PREFIX_MAP[[prefix]])
  }

  message("[llm_api] 无法推断提供商，回退 openai：", model)
  "openai"
}

# =============================================================================
# 核心函数：call_llm_engine_with_failover()
# 支持多提供商故障转移。依次尝试 failover_chain 中的提供商，
# 全部失败则抛出最后一个错误。
#
# 参数：
#   spec_json        — 字符型 JSON 或 R 列表，ADaM 变量规格
#   data_summary     — 字符型，SDTM 数据摘要
#   provider_key_map — 命名列表，providerKey → api_key 字符串
#   failover_chain   — 列表，每项 list(provider=, model=)，按优先级排列
#   sdtm_list        — 可选，list(dm=, ex=, ae=)，用于构建含前5行的精细 Prompt
#   base_url_map     — 命名列表，providerKey → 本地服务地址（本地推理使用）
#   mock             — 逻辑型，TRUE 强制 Mock 模式
#
# 返回：list(r_code = "...", risk_logs = list(...))
# =============================================================================
call_llm_engine_with_failover <- function(spec_json,
                                           data_summary,
                                           provider_key_map,
                                           failover_chain,
                                           sdtm_list       = NULL,
                                           base_url_map    = list(),
                                           mock            = MOCK_MODE,
                                           target_datasets = NULL) {
  if (is.list(spec_json)) {
    spec_json <- toJSON(spec_json, pretty = TRUE, auto_unbox = TRUE)
  }

  if (isTRUE(mock)) {
    message("[llm_api] Mock 模式 — 返回硬编码响应")
    return(.mock_llm_response(target_datasets = target_datasets %||% c("adsl", "adae")))
  }

  prompts    <- .build_prompts(spec_json, data_summary, sdtm_list, target_datasets)
  last_error <- NULL

  for (attempt in failover_chain) {
    prov  <- attempt$provider
    model <- attempt$model
    key   <- provider_key_map[[prov]] %||% ""
    url   <- base_url_map[[prov]]     %||% NULL

    message("[llm_api] 调用 ", prov, " · 模型=", model)

    result <- tryCatch(
      .call_real_api(prompts$system, prompts$user, key, model, prov, url),
      error = function(e) {
        last_error <<- conditionMessage(e)
        message("[llm_api] 提供商 '", prov, "' 失败，尝试下一个。原因：", last_error)
        NULL
      }
    )

    if (!is.null(result)) return(result)
  }

  stop("所有提供商均失败。最后错误：\n", last_error)
}

# =============================================================================
# 兼容性包装：call_llm_engine()
# 保持原签名，供现有 server.R 调用不受影响。
# =============================================================================
call_llm_engine <- function(spec_json,
                            data_summary,
                            api_key   = "",
                            model     = "gpt-4o",
                            sdtm_list = NULL,
                            mock      = MOCK_MODE) {
  if (is.list(spec_json)) {
    spec_json <- toJSON(spec_json, pretty = TRUE, auto_unbox = TRUE)
  }

  prov <- .infer_provider(model)

  call_llm_engine_with_failover(
    spec_json        = spec_json,
    data_summary     = data_summary,
    provider_key_map = setNames(list(api_key), prov),
    failover_chain   = list(list(provider = prov, model = model)),
    sdtm_list        = sdtm_list,
    mock             = mock
  )
}

# =============================================================================
# 统一 API 调用：.call_real_api()
# 从注册表读取 auth_scheme / json_mode / anthropic_style，
# 统一处理所有 7 个提供商，无平行分支。
# =============================================================================
.call_real_api <- function(system_prompt, user_prompt, api_key, model, provider,
                            base_url_override = NULL) {
  cfg      <- .get_provider_cfg(provider)
  base_url <- base_url_override %||% cfg$base_url %||% cfg[["base_url_default"]]

  if (is.null(base_url) || nchar(trimws(base_url)) == 0) {
    stop("提供商 '", provider, "' 未配置服务地址。",
         if (cfg$needs_url) "请在 API 配置面板填写本地服务地址。" else "")
  }

  if (cfg$needs_key && nchar(trimws(api_key)) == 0) {
    stop("API Key 为空（", cfg$name, "）。请在 LLM API 配置中输入有效的 Key。")
  }

  # ── 构建请求体 ──────────────────────────────────────────────────────────────
  body <- if (cfg$anthropic_style) {
    list(
      model      = model,
      max_tokens = 4096,
      system     = system_prompt,
      messages   = list(list(role = "user", content = user_prompt))
    )
  } else {
    b <- list(
      model       = model,
      temperature = 0.1,
      max_tokens  = 4096,
      messages    = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = user_prompt)
      )
    )
    if (cfg$json_mode) b$response_format <- list(type = "json_object")
    b
  }

  # ── 构建请求对象 ────────────────────────────────────────────────────────────
  req <- request(base_url) |>
    req_body_json(body) |>
    req_timeout(120) |>
    req_retry(
      max_tries    = 3,
      is_transient = \(r) resp_status(r) %in% c(429L, 500L, 502L, 503L)
    )

  req <- switch(cfg$auth_scheme,
    bearer = req_headers(req,
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    `x-api-key` = req_headers(req,
      `x-api-key`         = api_key,
      `anthropic-version` = "2023-06-01",
      `Content-Type`      = "application/json"
    ),
    none = req_headers(req,
      `Content-Type` = "application/json"
    ),
    stop("未知认证方案：", cfg$auth_scheme)
  )

  # ── 执行请求 ────────────────────────────────────────────────────────────────
  resp <- tryCatch(
    req_perform(req),
    error = function(e) .handle_http_error(e, provider)
  )

  # ── 提取原始文本 ─────────────────────────────────────────────────────────────
  raw_content <- if (cfg$anthropic_style) {
    resp_body_json(resp)$content[[1]]$text
  } else {
    resp_body_json(resp)$choices[[1]]$message$content
  }

  .parse_llm_json(raw_content, model)
}

# =============================================================================
# [L-1] Prompt 构建函数：.build_prompts()
# 新增 target_datasets 参数，动态生成数据集输出指令（向后兼容）
# =============================================================================
.build_prompts <- function(spec_json, data_summary, sdtm_list = NULL,
                            target_datasets = NULL) {

  # ── 推断目标数据集列表 ──────────────────────────────────────────────────────
  if (is.null(target_datasets) || length(target_datasets) == 0) {
    # 尝试从 spec_json 解析 dataset 字段
    parsed_spec <- tryCatch(
      fromJSON(spec_json, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.null(parsed_spec)) {
      if (is.data.frame(parsed_spec)) {
        target_datasets <- unique(tolower(na.omit(parsed_spec$dataset)))
      } else if (!is.null(parsed_spec$dataset)) {
        target_datasets <- tolower(as.character(parsed_spec$dataset))
      } else if (is.list(parsed_spec)) {
        target_datasets <- unique(tolower(na.omit(
          sapply(parsed_spec, function(s) s$dataset %||% NA_character_)
        )))
      }
    }
    if (is.null(target_datasets) || length(target_datasets) == 0) {
      target_datasets <- c("adsl", "adae")
    }
  }
  target_datasets <- tolower(trimws(target_datasets))

  # ── 构建数据集输出指令（动态）──────────────────────────────────────────────
  ds_instruction <- if (identical(sort(target_datasets), sort(c("adsl", "adae")))) {
    paste0(
      "2. 最终必须生成名为 adsl 的 data.frame（subject-level）",
      "和名为 adae 的 data.frame（adverse events），",
      "因为后续程序会用 get('adsl') 和 get('adae') 提取结果\n"
    )
  } else {
    ds_names_str <- paste(
      sapply(target_datasets, function(ds) paste0("'", ds, "'")),
      collapse = " 和 "
    )
    paste0(
      "2. 最终必须生成以下 data.frame：", ds_names_str,
      "，后续程序将用 get('数据集名') 逐一提取。\n"
    )
  }

  system_prompt <- paste0(
    "你是一位资深 CDISC ADaM 数据程序员，精通 R 语言（dplyr / lubridate / stringr）。\n",
    "你的任务是根据用户提供的 SDTM 源数据结构和 ADaM 变量规格，",
    "生成完整、可直接运行的 R 代码，将 SDTM 转换为 ADaM 数据集。\n\n",

    "【输出格式要求 - 严格执行】\n",
    "你必须且只能返回一个合法的 JSON 对象，包含以下两个字段：\n",
    "{\n",
    '  "r_code": "完整可运行的 R 代码字符串（换行用 \\n 转义）",\n',
    '  "risk_logs": [\n',
    '    {\n',
    '      "level": "WARNING 或 INFO 或 ERROR",\n',
    '      "variable": "涉及的 ADaM 变量名",\n',
    '      "description": "你做了什么推断或假设",\n',
    '      "assumption": "该假设的前提条件和需要人工确认的内容"\n',
    '    }\n',
    '  ]\n',
    "}\n\n",

    "【代码规范】\n",
    "1. 代码必须包含 library(dplyr)、library(lubridate) 等必要包加载语句\n",
    ds_instruction,
    "3. 日期变量使用字符型 YYYY-MM-DD 格式，",
    "Study Day 按 CDISC 规范：(date - ref_date) + 1\n",
    "4. 对所有自主推断（如缺失值处理、类型转换假设）必须在 risk_logs 中逐条记录\n",
    "5. 不要在 JSON 之外输出任何文字、注释或 Markdown 代码块标记"
  )

  part_spec <- paste0(
    "## 1. ADaM 变量规格（JSON 格式）\n",
    "以下是目标 ADaM 数据集的变量元数据，",
    "包含变量名、标签、类型、来源域和派生逻辑：\n\n",
    spec_json
  )

  part_preview <- ""
  if (!is.null(sdtm_list)) {
    domain_previews <- lapply(names(sdtm_list), function(domain) {
      df <- sdtm_list[[domain]]
      if (is.null(df) || nrow(df) == 0) return(NULL)
      preview_rows <- head(df, 5)
      csv_lines    <- c(
        paste(names(preview_rows), collapse = ","),
        apply(preview_rows, 1, function(r) {
          paste(ifelse(is.na(r), "", r), collapse = ",")
        })
      )
      paste0(
        "### ", toupper(domain), " 域（前", nrow(preview_rows), "行）\n",
        "列数：", ncol(df), "  总行数：", nrow(df), "\n",
        "```csv\n",
        paste(csv_lines, collapse = "\n"),
        "\n```"
      )
    })
    domain_previews <- Filter(Negate(is.null), domain_previews)
    if (length(domain_previews) > 0) {
      part_preview <- paste0(
        "\n\n## 2. SDTM 源数据结构（各域前5行）\n",
        "以下是上传的 SDTM 数据的实际列名和样本数据，",
        "请据此推断字段映射关系：\n\n",
        paste(domain_previews, collapse = "\n\n")
      )
    }
  }

  part_summary <- paste0(
    "\n\n## 3. SDTM 数据摘要\n",
    data_summary
  )

  list(system = system_prompt, user = paste0(part_spec, part_preview, part_summary))
}

# =============================================================================
# [保留不变] JSON 解析与验证：.parse_llm_json()
# =============================================================================
.parse_llm_json <- function(raw_content, model = "") {

  if (is.null(raw_content) || nchar(trimws(raw_content)) == 0) {
    stop("LLM 返回了空内容。请检查 API Key 权限或尝试更换模型。")
  }

  clean <- raw_content |>
    str_replace("^\\s*```json\\s*", "") |>
    str_replace("\\s*```\\s*$",     "") |>
    trimws()

  if (!str_starts(clean, "\\{")) {
    match <- str_extract(clean, "\\{[\\s\\S]+\\}")
    if (is.na(match)) {
      stop(
        "LLM 返回的内容不是合法 JSON。\n",
        "前100字符：", substr(clean, 1, 100), "\n",
        "建议：尝试切换到支持 JSON 模式的模型（如 gpt-4o）。"
      )
    }
    clean <- match
  }

  parsed <- tryCatch(
    fromJSON(clean, simplifyVector = FALSE),
    error = function(e) {
      stop(
        "JSON 解析失败：", conditionMessage(e), "\n",
        "原始内容前200字符：", substr(clean, 1, 200)
      )
    }
  )

  if (is.null(parsed$r_code)) {
    stop(
      "LLM 响应缺少 'r_code' 字段。\n",
      "收到的字段：", paste(names(parsed), collapse=", ")
    )
  }
  if (is.null(parsed$risk_logs)) {
    message("[llm_api] 警告：LLM 响应缺少 'risk_logs'，已补充为空列表")
    parsed$risk_logs <- list()
  }

  message("[llm_api] 解析成功  r_code=", nchar(parsed$r_code), "字符  ",
          "risk_logs=", length(parsed$risk_logs), "条")
  parsed
}

# =============================================================================
# HTTP 错误翻译：.handle_http_error()
# 新增本地推理连接失败分支
# =============================================================================
.handle_http_error <- function(e, provider = "") {

  msg <- conditionMessage(e)

  # 本地推理连接失败
  if (str_detect(msg, "ECONNREFUSED|Connection refused|Failed to connect|connect to")) {
    stop(
      "无法连接本地推理服务（", provider, "）。\n",
      "请确认：① 服务已启动  ② 端口正确  ③ 防火墙未阻断\n",
      "原始错误：", msg
    )
  }

  # 401 / 403：认证失败
  if (str_detect(msg, "401|403|Unauthorized|Forbidden|invalid_api_key")) {
    stop(
      "API Key 无效或权限不足（HTTP 401/403）。\n",
      "请检查：① Key 是否拼写正确  ② Key 是否有访问该模型的权限  ",
      "③ 账户余额是否充足"
    )
  }

  # 429：限流
  if (str_detect(msg, "429|rate.?limit|Too Many")) {
    stop(
      "请求频率超限（HTTP 429）。\n",
      "请稍等片刻后重试，或升级 API 账户的速率限制。"
    )
  }

  # 400：请求格式错误
  if (str_detect(msg, "400|Bad Request")) {
    stop(
      "请求参数错误（HTTP 400）。\n",
      "可能原因：所选模型不支持 JSON 强制输出模式（response_format）。\n",
      "建议切换为 gpt-4o 或 gpt-4-turbo。"
    )
  }

  # 超时
  if (str_detect(msg, "timeout|timed out|ETIMEDOUT")) {
    stop(
      "请求超时（>120秒）。\n",
      "可能原因：网络连接慢，或 Spec 过长导致 LLM 生成时间超限。\n",
      "建议：减少 Spec 变量数量，或检查网络连接。"
    )
  }

  stop("LLM API 请求失败（", provider, "）：", msg)
}

# =============================================================================
# [L-2] Mock 响应：.mock_llm_response()
# 新增 target_datasets 参数：adsl+adae 时返回原有硬编码响应；否则生成通用骨架
# =============================================================================
.mock_llm_response <- function(target_datasets = c("adsl", "adae")) {

  # 向后兼容：adsl + adae 时返回原有硬编码响应
  if (identical(sort(tolower(target_datasets)), sort(c("adsl", "adae")))) {

  r_code_str <- '
# ============================================================
# [LLM 生成] ADSL + ADAE 构建代码（Mock 响应）
# ⚠️ 请在执行前人工审阅下方 risk_logs 中的风险提示
# ============================================================
library(dplyr)
library(lubridate)

# ── ADSL ──────────────────────────────────────────────────────
trt_num_map <- c("Placebo" = "0", "Test Drug" = "1")

ex_summary <- ex |>
  group_by(USUBJID) |>
  summarise(
    NEX    = n(),
    TRTSDT = min(EXSTDTC, na.rm = TRUE),
    TRTEDT = max(EXENDTC, na.rm = TRUE),
    .groups = "drop"
  )

adsl <- dm |>
  left_join(ex_summary, by = "USUBJID") |>
  mutate(
    TRT01P  = ARM,
    TRT01A  = ACTARM,
    TRT01PN = unname(trt_num_map[TRT01P]),
    TRT01AN = unname(trt_num_map[TRT01A]),
    TRTSDT  = if_else(!is.na(TRTSDT), TRTSDT, RFXSTDTC),
    TRTEDT  = if_else(!is.na(TRTEDT), TRTEDT, RFXENDTC),
    TRTEDY  = if_else(
      !is.na(TRTEDT) & !is.na(TRTSDT),
      as.character(as.integer(ymd(TRTEDT) - ymd(TRTSDT)) + 1L),
      NA_character_
    ),
    NEX    = coalesce(NEX, 0L),
    SAFFL  = if_else(NEX > 0, "Y", "N"),
    ITTFL  = "Y"
  ) |>
  transmute(
    STUDYID, USUBJID, SUBJID, SITEID, COUNTRY,
    AGE, AGEU, SEX, RACE, ETHNIC,
    TRT01P, TRT01PN, TRT01A, TRT01AN,
    TRTSDT, TRTEDT, TRTEDY,
    DTHFL, SAFFL, ITTFL
  ) |>
  arrange(USUBJID)

# ── ADAE ──────────────────────────────────────────────────────
adae <- ae |>
  left_join(adsl |> select(USUBJID, TRT01P, TRT01PN, TRT01A, TRT01AN, TRTSDT),
            by = "USUBJID") |>
  mutate(
    ASTDT   = AESTDTC,
    AENDT   = AEENDTC,
    ASTDY   = if_else(!is.na(ASTDT) & !is.na(TRTSDT),
                      as.character(as.integer(ymd(ASTDT) - ymd(TRTSDT)) + 1L),
                      NA_character_),
    TRTEMFL = if_else(!is.na(ASTDT) & !is.na(TRTSDT) & ymd(ASTDT) >= ymd(TRTSDT),
                      "Y", "N"),
    RELGR1  = if_else(AEREL %in% c("RELATED","POSSIBLY RELATED"),
                      "RELATED", "NOT RELATED")
  ) |>
  transmute(
    STUDYID, USUBJID, AESEQ, TRT01A, TRT01AN,
    AETERM, AEDECOD, ASTDT, AENDT, ASTDY,
    AESEV, AEREL, RELGR1, AESER, TRTEMFL
  ) |>
  arrange(USUBJID, suppressWarnings(as.integer(AESEQ)))

message("Mock 代码执行完毕  ADSL=", nrow(adsl), "行  ADAE=", nrow(adae), "行")
'

  risk_logs_list <- list(
    list(level="WARNING", variable="ITTFL",
         description="默认所有 DM 受试者 ITTFL = 'Y'",
         assumption="若方案有额外 ITT 排除标准，需修改此逻辑"),
    list(level="WARNING", variable="TRTSDT/TRTEDT",
         description="优先取 EX 域给药日期，无 EX 记录时回退至 DM.RFXSTDTC/RFXENDTC",
         assumption="若两域日期存在系统性差异，请以方案规定来源为准"),
    list(level="INFO", variable="TRT01PN/TRT01AN",
         description="治疗编号硬编码：Placebo=0, Test Drug=1",
         assumption="若存在其他治疗臂，请更新 trt_num_map")
  )

  return(list(r_code = r_code_str, risk_logs = risk_logs_list))
  }

  # ── 非标准目标：动态生成通用骨架代码 ──────────────────────────────────────
  skeleton_blocks <- paste(
    sapply(target_datasets, function(ds) {
      paste0(
        "# ── ", toupper(ds), " ──────────────────────────────────────────────\n",
        ds, " <- data.frame(\n",
        "  USUBJID = dm$USUBJID,\n",
        "  STUDYID = dm$STUDYID,\n",
        "  stringsAsFactors = FALSE\n",
        ")\n",
        'message("', ds, ' 生成完毕  行数=", nrow(', ds, '))\n'
      )
    }),
    collapse = "\n"
  )

  r_code_generic <- paste0(
    '# ============================================================\n',
    '# [LLM Mock] 通用 ADaM 骨架代码\n',
    '# 目标数据集：', paste(toupper(target_datasets), collapse = ", "), '\n',
    '# ⚠ 请替换为基于实际规格的推导逻辑\n',
    '# ============================================================\n',
    'library(dplyr)\n',
    'library(lubridate)\n\n',
    skeleton_blocks
  )

  risk_generic <- list(
    list(level    = "WARNING",
         variable = "ALL",
         description = paste0("Mock 模式返回通用占位骨架，目标数据集：",
                              paste(target_datasets, collapse = ", ")),
         assumption  = "请根据 ADaM 规格替换为实际推导逻辑后再执行")
  )

  list(r_code = r_code_generic, risk_logs = risk_generic)
}

# =============================================================================
# [保留不变] 辅助：format_risk_logs()
# =============================================================================
format_risk_logs <- function(risk_logs) {
  if (length(risk_logs) == 0) return("（无风险提示）")
  lines <- lapply(seq_along(risk_logs), function(i) {
    log  <- risk_logs[[i]]
    icon <- switch(log$level %||% "INFO", "ERROR"="🔴", "WARNING"="🟡", "🔵")
    paste0(icon, " [", log$level, "] ", log$variable, "\n",
           "   描述: ", log$description, "\n",
           "   假设: ", log$assumption)
  })
  paste(lines, collapse="\n\n")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
