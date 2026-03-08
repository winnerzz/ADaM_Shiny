# llm_api.R
# ============================================================================
# 大模型 API 多源路由模块
# 核心改动：
#   1. 根据用户选择的 provider 动态切换 Base URL 与 model 字符串
#   2. 所有 Provider 均兼容 OpenAI 请求体格式（messages / model / temperature）
#   3. 强化 System Prompt：注入 CDISC ADaM 业务规则（ADSL → ADAE 依赖）
# ──────────────────────────────────────────────────────────────────────
# [FIX] 401 修复说明：
#   - 从 httr 迁移到 httr2，使用 req_auth_bearer_token() 确保
#     Authorization 头格式为严格的 "Bearer <token>"
#   - 增加 api_key 非空校验（防止 input$api_key 传入 NULL / 空串）
#   - 增加调试 message()：打印请求 URL、model、脱敏后的 key 前缀
# ============================================================================

library(httr2)
library(jsonlite)

# ============================================================
# 1. Provider 配置表（动态路由核心）
#    每个 provider 包含：base_url、default_model
#    全部兼容 OpenAI 风格请求体
# ============================================================
LLM_PROVIDERS <- list(
  kimi = list(
    name      = "Kimi (Moonshot AI)",
    base_url  = "https://api.moonshot.cn/v1/chat/completions",
    model     = "moonshot-v1-8k"
  ),
  deepseek = list(
    name      = "DeepSeek",
    base_url  = "https://api.deepseek.com/chat/completions",
    model     = "deepseek-chat"
  ),
  openai = list(
    name      = "OpenAI (GPT-4)",
    base_url  = "https://api.openai.com/v1/chat/completions",
    model     = "gpt-4"
  ),
  qwen = list(
    name      = "通义千问 (Qwen)",
    base_url  = "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions",
    model     = "qwen-plus"
  )
)

# ============================================================
# [FIX] 辅助函数：脱敏 API Key 用于调试日志
#   只显示前 6 位 + 尾 4 位，中间用 **** 替代
# ============================================================
mask_key <- function(key) {
  if (is.null(key) || !nzchar(key)) return("<EMPTY>")
  n <- nchar(key)
  if (n <= 10) return(paste0(substr(key, 1, 3), "****"))
  paste0(substr(key, 1, 6), "****", substr(key, n - 3, n))
}

# ============================================================
# 2. 强化版 System Prompt
#    包含 CDISC ADaM 业务规则：BDS/OCCDS 依赖 ADSL
# ============================================================
build_system_prompt <- function() {
  paste0(
    "你是一位精通 CDISC ADaM 标准的临床数据编程专家。",
    "用户会提供 SDTM 域的列名和 Specification（变量级元数据），",
    "你需要为每个 ADaM 数据集生成完整、可直接运行的 R 代码。\n\n",

    "## 关键业务规则\n",
    "注意：在 CDISC ADaM 标准中，BDS/OCCDS 数据集（如 ADAE）强依赖于 ADSL。",
    "如果在推导逻辑中看到类似于 `(via ADSL)` 的描述，你在生成 R 代码时，",
    "必须使用 `dplyr::left_join(ae_data, adsl_data, by = \"USUBJID\")` ",
    "将原始数据集与刚刚生成的 ADSL 合并，以提取受试者级别变量",
    "（如 ARM, ACTARM, TRTSDT 等）。",
    "请确保 R 代码严格按先 ADSL、后 ADAE 的顺序生成并正确引入 ADSL。\n\n",

    "## 代码规范\n",
    "1. 使用 library(readr); library(dplyr); library(lubridate); library(stringr)\n",
    "2. 所有 CSV 用 read_csv(..., col_types = cols(.default = col_character())) 读入\n",
    "3. 日期列可能有前导撇号（Excel 安全格式），请用 str_replace(x, \"^'\", \"\") 清理\n",
    "4. Study Day 计算：(date - ref) + 1，Day 1 = 首次给药日\n",
    "5. 最终用 write_csv() 输出，缺失值写空字符串 (na = \"\")\n",
    "6. 只输出纯 R 代码，不要 Markdown 代码围栏（```），不要解释文字\n",
    "7. ADSL 数据框命名为 `adsl`，ADAE 命名为 `adae`，以便后续引用\n",
    "8. ADAE 代码中如需 ADSL 变量，直接引用已存在的 `adsl` 数据框即可"
  )
}

# ============================================================
# 3. 构建发送给 LLM 的 User Prompt
# ============================================================
build_user_prompt <- function(dataset_name, spec_text, sdtm_info, extra_ctx = "") {
  parts <- c(
    sprintf("请为 %s 数据集生成完整的 R 代码。\n", dataset_name),
    "## SDTM 源数据列名\n",
    sdtm_info,
    "\n## Specification（变量级元数据）\n",
    spec_text
  )
  if (nzchar(extra_ctx)) {
    parts <- c(parts, "\n## 额外上下文\n", extra_ctx)
  }
  paste(parts, collapse = "\n")
}

# ============================================================
# 4. 核心调用函数：call_llm()
#    [FIX] 完全重写 HTTP 请求部分：
#      - httr  → httr2（管道式 API）
#      - 使用 req_auth_bearer_token() 设置 Authorization 头
#      - 使用 req_body_json() 自动序列化 + 自动设置 Content-Type
#      - 增加 api_key 非空校验
#      - 增加调试日志（脱敏 key）
# ============================================================
call_llm <- function(provider_key, api_key, user_prompt,
                     temperature = 0.2, max_tokens = 4096,
                     model_override = NULL) {

  # ── [FIX] 校验 api_key 非空 ──────────────────────────────
  # 防止 Shiny 的 input$api_key 在某些时机传入 NULL 或空字符串
  if (is.null(api_key) || !nzchar(trimws(api_key))) {
    stop("API Key 为空，请在侧边栏「AI 引擎配置」中输入有效的 Key")
  }
  api_key <- trimws(api_key)  # [FIX] 去除前后空白（复制粘贴常见问题）

  # ── 路由：获取当前 Provider 配置 ─────────────────────────
  cfg <- LLM_PROVIDERS[[provider_key]]
  if (is.null(cfg)) {
    stop(sprintf("未知的模型提供商: %s", provider_key))
  }
  if (!is.null(model_override) && nzchar(trimws(model_override))) {
    cfg$model <- trimws(model_override)
  }

  # ── [FIX] 调试日志：打印请求关键信息（脱敏 key） ─────────
  message("==================================================")
  message(sprintf("[LLM DEBUG] Provider : %s", cfg$name))
  message(sprintf("[LLM DEBUG] URL      : %s", cfg$base_url))
  message(sprintf("[LLM DEBUG] Model    : %s", cfg$model))
  message(sprintf("[LLM DEBUG] API Key  : %s", mask_key(api_key)))
  message(sprintf("[LLM DEBUG] Key长度   : %d 字符", nchar(api_key)))
  message(sprintf("[LLM DEBUG] Prompt长度: %d 字符", nchar(user_prompt)))
  message("==================================================")

  # ── 组装请求体（OpenAI 兼容格式） ────────────────────────
  body <- list(
    model = cfg$model,
    messages = list(
      list(
        role = "system",
        content = build_system_prompt()
      ),
      list(
        role = "user",
        content = user_prompt
      )
    ),
    temperature = temperature,
    max_tokens = max_tokens
  )

  # ── [FIX] 使用 httr2 构建并发送请求 ──────────────────────
  #
  #  req_auth_bearer_token(api_key) 会生成精确的请求头：
  #      Authorization: Bearer sk-xxxxxxxxxxxxxx
  #
  #  req_body_json(body, auto_unbox = TRUE) 会同时：
  #      1. 将 R list 用 jsonlite::toJSON 序列化
  #      2. 自动设置 Content-Type: application/json
  #
  #  这两步取代了旧版 httr 中手动拼接 add_headers + toJSON + encode="raw"
  #  的方式，避免了 Authorization 头格式不正确或 Content-Type 丢失的问题。

  resp <- request(cfg$base_url) |>
    req_auth_bearer_token(api_key) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_timeout(120) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  # ── [FIX] 解析响应（httr2 风格） ─────────────────────────
  status <- resp_status(resp)
  message(sprintf("[LLM DEBUG] 响应状态码: %d", status))

  if (status >= 400) {
    err_body <- resp_body_string(resp, encoding = "UTF-8")
    message(sprintf("[LLM DEBUG] 错误响应体: %s", err_body))
    stop(sprintf(
      "[%s] API 请求失败 (HTTP %s):\n%s",
      cfg$name, status, err_body
    ))
  }

  result   <- resp_body_json(resp)
  raw_text <- result$choices[[1]]$message$content

  message(sprintf("[LLM DEBUG] 成功! 返回代码长度: %d 字符", nchar(raw_text)))

  # ── 清理：去掉 LLM 可能包裹的 Markdown 代码围栏 ──────────
  clean_code <- gsub("^```[Rr]?\\s*\n?", "", raw_text)
  clean_code <- gsub("\n?```\\s*$", "", clean_code)
  trimws(clean_code)
}

# ============================================================
# 5. 辅助函数：从 Spec CSV 文本中判断数据集名称
# ============================================================
detect_dataset_name <- function(spec_df) {
  if ("Dataset" %in% names(spec_df)) {
    unique_ds <- unique(na.omit(spec_df$Dataset))
    if (length(unique_ds) > 0) return(toupper(unique_ds[1]))
  }
  return(NA_character_)
}

# ============================================================
# 6. 辅助函数：格式化 Spec 数据框为可读文本
# ============================================================
format_spec_text <- function(spec_df) {
  key_cols <- intersect(
    c("Dataset", "Variable", "Label", "Type", "Length",
      "Source", "Derivation", "CodeList", "Origin"),
    names(spec_df)
  )
  if (length(key_cols) == 0) {
    return(paste(capture.output(print(spec_df)), collapse = "\n"))
  }
  sub_df <- spec_df[, key_cols, drop = FALSE]
  paste(capture.output(print(sub_df, n = Inf, width = Inf)), collapse = "\n")
}

# ============================================================
# 7. 辅助函数：汇总 SDTM 域的列名信息
# ============================================================
summarize_sdtm_columns <- function(sdtm_list) {
  lines <- vapply(names(sdtm_list), function(nm) {
    cols <- paste(names(sdtm_list[[nm]]), collapse = ", ")
    sprintf("%s: [%s]", toupper(nm), cols)
  }, character(1))
  paste(lines, collapse = "\n")
}
