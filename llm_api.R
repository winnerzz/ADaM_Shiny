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
# 返回：list(r_code = "...", risk_logs = list(...), token_info = list(...))
# =============================================================================
call_llm_engine_with_failover <- function(spec_json,
                                           data_summary,
                                           provider_key_map,
                                           failover_chain,
                                           sdtm_list       = NULL,
                                           base_url_map    = list(),
                                           mock            = MOCK_MODE,
                                           target_datasets = NULL,
                                           prompt_profile  = list()) {
  if (is.list(spec_json)) {
    spec_json <- toJSON(spec_json, pretty = TRUE, auto_unbox = TRUE)
  }

  if (isTRUE(mock)) {
    message("[llm_api] Mock 模式 — 返回硬编码响应")
    return(.mock_llm_response(
      target_datasets = target_datasets,
      task       = prompt_profile$task %||% "full_generation",
      mock_mode  = prompt_profile$mock_mode %||% "default",
      spec_json  = spec_json
    ))
  }

  prompts    <- .build_prompts(
    spec_json       = spec_json,
    data_summary    = data_summary,
    sdtm_list       = sdtm_list,
    target_datasets = target_datasets,
    prompt_profile  = prompt_profile
  )
  last_error <- NULL

  for (attempt in failover_chain) {
    prov  <- attempt$provider
    model <- attempt$model
    key   <- provider_key_map[[prov]] %||% ""
    url   <- base_url_map[[prov]]     %||% NULL

    message("[llm_api] 调用 ", prov, " · 模型=", model)

    result <- tryCatch(
      .call_real_api(
        prompts$system,
        prompts$user,
        key,
        model,
        prov,
        url,
        request_timeout = as.integer(prompt_profile$request_timeout %||% 180L),
        max_tokens = as.integer(prompt_profile$max_tokens %||% 4096L)
      ),
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
                            mock      = MOCK_MODE,
                            prompt_profile = list()) {
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
    mock             = mock,
    prompt_profile   = prompt_profile
  )
}

# =============================================================================
# 统一 API 调用：.call_real_api()
# 从注册表读取 auth_scheme / json_mode / anthropic_style，
# 统一处理所有 7 个提供商，无平行分支。
# =============================================================================
.call_real_api <- function(system_prompt, user_prompt, api_key, model, provider,
                            base_url_override = NULL,
                            request_timeout = 180L,
                            max_tokens = 4096L) {
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
      max_tokens = max_tokens,
      system     = system_prompt,
      messages   = list(list(role = "user", content = user_prompt))
    )
  } else {
    b <- list(
      model       = model,
      temperature = 0.1,
      max_tokens  = max_tokens,
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
    req_timeout(request_timeout) |>
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

  # ── 提取原始文本 + token 用量 ────────────────────────────────────────────────
  resp_body   <- resp_body_json(resp, simplifyVector = FALSE)
  raw_content <- if (cfg$anthropic_style) {
    resp_body$content[[1]]$text
  } else {
    resp_body$choices[[1]]$message$content
  }

  # token 用量（OpenAI: prompt_tokens/completion_tokens；Anthropic: input_tokens/output_tokens）
  usage <- resp_body$usage %||% list()
  token_info <- if (cfg$anthropic_style) {
    list(input  = as.integer(usage$input_tokens  %||% 0L),
         output = as.integer(usage$output_tokens %||% 0L))
  } else {
    list(input  = as.integer(usage$prompt_tokens     %||% 0L),
         output = as.integer(usage$completion_tokens %||% 0L))
  }
  token_info$total <- token_info$input + token_info$output

  result <- .parse_llm_json(raw_content, model)
  result$token_info <- token_info
  result
}

# =============================================================================
# [L-0] Spec JSON → pipe-delimited table 压缩函数
# 将 pretty-print JSON spec 转换为紧凑管道分隔格式，节省 prompt token
#
# 参数：
#   spec_json   — 字符型，pretty-printed JSON 字符串
#   fold_copied — 逻辑型，TRUE 时将 Copied 变量折叠为一行摘要
#
# 返回：字符型，管道分隔格式文本；JSON 解析失败时原样返回
# =============================================================================
.spec_to_pipe_table <- function(spec_json, fold_copied = FALSE) {
  parsed <- tryCatch(
    jsonlite::fromJSON(spec_json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) return(spec_json)

  # parsed 可能是命名列表 {adsl: {...}, adae: {...}} 或未命名数组 [{...}, {...}]
  if (!is.list(parsed) || length(parsed) == 0) return(spec_json)

  # 统一为命名列表：从 dataset 字段提取名称
  if (is.null(names(parsed))) {
    nms <- vapply(parsed, function(x) {
      tolower(as.character(x$dataset %||% x$Dataset %||% "")[1])
    }, character(1))
    nms[nms == ""] <- paste0("ds_", seq_along(nms))[nms == ""]
    names(parsed) <- nms
  }
  datasets <- parsed

  sections <- character(0)
  for (nm in names(datasets)) {
    ds <- datasets[[nm]]
    if (!is.list(ds)) next
    vars <- ds$variables
    if (is.null(vars) || length(vars) == 0) next

    # 提取每个变量的字段
    rows <- lapply(vars, function(v) {
      src <- as.character(v$source %||% "")[1]
      # 去除 "SDTM." 前缀
      src <- sub("^SDTM\\.", "", src, ignore.case = TRUE)
      list(
        variable   = as.character(v$variable %||% "")[1],
        type       = as.character(v$type %||% "")[1],
        source     = src,
        derivation = as.character(v$derivation %||% "")[1]
      )
    })

    if (isTRUE(fold_copied)) {
      # 分离 Copied 和 Derived 变量
      is_copied <- vapply(rows, function(r) {
        grepl("^copied$", trimws(r$type), ignore.case = TRUE)
      }, logical(1))

      copied_rows  <- rows[is_copied]
      derived_rows <- rows[!is_copied]

      lines <- character(0)
      # 折叠 Copied 变量为一行
      if (length(copied_rows) > 0) {
        copied_parts <- vapply(copied_rows, function(r) {
          paste0(r$variable, "<-", r$source)
        }, character(1))
        lines <- c(lines, paste0("COPIED: ", paste(copied_parts, collapse = ", ")))
      }
      # Derived 变量用管道表
      if (length(derived_rows) > 0) {
        lines <- c(lines, "variable|source|derivation")
        for (r in derived_rows) {
          lines <- c(lines, paste0(r$variable, "|", r$source, "|", r$derivation))
        }
      }
      sections <- c(sections, paste0("### ", toupper(nm), "\n", paste(lines, collapse = "\n")))
    } else {
      # 不折叠：全部变量用管道表（不含 label 列）
      lines <- "variable|type|source|derivation"
      for (r in rows) {
        lines <- c(lines, paste0(r$variable, "|", r$type, "|", r$source, "|", r$derivation))
      }
      sections <- c(sections, paste0("### ", toupper(nm), "\n", paste(lines, collapse = "\n")))
    }
  }

  if (length(sections) == 0) return(spec_json)
  paste(sections, collapse = "\n\n")
}

# =============================================================================
# [L-1] Prompt 构建函数：.build_prompts()
# 新增 target_datasets 参数，动态生成数据集输出指令（向后兼容）
# =============================================================================
.build_prompts <- function(spec_json, data_summary, sdtm_list = NULL,
                            target_datasets = NULL,
                            prompt_profile = list()) {

  generation_mode <- tolower(trimws(prompt_profile$mode %||% "balanced"))
  if (!generation_mode %in% c("strict", "balanced", "adaptive")) {
    generation_mode <- "balanced"
  }
  task_mode <- tolower(trimws(prompt_profile$task %||% "full_generation"))
  if (!task_mode %in% c("full_generation", "code_from_plan", "repair_code")) {
    task_mode <- "full_generation"
  }
  traceability_mode <- isTRUE(prompt_profile$traceability)
  compact_mode      <- isTRUE(prompt_profile$compact_mode)
  preview_rows_n    <- suppressWarnings(as.integer(prompt_profile$preview_rows %||% 5L))
  if (is.na(preview_rows_n) || preview_rows_n < 3L) preview_rows_n <- 3L
  if (preview_rows_n > 8L) preview_rows_n <- 8L
  parsed_input      <- tryCatch(fromJSON(spec_json, simplifyVector = FALSE), error = function(e) NULL)

  # ── 推断目标数据集列表 ──────────────────────────────────────────────────────
  if (is.null(target_datasets) || length(target_datasets) == 0) {
    # 尝试从输入 JSON 中提取 dataset 字段
    parsed_spec <- tryCatch(fromJSON(spec_json, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(parsed_input) && is.list(parsed_input) && !is.null(parsed_input$target_datasets)) {
      target_datasets <- tolower(as.character(unlist(parsed_input$target_datasets, use.names = FALSE)))
    } else if (!is.null(parsed_input) && is.list(parsed_input) &&
               !is.null(parsed_input$current_plan) && !is.null(parsed_input$current_plan$datasets)) {
      target_datasets <- unique(tolower(vapply(parsed_input$current_plan$datasets, function(ds) {
        as.character(ds$dataset %||% NA_character_)[1]
      }, character(1))))
    } else if (!is.null(parsed_spec)) {
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
      stop("无法从输入中推断目标数据集。请确保 Spec 中包含 'dataset' 字段。")
    }
  }
  target_datasets <- tolower(trimws(target_datasets))
  target_datasets <- unique(target_datasets[!is.na(target_datasets) & nzchar(target_datasets)])

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

  # ── 按数据集独立拼装骨架片段 ──────────────────────────────────────────
  skeleton_parts <- character(0)

  # 辅助函数用法总是包含
  skeleton_header <- paste0(
    "# 以下函数已预注入执行环境，直接调用即可，禁止重新定义：\n",
    "# parse_sdtm_date(x)              → Date 或 NA（向量化，自动剥离 Excel 单引号）\n",
    "# study_day_chr(date_chr, ref_chr) → character Study Day（向量化）\n",
    "# map_trt_num(TRT01A, trt_levels, start_at) → 按 Spec 编码（向量化）\n",
    "# first_non_missing_chr(x, y, ...) → 首个非空 character（向量化）\n",
    "# yn_flag(condition)               → 'Y'/'N'（向量化）\n",
    "# derive_trtemfl(start, end, trtsdt, trtedt) → 'Y'/''（向量化，含边界检查）\n",
    "# derive_relgr1(AEREL)             → 'RELATED'/'NOT RELATED'（向量化）\n\n"
  )

  if ("adsl" %in% target_datasets) {
    skeleton_parts <- c(skeleton_parts, paste0(
      "# ── ADSL ─────────────────────────────────────────────────────────\n",
      "trt_levels <- sort(unique(na.omit(c(dm$ACTARM, dm$ARM))))\n",
      "# 治疗日期可从 EX 汇总或 DM 直接派生，两种均合法，关键是必须经 parse_sdtm_date()\n",
      "ex_summary <- ex |>\n",
      "  group_by(USUBJID) |>\n",
      "  summarise(NEX = n(), .groups='drop')\n",
      "adsl <- dm |>\n",
      "  left_join(ex_summary, by='USUBJID') |>\n",
      "  mutate(\n",
      "    TRT01A  = first_non_missing_chr(na_if(ACTARM,''), na_if(ARM,'')),\n",
      "    TRT01AN = map_trt_num(TRT01A, trt_levels, start_at = 0L),\n",
      "    TRTSDT  = parse_sdtm_date(RFXSTDTC),\n",
      "    TRTEDT  = parse_sdtm_date(RFXENDTC),\n",
      "    TRTEDY  = study_day_chr(format(TRTEDT,'%Y-%m-%d'), format(TRTSDT,'%Y-%m-%d')),\n",
      "    SAFFL   = yn_flag(!is.na(TRTSDT) & NEX > 0)\n",
      "    # ... 按 Spec 派生其余变量，最终 select() 仅保留 Spec 变量\n",
      "  )\n\n"
    ))
  }

  if ("adae" %in% target_datasets) {
    skeleton_parts <- c(skeleton_parts, paste0(
      "# ── ADAE ─────────────────────────────────────────────────────────\n",
      "adae <- ae |>\n",
      "  left_join(adsl |> select(USUBJID, TRTSDT, TRTEDT, TRT01A, TRT01AN, TRT01P, TRT01PN, SUBJID), by='USUBJID') |>\n",
      "  mutate(\n",
      "    ASTDT   = parse_sdtm_date(AESTDTC),\n",
      "    ASTDY   = study_day_chr(AESTDTC, format(TRTSDT,'%Y-%m-%d')),\n",
      "    TRTEMFL = yn_flag(ASTDT >= TRTSDT),  # 或 derive_trtemfl() 用于复杂边界\n",
      "    RELGR1  = derive_relgr1(AEREL)\n",
      "    # ... 按 Spec 派生其余变量，最终 select() 仅保留 Spec 变量\n",
      "  )\n\n"
    ))
  }

  # 对未识别的数据集给通用指导
  other_ds <- setdiff(target_datasets, c("adsl", "adae"))
  if (length(other_ds) > 0) {
    skeleton_parts <- c(skeleton_parts, paste0(
      "# ── 通用数据集骨架（", paste(toupper(other_ds), collapse = "/"), "）──────────\n",
      "# 1. 从相关 SDTM 域读取源数据（如 lb, vs, cm 等）\n",
      "# 2. left_join(adsl) 获取受试者级信息（如有依赖）\n",
      "# 3. 所有日期用 parse_sdtm_date()，研究日用 study_day_chr()\n",
      "# 4. 数值编码用 map_trt_num()，Y/N 标志用 yn_flag()\n",
      "# 5. 管道末尾用 select() 仅保留 Spec 声明的变量（C7）\n",
      "# 6. 最终对象名必须为小写数据集名（如 ", other_ds[1], "）\n",
      "# ─────────────────────────────────────────────────────────────────\n\n"
    ))
  }

  canonical_skeleton <- paste0(skeleton_header, paste(skeleton_parts, collapse = ""))

  system_prompt <- paste0(
    "你是一位资深 CDISC ADaM 数据程序员，精通 R 语言（dplyr / lubridate）。\n\n",

    # ── 硬约束（4 类）──────────────────────────────────────────────────────
    "【硬约束】\n",
    "R1. 输出：只返回合法 JSON，包含且仅包含两个字段：\n",
    '    {"r_code":"完整可运行 R 代码（\\n 转义）","risk_logs":[{"level":"WARNING|INFO|ERROR","variable":"变量名","description":"推断内容","assumption":"需人工确认的前提"}]}\n',
    "    不要在 JSON 之外输出任何文字或 Markdown 代码块标记。\n",
    "R2. 语义：SDTM 域对象名为小写(dm/ex/ae)；变量必须可追溯到输入域或已派生变量；",
    if (all(c("adsl", "adae") %in% target_datasets)) {
      "先创建 adsl 再创建 adae；"
    } else if (length(target_datasets) > 1) {
      "被 left_join 的数据集必须先创建；"
    } else { "" },
    "同一 mutate() 内被依赖变量先定义。\n",
    "R3. 稳健性：日期用 parse_sdtm_date()，研究日用 study_day_chr()，治疗编码用 map_trt_num()（禁止硬编码）；",
    "向量化逻辑用 & / |，禁止 && / ||；所有辅助函数已预注入且向量化，禁止重新定义。\n",
    "R4. 完整性：管道末尾 select() 仅保留 Spec 变量；代码可直接 eval()；",
    paste0("必须能 ", paste(sprintf("get('%s')", target_datasets), collapse = " / "), " 提取结果。\n\n"),

    # ── 代码规范 ───────────────────────────────────────────────────────────
    "【代码规范】\n",
    "1. 可用包（已预加载）：dplyr、lubridate、stringr、tidyr、readr、haven、purrr、forcats、janitor、glue、stats\n",
    ds_instruction,
    "3. 所有自主推断记入 risk_logs；只记录真实假设或 Spec 歧义\n",
    switch(task_mode,
      code_from_plan  = "4. code-from-plan 模式：严格按输入 plan 生成代码，输出紧凑\n",
      repair_code     = "4. repair-code 模式：基于输入的 current_code 修复缺失变量，返回完整修复后代码\n",
      ""
    ),
    "\n",

    # ── 规范骨架（few-shot）───────────────────────────────────────────────
    "【参考骨架 — 展示关键模式和函数用法，按 Spec 扩展为完整代码】\n",
    canonical_skeleton,
    "\n",

    # ── 高频错误提示（recency position）──────────────────────────────────────
    "【高频错误】\n",
    "1. SDTM 日期直接赋原始字符串（如 TRTSDT=RFXSTDTC）→ 必须经 parse_sdtm_date()\n",
    "2. mutate/filter 内使用 && / || → 改为向量化 & / |\n\n",

    # ── 生成策略 ───────────────────────────────────────────────────────────
    "【当前生成策略】",
    switch(generation_mode,
      strict   = " 稳健优先：信息不足时保守处理，不过度补全\n",
      adaptive = " 补全优先：可主动推断缺失映射，所有补全写入 risk_logs\n",
      balanced = " 平衡模式：可运行性与保守性平衡\n",
                 " 平衡模式：可运行性与保守性平衡\n"
    ),
    if (traceability_mode)
      "强化追溯：关键变量保持逻辑分段清晰，risk_logs 完整记录假设来源\n"
    else "",
    if (compact_mode)
      "上下文压缩模式：仅结构化摘要，无原始样本预览\n"
    else
      paste0("上下文：每域提供前 ", preview_rows_n, " 行样本预览\n")
  )

  # ── pipe_format 压缩（默认 TRUE）──────────────────────────────────────────

  use_pipe_format <- !isFALSE(prompt_profile$pipe_format)  # 默认 TRUE
  fold_copied     <- isTRUE(prompt_profile$fold_copied)    # 默认 FALSE

  spec_content <- if (isTRUE(use_pipe_format) && task_mode == "full_generation") {
    .spec_to_pipe_table(spec_json, fold_copied = fold_copied)
  } else {
    spec_json
  }

  spec_format_label <- if (isTRUE(use_pipe_format) && task_mode == "full_generation") {
    "\u7BA1\u9053\u5206\u9694\u683C\u5F0F"
  } else {
    "JSON \u683C\u5F0F"
  }

  part_spec <- paste0(
    if (task_mode == "code_from_plan") {
      paste0("## 1. Derivation Plan\uFF08", spec_format_label, "\uFF09\n")
    } else if (task_mode == "repair_code") {
      paste0("## 1. Repair Request\uFF08", spec_format_label, "\uFF09\n")
    } else {
      paste0("## 1. ADaM \u53D8\u91CF\u89C4\u683C\uFF08", spec_format_label, "\uFF09\n")
    },
    if (task_mode == "code_from_plan") {
      "\u4EE5\u4E0B\u662F\u5DF2\u7ECF\u786E\u8BA4\u7684 derivation plan\u3002\u8BF7\u4E25\u683C\u6309\u8BE5 plan \u751F\u6210\u4EE3\u7801\u3002\u8F93\u51FA JSON \u53EA\u5305\u542B r_code \u548C risk_logs \u4E24\u4E2A\u5B57\u6BB5\uFF1A\n\n"
    } else if (task_mode == "repair_code") {
      "\u4EE5\u4E0B\u662F\u4E00\u6B21\u7ED3\u6784\u5316\u4EE3\u7801\u4FEE\u590D\u8BF7\u6C42\uFF0C\u5305\u542B\u7F3A\u5931\u53D8\u91CF\u3001\u5F53\u524D derivation plan \u4E0E\u5F53\u524D\u4EE3\u7801\u3002\u8BF7\u4EC5\u4FEE\u590D\u7F3A\u5931\u53D8\u91CF\u76F8\u5173\u95EE\u9898\uFF0C\u5E76\u8FD4\u56DE\u5B8C\u6574\u4EE3\u7801\uFF1A\n\n"
    } else {
      "\u4EE5\u4E0B\u662F\u76EE\u6807 ADaM \u6570\u636E\u96C6\u7684\u53D8\u91CF\u5143\u6570\u636E\uFF0C\u5305\u542B\u53D8\u91CF\u540D\u3001\u7C7B\u578B\u3001\u6765\u6E90\u57DF\u548C\u6D3E\u751F\u903B\u8F91\u3002\u4F60\u5E94\u5148\u628A\u8FD9\u4E9B\u4FE1\u606F\u6574\u7406\u4E3A derivation plan\uFF0C\u518D\u751F\u6210\u4EE3\u7801\uFF1A\n\n"
    },
    spec_content,
    if (task_mode %in% c("code_from_plan", "repair_code")) {
      "\n\n\u8BF7\u7262\u8BB0\uFF1A\u7528\u6237\u4F1A\u76F4\u63A5\u6267\u884C\u4F60\u8F93\u51FA\u7684 r_code\uFF0C\u56E0\u6B64\u6700\u91CD\u8981\u7684\u662F\u6700\u7EC8\u73AF\u5883\u4E2D\u80FD\u7A33\u5B9A\u5F97\u5230\u76EE\u6807\u6570\u636E\u96C6\u5BF9\u8C61\uFF0C\u800C\u4E0D\u662F\u5C55\u793A\u591A\u79CD\u5907\u9009\u5199\u6CD5\u3002"
    } else {
      ""
    }
  )

  # ── 动态变量清单：从 spec 中提取所有变量名，要求 LLM 不得遗漏 ──────────
  part_varlist <- ""
  {
    parsed_for_vars <- parsed_input
    var_by_ds <- list()
    append_vars <- function(dataset_name, vars) {
      ds_name <- toupper(trimws(as.character(dataset_name %||% "UNKNOWN")[1]))
      vals <- as.character(unlist(vars, use.names = FALSE))
      vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
      if (!nzchar(ds_name) || length(vals) == 0) return()
      var_by_ds[[ds_name]] <<- unique(c(var_by_ds[[ds_name]], vals))
    }
    if (is.list(parsed_for_vars) && !is.null(parsed_for_vars)) {
      if (!is.null(parsed_for_vars$missing_spec_rows) &&
          (is.data.frame(parsed_for_vars$missing_spec_rows) || is.list(parsed_for_vars$missing_spec_rows))) {
        msr <- parsed_for_vars$missing_spec_rows
        if (is.data.frame(msr) && all(c("dataset", "variable") %in% names(msr))) {
          for (ds in unique(msr$dataset)) {
            append_vars(ds, msr$variable[msr$dataset == ds])
          }
        } else if (is.list(msr) && !is.null(msr$dataset) && !is.null(msr$variable)) {
          for (ds in unique(unlist(msr$dataset, use.names = FALSE))) {
            append_vars(ds, unlist(msr$variable[unlist(msr$dataset, use.names = FALSE) == ds], use.names = FALSE))
          }
        }
      }
      if (!is.null(parsed_for_vars$current_plan) && is.list(parsed_for_vars$current_plan$datasets)) {
        for (ds in parsed_for_vars$current_plan$datasets) {
          vars <- vapply(ds$variable_plan %||% list(), function(v) {
            as.character(v$variable %||% "")[1]
          }, character(1))
          append_vars(ds$dataset %||% "UNKNOWN", vars)
        }
      }
      # spec_json 结构可能是: [{dataset:"adsl", variables:[{variable:"X",...},...]}]
      # 或扁平结构: [{dataset:"adsl", variable:"X",...}, ...]
      items <- if (!is.null(names(parsed_for_vars))) list(parsed_for_vars) else parsed_for_vars
      for (item in items) {
        if (!is.list(item)) next
        vs <- NULL
        if (is.list(item$variables) && length(item$variables) > 0) {
          # 两种结构：[{variable:"X",...}, ...] 或 {variable:["X","Y",...], type:[...]}
          first <- item$variables[[1]]
          if (is.list(first) && !is.null(first$variable)) {
            vs <- vapply(item$variables, function(v) as.character(v$variable %||% "")[1], character(1))
          } else if (!is.null(item$variables$variable)) {
            vs <- vapply(item$variables$variable, as.character, character(1))
          }
        } else if (!is.null(item$variable)) {
          vs <- as.character(item$variable)
        }
        if (!is.null(vs)) {
          append_vars(item$dataset %||% "UNKNOWN", vs)
        }
      }
    }
    # 仅在 repair_code 模式下生成变量清单（full_generation 模式下 spec 已包含完整变量列表，无需重复）
    if (task_mode == "repair_code" && length(var_by_ds) > 0) {
      lines <- vapply(names(var_by_ds), function(d) {
        paste0(d, ": ", paste(var_by_ds[[d]], collapse = ", "))
      }, character(1))
      part_varlist <- paste0(
        "\n\n【必须生成的变量 - 不可遗漏】\n",
        paste(lines, collapse = "\n"), "\n",
        "你的 r_code 必须在最终数据集中包含且仅包含上述变量（通过末尾 select() 实现）。\n",
        "遗漏变量会触发自动修复重试；多余变量不符合 ADaM 规范。\n"
      )
    }
  }

  part_repair_context <- ""
  if (identical(task_mode, "repair_code") && is.list(parsed_input)) {
    missing_vars <- unique(as.character(unlist(parsed_input$missing_variables %||% character(0), use.names = FALSE)))
    affected_ds  <- unique(as.character(unlist(parsed_input$affected_datasets %||% character(0), use.names = FALSE)))
    current_code <- as.character(parsed_input$current_code %||% "")[1]
    instructions <- as.character(unlist(parsed_input$instructions %||% character(0), use.names = FALSE))

    repair_lines <- c()
    if (length(affected_ds) > 0) {
      repair_lines <- c(repair_lines, paste0("受影响数据集：", paste(toupper(affected_ds), collapse = ", ")))
    }
    if (length(missing_vars) > 0) {
      repair_lines <- c(repair_lines, paste0("缺失变量：", paste(missing_vars, collapse = ", ")))
    }
    if (length(instructions) > 0) {
      repair_lines <- c(repair_lines, paste0("修复要求：", paste(instructions, collapse = "；")))
    }

    part_repair_context <- paste0(
      if (length(repair_lines) > 0) {
        paste0("\n\n## 2. Repair Focus\n", paste(repair_lines, collapse = "\n"))
      } else {
        ""
      },
      if (nzchar(trimws(current_code))) {
        paste0("\n\n## 3. Current Code\n```r\n", current_code, "\n```")
      } else {
        ""
      }
    )
  }

  part_preview <- ""
  if (!is.null(sdtm_list)) {
    preview_rows_eff <- if (compact_mode) 0L else preview_rows_n
    domain_previews <- lapply(names(sdtm_list), function(domain) {
      df <- sdtm_list[[domain]]
      if (is.null(df) || nrow(df) == 0) return(NULL)
      col_line <- paste(names(df), collapse = ", ")
      if (preview_rows_eff == 0L) {
        # 0-row 模式：仅列名清单（节省 token）
        paste0("### ", toupper(domain), "  [", ncol(df), " cols / ", nrow(df), " rows]\n",
               "Columns: ", col_line)
      } else {
        preview_rows <- head(df, preview_rows_eff)
        csv_lines <- c(
          paste(names(preview_rows), collapse = ","),
          apply(preview_rows, 1, function(r) {
            paste(ifelse(is.na(r), "", r), collapse = ",")
          })
        )
        paste0(
          "### ", toupper(domain), "  [", ncol(df), " cols / ", nrow(df), " rows]\n",
          "```csv\n", paste(csv_lines, collapse = "\n"), "\n```"
        )
      }
    })
    domain_previews <- Filter(Negate(is.null), domain_previews)
    if (length(domain_previews) > 0) {
      header <- if (preview_rows_eff == 0L) {
        "\n\n## 2. SDTM 源数据列清单\n"
      } else {
        paste0("\n\n## 2. SDTM 源数据结构（各域前", preview_rows_eff, "行）\n",
               "以下是上传的 SDTM 数据的实际列名和样本数据，请据此推断字段映射关系：\n\n")
      }
      part_preview <- paste0(header, paste(domain_previews, collapse = "\n\n"))
    }
  }

  part_summary <- paste0(
    "\n\n## 3. SDTM 数据摘要\n",
    data_summary
  )

  list(system = system_prompt, user = paste0(part_spec, part_varlist, part_repair_context, part_preview, part_summary))
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

  # fromJSON 在输入为 JSON 字符串 "..." 或数组 [...] 时返回原子向量/列表
  # 而非预期的命名列表——此时 parsed$r_code 会抛出 "$ operator is invalid"
  if (!is.list(parsed) || is.null(names(parsed))) {
    recovered <- FALSE
    if (is.character(parsed) && length(parsed) == 1) {
      # LLM 有时把整个 JSON 包在外层引号里，fromJSON 返回内层字符串
      inner <- trimws(parsed)
      # 去除转义引号（如 \" → "）并重新解析
      inner <- gsub('\\\\"', '"', inner, fixed = FALSE)
      if (grepl("^\\{", inner)) {
        parsed2 <- tryCatch(fromJSON(inner, simplifyVector = FALSE), error = function(e) NULL)
        if (is.list(parsed2) && !is.null(names(parsed2))) {
          parsed <- parsed2
          recovered <- TRUE
        }
      }
    }
    if (!recovered) {
      stop(
        "LLM 返回的 JSON 结构不符合预期（非 object）。\n",
        "类型：", class(parsed)[1], "  前200字符：", substr(clean, 1, 200)
      )
    }
  }

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
  if (is.null(parsed$derivation_plan)) {
    message("[llm_api] 警告：LLM 响应缺少 'derivation_plan'，将由服务端回退生成")
    parsed$derivation_plan <- NULL
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
      "请求超时。\n",
      "可能原因：网络连接慢，或 Spec / plan 仍然过长导致 LLM 生成时间超限。\n",
      "建议：继续压缩规格输入，或改用更快的模型 / 本地推理服务。"
    )
  }

  stop("LLM API 请求失败（", provider, "）：", msg)
}

# =============================================================================
# [L-2] Mock 响应：.mock_llm_response()
# 新增 target_datasets 参数：adsl+adae 时返回原有硬编码响应；否则生成通用骨架
# =============================================================================
.mock_llm_response <- function(target_datasets = c("adsl", "adae"),
                               task       = "full_generation",
                               mock_mode  = "default",
                               spec_json  = NULL) {

  # ── repair_demo 模式：第一次故意缺变量，repair 时返回修复版 ──────────
  if (mock_mode == "repair_demo") {
    if (task == "full_generation") {
      return(.mock_repair_demo_initial(target_datasets))
    }
    if (task == "repair_code") {
      return(.mock_repair_demo_fixed(target_datasets, spec_json))
    }
  }

  # 向后兼容：adsl + adae 时返回原有硬编码响应
  if (identical(sort(tolower(target_datasets)), sort(c("adsl", "adae")))) {

  r_code_str <- '
# ============================================================
# [LLM Mock] ADSL + ADAE 构建代码 — 遵循执行契约
# ============================================================

# ── ADSL ──────────────────────────────────────────────────────
trt_levels <- sort(unique(na.omit(c(dm$ACTARM, dm$ARM))))

ex_summary <- ex |>
  group_by(USUBJID) |>
  summarise(
    TRTSDT = min(parse_sdtm_date(EXSTDTC), na.rm = TRUE),
    TRTEDT = max(parse_sdtm_date(EXENDTC), na.rm = TRUE),
    NEX    = n(),
    .groups = "drop"
  )

adsl <- dm |>
  left_join(ex_summary, by = "USUBJID") |>
  mutate(
    TRT01P  = first_non_missing_chr(na_if(ARM, "")),
    TRT01A  = first_non_missing_chr(na_if(ACTARM, ""), na_if(ARM, "")),
    TRT01PN = map_trt_num(TRT01P, trt_levels, start_at = 0L),
    TRT01AN = map_trt_num(TRT01A, trt_levels, start_at = 0L),
    TRTSDT  = coalesce(TRTSDT, parse_sdtm_date(RFXSTDTC)),
    TRTEDT  = coalesce(TRTEDT, parse_sdtm_date(RFXENDTC)),
    TRTEDY  = study_day_chr(format(TRTEDT, "%Y-%m-%d"), format(TRTSDT, "%Y-%m-%d")),
    SAFFL   = yn_flag(!is.na(TRTSDT) & NEX > 0),
    ITTFL   = "Y"
  )

# ── ADAE ──────────────────────────────────────────────────────
adae <- ae |>
  left_join(adsl |> select(USUBJID, TRTSDT, TRTEDT, TRT01A, TRT01AN), by = "USUBJID") |>
  mutate(
    ASTDT   = parse_sdtm_date(AESTDTC),
    AENDT   = parse_sdtm_date(AEENDTC),
    ASTDY   = study_day_chr(AESTDTC, format(TRTSDT, "%Y-%m-%d")),
    TRTEMFL = derive_trtemfl(AESTDTC, AEENDTC,
                              format(TRTSDT, "%Y-%m-%d"), format(TRTEDT, "%Y-%m-%d")),
    RELGR1  = derive_relgr1(AEREL)
  )

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
         description="治疗编号由 map_trt_num() 自动从 SDTM 实际值派生",
         assumption="若存在非标准治疗臂名称，请核实映射结果")
  )

  return(list(r_code = r_code_str, risk_logs = risk_logs_list,
              token_info = list(input=0L, output=0L, total=0L)))
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

  list(r_code = r_code_generic, risk_logs = risk_generic,
       token_info = list(input=0L, output=0L, total=0L))
}

# =============================================================================
# Mock repair_demo helpers
# =============================================================================
.mock_repair_demo_initial <- function(target_datasets) {
  # 故意缺少 RANDDT, EOTDT, EOTDY, EOTSTT, EOSDT, EOSSTT, DTHDT (ADSL)
  # 及 SUBJID, TRT01P, TRT01PN, ASEV, ASEVN, RELGR1N, ATOXGR, ATOXGRN (ADAE)
  r_code <- '
# [Mock repair_demo - initial] 故意缺少部分变量
trt_levels <- sort(unique(na.omit(c(dm$ACTARM, dm$ARM))))
ex_summary <- ex |>
  group_by(USUBJID) |>
  summarise(NEX = n(), .groups = "drop")

adsl <- dm |>
  left_join(ex_summary, by = "USUBJID") |>
  mutate(
    TRT01P  = first_non_missing_chr(na_if(ARM, "")),
    TRT01A  = first_non_missing_chr(na_if(ACTARM, ""), na_if(ARM, "")),
    TRT01PN = map_trt_num(TRT01P, trt_levels, start_at = 0L),
    TRT01AN = map_trt_num(TRT01A, trt_levels, start_at = 0L),
    TRTSDT  = parse_sdtm_date(RFXSTDTC),
    TRTEDT  = parse_sdtm_date(RFXENDTC),
    TRTEDY  = study_day_chr(format(TRTEDT, "%Y-%m-%d"), format(TRTSDT, "%Y-%m-%d")),
    SAFFL   = yn_flag(!is.na(TRTSDT) & NEX > 0),
    ITTFL   = "Y"
  )

adae <- ae |>
  left_join(adsl |> select(USUBJID, TRTSDT, TRTEDT, TRT01A, TRT01AN), by = "USUBJID") |>
  mutate(
    ASTDT   = parse_sdtm_date(AESTDTC),
    AENDT   = parse_sdtm_date(AEENDTC),
    ASTDY   = study_day_chr(AESTDTC, format(TRTSDT, "%Y-%m-%d")),
    AENDY   = study_day_chr(AEENDTC, format(TRTSDT, "%Y-%m-%d")),
    TRTEMFL = derive_trtemfl(AESTDTC, AEENDTC,
                              format(TRTSDT, "%Y-%m-%d"), format(TRTEDT, "%Y-%m-%d")),
    RELGR1  = derive_relgr1(AEREL)
  )
message("[repair_demo] initial: ADSL=", nrow(adsl), " ADAE=", nrow(adae))
'
  list(
    r_code = r_code,
    risk_logs = list(
      list(level = "WARNING", variable = "ADSL",
           description = "RANDDT/EOTDT/EOTDY/EOTSTT/EOSDT/EOSSTT/DTHDT 未派生",
           assumption = "故意缺少以演示修复流程")
    ),
    token_info = list(input = 0L, output = 0L, total = 0L)
  )
}

.mock_repair_demo_fixed <- function(target_datasets, spec_json) {
  # 补齐所有缺失变量
  r_code <- '
# [Mock repair_demo - fixed] 补齐所有缺失变量
trt_levels <- sort(unique(na.omit(c(dm$ACTARM, dm$ARM))))
ex_summary <- ex |>
  group_by(USUBJID) |>
  summarise(NEX = n(), .groups = "drop")

adsl <- dm |>
  left_join(ex_summary, by = "USUBJID") |>
  mutate(
    TRT01P  = first_non_missing_chr(na_if(ARM, "")),
    TRT01A  = first_non_missing_chr(na_if(ACTARM, ""), na_if(ARM, "")),
    TRT01PN = map_trt_num(TRT01P, trt_levels, start_at = 0L),
    TRT01AN = map_trt_num(TRT01A, trt_levels, start_at = 0L),
    TRTSDT  = parse_sdtm_date(RFXSTDTC),
    TRTEDT  = parse_sdtm_date(RFXENDTC),
    TRTEDY  = study_day_chr(format(TRTEDT, "%Y-%m-%d"), format(TRTSDT, "%Y-%m-%d")),
    RANDDT  = parse_sdtm_date(RFSTDTC),
    EOTDT   = TRTEDT,
    EOTDY   = study_day_chr(format(EOTDT, "%Y-%m-%d"), format(TRTSDT, "%Y-%m-%d")),
    EOTSTT  = ifelse(!is.na(TRTEDT), "COMPLETED", "ONGOING"),
    EOSDT   = parse_sdtm_date(RFENDTC),
    EOSSTT  = ifelse(DTHFL == "Y", "DISCONTINUED", "COMPLETED"),
    DTHDT   = parse_sdtm_date(DTHDTC),
    SAFFL   = yn_flag(!is.na(TRTSDT) & NEX > 0),
    ITTFL   = "Y"
  )

adae <- ae |>
  left_join(adsl |> select(USUBJID, SUBJID, TRTSDT, TRTEDT,
                           TRT01A, TRT01AN, TRT01P, TRT01PN), by = "USUBJID") |>
  mutate(
    ASTDT   = parse_sdtm_date(AESTDTC),
    AENDT   = parse_sdtm_date(AEENDTC),
    ASTDY   = study_day_chr(AESTDTC, format(TRTSDT, "%Y-%m-%d")),
    AENDY   = study_day_chr(AEENDTC, format(TRTSDT, "%Y-%m-%d")),
    ASEV    = AESEV,
    ASEVN   = as.character(match(toupper(AESEV), c("MILD", "MODERATE", "SEVERE"))),
    TRTEMFL = derive_trtemfl(AESTDTC, AEENDTC,
                              format(TRTSDT, "%Y-%m-%d"), format(TRTEDT, "%Y-%m-%d")),
    RELGR1  = derive_relgr1(AEREL),
    RELGR1N = ifelse(RELGR1 == "RELATED", "1", "0"),
    ATOXGR  = AETOXGR,
    ATOXGRN = AETOXGR
  )
message("[repair_demo] fixed: ADSL=", nrow(adsl), " ADAE=", nrow(adae))
'
  list(
    r_code = r_code,
    risk_logs = list(
      list(level = "INFO", variable = "ALL",
           description = "修复版本已补齐所有缺失变量",
           assumption = "repair_demo 模式硬编码修复")
    ),
    token_info = list(input = 0L, output = 0L, total = 0L)
  )
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
