# =============================================================================
# server.R
# ADaM 自动化生成平台 — 服务器端逻辑
#
# ── 本次修改摘要 ─────────────────────────────────────────────────────────────
#   [新增 A] 辅助函数区：
#              .heuristic_col_match()   — 关键词正则匹配列名
#              .call_llm_spec_parser()  — LLM 智能识别列名（含 Mock）
#              .check_row_completeness()— 逐行检查 Derivation 空值
#              .build_parse_modal()     — 构建解析报告 Modal UI
#   [新增 B] reactiveValues：spec_csv_raw / spec_parsed /
#              spec_parse_report / spec_confirmed / step_parse
#   [新增 C] observeEvent(input$file_spec)：CSV 上传后自动解析并弹 Modal
#   [新增 D] observeEvent(input$btn_confirm_spec)：用户确认解析结果
#   [新增 E] observeEvent(input$btn_reparse_spec)：取消/重新上传
#   [新增 F] output$spec_parse_status：侧边栏解析状态卡片
#   [修改 G] observeEvent(btn_generate)：
#              • 校验改为检查 rv$spec_confirmed（原：检查 input$file_spec）
#              • Spec 加载改为使用 rv$spec_parsed（原：load_spec_json()）
# =============================================================================

source("domain_registry.R",   local = TRUE)  # [S-1] SDTM 域注册表
source("data_utils.R",        local = TRUE)
source("validation_utils.R",  local = TRUE)
source("derivation_plan_utils.R", local = TRUE)
source("code_static_checks.R", local = TRUE)
source("llm_api.R",           local = TRUE)
source("provider_registry.R", local = TRUE)
library(shinyjs)   # reset() 用于清空 fileInput

# =============================================================================
# 辅助函数（server 私有，无修改部分）
# =============================================================================

.log_line <- function(..., icon = "›") {
  paste0(format(Sys.time(), "[%H:%M:%S]"), " ", icon, " ", paste0(...))
}

.normalize_risk_logs <- function(risk_logs) {
  if (length(risk_logs) == 0) return(NULL)
  rows <- lapply(risk_logs, function(x) {
    data.frame(level=x$level%||%"INFO", variable=x$variable%||%"—",
               description=x$description%||%"", assumption=x$assumption%||%"",
               stringsAsFactors=FALSE)
  })
  do.call(rbind, rows)
}

.badge_html <- function(level) {
  css <- switch(level,
    "ERROR"   = "badge-error-custom",
    "WARNING" = "badge-warning-custom",
    "PASS"    = "badge-pass-custom",
    "badge-info-custom"
  )
  ico <- switch(level, "ERROR"="✖", "WARNING"="⚠", "PASS"="✔", "ℹ")
  sprintf('<span class="%s">%s %s</span>', css, ico, level)
}

.placeholder_ui <- function(icon_name, line1, line2=NULL) {
  div(style="text-align:center;padding:3rem 1rem;color:#6e7681;",
    div(style="font-size:2rem;margin-bottom:0.6rem;",
        bsicons::bs_icon(icon_name, size="2rem", color="#30363d")),
    div(style="font-size:0.83rem;font-weight:500;color:#8b949e;", line1),
    if (!is.null(line2)) div(style="font-size:0.75rem;margin-top:0.3rem;", line2)
  )
}

.row_badge <- function(n) {
  span(style=paste0("background:rgba(45,212,191,0.12);color:#2dd4bf;",
    "border:1px solid rgba(45,212,191,0.3);border-radius:4px;",
    "font-size:0.72rem;font-family:'JetBrains Mono',monospace;",
    "padding:2px 8px;font-weight:600;"), paste0(n, " rows"))
}

.dt_options <- function(scroll_x=TRUE, page_length=10) {
  list(pageLength=page_length, scrollX=scroll_x, dom="lfrtip", autoWidth=FALSE,
    language=list(search="搜索：",lengthMenu="每页 _MENU_ 行",
      info="第 _START_ – _END_ 行，共 _TOTAL_ 行",
      paginate=list(previous="‹",`next`="›")),
    columnDefs=list(list(className="dt-left",targets="_all")))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

.non_empty <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(x)
  if (!nzchar(x)) return(NULL)
  x
}

.sum_true <- function(x) {
  if (length(x) == 0) return(0L)
  sum(vapply(x, isTRUE, logical(1)))
}

.sum_int <- function(x, fn) {
  if (length(x) == 0) return(0L)
  sum(vapply(x, fn, integer(1)))
}

.split_specs_by_dataset <- function(specs) {
  parsed_specs <- lapply(specs %||% list(), function(s) s$parsed %||% NULL)
  parsed_specs <- Filter(Negate(is.null), parsed_specs)
  if (length(parsed_specs) == 0) return(list())

  out <- list()
  for (spec in parsed_specs) {
    ds_name <- tolower(trimws(spec$dataset %||% ""))
    if (!nzchar(ds_name)) next
    spec_vars <- spec$variables %||% data.frame(stringsAsFactors = FALSE)

    if (is.null(out[[ds_name]])) {
      out[[ds_name]] <- list(dataset = ds_name, variables = spec_vars)
    } else {
      out[[ds_name]]$variables <- dplyr::bind_rows(out[[ds_name]]$variables, spec_vars)
    }
  }

  lapply(out, function(spec) {
    vars <- spec$variables %||% data.frame(stringsAsFactors = FALSE)
    if (is.data.frame(vars) && "variable" %in% names(vars) && nrow(vars) > 0) {
      vars <- vars[!duplicated(toupper(trimws(as.character(vars$variable)))), , drop = FALSE]
    }
    spec$variables <- vars
    spec
  })
}

.merge_token_info <- function(results) {
  toks <- lapply(results %||% list(), function(x) x$token_info %||% list())
  list(
    input = sum(vapply(toks, function(x) as.integer(x$input %||% 0L), integer(1))),
    output = sum(vapply(toks, function(x) as.integer(x$output %||% 0L), integer(1))),
    total = sum(vapply(toks, function(x) as.integer(x$total %||% 0L), integer(1)))
  )
}

.compact_spec_payload <- function(spec) {
  vars <- spec$variables %||% data.frame(stringsAsFactors = FALSE)
  keep_cols <- intersect(c("variable", "type", "source", "derivation", "dataset"), names(vars))
  if (length(keep_cols) > 0) {
    vars <- vars[, keep_cols, drop = FALSE]
  }
  for (col in intersect(c("source", "derivation"), names(vars))) {
    vars[[col]] <- substr(as.character(vars[[col]] %||% ""), 1L, if (col == "derivation") 220L else 120L)
  }
  list(dataset = spec$dataset %||% "unknown", variables = vars)
}

.compact_plan_payload <- function(plan) {
  datasets <- plan$datasets %||% list()
  list(
    plan_version = plan$plan_version %||% "0.1-compact",
    generated_by = plan$generated_by %||% "system-compact",
    datasets = lapply(datasets, function(ds) {
      list(
        dataset = ds$dataset %||% "unknown",
        dataset_role = ds$dataset_role %||% "analysis",
        required_inputs = ds$required_inputs %||% character(0),
        variable_plan = lapply(ds$variable_plan %||% list(), function(v) {
          list(
            variable = v$variable %||% "UNKNOWN",
            type = v$type %||% "char",
            source_domain = v$source_domain %||% NA_character_,
            source_columns = v$source_columns %||% character(0),
            derivation_rule = substr(v$derivation_rule %||% "Derived according to plan.", 1L, 220L)
          )
        })
      )
    })
  )
}

.make_llm_cache_key <- function(dataset, payload_json, profile_summary, model, provider, prompt_profile) {
  digest::digest(list(
    dataset = dataset,
    payload = payload_json,
    profile = profile_summary,
    model = model,
    provider = provider,
    mode = prompt_profile$mode %||% "balanced",
    task = prompt_profile$task %||% "full_generation",
    traceability = isTRUE(prompt_profile$traceability),
    compact = isTRUE(prompt_profile$compact_mode)
  ), algo = "xxhash64")
}

.llm_codegen_worker <- function(job) {
  call_llm_engine_with_failover(
    spec_json        = job$payload_json,
    data_summary     = job$data_summary,
    provider_key_map = job$provider_key_map,
    failover_chain   = job$failover_chain,
    sdtm_list        = job$sdtm_list,
    base_url_map     = job$base_url_map,
    target_datasets  = job$target_dataset,
    prompt_profile   = job$prompt_profile
  )
}

.run_llm_jobs <- function(jobs, project_dir) {
  if (length(jobs) == 0) return(list())
  if (length(jobs) == 1) {
    out <- list(.llm_codegen_worker(jobs[[1]]))
    names(out) <- names(jobs)
    return(out)
  }

  cl <- tryCatch(parallel::makeCluster(min(length(jobs), 2L)), error = function(e) NULL)
  if (is.null(cl)) {
    out <- lapply(jobs, .llm_codegen_worker)
    names(out) <- names(jobs)
    return(out)
  }
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  parallel::clusterExport(cl, varlist = c("project_dir"), envir = environment())
  parallel::clusterEvalQ(cl, {
    setwd(project_dir)
    library(jsonlite)
    library(httr2)
    library(stringr)
    source("provider_registry.R", local = TRUE)
    source("llm_api.R", local = TRUE)
    NULL
  })
  parallel::clusterExport(cl, varlist = c(".llm_codegen_worker"), envir = environment())
  out <- tryCatch(parallel::parLapply(cl, jobs, .llm_codegen_worker), error = function(e) NULL)
  if (is.null(out)) {
    out <- lapply(jobs, .llm_codegen_worker)
  }
  names(out) <- names(jobs)
  out
}

# =============================================================================
# [新增 A] Spec CSV 智能解析辅助函数
# =============================================================================

# -----------------------------------------------------------------------------
# [新增 A-1] .heuristic_col_match()
# 说明：用正则关键词对 CSV 列名进行启发式匹配，找出最可能对应标准字段的列。
#       返回每个标准字段的匹配结果和置信度（HIGH / MEDIUM / LOW / NONE）。
#
# 标准字段（5个必需 + 1个可选）：
#   variable   — 变量名列（如 USUBJID, SAFFL）
#   label      — 变量标签/描述
#   type       — 数据类型（Char / Num）
#   source     — 来源域（如 DM, EX）
#   derivation — 派生逻辑/公式
#   dataset    — 目标数据集名（可选，如 ADSL）
#
# 参数：col_names — 字符向量，CSV 的实际列名
# 返回：命名列表，每个标准字段对应 list(matched_col, confidence, candidates)
# -----------------------------------------------------------------------------
.heuristic_col_match <- function(col_names) {

  # 各标准字段的关键词模式（按置信度排序：精确 > 近似 > 宽泛）
  patterns <- list(
    variable = list(
      high   = "^(variable|var(iable)?_?name|varname|param(eter)?)$",
      medium = "^(col(umn)?(_name)?|field(_name)?|item|name)$"
    ),
    label = list(
      high   = "^(label|var_?label|long_?label|variable_?label)$",
      medium = "^(desc(ription)?|definition|full_?name|display_?name|title)$"
    ),
    type = list(
      high   = "^(type|var_?type|data_?type|d_?type|format)$",
      medium = "^(class|kind|category|dtype|length)$"
    ),
    source = list(
      high   = "^(source|origin|src|source_?domain|sdtm_?source|from)$",
      medium = "^(domain|input|ref(erence)?|based_?on|sdtm)$"
    ),
    derivation = list(
      high   = "^(deriv(ation)?|derivation_?logic|logic|algorithm|rule)$",
      medium = "^(method|computation|formula|how|note|process|mapping|comment)$"
    ),
    dataset = list(
      high   = "^(dataset|adam_?dataset|ds|table)$",
      medium = "^(domain|study|output|target)$"
    )
  )

  # 将 CSV 列名统一转为小写，便于不区分大小写匹配
  col_lower <- tolower(trimws(col_names))

  result <- lapply(names(patterns), function(field) {
    pat  <- patterns[[field]]

    # 先尝试精确匹配（HIGH）
    high_idx <- which(grepl(pat$high, col_lower, perl=TRUE))
    if (length(high_idx) > 0) {
      return(list(matched_col = col_names[high_idx[1]],  # 取第一个匹配
                  confidence  = "HIGH",
                  candidates  = col_names[high_idx]))
    }

    # 再尝试近似匹配（MEDIUM）
    med_idx <- which(grepl(pat$medium, col_lower, perl=TRUE))
    if (length(med_idx) > 0) {
      return(list(matched_col = col_names[med_idx[1]],
                  confidence  = "MEDIUM",
                  candidates  = col_names[med_idx]))
    }

    # 无匹配（LOW / NONE）
    list(matched_col = NA_character_, confidence = "LOW", candidates = character(0))
  })

  names(result) <- names(patterns)
  result
}

# -----------------------------------------------------------------------------
# [新增 A-2] .call_llm_spec_parser()
# 说明：将 CSV 表头和前5行发给 LLM，请求它识别列名映射。
#       当前处于 Mock 模式：直接调用启发式匹配，并模拟一条 LLM 的"推测"
#       补全 LOW 置信度字段。
#       接入真实 API 时：将 prompt 发至 LLM，解析其 JSON 响应返回。
#
# 参数：df       — data.frame，已读取的 Spec CSV
#       col_map  — .heuristic_col_match() 的输出，用于合并启发式结果
# 返回：list(
#         column_mapping  — 标准字段 → 实际列名 的命名字符向量
#         confidence      — 每个字段的置信度
#         llm_suggestions — LLM 对 LOW 置信度字段的补全建议（字符向量）
#       )
# -----------------------------------------------------------------------------
.call_llm_spec_parser <- function(df, col_map) {

  if (isTRUE(MOCK_MODE)) {
    # ── Mock 模式：直接使用启发式结果，并对 LOW 字段模拟 LLM 建议 ──────────

    column_mapping <- sapply(col_map, function(x) x$matched_col %||% NA_character_)
    confidence     <- sapply(col_map, function(x) x$confidence)

    # 对 LOW 置信度字段：从剩余未匹配列中选一个作为"LLM 建议"
    already_used   <- na.omit(unname(column_mapping))
    remaining_cols <- setdiff(names(df), already_used)

    llm_suggestions <- character(0)
    for (field in names(confidence[confidence == "LOW"])) {
      if (length(remaining_cols) > 0) {
        guessed_col            <- remaining_cols[1]
        remaining_cols         <- remaining_cols[-1]
        column_mapping[field]  <- guessed_col
        confidence[field]      <- "MEDIUM"   # LLM 介入后提升到 MEDIUM
        llm_suggestions <- c(llm_suggestions,
          sprintf("字段 '%s' 无精确匹配，LLM 根据内容推测为列 '%s'（置信度已提升至 MEDIUM）",
                  field, guessed_col))
      } else {
        llm_suggestions <- c(llm_suggestions,
          sprintf("字段 '%s' 无任何匹配列，将使用空值填充，请人工指定", field))
      }
    }

    return(list(
      column_mapping  = column_mapping,
      confidence      = confidence,
      llm_suggestions = llm_suggestions
    ))

  } else {
    # ── 真实 API 模式（接入 LLM 时启用）────────────────────────────────────
    # 构造给 LLM 的 Prompt
    header_str  <- paste(names(df), collapse=", ")
    preview_str <- paste(
      apply(head(df, 5), 1, function(r) paste(r, collapse=" | ")),
      collapse="\n"
    )

    system_msg <- paste0(
      "你是 ADaM 数据标准专家。给定一个 CSV 文件的列名和前几行数据，",
      "识别哪一列对应以下标准字段：variable, label, type, source, derivation, dataset。\n",
      "以 JSON 格式返回，结构为：\n",
      '{ "column_mapping": {"variable":"实际列名",...},',
      '  "confidence": {"variable":"HIGH|MEDIUM|LOW",...},',
      '  "llm_suggestions": ["说明1","说明2"] }'
    )
    user_msg <- paste0(
      "CSV 列名：", header_str, "\n",
      "前5行数据：\n", preview_str
    )

    # httr2 请求（使用 provider_registry，默认 openai）
    prov_cfg <- PROVIDER_REGISTRY[["openai"]]
    api_key  <- Sys.getenv(prov_cfg$api_key_env, "")
    if (nchar(api_key) == 0) {
      # 回退：若无环境变量 Key，使用启发式结果
      return(list(
        column_mapping  = sapply(col_map, function(x) x$matched_col %||% NA_character_),
        confidence      = sapply(col_map, function(x) x$confidence),
        llm_suggestions = "未配置 OPENAI_API_KEY 环境变量，已使用启发式匹配（跳过 LLM 补全）"
      ))
    }

    resp <- httr2::request(prov_cfg$base_url) |>
      httr2::req_headers("Authorization"=paste("Bearer",api_key),
                         "Content-Type"="application/json") |>
      httr2::req_body_json(list(
        model    = "gpt-4o",
        messages = list(list(role="system",content=system_msg),
                        list(role="user",  content=user_msg)),
        temperature     = 0.0,
        response_format = list(type="json_object")
      )) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries=3) |>
      httr2::req_perform()

    raw      <- httr2::resp_body_json(resp)
    raw_text <- raw$choices[[1]]$message$content
    clean    <- stringr::str_replace_all(raw_text, "^```json\\s*|\\s*```$", "")
    jsonlite::fromJSON(clean, simplifyVector=TRUE)
  }
}

# -----------------------------------------------------------------------------
# [新增 A-3] .check_row_completeness()
# 说明：对已映射好的 Spec data.frame 逐行检查，找出 Derivation / Source
#       等关键字段为空的变量，生成行级风险列表。
#
# 参数：df             — data.frame，原始 Spec CSV
#       column_mapping — 标准字段 → 实际列名 的命名字符向量
# 返回：data.frame(variable, field, issue)，每条对应一个潜在问题
# -----------------------------------------------------------------------------
.check_row_completeness <- function(df, column_mapping) {

  risks <- list()

  # 获取实际列名（可能为 NA 表示未匹配到）
  var_col    <- column_mapping["variable"]
  deriv_col  <- column_mapping["derivation"]
  source_col <- column_mapping["source"]

  var_values <- if (!is.na(var_col) && var_col %in% names(df))
                  df[[var_col]] else rep("?", nrow(df))

  # 检查 Derivation 列
  if (!is.na(deriv_col) && deriv_col %in% names(df)) {
    empty_idx <- which(is.na(df[[deriv_col]]) | trimws(df[[deriv_col]]) == "")
    for (i in empty_idx) {
      risks[[length(risks)+1]] <- data.frame(
        level    = "WARNING",
        variable = var_values[i] %||% paste0("第", i, "行"),
        field    = "derivation",
        issue    = "Derivation 字段为空，LLM 将尝试根据变量名推断，建议人工补充",
        stringsAsFactors = FALSE
      )
    }
  }

  # 检查 Source 列
  if (!is.na(source_col) && source_col %in% names(df)) {
    empty_idx <- which(is.na(df[[source_col]]) | trimws(df[[source_col]]) == "")
    for (i in empty_idx) {
      risks[[length(risks)+1]] <- data.frame(
        level    = "INFO",
        variable = var_values[i] %||% paste0("第", i, "行"),
        field    = "source",
        issue    = "Source 字段为空，将标记为来源不明",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(risks) == 0) return(data.frame(level=character(),variable=character(),
                                             field=character(),issue=character()))
  do.call(rbind, risks)
}


# -----------------------------------------------------------------------------
# [A-4a] .spec_parse_content() — shared inner content builder (no Modal wrapper)
# Used by both .build_parse_modal() (single file) and .build_multi_parse_modal() (multi-file)
# -----------------------------------------------------------------------------
.spec_parse_content <- function(parse_result, row_risks, df_preview, n_vars) {

  col_map    <- parse_result$column_mapping
  confidence <- parse_result$confidence
  llm_hints  <- parse_result$llm_suggestions %||% character(0)
  n_low      <- sum(confidence == "LOW",    na.rm = TRUE)
  n_med      <- sum(confidence == "MEDIUM", na.rm = TRUE)

  summary_ui <- div(
    style = "display:flex;gap:1rem;font-size:0.75rem;color:#8b949e;padding:0.5rem 0;",
    span(style="color:#e6edf3;", paste0("共 ", n_vars, " 个变量")),
    span("·"),
    span(style=if(n_low>0)"color:#f85149;" else "color:#3fb950;",
         paste0(6-n_low-n_med, " 字段高置信  ")),
    span(style=if(n_med>0)"color:#d29922;" else "color:#8b949e;",
         paste0(n_med, " 字段中置信  ")),
    span(style=if(n_low>0)"color:#f85149;" else "color:#8b949e;",
         paste0(n_low, " 字段低置信"))
  )

  field_labels <- c(
    variable="变量名 (variable)", label="标签 (label)", type="类型 (type)",
    source="来源域 (source)", derivation="派生逻辑 (derivation)",
    dataset="数据集 (dataset)"
  )
  conf_badge_html <- function(conf) {
    switch(conf,
      "HIGH"   = '<span class="conf-high">● HIGH</span>',
      "MEDIUM" = '<span class="conf-medium">◐ MEDIUM</span>',
      '<span class="conf-low">○ LOW</span>'
    )
  }
  map_rows <- lapply(names(col_map), function(field) {
    actual_col  <- col_map[[field]]
    conf        <- confidence[[field]] %||% "LOW"
    matched_str <- if (is.na(actual_col)) {
      '<span style="color:#6e7681;font-style:italic;">未识别</span>'
    } else {
      sprintf('<code style="color:#2dd4bf;background:rgba(45,212,191,0.1);padding:1px 5px;border-radius:3px;">%s</code>',
              actual_col)
    }
    tags$tr(tags$td(field_labels[[field]] %||% field),
            tags$td(HTML(matched_str)), tags$td(HTML(conf_badge_html(conf))))
  })
  mapping_table <- tags$table(class="parse-map-table",
    tags$thead(tags$tr(tags$th("标准字段"), tags$th("CSV 实际列名"), tags$th("置信度"))),
    tags$tbody(map_rows))

  llm_hint_ui <- if (length(llm_hints) > 0) {
    tagList(
      div(class="modal-section-title", "LLM 智能补全"),
      lapply(llm_hints, function(h)
        div(class="parse-risk-item risk-info",
            div(class="risk-tag","LLM"), div(style="color:#8b949e;", h)))
    )
  } else NULL

  row_risk_ui <- if (!is.null(row_risks) && nrow(row_risks) > 0) {
    n_warn <- sum(row_risks$level == "WARNING")
    n_info <- sum(row_risks$level == "INFO")
    items  <- apply(row_risks, 1, function(r) {
      cls <- if (r["level"]=="WARNING") "parse-risk-item risk-warn" else "parse-risk-item risk-info"
      div(class=cls, div(class="risk-tag", r["level"]),
          div(style="color:#c9d1d9;", tags$strong(r["variable"]), " . ", r["field"],
              " - ", span(style="color:#8b949e;", r["issue"])))
    })
    tagList(
      div(class="modal-section-title",
          paste0("行级风险(", nrow(row_risks), "条)  ",
                 trimws(paste0(if(n_warn>0) paste0(n_warn," WARNING "),
                               if(n_info>0) paste0(n_info," INFO"))))),
      items)
  } else {
    tagList(
      div(class="modal-section-title", "行级检查 OK"),
      div(style="font-size:0.78rem;color:#3fb950;padding:0.3rem 0;",
          "所有变量的 Derivation 和 Source 字段均已填写。")
    )
  }

  preview_rows <- apply(head(df_preview, 3), 1, function(r) {
    tags$tr(lapply(r, function(cell)
      tags$td(if (is.na(cell)||trimws(cell)=="")
              span(style="color:#6e7681;font-style:italic;", "空") else cell)))
  })
  preview_table <- tagList(
    div(class="modal-section-title", "前3行预览"),
    div(class="preview-scroll",
      tags$table(class="preview-table",
        tags$thead(tags$tr(lapply(names(df_preview), tags$th))),
        tags$tbody(preview_rows))))

  low_conf_banner <- if (n_low > 0)
    div(style=paste0("margin-top:0.8rem;padding:0.6rem 0.8rem;",
      "background:rgba(248,81,73,0.08);border:1px solid rgba(248,81,73,0.3);",
      "border-radius:5px;font-size:0.78rem;color:#f85149;"),
      bs_icon("exclamation-triangle", size="0.8rem"),
      paste0(" 存在 ", n_low, " 个无法自动识别的字段。建议修改 CSV 列名后重上传，",
             "或确认后在代码审查阶段手动调整。")
    ) else NULL

  tagList(
    summary_ui,
    div(class="modal-section-title", "列名自动识别结果"),
    mapping_table, llm_hint_ui, row_risk_ui, preview_table, low_conf_banner
  )
}

# -----------------------------------------------------------------------------
# [A-4b] .build_parse_modal() -- single-file Modal wrapper (backward compatible)
# -----------------------------------------------------------------------------
.build_parse_modal <- function(parse_result, row_risks, df_preview, n_vars) {
  modalDialog(
    title     = tagList(bs_icon("file-earmark-bar-graph", size="1rem", color="#2dd4bf"),
                        " Spec CSV 解析报告"),
    size = "l", easyClose = FALSE,
    div(style="max-height:60vh;overflow-y:auto;padding:0.2rem 0.4rem;",
        .spec_parse_content(parse_result, row_risks, df_preview, n_vars)),
    footer = tagList(
      actionButton("btn_reparse_spec",
        tagList(bs_icon("arrow-repeat"), " 取消，重新上传"),
        class = "btn-outline-secondary btn-sm"),
      actionButton("btn_confirm_spec",
        tagList(bs_icon("check-circle-fill"), " 确认解析，继续生成"),
        class = "btn-success")
    )
  )
}

# -----------------------------------------------------------------------------
# [A-4c] .build_multi_parse_modal() -- multi-file Modal
# specs: named list from rv$specs (filename / parse_report / csv_raw / step per entry)
# -----------------------------------------------------------------------------
.build_multi_parse_modal <- function(specs) {
  n_files    <- length(specs)
  n_warn_tot <- .sum_true(lapply(specs, function(s) s$step == "warn"))

  sections <- lapply(names(specs), function(fid) {
    s    <- specs[[fid]]
    step <- s$step %||% "ok"
    badge_sty <- if (step == "warn")
      "background:rgba(210,153,34,0.15);color:#d29922;border:1px solid rgba(210,153,34,0.35);"
    else
      "background:rgba(63,185,80,0.12);color:#3fb950;border:1px solid rgba(63,185,80,0.3);"
    tagList(
      div(style="display:flex;align-items:center;gap:0.5rem;margin:0.8rem 0 0.4rem 0;",
        bs_icon("file-earmark-spreadsheet", size="0.9rem", color="#2dd4bf"),
        div(style="font-family:'Syne',sans-serif;font-size:0.88rem;font-weight:600;color:#e6edf3;flex:1;",
            s$filename),
        span(style=paste0("font-family:'JetBrains Mono',monospace;font-size:0.65rem;",
                          "padding:1px 6px;border-radius:3px;flex-shrink:0;", badge_sty),
             toupper(step))
      ),
      .spec_parse_content(s$parse_report$parse_result, s$parse_report$row_risks,
                          s$csv_raw, nrow(s$csv_raw)),
      hr(style="border-color:#21262d;margin:0.6rem 0 0 0;")
    )
  })

  overview <- div(
    style="display:flex;gap:1rem;align-items:center;font-size:0.75rem;padding:0.4rem 0 0.6rem 0;border-bottom:1px solid #21262d;",
    span(style="color:#e6edf3;font-weight:600;", paste0(n_files, " 个 Spec 文件")),
    span(style="color:#6e7681;", "|"),
    span(style=if(n_warn_tot>0)"color:#d29922;" else "color:#3fb950;",
         if(n_warn_tot>0) paste0(n_warn_tot," 个文件含低置信字段")
         else "所有文件高置信匹配"),
    div(style="margin-left:auto;font-size:0.68rem;color:#6e7681;",
        "确认后全部 Spec 将合并送入 LLM")
  )

  modalDialog(
    title = tagList(bs_icon("file-earmark-bar-graph", size="1rem", color="#2dd4bf"),
                    paste0(" Spec CSV 解析报告（", n_files, " 个文件）")),
    size = "l", easyClose = FALSE,
    div(style="max-height:65vh;overflow-y:auto;padding:0.2rem 0.4rem;",
        overview, sections),
    footer = tagList(
      actionButton("btn_reparse_spec",
        tagList(bs_icon("arrow-repeat"), " 取消，重新上传"),
        class = "btn-outline-secondary btn-sm"),
      actionButton("btn_confirm_spec",
        tagList(bs_icon("check-circle-fill"),
                paste0(" 确认全部 ", n_files, " 个 Spec，继续生成")),
        class = "btn-success")
    )
  )
}


# =============================================================================
# server 函数主体
# =============================================================================
server <- function(input, output, session) {

  # ===========================================================================
  # 响应式状态池
  # ===========================================================================
  rv <- reactiveValues(
    # SDTM 数据
    sdtm          = NULL,
    sdtm_profile  = NULL,
    # LLM 生成结果
    llm_result    = NULL,
    llm_request_meta = NULL,
    llm_cache     = list(),
    derivation_plan = NULL,
    derivation_plan_issues_df = NULL,
    static_check_result = NULL,
    session_api_key = NULL,
    session_failover_api_key = NULL,
    risk_logs_df  = NULL,
    original_code = NULL,
    # ADaM 输出
    adsl          = NULL,
    adae          = NULL,
    validation_result   = NULL,
    validation_issues_df = NULL,
    validation_stats_df  = NULL,
    # 流水线状态
    step_load     = "idle",
    step_llm      = "idle",
    step_review   = "idle",
    step_run      = "idle",
    step_validate = "idle",
    # 日志
    log_lines     = character(0),
    # 运行代码状态
    run_result_ok  = NULL,
    run_result_err = NULL,

    # ── 多 Spec 文件解析状态 ──────────────────────────────────────────────
    # specs: 命名列表，key = safe_spec_id（由文件名派生）
    # 每个条目：list(
    #   file_id, filename, size, upload_time,
    #   csv_raw      — 原始 data.frame,
    #   parse_report — list(parse_result, row_risks),
    #   step         — "parsing"|"ok"|"warn"|"error",
    #   parsed       — list(dataset, variables)，确认后填充
    # )
    specs          = list(),
    spec_confirmed = FALSE,    # 全部确认后为 TRUE
    step_parse     = "idle",   # 聚合状态："idle"|"parsing"|"ok"|"warn"|"error"

    # ── [S-2] ADaM 输出（多数据集，动态）────────────────────────────────
    adam_datasets  = list(),          # 所有生成的 ADaM data.frame，键名为数据集名

    # ── [S-2] 当前激活的 SDTM 域（供上传面板和 uploaded_files_list 使用）──
    active_domains = c("dm","ex","ae"),

    # ── SDTM 文件元数据（动态，任意数量域）──────────────────────────────
    # 格式：list(name, size, upload_time, datapath, rows, cols)
    file_meta      = list()
  )

  # 日志追加
  .append_log <- function(..., icon="›") {
    rv$log_lines <- c(rv$log_lines, .log_line(..., icon=icon))
  }

  # ── 文件元数据辅助函数 ──────────────────────────────────────────────────
  .make_file_meta <- function(fi) list(
    name        = fi$name,
    size        = fi$size,
    upload_time = Sys.time(),
    datapath    = fi$datapath,
    rows        = NA_integer_,
    cols        = NA_integer_,
    preview     = NULL
  )

  .fmt_size <- function(b) {
    if (is.na(b) || b == 0) return("—")
    if (b < 1024)    return(paste0(b, " B"))
    if (b < 1048576) return(paste0(round(b / 1024, 1), " KB"))
    paste0(round(b / 1048576, 1), " MB")
  }

  .workflow_snapshot <- function() {
    req_domains <- .get_required_domain_ids()
    active_domains <- rv$active_domains %||% names(SDTM_DOMAIN_REGISTRY)
    n_uploaded <- .sum_true(lapply(rv$file_meta, Negate(is.null)))
    n_required_ok <- .sum_true(lapply(req_domains, function(sid) !is.null(rv$file_meta[[sid]])))
    n_active_ok <- .sum_true(lapply(active_domains, function(sid) !is.null(rv$file_meta[[sid]])))
    model_sel <- input$llm_model %||% "gpt-4o"
    prov <- .infer_provider(model_sel)
    cfg <- tryCatch(.get_provider_cfg(prov), error = function(e) NULL)
    api_key_val <- .non_empty(input$api_key %||% rv$session_api_key %||% "")
    has_key <- isTRUE(MOCK_MODE) || is.null(cfg) || !isTRUE(cfg$needs_key) ||
      !is.null(api_key_val)
    list(
      n_uploaded = n_uploaded,
      n_required_ok = n_required_ok,
      n_required = length(req_domains),
      n_active_ok = n_active_ok,
      n_active = length(active_domains),
      has_specs = length(rv$specs) > 0,
      spec_confirmed = isTRUE(rv$spec_confirmed),
      has_key = has_key,
      has_llm = !is.null(rv$llm_result),
      has_output = length(rv$adam_datasets) > 0
    )
  }

  # 从文件名生成安全 ID（去扩展名 + 非字母数字替换为下划线）
  .safe_spec_id <- function(filename) {
    base <- tools::file_path_sans_ext(filename)
    gsub("[^a-zA-Z0-9]", "_", base)
  }

  # ===========================================================================
  # [S-3] Observer：上传 SDTM 文件 → 更新文件元数据（覆盖全部注册域）
  # ===========================================================================
  for (.sid in names(SDTM_DOMAIN_REGISTRY)) {
    local({
      sid <- .sid
      inp <- paste0("file_", sid)
      observeEvent(input[[inp]], {
        fi <- input[[inp]]
        req(fi)
        meta <- .make_file_meta(fi)
        # Peek 行列数
        df_peek <- tryCatch(
          read.csv(fi$datapath, nrows=5, header=TRUE, stringsAsFactors=FALSE),
          error=function(e) NULL
        )
        if (!is.null(df_peek)) {
          # 获取完整行数
          full_count <- tryCatch({
            df_full <- read.csv(fi$datapath, header=TRUE, stringsAsFactors=FALSE)
            nrow(df_full)
          }, error=function(e) NA_integer_)
          meta$rows <- full_count
          meta$cols <- ncol(df_peek)
          meta$preview <- utils::head(df_peek, 4)
        }
        rv$file_meta[[sid]] <- meta
      })
    })
  }

  # [S-3] Observer：域选择复选框变化 → 更新 rv$active_domains
  observe({
    required       <- .get_required_domain_ids()   # dm, ex（始终激活）
    sel_core     <- input$sdtm_domains_core     %||% character(0)
    sel_basic    <- input$sdtm_domains_basic    %||% character(0)
    sel_extended <- input$sdtm_domains_extended %||% character(0)
    rv$active_domains <- unique(c(required, sel_core, sel_basic, sel_extended))
  })

  # ===========================================================================
  # Observer：上传 Spec CSV → 自动解析 → 弹出报告 Modal
  # ===========================================================================
  # ===========================================================================
  # Observer: upload Spec CSV(s) -> parse each -> show combined Modal
  # input$file_spec is a data.frame when multiple=TRUE (one row per file)
  # ===========================================================================
  observeEvent(input$file_spec, {
    req(input$file_spec)
    rv$spec_confirmed <- FALSE
    rv$step_parse     <- "parsing"

    fi_df <- input$file_spec  # data.frame: name, size, type, datapath

    new_specs <- list()
    for (i in seq_len(nrow(fi_df))) {
      fname    <- fi_df$name[i]
      fsize    <- fi_df$size[i]
      fpath    <- fi_df$datapath[i]
      file_id  <- .safe_spec_id(fname)

      df <- tryCatch({
        read.csv(fpath, header=TRUE, stringsAsFactors=FALSE,
                 na.strings=c("","NA","N/A"))
      }, error = function(e) {
        showNotification(
          paste0("Spec CSV [", fname, "] 读取失败：", conditionMessage(e)),
          type="error", duration=8)
        NULL
      })
      if (is.null(df) || ncol(df) == 0) next

      col_map      <- .heuristic_col_match(names(df))
      parse_result <- tryCatch(
        .call_llm_spec_parser(df, col_map),
        error = function(e) list(
          column_mapping  = sapply(col_map, function(x) x$matched_col %||% NA_character_),
          confidence      = sapply(col_map, function(x) x$confidence),
          llm_suggestions = paste0("LLM调用失败，已用启发式匹配：", conditionMessage(e)))
      )
      row_risks <- .check_row_completeness(df, parse_result$column_mapping)
      n_low     <- sum(parse_result$confidence == "LOW", na.rm=TRUE)

      new_specs[[file_id]] <- list(
        file_id      = file_id,
        filename     = fname,
        size         = fsize,
        upload_time  = Sys.time(),
        csv_raw      = df,
        parse_report = list(parse_result=parse_result, row_risks=row_risks),
        step         = if (n_low > 0) "warn" else "ok",
        parsed       = NULL
      )
    }

    if (length(new_specs) == 0) { rv$step_parse <- "error"; return() }

    # Merge into rv$specs (new uploads add to or replace existing entries)
    for (fid in names(new_specs)) rv$specs[[fid]] <- new_specs[[fid]]

    # Aggregate step_parse across all files
    all_steps     <- sapply(rv$specs, function(s) s$step)
    rv$step_parse <- if (any(all_steps == "error"))  "error"
                     else if (any(all_steps == "warn")) "warn"
                     else "ok"

    showModal(.build_multi_parse_modal(rv$specs))
  })

  # ===========================================================================
  # Observer: user clicks "Confirm All Specs"
  # ===========================================================================
  observeEvent(input$btn_confirm_spec, {
    req(length(rv$specs) > 0)

    for (fid in names(rv$specs)) {
      entry        <- rv$specs[[fid]]
      parse_result <- entry$parse_report$parse_result
      col_map      <- parse_result$column_mapping
      df           <- entry$csv_raw

      safe_col <- function(field) {
        col <- col_map[[field]]
        if (!is.na(col) && col %in% names(df)) df[[col]]
        else rep(NA_character_, nrow(df))
      }

      dataset_col  <- col_map[["dataset"]]
      dataset_name <- if (!is.na(dataset_col) && dataset_col %in% names(df)) {
        vals <- unique(na.omit(df[[dataset_col]]))
        if (length(vals) == 1) vals[1] else paste(vals, collapse="/")
      } else tools::file_path_sans_ext(entry$filename)

      rv$specs[[fid]]$parsed <- list(
        dataset   = dataset_name,
        variables = data.frame(
          variable   = safe_col("variable"),
          label      = safe_col("label"),
          type       = safe_col("type"),
          source     = safe_col("source"),
          derivation = safe_col("derivation"),
          stringsAsFactors = FALSE
        )
      )
    }

    rv$spec_confirmed <- TRUE
    removeModal()

    total_vars <- .sum_int(rv$specs, function(s) {
      if (!is.null(s$parsed)) nrow(s$parsed$variables) else 0L
    })
    showNotification(
      tagList(tags$strong("Spec 解析已确认"),
              tags$br(),
              paste0(length(rv$specs), " 个文件  共 ", total_vars, " 个变量")),
      type="message", duration=4)
  })

  # ===========================================================================
  # Observer: user cancels -> clear all specs, reset to idle
  # ===========================================================================
  observeEvent(input$btn_reparse_spec, {
    rv$specs          <- list()
    rv$spec_confirmed <- FALSE
    rv$step_parse     <- "idle"
    removeModal()
    showNotification("已取消。请重新上传 Spec CSV 文件。",
                     type="warning", duration=4)
  })

  # ===========================================================================
  # Output: sidebar Spec parse-status card (multi-file aware)
  # ===========================================================================
  output$spec_parse_status <- renderUI({
    step    <- rv$step_parse
    n_files <- length(rv$specs)
    if (step == "idle" && n_files == 0) return(NULL)

    cfg <- if (rv$spec_confirmed) {
      total_vars <- .sum_int(rv$specs, function(s) {
        if (!is.null(s$parsed)) nrow(s$parsed$variables) else 0L
      })
      list(dot="dot-ok",
           text=paste0("已确认 · ", n_files, " 个文件 · ",
                       total_vars, " 个变量"))
    } else {
      switch(step,
        "parsing" = list(dot="dot-parsing", text="正在解析..."),
        "ok"      = list(dot="dot-ok",
                         text=paste0(n_files, " 个文件解析完成，等待确认")),
        "warn"    = list(dot="dot-warn",
                         text=paste0(n_files, " 个文件已解析，存在低置信字段")),
        "error"   = list(dot="dot-error",
                         text="解析失败，请检查 CSV 格式"),
        list(dot="dot-idle", text="")
      )
    }

    div(class="spec-status-card",
      div(class="status-row",
        div(class=paste("dot", cfg$dot)),
        div(class="status-text", cfg$text)
      ),
      if (n_files > 0 && step != "parsing") {
        div(class="reopen-link",
            onclick="Shiny.setInputValue('btn_reopen_report', Math.random());",
            "查看解析报告 →")
      }
    )
  })

  # Reopen parse report Modal (multi-file)
  observeEvent(input$btn_reopen_report, {
    req(length(rv$specs) > 0)
    showModal(.build_multi_parse_modal(rv$specs))
  })

  # ===========================================================================
  # [修改 G] Observer：「生成 ADaM 与代码」按钮
  #   改动 1：校验条件改为检查 rv$spec_confirmed（原：input$file_spec 不为 NULL）
  #   改动 2：Spec 加载改为使用 rv$spec_parsed（原：load_spec_json()）
  # ===========================================================================
  observeEvent(input$btn_generate, {

    rv$adsl <- rv$adae <- rv$llm_result <- rv$risk_logs_df <- NULL
    rv$sdtm_profile <- NULL
    rv$llm_request_meta <- NULL
    rv$derivation_plan <- rv$derivation_plan_issues_df <- rv$static_check_result <- NULL
    rv$validation_result <- rv$validation_issues_df <- rv$validation_stats_df <- NULL
    rv$original_code <- rv$run_result_ok <- rv$run_result_err <- NULL
    rv$log_lines <- character(0)
    rv$step_load <- "running"
    rv$step_llm  <- rv$step_review <- rv$step_run <- rv$step_validate <- "idle"

    # ── [S-4] 校验：动态推断所需域 + spec_confirmed 检查 ──────────────────────
    needed_domains <- unique(c(
      .get_required_domain_ids(),
      if (isTRUE(rv$spec_confirmed)) infer_required_domains_from_spec(rv$specs)
      else character(0)
    ))
    missing_domains <- needed_domains[
      sapply(needed_domains, function(sid) is.null(rv$file_meta[[sid]]))
    ]
    missing_files <- c(
      if (length(missing_domains) > 0)
        paste0("SDTM 域文件缺失：", paste(toupper(missing_domains), collapse=", ")),
      if (!isTRUE(rv$spec_confirmed))
        "Analysis Spec（请上传 CSV 并确认解析报告）"
    )

    if (length(missing_files) > 0) {
      rv$step_load <- "error"
      .append_log("前置条件不满足：", paste(missing_files, collapse="、"), icon="✖")
      showNotification(
        tagList(tags$strong("⚠ 请先完成以下步骤"),
                tags$br(), paste(missing_files, collapse="、")),
        type="error", duration=7)
      shinyjs::runjs("adamProgress.error('✖ 前置条件不满足')")
      return()
    }

    # ── [S-4] 阶段 1：读取已上传的所有 SDTM 域 ────────────────────────────────
    .append_log("开始读取 SDTM 文件...", icon="⬤")
    valid_meta <- Filter(Negate(is.null), rv$file_meta)
    domain_paths <- setNames(
      sapply(names(valid_meta), function(sid) valid_meta[[sid]]$datapath),
      names(valid_meta)
    )
    sdtm_data <- tryCatch({
      load_sdtm_data(domain_paths)
    }, error = function(e) {
      rv$step_load <- "error"
      .append_log("读取 SDTM 失败：", conditionMessage(e), icon="✖")
      showNotification(paste0("SDTM 读取错误：", conditionMessage(e)), type="error", duration=8)
      NULL
    })
    if (is.null(sdtm_data)) {
      shinyjs::runjs("adamProgress.error('✖ SDTM 文件读取失败')")
      return()
    }

    rv$sdtm      <- sdtm_data
    rv$sdtm_profile <- profile_sdtm_domains(sdtm_data)
    rv$step_load <- "done"
    .append_log(paste0("SDTM 加载完成  ",
      paste(sapply(names(sdtm_data), function(sid)
        paste0(toupper(sid), "=", nrow(sdtm_data[[sid]]), "行")), collapse="  ")), icon="✔")
    .append_log("已生成结构化 SDTM profile，用于 LLM 规划与输入画像展示。", icon="🧱")

    # ── [修改 G-2] 阶段 2：直接使用 rv$spec_parsed（原：load_spec_json()）──
    .append_log("加载已确认的 Spec 解析结果...", icon="⬤")
    spec_map <- .split_specs_by_dataset(rv$specs)
    total_vars <- .sum_int(spec_map, function(s) {
      vars <- s$variables %||% NULL
      if (!is.null(vars) && is.data.frame(vars)) nrow(vars) else 0L
    })
    .append_log(sprintf("规格加载完成  %d 个数据集  共 %d 个变量",
                        length(spec_map), total_vars), icon="✔")

    # ── [S-4] 提取目标数据集列表（用于分批 LLM prompt 和结果提取）──────────────
    target_datasets <- names(spec_map)
    if (length(target_datasets) == 0) target_datasets <- c("adsl", "adae")

    # ── 阶段 3：调用 LLM 生成代码 ─────────────────────────────────────────────
    rv$step_llm <- "running"

    # ── 从 UI 获取模型/Key/本地配置，构建故障转移调用链 ─────────────────────
    model_val    <- input$llm_model %||% "gpt-4o"
    primary_prov <- .infer_provider(model_val)
    actual_model <- if (primary_prov %in% c("ollama", "vllm"))
                      trimws(input$local_model_name %||% "") else model_val
    api_key_val  <- .non_empty(input$api_key %||% rv$session_api_key %||% "") %||% ""
    prompt_profile <- list(
      mode         = input$llm_generation_mode %||% "balanced",
      traceability = isTRUE(input$llm_traceability_mode),
      preview_rows = suppressWarnings(as.integer(input$llm_preview_rows %||% 5L))
    )

    # 云端提供商需要 Key；本地不需要
    prov_cfg <- .get_provider_cfg(primary_prov)
    if (prov_cfg$needs_key && nchar(api_key_val) == 0 && !isTRUE(MOCK_MODE)) {
      rv$step_llm <- "error"
      .append_log("未填写 API Key，无法调用 LLM", icon="✖")
      showNotification(
        tagList(tags$strong("⚠ 请填写 API Key"),
                tags$br(), "在侧边栏「LLM API 配置」中输入有效的 API Key 后重试。"),
        type="error", duration=7)
      shinyjs::runjs("adamProgress.error('✖ 未配置 API Key')")
      return()
    }

    provider_key_map <- setNames(list(api_key_val), primary_prov)
    base_url_map     <- if (primary_prov %in% c("ollama", "vllm"))
                          setNames(list(trimws(input$local_base_url %||% "")), primary_prov)
                        else list()
    failover_chain   <- list(list(provider = primary_prov, model = actual_model))

    # 故障转移备用链路
    if (isTRUE(input$enable_failover) &&
        !is.null(input$failover_provider_1) &&
        input$failover_provider_1 != "none") {
      fb <- input$failover_provider_1
      provider_key_map[[fb]] <- .non_empty(input$failover_api_key_1 %||% rv$session_failover_api_key %||% "") %||% ""
      failover_chain <- c(failover_chain,
                          list(list(provider = fb, model = .default_model(fb))))
    }

    .append_log("正在调用 LLM 引擎（",
                if (MOCK_MODE) "模拟模式" else paste0(actual_model, " · ", primary_prov),
                "）...", icon="⬤")
    .append_log("生成策略：",
                switch(prompt_profile$mode,
                  "strict"   = "稳健优先",
                  "adaptive" = "补全优先",
                  "平衡模式"),
                " / 追踪=",
                if (isTRUE(prompt_profile$traceability)) "增强" else "标准",
                " / 默认预览=",
                prompt_profile$preview_rows,
                "行",
                icon="⚙")

    profile_summary_txt <- format_sdtm_profiles(rv$sdtm_profile)
    large_spec_threshold <- 18L
    batch_results <- vector("list", length(target_datasets))
    names(batch_results) <- target_datasets
    compact_datasets <- character(0)
    staged_datasets <- character(0)
    cached_datasets <- character(0)
    project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    pending_jobs <- list()
    pending_meta <- list()

    llm_res <- tryCatch({
      for (ds in target_datasets) {
        ds_spec <- spec_map[[ds]] %||% list(dataset = ds, variables = data.frame(stringsAsFactors = FALSE))
        ds_spec <- .compact_spec_payload(ds_spec)
        ds_vars <- ds_spec$variables %||% data.frame(stringsAsFactors = FALSE)
        ds_var_n <- if (is.data.frame(ds_vars)) nrow(ds_vars) else 0L
        prompt_profile_ds <- prompt_profile
        sdtm_prompt_data <- sdtm_data
        staged_mode <- FALSE

        if (ds_var_n >= large_spec_threshold) {
          prompt_profile_ds$preview_rows <- min(prompt_profile_ds$preview_rows %||% 5L, 3L)
          prompt_profile_ds$compact_mode <- TRUE
          prompt_profile_ds$request_timeout <- 180L
          prompt_profile_ds$max_tokens <- 2000L
          sdtm_prompt_data <- NULL
          compact_datasets <- c(compact_datasets, ds)
          staged_mode <- TRUE
          staged_datasets <- c(staged_datasets, ds)
          .append_log(sprintf("%s 规格较长（%d变量），已启用两段式压缩生成。", toupper(ds), ds_var_n), icon="⇢")
        }

        .append_log(sprintf("开始生成 %s（%d/%d，%d 个变量）...",
                            toupper(ds),
                            match(ds, target_datasets),
                            length(target_datasets),
                            ds_var_n), icon="⬤")

        if (staged_mode) {
          .append_log(sprintf("%s 第一步：使用系统计划（基于已确认 Spec）...", toupper(ds)), icon="⋯")
          plan_norm <- normalize_derivation_plan(NULL, list(list(parsed = ds_spec)), ds)
          plan_payload <- .compact_plan_payload(plan_norm)
          code_profile <- prompt_profile_ds
          code_profile$task <- "code_from_plan"
          code_profile$request_timeout <- 240L
          code_profile$max_tokens <- 2200L
          payload_json <- jsonlite::toJSON(plan_payload, pretty = TRUE, auto_unbox = TRUE)
          cache_key <- .make_llm_cache_key(ds, payload_json, profile_summary_txt, actual_model, primary_prov, code_profile)
          if (!is.null(rv$llm_cache[[cache_key]])) {
            batch_results[[ds]] <- list(
              derivation_plan = plan_norm,
              r_code = rv$llm_cache[[cache_key]]$r_code %||% "",
              risk_logs = c(list(list(
                level = "INFO",
                variable = toupper(ds),
                description = "命中会话级 LLM 缓存，已复用上次生成代码。",
                assumption = "当前生成条件与上次一致；若已变更输入或模型，请重新生成。"
              )), rv$llm_cache[[cache_key]]$risk_logs %||% list()),
              token_info = list(input = 0L, output = 0L, total = 0L)
            )
            cached_datasets <- c(cached_datasets, ds)
            .append_log(sprintf("%s 命中缓存，已跳过模型调用。", toupper(ds)), icon="⚡")
          } else {
            .append_log(sprintf("%s 第二步：基于 plan 生成代码...", toupper(ds)), icon="⋯")
            pending_jobs[[ds]] <- list(
              payload_json = payload_json,
              data_summary = profile_summary_txt,
              provider_key_map = provider_key_map,
              failover_chain = failover_chain,
              sdtm_list = NULL,
              base_url_map = base_url_map,
              target_dataset = ds,
              prompt_profile = code_profile
            )
            pending_meta[[ds]] <- list(
              cache_key = cache_key,
              derivation_plan = plan_norm,
              staged_mode = TRUE
            )
          }
        } else {
          ds_spec_json <- jsonlite::toJSON(list(ds_spec), pretty = TRUE, auto_unbox = TRUE)
          cache_key <- .make_llm_cache_key(ds, ds_spec_json, profile_summary_txt, actual_model, primary_prov, prompt_profile_ds)
          if (!is.null(rv$llm_cache[[cache_key]])) {
            batch_results[[ds]] <- rv$llm_cache[[cache_key]]
            cached_datasets <- c(cached_datasets, ds)
            .append_log(sprintf("%s 命中缓存，已跳过模型调用。", toupper(ds)), icon="⚡")
          } else {
            pending_jobs[[ds]] <- list(
              payload_json = ds_spec_json,
              data_summary = profile_summary_txt,
              provider_key_map = provider_key_map,
              failover_chain = failover_chain,
              sdtm_list = sdtm_prompt_data,
              base_url_map = base_url_map,
              target_dataset = ds,
              prompt_profile = prompt_profile_ds
            )
            pending_meta[[ds]] <- list(
              cache_key = cache_key,
              derivation_plan = NULL,
              staged_mode = FALSE
            )
          }
        }
      }

      if (length(pending_jobs) > 0) {
        if (length(pending_jobs) > 1) {
          .append_log(sprintf("并行调用 %d 个数据集的代码生成任务...", length(pending_jobs)), icon="⇄")
        }
        pending_results <- .run_llm_jobs(pending_jobs, project_dir)
        for (ds in names(pending_results)) {
          res <- pending_results[[ds]]
          meta_ds <- pending_meta[[ds]]
          if (is.null(res)) {
            stop(sprintf("%s 生成失败：未返回结果", toupper(ds)), call. = FALSE)
          }
          if (isTRUE(meta_ds$staged_mode)) {
            batch_results[[ds]] <- list(
              derivation_plan = meta_ds$derivation_plan,
              r_code = res$r_code %||% "",
              risk_logs = c(list(list(
                level = "INFO",
                variable = toupper(ds),
                description = "长规格模式已直接采用系统计划，跳过模型 plan-only 阶段。",
                assumption = "当前 derivation plan 主要来自已确认 Spec；复杂连接或业务规则需结合代码和风险日志复核。"
              )), res$risk_logs %||% list()),
              token_info = res$token_info %||% list(input = 0L, output = 0L, total = 0L)
            )
          } else {
            batch_results[[ds]] <- res
          }
          rv$llm_cache[[meta_ds$cache_key]] <- batch_results[[ds]]
          batch_tok <- batch_results[[ds]]$token_info %||% list(total = 0L)
          .append_log(sprintf("%s 生成完成  代码=%d字符  token=%d",
                              toupper(ds),
                              nchar(batch_results[[ds]]$r_code %||% ""),
                              batch_tok$total %||% 0L), icon="✔")
        }
      }

      combined_plan <- list(
        plan_version = "0.2-batched",
        generated_by = "llm-batched",
        datasets = unlist(lapply(batch_results, function(x) {
          (x$derivation_plan %||% list(datasets = list()))$datasets %||% list()
        }), recursive = FALSE, use.names = FALSE)
      )

      list(
        derivation_plan = combined_plan,
        r_code = paste(Filter(nzchar, vapply(batch_results, function(x) x$r_code %||% "", character(1))), collapse = "\n\n"),
        risk_logs = unlist(lapply(batch_results, function(x) x$risk_logs %||% list()), recursive = FALSE, use.names = FALSE),
        token_info = .merge_token_info(batch_results),
        batch_results = batch_results
      )
    }, error = function(e) {
      rv$step_llm <- "error"
      .append_log("LLM 调用失败：", conditionMessage(e), icon="✖")
      showNotification(paste0("LLM 错误：", conditionMessage(e)), type="error", duration=8)
      shinyjs::runjs("adamProgress.error('✖ API 调用失败')")
      NULL
    })
    if (is.null(llm_res)) return()

    rv$llm_result    <- llm_res
    rv$derivation_plan <- normalize_derivation_plan(llm_res$derivation_plan %||% NULL, rv$specs, target_datasets)
    rv$derivation_plan_issues_df <- validate_plan_against_spec(rv$derivation_plan, rv$specs)
    rv$llm_request_meta <- list(
      provider        = primary_prov,
      provider_name   = prov_cfg$name %||% primary_prov,
      model           = actual_model,
      failover        = isTRUE(input$enable_failover) &&
                        !is.null(input$failover_provider_1) &&
                        input$failover_provider_1 != "none",
      generation_mode = prompt_profile$mode,
      traceability    = isTRUE(prompt_profile$traceability),
      preview_rows    = prompt_profile$preview_rows,
      batch_mode      = length(target_datasets) > 1,
      batch_count     = length(target_datasets),
      compact_datasets = unique(compact_datasets),
      staged_datasets = unique(staged_datasets),
      cached_datasets = unique(cached_datasets),
      token_info      = llm_res$token_info %||% list(input=0L, output=0L, total=0L)
    )
    rv$step_llm      <- "done"
    rv$risk_logs_df  <- .normalize_risk_logs(llm_res$risk_logs)
    rv$original_code <- llm_res$r_code

    n_risks <- if (!is.null(rv$risk_logs_df)) nrow(rv$risk_logs_df) else 0
    plan_summary_df <- summarize_derivation_plan(rv$derivation_plan)
    plan_ds_n <- nrow(plan_summary_df)
    plan_var_n <- sum(plan_summary_df$variables %||% 0L)
    .append_log(sprintf("LLM 返回完成  代码长度=%d字符  风险点=%d条",
                        nchar(llm_res$r_code %||% ""), n_risks), icon="✔")
    .append_log(sprintf("分批生成完成  %d 个数据集%s",
                        length(target_datasets),
                        if (length(compact_datasets) > 0) {
                          paste0("；压缩上下文=", paste(toupper(unique(compact_datasets)), collapse = ", "))
                        } else ""), icon="🧩")
    if (length(staged_datasets) > 0) {
      .append_log(sprintf("两段式生成已应用于：%s",
                          paste(toupper(unique(staged_datasets)), collapse = ", ")), icon="⏱")
    }
    if (length(cached_datasets) > 0) {
      .append_log(sprintf("缓存复用于：%s",
                          paste(toupper(unique(cached_datasets)), collapse = ", ")), icon="⚡")
    }
    .append_log(sprintf("Derivation Plan 已生成  %d 个数据集  %d 个变量步骤",
                        plan_ds_n, plan_var_n), icon="🧭")
    if (!is.null(rv$derivation_plan_issues_df) && nrow(rv$derivation_plan_issues_df) > 0) {
      .append_log(sprintf("Plan 与 Spec 存在 %d 条对齐提醒", nrow(rv$derivation_plan_issues_df)), icon="⚠")
    }
    tok        <- llm_res$token_info %||% list(input=0L, output=0L, total=0L)
    shinyjs::runjs(sprintf(
      "adamProgress.complete('✔ LLM 生成完成，识别 %d 条风险点', %d, %d)",
      n_risks, tok$input %||% 0L, tok$output %||% 0L
    ))

    updateAceEditor(session, "code_editor", value=llm_res$r_code)
    rv$step_review <- "running"
    .append_log("代码已填入编辑器，请切换至「生成与审阅」页检查。", icon="→")

    nav_select(id = "main_tabs", selected = "tab_generate", session = session)
    showNotification(
      tagList(tags$strong("✔ LLM 生成完成"), tags$br(),
              paste0("识别到 ", n_risks, " 条风险点，请继续在“生成与审阅”页检查代码")),
      type="message", duration=5)
  })

  # ===========================================================================
  # [新增] Observer：「清空上传 (Clear Uploads)」按钮
  # 职责：
  #   1. shinyjs::reset() 重置四个 fileInput 的前端显示（文件名、进度条）
  #   2. 清空 rv 中所有与上传文件相关的后台状态
  #   3. 因为 rv$sdtm 等被置 NULL，output$uploaded_files_list 会自动重新渲染为空
  # ===========================================================================
  observeEvent(input$btn_clear_uploads, {

    # ── 1. [S-7] 重置所有注册域的 fileInput 控件 ─────────────────────────────
    for (sid in names(SDTM_DOMAIN_REGISTRY)) shinyjs::reset(paste0("file_", sid))
    shinyjs::reset("file_spec")

    # ── 2. 清空所有后台状态变量 ───────────────────────────────────────────────
    rv$file_meta      <- list()   # [S-7] 改为空列表（动态域）
    rv$specs          <- list()
    rv$spec_confirmed <- FALSE
    rv$step_parse     <- "idle"
    rv$sdtm           <- NULL
    rv$sdtm_profile   <- NULL
    rv$step_load      <- "idle"

    # [S-7] 清空多数据集容器
    rv$adam_datasets  <- list()
    rv$adsl           <- NULL
    rv$adae           <- NULL
    rv$derivation_plan <- NULL
    rv$derivation_plan_issues_df <- NULL
    rv$static_check_result <- NULL
    rv$validation_result   <- NULL
    rv$validation_issues_df <- NULL
    rv$validation_stats_df  <- NULL
    rv$llm_result     <- NULL
    rv$llm_request_meta <- NULL
    rv$risk_logs_df   <- NULL
    rv$original_code  <- NULL
    rv$run_result_ok  <- NULL
    rv$run_result_err <- NULL
    rv$log_lines      <- character(0)
    rv$step_validate  <- "idle"

    # ── 3. 重置 Ace 编辑器为初始提示文字 ────────────────────────────────────
    updateAceEditor(session, "code_editor",
                    value = "# 请先上传文件并点击「生成 ADaM 与代码」...")

    showNotification(
      tagList(tags$strong("✔ 已清空所有上传文件"),
              tags$br(),
              "所有 SDTM 文件和 Spec 规范已重置，可重新上传。"),
      type     = "message",
      duration = 4
    )
  })

  # ===========================================================================
  # Output：API 配置面板（动态渲染，根据选中提供商切换内容）
  # ===========================================================================
  observeEvent(input$api_key, {
    rv$session_api_key <- .non_empty(input$api_key)
  }, ignoreNULL = FALSE)

  observeEvent(input$failover_api_key_1, {
    rv$session_failover_api_key <- .non_empty(input$failover_api_key_1)
  }, ignoreNULL = FALSE)

  output$api_config_panel <- renderUI({
    model_sel    <- input$llm_model %||% "gpt-4o"
    prov         <- .infer_provider(model_sel)
    cfg          <- tryCatch(.get_provider_cfg(prov), error=function(e) NULL)
    needs_key    <- is.null(cfg) || isTRUE(cfg$needs_key)
    needs_url    <- !is.null(cfg) && isTRUE(cfg$needs_url)
    prov_name    <- if (!is.null(cfg)) cfg$name else prov

    div(class = "api-config-section",
      # 模型版本下拉（分组）
      selectInput("llm_model", "模型版本",
        choices = list(
          "─ OpenAI ─"   = c("GPT-4o"="gpt-4o","GPT-4o mini"="gpt-4o-mini","GPT-4 Turbo"="gpt-4-turbo"),
          "─ Anthropic ─"= c("Claude Sonnet 4.5"="claude-sonnet-4-5","Claude Opus 4.5"="claude-opus-4-5"),
          "─ DeepSeek ─" = c("DeepSeek Chat"="deepseek-chat","DeepSeek Reasoner"="deepseek-reasoner"),
          "─ Kimi ─"     = c("moonshot-v1-8k"="moonshot-v1-8k","moonshot-v1-32k"="moonshot-v1-32k"),
          "─ Qwen ─"     = c("qwen-max"="qwen-max","qwen-plus"="qwen-plus"),
          "─ 本地推理 ─" = c("Ollama (本地)"="ollama:local","vLLM (本地)"="vllm:local")
        ),
        selected = model_sel
      ),

      div(class = "llm-tuning-section",
        div(class="hint-text", style="margin-top:-0.15rem;margin-bottom:0.45rem;",
            "调节 AI 的推断保守性、风险追踪力度和上下文密度"),
        selectInput("llm_generation_mode", "生成策略",
          choices = c(
            "平衡模式" = "balanced",
            "稳健优先" = "strict",
            "补全优先" = "adaptive"
          ),
          selected = input$llm_generation_mode %||% "balanced"
        ),
        checkboxInput("llm_traceability_mode", "强化风险追踪与代码可读性",
                      value = isTRUE(input$llm_traceability_mode %||% TRUE)),
        selectInput("llm_preview_rows", "每域样本预览",
          choices = c("轻量 3 行" = 3, "标准 5 行" = 5, "详细 8 行" = 8),
          selected = as.character(input$llm_preview_rows %||% 5)
        )
      ),

      # 云端提供商：API Key 输入框
      if (needs_key) {
        tagList(
          div(class = "password-field-shell",
            tags$label(`for` = "api_key", class = "password-field-label", paste0(prov_name, " API Key")),
            div(class = "password-field-wrap",
              tags$input(
                id = "api_key",
                type = "password",
                class = "form-control",
                value = rv$session_api_key %||% "",
                placeholder = "sk-...",
                autocomplete = "off"
              ),
              tags$button(
                type = "button",
                class = "password-toggle-btn",
                onclick = "togglePasswordVisibility('api_key', this);",
                tags$span(class = "icon-show", bs_icon("eye", size = "0.9rem")),
                tags$span(class = "icon-hide", bs_icon("eye-slash", size = "0.9rem"))
              )
            )
          ),
          div(class="hint-text", "Key 仅保存在当前窗口会话内存中；关闭当前会话后不会保留。")
        )
      },

      # 本地推理：地址 + 模型名输入框
      if (needs_url) {
        tagList(
          div(class="hint-text", style="color:#3fb950;margin-bottom:0.4rem;",
              "本地推理无需 API Key"),
          textInput("local_base_url", "服务地址",
                    value = cfg[["base_url_default"]] %||% "",
                    placeholder = "http://localhost:11434/v1/chat/completions"),
          tags$small(class="hint-text local-url-input", "可修改端口或路径"),
          textInput("local_model_name", "模型名称",
                    value = "", placeholder = "llama3.2")
        )
      },

      # 可选：故障转移
      div(class = "failover-section",
        checkboxInput("enable_failover", "启用故障转移备用提供商", value=FALSE),
        conditionalPanel("input.enable_failover",
          selectInput("failover_provider_1", "备用提供商",
            choices = c(
              "无"="none",
              "OpenAI"="openai","Anthropic"="anthropic",
              "DeepSeek"="deepseek","Kimi"="kimi","Qwen"="qwen"
            )
          ),
          div(class = "password-field-shell",
            tags$label(`for` = "failover_api_key_1", class = "password-field-label", "备用 API Key"),
            div(class = "password-field-wrap",
              tags$input(
                id = "failover_api_key_1",
                type = "password",
                class = "form-control",
                value = rv$session_failover_api_key %||% "",
                placeholder = "sk-...",
                autocomplete = "off"
              ),
              tags$button(
                type = "button",
                class = "password-toggle-btn",
                onclick = "togglePasswordVisibility('failover_api_key_1', this);",
                tags$span(class = "icon-show", bs_icon("eye", size = "0.9rem")),
                tags$span(class = "icon-hide", bs_icon("eye-slash", size = "0.9rem"))
              )
            )
          )
        )
      )
    )
  })

  output$sdtm_section_status <- renderUI({
    n_uploaded <- .sum_true(lapply(rv$file_meta, Negate(is.null)))
    req_domains <- .get_required_domain_ids()
    n_required_ok <- .sum_true(lapply(req_domains, function(sid) !is.null(rv$file_meta[[sid]])))
    active_domains <- rv$active_domains %||% names(SDTM_DOMAIN_REGISTRY)
    n_active_ok <- .sum_true(lapply(active_domains, function(sid) !is.null(rv$file_meta[[sid]])))
    div(class="sidebar-section-card",
      div(class="sidebar-section-title",
        div(class="title-left", bs_icon("folder2-open", size="0.72rem"), "输入准备"),
        HTML(.badge_html(if (n_required_ok == length(req_domains) && n_uploaded > 0) "PASS" else "INFO"))
      ),
      div(class="sidebar-section-body",
        if (n_uploaded == 0) {
          "请先上传必要 SDTM 输入文件。核心域未齐备时，系统不会进入生成。"
        } else {
          paste0(
            "已上传 ", n_uploaded, " 个文件；核心域 ",
            n_required_ok, "/", length(req_domains),
            "；当前启用域 ", n_active_ok, "/", length(active_domains), "。"
          )
        }
      )
    )
  })

  output$llm_section_status <- renderUI({
    model_sel <- input$llm_model %||% "gpt-4o"
    prov      <- .infer_provider(model_sel)
    cfg       <- tryCatch(.get_provider_cfg(prov), error=function(e) NULL)
    has_key   <- is.null(cfg) || !isTRUE(cfg$needs_key) || !is.null(.non_empty(input$api_key %||% rv$session_api_key %||% ""))
    div(class="sidebar-section-card",
      div(class="sidebar-section-title",
        div(class="title-left", bs_icon("stars", size="0.72rem"), "AI 策略"),
        HTML(.badge_html(if (has_key) "PASS" else "WARNING"))
      ),
      div(class="sidebar-section-body",
        paste0(
          "当前模型：", model_sel, "；策略：",
          switch(input$llm_generation_mode %||% "balanced",
            "strict" = "稳健优先",
            "adaptive" = "补全优先",
            "平衡模式"
          ),
          if (!has_key) "。仍需填写 API Key 才能开始生成。" else "。"
        )
      )
    )
  })

  output$next_action_hint <- renderUI({
    hint <- if (length(rv$specs) == 0) {
      "后续动作：上传 Analysis Spec CSV。"
    } else if (!isTRUE(rv$spec_confirmed)) {
      "后续动作：确认 Spec 解析结果。"
    } else {
      model_sel <- input$llm_model %||% "gpt-4o"
      prov <- .infer_provider(model_sel)
      cfg  <- tryCatch(.get_provider_cfg(prov), error=function(e) NULL)
      has_key <- is.null(cfg) || !isTRUE(cfg$needs_key) || !is.null(.non_empty(input$api_key %||% rv$session_api_key %||% ""))
      if (!has_key && !isTRUE(MOCK_MODE)) {
        "后续动作：补充 API Key 或本地推理配置。"
      } else {
        "后续动作：进入“生成与审阅”页，并使用右侧控制区中的生成按钮启动本次生成。"
      }
    }
    div(class="sidebar-section-card",
      div(class="sidebar-section-title",
        div(class="title-left", bs_icon("signpost-split", size="0.72rem"), "下一步"),
        HTML(.badge_html("INFO"))
      ),
      div(class="sidebar-section-body", hint)
    )
  })

  observeEvent(input$goto_tab, {
    req(input$goto_tab)
    nav_select("main_tabs", selected = input$goto_tab, session = session)
  })

  observeEvent(list(input$btn_open_ai_settings, input$btn_open_ai_settings_inline), {
    showModal(
      modalDialog(
        title = tagList(bs_icon("sliders", size = "0.9rem", color = "#2dd4bf"), " AI 模型与接口设置"),
        size = "l",
        easyClose = TRUE,
        div(style = "display:flex;flex-direction:column;gap:0.85rem;",
          uiOutput("llm_config_status"),
          uiOutput("api_config_panel")
        ),
        footer = modalButton("关闭")
      )
    )
  })

  output$llm_config_status <- renderUI({
    model_sel <- input$llm_model %||% "gpt-4o"
    prov      <- .infer_provider(model_sel)
    cfg       <- tryCatch(.get_provider_cfg(prov), error=function(e) NULL)
    needs_key <- is.null(cfg) || isTRUE(cfg$needs_key)
    has_key   <- !needs_key || !is.null(.non_empty(input$api_key %||% rv$session_api_key %||% ""))
    failover_on <- isTRUE(input$enable_failover) &&
                   !is.null(input$failover_provider_1) &&
                   input$failover_provider_1 != "none"
    mode_label <- switch(input$llm_generation_mode %||% "balanced",
      "strict"   = "稳健优先",
      "adaptive" = "补全优先",
      "平衡模式"
    )
    trace_label <- if (isTRUE(input$llm_traceability_mode %||% TRUE)) "增强追踪" else "标准追踪"
    preview_label <- paste0(input$llm_preview_rows %||% 5, " 行预览")

    tagList(
      div(class="llm-status-card",
        div(class="llm-status-title",
          bs_icon("cpu", size="0.75rem"),
          " 当前 AI 配置"
        ),
        div(class="llm-chip-row",
          span(class="llm-chip llm-chip-primary", cfg$name %||% prov),
          span(class="llm-chip", model_sel),
          span(class="llm-chip", mode_label),
          span(class="llm-chip", trace_label),
          span(class="llm-chip", preview_label)
        ),
        div(class="llm-status-meta",
          span(if (has_key) "Key 已就绪" else "缺少 Key"),
          span("·"),
          span(if (failover_on) paste0("故障转移：", input$failover_provider_1) else "故障转移：关闭"),
          if (prov %in% c("ollama", "vllm")) tagList(
            span("·"),
            span(paste0("本地模型：", trimws(input$local_model_name %||% "未填写")))
          )
        )
      ),
      if (!is.null(rv$llm_request_meta)) {
        tok <- rv$llm_request_meta$token_info %||% list(total=0L)
        div(class="llm-status-card llm-last-run",
          div(class="llm-status-title",
            bs_icon("bar-chart-line", size="0.75rem"),
            " 最近一次生成"
          ),
          div(class="llm-status-meta",
            span(paste0(rv$llm_request_meta$provider_name, " / ", rv$llm_request_meta$model)),
            span("·"),
            span(paste0("Tokens: ", tok$total %||% 0L)),
            span("·"),
            span(paste0("风险点: ", nrow(rv$risk_logs_df %||% data.frame())))
          )
        )
      }
    )
  })

  # ===========================================================================
  # [S-3/S-7] Observer：单文件删除按钮（覆盖全部注册域）
  # ===========================================================================
  for (.sid in names(SDTM_DOMAIN_REGISTRY)) {
    local({
      sid <- .sid
      observeEvent(input[[paste0("btn_remove_", sid)]], {
        shinyjs::reset(paste0("file_", sid))
        rv$file_meta[[sid]] <- NULL
        if (length(rv$adam_datasets) > 0) {
          showNotification(
            tagList(tags$strong("⚠ 注意"), tags$br(),
                    paste0("已删除 ", toupper(sid), " 文件。现有 ADaM 结果可能需重新生成。")),
            type="warning", duration=6)
        }
      }, ignoreInit=TRUE)
    })
  }

  # Dynamic remove button for individual Spec files
  # Button onclick: Shiny.setInputValue('btn_remove_spec_which', FILE_ID, {priority:'event'})
  observeEvent(input$btn_remove_spec_which, {
    fid <- input$btn_remove_spec_which
    req(nchar(trimws(fid)) > 0, fid %in% names(rv$specs))

    rv$specs[[fid]] <- NULL

    # Update aggregate step_parse
    if (length(rv$specs) == 0) {
      rv$step_parse     <- "idle"
      rv$spec_confirmed <- FALSE
      shinyjs::reset("file_spec")
    } else {
      all_steps     <- sapply(rv$specs, function(s) s$step)
      rv$step_parse <- if (any(all_steps == "error"))  "error"
                       else if (any(all_steps == "warn")) "warn"
                       else "ok"
      rv$spec_confirmed <- FALSE  # require re-confirmation after deletion
    }

    if (length(rv$adam_datasets) > 0) {
      showNotification(
        tagList(tags$strong("⚠ 注意"), tags$br(),
                "已删除 Spec 文件。现有 ADaM 结果可能需重新生成。"),
        type="warning", duration=6)
    }
  }, ignoreInit=TRUE)

  # ===========================================================================
  # Observer：「重置代码」按钮（无修改）
  # ===========================================================================
  observeEvent(input$btn_reset_code, {
    req(rv$original_code)
    updateAceEditor(session, "code_editor", value=rv$original_code)
    .append_log("代码已重置为 LLM 原始版本", icon="↺")
    showNotification("代码已恢复为 LLM 原始版本", type="message", duration=3)
  })

  # ===========================================================================
  # Observer：主题切换 → 同步 Ace 编辑器配色
  # ===========================================================================
  observeEvent(input$theme_is_light, {
    ace_theme <- if (isTRUE(input$theme_is_light)) "github" else "tomorrow_night"
    updateAceEditor(session, "code_editor", theme = ace_theme)
  }, ignoreInit = TRUE)

  # ===========================================================================
  # Observer：确认并运行代码（无修改）
  # ===========================================================================
  observeEvent(input$btn_run_code, {
    if (is.null(rv$sdtm)) {
      showNotification("请先上传 SDTM 文件并点击「生成 ADaM 与代码」",
                       type="warning", duration=5)
      return()
    }
    code_str <- input$code_editor
    if (is.null(code_str) || nchar(trimws(code_str))==0) {
      showNotification("编辑器中无代码可执行", type="warning", duration=4)
      return()
    }

    rv$step_run <- "running"
    rv$step_validate <- "idle"
    rv$run_result_ok <- rv$run_result_err <- NULL
    rv$static_check_result <- NULL
    rv$validation_result <- rv$validation_issues_df <- rv$validation_stats_df <- NULL
    rv$adsl <- rv$adae <- NULL
    .append_log("开始执行用户确认的代码...", icon="▶")

    expected_ds <- tolower(unique(unlist(lapply(rv$specs, function(s)
      if (!is.null(s$parsed)) s$parsed$dataset else NULL))))
    if (length(expected_ds) == 0) expected_ds <- c("adsl", "adae")

    static_check <- run_code_static_checks(
      code_str = code_str,
      expected_datasets = expected_ds,
      allowed_packages = c("dplyr", "lubridate", "stringr", "tidyr", "readr", "haven", "purrr", "forcats", "janitor", "glue"),
      available_inputs = names(rv$sdtm %||% list())
    )
    rv$static_check_result <- static_check
    .append_log(
      sprintf("静态检查完成  状态=%s  ERR=%d  WARN=%d",
              static_check$summary$status,
              static_check$summary$errors,
              static_check$summary$warnings),
      icon = if (static_check$summary$status == "ERROR") "✖" else if (static_check$summary$status == "WARNING") "⚠" else "✔"
    )
    if (static_check$summary$status == "ERROR") {
      rv$step_run <- "error"
      rv$run_result_err <- paste(
        unique(head(static_check$issues$detail[static_check$issues$level == "ERROR"], 3)),
        collapse = "；"
      )
      showNotification(
        tagList(
          tags$strong("✖ 静态检查未通过"),
          tags$br(),
          rv$run_result_err
        ),
        type = "error", duration = 10
      )
      return()
    }

    # 预置包列表：核心5个 + 扩展5个（临床数据处理常用）
    needed_pkgs <- c(
      "dplyr", "lubridate", "stringr", "tidyr", "readr",   # 核心
      "haven", "purrr", "forcats", "janitor", "glue"        # 扩展
    )

    exec_env <- new.env(parent=baseenv())
    # [S-5] 动态注入所有已加载的 SDTM 域（替换原来的固定 dm/ex/ae 赋值）
    for (sid in names(rv$sdtm)) assign(sid, rv$sdtm[[sid]], envir=exec_env)

    # 注入预置包
    for (pkg in needed_pkgs) {
      if (requireNamespace(pkg, quietly=TRUE)) {
        for (fn in getNamespaceExports(pkg)) {
          tryCatch(assign(fn, getExportedValue(pkg,fn), envir=exec_env), error=function(e) NULL)
        }
      }
    }

    # 动态注入：扫描生成代码中额外的 library()/require() 调用
    extra_pkgs <- unique(c(
      regmatches(code_str, gregexpr("(?<=library\\()\\w+(?=\\))", code_str, perl=TRUE))[[1]],
      regmatches(code_str, gregexpr("(?<=require\\()\\w+(?=\\))", code_str, perl=TRUE))[[1]]
    ))
    extra_pkgs <- setdiff(extra_pkgs, needed_pkgs)
    failed_pkgs <- character(0)
    for (pkg in extra_pkgs) {
      if (requireNamespace(pkg, quietly=TRUE)) {
        for (fn in getNamespaceExports(pkg))
          tryCatch(assign(fn, getExportedValue(pkg, fn), envir=exec_env), error=function(e) NULL)
        .append_log("动态注入额外包：", pkg, icon="📦")
      } else {
        failed_pkgs <- c(failed_pkgs, pkg)
      }
    }
    if (length(failed_pkgs) > 0) {
      warn_msg <- paste0("以下包未安装，相关函数调用将失败：",
                         paste(failed_pkgs, collapse=", "))
      .append_log(warn_msg, icon="⚠")
      showNotification(warn_msg, type="warning", duration=8)
    }

    exec_env$strip_excel_apos <- strip_excel_apos
    exec_env$dy_char          <- dy_char
    # 屏蔽生成代码中的 library()/require()/install.packages() 调用
    # exec_env parent=baseenv()，utils 包函数不可见；包函数已通过预注入+动态注入处理
    exec_env$library  <- function(...) invisible(NULL)
    exec_env$require  <- function(...) invisible(TRUE)
    assign("install.packages", function(...) invisible(NULL), envir=exec_env)

    exec_result <- tryCatch({
      withCallingHandlers(
        eval(parse(text=code_str), envir=exec_env),
        message = function(m) {
          .append_log(trimws(conditionMessage(m)), icon="  ")
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          .append_log("警告：", conditionMessage(w), icon="⚠")
          invokeRestart("muffleWarning")
        }
      )
      "ok"
    }, error = function(e) conditionMessage(e))

    if (exec_result != "ok") {
      rv$step_run <- "error"
      rv$run_result_err <- exec_result
      .append_log("代码执行失败：", exec_result, icon="✖")
      showNotification(tagList(tags$strong("✖ 代码执行失败"), tags$br(), exec_result),
                       type="error", duration=10)
      return()
    }

    extracted <- list()
    for (ds in expected_ds) {
      obj <- tryCatch(get(ds, envir=exec_env), error=function(e) NULL)
      if (is.data.frame(obj)) extracted[[ds]] <- obj
    }

    if (length(extracted) == 0) {
      err_msg <- paste0("代码未生成任何预期数据集：", paste(expected_ds, collapse=", "))
      rv$step_run <- "error"
      rv$run_result_err <- err_msg
      .append_log(err_msg, icon="✖")
      showNotification(tagList(tags$strong("✖ 结果提取失败"), tags$br(), err_msg),
                       type="error", duration=8)
      return()
    }

    rv$adam_datasets <- extracted
    # [S-6] 向后兼容：同步 rv$adsl / rv$adae 供 Tab1 value_box 使用
    if ("adsl" %in% names(extracted)) rv$adsl <- extracted[["adsl"]]
    if ("adae" %in% names(extracted)) rv$adae <- extracted[["adae"]]

    rv$step_run <- "done"; rv$step_review <- "done"; rv$step_validate <- "running"
    exec_summary <- paste(
      sapply(names(extracted), function(ds)
        sprintf("%s=%d×%d", toupper(ds), nrow(extracted[[ds]]), ncol(extracted[[ds]]))),
      collapse="  ")
    .append_log(paste0("执行成功  ", exec_summary), icon="✔")
    .append_log("开始校验 ADaM 输出结果...", icon="⬤")

    validation_res <- tryCatch({
      validate_adam_datasets(extracted, rv$specs, rv$derivation_plan)
    }, error = function(e) {
      rv$step_validate <- "error"
      .append_log("结果校验失败：", conditionMessage(e), icon="✖")
      showNotification(
        tagList(tags$strong("⚠ 结果校验未完成"), tags$br(), conditionMessage(e)),
        type="warning", duration=8)
      NULL
    })

    if (!is.null(validation_res)) {
      rv$validation_result    <- validation_res
      rv$validation_issues_df <- validation_res$issues
      rv$validation_stats_df  <- validation_res$dataset_stats

      v_sum <- validation_res$summary
      rv$step_validate <- switch(v_sum$status,
        "ERROR"   = "error",
        "WARNING" = "warn",
        "done"
      )

      rv$run_result_ok <- paste0(
        exec_summary,
        "  |  校验: ",
        v_sum$status,
        " / ",
        v_sum$errors, " ERR / ",
        v_sum$warnings, " WARN"
      )

      .append_log(
        sprintf("结果校验完成  状态=%s  数据集=%d  错误=%d  警告=%d",
                v_sum$status, v_sum$datasets_total, v_sum$errors, v_sum$warnings),
        icon = if (v_sum$status == "ERROR") "✖" else if (v_sum$status == "WARNING") "⚠" else "✔"
      )

      if (nrow(validation_res$issues) > 0) {
        top_issues <- head(validation_res$issues, 3)
        for (i in seq_len(nrow(top_issues))) {
          .append_log(
            sprintf("[%s] %s · %s：%s",
                    top_issues$level[i], top_issues$dataset[i], top_issues$check[i], top_issues$detail[i]),
            icon = "  "
          )
        }
      }

      notif_type <- if (v_sum$status == "ERROR") "warning" else "message"
      showNotification(
        tagList(
          tags$strong(if (v_sum$status == "PASS") "✔ ADaM 生成并校验通过"
                      else if (v_sum$status == "WARNING") "✔ ADaM 已生成，存在校验警告"
                      else "⚠ ADaM 已生成，但校验发现错误"),
          tags$br(),
          paste0("错误 ", v_sum$errors, " 条；警告 ", v_sum$warnings, " 条")
        ),
        type = notif_type,
        duration = 6
      )
    } else {
      rv$run_result_ok <- exec_summary
    }

    nav_select(
      id = "main_tabs",
      selected = if (!is.null(validation_res) &&
                       validation_res$summary$status %in% c("ERROR", "WARNING")) "tab_generate" else "tab_output",
      session = session
    )
  })

  # ===========================================================================
  # Output 渲染（除 pipeline_steps 新增 step_parse 外，其余无修改）
  # ===========================================================================

  # 流水线进度（[修改 G-附] 新增「解析 Spec」阶段）
  output$pipeline_steps <- renderUI({
    steps <- list(
      list(label="解析 Spec",  state=rv$step_parse),   # [新增]
      list(label="加载文件",   state=rv$step_load),
      list(label="LLM 推理",   state=rv$step_llm),
      list(label="人工审阅",   state=rv$step_review),
      list(label="执行代码",   state=rv$step_run),
      list(label="结果校验",   state=rv$step_validate)
    )
    tagList(lapply(steps, function(s) {
      dot_class <- paste("step-dot", switch(s$state,
        "done"="done", "error"="warning", "running"="active", "warn"="warning", ""))
      div(class="step-indicator",
        div(class=dot_class),
        div(class="step-label", s$label,
          if (s$state=="running") tags$em(" ···")
          else if (s$state %in% c("error","warn")) tags$em(" ✖",style="color:#f85149;")
          else if (s$state=="done") tags$em(" ✔",style="color:#3fb950;")
        )
      )
    }))
  })

  # ===========================================================================
  # [新增] output$uploaded_files_list
  # 说明：实时监听四个 fileInput，渲染已上传/未上传状态卡片。
  #       使用 reactive() 而非 eventReactive()，任一文件上传即刻刷新。
  #       展示信息：文件名 + 行数（行数需文件已读取到 rv$sdtm 后才能显示）
  # ===========================================================================
  output$uploaded_files_list <- renderUI({

    # ── Helper: render one file row ──────────────────────────────────────────
    .uf_row <- function(label, meta, remove_onclick) {
      fname_display <- if (nchar(meta$name) > 22) paste0(substr(meta$name,1,20),"...") else meta$name
      rows_str <- if (!is.na(meta$rows)) formatC(meta$rows, big.mark=",") else "?"
      cols_str <- if (!is.na(meta$cols)) as.character(meta$cols) else "?"
      div(class="uf-item", style="flex-direction:column;align-items:flex-start;padding:0.25rem 0;",
        div(style="display:flex;align-items:center;gap:0.45rem;width:100%;",
          div(class="uf-dot ok"),
          div(style="flex:1;overflow:hidden;",
            tags$strong(style="color:#2dd4bf;margin-right:0.3rem;", paste0(label, ":")),
            span(style="color:#c9d1d9;font-size:0.73rem;", fname_display)
          ),
          tags$button(class="btn-remove-file", onclick=remove_onclick, HTML("&times;"))
        ),
        div(class="uf-meta", style="margin-left:1.1rem;margin-top:0.1rem;",
          rows_str, " 行", HTML(" <span class='uf-meta-sep'>x</span> "), cols_str, " 列",
          HTML(" <span class='uf-meta-sep'>|</span> "), .fmt_size(meta$size),
          HTML(" <span class='uf-meta-sep'>|</span> "), format(meta$upload_time, "%H:%M:%S")
        )
      )
    }

    # ── [S-9] SDTM slots：依 rv$active_domains 动态生成 ────────────────────
    sdtm_slots <- lapply(rv$active_domains, function(sid) list(sid=sid, label=toupper(sid)))
    sdtm_items <- lapply(sdtm_slots, function(s) {
      meta <- rv$file_meta[[s$sid]]
      if (!is.null(meta)) {
        onclick <- sprintf("Shiny.setInputValue('btn_remove_%s', Math.random(), {priority:'event'});", s$sid)
        .uf_row(s$label, meta, onclick)
      } else {
        div(class="uf-item missing-item",
            div(class="uf-dot missing"), span(paste0(s$label, " — 未上传")))
      }
    })

    # ── Dynamic Spec rows (one row per uploaded Spec file) ───────────────────
    spec_items <- lapply(names(rv$specs), function(fid) {
      s       <- rv$specs[[fid]]
      confirm_badge <- if (!is.null(s$parsed))
        span(style="font-size:0.63rem;color:#3fb950;margin-left:0.3rem;", "(已确认)")
      else NULL
      # Build a fake meta-like list for the helper
      meta_s <- list(
        name        = s$filename,
        size        = s$size,
        upload_time = s$upload_time,
        rows        = nrow(s$csv_raw),
        cols        = ncol(s$csv_raw)
      )
      onclick <- sprintf(
        "Shiny.setInputValue('btn_remove_spec_which', '%s', {priority:'event'});", fid)
      row <- .uf_row("SPEC", meta_s, onclick)
      # Append confirmation badge to the filename area
      if (!is.null(confirm_badge)) {
        # inject badge after the row (tagList wraps)
        tagList(row, div(style="margin-left:1.5rem;margin-top:-0.1rem;", confirm_badge))
      } else row
    })

    if (length(sdtm_items) == 0 && length(spec_items) == 0) {
      # No files at all
      any_sdtm <- any(sapply(sdtm_slots, function(s) !is.null(rv$file_meta[[s$sid]])))
      if (!any_sdtm && length(rv$specs) == 0) return(NULL)
    }

    # Only hide the card if truly nothing uploaded
    has_any <- any(sapply(sdtm_slots, function(s) !is.null(rv$file_meta[[s$sid]]))) ||
               length(rv$specs) > 0
    if (!has_any) return(NULL)

    div(class="uploaded-files-card",
      div(class="uf-title", bs_icon("folder2-open", size="0.65rem"), " 已选文件"),
      tagList(sdtm_items),
      if (length(rv$specs) > 0) tagList(spec_items)
      else div(class="uf-item missing-item",
               div(class="uf-dot missing"), span("SPEC — 未上传"))
    )
  })

  output$uploaded_preview_gallery <- renderUI({
    metas <- rv$file_meta %||% list()
    metas <- metas[rv$active_domains %||% names(metas)]
    metas <- Filter(function(x) !is.null(x) && is.data.frame(x$preview) && nrow(x$preview) > 0, metas)
    if (length(metas) == 0) return(NULL)

    preview_cards <- lapply(names(metas), function(sid) {
      meta <- metas[[sid]]
      df_preview <- meta$preview
      trunc_note <- if (ncol(df_preview) > 6) {
        paste0("预览显示前 6 列，完整列数 ", ncol(df_preview), "。")
      } else {
        paste0("预览行数 ", nrow(df_preview), "。")
      }
      show_df <- df_preview[, seq_len(min(ncol(df_preview), 6)), drop = FALSE]
      preview_rows <- apply(show_df, 1, function(r) {
        tags$tr(lapply(as.character(r), tags$td))
      })

      div(class = "input-section-block",
        div(class = "input-section-title", paste0(toupper(sid), " 样本预览")),
        div(class = "input-section-meta", trunc_note),
        div(class = "preview-scroll",
          tags$table(class = "preview-table",
            tags$thead(tags$tr(lapply(names(show_df), tags$th))),
            tags$tbody(preview_rows)
          )
        )
      )
    })

    div(style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:0.85rem;",
      tagList(preview_cards)
    )
  })

  output$run_status <- renderText({
    if (length(rv$log_lines)==0) return("# 等待操作... 上传文件后点击「生成 ADaM 与代码」开始")
    paste(rv$log_lines, collapse="\n")
  })

  output$run_log_teaser <- renderUI({
    teaser <- if (length(rv$log_lines) == 0) {
      "当前还没有执行日志。完成输入准备后，系统会在这里显示生成与执行的关键阶段。"
    } else {
      paste(utils::tail(rv$log_lines, 3), collapse = "  ")
    }
    div(class = "workspace-text", teaser)
  })

  output$vb_n_adsl    <- renderText(if(is.null(rv$adsl)) "—" else formatC(nrow(rv$adsl),big.mark=","))
  output$vb_n_adae    <- renderText(if(is.null(rv$adae)) "—" else formatC(nrow(rv$adae),big.mark=","))
  output$vb_n_risks   <- renderText({
    if (is.null(rv$risk_logs_df)) return("—")
    n_e <- sum(rv$risk_logs_df$level=="ERROR")
    if (n_e>0) paste0(nrow(rv$risk_logs_df)," (",n_e," ERR)") else as.character(nrow(rv$risk_logs_df))
  })
  output$vb_llm_status <- renderText(switch(rv$step_llm,"idle"="待机","running"="推理中","done"="完成","error"="失败","待机"))
  output$vb_validation_status <- renderText({
    if (is.null(rv$validation_result)) {
      return(switch(rv$step_validate, "running" = "校验中", "error" = "失败", "—"))
    }
    switch(rv$validation_result$summary$status,
      "PASS"    = "通过",
      "WARNING" = "告警",
      "ERROR"   = "失败",
      "—"
    )
  })
  output$vb_validation_errors <- renderText({
    if (is.null(rv$validation_result)) return("—")
    as.character(rv$validation_result$summary$errors %||% 0L)
  })
  output$vb_validation_warnings <- renderText({
    if (is.null(rv$validation_result)) return("—")
    as.character(rv$validation_result$summary$warnings %||% 0L)
  })

  output$sidebar_workflow_overview <- renderUI({
    snap <- .workflow_snapshot()
    stage_rows <- list(
      list(
        name = "输入准备",
        state = if (snap$n_required_ok == snap$n_required && snap$has_specs) "done" else "active",
        desc = paste0(
          "核心域 ", snap$n_required_ok, "/", snap$n_required,
          "；当前启用域 ", snap$n_active_ok, "/", snap$n_active,
          "；Spec ", if (snap$has_specs) "已上传" else "待上传"
        )
      ),
      list(
        name = "AI 生成",
        state = if (rv$step_llm == "running") "active" else if (snap$has_llm) "done" else if (snap$spec_confirmed && snap$has_key) "active" else "idle",
        desc = if (snap$spec_confirmed && snap$has_key) "可以启动生成或继续查看本次模型配置" else "等待 Spec 确认与模型配置完成"
      ),
      list(
        name = "人工审阅",
        state = if (rv$step_review == "running") "active" else if (rv$step_run == "done") "done" else "idle",
        desc = if (snap$has_llm) "生成完成后进入代码审阅阶段" else "尚未进入代码审阅"
      ),
      list(
        name = "输出结果",
        state = if (!is.null(rv$validation_result) && rv$validation_result$summary$status == "ERROR") "warn" else if (snap$has_output) "done" else "idle",
        desc = if (snap$has_output) paste0("已生成 ", length(rv$adam_datasets), " 个数据集") else "执行代码后查看输出与校验"
      )
    )

    div(class = "workflow-overview-card",
      div(class = "workflow-overview-title", "工作流概览"),
      lapply(stage_rows, function(x) {
        dot_cls <- paste(
          "workflow-step-dot",
          switch(x$state, active = "active", done = "done", warn = "warn", "")
        )
        div(class = "workflow-step-row",
          div(class = dot_cls),
          div(class = "workflow-step-copy",
            div(class = "workflow-step-name", x$name),
            div(class = "workflow-step-desc", x$desc)
          )
        )
      })
    )
  })

  output$workflow_hero <- renderUI({
    snap <- .workflow_snapshot()
    title <- "当前阶段：输入准备与生成控制"
    text <- "本工作台按“输入确认、AI 生成、人工审阅、结果输出”四个阶段推进。页面优先呈现当前阶段和下一项动作。"
    pills <- list(
      span(class = "hero-pill", paste0("SDTM 文件 ", snap$n_uploaded)),
      span(class = "hero-pill", paste0("Spec ", if (snap$spec_confirmed) "已确认" else if (snap$has_specs) "待确认" else "未上传")),
      span(class = "hero-pill", paste0("LLM ", switch(rv$step_llm, running = "生成中", done = "已完成", error = "失败", "待启动")))
    )

    if (rv$step_llm == "running") {
      title <- "AI 正在生成代码与风险日志"
      text <- "系统已进入核心推理阶段。当前最重要的是等待模型返回结构化结果，而不是查看下方的完整表格。"
    } else if (!is.null(rv$llm_result) && length(rv$adam_datasets) == 0) {
      title <- "代码已生成，下一步是人工审阅"
      text <- "当前应该重点关注生成策略、风险点和代码是否符合业务预期，再决定是否执行。"
      pills <- c(pills, span(class = "hero-pill", paste0("风险点 ", nrow(rv$risk_logs_df %||% data.frame()))))
    } else if (length(rv$adam_datasets) > 0 && !is.null(rv$validation_result)) {
      title <- if (rv$validation_result$summary$status == "PASS") "输出已生成，并通过结构与语义校验" else "输出已生成，但仍需关注风险与校验"
      text <- paste0(
        "本次共生成 ", length(rv$adam_datasets), " 个输出数据集。",
        " 当前校验状态：", rv$validation_result$summary$status,
        "；错误 ", rv$validation_result$summary$errors,
        "；警告 ", rv$validation_result$summary$warnings, "。"
      )
      pills <- c(pills, span(class = "hero-pill", paste0("输出集 ", length(rv$adam_datasets))))
    }

    div(class = "hero-panel",
      div(class = "hero-kicker", "ADaM Builder Workflow"),
      div(class = "hero-title", title),
      div(class = "hero-text", text),
      div(class = "hero-meta", tagList(pills))
    )
  })

  output$current_step_workspace <- renderUI({
    snap <- .workflow_snapshot()
    kicker <- "当前任务"
    title <- "补齐输入与配置"
    text <- "请先完成必需输入、Spec 确认和 AI 配置。满足条件后，再从右侧控制区启动生成。"
    actions <- NULL

    if (rv$step_llm == "running") {
      title <- "等待 AI 完成生成"
      text <- "模型调用进行中。建议先观察右侧流程进度，不必过早关注明细表格。"
    } else if (!snap$has_specs || snap$n_required_ok < snap$n_required) {
      title <- "输入条件尚未满足"
      text <- paste0(
        "核心域 ", snap$n_required_ok, "/", snap$n_required,
        "；当前启用域 ", snap$n_active_ok, "/", snap$n_active,
        "；Spec 状态：", if (snap$has_specs) "已上传，待确认" else "未上传。"
      )
    } else if (!snap$spec_confirmed) {
      title <- "确认 Spec 解析结果"
      text <- "当前输入已经接近齐备，但系统仍在等待你确认 Spec 解析结果后再生成。"
    } else if (!snap$has_key) {
      title <- "补充 AI 配置后即可生成"
      text <- "Spec 已确认，下一步只差模型凭证或本地推理配置。"
    } else if (!is.null(rv$llm_result) && length(rv$adam_datasets) == 0) {
      title <- "进入代码审阅"
      text <- "当前最重要的是阅读代码与风险摘要，确认逻辑后再执行。"
      actions <- tags$button(
        class = "btn-ghost-workflow",
        onclick = "Shiny.setInputValue('goto_tab','tab_generate',{priority:'event'});",
        "前往代码审阅"
      )
    } else if (length(rv$adam_datasets) > 0) {
      title <- "查看输出与校验结果"
      text <- "生成流程已经跑通。现在应该查看输出数据集，并根据校验结果判断是否需要回到代码页调整。"
      actions <- tagList(
        tags$button(
          class = "btn-ghost-workflow",
          onclick = "Shiny.setInputValue('goto_tab','tab_output',{priority:'event'});",
          "查看输出数据集"
        ),
        tags$button(
          class = "btn-ghost-workflow",
          onclick = "Shiny.setInputValue('goto_tab','tab_generate',{priority:'event'});",
          "返回代码审阅"
        )
      )
    } else {
      title <- "已满足生成条件"
      text <- "当前输入与配置均已就绪。请使用右侧“AI 与生成”区域中的唯一生成按钮启动本次生成。"
    }

    card(
      card_header(tagList(bs_icon("compass", size = "0.75rem"), " 当前任务区")),
      div(class = "workspace-card",
        div(class = "workspace-kicker", kicker),
        div(class = "workspace-title", title),
        div(class = "workspace-text", text),
        if (!is.null(actions)) div(class = "workspace-actions", actions)
      )
    )
  })

  output$focus_metric_grid <- renderUI({
    snap <- .workflow_snapshot()
    metrics <- list(
      list("输入进度", paste0(snap$n_required_ok, "/", snap$n_required), if (snap$n_uploaded > 0) paste0("共上传 ", snap$n_uploaded, " 个文件；启用域 ", snap$n_active_ok, "/", snap$n_active) else "必要输入文件尚未准备完成"),
      list("Spec 状态", if (snap$spec_confirmed) "已确认" else if (snap$has_specs) "待确认" else "未上传", paste0(length(rv$specs), " 个文件进入当前流程")),
      list("AI 状态", switch(rv$step_llm, idle = "待启动", running = "生成中", done = "已完成", error = "失败", "待启动"), paste0("模型：", rv$llm_request_meta$model %||% (input$llm_model %||% "gpt-4o"))),
      list("输出结果", if (length(rv$adam_datasets) > 0) paste0(length(rv$adam_datasets), " 个") else "尚未输出", if (is.null(rv$validation_result)) "等待执行与校验" else paste0("校验：", rv$validation_result$summary$status))
    )

    div(class = "metric-grid",
      lapply(metrics, function(x) {
        div(class = "metric-card",
          div(class = "metric-label", x[[1]]),
          div(class = "metric-value", x[[2]]),
          div(class = "metric-note", x[[3]])
        )
      })
    )
  })

  output$priority_digest <- renderUI({
    items <- list()

    if (!is.null(rv$risk_logs_df)) {
      top_risk <- rv$risk_logs_df[order(match(rv$risk_logs_df$level, c("ERROR", "WARNING", "INFO"))), , drop = FALSE]
      top_risk <- head(top_risk, 2)
      items[[length(items) + 1]] <- div(class = "digest-item",
        div(class = "digest-item-head",
          div(class = "digest-item-title", "LLM 风险摘要"),
          HTML(.badge_html(if (any(rv$risk_logs_df$level == "ERROR")) "ERROR" else if (any(rv$risk_logs_df$level == "WARNING")) "WARNING" else "INFO"))
        ),
        div(class = "digest-item-text",
          paste0("共识别 ", nrow(rv$risk_logs_df), " 条风险。重点关注：",
                 paste(paste0(top_risk$variable, " - ", top_risk$description), collapse = "；"))
        )
      )
    }

    if (!is.null(rv$validation_result)) {
      items[[length(items) + 1]] <- div(class = "digest-item",
        div(class = "digest-item-head",
          div(class = "digest-item-title", "结果校验摘要"),
          HTML(.badge_html(rv$validation_result$summary$status))
        ),
        div(class = "digest-item-text",
          paste0("错误 ", rv$validation_result$summary$errors,
                 " 条；警告 ", rv$validation_result$summary$warnings,
                 " 条。建议优先查看状态不是 PASS 的数据集。")
        )
      )
    }

    next_msg <- if (length(rv$adam_datasets) > 0) {
      "当前最合理的动作是查看输出数据集，并对照校验结果决定是否回到代码页调整。"
    } else if (!is.null(rv$llm_result)) {
      "当前最合理的动作是进入代码审阅页，先看生成逻辑，再决定是否执行。"
    } else {
      "当前最合理的动作是先补齐左侧输入与 AI 配置，不必急着查看下方明细表。"
    }
    items[[length(items) + 1]] <- div(class = "digest-item",
      div(class = "digest-item-head",
        div(class = "digest-item-title", "建议关注点"),
        HTML(.badge_html("INFO"))
      ),
      div(class = "digest-item-text", next_msg)
    )

    div(class = "digest-list", tagList(items))
  })

  output$input_next_step <- renderUI({
    snap <- .workflow_snapshot()
    title <- "后续动作"
    text <- paste0(
      "核心域 ", snap$n_required_ok, "/", snap$n_required,
      "；当前启用域 ", snap$n_active_ok, "/", snap$n_active,
      "。请按下述顺序完成当前阶段。"
    )
    actions <- NULL

    if (!snap$has_specs) {
      title <- "后续动作：上传 Analysis Specification"
      text <- "请上传一个或多个 Spec CSV。系统会自动解析列映射，并在确认后进入生成阶段。"
    } else if (!snap$spec_confirmed) {
      title <- "后续动作：确认 Spec 解析结果"
      text <- "Spec 已上传，但尚未确认。请先完成字段映射确认，生成按钮在此之前不会进入可执行状态。"
    } else if (!snap$has_key) {
      title <- "后续动作：补充 AI 设置"
      text <- "输入已满足生成前提。请打开 AI 设置补充模型或接口凭证，随后在“生成与审阅”页启动生成。"
      actions <- tags$button(
        class = "btn-ghost-workflow",
        onclick = "document.getElementById('btn_open_ai_settings').click();",
        "打开 AI 设置"
      )
    } else {
      title <- "后续动作：进入“生成与审阅”"
      text <- "当前输入和 AI 配置均已就绪。请进入“生成与审阅”页，使用右侧控制区中的生成按钮启动本次生成。"
      actions <- tagList(
        tags$button(
          class = "btn-ghost-workflow",
          onclick = "Shiny.setInputValue('goto_tab','tab_generate',{priority:'event'});",
          "进入生成与审阅"
        ),
        div(class = "hint-text", "进入下一页后，请使用右侧“AI 与生成”区域中的唯一生成按钮。")
      )
    }

    div(class = "input-next-step-card",
      div(class = "input-next-step-title", title),
      div(class = "input-next-step-text", text),
      if (!is.null(actions)) div(class = "input-next-step-actions", actions)
    )
  })

  output$run_summary_banner <- renderUI({
    cls <- "summary-banner"
    title <- "当前处于准备阶段"
    text <- "先完成 SDTM 与 Spec 上传，再配置 AI 并开始生成。"

    if (rv$step_llm == "running") {
      title <- "AI 正在生成代码"
      text <- "系统正在构建 Prompt 并调用模型，完成后会自动进入代码审查阶段。"
    } else if (rv$step_review == "running") {
      title <- "代码已生成，等待人工审阅"
      text <- paste0(
        "当前应重点查看风险日志和生成代码。",
        if (!is.null(rv$llm_request_meta)) paste0(" 本次模型：", rv$llm_request_meta$model, "。") else ""
      )
    } else if (rv$step_run == "done" && !is.null(rv$validation_result)) {
      if (rv$validation_result$summary$status == "ERROR") {
        cls <- "summary-banner error"
        title <- "结果已生成，但校验发现错误"
        text <- paste0("建议先在本页查看校验与风险，再决定是否继续使用输出结果。错误数：",
                       rv$validation_result$summary$errors, "。")
      } else if (rv$validation_result$summary$status == "WARNING") {
        cls <- "summary-banner warn"
        title <- "结果已生成，存在校验警告"
        text <- paste0("输出已可查看，但仍建议先检查警告项。警告数：",
                       rv$validation_result$summary$warnings, "。")
      } else {
        title <- "结果已生成并通过结构与语义校验"
        text <- "可以继续查看输出数据集，也可以回到代码页审查本次生成逻辑。"
      }
    }

    div(class=cls,
      div(class="summary-banner-title", bs_icon("info-circle", size="0.8rem"), title),
      div(class="summary-banner-text", text)
    )
  })

  filtered_risk_df <- reactive({
    df <- rv$risk_logs_df; if (is.null(df)) return(NULL)
    lv <- input$filter_risk_level %||% "ALL"
    if (lv!="ALL") df[df$level==lv,,drop=FALSE] else df
  })

  output$tbl_risk_logs <- renderDT({
    df <- filtered_risk_df(); req(!is.null(df)&&nrow(df)>0)
    df$level <- sapply(df$level,.badge_html)
    names(df) <- c("级别","变量","LLM 推断描述","需确认的假设")
    datatable(df, escape=FALSE, rownames=FALSE, selection="none",
      options=c(.dt_options(page_length=20),
        list(columnDefs=list(list(width="80px",targets=0),list(width="120px",targets=1),
                             list(width="300px",targets=2),list(className="dt-left",targets="_all")))),
      class="cell-border")
  }, server=FALSE)

  output$risk_logs_placeholder <- renderUI({
    df <- filtered_risk_df()
    if (!is.null(df)&&nrow(df)>0) return(NULL)
    if (is.null(rv$llm_result))
      .placeholder_ui("shield","暂无风险日志","点击「生成 ADaM 与代码」后 LLM 将自动识别推断风险点")
    else
      .placeholder_ui("shield-check","无匹配的风险记录",
        if(input$filter_risk_level!="ALL") "尝试切换过滤条件为「全部」" else NULL)
  })

  filtered_validation_df <- reactive({
    df <- rv$validation_issues_df
    if (is.null(df) || nrow(df) == 0) return(df)
    lv <- input$filter_validation_level %||% "ALL"
    if (lv != "ALL") df[df$level == lv, , drop = FALSE] else df
  })

  output$validation_overview <- renderUI({
    stats_df <- rv$validation_stats_df
    if (is.null(stats_df) || nrow(stats_df) == 0) return(NULL)

    cards <- lapply(seq_len(nrow(stats_df)), function(i) {
      row <- stats_df[i, , drop = FALSE]
      status_color <- switch(row$status[[1]],
        "PASS"    = "#3fb950",
        "WARNING" = "#d29922",
        "#f85149"
      )
      div(
        class = "validation-mini-card",
        style = paste0("border-left-color:", status_color, ";"),
        div(style="display:flex;align-items:center;justify-content:space-between;gap:0.6rem;",
          div(class = "dataset-name", row$dataset[[1]]),
          HTML(.badge_html(row$status[[1]]))
        ),
        div(class = "dataset-meta",
          sprintf("%s 行 × %s 列  |  %s ERR  |  %s WARN",
                  formatC(row$rows[[1]], big.mark=","), row$cols[[1]],
                  row$errors[[1]], row$warnings[[1]])
        )
      )
    })

    div(style="display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:0.6rem;",
        tagList(cards))
  })

  output$tbl_validation <- renderDT({
    df <- filtered_validation_df()
    req(!is.null(df) && nrow(df) > 0)
    df$level <- sapply(df$level, .badge_html)
    names(df) <- c("数据集", "级别", "检查项", "问题说明", "建议动作")
    datatable(
      df, escape = FALSE, rownames = FALSE, selection = "none",
      options = c(
        .dt_options(page_length = 12),
        list(columnDefs = list(
          list(width = "90px", targets = 0),
          list(width = "80px", targets = 1),
          list(width = "120px", targets = 2),
          list(className = "dt-left", targets = "_all")
        ))
      ),
      class = "cell-border"
    )
  }, server = FALSE)

  output$validation_placeholder <- renderUI({
    df <- filtered_validation_df()
    if (!is.null(df) && nrow(df) > 0) return(NULL)
    if (is.null(rv$validation_result)) {
      .placeholder_ui("clipboard2-check", "暂无结果校验",
        "执行代码成功后，系统将在这里展示结构、质量与 plan 语义检查结果")
    } else {
      .placeholder_ui("clipboard2-check", "未发现匹配的校验问题",
        if ((input$filter_validation_level %||% "ALL") != "ALL") "尝试切换过滤条件为「全部」" else "当前结果未发现问题")
    }
  })

  output$tbl_sdtm_profile <- renderDT({
    df <- flatten_sdtm_profiles(rv$sdtm_profile %||% list())
    req(nrow(df) > 0)
    names(df) <- c("域", "行数", "列数", "候选键", "日期列", "高缺失列")
    datatable(
      df, rownames = FALSE, selection = "none",
      options = c(
        .dt_options(page_length = 10),
        list(columnDefs = list(list(className = "dt-left", targets = "_all")))
      ),
      class = "cell-border"
    )
  }, server = FALSE)

  output$sdtm_profile_placeholder <- renderUI({
    df <- flatten_sdtm_profiles(rv$sdtm_profile %||% list())
    if (nrow(df) > 0) return(NULL)
    .placeholder_ui("diagram-3", "暂无输入画像",
      "启动生成后，系统会在读取 SDTM 文件时构建域级 profile，并在此处展示。")
  })

  output$tbl_plan_variables <- renderDT({
    df <- flatten_derivation_plan(rv$derivation_plan %||% list(datasets = list()))
    req(nrow(df) > 0)
    names(df) <- c("数据集", "变量", "类型", "来源域", "来源列", "派生规则", "置信度")
    datatable(
      df, rownames = FALSE, selection = "none",
      options = c(
        .dt_options(page_length = 12),
        list(columnDefs = list(
          list(width = "90px", targets = 0),
          list(width = "110px", targets = 1),
          list(width = "80px", targets = 2),
          list(width = "90px", targets = 3),
          list(className = "dt-left", targets = "_all")
        ))
      ),
      class = "cell-border"
    )
  }, server = FALSE)

  output$plan_variables_placeholder <- renderUI({
    df <- flatten_derivation_plan(rv$derivation_plan %||% list(datasets = list()))
    if (nrow(df) > 0) return(NULL)
    .placeholder_ui("bezier2", "暂无生成计划",
      "完成 LLM 生成后，这里会展示变量级 derivation plan。")
  })

  output$code_context_summary <- renderUI({
    meta <- rv$llm_request_meta
    if (is.null(meta) && is.null(rv$llm_result)) return(NULL)
    div(class="context-strip",
      div(class="context-strip-title", "本次生成上下文"),
      div(class="context-strip-body",
        paste0(
          "模型：", meta$model %||% "—",
          "；提供商：", meta$provider_name %||% "—",
          "；策略：", switch(meta$generation_mode %||% "balanced",
            "strict" = "稳健优先",
            "adaptive" = "补全优先",
            "平衡模式"
          ),
          "；风险追踪：", if (isTRUE(meta$traceability)) "增强" else "标准",
          "；风险点：", nrow(rv$risk_logs_df %||% data.frame()),
          "。请结合风险日志检查代码是否符合当前 Spec 和业务预期。"
        )
      )
    )
  })

  output$profile_context_summary <- renderUI({
    profile_df <- flatten_sdtm_profiles(rv$sdtm_profile %||% list())
    if (nrow(profile_df) == 0) return(NULL)
    div(class="context-strip",
      div(class="context-strip-title", "输入数据画像"),
      div(class="context-strip-body",
        paste0(
          "当前已构建 ", nrow(profile_df), " 个 SDTM 域的结构化 profile：",
          paste(profile_df$domain, collapse = ", "),
          "。这些 profile 会作为 LLM 的主输入上下文，用于推断候选键、日期列和高缺失字段。"
        )
      )
    )
  })

  output$plan_context_summary <- renderUI({
    plan <- rv$derivation_plan
    if (is.null(plan)) return(NULL)
    plan_df <- summarize_derivation_plan(plan)
    if (nrow(plan_df) == 0) return(NULL)

    issue_note <- if (!is.null(rv$derivation_plan_issues_df) && nrow(rv$derivation_plan_issues_df) > 0) {
      paste0("Plan/Spec 对齐提醒 ", nrow(rv$derivation_plan_issues_df), " 条。")
    } else {
      "Plan 已完成基础 Spec 对齐。"
    }

    div(class="context-strip",
      div(class="context-strip-title", "生成计划摘要"),
      div(class="context-strip-body",
        paste0(
          "本次已生成 derivation plan：",
          paste(paste0(plan_df$dataset, "(", plan_df$variables, " vars)"), collapse = "；"),
          "。", issue_note,
          " 当前代码应被视为该 plan 的实现，而不是唯一事实来源。"
        )
      )
    )
  })

  output$output_context_summary <- renderUI({
    dsets <- rv$adam_datasets
    if (length(dsets) == 0) return(NULL)
    val <- rv$validation_result
    val_text <- if (is.null(val)) {
      "尚未执行结果校验。"
    } else {
      paste0("校验状态：", val$summary$status,
             "；错误 ", val$summary$errors,
             "；警告 ", val$summary$warnings, "。")
    }
    div(class="context-strip",
      div(class="context-strip-title", "输出摘要"),
      div(class="context-strip-body",
        paste0("本次共生成 ", length(dsets), " 个数据集：",
               paste(toupper(names(dsets)), collapse = ", "),
               "。", val_text)
      )
    )
  })

  output$code_line_count <- renderText({
    code <- input$code_editor %||% ""
    paste0(length(strsplit(code,"\n")[[1]]), " 行")
  })

  output$run_code_status <- renderUI({
    if (!is.null(rv$run_result_ok))
      return(div(style="background:rgba(63,185,80,0.1);border:1px solid rgba(63,185,80,0.3);border-radius:6px;padding:0.5rem 0.9rem;font-size:0.8rem;color:#3fb950;display:flex;align-items:center;gap:0.5rem;",
                 bsicons::bs_icon("check-circle-fill",color="#3fb950"), rv$run_result_ok))
    if (!is.null(rv$run_result_err))
      return(div(style="background:rgba(248,81,73,0.08);border:1px solid rgba(248,81,73,0.3);border-radius:6px;padding:0.5rem 0.9rem;font-size:0.78rem;color:#f85149;font-family:'JetBrains Mono',monospace;word-break:break-all;",
                 bsicons::bs_icon("x-circle-fill",color="#f85149"), " ", rv$run_result_err))
    if (!is.null(rv$static_check_result) && identical(rv$static_check_result$summary$status, "WARNING"))
      return(div(class = "run-status-note",
                 bsicons::bs_icon("exclamation-triangle"),
                 paste0("静态检查发现 ", rv$static_check_result$summary$warnings,
                        " 条警告。建议先审阅，再决定是否执行。")))
    if (rv$step_review=="running")
      return(div(class = "run-status-note",
                 bsicons::bs_icon("pencil-square"), " 请审阅上方代码，确认无误后点击右侧按钮执行"))
    div(class = "run-status-note", "等待 LLM 生成代码...")
  })

  # ===========================================================================
  # [S-8] 动态 UI：SDTM 域选择复选框
  # ===========================================================================
  output$sdtm_domain_selector <- renderUI({
    all_groups <- c("core", "basic", "extended")
    tagList(lapply(all_groups, function(grp) {
      # 只显示非 required 的域（required 域始终激活，不需要复选框）
      dom_ids <- names(Filter(
        function(d) d$group == grp && !isTRUE(d$required),
        SDTM_DOMAIN_REGISTRY))
      if (length(dom_ids) == 0) return(NULL)
      choices_vec <- setNames(
        dom_ids,
        sapply(dom_ids, function(id) SDTM_DOMAIN_REGISTRY[[id]]$label))
      # core 组（ae）默认选中，其他组默认不选
      default_sel <- if (grp == "core") dom_ids else character(0)
      tagList(
        div(class="hint-text",
            style="margin:0.4rem 0 0.1rem 0;font-size:0.65rem;letter-spacing:0.08em;text-transform:uppercase;",
            DOMAIN_GROUP_LABELS[[grp]]),
        checkboxGroupInput(
          paste0("sdtm_domains_", grp),
          label    = NULL,
          choices  = choices_vec,
          selected = default_sel,
          inline   = FALSE
        )
      )
    }))
  })

  # ===========================================================================
  # [S-8] 动态 UI：SDTM 文件上传面板（依 rv$active_domains 实时渲染）
  # ===========================================================================
  output$sdtm_upload_panel <- renderUI({
    div(class = "sdtm-upload-grid",
      lapply(rv$active_domains, function(sid) {
        d <- SDTM_DOMAIN_REGISTRY[[sid]]
        div(class = "sdtm-upload-card",
          div(class = "upload-label", toupper(sid)),
          div(class = "domain-upload-meta", d$label),
          fileInput(paste0("file_", sid), NULL, accept=".csv",
                    placeholder=d$placeholder)
        )
      })
    )
  })

  # ===========================================================================
  # [S-8] 动态 UI：输出数据集标签页（依 rv$adam_datasets 实时渲染）
  # ===========================================================================
  output$output_dataset_tabs <- renderUI({
    dsets <- rv$adam_datasets
    if (length(dsets) == 0) {
      return(navset_card_underline(id="output_subtabs",
        nav_panel(title="数据集", value="subtab_empty",
          .placeholder_ui("table", "ADaM 数据集尚未生成",
                          "完成「代码审查与回档」流程后将在此处展示"))
      ))
    }
    panels <- lapply(names(dsets), function(ds) {
      nav_panel(
        title = tagList(bs_icon("table"), " ", toupper(ds)),
        value = paste0("subtab_", ds),
        card(card_header(layout_columns(col_widths=c(7,5),
          div(style="display:flex;align-items:center;gap:0.6rem;",
              span(paste0(toupper(ds), " Analysis Dataset")),
              uiOutput(paste0(ds, "_row_badge"))),
          div(style="text-align:right;",
              downloadButton(paste0("dl_", ds),
                tagList(bs_icon("download", size="0.8rem"),
                        paste0(" 下载 ", toupper(ds), ".csv")),
                class="btn-download"))
        )),
        DTOutput(paste0("tbl_", ds)),
        uiOutput(paste0(ds, "_placeholder"))
        )
      )
    })
    do.call(navset_card_underline, c(list(id="output_subtabs"), panels))
  })

  # ===========================================================================
  # [S-8] 动态注册：每个 adam_datasets 条目对应的 renderDT / downloadHandler
  # ===========================================================================
  observe({
    for (.ds in names(rv$adam_datasets)) {
      local({
        ds <- .ds
        output[[paste0(ds, "_row_badge")]] <- renderUI({
          df <- rv$adam_datasets[[ds]]
          if (is.null(df)) return(NULL)
          .row_badge(nrow(df))
        })
        output[[paste0("tbl_", ds)]] <- renderDT({
          df <- rv$adam_datasets[[ds]]
          req(!is.null(df))
          datatable(df, rownames=FALSE, selection="none",
                    options=.dt_options(scroll_x=TRUE, page_length=15), class="cell-border")
        }, server=TRUE)
        output[[paste0(ds, "_placeholder")]] <- renderUI({
          if (!is.null(rv$adam_datasets[[ds]])) return(NULL)
          .placeholder_ui("table", paste0(toupper(ds), " 数据集尚未生成"),
                          "完成「代码审查与回档」流程后将在此处展示")
        })
        output[[paste0("dl_", ds)]] <- downloadHandler(
          filename = function() paste0(ds, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
          content  = function(file) {
            d <- rv$adam_datasets[[ds]]
            req(!is.null(d))
            readr::write_csv(d, file, na="")
          }
        )
      })
    }
  })

}
