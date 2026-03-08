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

source("data_utils.R",        local = TRUE)
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
  css <- switch(level, "ERROR"="badge-error-custom", "WARNING"="badge-warning-custom", "badge-info-custom")
  ico <- switch(level, "ERROR"="✖", "WARNING"="⚠", "ℹ")
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
  n_warn_tot <- sum(sapply(specs, function(s) s$step == "warn"))

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
    # LLM 生成结果
    llm_result    = NULL,
    risk_logs_df  = NULL,
    original_code = NULL,
    # ADaM 输出
    adsl          = NULL,
    adae          = NULL,
    # 流水线状态
    step_load     = "idle",
    step_llm      = "idle",
    step_review   = "idle",
    step_run      = "idle",
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

    # ── SDTM 文件元数据（DM / EX / AE）────────────────────────────────
    # 格式：list(name, size, upload_time, datapath, rows, cols)
    file_meta      = list(dm=NULL, ex=NULL, ae=NULL)
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
    cols        = NA_integer_
  )

  .fmt_size <- function(b) {
    if (is.na(b) || b == 0) return("—")
    if (b < 1024)    return(paste0(b, " B"))
    if (b < 1048576) return(paste0(round(b / 1024, 1), " KB"))
    paste0(round(b / 1048576, 1), " MB")
  }

  # 从文件名生成安全 ID（去扩展名 + 非字母数字替换为下划线）
  .safe_spec_id <- function(filename) {
    base <- tools::file_path_sans_ext(filename)
    gsub("[^a-zA-Z0-9]", "_", base)
  }

  # ===========================================================================
  # Observer：上传 SDTM 文件 → 更新文件元数据
  # ===========================================================================
  for (.sid in c("dm", "ex", "ae")) {
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
        }
        rv$file_meta[[sid]] <- meta
      })
    })
  }

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

    total_vars <- sum(sapply(rv$specs, function(s)
      if (!is.null(s$parsed)) nrow(s$parsed$variables) else 0L))
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
      total_vars <- sum(sapply(rv$specs, function(s)
        if (!is.null(s$parsed)) nrow(s$parsed$variables) else 0L))
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
    rv$original_code <- rv$run_result_ok <- rv$run_result_err <- NULL
    rv$log_lines <- character(0)
    rv$step_load <- "running"
    rv$step_llm  <- rv$step_review <- rv$step_run <- "idle"

    # ── [修改 G-1] 校验：包含 spec_confirmed 检查 ──────────────────────────
    missing_files <- c(
      if (is.null(input$file_dm))       "DM (dm.csv)",
      if (is.null(input$file_ex))       "EX (ex.csv)",
      if (is.null(input$file_ae))       "AE (ae.csv)",
      if (!isTRUE(rv$spec_confirmed))   "Analysis Spec（请上传 CSV 并确认解析报告）"  # [修改 G-1]
    )

    if (length(missing_files) > 0) {
      rv$step_load <- "error"
      .append_log("前置条件不满足：", paste(missing_files, collapse="、"), icon="✖")
      showNotification(
        tagList(tags$strong("⚠ 请先完成以下步骤"),
                tags$br(), paste(missing_files, collapse="、")),
        type="error", duration=7)
      return()
    }

    # ── 阶段 1：读取 SDTM ─────────────────────────────────────────────────────
    .append_log("开始读取 SDTM 文件...", icon="⬤")
    sdtm_data <- tryCatch({
      load_sdtm_data(input$file_dm$datapath,
                     input$file_ex$datapath,
                     input$file_ae$datapath)
    }, error = function(e) {
      rv$step_load <- "error"
      .append_log("读取 SDTM 失败：", conditionMessage(e), icon="✖")
      showNotification(paste0("SDTM 读取错误：", conditionMessage(e)), type="error", duration=8)
      NULL
    })
    if (is.null(sdtm_data)) return()

    rv$sdtm      <- sdtm_data
    rv$step_load <- "done"
    .append_log(sprintf("SDTM 加载完成  DM=%d行  EX=%d行  AE=%d行",
                        nrow(sdtm_data$dm), nrow(sdtm_data$ex), nrow(sdtm_data$ae)), icon="✔")

    # ── [修改 G-2] 阶段 2：直接使用 rv$spec_parsed（原：load_spec_json()）──
    .append_log("加载已确认的 Spec 解析结果...", icon="⬤")  # [修改 G-2]
    # Combine all confirmed specs into a JSON array (one object per file)
    spec_list     <- lapply(rv$specs, function(s) s$parsed)
    spec_json_str <- jsonlite::toJSON(spec_list, pretty=TRUE, auto_unbox=TRUE)
    total_vars    <- sum(sapply(spec_list, function(s)
                      if (!is.null(s$variables)) nrow(s$variables) else 0L))
    .append_log(sprintf("规格加载完成  %d 个数据集  共 %d 个变量",
                        length(spec_list), total_vars), icon="✔")

    # ── 阶段 3：调用 LLM 生成代码 ─────────────────────────────────────────────
    rv$step_llm <- "running"

    # ── 从 UI 获取模型/Key/本地配置，构建故障转移调用链 ─────────────────────
    model_val    <- input$llm_model %||% "gpt-4o"
    primary_prov <- .infer_provider(model_val)
    actual_model <- if (primary_prov %in% c("ollama", "vllm"))
                      trimws(input$local_model_name %||% "") else model_val
    api_key_val  <- trimws(input$api_key %||% "")

    # 云端提供商需要 Key；本地不需要
    prov_cfg <- .get_provider_cfg(primary_prov)
    if (prov_cfg$needs_key && nchar(api_key_val) == 0 && !isTRUE(MOCK_MODE)) {
      rv$step_llm <- "error"
      .append_log("未填写 API Key，无法调用 LLM", icon="✖")
      showNotification(
        tagList(tags$strong("⚠ 请填写 API Key"),
                tags$br(), "在侧边栏「LLM API 配置」中输入有效的 API Key 后重试。"),
        type="error", duration=7)
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
      provider_key_map[[fb]] <- trimws(input$failover_api_key_1 %||% "")
      failover_chain <- c(failover_chain,
                          list(list(provider = fb, model = .default_model(fb))))
    }

    .append_log("正在调用 LLM 引擎（",
                if (MOCK_MODE) "模拟模式" else paste0(actual_model, " · ", primary_prov),
                "）...", icon="⬤")

    llm_res <- tryCatch({
      call_llm_engine_with_failover(
        spec_json        = spec_json_str,
        data_summary     = summarize_sdtm(sdtm_data),
        provider_key_map = provider_key_map,
        failover_chain   = failover_chain,
        sdtm_list        = sdtm_data,
        base_url_map     = base_url_map
      )
    }, error = function(e) {
      rv$step_llm <- "error"
      .append_log("LLM 调用失败：", conditionMessage(e), icon="✖")
      showNotification(paste0("LLM 错误：", conditionMessage(e)), type="error", duration=8)
      NULL
    })
    if (is.null(llm_res)) return()

    rv$llm_result    <- llm_res
    rv$step_llm      <- "done"
    rv$risk_logs_df  <- .normalize_risk_logs(llm_res$risk_logs)
    rv$original_code <- llm_res$r_code

    n_risks <- if (!is.null(rv$risk_logs_df)) nrow(rv$risk_logs_df) else 0
    .append_log(sprintf("LLM 返回完成  代码长度=%d字符  风险点=%d条",
                        nchar(llm_res$r_code %||% ""), n_risks), icon="✔")

    updateAceEditor(session, "code_editor", value=llm_res$r_code)
    rv$step_review <- "running"
    .append_log("代码已填入编辑器，请切换至「代码审查与回档」Tab 检查。", icon="→")

    nav_select(id = "main_tabs", selected = "tab_code", session = session)
    showNotification(
      tagList(tags$strong("✔ LLM 生成完成"), tags$br(),
              paste0("识别到 ", n_risks, " 条风险点，请审阅 Tab 2 中的代码")),
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

    # ── 1. 重置前端 fileInput 控件（用户可见的文件名 + 进度条）──────────────
    # shinyjs::reset() 的参数是 fileInput 的 inputId 字符串
    shinyjs::reset("file_dm")
    shinyjs::reset("file_ex")
    shinyjs::reset("file_ae")
    shinyjs::reset("file_spec")

    # ── 2. 清空所有后台状态变量 ───────────────────────────────────────────────
    rv$file_meta      <- list(dm=NULL, ex=NULL, ae=NULL)  # SDTM 文件元数据
    rv$specs          <- list()   # 所有 Spec 文件解析记录
    rv$spec_confirmed <- FALSE
    rv$step_parse     <- "idle"
    rv$sdtm           <- NULL
    rv$step_load      <- "idle"

    # 同时清空下游结果（避免旧数据残留在 Tab 3）
    rv$adsl           <- NULL
    rv$adae           <- NULL
    rv$llm_result     <- NULL
    rv$risk_logs_df   <- NULL
    rv$original_code  <- NULL
    rv$run_result_ok  <- NULL
    rv$run_result_err <- NULL
    rv$log_lines      <- character(0)

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

      # 云端提供商：API Key 输入框
      if (needs_key) {
        tagList(
          passwordInput("api_key", paste0(prov_name, " API Key"), placeholder="sk-..."),
          div(class="hint-text", "Key 仅存于当前会话内存，不会被持久化或传输给第三方")
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
          passwordInput("failover_api_key_1", "备用 API Key", placeholder="sk-...")
        )
      )
    )
  })

  # ===========================================================================
  # Observer：单文件删除按钮（4 个槽位）
  # ===========================================================================
  # Static remove-file buttons for DM / EX / AE
  for (.sid in c("dm", "ex", "ae")) {
    local({
      sid <- .sid
      observeEvent(input[[paste0("btn_remove_", sid)]], {
        shinyjs::reset(paste0("file_", sid))
        rv$file_meta[[sid]] <- NULL
        if (!is.null(rv$adsl) || !is.null(rv$adae)) {
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

    if (!is.null(rv$adsl) || !is.null(rv$adae)) {
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
    rv$run_result_ok <- rv$run_result_err <- NULL
    rv$adsl <- rv$adae <- NULL
    .append_log("开始执行用户确认的代码...", icon="▶")

    exec_env <- new.env(parent=baseenv())
    exec_env$dm <- rv$sdtm$dm
    exec_env$ex <- rv$sdtm$ex
    exec_env$ae <- rv$sdtm$ae

    for (pkg in c("dplyr","lubridate","stringr","tidyr","readr")) {
      if (requireNamespace(pkg, quietly=TRUE)) {
        for (fn in getNamespaceExports(pkg)) {
          tryCatch(assign(fn, getExportedValue(pkg,fn), envir=exec_env), error=function(e) NULL)
        }
      }
    }
    exec_env$strip_excel_apos <- strip_excel_apos
    exec_env$dy_char          <- dy_char

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

    adsl_r <- tryCatch(get("adsl", envir=exec_env), error=function(e) NULL)
    adae_r <- tryCatch(get("adae", envir=exec_env), error=function(e) NULL)

    errs <- c(
      if (is.null(adsl_r)||!is.data.frame(adsl_r)) "代码未生成名为 'adsl' 的 data.frame",
      if (is.null(adae_r)||!is.data.frame(adae_r)) "代码未生成名为 'adae' 的 data.frame"
    )
    if (length(errs)>0) {
      err_msg <- paste(errs, collapse="\n")
      rv$step_run <- "error"
      rv$run_result_err <- err_msg
      .append_log(err_msg, icon="✖")
      showNotification(tagList(tags$strong("✖ 结果提取失败"), tags$br(), err_msg),
                       type="error", duration=8)
      return()
    }

    rv$adsl <- adsl_r; rv$adae <- adae_r
    rv$step_run <- "done"; rv$step_review <- "done"
    rv$run_result_ok <- sprintf("执行成功  ADSL=%d×%d  ADAE=%d×%d",
                                nrow(adsl_r),ncol(adsl_r),nrow(adae_r),ncol(adae_r))
    .append_log(rv$run_result_ok, icon="✔")
    showNotification(
      tagList(tags$strong("✔ ADaM 生成成功"), tags$br(),
              sprintf("ADSL: %d行  ADAE: %d行", nrow(adsl_r), nrow(adae_r))),
      type="message", duration=5)
    nav_select(id = "main_tabs", selected = "tab_output", session = session)
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
      list(label="执行代码",   state=rv$step_run)
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

    # ── Static SDTM slots (DM / EX / AE) ────────────────────────────────────
    sdtm_slots <- list(
      list(sid="dm", label="DM"),
      list(sid="ex", label="EX"),
      list(sid="ae", label="AE")
    )
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

  output$run_status <- renderText({
    if (length(rv$log_lines)==0) return("# 等待操作... 上传文件后点击「生成 ADaM 与代码」开始")
    paste(rv$log_lines, collapse="\n")
  })

  output$vb_n_adsl    <- renderText(if(is.null(rv$adsl)) "—" else formatC(nrow(rv$adsl),big.mark=","))
  output$vb_n_adae    <- renderText(if(is.null(rv$adae)) "—" else formatC(nrow(rv$adae),big.mark=","))
  output$vb_n_risks   <- renderText({
    if (is.null(rv$risk_logs_df)) return("—")
    n_e <- sum(rv$risk_logs_df$level=="ERROR")
    if (n_e>0) paste0(nrow(rv$risk_logs_df)," (",n_e," ERR)") else as.character(nrow(rv$risk_logs_df))
  })
  output$vb_llm_status <- renderText(switch(rv$step_llm,"idle"="待机","running"="推理中","done"="完成","error"="失败","待机"))

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
    if (rv$step_review=="running")
      return(div(style="font-size:0.78rem;color:#8b949e;align-self:center;",
                 bsicons::bs_icon("pencil-square",color="#8b949e"), " 请审阅上方代码，确认无误后点击右侧按钮执行"))
    div(style="font-size:0.78rem;color:#6e7681;align-self:center;", "等待 LLM 生成代码...")
  })

  output$adsl_row_badge <- renderUI({ if(is.null(rv$adsl)) NULL else .row_badge(nrow(rv$adsl)) })
  output$adae_row_badge <- renderUI({ if(is.null(rv$adae)) NULL else .row_badge(nrow(rv$adae)) })

  output$tbl_adsl <- renderDT({
    req(!is.null(rv$adsl))
    datatable(rv$adsl, rownames=FALSE, selection="none", filter="top",
              options=.dt_options(scroll_x=TRUE,page_length=15), class="cell-border")
  }, server=TRUE)

  output$adsl_placeholder <- renderUI({
    if (!is.null(rv$adsl)) return(NULL)
    .placeholder_ui("person-lines-fill","ADSL 数据集尚未生成","完成「代码审查与回档」流程后将在此处展示")
  })

  output$tbl_adae <- renderDT({
    req(!is.null(rv$adae))
    datatable(rv$adae, rownames=FALSE, selection="none", filter="top",
              options=.dt_options(scroll_x=TRUE,page_length=15), class="cell-border")
  }, server=TRUE)

  output$adae_placeholder <- renderUI({
    if (!is.null(rv$adae)) return(NULL)
    .placeholder_ui("clipboard2-x","ADAE 数据集尚未生成","完成「代码审查与回档」流程后将在此处展示")
  })

  output$dl_adsl <- downloadHandler(
    filename = function() paste0("adsl_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv"),
    content  = function(file) { req(!is.null(rv$adsl)); readr::write_csv(rv$adsl,file,na="") }
  )
  output$dl_adae <- downloadHandler(
    filename = function() paste0("adae_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv"),
    content  = function(file) { req(!is.null(rv$adae)); readr::write_csv(rv$adae,file,na="") }
  )
}
