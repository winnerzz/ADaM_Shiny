# =============================================================================
# code_static_checks.R
# 功能：在执行前对 LLM 生成的 R 代码做轻量静态检查
# =============================================================================

run_code_static_checks <- function(code_str,
                                   expected_datasets  = character(),
                                   allowed_packages   = character(),
                                   available_inputs   = character(),
                                   available_columns  = list()) {
  issues <- list()

  .count_target_assignments <- function(code, dataset) {
    pattern <- paste0("(?i)(^|[^A-Za-z0-9_.])", dataset, "\\s*<-(?!=)")
    matches <- gregexpr(pattern, code, perl = TRUE)[[1]]
    if (length(matches) == 1 && identical(matches, -1L)) return(0L)
    length(matches)
  }

  add_issue <- function(level, check, detail, action) {
    issues[[length(issues) + 1L]] <<- data.frame(
      level = level,
      check = check,
      detail = detail,
      action = action,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(code_str) || !nzchar(trimws(code_str))) {
    add_issue("ERROR", "空代码", "当前编辑器中没有可执行代码。", "先完成生成，或恢复为原始代码。")
  }

  parsed <- tryCatch(parse(text = code_str), error = identity)
  if (inherits(parsed, "error")) {
    add_issue("ERROR", "语法解析", conditionMessage(parsed), "先修复语法错误，再执行代码。")
  }

  banned_patterns <- list(
    list("危险调用", "(?i)\\b(system|shell|shell\\.exec)\\s*\\(", "移除系统命令调用。"),
    list("文件写入", "(?i)\\b(write\\.(csv|table)|saveRDS|save|sink)\\s*\\(", "移除文件写入或重定向行为。"),
    list("外部脚本", "(?i)\\bsource\\s*\\(", "避免在生成代码中再 source 外部脚本。"),
    list("安装依赖", "(?i)\\binstall\\.packages\\s*\\(", "依赖应由应用环境预装，不在生成代码中安装。"),
    # VPS 部署安全加固：检测网络访问调用（已在沙箱中屏蔽，此处提前告警）
    list("网络访问", "(?i)\\b(httr2|curl|request|req_perform|download\\.file|GET|POST)\\s*[:(]", "代码执行环境中禁止网络访问，请移除相关调用。"),
    list("环境变量读取", "(?i)\\bSys\\.getenv\\s*\\(", "代码执行环境中禁止读取环境变量（可能包含 API Key），请移除。")
  )
  for (rule in banned_patterns) {
    if (grepl(rule[[2]], code_str, perl = TRUE)) {
      add_issue("ERROR", rule[[1]], paste0("检测到模式：", rule[[2]]), rule[[3]])
    }
  }

  lib_calls <- unique(c(
    regmatches(code_str, gregexpr("(?<=library\\()\\s*[A-Za-z][A-Za-z0-9._]*", code_str, perl = TRUE))[[1]],
    regmatches(code_str, gregexpr("(?<=require\\()\\s*[A-Za-z][A-Za-z0-9._]*", code_str, perl = TRUE))[[1]]
  ))
  lib_calls <- trimws(lib_calls[nzchar(lib_calls)])
  if (length(lib_calls) > 0) {
    disallowed <- setdiff(tolower(lib_calls), tolower(allowed_packages))
    if (length(disallowed) > 0) {
      add_issue(
        "ERROR", "额外依赖",
        paste0("代码引用了未列入允许清单的包：", paste(disallowed, collapse = ", ")),
        "沙箱不提供该包函数，代码会运行失败。请改用允许包或移除该依赖。"
      )
    }
  }

  if (length(expected_datasets) > 0) {
    missing_defs <- expected_datasets[!vapply(expected_datasets, function(ds) {
      pattern <- paste0("(?i)\\b", ds, "\\s*<-(?!=)|\\b", ds, "\\s*=\\s*(dplyr::)?(mutate|transmute|summarise|summarize|left_join|right_join|inner_join|full_join|data\\.frame|tibble)")
      grepl(pattern, code_str, perl = TRUE)
    }, logical(1))]
    if (length(missing_defs) > 0) {
      add_issue(
        "ERROR", "缺少目标输出",
        paste0("代码中未发现预期数据集定义：", paste(toupper(missing_defs), collapse = ", ")),
        "确保代码显式创建与 Spec 一致的数据集对象。"
      )
    }

    duplicate_defs <- expected_datasets[vapply(expected_datasets, function(ds) {
      .count_target_assignments(code_str, ds) > 1L
    }, logical(1))]
    if (length(duplicate_defs) > 0) {
      details <- vapply(duplicate_defs, function(ds) {
        paste0(toupper(ds), "=", .count_target_assignments(code_str, ds), " 次")
      }, character(1))
      add_issue(
        "WARNING", "重复定义目标输出",
        paste0("检测到目标数据集被重复赋值：", paste(details, collapse = "；"), "。这会导致前面生成的结果被后续代码覆盖。"),
        "确保每个目标数据集只在最终版本中赋值一次；如需中间步骤，请使用临时对象名。"
      )
    }
  }

  if (length(available_inputs) > 0) {
    referenced_inputs <- unique(regmatches(code_str, gregexpr("\\b[a-z]{2}\\b", tolower(code_str), perl = TRUE))[[1]])
    likely_domains <- intersect(referenced_inputs, c("dm","ex","ae","vs","lb","cm","mh","sv","eg","pe","tu","rs","mb"))
    missing_inputs <- setdiff(likely_domains, tolower(available_inputs))
    if (length(missing_inputs) > 0) {
      add_issue(
        "WARNING", "输入依赖",
        paste0("代码似乎引用了未上传域：", paste(toupper(missing_inputs), collapse = ", ")),
        "确认这些域是否真的需要，或先补齐输入。"
      )
    }
  }

  # ── 新增检查 1：SDTM 域名大小写 ────────────────────────────────────────────
  # LLM 常写 DM/EX/AE，但 exec_env 中只有小写对象 dm/ex/ae
  local({
    pd <- tryCatch(
      getParseData(parse(text = code_str, keep.source = TRUE)),
      error = function(e) NULL
    )
    if (is.null(pd)) return()
    upper_domains <- c("DM","EX","AE","VS","LB","CM","MH","SV","EG","PE","TU","RS","MB","DS","PR","SU")
    bad <- unique(pd$text[pd$token == "SYMBOL" & pd$text %in% upper_domains])
    if (length(bad) > 0)
      add_issue("ERROR", "域名大小写",
        paste0("检测到大写 SDTM 域对象名：", paste(bad, collapse = ", ")),
        "执行环境中域对象名为小写，如 dm / ex / ae，请统一改为小写。")
  })

  # ── 新增检查 2：原始日期解析（禁止绕过 parse_sdtm_date）────────────────────
  # 检测 ymd()/as.Date() 直接作用于大写列名或域$列名 —— 未经 strip_excel_apos 处理
  local({
    # 匹配：ymd(COLNAME) / as.Date(COLNAME) / ymd(dm$COLNAME) 等
    pat <- "(?:as\\.Date|\\bymd\\b|\\bmdy\\b|\\bdmy\\b)\\s*\\(\\s*(?:[a-z]{2}\\$)?(?!NA\\b|NULL\\b|TRUE\\b|FALSE\\b|Inf\\b|NaN\\b|NA_)[A-Z][A-Z0-9_]*(?:DTC|DT)?\\s*\\)"
    hits <- unique(regmatches(code_str, gregexpr(pat, code_str, perl = TRUE))[[1]])
    if (length(hits) > 0)
      add_issue("ERROR", "原始日期解析",
        paste0("检测到直接解析 SDTM 日期（未经 parse_sdtm_date）：", paste(head(hits, 3), collapse = "；")),
        "SDTM 日期可能含 Excel 前置单引号，只能使用 parse_sdtm_date(x) 或 study_day_chr() 处理。")
  })

  # ── 新增检查 3：手工 Study Day 算术（禁止手写日期差值）───────────────────────
  # 检测 ymd(x) - ymd(y) 或 as.Date(x) - as.Date(y) 模式
  local({
    pat <- "(?:parse_sdtm_date|ymd|as\\.Date|mdy|dmy)\\s*\\([^)]+\\)\\s*[-+]\\s*(?:parse_sdtm_date|ymd|as\\.Date|mdy|dmy)\\s*\\([^)]+\\)"
    if (grepl(pat, code_str, perl = TRUE))
      add_issue("ERROR", "手工研究日算术",
        "检测到手写日期差值运算（如 ymd(x) - ymd(ref)）。",
        "Study Day 变量只能使用 study_day_chr(date_chr, ref_chr) 计算，禁止手动写日期差值。")
  })

  # ── 新增检查 4：adsl / adae 定义顺序 ──────────────────────────────────────
  # adae 若 left_join(adsl) 但 adsl 尚未定义，会导致 object not found
  local({
    adsl_pos <- regexpr("\\badsl\\s*<-", code_str, perl = TRUE)[[1]]
    adae_pos <- regexpr("\\badae\\s*<-", code_str, perl = TRUE)[[1]]
    if (adsl_pos > 0L && adae_pos > 0L && adae_pos < adsl_pos)
      add_issue("ERROR", "输出定义顺序",
        "检测到 ADAE 在 ADSL 之前定义，而 ADAE 通常依赖 ADSL（left_join）。",
        "将 adsl <- ... 的赋值移动到 adae <- ... 之前。")
  })

  # ── 新增检查 5：domain$COLUMN 形式的未定义列引用 ─────────────────────────────
  # 仅在 available_columns 非空时执行，避免误报
  if (length(available_columns) > 0) {
    local({
      m <- stringr::str_match_all(code_str, "\\b([a-z]{2,3})\\$([A-Z][A-Z0-9_]*)\\b")[[1]]
      if (nrow(m) == 0) return()
      refs <- unique(data.frame(domain = m[, 2], col = m[, 3], stringsAsFactors = FALSE))
      bad_rows <- refs[!mapply(function(d, c) {
        cols <- available_columns[[d]]
        if (is.null(cols)) return(TRUE)   # 域不存在 — 也是问题，但由输入依赖检查覆盖
        c %in% cols
      }, refs$domain, refs$col), ]
      if (nrow(bad_rows) > 0) {
        detail <- paste0(bad_rows$domain, "$", bad_rows$col)
        add_issue("WARNING", "未定义列引用",
          paste0("以下域列引用在实际 SDTM 数据中不存在：", paste(head(detail, 8), collapse = ", ")),
          "请核对 SDTM 域的实际列名（区分大小写），或查看第四步预览数据。")
      }
    })
  }

  # ── 新增检查 6：C7 最终 select() 裁剪 ────────────────────────────────────────
  # 检测每个目标数据集的管道是否以 select() 结尾（防止源列泄露）
  if (length(expected_datasets) > 0) {
    local({
      for (ds in expected_datasets) {
        # 提取 ds <- ... 的整个赋值块（从赋值到下一个顶级赋值或文件结尾）
        pat <- paste0("(?s)\\b", ds, "\\s*<-(?!=)(.+?)(?=\\n[a-z]\\w*\\s*<-|\\Z)")
        m <- regmatches(code_str, regexpr(pat, code_str, perl = TRUE))
        if (length(m) == 1 && nzchar(m)) {
          # 检查是否包含 select( 调用
          if (!grepl("\\bselect\\s*\\(", m, perl = TRUE)) {
            add_issue("WARNING", "C7-缺少select",
              paste0(toupper(ds), " 管道未包含 select()，可能导致输出含 SDTM 源列。"),
              "在管道末尾添加 select() 仅保留 Spec 声明的变量（参照 C7 契约）。")
          }
        }
      }
    })
  }

  # ── 新增检查 7：标量 && / || 在向量化上下文中 ──────────────────────────────────
  local({
    lines <- strsplit(code_str, "\n")[[1]]
    for (i in seq_along(lines)) {
      ln <- lines[i]
      if (grepl("^\\s*#", ln)) next  # 跳过注释行
      if (grepl("&&|\\|\\|", ln, perl = TRUE)) {
        # if/while/else if 中的 &&/|| 是合法的
        if (grepl("^\\s*(if|while|\\}\\s*else\\s+if)\\s*\\(", ln, perl = TRUE)) next
        # yn_flag 同行有 && 一定是错误（不用 [^)]* 因为嵌套括号会打断匹配）
        if (grepl("yn_flag", ln, fixed = TRUE) && grepl("&&", ln, fixed = TRUE)) {
          add_issue("ERROR", "C8-标量运算符",
            paste0("第 ", i, " 行：yn_flag() 内使用了 && 标量运算符，只判断首元素，所有行将得到相同结果。"),
            "将 && 改为 & ，将 || 改为 | 。")
        } else {
          add_issue("WARNING", "C8-标量运算符",
            paste0("第 ", i, " 行：检测到 && 或 || 标量运算符，在 mutate/filter 上下文中可能导致只判断首元素。"),
            "确认是否应使用向量化运算符 & / | 。")
        }
      }
    }
  })

  # ── 新增检查 8：关键日期变量直接赋值（未经 parse_sdtm_date）─────────────────
  # 注意：RANDDT、EOSDT、DTHDT 等在 Spec 中可能定义为 character ISO date，直接赋值是合法的。
  # 仅对参与下游日期运算的关键变量（TRTSDT、TRTEDT）报 ERROR。
  local({
    # 严格模式：TRTSDT/TRTEDT 必须是 Date 对象
    critical_pat <- "\\b(TRTSDT|TRTEDT)\\s*=\\s*(?!parse_sdtm_date\\s*\\()(?!coalesce\\s*\\()(?!min\\s*\\()(?!max\\s*\\()([A-Z][A-Z0-9]*DTC)\\b"
    m <- gregexpr(critical_pat, code_str, perl = TRUE)[[1]]
    if (length(m) > 0 && as.integer(m[1]) > 0L) {
      hits <- unique(regmatches(code_str, list(m))[[1]])
      if (length(hits) > 0) {
        add_issue("ERROR", "C2-关键日期未解析",
          paste0("关键日期变量直接赋原始字符串(影响下游日期运算): ", paste(head(hits, 5), collapse = "; ")),
          "TRTSDT/TRTEDT 必须通过 parse_sdtm_date() 解析为 Date 对象。")
      }
    }
  })

  # ── 检查 9：TRTEMFL 实现方式（语义检查，非实现强制）──────────────────────
  local({
    has_trtemfl <- grepl("\\bTRTEMFL\\b", code_str, perl = TRUE)
    if (has_trtemfl) {
      if (grepl("\\bTRTEMFL\\s*=\\s*yn_flag\\s*\\(", code_str, perl = TRUE)) {
        # yn_flag 用于 TRTEMFL：合法但可能不够完整（缺上界检查）
        add_issue("WARNING", "TRTEMFL-简化实现",
          "TRTEMFL 使用了 yn_flag()，仅检查起始日期。如需包含上界/开放AE边界检查，建议改用 derive_trtemfl()。",
          "简单场景可接受；复杂场景建议 derive_trtemfl(start, end, trtsdt, trtedt)。")
      } else if (!grepl("\\bderive_trtemfl\\b", code_str, perl = TRUE) &&
                 !grepl("\\byn_flag\\b", code_str, perl = TRUE)) {
        add_issue("WARNING", "TRTEMFL-手写逻辑",
          "代码中定义了 TRTEMFL 但未使用 derive_trtemfl() 或 yn_flag()，可能使用了手写逻辑。",
          "建议使用预注入的辅助函数以确保一致性。")
      }
    }
  })

  # ── 检查 10：ADSL 中 TRTSDT/TRTEDT 是否被合法派生（语义检查）──────────────────
  if ("adsl" %in% expected_datasets) {
    local({
      # 提取 adsl 赋值块（从 adsl <- 到下一个顶级赋值，如 adae <-）
      adsl_pat <- "(?s)\\badsl\\s*<-(.+?)(?=\\n(?:adae|ae_joined)\\s*<-|\\Z)"
      m <- regmatches(code_str, regexpr(adsl_pat, code_str, perl = TRUE))
      if (length(m) == 1 && nzchar(m)) {
        window <- m
        has_trtsdt <- grepl("TRTSDT", window, fixed = TRUE)
        has_trtedt <- grepl("TRTEDT", window, fixed = TRUE)
        has_parse  <- grepl("parse_sdtm_date", window, fixed = TRUE)

        missing <- character(0)
        if (!has_trtsdt) missing <- c(missing, "TRTSDT")
        if (!has_trtedt) missing <- c(missing, "TRTEDT")

        if (length(missing) > 0) {
          add_issue("ERROR", "ADSL缺少治疗日期",
            paste0("ADSL 数据流中未找到 ", paste(missing, collapse = "、"),
                   " 的派生。这将导致治疗日期为空，影响所有下游计算。"),
            "TRTSDT/TRTEDT 可从 EX 汇总(min/max)或 DM(RFXSTDTC/RFXENDTC)派生，但必须经 parse_sdtm_date()。")
        } else if (!has_parse) {
          add_issue("WARNING", "治疗日期未经解析",
            "TRTSDT/TRTEDT 存在但数据流中未出现 parse_sdtm_date()，日期可能为原始字符串。",
            "确保治疗日期通过 parse_sdtm_date() 解析为 Date 类型。")
        }
      }
    })
  }

  issues_df <- if (length(issues) == 0) {
    data.frame(level = character(), check = character(), detail = character(), action = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, issues)
  }

  n_error <- sum(issues_df$level == "ERROR")
  n_warn <- sum(issues_df$level == "WARNING")
  status <- if (n_error > 0) "ERROR" else if (n_warn > 0) "WARNING" else "PASS"

  list(
    summary = list(status = status, errors = n_error, warnings = n_warn),
    issues = issues_df
  )
}

# =============================================================================
# sanitize_llm_code()
# 对 LLM 生成代码做安全的自动修正（仅限高确信度的文本替换）
# =============================================================================
sanitize_llm_code <- function(code_str) {
  if (is.null(code_str) || !nzchar(trimws(code_str))) return(code_str)

  # ── 阶段 1：移除对预注入辅助函数的重新定义 ──────────────────────────
  helper_names <- c("parse_sdtm_date", "study_day_chr", "map_trt_num",
                    "first_non_missing_chr", "yn_flag", "derive_trtemfl",
                    "derive_relgr1", "strip_excel_apos", "dy_char")
  for (fn in helper_names) {
    # 匹配 fn <- function(...) { ... } 块（含多行函数体）
    pat <- paste0("(?m)^", fn, "\\s*<-\\s*function\\s*\\([^)]*\\)\\s*\\{[^}]*\\}\\s*\n?")
    if (grepl(pat, code_str, perl = TRUE)) {
      code_str <- sub(pat, "", code_str, perl = TRUE)
      message("[sanitize] 移除重复定义: ", fn, "()")
    }
  }

  # ── 阶段 2：&& / || → & / | ──────────────────────────────────────────
  lines <- strsplit(code_str, "\n")[[1]]
  changed <- 0L
  for (i in seq_along(lines)) {
    ln <- lines[i]
    # 跳过注释行
    if (grepl("^\\s*#", ln)) next
    # 跳过 if/while/else if 控制流（&& 在此合法）
    if (grepl("^\\s*(if|while|\\}\\s*else\\s+if)\\s*\\(", ln, perl = TRUE)) next
    # 替换 && → & 和 || → |
    new_ln <- gsub("&&", "&", ln, fixed = TRUE)
    new_ln <- gsub("\\|\\|", "|", new_ln)
    if (!identical(new_ln, ln)) {
      lines[i] <- new_ln
      changed <- changed + 1L
    }
  }
  if (changed > 0L) message("[sanitize] 自动修正 ", changed, " 行中的 &&/|| → &/|")
  paste(lines, collapse = "\n")
}
