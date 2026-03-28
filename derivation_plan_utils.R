# =============================================================================
# derivation_plan_utils.R
# 功能：Derivation Plan 的标准化、回退生成、摘要和基础校验
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

.nz_chr <- function(x, default = NA_character_) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) default else x
}

.as_chr_vec <- function(x) {
  if (is.null(x)) return(character(0))
  unique(trimws(as.character(unlist(x, use.names = FALSE))))
}

# Known SDTM domain 2-letter codes (CDISC standard)
.SDTM_DOMAINS <- c(
  "dm","ex","ae","lb","cm","vs","mh","ds","pr","su",
  "qs","tu","rs","tr","fa","ho","dd","eg","ie","mb",
  "mi","ms","pc","pp","sc","sm","sr","ss","ec","ag",
  "ce","cv","dd","ml","nv","oe","re","rp","ur"
)

.infer_dataset_role <- function(ds) {
  ds_lc <- tolower(trimws(ds))
  # CDISC ADaM 标准数据集角色推断
  if (ds_lc == "adsl") return("subject-level")
  if (ds_lc %in% c("adae", "adcm", "admh")) return("event-level")
  if (ds_lc %in% c("adtte", "adttte")) return("time-to-event")
  if (ds_lc %in% c("adlb", "advs", "adeg")) return("findings")
  if (grepl("^ad", ds_lc)) return("analysis")
  "analysis"
}

.guess_source_domain <- function(source_text) {
  txt <- tolower(trimws(as.character(source_text %||% "")))
  if (!nzchar(txt)) return(NA_character_)
  # 跨 ADaM 数据集引用
  adam_refs <- regmatches(txt, gregexpr("\\bad[a-z]{1,4}\\b", txt, perl = TRUE))[[1]]
  adam_refs <- unique(adam_refs[nchar(adam_refs) >= 4])  # 至少 4 字符避免误匹配
  if (length(adam_refs) > 0) return(adam_refs[1])
  # Extract 2-letter tokens and match against known SDTM domain whitelist only
  hits <- regmatches(txt, gregexpr("\\b[a-z]{2}\\b", txt, perl = TRUE))[[1]]
  hits <- intersect(unique(hits), .SDTM_DOMAINS)
  if (length(hits) == 0) return(NA_character_)  # intra-dataset ADaM or unknown
  hits[1]
}

.spec_to_variable_plan <- function(spec_df) {
  if (is.null(spec_df) || !is.data.frame(spec_df) || nrow(spec_df) == 0) return(list())

  lapply(seq_len(nrow(spec_df)), function(i) {
    list(
      variable = .nz_chr(spec_df$variable[i], paste0("VAR_", i)),
      label = .nz_chr(spec_df$label[i], ""),
      type = tolower(.nz_chr(spec_df$type[i], "char")),
      source_domain = .guess_source_domain(spec_df$source[i]),
      source_columns = .as_chr_vec(spec_df$source[i]),
      derivation_rule = .nz_chr(spec_df$derivation[i], "Derived according to spec and source context."),
      depends_on = .as_chr_vec(spec_df$source[i]),
      confidence = "FALLBACK"
    )
  })
}

build_fallback_derivation_plan <- function(specs, target_datasets = NULL) {
  parsed_specs <- lapply(specs %||% list(), function(s) s$parsed %||% s)
  parsed_specs <- Filter(Negate(is.null), parsed_specs)

  if (is.null(target_datasets) || length(target_datasets) == 0) {
    target_datasets <- unique(tolower(unlist(lapply(parsed_specs, function(s) s$dataset %||% character(0)))))
  }
  if (length(target_datasets) == 0) {
    warning("[derivation_plan] 未能推断目标数据集，将使用已解析 Spec 中的数据集名。")
    target_datasets <- unique(tolower(unlist(lapply(parsed_specs, function(s) s$dataset %||% character(0)))))
    if (length(target_datasets) == 0) {
      stop("无法推断目标数据集且 Spec 中无 dataset 字段。")
    }
  }

  datasets <- lapply(target_datasets, function(ds) {
    match_idx <- which(vapply(parsed_specs, function(s) identical(tolower(s$dataset %||% ""), ds), logical(1)))
    spec <- if (length(match_idx) > 0) parsed_specs[[match_idx[1]]] else NULL
    variable_plan <- .spec_to_variable_plan(spec$variables %||% NULL)
    input_domains <- unique(na.omit(vapply(variable_plan, function(v) v$source_domain %||% NA_character_, character(1))))

    list(
      dataset = ds,
      dataset_role = .infer_dataset_role(ds),
      required_inputs = input_domains,
      join_plan = list(),
      variable_plan = variable_plan,
      assumptions = list("Fallback plan built from confirmed spec because model plan was missing or incomplete."),
      open_questions = list()
    )
  })

  list(
    plan_version = "0.1-fallback",
    generated_by = "system-fallback",
    datasets = datasets
  )
}

normalize_derivation_plan <- function(plan, specs, target_datasets = NULL) {
  fallback <- build_fallback_derivation_plan(specs, target_datasets)

  if (is.null(plan) || !is.list(plan)) return(fallback)

  plan_datasets <- plan$datasets %||% plan$plan %||% NULL
  if (is.null(plan_datasets)) return(fallback)
  if (!is.list(plan_datasets) || length(plan_datasets) == 0) return(fallback)

  norm_datasets <- lapply(plan_datasets, function(ds) {
    variable_plan <- ds$variable_plan %||% list()
    if (is.data.frame(variable_plan)) {
      variable_plan <- lapply(seq_len(nrow(variable_plan)), function(i) as.list(variable_plan[i, , drop = FALSE]))
    }

    variable_plan <- lapply(variable_plan, function(v) {
      list(
        variable = .nz_chr(v$variable, "UNKNOWN"),
        label = .nz_chr(v$label, ""),
        type = tolower(.nz_chr(v$type, "char")),
        source_domain = tolower(.nz_chr(v$source_domain, NA_character_)),
        source_columns = .as_chr_vec(v$source_columns %||% v$source %||% v$depends_on),
        derivation_rule = .nz_chr(v$derivation_rule %||% v$derivation, "Derived according to LLM plan."),
        depends_on = .as_chr_vec(v$depends_on %||% v$source_columns),
        confidence = toupper(.nz_chr(v$confidence, "MEDIUM"))
      )
    })

    list(
      dataset = tolower(.nz_chr(ds$dataset, "unknown")),
      dataset_role = .nz_chr(ds$dataset_role, "analysis"),
      required_inputs = tolower(.as_chr_vec(ds$required_inputs)),
      join_plan = ds$join_plan %||% list(),
      variable_plan = variable_plan,
      assumptions = as.list(.as_chr_vec(ds$assumptions)),
      open_questions = as.list(.as_chr_vec(ds$open_questions))
    )
  })

  missing_ds <- setdiff(vapply(fallback$datasets, function(x) x$dataset, character(1)),
                        vapply(norm_datasets, function(x) x$dataset, character(1)))
  if (length(missing_ds) > 0) {
    norm_datasets <- c(
      norm_datasets,
      Filter(function(x) x$dataset %in% missing_ds, fallback$datasets)
    )
  }

  list(
    plan_version = .nz_chr(plan$plan_version, fallback$plan_version),
    generated_by = .nz_chr(plan$generated_by, "llm"),
    datasets = norm_datasets
  )
}

summarize_derivation_plan <- function(plan) {
  datasets <- plan$datasets %||% list()
  if (length(datasets) == 0) {
    return(data.frame(
      dataset = character(), variables = integer(), inputs = character(), assumptions = integer(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(datasets, function(ds) {
    data.frame(
      dataset = toupper(ds$dataset %||% "UNKNOWN"),
      variables = length(ds$variable_plan %||% list()),
      inputs = paste(toupper(ds$required_inputs %||% character(0)), collapse = ", "),
      assumptions = length(ds$assumptions %||% list()),
      stringsAsFactors = FALSE
    )
  }))
}

validate_plan_against_spec <- function(plan, specs) {
  spec_map <- lapply(specs %||% list(), function(s) s$parsed %||% NULL)
  spec_map <- Filter(Negate(is.null), spec_map)
  spec_map <- setNames(lapply(spec_map, function(s) s$variables %||% data.frame()), tolower(vapply(spec_map, function(s) s$dataset, character(1))))

  issues <- list()
  add_issue <- function(dataset, level, detail) {
    issues[[length(issues) + 1L]] <<- data.frame(
      dataset = toupper(dataset),
      level = level,
      detail = detail,
      stringsAsFactors = FALSE
    )
  }

  for (ds in plan$datasets %||% list()) {
    ds_name <- tolower(ds$dataset %||% "")
    spec_df <- spec_map[[ds_name]] %||% NULL
    if (is.null(spec_df) || !is.data.frame(spec_df)) {
      add_issue(ds_name, "WARNING", "Plan 中存在数据集，但未找到对应 Spec。")
      next
    }

    spec_vars <- unique(trimws(as.character(spec_df$variable %||% character())))
    plan_vars <- unique(vapply(ds$variable_plan %||% list(), function(v) .nz_chr(v$variable, ""), character(1)))
    miss <- setdiff(spec_vars, plan_vars)
    if (length(miss) > 0) {
      add_issue(ds_name, "WARNING", paste0("Plan 缺少 Spec 变量：", paste(miss, collapse = ", ")))
    }
  }

  if (length(issues) == 0) {
    data.frame(dataset = character(), level = character(), detail = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, issues)
  }
}

format_derivation_plan <- function(plan) {
  ds <- plan$datasets %||% list()
  if (length(ds) == 0) return("无 derivation plan。")

  paste(vapply(ds, function(x) {
    vars <- x$variable_plan %||% list()
    input_txt <- if (length(x$required_inputs %||% character(0)) > 0) {
      paste(toupper(x$required_inputs), collapse = ", ")
    } else {
      "未指明"
    }
    paste0(
      toupper(x$dataset), ": ",
      length(vars), " 个变量；输入域=", input_txt,
      if (length(x$assumptions %||% list()) > 0) paste0("；假设=", length(x$assumptions)) else ""
    )
  }, character(1)), collapse = "\n")
}

flatten_derivation_plan <- function(plan) {
  datasets <- plan$datasets %||% list()
  if (length(datasets) == 0) {
    return(data.frame(
      dataset = character(), variable = character(), type = character(),
      source_domain = character(), source_columns = character(),
      derivation_rule = character(), confidence = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  for (ds in datasets) {
    vars <- ds$variable_plan %||% list()
    for (v in vars) {
      rows[[length(rows) + 1L]] <- data.frame(
        dataset = toupper(ds$dataset %||% "UNKNOWN"),
        variable = .nz_chr(v$variable, "UNKNOWN"),
        type = toupper(.nz_chr(v$type, "CHAR")),
        source_domain = toupper(.nz_chr(v$source_domain, "—")),
        source_columns = if (length(v$source_columns %||% character(0)) > 0) paste(v$source_columns, collapse = ", ") else "—",
        derivation_rule = .nz_chr(v$derivation_rule, "—"),
        confidence = toupper(.nz_chr(v$confidence, "MEDIUM")),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      dataset = character(), variable = character(), type = character(),
      source_domain = character(), source_columns = character(),
      derivation_rule = character(), confidence = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}
