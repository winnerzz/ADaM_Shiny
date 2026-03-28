# =============================================================================
# validation_utils.R
# 功能：对生成后的 ADaM 数据集执行结构、质量与 plan 语义校验
# 说明：校验结果以结构化 list 返回，由 server.R 决定如何展示
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

.infer_key_columns <- function(dataset_name, col_names) {
  ds <- tolower(trimws(dataset_name))
  # Subject-level 数据集：USUBJID 唯一
  if (ds == "adsl") return(intersect("USUBJID", col_names))
  # Event-level 数据集：USUBJID + 序号列
  seq_col <- intersect(c("AESEQ", "CMSEQ", "MHSEQ", "EXSEQ", "LBSEQ", "VSSEQ", "EGSEQ"), col_names)
  if (length(seq_col) > 0 && "USUBJID" %in% col_names) {
    return(c("USUBJID", seq_col[1]))
  }
  # 有 PARAMCD 的 BDS 数据集：USUBJID + PARAMCD + AVISIT (或 ADT)
  if (all(c("USUBJID", "PARAMCD") %in% col_names)) {
    visit_col <- intersect(c("AVISIT", "ADT", "AVISITN"), col_names)
    if (length(visit_col) > 0) return(c("USUBJID", "PARAMCD", visit_col[1]))
    return(c("USUBJID", "PARAMCD"))
  }
  character(0)
}

.missing_mask <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  is.na(x) | trimws(as.character(x)) == ""
}

.add_issue <- function(issues, dataset, level, check, detail, action) {
  issues[[length(issues) + 1L]] <- data.frame(
    dataset = dataset,
    level   = level,
    check   = check,
    detail  = detail,
    action  = action,
    stringsAsFactors = FALSE
  )
  issues
}

.collapse_issues <- function(issues) {
  if (length(issues) == 0) {
    return(data.frame(
      dataset = character(),
      level   = character(),
      check   = character(),
      detail  = character(),
      action  = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, issues)
}

.parseable_numeric_ratio <- function(x) {
  x <- as.character(x)
  miss <- .missing_mask(x)
  vals <- x[!miss]
  if (length(vals) == 0) return(1)
  mean(!is.na(suppressWarnings(as.numeric(vals))))
}

.valid_date_ratio <- function(x) {
  x <- as.character(x)
  miss <- .missing_mask(x)
  vals <- x[!miss]
  if (length(vals) == 0) return(1)
  mean(grepl("^\\d{4}-\\d{2}-\\d{2}$", vals))
}

.collect_spec_map <- function(specs) {
  parsed_specs <- lapply(specs, function(s) s$parsed)
  parsed_specs <- Filter(Negate(is.null), parsed_specs)

  spec_map <- list()
  for (spec in parsed_specs) {
    ds <- tolower(trimws(spec$dataset %||% ""))
    if (nchar(ds) == 0) next
    spec_map[[ds]] <- spec$variables
  }
  spec_map
}

.collect_plan_map <- function(plan) {
  datasets <- plan$datasets %||% list()
  if (length(datasets) == 0) return(list())
  setNames(datasets, tolower(vapply(datasets, function(ds) ds$dataset %||% "", character(1))))
}

.plan_expected_vars <- function(plan_ds) {
  vars <- vapply(plan_ds$variable_plan %||% list(), function(v) trimws(as.character(v$variable %||% "")), character(1))
  vars[nzchar(vars)]
}

.validate_dataset <- function(dataset_name, df, spec_df = NULL, plan_ds = NULL) {
  issues <- list()
  ds_key <- tolower(dataset_name)

  if (!is.data.frame(df)) {
    issues <- .add_issue(
      issues, dataset_name, "ERROR", "对象类型",
      "输出对象不是 data.frame，无法作为 ADaM 数据集展示或下载。",
      "检查生成代码中的最终对象类型。"
    )
    return(list(
      issues = .collapse_issues(issues),
      stats = data.frame(dataset = dataset_name, rows = NA_integer_, cols = NA_integer_,
                         errors = 1L, warnings = 0L, status = "ERROR",
                         stringsAsFactors = FALSE)
    ))
  }

  if (nrow(df) == 0) {
    issues <- .add_issue(
      issues, dataset_name, "ERROR", "空数据集",
      "数据集已生成，但行数为 0。",
      "检查筛选条件、关联键和派生逻辑。"
    )
  }

  if (!is.null(spec_df) && is.data.frame(spec_df) && nrow(spec_df) > 0) {
    expected_vars <- unique(trimws(as.character(spec_df$variable %||% character())))
    expected_vars <- expected_vars[nzchar(expected_vars)]

    missing_vars <- setdiff(expected_vars, names(df))
    extra_vars   <- setdiff(names(df), expected_vars)

    if (length(missing_vars) > 0) {
      issues <- .add_issue(
        issues, dataset_name, "ERROR", "缺失变量",
        paste0("缺少 Spec 中声明的变量：", paste(missing_vars, collapse = ", ")),
        "检查代码输出列名与 Spec 是否一致。"
      )
    }

    if (length(extra_vars) > 0) {
      issues <- .add_issue(
        issues, dataset_name, "WARNING", "额外变量",
        paste0("输出中存在 Spec 未声明变量：", paste(extra_vars, collapse = ", ")),
        "确认这些变量是否应保留，或更新 Spec。"
      )
    }

    ordered_expected <- intersect(expected_vars, names(df))
    if (length(ordered_expected) > 1 &&
        !identical(names(df)[match(ordered_expected, names(df))], ordered_expected)) {
      issues <- .add_issue(
        issues, dataset_name, "WARNING", "变量顺序",
        "输出变量顺序与 Spec 声明顺序不一致。",
        "如提交或下游依赖变量顺序，请按 Spec 重新排序。"
      )
    }

    for (i in seq_len(nrow(spec_df))) {
      var_name <- trimws(as.character(spec_df$variable[i] %||% ""))
      if (!nzchar(var_name) || !var_name %in% names(df)) next

      x <- df[[var_name]]
      miss_rate <- mean(.missing_mask(x))
      spec_type <- tolower(trimws(as.character(spec_df$type[i] %||% "")))

      if (identical(var_name, "USUBJID") && any(.missing_mask(x))) {
        issues <- .add_issue(
          issues, dataset_name, "ERROR", "关键字段缺失",
          sprintf("变量 %s 存在 %d 个缺失值。", var_name, sum(.missing_mask(x))),
          "检查主键映射与关联逻辑。"
        )
      } else if (miss_rate >= 0.5) {
        issues <- .add_issue(
          issues, dataset_name, "WARNING", "高缺失率",
          sprintf("变量 %s 缺失率为 %.1f%%。", var_name, miss_rate * 100),
          "确认该变量是否应由当前规则稳定生成。"
        )
      }

      if (identical(spec_type, "num")) {
        numeric_ratio <- .parseable_numeric_ratio(x)
        if (numeric_ratio < 0.9) {
          issues <- .add_issue(
            issues, dataset_name, "WARNING", "类型一致性",
            sprintf("变量 %s 声明为 Num，但仅 %.1f%% 的非空值可解析为数值。", var_name, numeric_ratio * 100),
            "检查数值派生、格式化或字符拼接逻辑。"
          )
        }
      }

      if (identical(spec_type, "char") && !(is.character(x) || is.factor(x))) {
        issues <- .add_issue(
          issues, dataset_name, "WARNING", "类型一致性",
          sprintf("变量 %s 在 Spec 中声明为 Char，但输出列类型为 %s。", var_name, class(x)[1]),
          "确认是否应显式转换为字符。"
        )
      }
    }
  }

  if (!is.null(plan_ds) && is.list(plan_ds)) {
    plan_vars <- unique(.plan_expected_vars(plan_ds))
    if (length(plan_vars) > 0) {
      missing_plan_vars <- setdiff(plan_vars, names(df))
      extra_output_vars <- setdiff(names(df), plan_vars)

      if (length(missing_plan_vars) > 0) {
        issues <- .add_issue(
          issues, dataset_name, "ERROR", "Plan 覆盖",
          paste0("输出缺少 derivation plan 中声明的变量：", paste(missing_plan_vars, collapse = ", ")),
          "检查生成代码是否完整实现了当前 derivation plan。"
        )
      }

      if (length(extra_output_vars) > 0 && is.null(spec_df)) {
        issues <- .add_issue(
          issues, dataset_name, "INFO", "Plan 以外变量",
          paste0("输出包含未出现在 derivation plan 中的变量：", paste(head(extra_output_vars, 10), collapse = ", ")),
          "确认这些变量是否为人工补充或中间产物。"
        )
      }
    }

    for (v in plan_ds$variable_plan %||% list()) {
      var_name <- trimws(as.character(v$variable %||% ""))
      if (!nzchar(var_name) || !var_name %in% names(df)) next

      expected_type <- tolower(trimws(as.character(v$type %||% "")))
      x <- df[[var_name]]

      if (expected_type == "num") {
        numeric_ratio <- .parseable_numeric_ratio(x)
        if (numeric_ratio < 0.9) {
          issues <- .add_issue(
            issues, dataset_name, "WARNING", "Plan 类型一致性",
            sprintf("变量 %s 在 derivation plan 中声明为 num，但仅 %.1f%% 的非空值可解析为数值。", var_name, numeric_ratio * 100),
            "检查该变量的派生实现是否与 plan 一致。"
          )
        }
      }

      if (expected_type == "char" && !(is.character(x) || is.factor(x))) {
        issues <- .add_issue(
          issues, dataset_name, "WARNING", "Plan 类型一致性",
          sprintf("变量 %s 在 derivation plan 中声明为 char，但输出列类型为 %s。", var_name, class(x)[1]),
          "确认该变量是否应显式转换为字符。"
        )
      }
    }
  }

  # 主键唯一性：基于数据集角色和可用列推断
  key_cols <- .infer_key_columns(ds_key, names(df))
  if (length(key_cols) > 0 && all(key_cols %in% names(df))) {
    key_vals <- do.call(paste, c(df[key_cols], sep = "|"))
    n_dup <- sum(duplicated(key_vals))
    if (n_dup > 0) {
      issues <- .add_issue(
        issues, dataset_name, "ERROR", "主键唯一性",
        paste0("按 ", paste(key_cols, collapse = "+"), " 组合存在 ", n_dup, " 行重复。"),
        "检查是否存在错误的重复行或 join 导致的行爆炸。"
      )
    }
  }

  date_cols <- names(df)[grepl("(DT|DTC|STDTC|ENDTC|ASTDT|AENDT)$", toupper(names(df)))]
  for (col in date_cols) {
    valid_ratio <- .valid_date_ratio(df[[col]])
    if (valid_ratio < 0.9) {
      issues <- .add_issue(
        issues, dataset_name, "WARNING", "日期格式",
        sprintf("列 %s 仅 %.1f%% 的非空值符合 YYYY-MM-DD。", col, valid_ratio * 100),
        "检查日期转换逻辑和字符格式化规则。"
      )
    }
  }

  issues_df <- .collapse_issues(issues)
  n_error <- sum(issues_df$level == "ERROR")
  n_warn  <- sum(issues_df$level == "WARNING")
  status  <- if (n_error > 0) "ERROR" else if (n_warn > 0) "WARNING" else "PASS"

  list(
    issues = issues_df,
    stats = data.frame(
      dataset  = dataset_name,
      rows     = nrow(df),
      cols     = ncol(df),
      errors   = as.integer(n_error),
      warnings = as.integer(n_warn),
      status   = status,
      stringsAsFactors = FALSE
    )
  )
}

validate_adam_datasets <- function(datasets, specs = list(), derivation_plan = NULL) {
  spec_map <- .collect_spec_map(specs)
  plan_map <- .collect_plan_map(derivation_plan)
  expected_ds <- names(spec_map)
  expected_ds <- unique(c(expected_ds, names(plan_map)))
  actual_ds   <- names(datasets %||% list())

  issues <- list()
  stats  <- list()

  for (ds in setdiff(expected_ds, actual_ds)) {
    issues <- .add_issue(
      issues, toupper(ds), "ERROR", "缺少数据集",
      sprintf("Spec 中声明了 %s，但执行结果未生成该数据集。", toupper(ds)),
      "检查生成代码是否创建了与 Spec 对应的数据集对象。"
    )
    stats[[length(stats) + 1L]] <- data.frame(
      dataset  = toupper(ds),
      rows     = 0L,
      cols     = 0L,
      errors   = 1L,
      warnings = 0L,
      status   = "ERROR",
      stringsAsFactors = FALSE
    )
  }

  for (ds in actual_ds) {
    ds_upper <- toupper(ds)
    spec_df  <- spec_map[[tolower(ds)]] %||% NULL
    plan_ds  <- plan_map[[tolower(ds)]] %||% NULL

    if (is.null(spec_df)) {
      issues <- .add_issue(
        issues, ds_upper, "WARNING", "未匹配 Spec",
        sprintf("输出了 %s，但未找到对应 Spec，已仅执行基础结构检查。", ds_upper),
        "确认该数据集是否应加入 Spec，或是否为额外输出。"
      )
    }

    if (is.null(plan_ds)) {
      issues <- .add_issue(
        issues, ds_upper, "WARNING", "未匹配 Plan",
        sprintf("输出了 %s，但未找到对应 derivation plan，已跳过语义级校验。", ds_upper),
        "确认 LLM 是否生成了完整 derivation plan。"
      )
    }

    res <- .validate_dataset(ds_upper, datasets[[ds]], spec_df, plan_ds)
    if (nrow(res$issues) > 0) issues[[length(issues) + 1L]] <- res$issues
    stats[[length(stats) + 1L]] <- res$stats
  }

  issues_df <- if (length(issues) == 0) .collapse_issues(list()) else do.call(rbind, issues)
  stats_df  <- if (length(stats)  == 0) {
    data.frame(dataset = character(), rows = integer(), cols = integer(),
               errors = integer(), warnings = integer(), status = character(),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, stats)
  }

  total_errors   <- sum(stats_df$errors %||% 0L)
  total_warnings <- sum(stats_df$warnings %||% 0L)
  overall_status <- if (total_errors > 0) "ERROR"
                    else if (total_warnings > 0) "WARNING"
                    else "PASS"

  list(
    summary = list(
      status         = overall_status,
      datasets_total = nrow(stats_df),
      errors         = as.integer(total_errors),
      warnings       = as.integer(total_warnings)
    ),
    dataset_stats = stats_df,
    issues = issues_df
  )
}
