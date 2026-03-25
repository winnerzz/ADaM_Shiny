# =============================================================================
# code_static_checks.R
# 功能：在执行前对 LLM 生成的 R 代码做轻量静态检查
# =============================================================================

run_code_static_checks <- function(code_str,
                                   expected_datasets = character(),
                                   allowed_packages = character(),
                                   available_inputs = character()) {
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
    list("安装依赖", "(?i)\\binstall\\.packages\\s*\\(", "依赖应由应用环境预装，不在生成代码中安装。")
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
        "WARNING", "额外依赖",
        paste0("代码引用了未列入允许清单的包：", paste(disallowed, collapse = ", ")),
        "确认运行环境中是否已安装这些包，或将逻辑改写为使用允许包。"
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
