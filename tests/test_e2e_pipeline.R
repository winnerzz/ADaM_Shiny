# =============================================================================
# test_e2e_pipeline.R
# 端到端管线测试：真实 LLM 调用 + 完整执行 + 校验
#
# 用法：
#   Rscript tests/test_e2e_pipeline.R                          # 用 Mock
#   Rscript tests/test_e2e_pipeline.R deepseek sk-xxx          # 用真实 API
#   Rscript tests/test_e2e_pipeline.R openai sk-xxx gpt-4o     # 指定模型
#
# 输出：tests/last_run.json（完整中间产物）
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

args <- commandArgs(trailingOnly = TRUE)
provider  <- if (length(args) >= 1) args[1] else "mock"
api_key   <- if (length(args) >= 2) args[2] else ""
model_arg <- if (length(args) >= 3) args[3] else NULL

ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (!is.null(ofile)) {
  setwd(file.path(dirname(ofile), ".."))
} else {
  # When run via Rscript, ofile may not be set; use script path from args
  script_dir <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1] %||% ""))
  if (nzchar(script_dir) && script_dir != ".") setwd(file.path(script_dir, ".."))
}
cat("Working dir:", getwd(), "\n")

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(jsonlite); library(lubridate); library(httr2)
})

source("data_utils.R")
source("derivation_plan_utils.R")
source("validation_utils.R")
source("provider_registry.R")
source("llm_api.R")
source("code_static_checks.R")

# ── 辅助函数 ──────────────────────────────────────────────────────────────────
extract_missing_variables <- function(validation_issues) {
  if (is.null(validation_issues) || nrow(validation_issues) == 0) return(character(0))
  hit <- validation_issues[validation_issues$check %in% c("\u7f3a\u5931\u53d8\u91cf", "Plan \u8986\u76d6"), , drop = FALSE]
  if (nrow(hit) == 0) return(character(0))
  vars <- character(0)
  for (i in seq_len(nrow(hit))) {
    txt <- sub("^.*\uff1a", "", hit$detail[i])
    vars <- c(vars, trimws(unlist(strsplit(txt, ",\\s*"))))
  }
  unique(vars[nzchar(vars)])
}

# ── 结果收集器 ────────────────────────────────────────────────────────────────
R <- list(
  timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  provider   = provider,
  model      = NULL,
  stages     = list(),
  success    = FALSE,
  error      = NULL
)

stage <- function(name) {
  cat("\n", strrep("=", 60), "\n")
  cat(" STAGE:", name, "\n")
  cat(strrep("=", 60), "\n")
}

record <- function(name, result) {
  R$stages[[name]] <<- result
  if (!is.null(result$error)) cat("  [ERROR]", result$error, "\n")
  else cat("  [OK]\n")
}

# =============================================================================
# Stage 1: Load SDTM
# =============================================================================
stage("1. Load SDTM")
sdtm <- tryCatch({
  load_sdtm_data(c(dm = "demo-data/dm.csv", ex = "demo-data/ex.csv", ae = "demo-data/ae.csv"))
}, error = function(e) { cat("FATAL:", conditionMessage(e), "\n"); quit("no", 1) })

record("load_sdtm", list(
  domains = names(sdtm),
  dims = lapply(sdtm, function(df) list(rows = nrow(df), cols = ncol(df))),
  error = NULL
))

# =============================================================================
# Stage 2: Parse Specs
# =============================================================================
stage("2. Parse Specs")
spec_files <- c(adsl = "demo-data/ads_adsl_full.csv", adae = "demo-data/ads_adae_full.csv")
specs <- list()
for (nm in names(spec_files)) {
  df <- read.csv(spec_files[[nm]], stringsAsFactors = FALSE, na.strings = c("", "NA"))
  specs[[nm]] <- list(
    file_id = nm,
    filename = basename(spec_files[[nm]]),
    parsed = list(
      dataset = toupper(nm),
      variables = data.frame(
        variable   = df$Variable,
        label      = df$Label,
        type       = df$Type,
        source     = df$Source,
        derivation = df$Derivation,
        stringsAsFactors = FALSE
      )
    )
  )
}

# split specs by dataset
.split_specs_by_dataset <- function(specs) {
  parsed_specs <- lapply(specs, function(s) s$parsed)
  parsed_specs <- Filter(Negate(is.null), parsed_specs)
  if (length(parsed_specs) == 0) return(list())
  out <- list()
  for (spec in parsed_specs) {
    ds_name <- tolower(trimws(spec$dataset %||% ""))
    if (!nzchar(ds_name)) next
    spec_vars <- spec$variables %||% data.frame(stringsAsFactors = FALSE)
    if (is.null(out[[ds_name]])) out[[ds_name]] <- list(dataset = ds_name, variables = spec_vars)
    else out[[ds_name]]$variables <- dplyr::bind_rows(out[[ds_name]]$variables, spec_vars)
  }
  lapply(out, function(spec) {
    vars <- spec$variables
    if (is.data.frame(vars) && "variable" %in% names(vars) && nrow(vars) > 0)
      vars <- vars[!duplicated(toupper(trimws(as.character(vars$variable)))), , drop = FALSE]
    spec$variables <- vars; spec
  })
}

spec_map <- .split_specs_by_dataset(specs)
target_datasets <- names(spec_map)
record("parse_specs", list(
  datasets = target_datasets,
  var_counts = sapply(spec_map, function(s) nrow(s$variables)),
  error = NULL
))

# =============================================================================
# Stage 3: SDTM Profile + Derivation Plan
# =============================================================================
stage("3. SDTM Profile & Derivation Plan")
profiles <- profile_sdtm_domains(sdtm)
profile_txt <- format_sdtm_profiles(profiles)

plan <- normalize_derivation_plan(NULL, specs, target_datasets)
record("plan", list(
  n_datasets = length(plan$datasets),
  roles = sapply(plan$datasets, function(d) paste0(d$dataset, "=", d$dataset_role)),
  error = NULL
))

# =============================================================================
# Stage 4: Build Prompts
# =============================================================================
stage("4. Build Prompts")

# Compact spec payload (mirrors server.R logic)
.compact_spec_payload <- function(spec) {
  vars <- spec$variables %||% data.frame(stringsAsFactors = FALSE)
  keep_cols <- intersect(c("variable", "label", "type", "source", "derivation", "dataset"), names(vars))
  if (length(keep_cols) > 0) vars <- vars[, keep_cols, drop = FALSE]
  trunc_limits <- c(derivation = 500L, source = 250L, label = 80L)
  for (col in intersect(names(trunc_limits), names(vars)))
    vars[[col]] <- substr(as.character(vars[[col]] %||% ""), 1L, trunc_limits[[col]])
  list(dataset = spec$dataset %||% "unknown", variables = vars)
}

spec_jsons <- lapply(spec_map, function(s) .compact_spec_payload(s))
full_spec_json <- toJSON(spec_jsons, pretty = TRUE, auto_unbox = TRUE)

prompts <- .build_prompts(
  spec_json       = full_spec_json,
  data_summary    = profile_txt,
  sdtm_list       = sdtm,
  target_datasets = target_datasets,
  prompt_profile  = list(mode = "balanced", preview_rows = 5L, pipe_format = TRUE)
)

record("prompts", list(
  system_len = nchar(prompts$system),
  user_len   = nchar(prompts$user),
  error      = NULL
))

# Save prompts for inspection
writeLines(prompts$system, "tests/last_system_prompt.txt")
writeLines(prompts$user,   "tests/last_user_prompt.txt")
cat("  Prompts saved to tests/last_system_prompt.txt and tests/last_user_prompt.txt\n")

# =============================================================================
# Stage 5: LLM Call
# =============================================================================
stage("5. LLM Call")

use_mock <- identical(provider, "mock")

if (use_mock) {
  cat("  Using MOCK mode\n")
  llm_res <- .mock_llm_response(target_datasets = target_datasets)
  R$model <- "mock"
} else {
  model <- model_arg %||% .default_model(provider)
  R$model <- model
  cat("  Provider:", provider, " Model:", model, "\n")

  llm_res <- tryCatch({
    call_llm_engine_with_failover(
      spec_json        = full_spec_json,
      data_summary     = profile_txt,
      provider_key_map = setNames(list(api_key), provider),
      failover_chain   = list(list(provider = provider, model = model)),
      sdtm_list        = sdtm,
      base_url_map     = list(),
      mock             = FALSE,
      target_datasets  = target_datasets,
      prompt_profile   = list(mode = "balanced", preview_rows = 5L, pipe_format = TRUE)
    )
  }, error = function(e) {
    cat("  LLM CALL FAILED:", conditionMessage(e), "\n")
    list(r_code = "", risk_logs = list(), token_info = list(input = 0, output = 0, total = 0),
         error = conditionMessage(e))
  })
}

has_error <- !is.null(llm_res$error)
record("llm_call", list(
  r_code_len = nchar(llm_res$r_code %||% ""),
  risk_logs  = length(llm_res$risk_logs %||% list()),
  tokens     = llm_res$token_info,
  error      = llm_res$error
))

if (has_error || !nzchar(llm_res$r_code %||% "")) {
  cat("  Aborting — no code to execute\n")
  R$error <- llm_res$error %||% "Empty r_code"
  write(toJSON(R, pretty = TRUE, auto_unbox = TRUE), "tests/last_run.json")
  quit("no", 1)
}

# Save generated code
writeLines(llm_res$r_code, "tests/last_generated_code.R")
cat("  Code saved to tests/last_generated_code.R\n")
cat("  Code preview (first 30 lines):\n")
cat(paste0("    ", head(strsplit(llm_res$r_code, "\n")[[1]], 30)), sep = "\n")
cat("\n")

# =============================================================================
# Stage 6: Static Check
# =============================================================================
stage("6. Static Check")
static <- run_code_static_checks(
  code_str          = llm_res$r_code,
  expected_datasets = target_datasets,
  allowed_packages  = c("dplyr", "lubridate", "stringr", "tidyr", "readr",
                        "haven", "purrr", "forcats", "janitor", "glue", "stats"),
  available_inputs  = names(sdtm),
  available_columns = lapply(sdtm, names)
)

record("static_check", list(
  status   = static$summary$status,
  errors   = static$summary$errors,
  warnings = static$summary$warnings,
  issues   = if (nrow(static$issues) > 0) as.list(as.data.frame(t(static$issues), stringsAsFactors = FALSE)) else list(),
  error    = if (static$summary$status == "ERROR") "Static check failed" else NULL
))

if (nrow(static$issues) > 0) {
  cat("  Issues:\n")
  for (i in seq_len(nrow(static$issues)))
    cat("   ", static$issues$level[i], "|", static$issues$check[i], "|", static$issues$detail[i], "\n")
}

# =============================================================================
# Stage 7: Sandbox Execution
# =============================================================================
stage("7. Sandbox Execution")

# Auto-sanitize high-confidence issues (e.g., && → &)
llm_res$r_code <- sanitize_llm_code(llm_res$r_code)

needed_pkgs <- c("dplyr", "lubridate", "stringr", "tidyr", "readr",
                 "haven", "purrr", "forcats", "janitor", "glue", "stats")
exec_parent <- new.env(parent = baseenv(), hash = TRUE)
for (pkg in needed_pkgs) {
  if (requireNamespace(pkg, quietly = TRUE))
    for (fn in getNamespaceExports(pkg))
      tryCatch(assign(fn, getExportedValue(pkg, fn), envir = exec_parent), error = function(e) NULL)
}
for (fn in c("head", "tail"))
  tryCatch(assign(fn, getExportedValue("utils", fn), envir = exec_parent), error = function(e) NULL)

# Inject helpers
exec_parent$strip_excel_apos      <- strip_excel_apos
exec_parent$dy_char               <- dy_char
exec_parent$parse_sdtm_date       <- parse_sdtm_date
exec_parent$study_day_chr         <- study_day_chr
exec_parent$map_trt_num           <- map_trt_num
exec_parent$yn_flag               <- function(test, missing = "N") {
  if (length(test) == 1L && !is.na(test))
    warning("yn_flag() received length-1 input - possible && misuse")
  yn_flag(test, missing)
}
exec_parent$first_non_missing_chr <- first_non_missing_chr
exec_parent$derive_trtemfl        <- derive_trtemfl
exec_parent$derive_relgr1         <- derive_relgr1
exec_parent$library               <- function(...) invisible(NULL)
exec_parent$require               <- function(...) invisible(TRUE)
assign("install.packages", function(...) invisible(NULL), envir = exec_parent)

exec_env <- new.env(parent = exec_parent, hash = TRUE)
for (sid in names(sdtm)) assign(sid, sdtm[[sid]], envir = exec_env)

exec_warnings <- character(0)
exec_result <- tryCatch({
  withCallingHandlers(
    eval(parse(text = llm_res$r_code), envir = exec_env),
    warning = function(w) {
      exec_warnings <<- c(exec_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  "ok"
}, error = function(e) conditionMessage(e))

# Extract datasets
extracted <- list()
for (ds in target_datasets) {
  if (exists(ds, envir = exec_env, inherits = FALSE)) {
    extracted[[ds]] <- get(ds, envir = exec_env)
  } else {
    # case-insensitive fallback
    candidates <- ls(exec_env)[tolower(ls(exec_env)) == ds]
    if (length(candidates) == 1) extracted[[ds]] <- get(candidates[1], envir = exec_env)
  }
}

# ── C7 defensive column pruning ──────────────────────────────────────────
for (ds in names(extracted)) {
  ds_spec <- spec_map[[ds]]
  if (!is.null(ds_spec) && is.data.frame(ds_spec$variables) && "variable" %in% names(ds_spec$variables)) {
    spec_vars <- toupper(trimws(as.character(ds_spec$variables$variable)))
    actual_vars <- names(extracted[[ds]])
    keep <- actual_vars[toupper(actual_vars) %in% spec_vars]
    if (length(keep) > 0 && length(keep) < length(actual_vars)) {
      dropped <- setdiff(actual_vars, keep)
      cat("  [C7]", toupper(ds), "- pruned", length(dropped), "extra cols:",
          paste(head(dropped, 8), collapse=", "), "\n")
      extracted[[ds]] <- extracted[[ds]][, keep, drop = FALSE]
    }
  }
}

record("execution", list(
  result        = exec_result,
  warnings      = exec_warnings,
  datasets_found = names(extracted),
  dims          = lapply(extracted, function(df) list(rows = nrow(df), cols = ncol(df))),
  error         = if (exec_result != "ok") exec_result else NULL
))

if (exec_result != "ok") {
  cat("  EXECUTION FAILED:", exec_result, "\n")
  cat("  Full generated code:\n")
  cat(llm_res$r_code, "\n")
}
if (length(exec_warnings) > 0) {
  cat("  Warnings during execution:\n")
  for (w in head(exec_warnings, 10)) cat("    -", w, "\n")
}

# =============================================================================
# Stage 8: Validation
# =============================================================================
if (length(extracted) > 0) {
  stage("8. Validation")
  val <- tryCatch(
    validate_adam_datasets(extracted, specs, plan),
    error = function(e) {
      cat("  VALIDATION ERROR:", conditionMessage(e), "\n")
      list(summary = list(status = "ERROR", errors = 1, warnings = 0),
           issues = data.frame(dataset = "SYSTEM", level = "ERROR", check = "校验异常",
                              detail = conditionMessage(e), action = "检查校验函数",
                              stringsAsFactors = FALSE),
           error = conditionMessage(e))
    }
  )

  record("validation", list(
    status   = val$summary$status,
    errors   = val$summary$errors,
    warnings = val$summary$warnings,
    issues   = if (!is.null(val$issues) && nrow(val$issues) > 0) {
      lapply(seq_len(nrow(val$issues)), function(i) as.list(val$issues[i, ]))
    } else list(),
    error    = val$error
  ))

  if (!is.null(val$issues) && nrow(val$issues) > 0) {
    cat("  Validation issues:\n")
    for (i in seq_len(nrow(val$issues)))
      cat("   ", val$issues$level[i], "|", val$issues$dataset[i], "|",
          val$issues$check[i], "|", val$issues$detail[i], "\n")
  }

  # Show sample output
  for (ds in names(extracted)) {
    cat("\n  ", toupper(ds), "first 3 rows:\n")
    print(head(extracted[[ds]], 3))
  }
}

# =============================================================================
# Stage 9: Repair (mock_mode = repair_demo)
# =============================================================================
if (use_mock && length(extracted) > 0) {
  stage("9. Repair (repair_demo)")

  # Identify missing variables from validation
  repair_missing <- extract_missing_variables(val$issues)
  repair_triggered <- length(repair_missing) > 0
  cat("  Missing variables:", length(repair_missing), "\n")
  if (repair_triggered) cat("   ", paste(head(repair_missing, 10), collapse = ", "), "\n")

  if (repair_triggered) {
    # Build repair spec rows
    repair_spec_rows <- do.call(rbind, lapply(specs, function(s) {
      if (is.null(s$parsed) || is.null(s$parsed$variables)) return(NULL)
      s$parsed$variables[s$parsed$variables$variable %in% repair_missing, , drop = FALSE]
    }))
    if (is.null(repair_spec_rows)) repair_spec_rows <- data.frame(stringsAsFactors = FALSE)

    # Build minimal repair payload (server.R's .build_repair_request_payload
    # depends on Shiny internals, so we construct a simplified version here)
    repair_payload <- toJSON(list(
      request_type      = "repair_missing_variables",
      target_datasets   = target_datasets,
      missing_variables = repair_missing,
      missing_spec_rows = repair_spec_rows,
      current_code      = llm_res$r_code,
      instructions      = "补全缺失变量，返回完整代码。"
    ), pretty = TRUE, auto_unbox = TRUE)

    # Call repair LLM (mock_mode=repair_demo returns fixed code)
    repair_res <- call_llm_engine_with_failover(
      spec_json        = repair_payload,
      data_summary     = profile_txt,
      provider_key_map = list(openai = ""),
      failover_chain   = list(list(provider = "openai", model = "gpt-4o")),
      sdtm_list        = NULL,
      base_url_map     = list(),
      mock             = TRUE,
      target_datasets  = target_datasets,
      prompt_profile   = list(mode = "strict", task = "repair_code",
                              mock_mode = "repair_demo", max_tokens = 4000L)
    )

    repair_code_ok <- nzchar(repair_res$r_code %||% "")
    cat("  Repair code received:", repair_code_ok, "\n")

    if (repair_code_ok) {
      # Sanitize + static check
      repair_res$r_code <- sanitize_llm_code(repair_res$r_code)
      repair_static <- run_code_static_checks(
        code_str          = repair_res$r_code,
        expected_datasets = target_datasets,
        allowed_packages  = c("dplyr", "lubridate", "stringr", "tidyr", "readr",
                              "haven", "purrr", "forcats", "janitor", "glue", "stats"),
        available_inputs  = names(sdtm),
        available_columns = lapply(sdtm, names)
      )
      cat("  Repair static:", repair_static$summary$status, "\n")

      # Execute repair code
      exec_env2 <- new.env(parent = exec_parent, hash = TRUE)
      for (sid in names(sdtm)) assign(sid, sdtm[[sid]], envir = exec_env2)
      repair_exec <- tryCatch({
        withCallingHandlers(
          eval(parse(text = repair_res$r_code), envir = exec_env2),
          message = function(m) invokeRestart("muffleMessage"),
          warning = function(w) invokeRestart("muffleWarning")
        )
        "ok"
      }, error = function(e) conditionMessage(e))
      cat("  Repair exec:", repair_exec, "\n")

      # Extract + validate
      repair_extracted <- list()
      if (identical(repair_exec, "ok")) {
        for (ds in target_datasets) {
          if (exists(ds, envir = exec_env2, inherits = FALSE))
            repair_extracted[[ds]] <- get(ds, envir = exec_env2)
        }
        # C7 prune
        for (ds in names(repair_extracted)) {
          ds_spec <- spec_map[[ds]]
          if (!is.null(ds_spec) && is.data.frame(ds_spec$variables)) {
            spec_vars <- toupper(trimws(as.character(ds_spec$variables$variable)))
            keep <- names(repair_extracted[[ds]])[toupper(names(repair_extracted[[ds]])) %in% spec_vars]
            if (length(keep) > 0) repair_extracted[[ds]] <- repair_extracted[[ds]][, keep, drop = FALSE]
          }
        }
        repair_val <- validate_adam_datasets(repair_extracted, specs, plan)
        repair_missing2 <- extract_missing_variables(repair_val$issues)
        cat("  Repair validation:", repair_val$summary$status,
            "(E=", repair_val$summary$errors, "W=", repair_val$summary$warnings, ")\n")
        cat("  Remaining missing:", length(repair_missing2), "\n")

        record("repair", list(
          triggered       = TRUE,
          initial_missing = repair_missing,
          repair_static   = repair_static$summary$status,
          repair_exec     = repair_exec,
          repair_val      = repair_val$summary$status,
          remaining_missing = repair_missing2,
          resolved        = length(repair_missing2) < length(repair_missing),
          error           = NULL
        ))
      } else {
        record("repair", list(
          triggered = TRUE, initial_missing = repair_missing,
          repair_exec = repair_exec, error = repair_exec
        ))
      }
    } else {
      record("repair", list(triggered = TRUE, error = "Empty repair code"))
    }
  } else {
    record("repair", list(triggered = FALSE, error = NULL))
    cat("  No missing variables — repair not needed\n")
  }
} else if (!use_mock) {
  cat("\n  [Repair stage skipped — only runs in mock mode]\n")
}

# =============================================================================
# Final Summary
# =============================================================================
stage("SUMMARY")
R$success <- exec_result == "ok" && length(extracted) > 0

cat("  Provider:  ", R$provider, "\n")
cat("  Model:     ", R$model, "\n")
cat("  Success:   ", R$success, "\n")
cat("  Datasets:  ", paste(names(extracted), collapse = ", "), "\n")
if (!is.null(R$stages$llm_call$tokens))
  cat("  Tokens:    ", R$stages$llm_call$tokens$total %||% 0, "\n")
cat("  Static:    ", R$stages$static_check$status %||% "N/A", "\n")
cat("  Execution: ", exec_result, "\n")
if (!is.null(R$stages$validation))
  cat("  Validation:", R$stages$validation$status,
      "(E=", R$stages$validation$errors, "W=", R$stages$validation$warnings, ")\n")

write(toJSON(R, pretty = TRUE, auto_unbox = TRUE, null = "null"), "tests/last_run.json")
cat("\n  Full results saved to tests/last_run.json\n")
