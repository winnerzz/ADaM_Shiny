"""Deterministic demo-code generation for mock LLM mode."""

from __future__ import annotations

import json


def default_mock_generated_code_response(target: str) -> str:
    """Return a strict LLM-code JSON payload for local mock/demo runs.

    This is intentionally not a production derivation engine. It gives the local
    UI a non-empty, auditable sandbox path when no external LLM is configured.
    """

    dataset = target.strip().upper()
    if dataset == "ADAE":
        r_code = _mock_adae_r_code()
        assumptions = [
            "Mock mode uses deterministic demo R code so the UI can exercise code review, sandbox execution, and output preview without an API key.",
            "ADAE is built from input_sdtm/ae.csv plus reference_adam/adsl.csv when available; otherwise it derives a minimal subject treatment context from DM/EX.",
        ]
        risk_points = [
            "This is demo-only code, not a production ADaM derivation.",
            "Clinical correctness still requires user spec review and a real LLM/provider or validated programmer-authored code.",
        ]
        used_inputs = [
            "input_sdtm/ae.csv",
            "input_sdtm/dm.csv",
            "input_sdtm/ex.csv",
            "reference_adam/adsl.csv",
        ]
    elif dataset == "ADSL":
        r_code = _mock_adsl_r_code()
        assumptions = [
            "Mock mode derives a minimal ADSL-like dataset from DM and EX for local workflow testing.",
        ]
        risk_points = [
            "This is demo-only code, not a production ADSL derivation.",
        ]
        used_inputs = ["input_sdtm/dm.csv", "input_sdtm/ex.csv"]
    else:
        r_code = _mock_generic_r_code(dataset)
        assumptions = [
            "Mock mode writes a small structural output for unsupported demo targets.",
        ]
        risk_points = [
            "This target has no deterministic demo derivation; use a real provider or approved R code for meaningful output.",
        ]
        used_inputs = []

    filename = dataset.lower()
    return json.dumps(
        {
            "dataset": dataset,
            "r_code": r_code,
            "assumptions": assumptions,
            "risk_points": risk_points,
            "used_inputs": used_inputs,
            "expected_outputs": [f"{filename}.csv"],
        }
    )


def _mock_adae_r_code() -> str:
    return r'''dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

resolve_input_path <- function(path) {
  if (file.exists(path)) {
    return(path)
  }
  sandbox_path <- file.path("..", "..", path)
  if (file.exists(sandbox_path)) {
    return(sandbox_path)
  }
  path
}

read_table <- function(path) {
  path <- resolve_input_path(path)
  if (!file.exists(path)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
}

strip_excel_apos <- function(x) {
  x <- ifelse(is.na(x), "", x)
  sub("^'", "", x)
}

parse_date <- function(x) {
  suppressWarnings(as.Date(strip_excel_apos(x)))
}

date_chr <- function(x) {
  value <- parse_date(x)
  ifelse(is.na(value), "", format(value, "%Y-%m-%d"))
}

dy_char <- function(date_value, ref_value) {
  date_value <- parse_date(date_value)
  ref_value <- parse_date(ref_value)
  result <- ifelse(is.na(date_value) | is.na(ref_value), NA, as.integer(date_value - ref_value) + 1L)
  ifelse(is.na(result), "", as.character(result))
}

get_col <- function(data, name) {
  if (name %in% names(data)) {
    return(data[[name]])
  }
  rep("", nrow(data))
}

first_by_subject <- function(data, cols) {
  if (!nrow(data) || !"USUBJID" %in% names(data)) {
    return(data.frame(USUBJID = character(), stringsAsFactors = FALSE))
  }
  keep <- unique(c("USUBJID", cols[cols %in% names(data)]))
  data <- data[!duplicated(data$USUBJID), keep, drop = FALSE]
  data
}

trt_num <- function(x) {
  value <- toupper(ifelse(is.na(x), "", x))
  ifelse(value == "PLACEBO", "0", ifelse(value == "TEST DRUG", "1", ""))
}

rel_group <- function(x) {
  value <- toupper(ifelse(is.na(x), "", x))
  ifelse(value %in% c("RELATED", "POSSIBLY RELATED"), "RELATED", "NOT RELATED")
}

rel_group_num <- function(x) {
  ifelse(x == "RELATED", "1", ifelse(x == "NOT RELATED", "0", ""))
}

sev_num <- function(x) {
  value <- toupper(ifelse(is.na(x), "", x))
  ifelse(value == "MILD", "1", ifelse(value == "MODERATE", "2", ifelse(value == "SEVERE", "3", "")))
}

tox_num <- function(x) {
  value <- suppressWarnings(as.integer(x))
  ifelse(is.na(value), "", as.character(value))
}

build_adsl_context <- function() {
  adsl <- read_table("reference_adam/adsl.csv")
  if (nrow(adsl)) {
    return(first_by_subject(adsl, c("USUBJID", "SUBJID", "TRT01A", "TRT01AN", "TRT01P", "TRT01PN", "TRTSDT")))
  }

  dm <- read_table("input_sdtm/dm.csv")
  ex <- read_table("input_sdtm/ex.csv")
  if (!nrow(dm)) {
    return(data.frame(USUBJID = character(), stringsAsFactors = FALSE))
  }

  ctx <- first_by_subject(dm, c("SUBJID", "ACTARM", "ARM"))
  names(ctx)[names(ctx) == "ACTARM"] <- "TRT01A"
  names(ctx)[names(ctx) == "ARM"] <- "TRT01P"
  ctx$TRT01AN <- trt_num(get_col(ctx, "TRT01A"))
  ctx$TRT01PN <- trt_num(get_col(ctx, "TRT01P"))

  if (nrow(ex) && "USUBJID" %in% names(ex) && "EXSTDTC" %in% names(ex)) {
    ex$EXSTDTC_CLEAN <- strip_excel_apos(ex$EXSTDTC)
    ex_dates <- aggregate(EXSTDTC_CLEAN ~ USUBJID, data = ex, FUN = min)
    names(ex_dates)[names(ex_dates) == "EXSTDTC_CLEAN"] <- "TRTSDT"
    ctx <- merge(ctx, ex_dates, by = "USUBJID", all.x = TRUE, sort = FALSE)
  }
  if (!"TRTSDT" %in% names(ctx)) {
    ctx$TRTSDT <- ""
  }
  ctx
}

ae <- read_table("input_sdtm/ae.csv")
if (!nrow(ae)) {
  stop("input_sdtm/ae.csv is required for mock ADAE generation and contains no rows.")
}

adsl_ctx <- build_adsl_context()
if (nrow(adsl_ctx)) {
  data <- merge(ae, adsl_ctx, by = "USUBJID", all.x = TRUE, sort = FALSE)
} else {
  data <- ae
}

astdt <- date_chr(get_col(data, "AESTDTC"))
aendt <- date_chr(get_col(data, "AEENDTC"))
trtsdt <- get_col(data, "TRTSDT")
relgr1 <- rel_group(get_col(data, "AEREL"))
trtemfl <- ifelse(parse_date(astdt) >= parse_date(trtsdt), "Y", "N")
trtemfl[is.na(trtemfl)] <- ""

output <- data.frame(
  STUDYID = get_col(data, "STUDYID"),
  USUBJID = get_col(data, "USUBJID"),
  SUBJID = get_col(data, "SUBJID"),
  AESEQ = get_col(data, "AESEQ"),
  TRT01A = get_col(data, "TRT01A"),
  TRT01AN = get_col(data, "TRT01AN"),
  TRT01P = get_col(data, "TRT01P"),
  TRT01PN = get_col(data, "TRT01PN"),
  AESPID = get_col(data, "AESPID"),
  AETERM = get_col(data, "AETERM"),
  AEDECOD = get_col(data, "AEDECOD"),
  ASTDT = astdt,
  AENDT = aendt,
  ASTDY = dy_char(astdt, trtsdt),
  AENDY = dy_char(aendt, trtsdt),
  AESEV = get_col(data, "AESEV"),
  ASEV = get_col(data, "AESEV"),
  ASEVN = sev_num(get_col(data, "AESEV")),
  AEREL = get_col(data, "AEREL"),
  RELGR1 = relgr1,
  RELGR1N = rel_group_num(relgr1),
  AETOXGR = get_col(data, "AETOXGR"),
  ATOXGR = get_col(data, "AETOXGR"),
  ATOXGRN = tox_num(get_col(data, "AETOXGR")),
  AESER = get_col(data, "AESER"),
  AESDTH = get_col(data, "AESDTH"),
  AEOUT = get_col(data, "AEOUT"),
  TRTEMFL = trtemfl,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

write.csv(output, file = "outputs/adae.csv", row.names = FALSE, na = "")
'''


def _mock_adsl_r_code() -> str:
    return r'''dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

resolve_input_path <- function(path) {
  if (file.exists(path)) {
    return(path)
  }
  sandbox_path <- file.path("..", "..", path)
  if (file.exists(sandbox_path)) {
    return(sandbox_path)
  }
  path
}

read_table <- function(path) {
  path <- resolve_input_path(path)
  if (!file.exists(path)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
}

strip_excel_apos <- function(x) {
  x <- ifelse(is.na(x), "", x)
  sub("^'", "", x)
}

get_col <- function(data, name) {
  if (name %in% names(data)) {
    return(data[[name]])
  }
  rep("", nrow(data))
}

trt_num <- function(x) {
  value <- toupper(ifelse(is.na(x), "", x))
  ifelse(value == "PLACEBO", "0", ifelse(value == "TEST DRUG", "1", ""))
}

dm <- read_table("input_sdtm/dm.csv")
if (!nrow(dm)) {
  stop("input_sdtm/dm.csv is required for mock ADSL generation and contains no rows.")
}

ex <- read_table("input_sdtm/ex.csv")
trtsdt <- rep("", nrow(dm))
trtedt <- rep("", nrow(dm))
if (nrow(ex) && "USUBJID" %in% names(ex)) {
  if ("EXSTDTC" %in% names(ex)) {
    ex$EXSTDTC_CLEAN <- strip_excel_apos(ex$EXSTDTC)
    start_dates <- aggregate(EXSTDTC_CLEAN ~ USUBJID, data = ex, FUN = min)
    trtsdt <- start_dates$EXSTDTC_CLEAN[match(dm$USUBJID, start_dates$USUBJID)]
    trtsdt[is.na(trtsdt)] <- ""
  }
  if ("EXENDTC" %in% names(ex)) {
    ex$EXENDTC_CLEAN <- strip_excel_apos(ex$EXENDTC)
    end_dates <- aggregate(EXENDTC_CLEAN ~ USUBJID, data = ex, FUN = max)
    trtedt <- end_dates$EXENDTC_CLEAN[match(dm$USUBJID, end_dates$USUBJID)]
    trtedt[is.na(trtedt)] <- ""
  }
}

output <- data.frame(
  STUDYID = get_col(dm, "STUDYID"),
  USUBJID = get_col(dm, "USUBJID"),
  SUBJID = get_col(dm, "SUBJID"),
  SITEID = get_col(dm, "SITEID"),
  COUNTRY = get_col(dm, "COUNTRY"),
  AGE = get_col(dm, "AGE"),
  AGEU = get_col(dm, "AGEU"),
  SEX = get_col(dm, "SEX"),
  RACE = get_col(dm, "RACE"),
  ETHNIC = get_col(dm, "ETHNIC"),
  TRT01P = get_col(dm, "ARM"),
  TRT01PN = trt_num(get_col(dm, "ARM")),
  TRT01A = get_col(dm, "ACTARM"),
  TRT01AN = trt_num(get_col(dm, "ACTARM")),
  TRTSDT = trtsdt,
  TRTEDT = trtedt,
  SAFFL = ifelse(trtsdt == "", "N", "Y"),
  ITTFL = ifelse(get_col(dm, "USUBJID") == "", "", "Y"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

write.csv(output, file = "outputs/adsl.csv", row.names = FALSE, na = "")
'''


def _mock_generic_r_code(dataset: str) -> str:
    filename = dataset.lower()
    return f'''dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
output <- data.frame(NOTE = "Mock mode has no deterministic demo derivation for {dataset}.", stringsAsFactors = FALSE)
write.csv(output, file = "outputs/{filename}.csv", row.names = FALSE)
'''
