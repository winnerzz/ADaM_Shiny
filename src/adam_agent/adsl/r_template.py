"""Deterministic R code rendering for the Phase 5 ADSL starter loop."""

from __future__ import annotations

from pathlib import Path


def render_build_adsl_r(*, dm_path: str | Path, ex_path: str | Path, output_path: str | Path) -> str:
    """Render a small base-R ADSL builder.

    The generated script intentionally labels TRTSDT/TRTEDT/SAFFL as MVP
    starter derivations. It is not a production ADaM program.
    """

    dm = _r_string(Path(dm_path).as_posix())
    ex = _r_string(Path(ex_path).as_posix())
    output = _r_string(Path(output_path).as_posix())
    return f'''# Auto-generated Phase 5 ADSL starter script.
# This script is for MVP/demo execution only. It is not a production ADaM program.

read_input <- function(path) {{
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {{
    return(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"))
  }}
  if (ext == "sas7bdat") {{
    if (!requireNamespace("haven", quietly = TRUE)) {{
      stop("Reading sas7bdat requires the R package 'haven'.")
    }}
    return(as.data.frame(haven::read_sas(path), stringsAsFactors = FALSE))
  }}
  stop(paste("Unsupported input format:", ext))
}}

date10 <- function(x) {{
  parsed <- suppressWarnings(as.Date(substr(as.character(x), 1, 10)))
  parsed
}}

dm <- read_input({dm})
ex <- read_input({ex})

if (!"USUBJID" %in% names(dm)) {{
  stop("DM.USUBJID is required for the Phase 5 ADSL starter loop.")
}}
if (!"USUBJID" %in% names(ex)) {{
  stop("EX.USUBJID is required for the Phase 5 ADSL starter loop.")
}}

direct_cols <- intersect(c("STUDYID", "USUBJID", "AGE", "SEX", "RACE", "ARM", "ACTARM"), names(dm))
adsl <- dm[!duplicated(dm$USUBJID), direct_cols, drop = FALSE]

if ("EXSTDTC" %in% names(ex)) {{
  ex$.__TRTSDT <- date10(ex$EXSTDTC)
  trtsdt <- aggregate(.__TRTSDT ~ USUBJID, data = ex, FUN = function(x) min(x, na.rm = TRUE))
  names(trtsdt)[names(trtsdt) == ".__TRTSDT"] <- "TRTSDT"
  trtsdt$TRTSDT[is.infinite(trtsdt$TRTSDT)] <- NA
  adsl <- merge(adsl, trtsdt, by = "USUBJID", all.x = TRUE, sort = FALSE)
}}

if ("EXENDTC" %in% names(ex)) {{
  ex$.__TRTEDT <- date10(ex$EXENDTC)
}} else if ("EXSTDTC" %in% names(ex)) {{
  ex$.__TRTEDT <- date10(ex$EXSTDTC)
}}
if (".__TRTEDT" %in% names(ex)) {{
  trtedt <- aggregate(.__TRTEDT ~ USUBJID, data = ex, FUN = function(x) max(x, na.rm = TRUE))
  names(trtedt)[names(trtedt) == ".__TRTEDT"] <- "TRTEDT"
  trtedt$TRTEDT[is.infinite(trtedt$TRTEDT)] <- NA
  adsl <- merge(adsl, trtedt, by = "USUBJID", all.x = TRUE, sort = FALSE)
}}

# Demo/MVP candidate only. Production SAFFL must come from study evidence.
has_ex <- unique(ex$USUBJID[!is.na(ex$USUBJID) & ex$USUBJID != ""])
adsl$SAFFL <- ifelse(adsl$USUBJID %in% has_ex, "Y", "N")

dir.create(dirname({output}), recursive = TRUE, showWarnings = FALSE)
write.csv(adsl, {output}, row.names = FALSE, na = "")
'''


def _r_string(value: str) -> str:
    return '"' + value.replace("\\", "/").replace('"', '\\"') + '"'
