source("code_static_checks.R")

bad_code <- '
trt_levels <- sort(unique(na.omit(c(dm$ACTARM, dm$ARM))))
ex_summary <- ex |>
  group_by(USUBJID) |>
  summarise(NEX = n(), .groups="drop")
adsl <- dm |>
  left_join(ex_summary, by="USUBJID") |>
  mutate(
    TRT01P = ARM,
    TRT01PN = map_trt_num(TRT01P, trt_levels, start_at = 0L),
    TRT01A = ACTARM,
    TRT01AN = map_trt_num(TRT01A, trt_levels, start_at = 0L),
    RANDDT = RFSTDTC,
    TRTSDT = RFXSTDTC,
    TRTEDT = RFXENDTC,
    TRTEDY = study_day_chr(TRTEDT, TRTSDT),
    SAFFL = yn_flag(!is.na(NEX) && NEX > 0),
    ITTFL = "Y"
  ) |>
  select(STUDYID, USUBJID, SUBJID, SITEID, COUNTRY, AGE, AGEU, SEX, RACE, ETHNIC, TRT01P, TRT01PN, TRT01A, TRT01AN, RANDDT, TRTSDT, TRTEDT, TRTEDY, EOTDT, EOTDY, EOTSTT, EOSDT, EOSSTT, DTHFL, DTHDT, SAFFL, ITTFL)
adae <- ae |>
  left_join(adsl |> select(USUBJID, SUBJID, TRTSDT, TRTEDT, TRT01A, TRT01AN, TRT01P, TRT01PN), by="USUBJID") |>
  mutate(
    ASTDT = parse_sdtm_date(AESTDTC),
    TRTEMFL = yn_flag(ASTDT >= TRTSDT)
  ) |>
  select(STUDYID, USUBJID, AESEQ, TRTEMFL)
'

# Debug Check 10
ex_pos <- regexpr("\\bex_summary\\s*<-", bad_code, perl = TRUE)
cat("ex_pos:", as.integer(ex_pos), "\n")
if (as.integer(ex_pos) > 0L) {
  window <- substr(bad_code, as.integer(ex_pos), min(as.integer(ex_pos) + 1000L, nchar(bad_code)))
  cat("window (first 200 chars):\n", window, "\n\n")
  cat("has TRTSDT:", grepl("TRTSDT", window, fixed = TRUE), "\n")
}

cat("\n--- Running full check ---\n")
res <- run_code_static_checks(bad_code, expected_datasets = c("adsl", "adae"), available_inputs = c("dm","ex","ae"))
cat("Status:", res$summary$status, "\n")
cat("Errors:", res$summary$errors, "Warnings:", res$summary$warnings, "\n\n")
if (nrow(res$issues) > 0) {
  for (i in seq_len(nrow(res$issues))) {
    cat(res$issues$level[i], "|", res$issues$check[i], "|", res$issues$detail[i], "\n")
  }
}
