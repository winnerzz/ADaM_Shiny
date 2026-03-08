# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A Shiny web application that automates CDISC ADaM dataset generation (ADSL, ADAE) from SDTM source data. Users upload SDTM CSVs and ADaM specification CSVs, then the app calls an LLM API to generate R code that transforms SDTM into ADaM datasets. The generated code is executed in-app, and results can be previewed and downloaded.

## Running the App

```bash
# From the project root
Rscript app.R
# Or in R console:
shiny::runApp(".")
```

The app runs on `http://127.0.0.1:3838` and auto-opens in the browser.

## Dependencies

R packages (auto-installed on first run via `app.R`): shiny, bslib, bsicons, DT, shinyAce, dplyr, readr, lubridate, stringr, tidyr, jsonlite, httr2, shinyjs. PyTorch is NOT involved — this is a pure R/Shiny project.

## Architecture

The app follows a flat 4-file structure (no R package, no modules):

- **`app.R`** — Entry point. Installs missing packages, sources all modules, launches `shinyApp()`.
- **`ui.R`** — UI definition using `bslib::page_sidebar`. Sidebar has accordion panels for data upload and AI engine config. Main panel has tabs: generated R code, execution log, ADSL preview, ADAE preview, downloads.
- **`server.R`** — Server logic. Manages reactive state (`rv_sdtm`, `rv_specs`, `rv_code`, `rv_log`, `rv_adsl`, `rv_adae`). Core pipeline (`btn_run_pipeline`): sorts specs ADSL-first, calls LLM per dataset, executes returned R code in a shared isolated `new.env()` so ADSL is available when building ADAE.
- **`llm_api.R`** — LLM API routing. Supports 4 providers (Kimi/DeepSeek/OpenAI/Qwen), all using OpenAI-compatible chat completion format via `httr2`. Contains the system prompt with CDISC ADaM business rules and code conventions.
- **`data_utils.R`** — Utility functions: `load_sdtm_data()`, `load_spec_json()`, `summarize_sdtm()`, `strip_excel_apos()`, `dy_char()` (CDISC Study Day calculation), `generate_spec_template()`.

## Key Design Decisions

- **All CSV columns read as character** (`col_types = cols(.default = col_character())`) to avoid type inference issues with dates and coded values.
- **Shared execution environment**: LLM-generated code for each dataset runs in the same `new.env(parent = globalenv())`, so ADAE code can reference the `adsl` data frame created by ADSL code.
- **SDTM data injected into exec env**: uploaded SDTM data frames (e.g., `dm`, `ex`, `ae`) are assigned into the execution environment before running LLM code.
- **Dataset ordering**: Specs are sorted with ADSL first (priority 1), ADAE second (priority 2), ensuring dependency order.
- **Excel apostrophe cleanup**: `strip_excel_apos()` removes leading `'` from date columns (`*DTC`) that Excel adds as a formatting artifact.

## Sample Data

Included CSV files for testing:
- SDTM: `dm.csv`, `ex.csv`, `ae.csv`
- Specs: `ads_adsl_full.csv`, `ads_adae_full.csv`

## LLM Provider Configuration

Providers are defined in `LLM_PROVIDERS` list in `llm_api.R`. To add a new provider, add an entry with `name`, `base_url`, and `model` fields — it must support OpenAI-compatible chat completion API format. The API key is entered per-session in the UI (not stored).
