# Phase 8.2 Local Web UI

Phase 8.2 adds a local browser UI on top of the Phase 8.1 API.

This is still a local prototype. It is meant to make the Phase 7/8 backend easy
to inspect, not to solve deployment, access control, or production review.

## Start Command

```powershell
python -m uvicorn adam_agent.api.app:app --reload --host 127.0.0.1 --port 8000
```

Open:

```text
http://127.0.0.1:8000
```

## What The Page Does

The page now follows the human-review-first workflow defined in
`docs/phase8_ui_redesign.md`:

1. Create or open a study workspace.
2. Upload files by role: SDTM, SPEC, Reference ADaM, Define, Legacy.
3. Scan and review recognized inputs.
4. Choose target ADaM datasets and prepare a dependency plan.
5. Generate R code without executing it.
6. Review generated R code, assumptions, risk points, and expected outputs.
7. Approve code.
8. Execute approved code in the local R sandbox.
9. Review generated ADaM output, validation, reference evidence, and audit
   artifacts.

The main screen shows human-facing information first:

- SDTM inputs
- spec files
- reference ADaM files
- selected target dataset
- dependency planning result
- generated R code before execution
- code review decision
- generated ADaM output preview
- assumptions and risk points parsed from the LLM response
- validation warnings and errors

Low-level JSON artifacts are still available, but they are placed under the
Advanced Audit view instead of being the main product experience.

Advanced settings allow direct entry of config path, Rscript path, and approved
dependency-generation datasets.

## UI-Friendly API Endpoints

In addition to the original JSON artifact endpoints, Phase 8.2 now exposes:

- `POST /studies/workspace`
- `POST /studies/files?study_dir=...&role=sdtm|spec|reference|define|legacy`
- `GET /study-inputs?study_dir=...`
- `POST /runs/prepare`
- `POST /runs/{run_id}/datasets/{dataset}/generate-code`
- `POST /runs/{run_id}/datasets/{dataset}/code-review`
- `POST /runs/{run_id}/datasets/{dataset}/execute-approved-code`
- `GET /runs/{run_id}/review-summary?study_dir=...`

These endpoints summarize canonical study folders and run artifacts into a
browser-friendly shape. The UI should use these first; raw artifact JSON should
be treated as audit detail.

Important split:

```text
generate-code
  builds LLM context, calls the provider, parses the strict JSON response, and
  writes build_{dataset}.R

code-review
  records approve/reject before execution

execute-approved-code
  runs the already-generated script with LocalRRunner only after approval
```

The older `POST /runs` endpoint is still available for compatibility and smoke
tests, but the redesigned UI does not use it as the primary workflow.

## Demo Study Preparation

The `POST /demo-study` endpoint prepares a local study folder for the UI. By
default it copies only the root-level files from:

```text
D:\Archive\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\demo-data
```

into:

```text
.tmp_tests/ui_demo_study/demo_adam_{timestamp}/
```

Prepared shape:

```text
demo_adam_{timestamp}/
  input_sdtm/
    ae.csv
    dm.csv
    ex.csv
  input_spec/
    ads_adae_full.csv
    ads_adsl_full.csv
  reference_adam/
    adae.csv
    adsl.csv
  runs/
```

Source mapping:

- `ae.csv`, `dm.csv`, and `ex.csv` are SDTM source data.
- `adsl.csv` and `adae.csv` are reference ADaM outputs used for final
  comparison evidence.
- `ads_adae_full.csv` and `ads_adsl_full.csv` are spec files copied into
  `input_spec/` with their original filenames preserved.
- `PSY201/` under the demo source is a separate project and is intentionally not
  copied into this demo workflow.

The copied ADS metadata CSV files are treated as user-provided spec evidence.
They are not silently converted into production-approved clinical rules.

If local `Rscript` exists at `C:\Dev\R-4.5.2\bin\Rscript.exe`, the demo form
defaults to `llm_downstream_r_sandbox`. If not, it defaults to
`llm_downstream_provider`, which writes and validates structural artifacts but
does not execute generated R code locally.

## What The Page Does Not Do Yet

- no run history database
- no asynchronous job queue
- no authentication
- no artifact download packaging
- no streaming logs
- no final ADaM-vs-reference comparison table beyond showing the reference
  preview as comparison evidence
- no production CDISC/P21 validation
- no full clinical review workflow beyond local approve/reject JSON records

## Design Boundary

The page does not import or call LangGraph directly. It uses the HTTP endpoints
from `docs/phase8_1_api_contract.md`.

The current run action is synchronous. For long real-provider or real-R runs,
the browser waits for the HTTP request to finish. A later Phase 8 step should
move runs to an asynchronous job model once the review screens are clearer.
