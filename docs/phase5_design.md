# Phase 5 Design - ADSL Minimal Real Loop

Last updated: 2026-05-23

Phase status: implemented locally, awaiting review and commit

## Purpose

Phase 5 is the first step where the project should produce a real output file,
not only graph stubs and tool interfaces.

The target is one dataset:

```text
ADSL minimal real loop
```

Plain language:

```text
read local DM/EX
  -> load or draft a small ADSL spec
  -> create approved-spec metadata for demo or reviewed variables
  -> generate deterministic R code
  -> run local R
  -> write adsl.csv
  -> validate key structure
  -> write audit artifacts
```

## Non-Goals

Phase 5 should not try to become a production ADaM generator.

Do not include these in the first Phase 5 implementation:

- all ADaM datasets
- full ADSL variable coverage
- full CDISC/P21 validation
- real LLM provider calls
- automatic production population-flag rules
- Docker or remote sandboxing
- UI workflow
- regulatory-grade validation claims

Those belong to later phases after the single-dataset loop is stable.

## Core Boundary: Starter Variables Are Not Production Rules

The first ADSL variable set is an MVP starter set for a runnable demo-style loop.
It is not a universal ADaM standard and must not be presented as production
logic.

Starter variable candidates:

| Variable or concept | Phase 5 role | Production caution |
|---|---|---|
| `USUBJID` | subject identifier from `DM` | low risk when present and unique |
| direct demographics from `DM` | optional direct mappings such as `AGE`, `SEX`, `RACE` when present | do not invent or recode without evidence |
| treatment label or assignment | optional direct mapping from `DM` when present | treatment derivation is study-specific |
| `TRTSDT` | candidate first exposure date from `EX` | review-required unless supported by spec/SAP/legacy/human approval |
| `TRTEDT` | candidate last exposure date from `EX` | review-required unless supported by spec/SAP/legacy/human approval |
| `SAFFL` | demo/MVP candidate derived from exposure presence | not a production safety-population rule |

Hard rule:

```text
If treatment dates or population flags are inferred from DM/EX profile patterns
or starter templates without stronger study evidence, mark them
review_required = true.
```

For current implementation work, an explicit `demo_only_no_review` mode may
allow the loop to run through, but the run must record that this is a demo
bypass and not production approval.

## How Phase 5 Connects to Phase 0-4

Phase 5 should reuse the first-stage architecture instead of rewriting it.

| First-stage output | Phase 5 usage |
|---|---|
| Phase 1 input/output contracts | Use `studies/{study_id}/` and `runs/{run_id}/` folders |
| Phase 2 schemas | Use `ArtifactRef`, `SpecDocument`, `ApprovalRecord`, and validation summaries |
| Phase 3 graph skeleton | Replace only the ADSL stub internals with real work |
| Phase 4 tools | Use scanner, profiler, artifact store, config loader, and R runner boundary |

Implementation principle:

```text
Keep the StudyGraph/DatasetGraph shape. Replace stub nodes with ADSL-specific
tool calls and deterministic implementation.
```

## Proposed Phase 5 Subtasks

### 1. Design and Review

Create and review this document before coding.

Questions to settle:

- Is ADSL minimal real loop the correct Phase 5 target?
- Are starter variables clearly labeled as MVP candidates?
- Is `demo_only_no_review` acceptable for local demo runs only?
- Should `.sas7bdat` support be implemented in Python, R, or both?

### 2. Study Fixture for ADSL

Add a small local fixture under `tests/fixtures/` or `studies/_template/` for
the first runnable loop.

Minimum data:

```text
DM:
  USUBJID
  optional STUDYID
  optional AGE / SEX / RACE
  optional ARM / ACTARM

EX:
  USUBJID
  EXSTDTC
  optional EXENDTC
```

The fixture should be synthetic or processed demo data only.

### 3. Data Loading

CSV support:

- continue using standard library CSV for profiling
- generated R code can use `readr` or base R

`.sas7bdat` support:

- supported first through the generated R runtime, using `haven::read_sas()`
- Python profiling remains metadata-light in Phase 5 and does not require
  `pyreadstat`
- for `.sas7bdat`, Phase 5 now asks R/haven to read the real column names before
  drafting the starter spec, so the spec does not claim source variables that
  were only guessed by Python
- if the R package `haven` is unavailable, return a clear R execution failure
  instead of pretending the data was read

Do not block the first CSV loop on `.sas7bdat` package installation.

### 4. Spec Loading or Drafting

Preferred path:

```text
input_spec/adsl_spec.json
  -> validate into SpecDocument
```

Fallback path:

```text
DM/EX profiles + optional evidence
  -> create ADSL starter draft spec
  -> mark high-risk candidates review_required
```

Phase 5 should support an explicit demo bypass:

```text
demo_only_no_review
```

This means:

- useful for local development
- not equivalent to human clinical approval
- must be recorded in approval metadata and audit

### 5. Deterministic R Code Generation

Phase 5 should start with deterministic R code generation, not real LLM code
generation.

Reason:

- avoids API key and model instability
- lets us verify the data/R/audit loop first
- still uses the same output folder and artifact contracts

The generated R file should live at:

```text
runs/{run_id}/code/build_adsl.R
```

It should produce:

```text
runs/{run_id}/outputs/adsl.csv
```

### 6. Local R Runner

Add a real local runner behind the existing runner boundary.

Suggested class:

```text
LocalRRunner
```

Behavior:

- call local `Rscript`
- set working directory to the run folder or study folder
- capture stdout, stderr, exit code
- return structured `RRunResult`
- do not run arbitrary user-provided commands outside the generated script path

This is a local runner, not a production sandbox.

### 7. Validation

Add minimal structured validation:

- output file exists
- required columns present
- one row per `USUBJID`
- no missing `USUBJID`
- `TRTSDT <= TRTEDT` when both are present
- `SAFFL` values are only expected flag values if generated

Validation should write:

```text
runs/{run_id}/validation/adsl_validation_report.json
```

If reference ADSL exists, compare can be minimal in a later Phase 5 iteration:

- row count
- key overlap by `USUBJID`
- column overlap

The current Phase 5 starter implementation writes a skipped compare report.
Reference ADSL comparison is intentionally not implemented yet.

## Proposed ADSL Runtime Flow

```text
scan_study_inputs
  -> profile DM/EX
  -> load_or_draft_adsl_spec
  -> approve_or_demo_bypass_spec
  -> render_build_adsl_R
  -> run_local_R
  -> validate_adsl
  -> compare_reference_if_available
  -> write_manifest
```

This can be implemented first as a focused service/function and then wired into
the ADSL branch of `DatasetGraph`.

## Suggested New Modules

```text
src/adam_agent/adsl/
  spec_builder.py      # load or draft starter ADSL spec
  r_template.py        # deterministic R code rendering
  validator.py         # minimal ADSL validation
  runner.py            # orchestration helper for ADSL real loop, if needed

src/adam_agent/
  cli.py               # local development CLI for running the ADSL loop

src/adam_agent/tools/
  r_runner.py          # add LocalRRunner next to StubRRunner

tests/
  test_phase5_adsl_loop.py
```

Keep these modules small. Phase 5 should not create a broad generic ADaM engine
yet.

## Exit Criteria

Phase 5 is complete when:

- a synthetic/demo study with DM and EX can run locally
- `adsl.csv` is written under `runs/{run_id}/outputs/`
- `build_adsl.R` is written under `runs/{run_id}/code/`
- draft and approved spec JSON files are written
- validation report JSON is written
- compare report JSON exists, even if skipped
- manifest includes input, code, output, spec, validation, and compare artifacts
- tests pass without a real LLM API key
- production-rule caveats are visible in spec/audit for treatment dates and
  `SAFFL`

## Phase 5 Review Questions

Resolved in the current local implementation:

1. Phase 5 begins with CSV execution. `.sas7bdat` remains supported at the file
   boundary and in generated R code through `haven::read_sas()`, but full
   Python-side profiling is not implemented yet.
2. Deterministic R code generation is used before real LLM code generation.
3. Demo runs use `demo_only_no_review`, and the approval record states that the
   starter rules are not production-approved.
4. The focused ADSL service was implemented first, then wired into
   `DatasetGraph` behind explicit `execution_mode = real_adsl_minimal`.

Current local verification:

```text
python -m unittest discover -s tests -p "test_*.py"
Ran 46 tests ... OK
```

Latest `.sas7bdat` decision:

- Python is not the primary `.sas7bdat` reader in Phase 5.
- The scanner recognizes `dm.sas7bdat` and `ex.sas7bdat`.
- `SDTMReader` still reports Python-side `.sas7bdat` profiling as not
  implemented.
- `run_adsl_minimal()` profiles `.sas7bdat` columns through a short R/haven
  script before drafting the spec, then the generated R script reads the actual
  data through `haven::read_sas()`.
- This keeps the product direction aligned with the current priority: make the
  R side run first, without adding a Python `.sas7bdat` dependency.

Latest review-driven fixes:

- CSV input is read with `colClasses = "character"` so subject identifiers such
  as `01` are not silently converted to `1`.
- `.sas7bdat` missing date columns no longer produce fake `TRTSDT` or `TRTEDT`
  source-variable evidence.
- Graph and CLI expected failures now return structured failed results instead
  of crashing the caller.

Local manual run shape:

```powershell
python -m adam_agent.cli run-adsl-minimal `
  --study-dir studies/PSY201 `
  --run-id run_001 `
  --rscript-path C:\Dev\R-4.5.2\bin\Rscript.exe
```
