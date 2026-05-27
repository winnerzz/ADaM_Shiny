# Phase 8 UI Redesign - Human Review First Workflow

Last updated: 2026-05-26

Status: design draft

## Purpose

The previous Phase 8 UI direction failed because it exposed backend artifacts and
JSON-heavy run output before it explained the user's real workflow.

The redesigned UI must follow the clinical programmer's mental model:

```text
prepare study inputs
  -> inspect what the system recognized
  -> choose target ADaM datasets
  -> generate R code from SDTM + spec + evidence
  -> review generated R code, assumptions, and risk points
  -> approve execution
  -> run R sandbox
  -> review generated ADaM, validation, comparison, and audit
```

This is not a dashboard for raw LangGraph internals. It is a local ADaM Agent
Studio for controlled generation and review.

## Core Product Rule

The UI must separate these actions:

```text
Generate code
Approve code
Execute sandbox
Review output
```

The current backend can run much of the chain, but some parts are still coupled.
The UI design should make the desired product boundary explicit before further
implementation.

## Target User

Primary user:

- statistical programmer or clinical data programmer
- understands SDTM/ADaM/specs at a working level
- may not understand LangGraph, JSON artifacts, or internal state machines

Therefore the UI should speak in workflow terms:

```text
SDTM files
Spec files
Reference ADaM
Generated R code
Risk points
Sandbox execution
Generated ADaM
Validation / comparison
Audit trail
```

Avoid making the user interpret internal terms first:

```text
state
artifact ref
dataset graph
dependency resolution JSON
LLM call record
```

Those can exist under an Advanced Audit area.

## Canonical Study Workspace Mapping

The UI should not invent a new storage model. It should write into the Phase 1
study folder contract.

| UI area | User action | Backend folder | Current scanner support |
|---|---|---|---|
| SDTM | upload/select SDTM source datasets | `input_sdtm/` | yes, `StudyInputScanner` |
| SPEC | upload/select ADaM specs | `input_spec/` | yes, `StudyInputScanner` |
| Define | upload/select define.xml or metadata | `input_define/` | yes, `StudyInputScanner` |
| Reference ADaM | upload/select benchmark ADaM datasets | `reference_adam/` | yes, `StudyInputScanner` |
| Legacy / Other Evidence | upload/select SAS/R programs or notes | `legacy_code/` | yes, `StudyInputScanner` |
| Runs | generated artifacts per run | `runs/{run_id}/` | yes, graph and runners write here |

Important data role examples for the current demo:

```text
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
```

`PSY201/` in the old demo folder is a separate project and must not be silently
mixed into this demo workspace.

## Main UI Screens

### 1. Study Setup

Goal:

Create or select a local study workspace.

Visible controls:

- Study name / study folder path
- New study button
- Open existing study button
- Current workspace summary

Backend connection:

- Create canonical folders if missing:
  - `input_sdtm/`
  - `input_spec/`
  - `input_define/`
  - `reference_adam/`
  - `legacy_code/`
  - `runs/`
- No graph execution yet.

User-facing output:

```text
Study workspace ready.
Next: add source files.
```

### 2. Input Preparation

Goal:

Let the user submit files by role, not by backend folder name.

Layout:

- SDTM panel
- SPEC panel
- Define panel
- Reference ADaM panel
- Legacy / Other Evidence panel

Each panel should show:

- upload/select button
- accepted formats
- files already present
- dataset/domain inferred from file name
- row count and columns when available
- warnings for unsupported or unreadable files

Backend connection:

- Files are copied or moved into canonical folders.
- Then call existing input summary logic:

```text
GET /study-inputs?study_dir=...
```

Current backend support:

- `summarize_study_inputs()` already previews CSV files.
- `.sas7bdat` can be recognized, but Python-side preview is limited.
- R-side `.sas7bdat` reading exists for execution paths through `haven`.

UI wording for `.sas7bdat`:

```text
Found SAS dataset. It can be used by the R sandbox when the required R package is available. Full table preview may be limited in the current UI.
```

### 3. Input Review

Goal:

Before generation, show what the system thinks the user provided.

Do not start LLM or R execution here.

Show grouped summary:

```text
SDTM recognized:
  DM: dm.csv, 20 rows, columns ...
  AE: ae.csv, 30 rows, columns ...
  EX: ex.csv, 96 rows, columns ...

Specs recognized:
  ADSL: ads_adsl_full.csv
  ADAE: ads_adae_full.csv

Reference ADaM recognized:
  ADSL: adsl.csv
  ADAE: adae.csv

Optional evidence:
  define.xml: not provided
  legacy code: not provided
```

UI decisions:

- Let user confirm file roles if inference is wrong.
- Let user remove or reassign a file before generation.
- Show invalid files clearly but do not block the whole study unless required
  input is missing for the selected target.

Backend gap:

- Current scanner infers by folder and filename.
- A polished UI may need a file-role override metadata file later, for example:

```text
study_dir/input_manifest.json
```

This is optional for first redesign.

### 4. Target Dataset Selection

Goal:

Let user choose what ADaM to generate.

Visible controls:

- dataset chips or checkboxes: `ADSL`, `ADAE`, `ADLB`, `ADCM`, custom `ADxx`
- execution mode selector, hidden under Advanced for normal users
- provider/config selector, also Advanced in first UI

Default behavior:

- If specs exist, suggest targets from spec files.
- If reference ADaM exists, show it as comparison evidence, not as the derivation
  source.
- If user chooses a downstream dataset, run dependency planning before code
  generation.

Backend connection:

- Current graph accepts:

```text
POST /runs
{
  "study_dir": "...",
  "run_id": "...",
  "target_datasets": ["ADAE"],
  "config_path": "...",
  "execution_mode": "...",
  "approved_dependency_datasets": [],
  "rscript_path": "..."
}
```

Current mismatch:

- `POST /runs` currently tends to run the graph through execution.
- The redesigned UI needs a separate dependency planning/preparation step before
  code generation and before sandbox execution.

Recommended future API boundary:

```text
POST /studies
POST /studies/{study_id}/files
GET  /studies/{study_id}/inputs
POST /runs/prepare
```

Where `prepare` does input scan and dependency planning only.

### 5. Dependency Plan Review

Goal:

Show the user whether requested ADaM datasets need other ADaM datasets.

Example:

```text
Requested: ADAE
Dependency check:
  ADAE requires ADSL
  ADSL is available from reference_adam/adsl.csv
Status: ready to generate ADAE
```

Or:

```text
Requested: ADTTE
Dependency check:
  ADTTE requires ADLB
  ADLB requires ADSL
  ADSL is available
  ADLB is missing
Action required: provide ADLB, approve system generation of ADLB, or skip ADTTE
```

Backend connection:

- Current graph writes:

```text
runs/{run_id}/planning/dependency_plan.json
runs/{run_id}/planning/dependency_review.md
```

- Current dependency modules:
  - `graph/dependencies.py`
  - `graph/dependency_resolution.py`

UI rule:

- Never say "auto-added means approved".
- Say:

```text
Required by dependency planning
```

not:

```text
Automatically generated
```

Backend gap:

- A clean UI should be able to request dependency planning without starting code
  generation or sandbox execution.

Recommended future API:

```text
POST /runs/{run_id}/plan
GET  /runs/{run_id}/dependency-plan
```

### 6. Generate R Code

Goal:

Generate R code, but do not execute it yet.

Input to this step:

- selected target dataset
- confirmed source files
- selected spec
- resolved dependency artifacts
- LLM provider/exposure policy

Backend components already available:

- `llm/context.py`
  - builds context package
- `llm/clients.py`
  - calls mock / OpenAI-compatible / Anthropic-style providers
- `llm/generated_code.py`
  - parses strict JSON response
  - writes raw response, parsed response, and `build_{dataset}.R`
  - rejects obvious unsafe R calls

Desired artifact outputs:

```text
runs/{run_id}/llm/{dataset}_context.json
runs/{run_id}/llm/{dataset}_response.json
runs/{run_id}/llm/{dataset}_parsed_response.json
runs/{run_id}/code/build_{dataset}.R
```

UI should show after generation:

- generated R code
- LLM assumptions
- LLM risk points
- used inputs
- expected output path
- blocked unsafe-code warning if any

Current backend mismatch:

- `run_downstream_adam()` currently combines:

```text
build context -> call LLM -> parse/write R -> run R -> validate
```

The redesigned UI needs this split:

```text
build context -> call LLM -> parse/write R
```

without running R.

Recommended future API:

```text
POST /runs/{run_id}/datasets/{dataset}/generate-code
GET  /runs/{run_id}/datasets/{dataset}/generated-code
```

### 7. Code Review

Goal:

Let the user approve or reject generated R before sandbox execution.

Screen sections:

- R code editor/viewer
- assumptions
- risk points
- input files used
- expected outputs
- provider and data exposure summary
- buttons:
  - Approve and run sandbox
  - Reject and regenerate
  - Save note / request edit
  - Download code

Important UI principle:

The default user sees clinical-programming review material, not raw JSON.
Raw JSON stays in Advanced Audit.

Approval record should eventually capture:

```text
approved_by
approved_at
approval_mode
notes
code_artifact_id
risk_points_seen
```

Backend gap:

- Current approval model exists in schemas, but the downstream code review step
  does not yet persist a user approval record before R execution.

Recommended future API:

```text
POST /runs/{run_id}/datasets/{dataset}/code-review
```

Example payload:

```json
{
  "decision": "approve",
  "reviewer": "local_user",
  "notes": "Approved for sandbox execution only."
}
```

### 8. Sandbox Execution

Goal:

Execute only approved generated R code.

Backend components already available:

- `tools/r_runner.py`
  - `LocalRRunner`
- `downstream/runner.py`
  - execution and validation logic, currently coupled with generation
- `execution_mode = llm_downstream_r_sandbox`

Desired behavior:

```text
read existing runs/{run_id}/code/build_{dataset}.R
verify it was approved
execute with LocalRRunner
write output to runs/{run_id}/outputs/{dataset}.csv
write validation report
write diagnostics on failure
update manifest
```

Recommended future API:

```text
POST /runs/{run_id}/datasets/{dataset}/execute-approved-code
```

UI should show:

- running status
- R stdout/stderr in collapsible technical section
- success/failure summary
- path to generated ADaM

Safety boundary:

- Generated code must remain inside `runs/{run_id}/code/`.
- Output must remain inside `runs/{run_id}/outputs/`.
- The R runner should not execute arbitrary uploaded scripts from outside the
  run folder.

### 9. Result Review

Goal:

Let user inspect the generated ADaM output and compare it to reference if
available.

Visible sections:

- Generated ADaM preview
- Key validation status
- Compare to reference ADaM, if available
- Diagnostics if failed
- Generated code and approval record
- Audit trail

Backend components already available:

- `build_run_review_summary()`
- `GET /runs/{run_id}/review-summary`
- validation artifacts
- diagnostics artifacts
- audit manifest artifacts

Current validation boundary:

- Current downstream validation is structural.
- UI must not label it as regulatory-grade CDISC/P21 validation.

Recommended UI wording:

```text
Structural validation passed.
This does not replace formal ADaM compliance validation.
```

### 10. Advanced Audit

Goal:

Give technical users access to full traceability without making it the main UI.

Show links to:

```text
planning/dependency_plan.json
planning/dependency_review.md
llm/{dataset}_context.json
llm/{dataset}_response.json
llm/{dataset}_parsed_response.json
code/build_{dataset}.R
validation/{dataset}_validation_report.json
diagnostics/{dataset}_failure_report.json
audit/manifest.json
```

Backend components already available:

- `GET /runs/{run_id}/dependency-plan`
- `GET /runs/{run_id}/audit-manifest`
- `GET /runs/{run_id}/datasets/{dataset}/validation`
- `GET /runs/{run_id}/datasets/{dataset}/diagnostics`
- `POST /runs/{run_id}/artifacts/read`

## Page-Level Workflow

Recommended first product layout:

```text
Top bar:
  Study name | Run ID | Provider status | Rscript status

Left workflow rail:
  1 Inputs
  2 Plan
  3 Generate Code
  4 Review Code
  5 Run Sandbox
  6 Review ADaM
  7 Audit

Main panel:
  current step content

Right panel:
  study readiness / warnings / next action
```

The UI should always answer:

```text
What did I provide?
What will the system generate?
What does it depend on?
What code did the LLM create?
What risks did it declare?
Have I approved execution?
What ADaM output was produced?
How was it validated or compared?
Where is the audit trail?
```

## State Machine For UI

The UI should treat a run as progressing through these human-readable states:

```text
workspace_ready
inputs_ready
plan_ready
code_generated
code_approved
sandbox_running
sandbox_completed
review_completed
failed_needs_attention
```

Possible transitions:

```text
workspace_ready -> inputs_ready
inputs_ready -> plan_ready
plan_ready -> code_generated
code_generated -> code_approved
code_generated -> code_rejected
code_rejected -> code_generated
code_approved -> sandbox_running
sandbox_running -> sandbox_completed
sandbox_running -> failed_needs_attention
sandbox_completed -> review_completed
```

This UI state does not need to replace LangGraph state. It is a product-level
view of the workflow.

## Backend Changes Required Later

No code is changed by this design document. The likely future implementation
work is:

1. Add upload/copy endpoints for canonical input folders.
2. Add a planning-only endpoint that does not generate code or run R.
3. Split downstream generation from execution:
   - generate code only
   - approve code
   - execute approved code
4. Persist code-review approval records.
5. Add side-by-side output/reference comparison summary.
6. Improve `.sas7bdat` preview using R-side profiling or optional Python reader.
7. Keep raw JSON artifacts in Advanced Audit, not in the main workflow.

## Minimal First Redesign Scope

The first corrected UI should not try to solve everything.

Minimum useful version:

- select or create study folder
- upload/copy files into five input role areas
- show recognized inputs using existing summary API
- choose target dataset
- run dependency planning
- generate code without execution
- show generated code, assumptions, risk points
- approve and execute sandbox
- show output preview and validation summary
- link to audit artifacts

Out of scope for the first corrected UI:

- production CDISC/P21 validation
- multi-user authentication
- cloud deployment
- full data warehouse integration
- visual drag-and-drop dependency editing
- complete SAS execution support

## Acceptance Criteria

The redesigned UI is acceptable only if a user can complete this story:

```text
I select SDTM files in the SDTM area.
I select ADaM spec files in the SPEC area.
I optionally select define/reference/legacy evidence.
The system tells me what it recognized.
I choose ADAE as the target.
The system tells me whether ADSL is needed and whether it is already available.
The system generates build_adae.R but does not run it yet.
I review the R code, assumptions, and risk points.
I approve sandbox execution.
The system runs Rscript locally and creates adae.csv.
I review the generated ADAE, validation summary, comparison evidence, and audit trail.
```

If the UI cannot support this story clearly, it is still not aligned with the
product goal.
