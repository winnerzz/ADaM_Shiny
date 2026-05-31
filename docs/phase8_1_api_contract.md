# Phase 8.1 API Contract

Phase 8.1 exposes a local FastAPI backend for ADaM Agent Studio.

The current product path is a graph-controlled split flow. The browser and any
external caller should treat `GraphGateway` as the workflow boundary: API calls
start graph transitions, human review calls record decisions at graph-owned
gates, and read endpoints render graph-owned state and artifacts.

`POST /runs` is still present, but only as a legacy compatibility and smoke-test
endpoint.

## Entry Point

```text
adam_agent.api.app:app
```

Local development command:

```powershell
$env:PYTHONPATH = "D:/Archive/Research/Projects/ADaM_Shiny_LangGraph/src"
uvicorn adam_agent.api.app:app --reload --host 127.0.0.1 --port 8000
```

## Current Product Flow

```text
create/open workspace
  -> upload SDTM/spec/define/reference/legacy files
  -> prepare run dependency plan
  -> review dependency plan if needed
  -> finalize one dataset's inputs
  -> if no input_spec: review generated draft spec
  -> generate R code
  -> review generated code
  -> execute approved R code
  -> preview/download/compare output
  -> triage terminal failure if execution fails
```

State ownership:

- `runs/{run_id}/graph_state.json` is the product state source.
- `runs/{run_id}/workflow_state.json` is a compatibility projection generated
  from graph state for the current UI/API read path.
- FastAPI service helpers should validate requests, resolve config/provider
  settings, shape responses, and expose artifact previews. They should not own
  workflow state transitions.

## Workspace And Inputs

### POST /product-workspace

Creates a default local study workspace without requiring the user to choose a
technical path first.

Response includes:

- `study_id`
- `study_dir`
- `run_id`
- default config/Rscript hints
- current `input_summary`

### POST /studies/workspace

Creates or opens an explicit local study workspace.

Request:

```json
{
  "study_dir": "D:/path/to/PSY201",
  "study_id": "PSY201"
}
```

### POST /studies/files

Uploads files into one canonical input role.

Query parameters:

```text
study_dir=D:/path/to/PSY201
role=sdtm|spec|define|reference|legacy
study_id=PSY201
```

Body: multipart file upload.

Canonical folders:

- `sdtm` -> `input_sdtm/`
- `spec` -> `input_spec/`
- `define` -> `input_define/`
- `reference` -> `reference_adam/`
- `legacy` -> `legacy_code/`

After upload, the backend asks `GraphGateway` to mark existing graph runs stale
when the input fingerprint changed. The response reports:

- `input_fingerprint`
- `input_diff`
- `touched_graph_runs`
- `touched_runs` for older compatibility projections
- `skipped_graph_runs`

### GET /study-inputs

Returns a human-oriented scan of the current workspace inputs.

Query parameter:

```text
study_dir=D:/path/to/PSY201
```

Reference ADaM files are shown as comparison/output-shape evidence. They do not
silently define derivation logic.

## Run Planning

### POST /runs/prepare

Starts or refreshes a graph-owned dependency plan. This endpoint does not
generate code and does not run R.

Request:

```json
{
  "study_dir": "D:/path/to/PSY201",
  "run_id": "run_demo_api",
  "target_datasets": ["ADAE", "ADLB"],
  "approved_dependency_datasets": [],
  "study_id": "PSY201"
}
```

Response includes requested targets, planned targets, runnable datasets, blocked
datasets, dependency decisions, warnings, and graph/projection paths.

If the dependency plan needs user input, use `POST /runs/{run_id}/dependency-review`.

### POST /runs/{run_id}/dependency-review

Records a human decision for the graph-native dependency-review gate.

Request:

```json
{
  "study_dir": "D:/path/to/PSY201",
  "reviewer": "local_user",
  "decision": "approve",
  "notes": "Accept dependency plan for this test.",
  "approved_dependency_datasets": ["ADSL"]
}
```

## Dataset Split Flow

All dataset-level product endpoints below mutate graph-owned state through
`GraphGateway` and return compatibility projection metadata.

### POST /runs/{run_id}/datasets/{dataset}/finalize-inputs

Confirms that upload is complete for one target dataset.

Behavior:

- If a user-provided `input_spec` exists and passes the current checks, the next
  action is `generate_code`.
- If no approved input spec exists, the system generates a review-required draft
  spec from uploaded evidence.
- A same-run approved draft spec can be reused only when its input fingerprint
  still matches.

### POST /runs/{run_id}/datasets/{dataset}/draft-spec

Forces generation of a new draft spec for review. This is used when the graph
routes a failed run back to spec revision.

### POST /runs/{run_id}/datasets/{dataset}/draft-spec-review

Records approval or rejection of a generated draft spec.

Request:

```json
{
  "study_dir": "D:/path/to/PSY201",
  "reviewer": "local_user",
  "decision": "approve",
  "notes": "Accepted for local smoke test."
}
```

### POST /runs/{run_id}/datasets/{dataset}/generate-code

Generates auditable R code for one dataset without executing it.

The endpoint requires either:

- a user-provided approved input spec, or
- a same-run approved draft spec with matching input fingerprint.

Generated code still requires code review before execution.

### POST /runs/{run_id}/datasets/{dataset}/code-review

Records the human code-review decision.

Request:

```json
{
  "study_dir": "D:/path/to/PSY201",
  "reviewer": "local_user",
  "decision": "approve",
  "notes": "Approved for local execution."
}
```

### POST /runs/{run_id}/datasets/{dataset}/execute-approved-code

Runs previously approved generated R code through the local R execution
boundary. It does not generate new code and does not approve code by itself.

### POST /runs/{run_id}/datasets/{dataset}/terminal-failure-review

Records a human triage decision after terminal execution failure.

Supported decisions are controlled by the graph progress read model. Typical
actions include retry execution, revise spec, or skip/mark failed.

## LLM Configuration

### POST /llm/test-connection

Runs a minimal provider call without study data. This is for validating a
browser-supplied provider configuration.

Study-data exposure is controlled separately by the LLM exposure policy. Demo
data may be sent only when the request policy explicitly allows it.

## Graph Read Models

### GET /runs/{run_id}/progress

Returns graph-owned progress guidance for the UI:

- study status
- next study action
- target/runnable/blocked datasets
- review queue
- per-dataset next action
- output quality warnings
- graph/projection paths

The UI should prefer this endpoint for action availability and progress display.

### GET /runs/{run_id}/graph-state

Returns the canonical graph state. This endpoint is for debugging and handoff,
not for ordinary user-facing display.

### GET /runs/{run_id}/review-summary

Returns a UI-friendly review bundle. It prefers `graph_state.json` and falls
back to compatibility artifacts only when canonical graph state is unavailable.

## Output Preview, Compare, And Download

### GET /runs/{run_id}/datasets/{dataset}/table

Returns one page of a generated or reference table.

Query parameters:

```text
study_dir=D:/path/to/PSY201
kind=generated|reference
page=1
page_size=25
```

CSV files can be paged in the browser. `sas7bdat` files are accepted as study
inputs and can be profiled when local R plus `haven` is available, but full
browser table paging is currently CSV-first.

### GET /runs/{run_id}/datasets/{dataset}/compare

Compares generated ADaM output with reference ADaM when both are available.

Reference ADaM is comparison evidence, not derivation authority.

### GET /runs/{run_id}/datasets/{dataset}/download

Downloads one artifact.

Supported `kind` values:

```text
generated
reference
code
validation_report
compare_report
```

### POST /runs/{run_id}/artifacts/read

Reads a JSON artifact under `runs/{run_id}/`.

Request:

```json
{
  "relative_path": "llm/adae_context.json"
}
```

The path must stay under the requested run directory.

## Compatibility Artifact Reads

These endpoints still exist for compatibility and smoke/debug workflows. New UI
work should prefer `/progress`, `/graph-state`, and `/review-summary` when it
needs current workflow state.

### GET /runs/{run_id}/dependency-plan

Reads `runs/{run_id}/planning/dependency_plan.json`.

### GET /runs/{run_id}/audit-manifest

Reads `runs/{run_id}/audit/manifest.json`.

### GET /runs/{run_id}/datasets/{dataset}/validation

Reads `runs/{run_id}/validation/{dataset}_validation_report.json`.

### GET /runs/{run_id}/datasets/{dataset}/diagnostics

Reads `runs/{run_id}/diagnostics/{dataset}_failure_report.json`.

This endpoint may return 404 for successful runs because diagnostics are only
written when a failure occurred or an initial failure was repaired.

## Legacy Compatibility

### POST /runs

Legacy run-to-completion orchestration request.

This endpoint is retained for compatibility and smoke tests. It is synchronous,
but it is not the product path for LLM/R ADaM generation. Requests using
`llm_downstream_provider` or `llm_downstream_r_sandbox` are rejected because
they would bypass draft-spec, code-review, and execution approval gates. Use
`POST /runs/prepare` and the dataset-level split-flow endpoints for real
generation.

Allowed `POST /runs` `execution_mode` values are intentionally narrow:
`stub`, `llm_downstream_provider`, and `llm_downstream_r_sandbox`. `stub` is
the only mode that may complete through this legacy endpoint. The two
`llm_downstream_*` modes are accepted only so the endpoint can write the
`split_flow_required` compatibility projection and reject the request with a
clear migration message. Unknown modes are rejected before invoking the graph.

Legacy responses use:

```json
{
  "workflow_control": "legacy_run_to_completion_compatibility_shim",
  "graph_state_path": null,
  "workflow_state_path": "D:/path/to/PSY201/runs/run_demo_api/workflow_state.json"
}
```

The `graph_state_path: null` value is intentional. It distinguishes legacy
run-to-completion smoke output from the graph-owned product flow.

## Current Non-Goals

- production deployment
- authentication
- multi-user isolation
- background job queue
- streaming logs
- hardened OS/container R sandbox
- full CDISC/P21/company-standard rule engine
