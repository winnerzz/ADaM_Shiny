# Phase 8.1 API Contract

Phase 8.1 adds a local FastAPI backend around the Phase 7 LangGraph workflow.

The goal is not to build the final UI yet. The goal is to give the future UI a
small, stable HTTP boundary so it does not call LangGraph internals directly.

## Scope

Included:

- health check
- create one synchronous study run
- return study/dataset run summary
- read dependency plan
- read validation report
- read diagnostics report
- read study audit manifest
- read a JSON artifact under a run directory

Not included:

- database persistence
- background job queue
- authentication
- multi-user run isolation
- file upload UI
- streaming logs
- production deployment

## App Entry Point

```text
adam_agent.api.app:app
```

Local development command:

```powershell
uvicorn adam_agent.api.app:app --reload --host 127.0.0.1 --port 8000
```

## POST /runs

Create and run a study orchestration request.

The current implementation is synchronous. The HTTP response returns after the
graph run finishes.

Request:

```json
{
  "study_dir": "D:/path/to/PSY201",
  "run_id": "run_demo_api",
  "target_datasets": ["ADAE"],
  "config_path": "studies/_template/configs/mock_downstream.json",
  "execution_mode": "llm_downstream_provider",
  "approved_dependency_datasets": [],
  "rscript_path": null,
  "study_id": null
}
```

Response:

```json
{
  "study_id": "PSY201",
  "run_id": "run_demo_api",
  "status": "completed",
  "execution_mode": "llm_downstream_provider",
  "requested_datasets": ["ADAE"],
  "target_datasets": ["ADSL", "ADAE"],
  "runnable_datasets": ["ADAE"],
  "blocked_datasets": [],
  "dependency_review_status": "warning",
  "run_dir": "D:/path/to/PSY201/runs/run_demo_api",
  "audit_manifest": "D:/path/to/PSY201/runs/run_demo_api/audit/manifest.json",
  "dataset_results": []
}
```

## GET /runs/{run_id}/dependency-plan

Query parameter:

```text
study_dir=D:/path/to/PSY201
```

Reads:

```text
runs/{run_id}/planning/dependency_plan.json
```

## GET /runs/{run_id}/audit-manifest

Query parameter:

```text
study_dir=D:/path/to/PSY201
```

Reads:

```text
runs/{run_id}/audit/manifest.json
```

## GET /runs/{run_id}/datasets/{dataset}/validation

Query parameter:

```text
study_dir=D:/path/to/PSY201
```

Reads:

```text
runs/{run_id}/validation/{dataset}_validation_report.json
```

## GET /runs/{run_id}/datasets/{dataset}/diagnostics

Query parameter:

```text
study_dir=D:/path/to/PSY201
```

Reads:

```text
runs/{run_id}/diagnostics/{dataset}_failure_report.json
```

This endpoint may return 404 for successful runs because diagnostics are only
written when a failure occurred or an initial failure was repaired.

## POST /runs/{run_id}/artifacts/read

Query parameter:

```text
study_dir=D:/path/to/PSY201
```

Request:

```json
{
  "relative_path": "llm/adae_context.json"
}
```

The path must stay under:

```text
runs/{run_id}/
```

Only JSON artifacts are supported in Phase 8.1.

## Design Notes

- The API is local-first and path-based for now.
- API keys should not be sent through these endpoints in Phase 8.1.
- The UI should use these endpoints instead of reading internal graph state.
- Future Phase 8 work can add asynchronous runs and run history once the basic
  review screens are clear.
