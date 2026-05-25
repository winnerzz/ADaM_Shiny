# Phase 8.2 Local Web UI

Phase 8.2 adds a minimal browser UI on top of the Phase 8.1 API.

This is still a local prototype. It is meant to make the Phase 7/8 backend easy
to inspect, not to solve deployment, access control, or production review.

## Start Command

```powershell
uvicorn adam_agent.api.app:app --reload --host 127.0.0.1 --port 8000
```

Open:

```text
http://127.0.0.1:8000
```

## What The Page Does

The page lets a local user:

- enter a study folder path
- enter a run id
- choose target datasets
- choose an execution mode
- pass a config path
- optionally pass an Rscript path
- optionally approve dependency datasets
- run the study graph through `POST /runs`
- inspect dataset result summaries
- open dependency plan JSON
- open validation JSON
- open diagnostics JSON when present
- open audit manifest JSON
- open the LLM context JSON

## What The Page Does Not Do Yet

- no file upload
- no run history database
- no asynchronous job queue
- no authentication
- no human approval writeback
- no artifact download packaging
- no streaming logs

## Design Boundary

The page does not import or call LangGraph directly. It uses the HTTP endpoints
from `docs/phase8_1_api_contract.md`.

The current run action is synchronous. For long real-provider or real-R runs,
the browser waits for the HTTP request to finish. A later Phase 8 step should
move runs to an asynchronous job model once the review screens are clearer.
