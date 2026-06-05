# Run Artifact Lifecycle

This project now has three different kinds of generated files. They should not
be cleaned with one broad rule.

## 1. Test Temporary Artifacts

Location:

```text
.tmp_tests/session_*/
```

Purpose:

- temporary study folders used by unit tests
- temporary run folders, sqlite checkpoints, generated R code, and output CSVs
- scratch JavaScript files used by UI tests

Rule:

- normal test runs remove the whole session folder when the Python process exits
- set `ADAM_AGENT_KEEP_TEST_ARTIFACTS=1` only when debugging a failed test
- `.tmp_tests/` itself is ignored by Git and can be safely deleted between runs

## 2. Development Output And Logs

Locations:

```text
output/
test-results/
ui_server_*.log
```

Purpose:

- Playwright screenshots
- regression logs
- local uvicorn logs

Rule:

- safe to delete during local cleanup
- should not be treated as clinical audit evidence

## 3. Product Run Artifacts

Location:

```text
studies/{study_id}/runs/{run_id}/
```

Purpose:

- graph state and workflow projection
- LangGraph/sqlite checkpoints
- dependency plans
- LLM prompt/response/context artifacts
- generated R code
- static check reports
- generated ADaM outputs
- validation, diagnostics, compare reports, and audit manifests

Rule:

- do not auto-delete completed or reviewed runs
- do not delete graph state without also deleting its generated outputs and audit
  references
- draft or failed runs may be deleted only through an explicit product action
  that explains what will be removed

## Product Follow-Up

The UI/API should later expose a run manager with these actions:

- delete draft run
- delete failed run
- archive completed run
- keep last N local development runs
- export audit package before deletion

Until that exists, manual cleanup should stay limited to `.tmp_tests/`,
`output/`, `test-results/`, and local server logs.
