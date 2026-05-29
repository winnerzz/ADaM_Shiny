# Phase 7 Closeout

Phase 7 made the project usable as a backend orchestration prototype for
multi-dataset ADaM generation.

It did not make the system production validated. The purpose was to prove that
the architecture can plan dependencies, run dataset subgraphs independently,
call an isolated LLM provider boundary, execute generated R through a sandbox
boundary, and preserve audit evidence.

## What Phase 7 Proves

- A study run can request one or more target ADaM datasets.
- The system can plan dataset dependencies before execution.
- User-provided `input_spec/` is treated as the primary dependency source.
- Secondary evidence such as legacy SAS and define.xml can be scanned for
  dependency conflicts.
- Missing dependencies do not run silently. They require either an available
  artifact or explicit user approval for system generation.
- Dataset state is isolated. One dataset failure does not mutate another
  dataset's repair count or result.
- ADaM targets, including ADSL, can build an LLM context package.
- The LLM layer is provider-isolated.
- Generated downstream R code is persisted before execution.
- Downstream execution can be stubbed or run through local `Rscript`.
- Failures are classified and written to diagnostics.
- Code/contract/runtime failures may trigger one repair attempt.
- Spec/input/environment failures are not hidden by repair loops.

## Current Execution Modes

| Mode | Meaning | Real R execution | Typical status |
|---|---|---:|---|
| `stub` | Phase 3 graph skeleton behavior | no | `completed` or `failed` |
| `real_adsl_minimal` | retired legacy ADSL template mode; no longer accepted by DatasetGraph | no product path | `failed` |
| `llm_downstream_stubbed` | mock LLM plus structural stub R runner | no | `completed_stub` |
| `llm_downstream_provider` | configured LLM provider plus structural stub R runner | no | `completed_stub` |
| `llm_downstream_r_sandbox` | configured LLM provider plus local `Rscript` | yes | `completed` or `failed` |

ADSL is no longer a special deterministic-template branch in the current
product architecture. It follows the same ADaM flow as ADAE/ADCM/ADLB:
approved input spec or approved draft spec, LLM-generated R, human code review,
approved R execution, validation, and audit artifacts. The old
`src/adam_agent/adsl/` package remains only as Phase 5 legacy/regression code.

## Minimal Study Shape For Downstream Runs

For an ADAE smoke run:

```text
PSY201/
  input_sdtm/
    ae.csv
  input_spec/
    adae.json
  reference_adam/
    adsl.csv
  runs/
```

`reference_adam/adsl.csv` is compare/output-shape evidence only. It does not
satisfy ADAE's runtime dependency on a generated or user-approved upstream
ADSL, and it must not define derivation logic by itself.

## Mock Downstream Smoke Command

```powershell
python -m adam_agent.cli run-study `
  --study-dir "D:\path\to\PSY201" `
  --run-id run_phase7_mock `
  --target ADAE `
  --config studies\_template\configs\mock_downstream.json `
  --execution-mode llm_downstream_provider
```

This should produce a `completed_stub` dataset result. That confirms graph,
planning, context, LLM contract, and artifact wiring. It does not prove a real
ADaM derivation.

## Local R Sandbox Smoke Command

```powershell
python -m adam_agent.cli run-study `
  --study-dir "D:\path\to\PSY201" `
  --run-id run_phase7_r `
  --target ADAE `
  --config studies\_template\configs\mock_downstream.json `
  --execution-mode llm_downstream_r_sandbox `
  --rscript-path "C:\Dev\R-4.5.2\bin\Rscript.exe"
```

This should produce `outputs/adae.csv` when the generated mock R code runs.
Because the provider is mock, the result is an execution-chain smoke test, not a
clinical derivation claim.

## Output Checklist

After a study run, inspect:

```text
runs/{run_id}/planning/dependency_plan.json
runs/{run_id}/planning/dependency_review.md
runs/{run_id}/llm/{dataset}_context.json
runs/{run_id}/llm/{dataset}_response.json
runs/{run_id}/code/build_{dataset}.R
runs/{run_id}/outputs/{dataset}.csv
runs/{run_id}/validation/{dataset}_validation_report.json
runs/{run_id}/audit/manifest.json
```

If a failure occurred or was repaired, also inspect:

```text
runs/{run_id}/diagnostics/{dataset}_failure_report.json
```

## Handoff To Phase 8

Phase 8 should build the human-facing workflow on top of these backend artifacts.

Recommended first UI/API views:

- create a run
- select target datasets
- show dependency plan and blocked dependencies
- show LLM exposure/provider configuration
- show generated code and validation output
- show failure diagnosis and recommended route
- let a user approve dependency generation or request rerun

Phase 8 should not broaden clinical derivation scope before these review and
audit surfaces are usable.
