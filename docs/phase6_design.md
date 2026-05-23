# Phase 6 Design - Failure Diagnosis and Rollback MVP

Last updated: 2026-05-23

Phase status: design started

## Purpose

Phase 6 makes failures explicit and routable.

Phase 5 can run a minimal ADSL loop. When it fails, the system should not only
return a raw R error string. It should classify the failure and tell the graph
what the next safe route should be.

Plain language:

```text
run ADSL
  -> if it fails, diagnose why
  -> write failure_report.json
  -> tell the graph whether to fail, revise spec, repair code, or ask human
```

## Non-Goals

Phase 6 MVP should not implement full autonomous repair.

Do not include these yet:

- LLM-generated code repair
- automatic spec rewriting
- multi-step rollback UI
- real human review screen
- production-grade CDISC/P21 diagnosis
- full compare-driven repair

Those belong after the diagnosis contract is stable.

## Why This Phase Matters

Without diagnosis, every failure looks like a code error. That creates the bad
loop we discussed earlier:

```text
wrong spec -> generated code fails -> repair code -> generated code fails again
```

The graph needs enough structure to say:

```text
This is not a code repair problem. Go back to spec or input review.
```

## Phase 6 MVP Scope

The first implementation should focus on the real Phase 5 ADSL path.

Inputs to diagnosis:

- input scan errors
- DM/EX profile errors
- local R execution result
- validation report

Outputs from diagnosis:

```text
runs/{run_id}/diagnostics/adsl_failure_report.json
```

The report should contain:

- `FailureRecord`
- root-cause-specific `failure_id`, for example
  `failure_adsl_missing_required_input`
- recommended route
- short human-readable reason
- related artifact ids when available
- raw stderr/stdout excerpts only as short strings

## Failure Classes

Use the existing Phase 2 `FailureRecord.failure_type` values.

Initial mappings:

| Situation | failure_type | root_cause | recommended_route |
|---|---|---|---|
| Missing DM/EX input | `input_error` | `missing_required_input` | `fail` |
| `.sas7bdat` cannot be profiled because R/haven is unavailable | `input_error` | `unsupported_or_unreadable_input` | `fail` |
| R exits because a source column is missing | `spec_error` | `source_variable_missing` | `revise_spec` |
| R exits because package/function/runtime fails | `sandbox_error` | `r_runtime_error` | `fail` |
| Validation fails because output is missing required columns | `validation_error` | `output_contract_violation` | `revise_spec` |
| Validation fails because `USUBJID` is missing or duplicated | `validation_error` | `key_integrity_error` | `human_review` |
| Unknown stderr or validation failure | `unknown` | `unknown` | `human_review` |

## What Counts as Rollback in Phase 6 MVP

Rollback is represented as a route, not a UI action yet.

Examples:

```text
recommended_route = revise_spec
recommended_route = repair_code
recommended_route = human_review
recommended_route = fail
```

The graph can then use this route in later phases to move backward to the right
node. In this phase, recording the route is enough.

This means Phase 6 MVP is a diagnosis layer, not a complete repair/rollback
system.

For the real Phase 5 ADSL path, a failed run must summarize as failed after
recording the recommended route. It must not pass through the stub
`repair_code` or `revise_spec` nodes, because those stubs would falsely imply a
real repair or rollback happened.

## Artifact Boundary

Add a diagnostics folder:

```text
runs/{run_id}/diagnostics/
  adsl_failure_report.json
```

The artifact should be registered as:

```text
kind = tool_log
role = audit
dataset = ADSL
```

This keeps schema changes small. A future phase can add a dedicated
`failure_report` artifact kind if needed.

## Graph Boundary

For `execution_mode = real_adsl_minimal`:

- successful ADSL runs continue as before
- failed ADSL runs should carry:
  - `failure_type`
  - `real_run_error`
  - `failure_records`
  - `audit_artifacts`
  - `summary.failure_ids`

The graph should not throw expected input/runtime failures through the caller.

## Exit Criteria

Phase 6 MVP is complete when:

- failed ADSL service runs write `adsl_failure_report.json`
- graph-level real ADSL failure returns a failed `DatasetResultSummary`
- failure type and recommended route are machine-readable
- tests cover at least:
  - missing required input
  - validation failure
  - a source-variable/spec-style failure
- full test suite passes

## Open Follow-Ups

- Real code repair node
- Real spec revision node
- Human review UI
- Persist full failure history across repeated attempts
- Compare-report-driven diagnosis
