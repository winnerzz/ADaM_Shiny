# Code Agent V1 Design

Last updated: 2026-06-13

Status: draft for implementation

## Goal

Move R code generation from a one-shot LLM call to a graph-controlled code
agent loop.

The first version should build a reviewable code package:

```text
approved spec
-> inspect inputs
-> draft R code
-> static check
-> trial run in sandbox
-> limited repair loop
-> package code and evidence for human review
```

It must not turn LLM output directly into an official ADaM dataset. Official
outputs are still produced only after human code approval.

## Product Boundary

Code Agent V1 writes human-readable review artifacts under
`code_agent/{dataset}` and keeps the actual trial workspace short to avoid
Windows path-length failures:

```text
runs/{run_id}/code_agent/{dataset}/package.json
runs/{run_id}/code_agent/{dataset}/review.md
runs/{run_id}/_ca/{dataset}/a01/code.R
runs/{run_id}/_ca/{dataset}/a01/static_check.json
runs/{run_id}/_ca/{dataset}/a01/runtime_report.json
runs/{run_id}/_ca/{dataset}/a01/outputs/{dataset}.csv
```

It must not write or bless the official output path:

```text
runs/{run_id}/outputs/{dataset}.csv
```

The official output path is owned by the approved-code execution step.

## Current Problem

The current `Generate R Code` path mostly does this:

```text
build context
-> call LLM once
-> parse returned R code
-> write generated code artifacts
-> wait for human code review
```

That is useful, but it is not a code agent. The model does not reliably inspect
real inputs, run its code, read failures, repair, or produce runtime evidence
before review.

## Target User-Facing Flow

Rename the user action from:

```text
Generate R Code
```

to:

```text
Build Code Package
```

The package shown for review should include:

- candidate R code,
- static check result,
- trial run result,
- repair attempt summary,
- assumptions,
- risk points,
- used evidence,
- expected output path contract.

The next human action stays explicit:

```text
Approve code package -> Run approved code
Reject code package -> Rebuild code package or return to spec review
```

## LangGraph Role

The code agent is not an external black box. It should be represented as a
dataset-level subflow owned by `DatasetGraph` and persisted through
`GraphGateway`.

V1 can implement the loop as a plain Python component called by the existing
dataset graph node, but it must write state as if it were a graph-owned
operation. This keeps the door open to replace the component with a native
LangGraph subgraph later.

Target V2 subgraph shape:

```text
inspect_inputs
-> draft_code
-> static_check
-> trial_run
-> classify_failure
-> repair_code
-> package_for_review
```

Conditional exits:

```text
static/runtime success -> package_for_review
code error and attempts remain -> repair_code
spec/input error -> draft_spec/spec_review interrupt
environment error -> terminal_failure interrupt
attempts exhausted -> terminal_failure interrupt
```

## State Ownership Rules

These rules follow `docs/state_ownership.md`.

1. `graph_state.json` remains the durable product truth.
2. Code Agent V1 writes canonical state only through `GraphGateway`.
3. `workflow_state.json` is only a UI projection.
4. Browser state must not decide whether code is approved, trialed, failed, or
   executable.
5. Trial artifacts are evidence, not official output.
6. Any uploaded input change invalidates prior code-agent packages for affected
   datasets.

## New Schemas

Add:

```text
src/adam_agent/schemas/code_agent.py
```

Suggested models:

```text
CodeAgentTask
CodeAgentAttempt
CodeAgentResult
CodeAgentFailureClassification
```

Minimum fields:

```text
CodeAgentTask
- study_dir
- study_id
- run_id
- dataset
- approved_spec_path
- target_output_path
- trial_output_dir
- llm_provider
- llm_exposure
- rscript_path
- max_attempts

CodeAgentAttempt
- attempt
- code_path
- static_check_path
- runtime_report_path
- trial_output_path
- status
- errors
- warnings
- repair_reason

CodeAgentResult
- status
- final_code_path
- final_code
- review_package_path
- attempts
- assumptions
- risk_points
- used_evidence
- ready_for_human_review
- failure_classification
```

## New Agent Module

Add:

```text
src/adam_agent/agents/code_agent.py
```

Initial public function:

```text
build_code_package(task: CodeAgentTask) -> CodeAgentResult
```

Internal steps:

1. Build compact LLM context from approved spec and input profiles.
2. Call existing LLM client to draft R code.
3. Parse generated code with the existing generated-code parser.
4. Run static checks.
5. If static checks fail for repairable reasons, ask the model to repair.
6. If static checks pass, run the code in sandbox against a trial output path.
7. If runtime fails for repairable code reasons, ask the model to repair.
8. Stop after `max_attempts`.
9. Write a review package.

## Repair Scope

V1 should repair only obvious code problems:

- R syntax error,
- missing output write,
- unsafe or blocked function usage,
- known column quoting issue,
- output path mismatch.

V1 should not pretend to repair clinical logic.

If the failure suggests any of these, stop and ask for human review:

- missing source domain,
- missing required variable,
- spec contradicts available data,
- dependency output is unavailable,
- R environment is unavailable,
- `haven` or required package is missing,
- memory allocation failure.

## Existing Code To Reuse

Reuse these modules instead of creating parallel systems:

- `adam_agent.llm.clients`
- `adam_agent.llm.generated_code`
- `adam_agent.llm.prompt_compaction`
- `adam_agent.tools.static_rules`
- `adam_agent.tools.sandbox`
- `adam_agent.tools.r_runner`
- `adam_agent.tools.sdtm_reader`
- `adam_agent.tools.compare`

## Integration Points

### API

Keep the existing endpoint for compatibility:

```text
POST /runs/{run_id}/datasets/{dataset}/generate-code
```

But update its product meaning:

```text
build a code package for review
```

The UI can display this as `Build Code Package`.

### Service

Update:

```text
src/adam_agent/api/service.py
```

Function:

```text
generate_dataset_code()
```

It should call `GraphGateway.generate_code()` and return code-agent evidence in
the existing response shape plus new optional fields.

### GraphGateway

Update:

```text
src/adam_agent/graph/gateway.py
```

Function:

```text
generate_code()
```

It should:

1. validate dependency/spec state,
2. call the dataset graph product code-generation node,
3. persist code-agent attempts into `DatasetRunState`,
4. add artifacts to the dataset state,
5. return a UI-ready next action of code review.

### DatasetGraph

Update:

```text
src/adam_agent/graph/dataset_graph.py
```

The current code-generation node should call `build_code_package()`.

In V1 this can stay inside the existing DatasetGraph instead of adding a full
nested LangGraph. The persisted state must still make the code-agent loop
auditable.

## Artifact Layout

Implemented layout:

```text
runs/{run_id}/code_agent/{dataset}/
  package.json
  review.md
runs/{run_id}/_ca/{dataset}/
  a01/
    code.R
    parsed_response.json
    response.json
    static_check.json
    runtime_report.json
    outputs/{dataset}.csv
  a02/
    code.R
    repair_prompt.md
    parsed_response.json
    response.json
    static_check.json
    runtime_report.json
    outputs/{dataset}.csv
```

The final reviewable code can still be copied or referenced from the existing
generated-code artifact path for compatibility.

## UI Changes

Minimal UI changes for V1:

1. Rename `Generate R Code` to `Build Code Package`.
2. Show package status:
   - static check passed/failed,
   - trial run passed/failed,
   - repair attempts used,
   - ready for review or blocked.
3. Keep `Approve Code` and `Reject Code`.
4. Keep `Run Approved Code` as the official execution action.
5. Label trial output clearly:

```text
Trial output only. It is not the official ADaM output.
```

## Tests

Add or update tests for:

1. successful code package build with mock LLM,
2. static check failure produces a repair attempt,
3. runtime failure produces a repair attempt,
4. attempts exhausted creates a terminal failure state,
5. trial output does not appear as official output,
6. approved code execution still writes official output,
7. input upload invalidates prior code package state,
8. UI progress returns `Build Code Package` and then code-review next action.

Suggested test files:

```text
tests/test_code_agent_v1.py
tests/test_graph_gateway.py
tests/test_api_phase8.py
```

## Acceptance Criteria

Code Agent V1 is complete when:

1. A dataset with approved spec can build a code package.
2. The package includes code, static check, trial runtime evidence, and attempt
   history.
3. A repairable R failure triggers a bounded repair loop.
4. An unrepairable spec/input/environment failure stops with a clear user
   action.
5. Human review still gates official execution.
6. Trial output is never treated as official output.
7. `graph_state.json` records all code-agent evidence.
8. The browser can show the next action without interpreting raw JSON.

## Deferred

Do not include these in V1:

- multiple model debate,
- separate reviewer agent,
- full CDISC rule engine,
- automatic clinical derivation correction,
- unlimited repair attempts,
- parallel code candidates,
- production container sandbox hardening.
