# LangGraph-2 Construction Plan

This document is the second-stage construction plan for ADaM Agent Studio after
the Phase 0-8 MVP. It turns the known deviations and unfinished work into a
system-level engineering plan.

The key rule for LangGraph-2:

> Do not add another patch layer beside the current FastAPI service flow. Move
> the real workflow into one graph-controlled state machine, and make FastAPI/UI
> a gateway over that graph.

## 1. Current Baseline

Current useful assets:

- `src/adam_agent/api/service.py` contains the most complete real workflow:
  dependency plan, finalize inputs, draft spec, draft spec review, code
  generation, code review, approved R execution, validation, compare, download.
- `src/adam_agent/graph/study_graph.py` contains study-level dependency
  planning, dataset batching, dispatch, reduction, and study audit manifest.
- `src/adam_agent/graph/dataset_graph.py` contains the dataset graph skeleton,
  LLM downstream execution modes, failure routing, and summary reduction.
- `src/adam_agent/graph/workflow_state.py` persists `workflow_state.json` and a
  SQLite sidecar checkpoint history for the current UI/API flow.
- `src/adam_agent/llm/`, `src/adam_agent/downstream/`, and
  `src/adam_agent/tools/` already provide useful tool boundaries.
- ADSL has been corrected to follow the same ADaM product flow as other AD
  targets. The old `src/adam_agent/adsl/` package is legacy/regression only.

Current architectural deviations:

- The real human-in-the-loop workflow is mostly in FastAPI service functions,
  not in LangGraph `interrupt` nodes.
- `workflow_state.json` is the current product state source; LangGraph
  checkpointer is not the single source of truth.
- `DatasetGraph` still contains early `*_stub` nodes. The graph is not yet the
  full product workflow.
- The current product is a controlled pipeline with LLM calls, not yet a true
  multi-agent graph with explicit specialist roles.
- UI dataset cards and target switching still behave more like a single-target
  controller than a graph view over multiple persistent dataset runs.
- Static ADaM/CDISC checks are placeholders, not a rules engine.
- R execution is local `Rscript` with application-level path discipline, not a
  hardened sandbox.

Unfinished product areas:

- Graph-native checkpoint/resume after process restart.
- Graph-native interrupts for dependency review, draft spec review, code
  review, and terminal failure triage.
- Multi-dataset orchestration visible in the UI as persistent per-dataset cards.
- Agent role separation for evidence, spec, code, validation, repair, reference,
  and audit.
- CDISC/P21/company-standard retrieval tools.
- Production-grade validation, security, and deployment controls.

## 2. Target Architecture

LangGraph-2 should converge on this shape:

```text
FastAPI/UI
  -> GraphGateway
    -> StudyGraph
      -> scan_inputs
      -> plan_dependencies
      -> interrupt dependency_review when needed
      -> dispatch DatasetGraph per runnable target
      -> reduce results
      -> write study audit

DatasetGraph(target)
  -> prepare_context
  -> if input_spec exists: use_input_spec
     else: draft_spec_agent -> interrupt draft_spec_review
  -> code_agent
  -> static_rule_check
  -> interrupt code_review
  -> r_sandbox_runner
  -> validate_output
  -> compare_reference_when_available
  -> diagnose_failure
  -> route repair_code / revise_spec / terminal_failure
  -> dataset_audit
```

FastAPI should not own workflow business state. It should:

- create graph runs
- send user commands into graph interrupts
- read graph state and artifacts for UI rendering
- expose artifact preview/download/compare endpoints
- keep legacy endpoints only as compatibility shims during migration

The graph state should be the durable source of truth. `workflow_state.json` may
remain as a read-friendly projection for the UI, but it must be generated from
graph state, not maintained as an independent state machine.

## 3. Core Design Principles

1. Preserve working tool boundaries.

   Reuse existing scanners, LLM clients, prompt compaction, generated-code
   parser, downstream runner, R runner, validation, diagnostics, and artifact
   writing. Do not rewrite them just to change orchestration.

2. Move orchestration, not low-level tools, into LangGraph.

   Service functions in `api/service.py` should be decomposed into graph nodes
   or node-callable tools. FastAPI should become thin.

3. Treat each ADaM target uniformly.

   ADSL, ADAE, ADCM, ADLB, and future ADxx targets must use the same graph
   contract: spec -> code -> review -> execute -> validate. Dataset-specific
   logic belongs in specs, standards tools, or generated code, not in graph
   branches.

4. Human review is an explicit interrupt, not a UI convention.

   The graph must stop at review points and resume only with a recorded human
   decision payload.

5. Multi-agent means specialist graph nodes with auditable state.

   Do not create free-form agents that talk to each other without artifacts.
   Each agent node must have inputs, outputs, risk flags, and artifact records.

6. Reference ADaM remains evidence, not authority.

   Reference ADaM can support output-shape comparison and dependency
   availability. It must not silently define derivation logic.

7. Production claims are forbidden until validation exists.

   CDISC/P21 checks, hardened sandboxing, and submission-grade ADaM correctness
   are Phase 9+ hardening goals, not implied by LangGraph-2.

## 4. Phase LG2.0 - Architecture Lock And State Contract

Goal:

Define the graph-native state contract before moving code.

Tasks:

- Create canonical `StudyRunState` and `DatasetRunState` schemas or TypedDicts
  that cover the current `workflow_state.json` fields:
  - study/run ids
  - target datasets
  - dependency plan and decisions
  - current interrupt
  - input fingerprint
  - per-dataset spec/code/review/execution status
  - artifact references
  - failure records
  - compare and validation summaries
- Define the allowed interrupt names:
  - `dependency_review`
  - `draft_spec_review`
  - `code_review`
  - `terminal_failure`
  - `dependency_user_action_required`
- Define user command payloads:
  - approve/reject dependency plan
  - approve/reject draft spec
  - approve/reject generated code
  - retry/revise after terminal failure
  - approve system generation of missing dependency dataset
- Define state projection rules:
  - graph state -> `workflow_state.json`
  - graph state -> UI review summary
  - graph state -> audit manifest
- Reserve fields for later multi-agent work so the state contract does not need
  another disruptive migration:
  - `agent_decisions`
  - `risk_flags`
  - `evidence_bundle_id`
  - `reference_queries`

Files likely involved:

- `src/adam_agent/graph/state.py`
- `src/adam_agent/schemas/states.py`
- `src/adam_agent/schemas/approval.py`
- `src/adam_agent/graph/workflow_state.py`
- `docs/state_model.md`
- `tests/test_state_schemas.py`

Exit criteria:

- One test can serialize/deserialize graph state with two datasets and two
  different interrupts.
- `workflow_state.json` is documented as a projection, not a separate product
  authority.
- State projection tests verify that graph checkpoint state and
  `workflow_state.json` agree on dataset status, current interrupt, and artifact
  references.

Current implementation status:

- Done in `LangGraph-v2`:
  - Added canonical `StudyRunState`, `DatasetRunState`, `InterruptState`, and
    `HumanCommand` schemas.
  - Added graph-to-workflow projection and consistency checks.
  - Added fields for risk flags, agent decisions, evidence bundles, and
    reference queries.
- Still open:
  - Convert every remaining service-owned status transition into graph-owned
    state updates.

## 5. Phase LG2.1 - GraphGateway And Native Checkpointing

Goal:

Introduce one gateway that owns graph invocation, resume, and state reads.

Tasks:

- Add a `GraphGateway` module responsible for:
  - compiling StudyGraph with SQLite checkpointer for local use
  - starting a run
  - resuming an interrupt with a user command
  - reading current graph state
  - writing `workflow_state.json` projection for UI compatibility
- Add graph-native internal entry points first. Then migrate existing FastAPI
  endpoints one at a time to call the gateway. Do not flip every endpoint in one
  change.
- Stop adding new independent state transitions directly in `api/service.py`.
- Use the existing SQLite sidecar only as a compatibility/debug projection until
  LangGraph checkpointer replaces it.
- Add restart tests:
  - start run -> hit draft spec interrupt
  - recreate app/gateway
  - resume with approval
  - continue to code generation

Files likely involved:

- `src/adam_agent/graph/gateway.py` new
- `src/adam_agent/graph/study_graph.py`
- `src/adam_agent/graph/workflow_state.py`
- `src/adam_agent/api/service.py`
- `src/adam_agent/api/app.py`
- `tests/test_graph_gateway.py` new
- `tests/test_api_phase8.py`

Exit criteria:

- A user review decision is persisted through LangGraph checkpoint state.
- Process restart does not lose the current interrupt.
- FastAPI no longer needs to manually decide the next workflow node for new
  graph-native paths.
- Existing UI flow still works while endpoints are migrated incrementally.

Current implementation status:

- Done in `LangGraph-v2`:
  - Added `GraphGateway` for graph-native dependency planning.
  - `/runs/prepare` now starts through the gateway and writes
    `graph_state.json`, `graph_checkpoints.sqlite`, and a workflow projection.
  - Added graph state read and dependency review endpoints.
  - Dependency review decisions are recorded in canonical graph state and then
    projected for the UI.
- Still open:
  - Draft spec review, code review, R execution, repair, validation, and compare
    endpoints still need to migrate from service-owned transitions to graph
    interrupts.

## 6. Phase LG2.2 - Replace DatasetGraph Stub Path With Product Nodes

Goal:

Make `DatasetGraph` the real per-dataset product workflow.

Tasks:

- Replace product use of these skeleton nodes:
  - `draft_lineage_stub`
  - `draft_spec_stub`
  - `generate_code_stub`
  - `run_sandbox_stub`
  - `repair_code_stub`
  - `revise_spec_stub`
- Add real nodes that wrap existing service/tool behavior:
  - `prepare_dataset_context`
  - `select_or_request_spec`
  - `draft_spec_agent`
  - `run_spec_static_check`
  - `wait_for_draft_spec_review`
  - `generate_r_code_agent`
  - `run_static_rule_check`
  - `wait_for_code_review`
  - `execute_r_sandbox`
  - `validate_dataset_output`
  - `compare_reference_output`
  - `diagnose_dataset_failure`
  - `route_after_diagnosis`
  - `write_dataset_audit`
- Keep old stub behavior only under an explicit test/demo mode, not the default
  product path.
- Before any graph-native R execution path becomes product-default, run at least
  the existing minimum forbidden-call checks against generated R code.
- Reuse these existing functions by extraction or wrappers:
  - `build_target_llm_context`
  - `generate_draft_spec_from_evidence`
  - `parse_generated_code_response`
  - `write_generated_code_artifacts`
  - `LocalRRunner`
  - `diagnose_downstream_failure`
  - static-check placeholder
  - compare/download helpers

Files likely involved:

- `src/adam_agent/graph/dataset_graph.py`
- `src/adam_agent/graph/routing.py`
- `src/adam_agent/api/service.py`
- `src/adam_agent/downstream/runner.py`
- `src/adam_agent/llm/draft_spec.py`
- `src/adam_agent/llm/context.py`
- `tests/test_graph_smoke.py`
- `tests/test_api_phase8.py`

Exit criteria:

- A graph invocation for ADSL and ADAE can stop at code review and resume to R
  execution.
- No product test depends on `*_stub` nodes.
- `real_adsl_minimal` remains retired.
- Generated R containing obvious forbidden calls is blocked before R execution.

Current implementation status:

- Done in `LangGraph-v2`:
  - Added explicit `graph_product_prepare` DatasetGraph mode.
  - `graph_product_prepare` builds the target LLM context, uses user
    `input_spec` when present, or generates a review-required draft spec when
    no input spec exists.
  - Draft spec artifacts are bound to the current input fingerprint and include
    the reference ADaM policy.
  - Added explicit `graph_product_generate_code` DatasetGraph mode.
  - `graph_product_generate_code` accepts either user `input_spec` or a same-run
    approved draft spec with matching input fingerprint.
  - Stale or fingerprint-missing approved draft specs fail closed before code
    generation.
  - Generated code, raw LLM response, parsed response, compact prompt, and a
    static-check placeholder are written as audit artifacts.
  - The graph stops at `code_review`; it does not execute R in this mode.
  - The old stub chain remains available only through explicit legacy/test
    modes, while graph-product modes skip stub code generation and sandbox
    execution.
  - FastAPI `/datasets/{dataset}/finalize-inputs` now delegates to
    `graph_product_prepare`.
  - FastAPI `/datasets/{dataset}/generate-code` now delegates to
    `graph_product_generate_code`.
  - Service-level wrappers still map graph state back to the existing UI
    response models and `workflow_state.json` projection for compatibility.
- Still open:
  - Graph-native resume from `code_review` into R execution.
  - Graph-native validation, compare, terminal failure routing, and repair.
  - Removal or deeper isolation of the old stub nodes after all product tests
    use graph-product modes.

## 7. Phase LG2.3 - StudyGraph Multi-Dataset Product Orchestration

Goal:

Make multi-target ADaM generation a first-class graph behavior, not a UI
selection loop.

Tasks:

- Keep `plan_dataset_dependencies()` as the deterministic planner, but make its
  result part of `StudyRunState`.
- Bind dependency plans and user dependency decisions to:
  - the current input fingerprint
  - a dependency decision version
  - the target dataset set
- Use dependency batches to dispatch independent DatasetGraphs.
- For each target dataset, maintain an independent dataset state:
  - current interrupt
  - approvals
  - generated code
  - validation status
  - failure diagnosis
  - output artifact
- When a dependency is missing:
  - detect if user supplied it as reference/output
  - if not supplied, interrupt with `dependency_user_action_required`
  - allow user to choose whether system should generate the dependency dataset
  - do not silently auto-run upstream ADaM.
- Preserve results when UI switches active target.
- Aggregate study status from dataset statuses:
  - completed
  - waiting_for_user
  - running
  - terminal_failure
  - partially_completed

Files likely involved:

- `src/adam_agent/graph/study_graph.py`
- `src/adam_agent/graph/dependencies.py`
- `src/adam_agent/graph/dependency_resolution.py`
- `src/adam_agent/api/web.py`
- `src/adam_agent/api/models.py`
- `tests/test_graph_smoke.py`
- `tests/test_api_phase8.py`

Exit criteria:

- A run requesting `ADAE, ADCM` keeps both dataset cards and states.
- If both depend on `ADSL` and user chooses system-generated ADSL, ADSL runs
  once and both downstream datasets consume its output.
- If user declines system-generated dependency, downstream datasets remain
  blocked with an explicit action message.
- Uploading new files invalidates stale dependency decisions when the input
  fingerprint changes.

## 8. Phase LG2.4 - Multi-Agent Node Model

Goal:

Introduce multi-agent architecture without uncontrolled autonomy.

Agent roles:

- Evidence Agent:
  - scans SDTM, define, legacy SAS, input spec, reference ADaM
  - produces an evidence bundle and risk flags
- Dependency Agent:
  - reviews deterministic dependency plan and flags uncertain dependencies
  - does not invent dependencies without evidence
- Spec Agent:
  - drafts a spec only when no user input spec exists
  - records assumptions and risk points
- Code Agent:
  - generates R from approved spec and runtime contract
  - outputs strict JSON with R code and expected outputs
- Static Review Agent:
  - runs deterministic checks first
  - checks draft spec structure before code generation when no input spec exists
  - may ask the reference/rules tool for specific rule context
- Execution Agent:
  - runs approved code in the configured sandbox
  - never edits code
- Diagnosis/Repair Agent:
  - classifies failures
  - routes to repair code, revise spec, request input, or terminal failure
- Audit Agent:
  - writes run-level and dataset-level audit summaries

Important boundary:

An "agent" in this project means a LangGraph node with a clear tool contract,
not a free-form process that can mutate arbitrary files.

Files likely involved:

- `src/adam_agent/agents/` new package
- `src/adam_agent/graph/dataset_graph.py`
- `src/adam_agent/graph/study_graph.py`
- `src/adam_agent/llm/`
- `src/adam_agent/tools/`
- `tests/test_agents_contract.py` new

Exit criteria:

- Each agent node has typed input/output.
- Each agent writes artifacts and risk flags.
- A dataset run audit can show which agent made which decision.

## 9. Phase LG2.5 - Reference And Static Rule Layer

Goal:

Add standards-aware checks without pretending to be production complete.

Tasks:

- Add reference tool interfaces:
  - `search_cdisc_reference`
  - `lookup_adam_rule`
  - `lookup_p21_rule`
  - `lookup_company_standard`
- Start with small local fixtures or indexed markdown/PDF snippets.
- Add deterministic static checks before human code review:
  - required output file path
  - expected dataset name
  - key variable presence such as `USUBJID` when applicable
  - no dangerous R calls
  - no network/system command calls
  - spec variable vs generated code output mismatch where cheaply detectable
- Keep all checks labeled by confidence:
  - blocking error
  - warning
  - informational
- Do not claim full CDISC compliance.

Files likely involved:

- `src/adam_agent/tools/reference_store.py` new
- `src/adam_agent/tools/static_rules.py` new
- `references/CDISC/`
- `references/P21_Rules/`
- `references/company_standards/`
- `src/adam_agent/api/service.py`
- `tests/test_static_rules.py` new

Exit criteria:

- Generated code with `system()` or `download.file()` is blocked before review.
- Missing expected output path is blocked.
- A warning explicitly says full CDISC/P21 compliance is not proven.

## 10. Phase LG2.6 - R Sandbox Hardening Boundary

Goal:

Turn the current R execution boundary into a replaceable sandbox interface.

Tasks:

- Define a `SandboxRunner` protocol:
  - local Rscript runner
  - future Docker runner
  - future Windows-isolated runner
- Keep local `Rscript` as the default developer runner.
- Enforce:
  - working directory = run directory
  - timeout
  - output whitelist
  - environment variable control
  - network disabled when using a real sandbox backend
- Add preflight checks:
  - script path under run dir
  - output path under run dir
  - no obvious forbidden calls
- Document that local Rscript is not production isolation.

Files likely involved:

- `src/adam_agent/tools/r_runner.py`
- `src/adam_agent/tools/sandbox.py` new
- `src/adam_agent/downstream/runner.py`
- `src/adam_agent/api/service.py`
- `tests/test_r_sandbox.py`

Exit criteria:

- The graph calls a sandbox interface, not a hard-coded local runner.
- Local runner remains available for development.
- Production docs clearly mark local runner as non-hardened.

## 11. Phase LG2.7 - UI As Graph State Viewer

Goal:

Make the UI understandable because it mirrors graph state, not because it
reimplements workflow logic.

Tasks:

- Replace single active-target mental model with:
  - study-level progress
  - dependency batches
  - persistent dataset cards
  - current interrupt/action per dataset
  - artifacts per dataset
- Hide technical paths by default.
- Show path/config details only under Advanced.
- Make dependency map text-based first:
  - "ADAE needs ADSL"
  - "ADSL available from user reference"
  - "ADLB waiting for user decision"
- Any reference ADaM panel must state that reference ADaM is used for
  comparison/output-shape/dependency availability evidence only, not derivation
  authority.
- Preserve generated/review/execution state when switching targets.
- Show why a button is disabled.
- Use graph state projection endpoint as the UI source.

Files likely involved:

- `src/adam_agent/api/web.py`
- `src/adam_agent/api/models.py`
- `src/adam_agent/api/app.py`
- `src/adam_agent/api/service.py`
- `tests/test_api_phase8.py`

Exit criteria:

- User can request multiple outputs and see each dataset's independent state.
- Switching target does not hide prior progress.
- UI shows the next required human action from graph state.
- UI clearly distinguishes input spec, approved draft spec, reference ADaM, and
  generated output roles.

## 12. Phase LG2.8 - Compatibility And Deprecation

Goal:

Avoid breaking the current MVP while migrating to graph-native control.

Tasks:

- Keep current endpoints initially:
  - `/runs/prepare`
  - `/finalize-inputs`
  - `/draft-spec-review`
  - `/generate-code`
  - `/code-review`
  - `/execute-approved-code`
- Rewire them one by one to GraphGateway.
- Mark old direct service transitions as compatibility shims.
- Add tests that ensure service shims and graph-native endpoints produce the
  same state projection.
- Keep `src/adam_agent/adsl/` legacy code out of product graph.
- Keep old `DatasetGraph` stub tests while they are useful for regression, but
  mark them as explicit demo/test mode so product tests do not depend on them.

Exit criteria:

- No user-facing workflow regresses during migration.
- New graph-native flow and old endpoint sequence converge to the same audit
  artifacts and UI state.

## 13. Recommended Execution Order

Do not start with UI polish or CDISC rules. The order should be:

1. LG2.0 State contract.
2. LG2.1 GraphGateway/checkpointer/resume.
3. LG2.2 DatasetGraph real product nodes.
4. LG2.3 StudyGraph multi-dataset orchestration.
5. LG2.7 UI graph-state viewer.
6. LG2.4 Multi-agent role packaging.
7. LG2.5 Reference/static rule layer.
8. LG2.6 Sandbox hardening boundary.
9. LG2.8 Compatibility cleanup throughout the process.

Reason:

The main architectural deviation is state ownership. If UI, service, and graph
continue to own different slices of workflow state, every later feature becomes
another patch. The graph state contract and gateway must be first.

## 14. Non-Goals For LangGraph-2

- Do not build full production CDISC/P21 compliance.
- Do not execute legacy SAS programs.
- Do not infer derivation rules from reference ADaM alone.
- Do not remove existing working APIs before graph-native replacements pass
  tests.
- Do not make ADSL special again.
- Do not introduce a broad autonomous agent that can write or run arbitrary
  code without typed state, artifacts, and review gates.

## 15. Main Risks And Controls

Risk: Graph migration breaks current UI flow.

Control: Keep compatibility endpoints and add equivalence tests.

Risk: LangGraph interrupt state diverges from `workflow_state.json`.

Control: Make `workflow_state.json` a projection generated from graph state.

Risk: Multi-agent design becomes vague.

Control: Every agent must be a node with typed input/output and artifact writes.

Risk: Standards layer overclaims compliance.

Control: Label checks as blocking/warning/info and keep full compliance out of
scope until Phase 9 hardening.

Risk: R sandbox appears safer than it is.

Control: Keep local Rscript labeled as developer mode and add a replaceable
SandboxRunner interface before claiming isolation.

Risk: Terminal failure leaves the user without a controlled next action.

Control: Define terminal failure resume commands before implementation:
`retry_execution`, `repair_code`, `revise_spec`, `request_new_input`,
`skip_dataset`, and `continue_other_datasets`.

## 16. First Concrete Implementation Ticket

Start with LG2.0 + LG2.1 only:

1. Add graph-native state schemas.
2. Add `GraphGateway`.
3. Add SQLite checkpointer-backed start/resume/read operations.
4. Add projection consistency tests between graph state and `workflow_state.json`.
5. Rewire only `prepare_run_plan` and one review interrupt path as a proof.
6. Add restart/resume tests.

This creates the foundation for the rest. Starting with UI redesign, CDISC
rules, or more provider options would not fix the core architectural deviation.

## 17. Implementation Log

### 2026-05-29 - LG2.0/LG2.1 Start

Completed:

- Created the second-stage development branch `LangGraph-v2` from `LangGraph`.
- Added graph-native canonical state contracts:
  - `StudyRunState`
  - `DatasetRunState`
  - `InterruptState`
  - `HumanCommand`
- Reserved multi-agent state fields:
  - `agent_decisions`
  - `risk_flags`
  - `evidence_bundle_id`
  - `reference_queries`
- Added `graph_gateway_mode=plan_only` to `StudyGraph`, allowing the graph to
  stop after dependency planning instead of running dataset work.
- Added `GraphGateway`:
  - starts graph-native dependency planning
  - converts StudyGraph output into canonical `StudyRunState`
  - writes `runs/{run_id}/graph_state.json`
  - writes `runs/{run_id}/graph_checkpoints.sqlite` as the current local durable ledger
  - writes `workflow_state.json` as a UI projection
  - provides `load_graph_state()` for restart reads
- Rewired `/runs/prepare` to use `GraphGateway`.
- Added graph-native dependency review entry point:
  - `POST /runs/{run_id}/dependency-review`
  - currently records approve/reject dependency review commands into canonical
    graph state.
- Added read-only API:
  - `GET /runs/{run_id}/graph-state`
- Added projection consistency checks between canonical graph state and
  `workflow_state.json`.

Current boundary:

- Only dependency planning is migrated.
- Dependency review has a minimal graph command/resume record, but does not yet
  dispatch dataset product nodes.
- Draft spec, code review, and R execution still run mostly through the existing
  FastAPI service compatibility path.
- Current persistence uses local `graph_state.json` plus
  `graph_checkpoints.sqlite`; a real LangGraph SQLite/Postgres checkpointer can
  replace this later.
- Product DatasetGraph node replacement remains LG2.2 work.

Verified with:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_state_schemas -v
```

Result: 51 tests passed.

### 2026-05-29 - LG2.2 Minimal Slice

Completed:

- Added explicit `DatasetGraph` execution mode `graph_product_prepare`.
- This mode only covers the first product-path gate:
  - build the target LLM context
  - write `runs/{run_id}/llm/{dataset}_context.json`
  - check whether a target input spec exists
  - stop at `code_generation_ready` when input spec exists
  - when input spec is missing, call the draft spec agent, write
    `runs/{run_id}/specs/{dataset}_draft_spec.json`, then stop at
    `draft_spec_review`
- The new mode does not enter the old `generate_code_stub` or
  `run_sandbox_stub` fake execution chain.
- Added tests for:
  - input spec present without stub code generation
  - missing input spec generating a fingerprint-bound draft spec and routing to
    draft spec review

Current boundary:

- This is not the full DatasetGraph product replacement.
- Draft spec LLM generation is now in the minimal `graph_product_prepare` path.
- Code generation, code review, and R execution are not yet graph-native
  DatasetGraph nodes.
- Current UI/API still mostly uses the compatibility service path.

Verified with:

```text
python -B -m unittest tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_context tests.test_downstream_runner -v
```

Result: 113 tests passed.

### 2026-05-29 - LG2.2 Review Fixes

Completed:

- Addressed subagent review findings from the first LG2.2 product-node slice.
- Added a dependency gate before `finalize-inputs` and `generate-code`:
  - blocked or unrunnable targets now fail closed instead of bypassing dependency review
  - review-required dependency evidence from legacy SAS/define/spec conflicts must be resolved before product steps continue
  - no-dependency-evidence remains allowed so missing-spec targets can still enter draft-spec generation
- Tightened the Reference ADaM boundary:
  - Reference ADaM can still appear as comparison/output-shape evidence
  - Reference ADaM no longer satisfies runtime dependency availability
  - LLM context no longer exposes `reference_adam/*` artifacts as `resolved_dependencies`
  - mock ADAE code no longer reads `reference_adam/adsl.csv`
  - generated/run output ADaM remains the valid runtime dependency source
- Bound approved draft specs to both:
  - current study input fingerprint
  - approved spec artifact sha256 recorded at review time
- Added regression tests for:
  - dependency-gate bypass prevention
  - dependency warning bypass prevention
  - approved draft spec tampering after approval
  - Reference ADaM not entering runtime LLM context
  - run-output ADaM still entering runtime LLM context
  - run-output ADaM taking precedence over same-named Reference ADaM

Current boundary:

- `workflow_state.json` is still updated directly by compatibility service wrappers after dataset actions.
- `code-review` and `execute-approved-code` are still not fully graph-native interrupts.
- Product modes still pass through some legacy stub node names before summary, although the stub nodes no-op for product modes.

Verified with:

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Follow-up subagent review found two additional issues, now fixed:

- dependency-plan warnings now block `finalize-inputs` / `generate-code` until reviewed
- dependency artifact lookup now prefers same-run generated outputs over same-named Reference ADaM

Result: 126 tests passed.

### 2026-05-29 - LG2.2 Draft Spec Gateway Slice

Completed:

- Moved generated draft spec state into `GraphGateway` canonical graph state:
  - `record_draft_spec_generation()` records the generated draft spec path/hash,
    LLM prompt/response artifacts, variables, warnings, and current input
    fingerprint.
  - The dataset enters a graph-owned `draft_spec_review` interrupt instead of
    relying only on service-local `workflow_state.json`.
- Moved draft spec review decisions into `GraphGateway`:
  - `record_draft_spec_review()` records approve/reject commands into both the
    dataset and study human-command history.
  - The approved spec path/hash and review artifact are attached to
    `DatasetRunState.spec_state`.
- Added fail-closed graph validation before trusting a draft-spec review:
  - review is rejected if no graph draft state exists
  - review is rejected if the draft spec path differs from graph state
  - review is rejected if the draft spec hash changed after generation
  - review is rejected if the draft spec artifact itself has no input
    fingerprint or the fingerprint is stale
  - approve decisions require an approved spec artifact and hash
  - approved spec hash must match the review payload
- Tightened approved draft spec consumption:
  - DatasetGraph code generation now refuses filesystem-only approved draft spec
    files unless canonical graph state also has `spec_state.status == approved`
  - `GraphGateway.record_code_generation()` rechecks the graph-approved draft
    spec path/hash/fingerprint before recording generated code
  - approving one dataset no longer clears another dataset's open run-level
    interrupt
- Updated the FastAPI compatibility service so `finalize-inputs`,
  `draft-spec`, and `draft-spec-review` write through `GraphGateway`.
- Added regression coverage for:
  - graph-state showing `draft_spec_review` after draft generation
  - graph-state showing approved draft spec review after approval
  - graph gateway rejecting draft spec tampering before approval

Current boundary:

- `workflow_state.json` remains a UI projection for compatibility, but this
  slice makes draft spec generation/review and approved-draft consumption
  graph-owned.
- Code review and execution are partly graph-owned already, but terminal
  validation/compare/repair are still service-driven or compatibility-layer
  paths.
- DatasetGraph still has legacy stub nodes for non-product test modes.

Verified with:

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 144 tests passed after the subagent review fixes.

### 2026-05-29 - LG2.2 Compare Gateway Slice

Completed:

- Moved reference-compare result recording into `GraphGateway` canonical graph
  state:
  - `record_compare()` writes `DatasetRunState.compare_summary`.
  - `DatasetResultSummary.compare_status` now reflects the latest graph-recorded
    compare status.
  - compare report artifacts are attached to the dataset artifact refs when a
    report file exists.
- Updated the FastAPI compatibility service so `/compare` still returns the
  same `DatasetCompareResponse`, but now also projects the compare result from
  graph state back to `workflow_state.json`.
- Kept compare algorithm scope unchanged:
  - still CSV-to-CSV structural and sampled cell comparison
  - still not a clinical conformance validator
  - still treats Reference ADaM as comparison evidence, not derivation authority
- Preserved open graph interrupts while recording compare output:
  - dataset-level interrupts remain open
  - study-level dependency review is not cleared by a compare call
- Addressed subagent review findings:
  - compare recording no longer creates a canonical graph run when
    `graph_state.json` does not exist
  - compare recording no longer refreshes the dataset product
    `input_fingerprint`; the current fingerprint is stored only inside
    `compare_summary`
  - study-level interrupts keep precedence when both study and dataset
    interrupts are open
  - missing generated/reference statuses are written back to existing graph
    state so canonical compare state does not stay stale

Current boundary:

- Compare is now graph-owned state, but the comparison calculation itself is
  still a service/tool helper.
- Static ADaM/CDISC checks and repair routing are still future LG2.x / Phase 9
  work.

Verified with:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

Result: 65 tests passed after the subagent review fixes.

Core verification:

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 151 tests passed.
