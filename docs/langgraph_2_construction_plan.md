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

- `src/adam_agent/graph/gateway.py` is now the main product transition
  boundary. FastAPI service helpers delegate dependency planning, input
  finalization, draft-spec generation/review, code generation/review, approved
  R execution, terminal-failure review, compare recording, upload invalidation,
  and legacy `/runs` compatibility writes through `GraphGateway`.
- `src/adam_agent/api/service.py` is now mostly request validation,
  config/provider resolution, response shaping, artifact preview/download, and
  read-model helpers. It should not own workflow state transitions.
- `src/adam_agent/graph/study_graph.py` contains study-level dependency
  planning, dataset batching, dispatch, reduction, and study audit manifest.
- `src/adam_agent/graph/dataset_graph.py` contains the dataset graph skeleton,
  product-mode nodes for prepare/generate/execute, LLM downstream execution
  modes, failure routing, and summary reduction.
- `src/adam_agent/graph/workflow_state.py` still persists the compatibility
  `workflow_state.json` read model and a SQLite sidecar history for the current
  UI/API flow.
- `src/adam_agent/llm/`, `src/adam_agent/downstream/`, and
  `src/adam_agent/tools/` already provide useful tool boundaries.
- ADSL has been corrected to follow the same ADaM product flow as other AD
  targets. The old `src/adam_agent/adsl/` package is legacy/regression only.

Current architectural deviations:

- `workflow_state.json` still exists as a UI/API compatibility read model, but
  product truth is canonical `graph_state.json`; compatibility projection
  writes now belong behind `GraphGateway`, not in FastAPI service helpers.
- The product flow is graph-gateway owned, but the UI still drives it as
  stepwise FastAPI calls. It is not yet a single StudyGraph run that dispatches
  all dataset subgraphs through native LangGraph interrupts and checkpointer
  resume.
- `DatasetGraph` still contains early `*_stub` nodes for explicit legacy/test
  modes. Product modes are guarded from falling back into the legacy stub
  chain, but the old nodes have not been removed.
- The current product is a controlled pipeline with LLM calls, not yet a true
  multi-agent graph with explicit specialist nodes for every role.
- UI dataset cards and target switching still behave more like a single-target
  controller than a graph view over multiple persistent dataset runs.
- Static ADaM/CDISC checks now have a limited policy-driven gate, but they are
  not a full rules engine.
- R execution is local `Rscript` with application-level path discipline, not a
  hardened sandbox.

Unfinished product areas:

- Full graph-native checkpointer resume for the whole product flow after
  process restart, beyond the current persisted `graph_state.json` recovery.
- Native LangGraph interrupt execution for dependency review, draft spec review,
  code review, and terminal failure triage, instead of API methods recording
  review commands into canonical state.
- StudyGraph-driven multi-dataset product orchestration visible in the UI as
  persistent per-dataset cards.
- Deeper agent role separation for evidence, spec, code, validation, repair,
  reference, and audit. Current agent decisions are auditable records, not yet
  a complete specialist-node graph.
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
  - The product split-flow status transitions are now `GraphGateway`-owned, but
    they are still exposed as stepwise gateway calls. A later phase still needs
    to make the whole product run a native LangGraph interrupt/checkpointer
    execution rather than a sequence of API-triggered graph-state updates.

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
  - Product split-flow endpoints now delegate state-changing work through
    `GraphGateway`, including input finalization, draft-spec generation/review,
    code generation/review, approved R execution, terminal-failure triage,
    compare recording, upload invalidation, and legacy `/runs` compatibility
    projection writes.
  - API regression tests guard against service helpers writing
    `workflow_state.json` directly or calling low-level recorders for protected
    product actions.
  - Added a checkpointer boundary that supports the default in-memory saver and
    a guarded local SQLite saver when the optional
    `langgraph-checkpoint-sqlite` package is installed.
  - The local SQLite saver writes to `runs/{run_id}/langgraph_checkpoints.sqlite`,
    deliberately separate from the existing product audit ledger
    `graph_checkpoints.sqlite`.
  - Added restart-style coverage proving a new `GraphGateway` can read a
    native dependency-review interrupt from the same LangGraph SQLite
    checkpoint when the optional package is available.
- Still open:
  - Review decisions and product actions are persisted in canonical graph state,
    and native pilot interrupts can use a local SQLite checkpointer when the
    optional dependency is installed. The full product loop is still not a
    single native LangGraph run that resumes every gate after process restart.
    Public FastAPI/UI defaults still use the in-memory checkpointer unless a
    later slice wires explicit product configuration.
  - Repair/spec-revision loops still require more native graph routing; current
    terminal-failure triage records the controlled next action but does not run
    an autonomous repair cycle.

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
  - limited static-check report
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
    limited static-check report are written as audit artifacts.
  - The graph stops at `code_review`; it does not execute R in this mode.
  - Product agent nodes now route directly to `summarize_dataset` instead of
    passing through no-op legacy `*_stub` nodes.
  - `graph_product_execute` no longer invokes R during `prepare_dataset`; R
    execution happens only in the explicit `execute_approved_code` graph node.
  - The old stub chain remains available only through explicit legacy/test
    modes, while graph-product modes skip stub code generation and sandbox
    execution.
  - FastAPI `/datasets/{dataset}/finalize-inputs` now delegates to
    `graph_product_prepare`.
  - FastAPI `/datasets/{dataset}/generate-code` now delegates to
    `graph_product_generate_code`.
  - FastAPI code-review, approved execution, terminal-failure review, compare,
    and upload invalidation paths now call high-level `GraphGateway` methods and
    then map graph-owned results back to existing response models.
  - `GraphGateway` writes the `workflow_state.json` compatibility projection;
    service-level wrappers only shape response models from gateway results.
- Still open:
  - The dataset product steps are graph-gateway owned, but not yet driven as one
    continuous native LangGraph interrupt/resume execution from code review into
    R execution and follow-up routing.
  - Automatic repair and spec-revision loops are not complete. Current
    terminal-failure handling records human triage and controlled next actions.
  - Final removal of old stub nodes once legacy/test-mode coverage is no longer
    useful.

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

Current implementation status:

- Done in `LangGraph-v2`:
  - Dependency plans are stored in canonical `StudyRunState` and are bound to
    the requested target set and current input fingerprint.
  - The gateway rolls study status and `current_interrupt` up from durable
    per-dataset state instead of letting each API path write its own version of
    study progress.
  - Multi-target planning is supported: `/runs/prepare` accepts multiple ADaM
    targets, persists all requested dataset states, and keeps previous dataset
    progress when a later plan/view action touches another target.
  - Dependency decisions fail closed for missing or uncertain upstream ADaM
    evidence. Reference ADaM is recognized as evidence for availability or
    comparison, but not as a runtime dependency artifact or derivation
    authority.
  - Upload invalidation now marks affected canonical graph runs stale and
    forces re-planning when the input fingerprint changes.
  - UI target selection is split into planning targets and one active detail
    target, so switching the active view does not erase other dataset state.
- Boundary:
  - This phase has graph-owned multi-target planning, state preservation,
    dependency gating, and status rollup.
  - Full automatic batch generation/review/execution for every checked target
    is not complete. Draft-spec review, code review, and local execution still
    operate on one active dataset at a time through graph-gateway gates.
  - The current design intentionally does not silently auto-run upstream ADaM
    dependencies; user dependency decisions remain explicit.

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
- Validation Agent:
  - records validation and generated-vs-reference comparison evidence
  - labels scope limits instead of claiming clinical correctness
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

Current implementation status:

- Done in `LangGraph-v2`:
  - Added bounded agent contracts in `src/adam_agent/agents/` for
    `AgentDecision`, `AgentNodeInput`, and `AgentNodeOutput`.
  - DatasetGraph product nodes now emit typed node IO for evidence/spec/code,
    static review, execution, validation/compare, diagnosis/repair, and audit
    handoffs.
  - GraphGateway preserves DatasetGraph node IO, adds compatible fallback node
    IO for direct recorder paths, and rolls dataset-level node IO into the
    canonical study state.
  - Dependency planning now emits study-level `dependency_agent` node IO.
  - The derived agent audit summary reports study-level and dataset-level node
    IO counts, invalid node IO counts, per-agent counts, and latest node output
    summaries.
  - Tests cover node IO contract validation, graph gateway persistence,
    StudyGraph audit rollup, malformed node IO visibility, and unchanged
    product behavior around compare, terminal failure, dependency planning,
    execution, and fallback recorder paths.
- Boundary:
  - The project now has bounded multi-agent node packaging and auditability.
  - The agents remain controlled graph/tool nodes. They are not autonomous
    free-form processes, do not bypass human gates, and do not change Reference
    ADaM authority.
  - Dedicated LLM repair, spec-revision loops, and deeper standards retrieval
    remain later hardening work.

## 9. Phase LG2.5 - Reference And Static Rule Layer

Goal:

Add standards-aware check boundaries without pretending to be production
complete. Static checks must be generic policy checks, not demo-specific ADaM
derivation rules.

Design principle:

- Static checks are a policy/rule-pack layer, not a growing list of one-off
  patches for PSY201, ADAE, ADSL, or any single demo variable.
- Static-rule design starts from first principles: define the reusable contract
  being checked before writing code. A concrete failure may motivate a rule, but
  it is not itself the rule.
- Static-rule work must start from a reusable rule shape, not from a failed
  example. The implementation question is always "what general contract is
  violated?" rather than "how do we stop this one file from failing?"
- A rule is acceptable only when its scope can be written without naming a demo
  study, a legacy program, a reference ADaM file, or a hand-picked clinical
  variable. If that cannot be done, the observation stays as a reviewer note or
  candidate rule-pack backlog item.
- Static checks should be designed from first principles as reusable contract
  checks. A demo failure is only evidence that a broader contract may be
  missing; it is not itself a production rule.
- This is a hard architecture boundary: the generic static-check engine must
  not branch on dataset names, study names, demo folders, or individual clinical
  variable anecdotes. Those observations may only become tests for generic
  contracts or source-backed rule-pack entries.
- Static checks verify declared contracts; they do not decide what clinical
  derivation contract should exist. For example, the engine can verify that
  generated code visibly writes the approved output path or references a
  caller-provided spec variable. It cannot decide that a dataset needs a
  particular ADaM variable or derivation unless that requirement is supplied by
  an approved spec or a source-backed rule pack.
- The implementation must be layered:
  - `StaticRuleEngine`: domain-neutral evaluator for artifact integrity,
    execution safety, declared contracts, and rule-pack execution.
  - `StaticRulePolicy`: run-specific configuration, such as required output
    paths, current code hash, declared target dataset, and caller-provided
    identifiers from approved specs.
  - `StaticRulePack`: optional standards/company rules with explicit
    authority_type, source, version, scope, severity, and evidence.
    Clinical/domain knowledge enters here, not inside the engine.
- A static rule may inspect generic artifacts and contracts:
  - generated-code path/hash binding
  - expected output file contract
  - declared target dataset
  - approved-spec variables, labels, and types
  - allowed/forbidden R execution primitives
  - reference rule identifiers supplied by a standards pack
- Blocking rules should come only from:
  - execution safety violations
  - artifact integrity or hash/path mismatch
  - explicit user-approved contracts
  - versioned rule-pack rules with evidence and declared severity
- A static rule must not invent clinical derivation logic from observed demo
  data. For example, it may say "a variable listed in the approved spec is not
  visibly produced by the generated code"; it must not say "this dataset must
  derive TRTEMFL this exact way" unless that rule comes from an explicit
  approved spec, company standard, or referenced CDISC/P21 rule pack.
- Dataset-specific standards should enter as versioned rule packs with
  authority_type, source, scope, severity, and evidence. The engine remains
  generic; the rule pack supplies domain knowledge.
- Demo-discovered issues may create candidate rules only after being converted
  into a source-backed rule-pack item. Until then they are implementation notes
  or tests for generic contracts, not production static rules.
- Rule-pack admission is a product/governance decision, not a quick code change:
  every clinical/static standards rule needs authority_type, source, version,
  scope, severity, and evidence before it can block a run.
- `source` must also be a real rule authority. Values such as
  `demo-observation`, `implementation-note`, `candidate-rule`, or
  `reviewer-note` cannot be disguised as `user_policy` or a company standard.
  They remain candidate backlog items or reviewer notes until promoted through
  governance.
- There is no "exception registry" inside the generic engine. If a future issue
  seems to require an exception, the engineering response must be one of:
  revise the approved spec contract, add a source-backed rule-pack item, or keep
  the issue as a non-blocking reviewer note until it has proper authority.
- Before any new blocking static rule is implemented, it must pass an
  abstraction gate: after removing the original study name, uploaded file name,
  dataset name, and individual variable anecdote, the rule must still be
  explainable as either a generic declared-contract check or a governed
  rule-pack item. If it cannot pass that gate, it remains a reviewer note or
  backlog candidate.
- When a check is heuristic or incomplete, it must be warning/informational and
  must record that it does not prove clinical correctness.
- Every static finding must carry rule-governance metadata:
  - `category`: one of `artifact_contract`, `execution_boundary`,
    `spec_contract`, or `standards_pack`
  - `source_type`: one of `system_contract`, `approved_spec`,
    `standards_pack`, or `user_policy`
  - optional `source_id`: spec artifact id, rule-pack id, or policy id
- User review note: static rules must not be implemented as a sequence of
  concrete demo fixes. If a future failure suggests a new check, first rewrite
  it as a general contract or a governed rule-pack item. If that rewrite is not
  possible, the product should surface the issue as a reviewer note, not as a
  blocking static rule.
- New blocking clinical rules are not allowed in the generic engine. They must
  be added through a versioned rule pack with explicit `authority_type`,
  source, scope, severity, and evidence, then reviewed as a rule-pack change.
  Accepted authority classes are limited to CDISC standards, P21 rules, company
  standards, or user policy. A demo observation or implementation note is not a
  rule authority.
- The first LG2.5 hardening priority is therefore rule-pack admission and
  provenance, not a broader list of clinical rules. Adding a new ADaM/CDISC
  rule before the rule-pack contract exists is treated as a design error.
- Static-check artifacts without governance metadata are not silently
  grandfathered for review/execution. They must be regenerated from the current
  code/spec context so the audit trail can show each finding's source.
- "Generic" means the engine evaluates a declared contract in the current run,
  not a remembered clinical anecdote. A future issue found in PSY201 or any
  other study must first be expressed as one of:
  - a broader artifact/execution/spec contract that applies without knowing the
    study or dataset name
  - a versioned rule-pack item with authority_type, source, scope, severity,
    and evidence
  - a non-blocking reviewer note or candidate-rule backlog item
- If an engineer cannot explain a static check without naming a demo study,
  a specific uploaded file, or a single clinical variable exception, that check
  is not allowed in the generic static-rule engine.
- Static-rule design must pass an abstraction gate before implementation:
  the reviewer should be able to delete the motivating demo name, file name,
  dataset name, and variable anecdote from the issue description and still
  state the rule as either a generic declared contract or a governed rule-pack
  item. If not, the item remains a reviewer note or backlog candidate.

Static-rule architecture adjustment:

- Treat every proposed rule as two separate objects:
  - `RuleObservation`: where the problem was noticed, such as a failed demo,
    a user review comment, or a bad generated script. This is evidence for
    investigation only and cannot block a run.
  - `RuleAuthority`: the declared contract that allows the system to enforce
    the rule, such as a system execution boundary, an approved spec, user
    policy, or an admitted rule-pack item.
- The generic engine may contain evaluator functions only. Evaluators answer
  questions such as:
  - is this artifact bound to the current code hash?
  - does the code attempt a forbidden execution primitive?
  - does the code visibly satisfy caller-declared output contracts?
  - does a source-backed rule-pack item have enough metadata to be enforceable?
- Evaluators must receive all domain terms as policy/rule-pack parameters.
  They must not carry clinical variables, dataset names, study names, legacy
  program names, or reference-output patterns in engine code.
- A future static rule definition must therefore include:
  - stable `rule_id`
  - `rule_family`: artifact contract, execution boundary, spec contract, or
    standards pack
  - authority source: system contract, approved spec, user policy, or rule pack
  - parameter payload supplied by the current run or rule pack
  - severity and confidence
  - evidence pointer
  - human-readable limitation text that states what the rule does not prove
- A concrete failure can become a blocking rule only after this conversion:
  observation -> generic rule shape -> authority binding -> deterministic
  evaluator -> audit-visible report. If any step is missing, the product should
  show the issue as a reviewer note rather than patch the engine.
- Regression tests for new static checks must prove generality. Each new
  blocking static rule needs at least one neutral, non-demo fixture and a
  source-level guard that prevents the rule from depending on the original
  demo/study/dataset/variable name.

Tasks:

- Add reference tool interfaces:
  - `search_cdisc_reference`
  - `lookup_adam_rule`
  - `lookup_p21_rule`
  - `lookup_company_standard`
- Start with small local fixtures or indexed markdown/PDF snippets.
- Add deterministic static checks before human code review:
  - contract rules: caller-provided output path, dataset name, code path/hash
    binding, and generated-code dataset contract from the LLM parser
  - execution-boundary rules: no dangerous R calls and no network/system command
    calls
  - spec/code consistency rules: caller-provided identifier visibility from
    approved spec variables, with warnings when code cannot be cheaply proven to
    produce expected variables
  - rule-pack loader contract: load only explicit standards/company rules with
    authority_type/source/version/scope/severity/evidence metadata, and keep
    missing packs as a visible limitation rather than silently replacing them
    with heuristics
  - rule-pack admission checks: reject rule-pack items without an allowed
    authority_type, source, version, scope, declared severity, and evidence
    pointer before they can affect code review or execution
  - non-authority source checks: candidate/demo/reviewer/implementation notes
    cannot enter binding rule packs as formal sources
  - future standards-pack rules: CDISC/P21/company-standard checks loaded from
    explicit references rather than hard-coded demo observations
  - later: spec variable vs generated code output mismatch where cheaply detectable
- Add a regression guard that rejects or flags new static checks if they are
  implemented as dataset/study/demo special cases instead of generic contracts
  or rule-pack rules.
- Add a source-level regression guard for the static-rule module: dataset names,
  demo study names, and demo-derived clinical variable anecdotes may appear in
  tests or rule-pack fixtures, but not as branching logic inside the generic
  engine.
- Add a rule lifecycle checklist before implementation:
  - candidate observation: problem found in demo/test/real run, cannot block
    production
  - generic contract: problem restated without dataset/study/file-specific
    assumptions
  - authority binding: source is system contract, approved spec, user policy, or
    versioned rule pack
  - implemented check: deterministic evaluator with audit metadata
  - reviewer visibility: report explains scope and what the rule does not prove
- Add a rule-abstraction gate to static-check artifacts and future reviews:
  - a concrete failure may motivate investigation, but cannot be the rule text
  - the implemented rule must be described without demo/study/file-specific
    names unless those names are part of a governed rule pack scope
  - the audit report must remind reviewers that new blocking rules need a
    generic declared contract or admitted source-backed rule-pack authority
- Add a rule-design review checklist for every future static-check PR:
  - What declared contract is being checked?
  - Where does the rule authority come from: system contract, approved spec,
    user policy, or versioned rule pack?
  - Is the rule independent of demo-study names and file-specific observations?
  - If it blocks a run, where are authority_type, source, version, scope,
    severity, and evidence recorded?
- Add an implementation acceptance checklist for every future static-rule
  change:
  - The generic engine code contains no study-name, dataset-name, demo-folder,
    legacy-program, or reference-output special branches.
  - Any dataset/domain-specific knowledge is loaded through a governed rule
    pack or comes from the approved spec for the current run.
  - The test that motivated the rule includes at least one generic/non-demo
    fixture so the rule proves a reusable contract instead of memorizing the
    current demo.
  - The report text states what the rule checks and what it does not prove.
- Keep all checks labeled by confidence:
  - blocking error
  - warning
  - informational
- Do not claim full CDISC compliance.

LG2.5 slice implemented:

- `StaticRulePolicy` now drives generated R checks. The rule engine itself does
  not hard-code ADAE, ADSL, PSY201, or `USUBJID`.
- `DatasetGraph` and the downstream runner both write
  `runs/{run_id}/static_checks/{dataset}_static_check.json` before code review
  or sandbox execution.
- Blocking checks currently cover forbidden R calls and missing caller-provided
  output paths.
- Identifier checks are warnings derived from the approved spec context. They
  are visibility checks only and must not be interpreted as proof that the
  derivation is correct.
- `LocalReferenceStore` provides a small file-backed lookup boundary for future
  CDISC/P21/company-standard references.
- The old empty static-check implementation was removed from the active service
  and graph gateway wording now records limited-scope static checks.
- After subagent review, `GraphGateway.record_code_generation`,
  `validate_code_review`, and approved-code execution now fail closed when the
  static-check artifact is missing, changed, incomplete, has blocking findings,
  or is not bound to the current generated R code path/hash.
- StudyGraph downstream audit manifests now retain the downstream runner's
  `static_check` artifact instead of dropping it during dataset-result rollup.
- Tests cover generic `CUSTOM`/`ANY` datasets to prevent demo-shaped rules from
  becoming the framework.
- Tests also cover incomplete static reports and attempts to reuse a passing
  static report from another R script.
- Static-rule findings now include category/source metadata and policy
  governance fields so anonymous demo-derived rules cannot be hidden inside the
  generic checker.
- `validate_static_rule_report_artifact` rejects findings with unknown
  category/source/severity values and requires non-system sources such as
  approved specs, standards packs, or user policy to carry a non-empty
  `source_id`.
- `StaticRulePack` admission now requires an explicit `authority_type`
  (`cdisc_standard`, `p21_rule`, `company_standard`, or `user_policy`) at the
  pack level. Rule items inherit that authority and cannot override it with a
  different class.
- `StaticRulePack` admission rejects non-authority sources such as
  `demo-observation`, `implementation-note`, `candidate-rule`, and
  `reviewer-note`, so implementation notes cannot be packaged as binding
  static rules.

LG2.5 slice verification:

- `python -B -m unittest tests.test_downstream_runner -v`
- `python -B -m unittest tests.test_graph_smoke -v`
- `python -B -m unittest tests.test_static_rules tests.test_llm_generated_code tests.test_api_phase8 tests.test_graph_gateway tests.test_agents_contract -v`
- `python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_generated_code tests.test_static_rules -v`

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

LG2.6 current slice implemented:

- Added `SandboxRunner` protocol and `LocalRscriptSandboxRunner` in
  `src/adam_agent/tools/sandbox.py`.
- Graph-owned approved-code execution now calls the sandbox interface instead
  of constructing `LocalRRunner` directly.
- The `llm_downstream_r_sandbox` path also uses `LocalRscriptSandboxRunner`
  when local R execution is requested.
- Validation reports now include sandbox boundary metadata:
  backend name, whether the backend is hardened, run directory, network
  isolation flag, and notes warning that local Rscript is developer mode only.
- Local sandbox preflight blocks script paths outside the run directory and
  mismatched working directories before Rscript is launched.
- `LocalRRunner` now resolves relative `script_path` values under
  `working_dir`, matching the sandbox contract and avoiding accidental
  resolution against the caller's shell directory.
- The local sandbox now passes only an allowlisted environment to Rscript.
  API keys, `R_PROFILE_USER`, and `R_ENVIRON_USER` are not passed by default.
- Local sandbox execution adds `--vanilla` to Rscript invocations so user-level
  R startup files and saved workspaces are not part of generated-code runs.
- Local sandbox rejects caller-supplied Rscript arguments. This prevents
  executable command-line options from bypassing the generated-script preflight.
- `LocalRRunner` still supports explicit environment/argument injection so the
  sandbox boundary can control execution without changing unrelated profiling
  or legacy helper callers.

LG2.6 current slice verification:

- `python -B -m unittest tests.test_sandbox tests.test_downstream_runner -v`
- `python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit -v`
- `python -B -m unittest tests.test_tools_phase4 tests.test_phase5_adsl_loop tests.test_sandbox -v`
- `python -B -m unittest tests.test_sandbox tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_static_rules tests.test_llm_generated_code tests.test_tools_phase4 tests.test_phase5_adsl_loop -v`

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

Current implementation status:

- Done in `LangGraph-v2`:
  - The UI now reads graph-owned progress through `GET /runs/{run_id}/progress`
    and refreshes graph read models after state-changing actions.
  - The top status area shows the active operation, loaded study, active detail
    target, graph-owned next action, and visible operation progress.
  - Dataset cards persist and show per-dataset graph state, including
    spec/code/execution/validation/compare status and agent-node trace context.
  - Planning targets are visually separated from the active detail target.
  - The dependency map now uses plain-language per-target cards that explain
    evidence, graph decision, runtime meaning, and next action.
  - Main workflow panes hide technical paths by default; path and source
    metadata are kept in Advanced/audit surfaces.
  - Reference ADaM is labeled as comparison/output-shape/dependency-availability
    evidence only, not derivation authority.
  - Button availability and disabled reasons are driven by graph progress, so
    obvious blocked states are not left to backend errors alone.
- Boundary:
  - The UI is now much closer to a graph-state viewer, but it is still a local
    browser UI over compatibility endpoints and read models.
  - It does not yet drive a single native LangGraph run with interrupt resume
    for all user actions.
  - Code generation, review, and execution are still active-dataset actions,
    while multi-target selection controls planning and dashboard context.

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

Current implementation status:

- Done in `LangGraph-v2`:
  - The split-flow product endpoints delegate state transitions to
    `GraphGateway` for dependency review, input finalization, draft-spec review,
    code generation, code review, approved-code execution, terminal-failure
    review, compare recording, upload invalidation, and read-model projection.
  - `workflow_state.json` is treated as a compatibility projection/read model;
    canonical product truth is `graph_state.json`.
  - Compatibility responses include explicit `workflow_control` metadata so old
    routes can be distinguished from graph-native product state.
  - Legacy `/runs` LLM run-to-completion is blocked and routed toward the
    split-flow review gates. Explicit `execution_mode="stub"` remains available
    only for legacy compatibility and tests.
  - Product DatasetGraph topology no longer includes legacy stub nodes; the old
    stub chain is available only through the explicit legacy stub graph.
  - ADSL remains on the unified ADaM LLM flow and is not wired back to the old
    deterministic R-template product path.
  - API/CLI paths now require explicit execution mode instead of silently
    choosing stub behavior for mock-provider requests.
- Boundary:
  - This phase has largely cleaned up compatibility ownership and made legacy
    behavior explicit.
  - Native LangGraph interrupt/checkpointer resume for the entire product flow
    is still not fully replacing the compatibility endpoint sequence.
  - `workflow_state.json` remains for UI/API compatibility, but new workflow
    logic should continue to enter through GraphGateway and canonical graph
    state.

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
- At the time of this slice, code generation, code review, and R execution were
  not yet graph-owned product entry points.
- Later LG2.2/LG2.8 slices moved finalize inputs, draft spec generation, code
  generation, code review, dependency review, approved execution, terminal
  failure review, and compare recording behind `GraphGateway` while preserving
  compatibility route paths.

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
- Later LG2.2 work removed the product-mode pass-through via no-op legacy stub
  nodes; this historical slice predates that cleanup.

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
- At the time of this slice, code review, execution, validation, compare, and
  repair routing still had service/compatibility responsibilities.
- Later LG2.2/LG2.8 slices moved code review, approved execution, terminal
  failure review, dependency review, and compare recording into `GraphGateway`.
  The current remaining boundary is that compatibility route paths still call
  graph-owned product steps one at a time.
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

### 2026-05-29 - LG2.2 Terminal Failure Review Slice

Completed:

- Added graph-owned terminal-failure triage recording:
  - `GraphGateway.record_terminal_failure_review()` records human decisions after
    an execution reaches `terminal_failure`.
  - Allowed triage actions are `retry_execution`, `repair_code`,
    `revise_spec`, `request_new_input`, `skip_dataset`, and
    `continue_other_datasets`.
  - The decision is stored in dataset and study `human_commands`, plus
    `DatasetRunState.execution_state.terminal_failure_review`.
  - The gateway records a deterministic `next_action` but does not pretend that
    repair or spec revision has already happened.
- Added a FastAPI compatibility endpoint:
  - `POST /runs/{run_id}/datasets/{dataset}/terminal-failure-review`
  - It fails closed unless the dataset is currently waiting at an open
    graph-owned `terminal_failure` interrupt.
- Preserved the current MVP boundary:
  - `repair_code`, `revise_spec`, and `request_new_input` keep a
    `terminal_failure` interrupt open with a more specific next action.
  - `retry_execution` resolves the interrupt and returns the dataset to
    `pending`; the user still has to explicitly run execution again.
  - `skip_dataset` / `continue_other_datasets` record the decision without
    silently repairing or producing output.

Current boundary:

- This slice records terminal-failure triage in canonical graph state; it does
  not yet implement graph-native repair-code or revise-spec subflows.
- UI buttons for these decisions are still future work; the backend/API
  contract is now available.

Verified with:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

Result: 66 tests passed before subagent review.

Subagent review fixes applied:

- Added a graph-owned hard gate before approved-code execution:
  - a dataset with an open `terminal_failure` interrupt cannot be retried by
    calling execution directly
  - the user must first record a terminal-failure review decision
  - `retry_execution` closes the interrupt and puts the dataset back into
    `pending`; execution still requires an explicit user call
- Prevented failed partial run outputs from satisfying downstream dependencies:
  - dependency resolution now checks `runs/{run_id}/graph_state.json` before
    trusting `runs/{run_id}/outputs/{dataset}.csv`
  - outputs whose producer dataset is `terminal_failure` or `failed`, or whose
    execution state says `partial_output_usable=false`, are marked
    `found_but_unusable`
  - run outputs without canonical graph `output_adam` artifact backing are also
    treated as unusable, even if a CSV exists on disk
  - this prevents a partial failed ADSL CSV from becoming the runtime ADSL input
    for ADAE or other downstream ADaM datasets
- Tightened terminal-failure triage state semantics:
  - `skip_dataset` now leaves the dataset `failed`
  - if no interrupt remains and any dataset is `failed`, the study-level graph
    status becomes `failed`, not `running`

Final verification after fixes:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke -v
```

Result: 118 tests passed.

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 155 tests passed.

### 2026-05-29 - LG2.2 Terminal Failure Follow-Up Gate Slice

Completed:

- Added graph-owned follow-up gates after terminal-failure triage:
  - product steps now fail closed if a dataset is still at `terminal_failure`
    without a recorded terminal-failure review decision
  - `generate-code` is allowed only after the user chose `repair_code`
  - `finalize-inputs` / `draft-spec` are allowed only after the user chose
    `revise_spec` or `request_new_input`
  - `execute-approved-code` remains allowed only after `retry_execution`
- Reused the existing product flow as the first repair/revise implementation:
  - `repair_code` means the user may regenerate R code from the current
    approved spec, then re-enter code review and execution
  - `revise_spec` means the user must rerun input/spec finalization before
    regenerating code
  - this does not invent a fake repair result or bypass review
- Added `GraphGateway.record_input_spec_ready()` so input-spec based
  finalization updates canonical graph state rather than only the legacy
  workflow projection.
- Recorded terminal-failure follow-up provenance in `spec_state` and
  `code_state` so the audit trail shows which human triage action unlocked the
  next product step.

Current boundary:

- The actual LLM code repair prompt is still the normal code-generation prompt;
  a dedicated repair prompt remains future LG2.2/LG2.3 work.
- `request_new_input` is treated like `revise_spec` once the user has supplied
  or corrected input files; upload-triggered re-plan remains a separate
  workflow concern.

Verified with:

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

Result: 70 tests passed.

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 158 tests passed before subagent review.

Post-review follow-up:

- A fresh `gpt-5.5` subagent review found no major blocker in the
  terminal-failure follow-up gate design.
- One medium issue was fixed: `generate-code`, `finalize-inputs`, and
  `draft-spec` now validate the canonical graph terminal-failure gate before
  calling `mark_workflow_inputs_current()`. A rejected product step therefore
  cannot dirty the legacy `workflow_state.json` read model before the graph gate
  rejects it.
- The terminal-failure gate helper was simplified to remove duplicate state
  checks.
- Regression coverage now asserts that a rejected `generate-code` call after
  `retry_execution` does not overwrite the workflow projection with
  `generate_code_start`.

Final verification:

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

Result: 73 tests passed.

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 161 tests passed.

### 2026-05-29 - LG2.3 Multi-Target State Rollup and UI Recovery Slice

Completed:

- Added a graph-owned study status rollup in `GraphGateway` so study-level
  `status` and `current_interrupt` are derived from durable per-dataset state
  instead of being hand-written differently by each endpoint.
- Preserved multi-target dataset progress across public `/runs/prepare` calls:
  - a later prepare call for another target no longer drops generated-code
    state for a previously touched dataset in the same run
  - existing product progress remains attached to the canonical
    `StudyRunState.datasets` map
  - target lists merge existing progress targets with the newly planned target
- Fixed interrupt priority after subagent review:
  - open study-level `dependency_review` stays ahead of old dataset interrupts
    created before the re-plan
  - dataset `terminal_failure` is prioritized over normal dataset review gates
    such as `code_review`
  - approving a study-level dependency review clears that study interrupt
  - terminal-failure triage clears stale dependency-review projection before
    re-rolling the study status
- Updated the local UI to recover per-dataset state from `/graph-state` after
  dependency planning:
  - draft spec, approved spec, generated-code metadata, code review,
    execution, and compare summaries are restored into the UI's per-dataset
    maps
  - switching targets or re-preparing a plan no longer makes previously
    generated dataset cards appear empty
  - stale generated code is shown as `stale`, not as a normal review-ready
    generated artifact
  - approving/running is disabled when only a code path is known but the code
    text is not loaded in the browser, preserving the human code-review gate
- Added regression coverage for:
  - preserving another target's generated-code state when preparing a new
    target in the same run
  - rollup preserving another dataset's interrupt while recording compare
  - terminal failure outranking ordinary code review
  - dependency review outranking old dataset interrupts after re-plan

Current boundary:

- `selectedTargets()` in the UI still returns the active target only. This
  slice makes multi-target state durable and visible, but full batch selection
  and graph-dispatched multi-dataset execution remain future LG2.3 work.
- UI recovery from graph state restores code metadata, not code text. A fresh
  browser must load the run review/code text before approving local execution.

Subagent review:

- A `gpt-5.5` subagent found two major issues and two medium UI issues.
- Fixed before commit:
  - terminal failure can no longer be hidden by a lexically earlier ordinary
    dataset interrupt
  - a new dependency-review interrupt from re-plan is no longer overwritten by
    old dataset product interrupts
  - stale code is no longer displayed as normal generated code
  - approve/run is disabled when graph recovery only has code metadata but no
    reviewable code text

Final verification:

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

Result: 77 tests passed.

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 165 tests passed.

### 2026-05-30 - LG2.3 Multi-Target Planning Selection Slice

Completed:

- Split the local UI target concept into:
  - `selectedTargetsForPlan`: the ADaM datasets sent together to
    `/runs/prepare` for dependency planning
  - `selectedTarget`: the active dataset shown in the draft-spec, code-review,
    execution, and results panes
- Changed target controls from single active buttons into planning checkboxes
  plus an explicit view button:
  - checked datasets participate in the dependency plan
  - the active dataset controls the single-dataset generation/review/run actions
  - at least one target remains selected for planning
- Updated dependency-plan copy and event text so the UI explains planned
  targets separately from the active detail target.
- Restored multi-target planning selection from `requested_datasets` in
  `/graph-state`. `target_datasets` remains the broader run inventory used for
  cards/progress, so historical dataset progress does not silently become the
  current checkbox selection.
- Updated plan/event display to use `requested_datasets` for "Planned targets",
  so the UI does not label the broader run inventory as the current planning
  selection.
- Added regression coverage that `/runs/prepare` accepts `["ADAE", "ADCM"]`
  in one request and persists both dataset states in canonical graph state.
- Added UI contract coverage that View/card interactions do not mutate planning
  selection or trigger a new dependency plan.

Current boundary:

- This is still a planning and state-preservation slice. The product does not
  yet auto-dispatch code generation or R execution for every checked target.
  Generation, draft-spec approval, code review, and local execution remain
  one active dataset at a time.
- Viewing a dataset card does not mutate planning selection and does not trigger
  a new dependency plan. Only checkbox/manual-target planning changes re-run
  `/runs/prepare`.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

Result: 78 tests passed.

After subagent review fix:

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

Result: 79 tests passed.

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 167 tests passed.

### 2026-05-30 - LG2.4 Agent Decision Contract Slice

Completed:

- Added a bounded agent contract package under `src/adam_agent/agents/`.
  An agent decision is now a typed audit record, not an unbounded autonomous
  process.
- Added explicit roles for:
  - `evidence_agent`
  - `dependency_agent`
  - `spec_agent`
  - `code_agent`
  - `static_review_agent`
  - `execution_agent`
  - `diagnosis_repair_agent`
  - `audit_agent`
- Added `agent_decisions` and `risk_flags` reducers to the in-flight
  `DatasetGraphState` and `StudyGraphState`.
- Recorded agent decisions from existing graph-owned product nodes:
  - dependency planning records a `dependency_agent` decision
  - product context preparation records an `evidence_agent` decision
  - draft spec generation records a `spec_agent` decision
  - R code generation records a `code_agent` decision
  - limited static checking records a `static_review_agent` warning
  - approved R execution records an `execution_agent` decision
- Persisted these decisions into canonical `graph_state.json` through
  `GraphGateway`, and projected them into `workflow_state.json` for the current
  UI read model.
- Kept the existing FastAPI service as a caller of graph transitions. It passes
  graph-produced decisions into `GraphGateway`; it does not become the source of
  agent truth.
- Added compatibility handling so older loose `agent_decisions` dictionaries do
  not break graph-state rollup. New decisions written by graph nodes are still
  validated against the strict `AgentDecision` schema.
- After subagent review, also wired the StudyGraph batch execution path to
  collect dataset subgraph `agent_decisions` and `risk_flags`; this prevents
  LG2.4 from working only through FastAPI split-flow endpoints.
- Marked Gateway-created compatibility decisions with
  `record_source: graph_gateway_default`, so audit readers can distinguish
  default persistence records from decisions emitted directly by DatasetGraph
  nodes.
- Added tests for the agent contract, dependency-plan agent decisions, spec
  agent decisions, code/static-review agent decisions, and execution agent
  decisions.

Current boundary:

- This slice introduces auditable agent roles and state records. It does not yet
  implement tool-calling reference agents, full static ADaM/CDISC rule checks,
  or autonomous multi-step repair planning.
- The `static_review_agent` decision is explicitly a limited-scope policy check.
  It does not claim CDISC compliance.
- ADSL remains on the unified ADaM split flow; no deterministic ADSL template
  path was reintroduced.
- Reference ADaM remains compare/output-shape evidence only and is not recorded
  as derivation authority by this slice.
- Study-level `agent_decisions` are append-only audit history in this slice.
  They are not yet a rebuilt "current-only" view after dataset rollback or
  replacement; a future audit-view slice should separate immutable history from
  current active decisions.

Verification:

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway -v
```

Result: 27 tests passed.

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 41 tests passed.

After subagent review fixes:

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_graph_smoke -v
```

Result: 79 tests passed.

```text
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 171 tests passed.

### 2026-05-30 - LG2.4 Audit Agent Summary Slice

Completed:

- Added `src/adam_agent/agents/audit.py` as the bounded audit-agent summary
  layer.
- The new audit summary is a derived read model from canonical graph state:
  `agent_decisions`, `risk_flags`, dataset status, current interrupts, and
  artifact ids.
- `GraphGateway` now writes `runs/{run_id}/audit/agent_summary.json` whenever
  it persists canonical `graph_state.json`.
- `StudyGraph` batch execution now writes the same
  `audit/agent_summary.json` artifact before writing the study-level
  `audit/manifest.json`.
- `StudyRunState` and `DatasetRunState` now carry `agent_audit_summary` so the
  UI/API projection can expose a human-readable agent summary without parsing
  raw decision lists.
- `workflow_state.json` now includes:
  - study-level `agent_audit_summary`
  - dataset-level `agent_audit_summary`
  - study-level audit `artifacts`, including `agent_summary_*`
- The audit manifest metadata now embeds `agent_audit_summary` alongside raw
  `agent_decisions` and `risk_flags`.
- After subagent review, fixed one medium traceability issue: the direct
  `StudyGraph` batch path now groups `state.audit_artifacts` by dataset when
  writing `agent_summary.json`, and regression coverage asserts that the direct
  StudyGraph summary includes a dataset artifact id.

Current boundary:

- `agent_summary.json` is not a workflow state source. Canonical truth remains
  `graph_state.json`.
- The summary is intentionally human/audit facing. It does not unlock workflow
  gates, change dependency planning, or alter dataset execution.
- The summary still reflects append-only decision history. It does not yet
  solve the future problem of separating immutable audit history from a
  current-only view after rollback/replacement.
- Static review entries are limited-scope policy checks. They do not prove full
  CDISC/P21/company-standard compliance.

Focused verification:

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_graph_smoke -v
```

Result: 80 tests passed.

Full core verification:

```text
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

Result: 172 tests passed.

### 2026-05-30 - LG2.7 Study Progress Viewer Slice

Completed:

- Added a Study Dashboard progress panel in the local UI:
  - study-level title and detail text
  - next-action pill
  - five compact stages: Inputs, Plan, Spec, Code Review, Run
- The panel is derived from existing UI projections of canonical graph state:
  - `state.graphState.status`
  - `state.graphState.current_interrupt`
  - dependency plan status
  - per-dataset spec/code/review/execution maps
- Added front-end contract tests to keep the progress panel tied to graph state
  and active dataset state rather than a separate workflow implementation.
- Kept the slice presentation-only. It does not change dependency planning,
  generation gates, code review, execution, or repair routing.

Current boundary:

- The panel is a browser-side projection from graph state plus existing
  per-dataset UI maps. It improves user orientation, but canonical workflow
  truth remains `graph_state.json`.
- This is not a full batch execution UI. Generation, review, and local execution
  remain active-dataset actions.
- Browser-plugin screenshot verification was not available in this tool
  session; the page was verified through FastAPI HTTP response plus UI contract
  tests.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8 -v
```

Result: 56 tests passed.

### 2026-05-30 - LG2.7 Disabled Action Reason Slice

Completed:

- Added visible action hints beside the main review buttons:
  - Finalize Inputs / Draft Spec
  - Approve Draft Spec
  - Generate R Code
  - Approve And Run Locally
- Centralized action availability text in `actionAvailability()`, derived from
  existing UI projections of graph/run state:
  - selected target
  - dependency plan and active blocked dependency
  - input-spec / approved-draft-spec gate
  - generated-code state
  - review/execution state
- Added button `title`, `aria-disabled-reason`, and `data-action-ready`
  attributes so the UI can explain what is missing without owning workflow
  gating.
- Kept this as UI projection only. It does not change API behavior, graph
  transitions, button gating, generation gates, approval gates, or sandbox
  execution.

Current boundary:

- The action hints are browser-side explanations. Canonical workflow truth
  remains graph state plus existing run artifacts.
- The hint renderer must not set `button.disabled`. Existing handlers and
  backend/graph checks remain responsible for actual transitions, including
  their auto-prepare behavior.
- Existing compatibility buttons are still one-dataset-at-a-time actions.
- Some older direct `button.disabled = ...` assignments remain in legacy UI
  paths, but `renderActionAvailability()` is called from dashboard and active
  target refresh paths to make the visible reason layer authoritative.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8 -v
```

Result: 57 tests passed.

### 2026-05-30 - LG2.7 Graph-State Plan Recovery Slice

Completed:

- Added `planFromGraphState()` in the local UI to rebuild the dependency-plan
  read model from canonical graph state fields:
  - requested datasets
  - target datasets
  - runnable datasets
  - blocked datasets
  - dependency review status
  - dependency decisions
  - dependency plan/resolution payloads
- `applyGraphState()` now restores `state.plan` from `graph_state.json` and
  rerenders the dependency plan panel after the active target is restored.
- Adjusted action-hint wording so a missing browser-side plan says the click
  will auto-prepare first, matching the existing handlers.

Current boundary:

- This is a UI read-model recovery slice only. Canonical truth remains
  `graph_state.json`; the browser-side `state.plan` is a projection.
- It does not change dependency planning, dependency review, generation gates,
  approval gates, DatasetGraph execution, or sandbox behavior.
- It reduces reliance on transient browser cache after refresh/resume, but it
  is not yet a complete pure graph-state UI.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8 -v
```

Result: 58 tests passed.

### 2026-05-30 - LG2.4 Validation Agent Compare Audit Slice

Completed:

- Extended the bounded agent role contract with `validation_agent`.
- `GraphGateway.record_compare()` now records a `validation_agent` decision
  whenever generated-vs-reference ADaM comparison evidence is persisted.
- The decision records:
  - compare status
  - compare report artifact id when available
  - current input fingerprint digest
  - `reference_compare_limited_scope` risk flag
- The study-level and dataset-level audit summaries now show compare evidence
  as an auditable post-processing agent decision, not as an unowned service-side
  state mutation.

Current boundary:

- `validation_agent` is a bounded graph/audit role, not an autonomous reviewer.
- Reference ADaM remains comparison/output-shape evidence only. This slice does
  not let reference ADaM define derivation logic.
- The compare algorithm remains the existing structural/sample comparison. This
  does not claim clinical derivation correctness or CDISC/P21 compliance.

Verification:

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state -v
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_api_phase8 -v
```

### 2026-05-30 - LG2.7 Agent Audit Viewer Slice

Completed:

- Added an `Agent Audit` panel to the local UI study dashboard.
- The panel reads from canonical `/graph-state` data already loaded in the
  browser:
  - active dataset `agent_decisions`
  - fallback study-level `agent_decisions`
  - graph and active-dataset `risk_flags`
- Agent records are shown as short human-readable cards instead of raw JSON.
- The panel includes the LG2.4 roles added so far, including `validation_agent`
  and `diagnosis_repair_agent`.

Current boundary:

- This is a read-only graph-state viewer. It does not create or mutate graph
  state.
- It does not add a new audit source. Canonical truth remains
  `graph_state.json`; the UI only renders the existing projection payload.
- It does not claim that agent decisions prove clinical correctness. Risk flags
  remain visibility signals for human review.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -m unittest tests.test_api_phase8 -v
```

### 2026-05-30 - LG2.7 Human Review Queue Viewer Slice

Completed:

- Added a `Human Review Queue` panel to the local UI study dashboard.
- The panel reads from the canonical `/graph-state` payload already loaded in
  the browser:
  - study-level `current_interrupt`
  - per-dataset `current_interrupt`
  - per-dataset `status` fallback for `needs_review` and `terminal_failure`
- Review gates are displayed as human-readable cards for dependency review,
  draft-spec review, code review, and terminal-failure triage.
- Added UI contract coverage that asserts the queue is sourced from graph state
  and does not expose raw JSON.

Current boundary:

- This is a read-only graph-state viewer. It does not create, close, approve,
  reject, or mutate any graph interrupt.
- It does not add a second human-review state machine. Canonical truth remains
  `graph_state.json`.
- It does not alter static-rule behavior. Static checks remain generic
  contract/rule-pack governance, not demo/study/dataset-specific patches.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_state_progress_panel -v
python -m compileall -q src\adam_agent
```

### 2026-05-30 - LG2.7 Hide Technical Paths In Main UI Slice

Completed:

- Removed direct spec/draft-spec artifact paths from the main draft-spec and
  code-review panes.
- Replaced those paths with human-facing artifact-recorded messages.
- Kept technical paths available in the existing Advanced settings and audit
  files table.
- Added a UI contract test so ordinary review panes do not reintroduce
  `input_spec_path`, `approved_spec_path`, `draft.spec_path`, or
  `generated.draft_spec_path` rendering.

Current boundary:

- This is UI presentation only. It does not change artifact storage, download
  behavior, graph state, workflow projection, or audit files.
- Technical paths are still available for audit/debug under Advanced.
- Static-rule behavior is unchanged and remains generic contract/rule-pack
  governance, not demo/study/dataset-specific patches.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
python -m compileall -q src\adam_agent
```

### 2026-05-30 - LG2.4 Diagnosis/Repair Agent Triage Audit Slice

Completed:

- `GraphGateway.record_terminal_failure_review()` now records a
  `diagnosis_repair_agent` decision when a human triages a terminal failure.
- The decision captures:
  - failure ids and prior recommended routes
  - human action such as `retry_execution`, `repair_code`, or `revise_spec`
  - the next controlled product action
  - whether the terminal-failure interrupt remains open
- Added `terminal_failure_triage_limited_scope` to the dataset and study risk
  flags so audit readers can distinguish triage recording from actual repair or
  re-execution.

Current boundary:

- This slice does not implement autonomous repair and does not execute a retry.
- Existing gates remain unchanged:
  - `repair_code` only unlocks code regeneration
  - `revise_spec` / `request_new_input` only unlock input/spec finalization
  - `retry_execution` only unlocks an explicit later execution request
- The diagnosis/repair agent record is audit metadata over the existing
  graph-owned human decision, not a second workflow state machine.

Verification:

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage -v
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_api_phase8 -v
```

### 2026-05-30 - LG2.6 Local R Environment-Control Slice

Completed:

- Added optional `environment` and `arguments` fields to `RRunRequest`, and
  passed them through `LocalRRunner` to `subprocess.run()`.
- Added local sandbox environment allowlisting. By default, generated-code R
  execution receives only a small runtime allowlist and does not receive API key
  variables, `R_PROFILE_USER`, or `R_ENVIRON_USER`.
- Preserved explicit empty allowlists as empty. This keeps "no inherited
  environment" distinct from "use the default allowlist".
- Added `--vanilla` to local sandbox Rscript invocations so generated-code runs
  do not consume user startup files or saved workspaces.
- Added boundary metadata for environment control and Rscript arguments so
  validation/audit reports can show what the local runner did.
- Re-stated the static-rule governance boundary: static checks must remain
  generic contract/rule-pack checks and must not become demo-specific patch
  rules.

Current boundary:

- This is still a local developer runner, not system-level isolation. It reduces
  accidental environment leakage and startup-file effects but does not prevent
  arbitrary filesystem or network behavior by generated R code.
- The change does not add any clinical, dataset-specific, study-specific, or
  demo-specific static ADaM rule.

Verification:

```text
python -B -m unittest tests.test_sandbox -v
python -B -m unittest tests.test_sandbox tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway -v
python -m compileall -q src\adam_agent
git diff --check -- src/adam_agent/tools/r_runner.py src/adam_agent/tools/sandbox.py tests/test_sandbox.py docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
```

Result: 12 sandbox tests passed; 207 related sandbox/downstream/graph/API
tests passed; compileall passed; diff check reported no whitespace errors.

### 2026-05-30 - LG2.6 Sandbox Forbidden-Call Preflight Slice

Completed:

- Added a second fail-closed execution-boundary guard inside
  `LocalRscriptSandboxRunner`.
- The local sandbox now scans the R code that is about to execute and blocks
  forbidden R calls such as `system()` before starting Rscript.
- Added a neutral `r_safety` helper and reused it from both static rules and
  sandbox preflight, so execution-boundary checks share one generic
  implementation instead of drifting into separate patch lists.
- The detector ignores comments and string literals, so warning text like
  `"system('not-a-call')"` does not trigger a sandbox block.

Current boundary:

- This is still not production isolation. Local Rscript remains a developer
  runner and the validation report must continue to mark it as not hardened.
- This slice adds a generic execution-boundary guard only. It does not add
  clinical, ADaM, CDISC, dataset-specific, study-specific, or demo-specific
  rules.
- Static code review remains the primary pre-review gate. Sandbox preflight is
  a last-mile execution guard in case approved code artifacts are tampered with
  or invoked outside the normal review path.

Focused verification:

```text
python -B -m unittest tests.test_r_safety tests.test_sandbox tests.test_static_rules -v
```

Result: 33 shared R-safety, sandbox, and static-rule tests passed.

### 2026-05-30 - LG2.7 Reference Evidence Dependency Map Slice

Completed:

- Tightened the local UI dependency map wording so Reference ADaM is not shown
  as a runtime dependency by itself.
- Split the browser projection into:
  - runtime availability: selected/planned/runnable in this run
  - reference evidence: uploaded Reference ADaM for compare/output-shape
    evidence only
- Dataset status now labels pure Reference ADaM presence as `reference
  evidence`, not a ready runtime input.
- Added a UI contract regression that prevents the dependency map from
  treating `hasReferenceAdamEvidence()` as runtime availability.

Current boundary:

- This is a UI graph-state viewer correction only. It does not change backend
  dependency planning, graph execution, LLM prompts, or sandbox behavior.
- Reference ADaM remains usable as comparison/output-shape/dependency evidence,
  but it is not derivation authority and does not satisfy runtime dependency
  availability by itself.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
```

Result: 2 focused UI dependency-map tests passed; full
`tests.test_api_phase8` passed 73 tests.

### 2026-05-30 - LG2.5 Static Rule Non-Authority Source Guard Slice

Completed:

- Tightened static-rule governance around rule-pack `source`, not just
  `authority_type`.
- `StaticRulePack` admission now rejects note-like sources before they can
  become binding rules:
  - `demo-observation`
  - `implementation-note`
  - `candidate-rule`
  - `reviewer-note`
- The rejection also covers underscore/space variants and suffixed forms such
  as `demo-observation-2026`, so implementation notes cannot be renamed into
  binding user policy.
- Updated `StaticRulePolicy` governance metadata so generated static-check
  reports explicitly state that note-like sources cannot be admitted as binding
  rule-pack sources.
- Updated the LG2 construction plan in English and Chinese to make this a hard
  design rule.

Current boundary:

- This slice does not add any clinical, ADaM, CDISC, P21, study-specific,
  dataset-specific, or variable-specific rule.
- Reasonable `user_policy`, `company_standard`, `p21_rule`, and
  `cdisc_standard` packs remain valid when they provide real source, version,
  scope, severity, and evidence metadata.
- Static checks remain generic contract/rule-pack governance. Demo observations
  may inform candidate backlog items, but they cannot directly block a run.

Verification:

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_static_rules tests.test_graph_gateway tests.test_api_phase8 -v
```

Result before suffix hardening: 26 static-rule tests passed; 151 related
static/gateway/API tests passed.

Subagent review:

- Read-only subagent review returned GO.
- No blocking issue was reported.
- The reviewer suggested guarding suffixed note-like sources; this was applied
  before final verification.

### 2026-05-30 - LG2.8 Legacy Workflow Write Boundary Slice

Completed:

- Concentrated the remaining service-layer `workflow_state.json` direct writes
  into two explicitly named legacy `/runs` compatibility helpers:
  - `_write_legacy_run_blocked_workflow_state`
  - `_write_legacy_run_completion_workflow_state`
- Updated `run_study_from_request()` so its main flow delegates those writes
  instead of calling `update_workflow_state()` inline.
- Added an AST regression guard that scans `api/service.py` and fails if any
  new service function writes workflow state directly outside those legacy
  helpers.
- Added a second AST guard so the legacy helpers can be called only by
  `run_study_from_request()`, preventing them from becoming reusable
  service-layer state writers.
- Kept the product endpoint boundary unchanged: product dataset actions still
  enter through `GraphGateway` product methods and must not call low-level
  recorders or workflow-state write helpers.

Current boundary:

- This slice does not remove the legacy `/runs` shim; it only makes its direct
  projection writes explicit and guarded.
- Upload invalidation remains graph-owned through `GraphGateway.mark_all_inputs_changed()`
  plus the existing legacy projection invalidation helper.
- Static-rule behavior is unchanged. Static checks remain generic
  contract/rule-pack checks, not demo/study/dataset-specific patches.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_legacy_run_endpoint_owns_only_remaining_service_workflow_writes tests.test_api_phase8.Phase8ApiTests.test_run_study_from_request_delegates_legacy_workflow_writes tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion -v
```

Result: 4 tests passed before the helper-caller guard; focused helper guards
were rerun afterward and passed.

### 2026-05-30 - LG2.2 DatasetGraph Non-Legacy Route Guard Slice

Completed:

- Added a route-level regression guard for `DatasetGraph` so non-legacy modes
  cannot fall through into the old stub chain:
  - `graph_product_prepare`
  - `graph_product_generate_code`
  - `graph_product_execute`
  - `llm_downstream_stubbed`
  - `llm_downstream_provider`
  - `llm_downstream_r_sandbox`
  - retired `real_adsl_minimal`
- Kept the existing graph-shape guard that product agent nodes route directly
  to `summarize_dataset`, not to `draft_lineage_stub`.
- Did not remove legacy stub nodes. They remain reachable only through the
  explicit `stub_chain` branch for old tests/compatibility.

Current boundary:

- This is a regression guard only. It does not change `DatasetGraph` runtime
  behavior, product steps, LLM prompts, R execution, or sandbox handling.
- Stub modes still exist, but the product and LLM downstream paths are now
  protected against accidental routing back into them.
- Static-rule behavior is unchanged and remains a generic contract/rule-pack
  layer.

Focused verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_nodes_do_not_flow_through_legacy_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_non_legacy_modes_never_route_to_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_uses_input_spec_without_stub_code tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
```

Result: 4 tests passed.

### 2026-05-30 - LG2.5 Static Rule Authority Admission Slice

Completed:

- Tightened the static-rule design so the generic engine remains a
  contract/rule-pack layer, not a list of demo or dataset-specific fixes.
- Added `authority_type` to `StaticRulePack` and `StaticRulePackItem`.
- Allowed authority classes are intentionally narrow:
  - `cdisc_standard`
  - `p21_rule`
  - `company_standard`
  - `user_policy`
- Rule-pack items inherit the pack authority. If an item declares a different
  authority type, admission fails closed.
- Static-rule policy output now records rule-pack admission and candidate-rule
  governance notes, so reports explain that demo observations remain reviewer
  notes until promoted through a governed rule-pack process.
- Updated the construction plan language to require
  authority_type/source/version/scope/severity/evidence before any future
  standards rule can affect review or execution.

Current boundary:

- This slice does not add any clinical, ADaM, CDISC, P21, study-specific, or
  dataset-specific static rule.
- Current generated-R checks remain limited to generic artifact contracts,
  execution-boundary checks, and caller-approved spec identifier visibility.
- Existing rule-pack payloads without `authority_type` are intentionally
  rejected. No current product path depends on loading legacy rule-pack payloads.

Subagent review:

- Subagent review returned GO.
- The reviewer found no blocking issue and confirmed the change keeps static
  rules generic rather than patch-like.
- Non-blocking follow-up: future audit hardening may validate that an authority
  type semantically matches its source/version, not just that required fields
  are present.

Focused verification:

```text
python -B -m unittest tests.test_static_rules -v
```

Result: 23 tests passed.

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

Result: 116 tests passed.

```text
python -B -m unittest tests.test_static_rules tests.test_state_schemas tests.test_graph_smoke -v
```

Result: 89 tests passed.

```text
python -m compileall -q src\adam_agent
```

Result: passed.

### 2026-05-30 - LG2.8 Service Preflight Ownership Slice

Completed:

- Removed direct `GraphGateway.validate_product_step_start()` calls from the
  FastAPI compatibility service wrappers for:
  - finalize inputs
  - draft spec generation
  - code generation
- Kept the fail-closed behavior inside GraphGateway product methods. The service
  layer still validates request shape and builds API responses, but it no longer
  owns terminal-failure preflight routing for those product steps.
- Added an AST-based regression test asserting these service wrappers do not
  call the gateway preflight method directly.
- Added a runtime regression test proving `/draft-spec` is blocked by
  GraphGateway terminal-failure preflight before DatasetGraph invocation.

Current boundary:

- This is a responsibility cleanup only. It does not change route paths,
  response schemas, dependency planning semantics, terminal-failure routing,
  LLM provider behavior, static checks, or R execution.
- Superseded by the next LG2.8 slice below: dependency-plan gating has now
  moved into GraphGateway product methods, leaving compatibility service
  wrappers responsible for request/config parsing and response shaping.
- This slice does not add any clinical/static ADaM rule. Static checks remain
  limited to generic contracts and source-backed rule-pack governance. Demo
  observations must not be promoted into blocking checks unless they are first
  rewritten as generic contracts or admitted through a versioned rule pack with
  authority, scope, severity, and evidence.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_api_phase8.Phase8ApiTests.test_draft_spec_uses_gateway_terminal_failure_preflight -v
```

Result: 2 tests passed.

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_generate_code_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_draft_spec_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_requires_repair_code_before_regenerating_code tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_requires_finalize_before_regenerating_code tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_with_approved_draft_spec_generates_new_draft_and_clears_old_review -v
```

Result: 7 tests passed.

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

Result: 118 tests passed.

### 2026-05-30 - LG2.8 Gateway Product Dependency Gate Ownership Slice

Completed:

- Moved dependency-plan gating into GraphGateway product methods for:
  - finalize inputs
  - explicit draft spec generation through `finalize_inputs(force_new_draft_spec=True)`
  - R code generation
  - approved R execution
- Removed direct `dependency_gate_for_product_step()` calls and dependency-artifact
  construction from the FastAPI compatibility service wrappers.
- Kept the explicit `dependency_gate_for_product_step()` method as a diagnostic
  and dependency-review API boundary, but product steps no longer depend on the
  service layer to call it first.
- Added GraphGateway-owned dependency artifact resolution for code generation,
  excluding reference ADaM from satisfying runtime dependencies.
- Removed the external `dependency_artifacts` injection surface from the
  `GraphGateway.generate_code()` product method. Runtime dependency artifacts
  are always derived from the gateway-owned dependency plan before code-review
  state is recorded.
- Removed the external `dependency_resolution` injection surface from
  `GraphGateway.finalize_inputs()`, `GraphGateway.generate_draft_spec()`, and
  `GraphGateway.generate_code()`. Those product methods now always read
  dependency resolution from the gateway-owned plan.
- Added a dependency-review handoff inside GraphGateway:
  - blocking dependency statuses still fail closed before any product graph call
  - nonblocking `no_dependency_evidence` review is preserved in audit metadata
    but no longer leaves a study-level `dependency_review` interrupt that hides
    the product-level `draft_spec_review` or `code_review` interrupt
- Tightened coverage so:
  - product service wrappers must not directly call either
    `validate_product_step_start()` or `dependency_gate_for_product_step()`
  - `GraphGateway.generate_code()` must not expose a caller-provided
    `dependency_artifacts` parameter
  - GraphGateway product spec/code methods must not expose caller-provided
    `dependency_resolution`

Current boundary:

- Public route paths and response schemas are unchanged.
- FastAPI service wrappers still resolve config/provider overrides and build
  API response models. They no longer own terminal-failure preflight or
  dependency-plan gating for product steps.
- Study-level dependency planning still belongs to `StudyGraph`; this slice
  only moves the product-step gate and handoff responsibility into
  `GraphGateway`.
- Static-rule governance is unchanged. This slice does not add clinical,
  dataset-specific, study-specific, or demo-specific static checks.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_graph_gateway.GraphGatewayTests.test_gateway_generate_code_does_not_accept_external_dependency_artifacts tests.test_graph_gateway.GraphGatewayTests.test_gateway_product_spec_methods_do_not_accept_external_dependency_resolution tests.test_api_phase8.Phase8ApiTests.test_execute_rejects_changed_runtime_dependency_artifact -v
```

Result: 6 focused tests passed.

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_starts_plan_and_blocks_unresolved_dependency tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_returns_plan_when_open tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_generate_code_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_draft_spec_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry -v
python -m compileall -q src\adam_agent
```

Result: 6 gateway tests passed; 5 API tests passed; compileall passed.

### 2026-05-30 - LG2.8 Gateway-Owned Dependency Plan Verification Slice

Completed:

- Added regression coverage for the boundary that
  `GraphGateway.generate_code()` uses the dependency resolution from the
  gateway-owned canonical dependency plan, not a caller-supplied dependency
  list.
- The test seeds both a graph-backed completed `ADSL` run output and a decoy
  `reference_adam/adsl.csv`, replans `ADAE`, then verifies:
  - the `DatasetGraph` invocation receives the `ADAE -> ADSL` dependency
    resolution from the current graph plan
  - generated-code state records the same graph-backed runtime dependency
    artifact
  - the recorded runtime dependency artifact points to `run_output`, not the
    decoy reference ADaM
- No runtime logic was changed; the existing implementation already satisfied
  this boundary.

Current boundary:

- `GraphGateway.record_code_generation()` still remains a lower-level trusted
  persistence boundary for tests and graph internals. Product callers should use
  `GraphGateway.generate_code()`, which derives dependency artifacts from the
  graph-owned dependency plan.
- This slice does not change dependency planning semantics, UI behavior, static
  rules, R execution, or provider behavior.
- Static-rule governance remains unchanged: static checks are still generic
  contract/rule-pack checks, not demo/study/dataset-specific rules.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generate_code_uses_gateway_owned_dependency_plan -v
```

Result: 1 focused test passed.

### 2026-05-30 - LG2.8 Product Service Low-Level Recorder Guard Slice

Completed:

- Strengthened the product service wrapper AST guard so the compatibility
  endpoints for dependency review, finalize inputs, explicit draft spec
  generation, draft-spec review, R code generation, code review, approved R
  execution, and terminal-failure review cannot call low-level GraphGateway
  recorder methods or direct workflow-state write helpers.
- The guarded service wrappers must continue to enter through GraphGateway
  product/review methods such as `finalize_inputs()`, `generate_draft_spec()`,
  `generate_code()`, `review_code()`, and `execute_approved_code()`.
- This is a regression guard only; no runtime logic was changed.

Current boundary:

- `GraphGateway.record_compare()` remains the explicit compare/report action
  boundary and is not part of the product split-flow wrapper guard in this
  slice.
- Lower-level recorder methods remain available for tests and graph internals,
  but compatibility product endpoints should not use them as business entry
  points.
- Static-rule governance remains unchanged. This slice does not add clinical,
  dataset-specific, study-specific, or demo-specific static checks.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods -v
```

Result: 1 focused test passed.

### 2026-05-30 - LG2.8 Graph-State Input Upload Invalidation Slice

Completed:

- Routed study input uploads into graph-owned stale marking instead of leaving
  invalidation only in the UI `workflow_state.json` read model.
- Added `GraphGateway.mark_inputs_changed()`:
  - reloads canonical `graph_state.json`
  - recomputes the study input fingerprint
  - records `dependency_plan.plan_stale`, `dependency_plan.input_diff`, and a
    stale reason
  - opens a study-level `dependency_review` interrupt when inputs changed
  - marks datasets with product progress as `needs_review`
  - marks generated code state as `stale` when code exists
  - marks spec state as `stale` when only spec/draft-spec progress exists
  - writes a fresh `workflow_state.json` projection from graph state
- The `/studies/files` response now includes `touched_graph_runs` so callers can
  see which active graph runs were invalidated, separately from legacy workflow
  read-model touches.
- Product steps now fail closed when the dependency plan is stale. A caller must
  re-run dependency planning before finalize inputs, draft spec, or code
  generation can continue.

Static-rule boundary reaffirmed:

- This slice does not add any clinical/static ADaM rule.
- Static checks remain generic contract/rule-pack checks.
- Future blocking rules must verify a declared contract and identify their
  authority source: system contract, approved spec, user policy, or a
  source-backed versioned rule pack with authority_type, source, scope,
  severity, and evidence.
- Demo failures or PSY201 observations may become tests for a generic contract
  or candidates for a governed rule-pack item; they must not be embedded as
  dataset/study/file-specific branches in the generic static-rule engine.

Current boundary:

- `workflow_state.json` remains a projection, not the source of truth.
- Legacy runs without `graph_state.json` are skipped by upload-time graph
  invalidation and remain covered only by legacy read-model invalidation.
- The user-facing UI still clears browser-side cached plan/code/review data on
  upload. The backend graph-state invalidation is the authoritative guard.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_inputs_changed_updates_canonical_state_and_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_inputs_changed_raises_for_legacy_run_without_graph_state tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_workflow_state_stale tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation -v
```

Result: 4 focused tests passed.

Related verification:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

Result: 111 gateway/API tests passed.

Subagent review:

- Read-only subagent review returned GO.
- No blocking schema, backcompat, stale-gate, or static-rule-boundary issue was
  found.
- Follow-up noted for a later slice: upload invalidation still discovers active
  runs from the legacy workflow read model before updating canonical graph
  state. A later hardening slice should scan `runs/*/graph_state.json`
  directly and treat `workflow_state.json` only as projection.

### 2026-05-30 - LG2.8 Canonical Graph-Run Upload Invalidation Hardening Slice

Completed:

- Moved graph-run discovery for upload invalidation into `GraphGateway`.
- Added `GraphGateway.list_graph_runs()` to discover runs from
  `runs/*/graph_state.json`, not from the UI `workflow_state.json` projection.
- Added `GraphGateway.mark_all_inputs_changed()` so the API upload path can
  invalidate all canonical graph runs whose stored input fingerprint changed.
- `/studies/files` now reports:
  - `touched_runs`: legacy workflow read-model invalidations
  - `touched_graph_runs`: canonical graph-state invalidations
  - `skipped_graph_runs`: graph-state files that could not be loaded or updated
- Added regression coverage for the important failure mode: if
  `workflow_state.json` is missing, upload still finds `graph_state.json`,
  marks the dependency plan stale, opens graph-level `dependency_review`, and
  regenerates the workflow projection from canonical state.
- Preserved existing stale state across repeated upload rescans without
  repeatedly reporting the run as newly touched when no new input change
  occurred.
- Added skipped-run reporting and regression coverage for corrupt
  `graph_state.json`.
- Updated the UI upload message so it reports canonical graph invalidations
  from `touched_graph_runs`, not only legacy workflow projection touches.

Static-rule boundary reaffirmed:

- This slice does not modify static-rule logic and does not add clinical,
  dataset-specific, study-specific, or demo-specific rules.
- Static checks continue to be generic contract/rule-pack checks only.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_scans_canonical_graph_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_preserves_existing_stale_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_reports_corrupt_graph_state_as_skipped tests.test_api_phase8.Phase8ApiTests.test_upload_invalidates_graph_run_even_when_workflow_projection_is_missing tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
```

Result: 5 focused tests passed.

Related verification:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
python -B -m unittest tests.test_static_rules tests.test_state_schemas tests.test_graph_smoke -v
```

Result: 115 gateway/API tests passed; 87 static/state/graph smoke tests passed.

### 2026-05-30 - LG2.8 Legacy `/runs` Shim Boundary Slice

Completed:

- Marked `POST /runs` run-to-completion responses with explicit legacy metadata:
  - `workflow_control: legacy_run_to_completion_compatibility_shim`
  - `graph_state_path: null`
  - `workflow_state_path`
- Marked the blocked LLM run-to-completion path the same way in
  `workflow_state.json`, while keeping `current_interrupt: split_flow_required`
  and the existing error that points callers to `/runs/prepare` plus the
  dataset-level review gates.
- Added API regression coverage for both paths:
  - stub `/runs` succeeds but is labeled legacy compatibility
  - LLM `/runs` is rejected and records that product split-flow is required
- Updated `docs/phase8_1_api_contract.md` so it no longer implies real LLM/R
  generation should use `POST /runs`.
- After subagent review, updated the `RunStudyRequest` schema docstring so the
  OpenAPI schema also describes `/runs` as legacy/smoke compatibility.

Current boundary:

- This slice does not remove `POST /runs` and does not change its stub/smoke
  behavior.
- It does not create canonical `graph_state.json` for the legacy
  run-to-completion path. The null `graph_state_path` is intentional so callers
  can distinguish it from GraphGateway-owned product flow.
- Real LLM/R ADaM generation remains blocked on this endpoint and must use
  `/runs/prepare` plus dataset-level finalize/draft/code-review/execute gates.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion tests.test_api_phase8.Phase8ApiTests.test_demo_study_rejects_run_to_completion_llm_endpoint -v
```

Result: 3 tests passed.

Broader verification:

```text
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_static_rules tests.test_reference_store tests.test_state_schemas -v
git diff --check -- docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md docs/phase8_1_api_contract.md src/adam_agent/api/models.py src/adam_agent/api/service.py tests/test_api_phase8.py
python -m compileall -q src/adam_agent
```

Result: 63 Phase 8 API tests passed; 135 related graph/static/schema tests
passed; diff check and compileall passed before subagent review.

Subagent review:

- Subagent review returned GO with no blocking findings.
- The non-blocking request to clarify the `RunStudyRequest` schema docstring was
  addressed before commit.

### 2026-05-30 - LG2.8 Product Read-Model Write Isolation Slice

Completed:

- Removed direct `mark_workflow_inputs_current()` calls from the product
  compatibility endpoints:
  - `generate-code`
  - `finalize-inputs`
  - explicit `draft-spec`
- These endpoints now leave `workflow_state.json` updates to
  `GraphGateway`/canonical graph projection instead of writing a service-owned
  `*_start` checkpoint before the graph-owned product step.
- Added API regression coverage for dependency-gated product calls:
  when the graph gate blocks `finalize-inputs`, `generate-code`, or explicit
  `draft-spec`, the persisted `workflow_state.json` remains a LangGraph
  projection and is not overwritten with a service-owned `*_start` node.
- Updated the LG2 baseline wording: `workflow_state.json` is a compatibility
  read model, not product truth; direct compatibility writes are legacy surfaces
  to remove or isolate.

Current boundary:

- This slice does not remove `workflow_state.json`; the current UI still reads
  it as a compatibility projection.
- Upload invalidation and old `/runs` non-LLM compatibility paths still use
  workflow-state helpers. This slice only removes product split-flow start
  writes that competed with graph projection.
- Canonical input fingerprint protection remains in `GraphGateway` and graph
  state records.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_requires_finalize_before_regenerating_code -v
```

Result: the initial 3 focused tests passed. After subagent review, two more
dependency-gate read-model tests were added for `generate-code` and explicit
`draft-spec`; the 3 read-model isolation tests passed.

Broader verification:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
git diff --check -- src/adam_agent/api/service.py tests/test_api_phase8.py docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
python -m compileall -q src/adam_agent
```

Result: 183 related gateway/API/graph/static/reference tests passed; 49 core
tests passed; diff check and compileall passed.

Static-rule plan adjustment:

- The LG2.5 plan now explicitly says static rules must be designed as reusable
  contract checks from first principles, not as demo failure patches.
- Added a rule lifecycle checklist: candidate observation -> generic contract
  -> authority binding -> deterministic check -> reviewer visibility.
- Reiterated that the generic engine has no exception registry; future
  clinical/domain rules must enter through approved specs or source-backed rule
  packs before they can block a run.

Subagent review:

- Subagent review returned GO.
- It reported no blocking findings.
- Its main non-blocking suggestion was to add the same rejected-gate read-model
  invariant for `generate-code` and explicit `draft-spec`; those tests were
  added before commit.
- After a small helper-name cleanup, final subagent re-review again reported no
  blocking findings.

### 2026-05-30 - LG2 Static-Rule Boundary And Terminal-Failure Review Gateway Slice

Completed:

- Tightened the LG2.5 static-rule plan around a product-level constraint:
  static checks must be generic contract/rule-pack checks, not patches for
  PSY201, a specific uploaded file, a single ADaM dataset, or a demo-derived
  clinical variable anecdote.
- Added a rule-design review checklist for future static-check work:
  every blocking check must identify the declared contract being checked and
  its authority source: system contract, approved spec, user policy, or a
  versioned rule pack with source/scope/severity/evidence.
- Added `GraphGateway.review_terminal_failure()` as the graph-owned entry
  point for terminal-failure triage compatibility endpoints.
- `persist_terminal_failure_review()` now delegates graph-state loading,
  dataset validation, action normalization, and `HumanCommand` construction to
  `GraphGateway`.
- Added a gateway-level regression test proving the new entry point persists a
  retry triage decision, clears the terminal-failure interrupt, and records the
  next action in canonical graph state.
- Added direct negative wrapper tests proving `GraphGateway.review_terminal_failure()`
  rejects unsupported decisions and refuses to write review state when the
  dataset is not waiting on an open `terminal_failure` interrupt.

Current boundary:

- This slice does not add any new clinical/static ADaM rule.
- Static checks remain generic contract/rule-pack checks. A future
  dataset-specific standards rule must enter through a source-backed rule pack
  with authority_type/source/scope/severity/evidence, not through branches in
  the generic engine.
- This slice does not change terminal-failure routing semantics:
  `retry_execution`, `repair_code`, `revise_spec`, `request_new_input`,
  `skip_dataset`, and `continue_other_datasets` keep the existing behavior.
- `record_terminal_failure_review()` remains the lower-level recorder used by
  tests and graph internals; `review_terminal_failure()` is the safer
  compatibility endpoint entry point.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_skip_terminal_failure_marks_study_failed tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_skip_dataset_does_not_unlock_code_regeneration -v
```

Result: 7 initial tests passed before the subagent review. After the subagent
review, two negative wrapper tests were added and the 9 focused tests passed.

Broader verification:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
git diff --check -- src/adam_agent/graph/gateway.py src/adam_agent/api/service.py tests/test_graph_gateway.py docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
python -m compileall -q src/adam_agent
```

Result: 181 related gateway/API/graph/static/reference tests passed; 49 core
tests passed; diff check and compileall passed.

Subagent review:

- Subagent review returned GO.
- It reported no major business, logic, or schema issue.
- The only note was a non-blocking suggestion to add direct negative tests for
  the new gateway wrapper; those tests were added before commit.

### 2026-05-30 - LG2.2 Gateway Execution Approval Preflight Slice

Completed:

- Moved the "approved code must exist in canonical graph state" execution
  preflight up into `GraphGateway.execute_approved_code()`.
- The gateway now verifies the graph-owned code-review decision before invoking
  `DatasetGraph` in `graph_product_execute` mode.
- The preflight reuses the existing graph execution contract checks for:
  - approved `code_state.status` and `decision`
  - current input fingerprint
  - review artifact path
  - generated R code hash
  - static-check artifact path/hash/schema/status
  - approved spec path/hash when present
  - runtime dependency artifact hashes
- Added gateway coverage that asserts execution is blocked before
  `DatasetGraph` invocation when graph state has generated code but no approved
  code-review decision.
- Updated the positive gateway execution test so it seeds the same
  generate-code -> code-review -> execute approval chain used by the product
  flow.

Current boundary:

- This is a workflow-contract hardening slice, not a new clinical/static ADaM
  rule.
- Static checks remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.
- The deeper execution boundary still performs the same approval checks again,
  so this change adds an earlier fail-closed gate rather than replacing the
  sandbox-side validation.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_execute_requires_graph_approved_code_before_dataset_graph_invocation -v
```

Result: 2 focused tests passed.

### 2026-05-30 - LG2.2 Gateway-Owned Code Review Artifact Slice

Completed:

- Added `GraphGateway.review_code()` as the graph-owned entry point for the
  code-review compatibility endpoint.
- The gateway now owns the full code-review transition:
  - verify generated code is recorded in canonical graph state
  - verify current generated-code hash, static-check artifact, approved spec
    hash, dependency artifact hashes, and input fingerprint
  - write `runs/{run_id}/review/{dataset}_code_review.json`
  - persist the code-review decision into `graph_state.json`
  - refresh the UI `workflow_state.json` projection
- Reduced `api/service.py::persist_code_review()` to request validation,
  delegation to `GraphGateway.review_code()`, and compatibility response
  construction.
- Added gateway tests for the direct review-code entry point and for cleanup
  when graph-state recording fails after the review artifact is written.

Current boundary:

- Public route paths and response shapes are unchanged.
- This moves artifact/state ownership into the gateway; it does not create a
  new LangGraph interrupt-resume API yet.
- Static checks remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_writes_artifact_and_records_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_cleans_artifact_when_recording_fails tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_dataset_code_review_in_canonical_state -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_code_review_cleans_approval_json_when_graph_recording_fails tests.test_api_phase8.Phase8ApiTests.test_code_review_requires_graph_generated_code_state tests.test_api_phase8.Phase8ApiTests.test_code_approval_is_invalidated_when_code_or_inputs_change -v
```

Result: 3 focused gateway tests passed; 4 focused API tests passed.

### 2026-05-30 - LG2.2 Gateway-Owned Draft-Spec Review Artifact Slice

Completed:

- Added `GraphGateway.review_draft_spec()` as the graph-owned entry point for
  the draft-spec review compatibility endpoint.
- The gateway now owns the draft-spec review transition:
  - verify the draft spec is recorded in canonical graph state
  - verify the draft-spec hash and current input fingerprint before trust
  - write `runs/{run_id}/reviews/{dataset}_draft_spec_review.json`
  - write `runs/{run_id}/approved_specs/{dataset}_approved_spec.json` when the
    human decision approves the draft
  - persist the review decision into `graph_state.json`
  - refresh the UI `workflow_state.json` projection
- Reduced `api/service.py::persist_draft_spec_review()` to request validation,
  delegation to `GraphGateway.review_draft_spec()`, and compatibility response
  construction.
- Added gateway and API tests for the direct review-draft-spec entry point and
  for cleanup when graph-state recording fails after review artifacts are
  written.

Current boundary:

- Public route paths and response shapes are unchanged.
- This moves draft-spec review artifact/state ownership into the gateway; it
  does not create a new LangGraph interrupt-resume API yet.
- Static checks remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_draft_spec_writes_artifacts_and_records_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_draft_spec_cleans_artifacts_when_recording_fails tests.test_api_phase8.Phase8ApiTests.test_draft_spec_review_cleans_artifacts_when_graph_recording_fails -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_draft_spec_review_in_canonical_state tests.test_api_phase8.Phase8ApiTests.test_missing_input_spec_requires_draft_spec_approval_before_code_generation tests.test_api_phase8.Phase8ApiTests.test_approved_draft_spec_is_invalidated_when_inputs_change tests.test_api_phase8.Phase8ApiTests.test_approved_draft_spec_is_invalidated_when_approved_file_changes -v
```

Result: 3 focused ownership/cleanup tests passed; 4 existing draft-spec review
regression tests passed.

### 2026-05-30 - LG2.2 Gateway-Owned Compare Report Artifact Slice

Completed:

- Moved compare-report artifact writing behind `GraphGateway.record_compare()`.
- The service still computes the current CSV structural/reference comparison,
  but it no longer writes `runs/{run_id}/compare/{dataset}_compare_report.json`
  directly for graph-backed runs.
- `GraphGateway.record_compare()` can now write the compare report artifact,
  record the artifact in canonical dataset state, update compare summary, and
  refresh the UI `workflow_state.json` projection in one graph-owned transition.
- Kept the compatibility behavior for ad-hoc compare calls without graph state:
  the endpoint returns a transient compare response but does not create a graph
  state or canonical compare report.
- Added gateway and API coverage for gateway-written compare reports and for
  the no-graph-state compatibility boundary.
- Clarified the read/write boundary: `/review-summary` may compute a transient
  compare status for display, but it must not call `record_compare()` or mutate
  canonical graph/workflow state. Explicit `/datasets/{dataset}/compare` remains
  the compare/report persistence action.

Current boundary:

- This does not change the compare algorithm. It is still an initial CSV
  structural and sampled-cell comparison, not a clinical conformance validator.
- Review-summary is a read-model endpoint. If reference files change after a
  prior explicit compare, the summary can show the current transient comparison
  while graph state keeps the last explicit compare record until the user runs
  compare again.
- Reference ADaM remains comparison/output-shape evidence only. This slice does
  not make reference ADaM a derivation authority.
- Static checks remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_does_not_create_graph_state_without_prepared_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_reports_compare_without_mutating_graph_when_reference_disappears tests.test_api_phase8.Phase8ApiTests.test_review_summary_read_model_helpers_do_not_record_compare tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_writes_compare_report_artifact_when_requested tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_requires_existing_graph_state -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
```

Result: 7 focused compare/read-model tests passed; 178 related gateway/API/graph/static/
reference tests passed.

### 2026-05-30 - LG2.2 Explicit Draft-Spec Gateway Slice

Completed:

- Added `GraphGateway.generate_draft_spec()` as the graph-owned entry point for
  the explicit `/draft-spec` compatibility endpoint.
- The endpoint now delegates fresh draft-spec generation to `DatasetGraph`
  through the gateway instead of building LLM context, calling the provider, and
  recording graph state inside `api/service.py`.
- Added `force_new_draft_spec` to the dataset graph state so explicit
  draft-spec generation can request a new review-required draft instead of
  reusing a previously approved draft spec.
- The gateway rejects explicit draft-spec generation when a user-supplied
  `input_spec` already exists, before writing canonical graph state.
- Kept dependency gating in the service compatibility wrapper for this slice,
  consistent with the current LG2.2 boundary.

Current boundary:

- Public route paths and response shapes are unchanged.
- `finalize-inputs` and explicit `/draft-spec` now share the same graph-owned
  prepare/draft-spec path, but they keep different user intent:
  `finalize-inputs` may accept existing input specs or approved drafts, while
  explicit `/draft-spec` forces a fresh draft only when no input spec exists.
- Static rules remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generate_draft_spec_forces_fresh_draft_and_rejects_input_spec tests.test_api_phase8.Phase8ApiTests.test_missing_input_spec_requires_draft_spec_approval_before_code_generation tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_generates_review_required_draft_spec_when_spec_missing tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
```

Result: 4 focused tests passed.

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
```

Result: 169 related gateway/API/graph/static/reference tests passed; 49
additional core tests passed.

### 2026-05-30 - LG2.2 GraphGateway-Owned Dependency Gate Slice

Completed:

- Added `GraphGateway.dependency_gate_for_product_step()` as the graph-owned
  dependency gate for dataset product steps.
- Moved the product-step dependency decision out of `api/service.py` for:
  - finalize inputs
  - explicit draft spec generation
  - R code generation
  - approved R execution
- The service compatibility wrappers now still resolve config/provider inputs
  and shape API responses, but they ask the gateway whether the dependency gate
  is open before entering the product step.
- Removed the old service-local `_assert_target_dependency_gate_open_for_product_step()`
  and plan-read/start helper that duplicated graph dependency state decisions.
- Added gateway-level tests for:
  - auto-starting a dependency plan when a product step has no plan yet
  - failing closed when an unresolved upstream ADaM dependency blocks the target
  - returning dependency plan fields when the gate is open

Current boundary:

- Public route paths and response shapes are unchanged.
- Dependency planning itself still belongs to `StudyGraph`; this slice only
  moves product-step gate ownership into `GraphGateway`.
- Static rules remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_starts_plan_and_blocks_unresolved_dependency tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_returns_plan_when_open tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_dependency_warning tests.test_api_phase8.Phase8ApiTests.test_execute_rejects_changed_runtime_dependency_artifact -v
```

Result: 5 focused tests passed.

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
```

Result: 171 related gateway/API/graph/static/reference tests passed; 49
additional core tests passed.

### 2026-05-30 - LG2.2 GraphGateway-Owned Execution Slice

Completed:

- Added `GraphGateway.execute_approved_code()` as the graph-owned entry point
  for approved R execution.
- The new gateway method owns:
  - terminal-failure preflight through `validate_product_step_start(step="execute")`
  - `DatasetGraph` invocation in `graph_product_execute` mode
  - response-field extraction for API compatibility
  - canonical graph-state recording through `record_execution()`
- Reduced `api/service.py::execute_approved_dataset_code()` to a compatibility
  wrapper that validates the dependency plan, delegates execution to
  `GraphGateway`, and returns the existing response shape.
- Updated retry-gate tests to patch `adam_agent.graph.gateway.compile_dataset_graph`,
  proving the execution graph call has moved out of the service layer.
- Added gateway-level coverage that asserts approved-code execution invokes
  `DatasetGraph` with `execution_mode == graph_product_execute`, persists
  canonical `graph_state.json`, and refreshes the UI projection.

Current boundary:

- The public route path and UI behavior are unchanged.
- Dependency-plan gating remains in the service compatibility wrapper for now;
  a later slice can move dependency gate ownership into the gateway once the
  service wrappers are further reduced.
- This does not introduce autonomous retry or broader repair policy. Terminal
  failure routing still depends on the existing human terminal-failure review
  decisions.
- Static rules remain governed by generic contracts and source-backed rule
  packs; this slice does not add dataset/study/demo-specific static checks.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path -v
```

Result: 3 focused tests passed.

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules tests.test_reference_store -v
```

Result: 111 related gateway/API/static/reference tests passed.

### 2026-05-30 - LG2.2 GraphGateway-Owned Code Generation Slice

Completed:

- Added `GraphGateway.generate_code()` as the graph-owned entry point for
  generated R code creation and code-review interrupt persistence.
- The new gateway method owns:
  - terminal-failure preflight for `generate_code`
  - `DatasetGraph` invocation in `graph_product_generate_code` mode
  - generated-code/static-check/spec artifact hash extraction
  - canonical graph-state recording through `record_code_generation()`
  - API-facing field extraction for the existing response shape
- Reduced `api/service.py::generate_dataset_code()` to:
  - HTTP/request validation
  - config/provider/exposure resolution
  - dependency-plan gating
  - delegation to `GraphGateway.generate_code()`
  - compatibility response construction
- Kept provider and context builders injectable from the service so provider
  policy remains isolated and existing browser-scoped provider tests keep their
  boundary.
- Added gateway-level coverage that asserts generated-code orchestration invokes
  `DatasetGraph` with `execution_mode == graph_product_generate_code`, writes
  canonical graph state, and refreshes the UI projection.

Current boundary:

- The public route path and response shape are unchanged.
- At the time of this slice, dependency-plan gating and `finalize-inputs` were
  still compatibility-wrapper responsibilities.
- Superseded by later slices: dependency-plan gating and `finalize-inputs` are
  now graph/gateway-owned product boundaries.
- Static checks remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_generate_code_uses_browser_scoped_real_provider_settings -v
```

Result: 3 focused tests passed.

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
```

Result: 166 related gateway/API/graph/static/reference tests passed.

### 2026-05-30 - LG2.2 GraphGateway-Owned Finalize Inputs Slice

Completed:

- Added `GraphGateway.finalize_inputs()` as the graph-owned entry point for the
  upload-complete/spec-readiness checkpoint.
- The new gateway method owns:
  - terminal-failure preflight for `finalize_inputs`
  - `DatasetGraph` invocation in `graph_product_prepare` mode
  - input-spec ready recording through `record_input_spec_ready()`
  - approved-draft-spec ready recording through
    `record_approved_draft_spec_ready()`
  - review-required draft-spec recording through
    `record_draft_spec_generation()`
- Reduced `api/service.py::finalize_dataset_inputs()` to:
  - HTTP/request validation
  - config/provider/exposure resolution
  - dependency-plan gating
  - delegation to `GraphGateway.finalize_inputs()`
  - compatibility response construction
- Removed the service-layer direct `compile_dataset_graph` import. The service
  no longer invokes `DatasetGraph` directly for product `finalize`, `generate`,
  or `execute` steps.
- Added gateway-level coverage for:
  - existing input_spec branch
  - missing-spec draft-spec branch

Current boundary:

- The public route path and response shape are unchanged.
- At the time of this slice, dependency-plan gating and the explicit
  `/draft-spec` endpoint still had compatibility-wrapper responsibilities.
- Superseded by later slices: dependency-plan gating and explicit draft-spec
  generation now route through graph/gateway-owned product methods.
- Static checks remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_uses_existing_input_spec_without_draft_generation tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_generates_review_required_draft_spec_when_spec_missing tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_passes_rscript_path_to_context_builder -v
```

Result: 5 focused tests passed.

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
```

Result: 168 related gateway/API/graph/static/reference tests passed; 49
additional core tests passed.

### 2026-05-30 - LG2.5 Static-Rule Governance Clarification And Retry Regression Slice

Completed:

- Tightened the LG2.5 static-rule plan around the user's review point: static
  rules verify declared contracts and must not become a patch list of
  demo/dataset-specific clinical observations.
- Clarified that new blocking ADaM/CDISC/company-standard checks must first
  enter through a source-backed rule-pack admission contract with source,
  version, scope, declared severity, and evidence.
- Added a source-level regression guard that keeps demo study names, dataset
  names, and demo-derived clinical variable anecdotes out of the generic
  `static_rules.py` engine.
- Strengthened that guard with an AST-level branch scan: generic static-rule
  branch conditions cannot depend on demo/study/dataset names or hand-picked
  clinical-variable literals.
- Added a positive terminal-failure retry regression: after a human
  `retry_execution` review, `/execute-approved-code` is allowed to enter the
  graph-owned `graph_product_execute` path and records the retry follow-up as
  consumed by execution.

Current boundary:

- The static-rule guard is intentionally about the generic engine. Dataset names
  and standards terms may still appear in tests or future versioned rule-pack
  fixtures.
- The AST guard is intentionally structural. It blocks branch conditions that
  would turn the generic engine into a patch table, while still allowing
  dataset terms in tests, approved specs, and governed rule-pack fixtures.
- The retry regression uses a mocked DatasetGraph return value. It proves the
  compatibility wrapper gate and graph-state recording path, not real R
  execution.

Verification:

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_generated_code tests.test_static_rules tests.test_sandbox -v
```

Result: 24 static-rule tests passed; 3 focused retry tests passed; 205 core
tests passed.

Subagent review:

- Subagent review returned GO.
- It reported no major logic flaw, no misleading architecture claim, and no
  invalid retry regression.

### 2026-05-30 - LG2.5 Rule-Pack Admission Contract Slice

Completed:

- Added generic `StaticRulePack` and `StaticRulePackItem` contracts.
- Added `validate_static_rule_pack_payload()` and `load_static_rule_pack()` so
  standards/company rules must pass provenance admission before any later slice
  can use them.
- Admission currently requires authority_type, source, version, scope, declared
  severity, evidence, and unique `rule_id` values.
- Scope values must be explicit strings. This prevents ambiguous object-shaped
  scope payloads from becoming hidden engine logic.

Current boundary:

- This slice does not execute standards-pack rules and does not add any
  ADaM/CDISC clinical checks.
- It creates the gate through which future CDISC/P21/company-standard rules
  must enter.

Verification:

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_static_rules tests.test_llm_generated_code tests.test_downstream_runner tests.test_graph_gateway tests.test_api_phase8 -v
```

Result: 20 static-rule tests passed; 126 related core tests passed.

### 2026-05-30 - LG2.5 Reference Tool Interface Slice

Completed:

- Added local reference lookup tool contracts:
  - `search_cdisc_reference`
  - `lookup_adam_rule`
  - `lookup_p21_rule`
  - `lookup_company_standard`
- Each tool maps to an explicit local reference root under `references/`.
- Added `ReferenceToolRequest` and `ReferenceToolResult` so agents can record
  which tool was called, what query was used, which hits were returned, and what
  warnings apply.
- Split reference-store tests into `tests/test_reference_store.py`.

Current boundary:

- These tools only search local text-like files. They do not prove compliance
  and do not execute standards rules.
- Empty lookup results are recorded as warnings, not evidence that a rule does
  not exist.
- Reference hits remain evidence for agents/review, not hidden derivation
  authority.
- `reference_root` is a local configuration/testing parameter. If a later API
  exposes it to user or agent input, it must gain resolved-path allowlisting and
  symlink escape checks first.

Verification:

```text
python -B -m unittest tests.test_reference_store tests.test_static_rules -v
python -B -m unittest tests.test_reference_store tests.test_static_rules tests.test_state_schemas tests.test_llm_context tests.test_downstream_runner tests.test_api_phase8 -v
```

Result: 22 focused reference/static tests passed; 117 related core tests passed.

Subagent review:

- Subagent review returned GO.
- The only residual note was that caller-configurable `reference_root` is
  acceptable for the local tool contract, but must be hardened before exposure
  through API or agent-supplied parameters.

### 2026-05-30 - LG2.2 Product Stub-Path Isolation Slice

Completed:

- Routed graph-product agent nodes directly to `summarize_dataset`:
  - `draft_spec_agent`
  - `generate_r_code_agent`
  - `execute_approved_code`
- Kept the legacy stub chain available only through the explicit
  `stub_chain` branch from `prepare_dataset`.
- Removed the early `graph_product_execute` call from `prepare_dataset`, so R
  execution happens once in the explicit `execute_approved_code` graph node.
- Preserved the terminal-failure `revise_spec` semantic: after a human chooses
  to revise the spec, a later finalize/draft step must create a new draft spec
  instead of reusing the old approved draft spec.
- Added graph smoke tests that assert product nodes no longer flow through
  `draft_lineage_stub`, and that prepare does not execute R in
  `graph_product_execute` mode.

Current boundary:

- Legacy stub nodes still exist for explicit legacy/test modes.
- Compatibility FastAPI endpoints still call graph modes one step at a time;
  this slice only cleans the DatasetGraph product path itself.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_with_approved_draft_spec_generates_new_draft_and_clears_old_review tests.test_graph_smoke -v
```

Result: 55 tests passed.

### 2026-05-30 - LG2.2 Execution Preflight Gate Slice

Completed:

- Moved the terminal-failure execution gate before DatasetGraph execution in the
  FastAPI compatibility wrapper.
- `/execute-approved-code` now calls
  `GraphGateway.validate_product_step_start(step="execute")` before invoking
  `graph_product_execute`, so an unreviewed terminal failure cannot start the
  execution graph or R boundary.
- Kept the graph-layer `record_execution()` validation as a second fail-closed
  guard after execution.
- Preserved the user-facing retry error wording for execute attempts blocked by
  an unresolved terminal failure.
- Added API regression coverage that patches `compile_dataset_graph` and asserts
  it is not called when a second execution attempt is blocked before terminal
  failure review.

Current boundary:

- This is a compatibility-wrapper preflight guard. The endpoint still calls one
  graph mode at a time; full LangGraph interrupt resume remains a later slice.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit -v
```

Result: 3 tests passed.

### 2026-05-30 - LG2.8 Compatibility Projection Equivalence Slice

Completed:

- Added API regression coverage that treats compatibility endpoint metadata as
  a contract, not just paths on disk.
- Representative compatibility responses now load their `graph_state_path` and
  `workflow_state_path` and run `workflow_projection_consistency()`:
  - finalize inputs with an existing input spec
  - finalize inputs with generated draft spec
  - draft spec generation
  - draft spec review
  - R code generation
  - code review
  - approved local execution

Current boundary:

- This is test coverage only. It does not change API routes, graph transitions,
  generation, review gates, execution, compare, or sandbox behavior.
- The equivalence check covers core projection fields already defined by
  `workflow_projection_consistency()`. It is not a full byte-for-byte equality
  check of every UI read-model field.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8 -v
```

Result: 58 tests passed.

### 2026-05-30 - LG2.8 Graph-Mutating Endpoint Projection Slice

Completed:

- Added `_assert_run_projection()` test helper for endpoints that mutate a run
  but do not return compatibility shim metadata.
- The helper reads `runs/{run_id}/graph_state.json` and
  `runs/{run_id}/workflow_state.json`, then runs
  `workflow_projection_consistency()` so tests assert the UI projection is still
  derived from canonical graph state.
- Added representative coverage after:
  - compare report generation
  - compare-summary refresh when a reference file disappears
  - terminal-failure review with `repair_code`
  - terminal-failure review with `retry_execution`
  - terminal-failure review with `skip_dataset`

Current boundary:

- This is regression coverage only. It does not make `workflow_state.json` a
  source of truth and does not broaden the projection equivalence contract beyond
  `workflow_projection_consistency()`.
- It focuses on graph-mutating endpoint classes that previously lacked the
  compatibility metadata response checked by the prior LG2.8 slice.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8 -v
```

Result: 58 tests passed.

### 2026-05-30 - LG2.8 Compatibility Shim Metadata Slice

Completed:

- Added explicit compatibility metadata to the old dataset-level product
  endpoint responses:
  - `workflow_control: graph_gateway_compatibility_shim`
  - `graph_state_path`
  - `workflow_state_path`
- Covered the main compatibility endpoints:
  - finalize inputs
  - draft spec generation
  - draft spec review
  - R code generation
  - code review
  - approved local execution
- Added API tests that assert these responses point to existing canonical graph
  state and workflow projection files.

Current boundary:

- This is an observability/deprecation slice. It marks old URLs as compatibility
  shims and makes graph ownership visible to callers.
- It does not remove old endpoints, rewrite their route paths, or change the
  backend execution order.
- Canonical truth remains `graph_state.json`; `workflow_state.json` remains a
  compatibility projection.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8 -v
```

Result: 58 tests passed.

Subagent review:

- Subagent review returned GO.
- No major logic bug, schema/backcompat issue, or misleading metadata was
  reported.
- The reviewer confirmed the docs do not overclaim: this is compatibility
  observability/deprecation metadata only, and does not change gates, execution
  order, route paths, or sandbox behavior.

Final verification:

```text
git diff --check -- docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md src/adam_agent/api/models.py src/adam_agent/api/service.py tests/test_api_phase8.py
```

Result: no whitespace errors.

```text
python -B -m unittest tests.test_api_phase8 -v
```

Result: 58 tests passed.

### 2026-05-30 - LG2.8 Dependency Review Gateway Ownership Slice

Completed:

- Added `GraphGateway.review_dependency()` as the graph-owned high-level entry
  point for study-level dependency review decisions.
- Moved dependency-review state loading, interrupt validation, human command
  construction, and resume into the gateway.
- Reduced `api/service.py::persist_dependency_review()` to request validation,
  gateway delegation, and compatibility response shaping.
- Strengthened the product service wrapper regression guard so wrappers must
  call the corresponding GraphGateway high-level method and must not call
  `load_graph_state()`, `resume()`, low-level recorders, or direct workflow
  writes.
- Added gateway coverage proving dependency-review approval persists the human
  command in canonical graph state and refreshes the UI projection.

Current boundary:

- Public route path and response shape are unchanged.
- This does not remove the lower-level `resume()` primitive; it remains a
  graph-internal building block and explicit test helper.
- This does not change dependency planning semantics, product generation,
  execution, compare behavior, UI layout, or sandbox behavior.
- Static-rule governance is unchanged. No clinical, dataset-specific,
  study-specific, demo-specific, or variable-specific static rule is added.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods tests.test_graph_gateway.GraphGatewayTests.test_dependency_review_endpoint_resumes_graph_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_dependency_owns_interrupt_resume -v
python -m compileall -q src\adam_agent
```

Result: 3 focused gateway/API tests passed; compileall passed.

### 2026-05-30 - LG2.8 Prepare Response Graph-State Metadata Slice

Completed:

- Added `graph_state_path` to `RunPlanResponse`.
- Updated `prepare_run_plan()` so `/runs/prepare` responses expose both the
  canonical `graph_state.json` path and the compatibility `workflow_state.json`
  projection path.
- Added regression coverage for the service helper and FastAPI endpoint shape.

Current boundary:

- This is compatibility/read-model observability only.
- It does not change dependency planning semantics, interrupts, generation,
  execution, compare, UI layout, or sandbox behavior.
- Static-rule governance is unchanged. No clinical, dataset-specific,
  study-specific, demo-specific, or variable-specific static rule is added.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_prepare_run_plan_uses_graph_projection tests.test_graph_gateway.GraphGatewayTests.test_graph_state_endpoint_returns_canonical_state -v
```

### 2026-05-30 - LG2.8 GraphGateway-Owned Compare Entry Point Slice

Completed:

- Added `src/adam_agent/tools/compare.py` as the deterministic compare tool
  boundary for:
  - generated output usability checks
  - reference ADaM lookup
  - generated-vs-reference CSV structural/cell compare
- Added `GraphGateway.compare_reference_output()` as the graph-owned high-level
  entry point for stateful compare.
- The compare endpoint now delegates prepared graph runs to
  `GraphGateway.compare_reference_output()` instead of computing compare and
  then calling a low-level recorder from the service layer.
- Preserved the legacy compatibility behavior for ad-hoc compare calls without
  `graph_state.json`: the endpoint can still return a transient compare
  response, but it does not create graph state or write a compare report.
- Added regression coverage proving:
  - stateful compare is delegated to the gateway;
  - gateway compare computes and records canonical compare state;
  - no-graph-state compare remains transient.

Current boundary:

- This does not change the compare algorithm. It remains an initial CSV
  structural and sampled-cell compare, not clinical derivation validation.
- `review-summary` remains a read-model endpoint and still computes a fresh
  non-mutating compare preview so disappearing reference files are visible
  without rewriting historical graph state.
- Static-rule governance is unchanged. No clinical, dataset-specific,
  study-specific, demo-specific, or variable-specific static rule is added.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_does_not_create_graph_state_without_prepared_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_reports_compare_without_mutating_graph_when_reference_disappears -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_reference_output_computes_and_records_compare tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_writes_compare_report_artifact_when_requested tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_requires_existing_graph_state -v
python -m compileall -q src\adam_agent
```

Result: 4 focused API tests passed; 4 focused gateway tests passed; compileall
passed.

### 2026-05-30 - LG2.8 Upload Invalidation Gateway Ownership Slice

Completed:

- Added `GraphGateway.mark_study_inputs_changed()` as the single high-level
  entry point for upload-triggered input invalidation.
- Reduced `api/service.py::save_uploaded_file_bytes()` so the service saves
  files, rescans inputs, and delegates all run invalidation to the gateway.
- Preserved the public upload response fields:
  - `touched_runs` remains a compatibility projection/read-model field;
  - `touched_graph_runs` remains the canonical graph-state invalidation signal;
  - `skipped_graph_runs` reports canonical graph runs that could not be loaded.
- Added boundary coverage proving the upload service helper calls
  `GraphGateway.mark_study_inputs_changed()` and no longer calls
  `invalidate_active_workflows()` or `mark_all_inputs_changed()` directly.
- Added gateway coverage proving the high-level upload invalidation entry point
  returns both compatibility and graph touch lists while preserving projection
  consistency.

Current boundary:

- `invalidate_active_workflows()` still exists as a compatibility projection
  helper, but it is no longer called by the FastAPI service upload helper.
  Product service code should use `GraphGateway.mark_study_inputs_changed()`.
- The old `touched_runs` name is retained only for API/UI compatibility. New
  graph-aware behavior must rely on `touched_graph_runs`.
- This does not change dependency planning semantics, generation, execution,
  compare, UI layout, route paths, or sandbox behavior.
- Static-rule governance is unchanged. No clinical, dataset-specific,
  study-specific, demo-specific, or variable-specific static rule is added.
  Static rules remain generic contract checks or governed rule-pack items only.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_scans_canonical_graph_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_preserves_existing_stale_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_reports_corrupt_graph_state_as_skipped tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_study_inputs_changed_returns_legacy_and_graph_touches tests.test_api_phase8.Phase8ApiTests.test_upload_endpoint_delegates_input_invalidation_to_gateway tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_workflow_state_stale tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation tests.test_api_phase8.Phase8ApiTests.test_upload_invalidates_graph_run_even_when_workflow_projection_is_missing tests.test_api_phase8.Phase8ApiTests.test_upload_reports_corrupt_graph_state_as_skipped -v
```

Result: 9 focused tests passed.

### 2026-05-30 - LG2.8 Legacy `/runs` Gateway Ownership Slice

Completed:

- Added graph-gateway entry points for the remaining legacy run-to-completion
  surface:
  - `GraphGateway.block_legacy_run_to_completion()` writes the blocked
    compatibility projection for LLM `/runs` calls that must use the split-flow
    review gates.
  - `GraphGateway.run_legacy_to_completion()` runs the old stub/test
    compatibility graph path and writes the legacy workflow projection.
- Removed the remaining direct `workflow_state.json` write helpers from
  `api/service.py`.
- Removed the direct `compile_study_graph()` call from
  `api/service.py::run_study_from_request()`.
- Strengthened API boundary tests so service helpers must not call
  `update_workflow_state()` directly, and legacy `/runs` state is delegated to
  gateway methods.
- Added gateway coverage for both rejected LLM run-to-completion and old stub
  run-to-completion projections.

Current boundary:

- `POST /runs` remains a legacy compatibility route. It is not the product LLM
  generation path.
- The current product path remains `/runs/prepare` plus per-dataset
  finalize/draft-spec/code-review/execute gates.
- The legacy `/runs` response shape is preserved, including
  `workflow_control: legacy_run_to_completion_compatibility_shim` and
  `graph_state_path: null`.
- Static-rule governance is unchanged. This slice does not add clinical,
  dataset-specific, study-specific, demo-specific, or variable-specific static
  rules.

Focused verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_blocks_legacy_llm_run_to_completion_with_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_runs_legacy_stub_to_completion_with_projection tests.test_api_phase8.Phase8ApiTests.test_service_layer_no_longer_writes_workflow_state_directly tests.test_api_phase8.Phase8ApiTests.test_run_study_from_request_delegates_legacy_run_state_to_gateway tests.test_api_phase8.Phase8ApiTests.test_legacy_run_workflow_helpers_are_removed_from_service_layer tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion tests.test_api_phase8.Phase8ApiTests.test_demo_study_rejects_run_to_completion_llm_endpoint -v
```

Result: 8 focused tests passed.

### 2026-05-30 - LG2.8 Graph-Owned Progress Read Model Slice

Completed:

- Added `GraphGateway.progress_summary()` as a graph-owned read model for run
  progress and next actions.
- Added `GET /runs/{run_id}/progress` so the UI can ask the gateway what the
  current graph state means instead of reconstructing workflow logic from
  browser state or raw JSON.
- Added response contracts:
  - `RunProgressResponse`
  - `DatasetProgressItem`
- The progress payload reports:
  - study-level status and current interrupt;
  - graph-owned next action;
  - per-dataset status, current interrupt, spec/code/execution/validation/
    compare status;
  - whether a dataset's local next action is currently blocked by a
    study-level dependency gate.
- Added tests proving:
  - the gateway owns the next-action read model;
  - the FastAPI endpoint exposes it;
  - the service helper delegates to `GraphGateway.progress_summary()` and does
    not call `load_graph_state()` directly.
  - stale dependency plans are surfaced as `replan_dependencies` instead of a
    generic dependency-review action.
  - progress blocking mirrors the product dependency gate for review-required
    dependency evidence.

Current boundary:

- This is a read-model slice only. It does not change dependency planning,
  draft-spec generation, code generation, code review, execution, compare,
  route semantics, or UI layout.
- The endpoint still reads from the current canonical `graph_state.json`
  through `GraphGateway`. It is a step toward graph-owned UI orchestration, not
  a native LangGraph interrupt/checkpointer replacement.
- Static-rule governance is unchanged. This slice adds no clinical,
  dataset-specific, study-specific, demo-specific, or variable-specific static
  rule. Static rules remain generic contract checks or governed rule-pack
  items only.
- Static-rule design remains first-principles and reusable: future blocking
  ADaM/CDISC/company checks must be expressed as generic contracts or admitted
  through versioned source-backed rule packs, not as patches for observed demo
  failures.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions -v
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
git diff --check -- src\adam_agent\api\models.py src\adam_agent\api\service.py src\adam_agent\api\app.py src\adam_agent\graph\gateway.py tests\test_graph_gateway.py tests\test_api_phase8.py
```

Result: focused tests passed; compileall passed; 168 related
gateway/API/static-rule tests passed; diff check passed.

Subagent review:

- Initial review returned NO-GO for two progress-read-model issues:
  - stale input fingerprints could be presented as a normal dependency review
    instead of a replan action;
  - `review_required` dependency decisions from real evidence sources could be
    unblocked in progress even though product methods would reject them.
- Both issues were fixed and covered with regression tests.
- Final related verification passed with 168 gateway/API/static-rule tests.

### 2026-05-30 - LG2.8 UI Progress Read Model Wiring Slice

Completed:

- Added `state.runProgress` to the local UI and refresh it from
  `GET /runs/{run_id}/progress`.
- The study progress panel, top graph status, dataset cards, action hints, and
  human review queue now prefer `/progress` next actions, blocked reasons, and
  per-dataset spec/code/execution state.
- Raw `graphState` remains as an audit/debug and compatibility fallback, but the
  UI no longer treats browser-side reconstruction from raw graph JSON as the
  primary source for "what happens next".
- Upload, prepare plan, finalize inputs, draft-spec review, code generation,
  code review, execution, and review-summary refresh now refresh graph read
  models after state-changing operations.
- Added UI contract coverage proving the page calls `/progress`, stores
  `runProgress`, and uses the graph-owned read model in the progress panel and
  review queue.

Current boundary:

- This is a UI wiring slice only. It does not change dependency planning, LLM
  generation, R execution, compare, route semantics, or page layout.
- Static-rule governance is unchanged. This slice adds no clinical,
  dataset-specific, study-specific, demo-specific, or variable-specific static
  rule.
- Per the latest user review, future static-rule work must keep the generic
  boundary: the static engine may enforce artifact/execution/spec
  declared-contract checks or admitted rule-pack items with
  authority/source/version/scope/severity/evidence. Problems observed in a
  demo, PSY201, one legacy program, one ADaM dataset, or one variable cannot be
  promoted directly into blocking static rules; they must first become a
  generic contract or a governed rule-pack/backlog/reviewer-note item.

Verification:

```text
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
```

Result: focused UI/API tests passed; full related gateway/API/static-rule
regression passed with 168 tests.

Subagent review:

- Initial review returned NO-GO: the UI surfaced graph-owned blocked reasons
  from `/progress`, but the primary action buttons could still remain
  clickable because `setButtonAvailability()` only wrote hints and
  `data-action-ready`.
- Fixed by making `setButtonAvailability()` disable buttons from the same
  `actionAvailability()` contract and adding front-door guards to finalize
  inputs, generate code, and approve/run. A blocked graph progress state now
  prevents product actions in the UI before the backend has to reject them.
- Follow-up review found that approve/run still missed the same
  `progressBlocked` predicate. `approveRun.ready` now also requires
  `!progressBlocked` and shows the graph-owned blocked reason first.
- Final review returned GO.

### 2026-05-30 - LG2.8 Explicit Legacy Stub Opt-In Slice

Completed:

- `DatasetGraph` no longer falls into the legacy fake stub chain when
  `execution_mode` is missing or unknown.
- `StudyGraph` no longer auto-fills `execution_mode="stub"` for dataset tasks.
  Callers that intentionally exercise legacy/test stub behavior must pass
  `execution_mode="stub"` explicitly.
- `graph_product_execute` remains a valid product mode: prepare initializes
  execution state and the dedicated `execute_approved_code` node still owns R
  execution.
- Smoke tests that intentionally verify legacy stub behavior now declare
  explicit stub mode. New regression tests prove that missing execution mode
  fails closed instead of producing a completed stub dataset.

Current boundary:

- This slice does not remove the legacy stub nodes; it makes them explicit
  compatibility/test behavior. Product graph modes and LLM downstream modes are
  unchanged.
- Static-rule governance is unchanged and remains first-principles. This slice
  adds no static rule. Future static checks must enter either as generic
  artifact/execution/spec contract evaluators or as governed versioned rule-pack
  items; demo, study, dataset, or variable observations cannot be promoted
  directly into blocking rules.

Verification:

```text
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_graph_smoke -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_runs_legacy_stub_to_completion_with_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_blocks_legacy_llm_run_to_completion_with_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
```

Result: compileall passed; 57 graph smoke tests passed; focused gateway
compatibility/product tests passed.

### 2026-05-30 - LG2.8 Product DatasetGraph / Legacy StubGraph Split Slice

Completed:

- `compile_dataset_graph()` now compiles the product dataset graph without the
  old synthetic `*_stub` nodes. By default it does not use its own persistent
  checkpointer; later native interrupt pilots may pass an explicit checkpointer
  for internal tests.
- Added `compile_legacy_stub_dataset_graph()` as the only compiler that includes
  the legacy/test stub chain.
- `execution_mode="stub"` is rejected by the product graph even if caller input
  tries to set internal stub flags. Stub execution can only enter through the
  explicit legacy/test graph compiler.
- `StudyGraph` dispatches to the legacy stub graph only for explicit
  `execution_mode="stub"` compatibility/test runs. Product and LLM downstream
  modes still use the product dataset graph.
- Graph smoke tests now prove:
  - product graph topology does not include legacy stub nodes;
  - the legacy stub graph contains the explicit stub chain;
  - direct product-graph `stub` requests fail closed;
  - existing explicit legacy stub compatibility behavior still works.

Current boundary:

- This slice still keeps legacy stub node functions for compatibility tests and
  old `/runs` stub behavior. It removes them from the default product graph
  topology rather than deleting all historical test scaffolding.
- Static-rule governance is unchanged. This slice adds no static rule and no
  clinical, dataset-specific, study-specific, demo-specific, or
  variable-specific check.

Verification:

```text
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_state_isolation_across_stub_runs tests.test_graph_smoke.GraphSmokeTests.test_product_dataset_graph_rejects_stub_mode_without_legacy_compiler tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes tests.test_graph_smoke.GraphSmokeTests.test_legacy_stub_dataset_graph_contains_only_explicit_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_study_graph_runs_foundation_then_downstream_stub_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
```

Result: focused graph split tests passed; compileall passed; 228 related
graph/gateway/API/static-rule tests passed.

### 2026-05-31 - LG2.8 API/CLI No Implicit Stub Entry Slice

Completed:

- Removed the remaining API entry fallback where `POST /runs` could silently
  choose `execution_mode="stub"` when the loaded run config used the mock
  provider and the request omitted `execution_mode`.
- Removed the same CLI fallback from `adam-agent run-study`; mock-provider CLI
  runs now fail closed unless the caller explicitly chooses `--execution-mode`.
- Preserved explicit legacy compatibility:
  - `POST /runs` with `execution_mode="stub"` still exercises the legacy
    compatibility/test path.
  - `--execution-mode llm_downstream_provider` with a mock config still
    exercises the configured provider boundary test path.
  - non-mock legacy `/runs` requests that omit `execution_mode` still resolve
    to LLM run-to-completion and are blocked by the split-flow gate, so they do
    not bypass review gates.
- Added API and CLI regressions proving omitted execution mode no longer
  creates a completed legacy stub run.

Current boundary:

- This slice does not change product split-flow endpoints, dependency planning,
  DatasetGraph product topology, or LLM/R sandbox behavior.
- Static-rule governance is unchanged. This slice adds no static rule and no
  clinical, dataset-specific, study-specific, demo-specific, or
  variable-specific check. Future static checks remain limited to generic
  artifact/execution/spec contracts or governed rule-pack items with explicit
  authority/source/version/scope/severity/evidence.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_requires_explicit_execution_mode_instead_of_implicit_stub tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_requires_explicit_execution_mode_with_mock_config tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_uses_configured_provider_boundary_with_mock tests.test_graph_smoke.GraphSmokeTests.test_study_graph_missing_execution_mode_fails_closed_not_completed_stub -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
python -B -c "import ast, pathlib; count=0; ...; print(f'syntax ok: {count} files')"
git diff --check -- src\adam_agent\api\service.py src\adam_agent\cli.py tests\test_api_phase8.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

Result: focused API and CLI entry tests passed; 230 related
API/graph/gateway/static-rule tests passed; AST syntax check covered 61 Python
files; diff check passed. `python -m compileall` remains blocked by the local
Windows pycache permission issue (`PermissionError` / `WinError 5`), not by a
syntax failure.

Subagent review:

- Read-only review returned GO.
- The review confirmed that API/CLI implicit stub fallback is removed, explicit
  legacy stub compatibility remains available, explicit LLM run-to-completion is
  still blocked by the split-flow gate, DatasetGraph/legacy stub graph
  separation was not weakened, and static-rule governance remains generic
  contract/rule-pack only.

### 2026-05-31 - LG2.8 Execution-Mode Entry Contract Slice

Completed:

- Added a shared execution-mode contract module for entry-point allowlists.
- `POST /runs` now rejects unknown `execution_mode` values before invoking the
  legacy graph path. The legacy endpoint allowlist is intentionally narrow:
  `stub`, `llm_downstream_provider`, and `llm_downstream_r_sandbox`.
- `adam-agent run-study` now rejects unknown `--execution-mode` values before
  compiling/invoking `StudyGraph`. Its CLI allowlist keeps explicit developer
  modes available while still rejecting arbitrary strings.
- Updated the Phase 8.1 API contract so it no longer implies that arbitrary
  modes are accepted by the legacy run endpoint.
- Added API and CLI regressions proving unknown execution modes do not create
  run directories or workflow projections.

Current boundary:

- This slice does not change DatasetGraph routing, product split-flow
  endpoints, dependency planning, provider behavior, R execution, or UI state.
- `llm_downstream_provider` and `llm_downstream_r_sandbox` are still accepted by
  `POST /runs` only so they can be blocked with `split_flow_required`; they are
  not product run-to-completion paths.
- Static-rule governance is unchanged. This slice adds no clinical,
  dataset-specific, study-specific, demo-specific, or variable-specific rule.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_unknown_execution_mode_before_legacy_graph tests.test_api_phase8.Phase8ApiTests.test_create_run_requires_explicit_execution_mode_instead_of_implicit_stub tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_rejects_unknown_execution_mode_before_graph tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_requires_explicit_execution_mode_with_mock_config tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_uses_configured_provider_boundary_with_mock tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_can_execute_llm_downstream_r_sandbox -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
git diff --check -- src\adam_agent\graph\execution_modes.py src\adam_agent\api\service.py src\adam_agent\cli.py tests\test_api_phase8.py tests\test_graph_smoke.py docs\phase8_1_api_contract.md docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

Result: focused API and CLI entry tests passed; 232 related
API/graph/gateway/static-rule tests passed; diff check passed.

Subagent review:

- Read-only review returned GO.
- The review confirmed that unknown API/CLI modes are rejected before graph
  invocation, omitted mode remains fail-closed, explicit legacy `stub`
  compatibility remains available, explicit LLM `/runs` requests remain blocked
  by the split-flow gate, CLI developer modes still work, Product Graph /
  legacy stub boundaries were not weakened, and static-rule governance was
  untouched.

### 2026-05-31 - LG2.8 Graph Execution-Mode Constant Boundary Slice

Completed:

- Extended `src/adam_agent/graph/execution_modes.py` from API/CLI entry
  allowlists into the shared source of truth for graph execution-mode names.
- Replaced graph-internal product/downstream/stub mode branch checks in
  DatasetGraph, routing helpers, StudyGraph dispatch, GraphGateway product
  invocations, and demo-study defaults with shared constants.
- Updated the non-legacy route guard regression so its downstream-mode cases
  come from the shared mode set instead of a copied test-only string list.

Current boundary:

- This slice is behavior-preserving. It does not change product routing,
  dependency planning, LLM generation, R execution, compare, UI state, or static
  rule semantics.
- Remaining string keys such as metadata fields (`"stub"` or
  `"graph_product_execute"`) are audit labels, not execution-mode routing
  decisions.
- Static-rule governance is unchanged. This slice adds no clinical,
  dataset-specific, study-specific, demo-specific, or variable-specific rule.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_non_legacy_modes_never_route_to_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_unknown_execution_mode_fails_closed_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_graph_product_execute_prepare_does_not_run_r_before_execute_node tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_demo_study_endpoint_prepares_shiny_demo_shape tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_unknown_execution_mode_before_legacy_graph tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_rejects_unknown_execution_mode_before_graph -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\execution_modes.py src\adam_agent\graph\dataset_graph.py src\adam_agent\graph\routing.py src\adam_agent\graph\study_graph.py src\adam_agent\graph\gateway.py src\adam_agent\api\service.py tests\test_graph_smoke.py
```

Result: focused graph/API checks passed; 232 related
API/graph/gateway/static-rule tests passed; AST syntax check covered 79 Python
files; diff check passed.

Subagent review:

- Read-only review returned GO.
- The review confirmed that the constant sets cover the previous mode strings,
  DatasetGraph behavior remains equivalent and fail-closed, Product Graph /
  legacy stub graph separation was not weakened, remaining literals are test
  examples or audit metadata labels rather than routing decisions, and
  static-rule governance was untouched.

### 2026-05-31 - LG2.8 StudyGraph Execution-Mode Preflight Slice

Completed:

- Added a StudyGraph-level execution-mode preflight for explicit unsupported
  modes before runnable dataset tasks are dispatched.
- Kept dependency planning and plan-only gateway behavior unchanged. Missing
  execution mode is still handled by existing lower-level fail-closed paths or
  by planning-only tests that intentionally do not execute dataset tasks.
- Added a smoke regression proving an explicit unknown StudyGraph mode does not
  invoke dataset subgraphs.

Current boundary:

- This slice does not change API entry validation, dependency semantics,
  product split-flow endpoints, LLM generation, R execution, compare, UI state,
  or static-rule governance.
- Unsupported datasets and unresolved dependencies keep their original business
  failure reasons; they are not overwritten by the execution-mode preflight.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_unknown_execution_mode_fails_before_dataset_dispatch tests.test_graph_smoke.GraphSmokeTests.test_study_graph_missing_execution_mode_fails_closed_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_non_ad_target_is_blocked_as_unsupported_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_reference_sas7bdat_dependency_artifact_does_not_satisfy_runtime_dependency tests.test_graph_smoke.GraphSmokeTests.test_run_output_dependency_artifact_wins_over_reference_adam tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions tests.test_graph_smoke.GraphSmokeTests.test_study_graph_writes_dependency_plan_review_artifacts tests.test_graph_smoke.GraphSmokeTests.test_dependency_review_artifacts_include_conflict_warning tests.test_graph_smoke.GraphSmokeTests.test_terminal_failure_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\execution_modes.py src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py
```

Result: focused StudyGraph preflight and dependency-regression tests passed;
233 related API/graph/gateway/static-rule tests passed; AST syntax check
covered 79 Python files; diff check passed.

Subagent review:

- Read-only review returned GO.
- The review confirmed that the StudyGraph mode set covers current allowed
  study/product entries without reviving retired ADSL template mode, the
  preflight condition is narrow enough to preserve plan-only, unsupported, and
  dependency-blocked outcomes, explicit unknown mode does not dispatch dataset
  subgraphs, and static-rule governance was untouched.

### 2026-05-31 - LG2.8 StudyGraph Audit Node Naming Slice

Completed:

- Renamed the StudyGraph final audit node from `write_audit_manifest_stub` to
  `write_audit_manifest`.
- Kept audit manifest behavior unchanged. The node already writes a real
  study-level audit manifest when a study directory is available and records
  `stub: false` in manifest metadata.
- Added a topology regression proving the product StudyGraph exposes no
  `*_stub` node names and still routes `reduce_dataset_results` into the real
  audit manifest node.

Current boundary:

- This is a compatibility/deprecation cleanup only. It does not change
  dependency planning, dataset dispatch, LLM generation, R execution, compare,
  UI state, or static-rule governance.
- This slice intentionally renames a StudyGraph node id. Existing historical
  checkpoints paused exactly at the old final audit node are not part of the
  compatibility promise for this cleanup branch.
- Historical docs may still mention old Phase 3 stub node names as history;
  current product StudyGraph topology must not expose them.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_uses_real_audit_manifest_node_name tests.test_graph_smoke.GraphSmokeTests.test_study_graph_runs_foundation_then_downstream_stub_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

Result: focused StudyGraph topology and smoke tests passed; 234 related
graph/gateway/API/static-rule tests passed; AST syntax check covered 79 Python
files; diff check passed. `python -m compileall -q src\adam_agent` was also
attempted, but this workspace currently blocks pyc writes in existing
`__pycache__` paths with `PermissionError`, so AST parsing was used as the
write-free syntax check.

Subagent review:

- Read-only review returned GO.
- The review confirmed that the change is a node/function rename around the
  same audit-manifest logic, product StudyGraph topology no longer exposes
  `*_stub` node names, DatasetGraph legacy stub coverage is not affected, and
  static-rule governance was untouched.

### 2026-05-31 - LG2.8 Generic Dependency Status Label Slice

Completed:

- Replaced the StudyGraph task label `depends_on_adsl` with the generic
  `depends_on_upstream_adam`.
- Removed the ADSL-specific branch from `_dependency_status()`. Any dataset
  with upstream ADaM dependencies now receives the same generic status label.
- Added a smoke regression proving both an ADSL dependency and an ADLB
  dependency produce `depends_on_upstream_adam`, not an ADSL-special case.

Current boundary:

- This is a task-label cleanup only. It does not change dependency planning,
  execution batches, dependency approval semantics, DatasetGraph routing, LLM
  generation, R execution, compare, UI state, or static-rule governance.
- `ADSL` remains a normal ADaM dataset when evidence says a target depends on
  it; it is not treated as a product template path.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_dependency_status_is_generic_for_upstream_adam tests.test_graph_smoke.GraphSmokeTests.test_midstream_dependency_failure_blocks_only_dependent_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "depends_on_adsl|depends_on_adam" src\adam_agent tests --glob "*.py"
```

Result: focused dependency-status regression passed; 235 related
graph/gateway/API/static-rule tests passed; AST syntax check covered 79 Python
files; diff check passed. Source scan found no runtime use of the retired
`depends_on_adsl` / `depends_on_adam` labels; the only remaining Python hit is
the regression assertion that the old ADSL-specific label is absent.

Subagent review:

- Read-only review returned GO.
- The review confirmed that dependency scheduling is still controlled by
  `dataset_dependencies` and `execution_batches`, not by the task label; the new
  test is not order-sensitive under ThreadPool execution; DatasetGraph legacy
  stub behavior and static-rule governance were untouched.

### 2026-05-31 - LG2.8 Legacy Stub Sandbox Failure Scenario Naming Slice

Completed:

- Added the dataset-neutral legacy stub scenario name `sandbox_failure`.
- Kept the old `fail_adsl` string as an explicit compatibility alias for
  existing stub tests or historical harnesses.
- Updated current graph smoke tests to use `sandbox_failure` when simulating a
  sandbox failure for ADSL, ADAE, or ADLB.
- Added focused regression coverage proving the generic scenario works for
  multiple ADaM datasets and that the old `fail_adsl` alias still fails in the
  same controlled way.

Current boundary:

- This is a legacy/test stub naming cleanup only. It does not change product
  DatasetGraph routing, StudyGraph dependency semantics, LLM generation, R
  execution, compare, UI state, or static-rule governance.
- The product path must not use `fail_adsl` as business language. The alias is
  retained only so old stub harnesses fail compatibly instead of breaking with
  an unrelated input error.
- No dataset-specific clinical or static-rule behavior was added.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_legacy_stub_sandbox_failure_scenario_is_dataset_neutral tests.test_graph_smoke.GraphSmokeTests.test_legacy_fail_adsl_stub_scenario_remains_alias tests.test_graph_smoke.GraphSmokeTests.test_adsl_failure_blocks_downstream_without_running_it tests.test_graph_smoke.GraphSmokeTests.test_downstream_stub_failure_does_not_change_completed_adsl_status tests.test_graph_smoke.GraphSmokeTests.test_midstream_dependency_failure_blocks_only_dependent_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\state.py src\adam_agent\graph\dataset_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "fail_adsl|sandbox_failure" src\adam_agent tests docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

Result: focused legacy stub naming regressions passed; 237 related
graph/gateway/API/static-rule tests passed; AST syntax check covered 79 Python
files; diff check passed. Source scan found `fail_adsl` only in the explicit
legacy alias path, alias regression, and documentation note; current failure
scenario tests use the dataset-neutral `sandbox_failure` name.

Subagent review:

- Read-only review returned GO.
- The review confirmed that `fail_adsl` remains only as the explicit legacy
  stub alias and type literal, the dataset-neutral `sandbox_failure` path is
  covered for multiple datasets, the alias is covered separately, and no
  dependency-planning or static-rule semantic drift was introduced.

### 2026-05-31 - LG2.8 Study Audit Virtual Artifact Naming Slice

Completed:

- Removed the residual `*_study_stub` artifact id from StudyGraph's
  no-`study_dir` audit manifest reference.
- Replaced that fallback id with `*_study_virtual`, because the artifact is a
  graph-state reference that was not materialized to disk, not a fake/stub study
  product.
- Kept `metadata["stub"] = False` for study audit manifests and added
  `manifest_materialized` so callers can distinguish a written manifest from a
  virtual reference without using stub language.
- Replaced the no-`study_dir` audit-agent summary metadata flag
  `stub: true` with `materialized: false`.
- Added a graph smoke regression proving virtual study audit refs are not marked
  with stub metadata.

Current boundary:

- This is an audit metadata and naming cleanup only. It does not change
  StudyGraph planning, dataset dispatch, DatasetGraph product routing, legacy
  stub graph behavior, LLM generation, R execution, compare, UI state, or
  static-rule governance.
- Dataset-level legacy stub audit artifacts remain explicitly stub-labeled
  inside `compile_legacy_stub_dataset_graph()`; this slice only removes
  misleading study-level fallback language.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_virtual_audit_refs_are_not_stub_metadata tests.test_graph_smoke.GraphSmokeTests.test_study_graph_uses_real_audit_manifest_node_name tests.test_graph_smoke.GraphSmokeTests.test_study_graph_runs_foundation_then_downstream_stub_datasets tests.test_graph_smoke.GraphSmokeTests.test_study_graph_writes_dependency_plan_review_artifacts -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "study_stub|study_virtual|manifest_materialized|materialized: false|materialized\": false" src\adam_agent tests docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

Result: focused StudyGraph audit metadata regressions passed; 238 related
graph/gateway/API/static-rule tests passed; AST syntax check covered 79 Python
files; diff check passed. Source scan found no runtime `study_stub` artifact id
use; the remaining `study_stub` hits are documentation notes and the regression
assertion that the old suffix is absent.

Subagent review:

- Read-only review returned GO.
- The review confirmed that the no-`study_dir` StudyGraph manifest now uses the
  virtual artifact id and materialization metadata, the audit-agent summary no
  longer uses stub metadata for a virtual reference, legacy DatasetGraph stub
  audit metadata remains intact, and no static-rule or product-routing drift was
  introduced.

### 2026-05-31 - LG2.8 Dataset-Neutral Stub Scenario Defaults Slice

Completed:

- Removed the remaining StudyGraph default stub scenario special case for
  `ADAE`.
- StudyGraph now gives every runnable dataset the same default legacy stub
  scenario, `success`.
- Tests that need repair/failure simulation must pass `stub_scenarios`
  explicitly.
- Added regressions proving ADAE and ADLB both get zero implicit repair attempts
  under default stub scenarios, while an explicit `code_error_then_success`
  scenario still exercises the legacy repair path.

Current boundary:

- This is a legacy/test stub default cleanup only. It does not change dependency
  planning, product DatasetGraph routing, LLM generation, R execution, compare,
  UI state, or static-rule governance.
- The explicit DatasetGraph `code_error_then_success` scenario remains available
  for testing repair routing. It is no longer hidden as a StudyGraph ADAE
  default.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_default_stub_scenarios_are_dataset_neutral tests.test_graph_smoke.GraphSmokeTests.test_study_graph_stub_repair_requires_explicit_scenario -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "code_error_then_success\" if dataset == \"ADAE\"|dataset == \"ADAE\"" src\adam_agent\graph tests\test_graph_smoke.py
```

Result: focused default-scenario regressions passed; 240 related
graph/gateway/API/static-rule tests passed; AST syntax check covered 79 Python
files; diff check passed. Source scan found no remaining `ADAE` special-case
default stub repair scenario in graph runtime code.

Subagent review:

- Read-only review returned GO.
- The review confirmed that StudyGraph now uses the same default stub scenario
  for downstream and foundation tasks, explicit repair simulation remains
  available through `stub_scenarios`, the new tests cover both paths, and no
  dependency-planning, product-routing, or static-rule governance drift was
  introduced.

### 2026-05-31 - LG2.8 StudyGraph Non-Execution Compare Status Slice

Completed:

- Changed StudyGraph-level non-execution failures from `compare_status:
  not_run_stub` to `compare_status: not_run`.
- Covered unsupported targets, unresolved/blocked dependency results, downstream
  blocked-by-dependency summaries, and StudyGraph execution-mode preflight
  failures.
- Added regression assertions proving those StudyGraph paths no longer report a
  stub compare status.

Current boundary:

- This is a status-label cleanup only. It does not change dependency planning,
  dataset dispatch, DatasetGraph product routing, legacy stub graph behavior,
  LLM generation, R execution, compare implementation, UI state, or static-rule
  governance.
- DatasetGraph legacy stub summaries still use `not_run_stub` and `passed_stub`
  inside the explicit legacy/test compiler path.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_state_isolation_across_stub_runs tests.test_graph_smoke.GraphSmokeTests.test_legacy_stub_sandbox_failure_scenario_is_dataset_neutral tests.test_graph_smoke.GraphSmokeTests.test_reference_sas7bdat_dependency_artifact_does_not_satisfy_runtime_dependency tests.test_graph_smoke.GraphSmokeTests.test_non_ad_target_is_blocked_as_unsupported_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_study_graph_unknown_execution_mode_fails_before_dataset_dispatch tests.test_graph_smoke.GraphSmokeTests.test_midstream_dependency_failure_blocks_only_dependent_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "compare_status=\"not_run_stub\"|not_run_stub" src\adam_agent\graph tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

Result: focused StudyGraph non-execution compare-status regressions passed,
including direct dependency-preflight blocking and the explicit legacy stub
boundary; 240 related graph/gateway/API/static-rule tests passed; AST syntax
check covered 79 Python files; diff check passed. Source scan found
`not_run_stub` remaining only in DatasetGraph legacy stub runtime code, legacy
test fixtures, and this documentation note.

Subagent review:

- GO. No major business or architecture regression found. The review confirmed
  DatasetGraph legacy stub behavior is unchanged, StudyGraph changes are
  label-only for non-execution/no-output cases, and real execution/compare
  failures remain owned by DatasetGraph/Product/Gateway paths.

### 2026-05-31 - LG2.8 Stub Dependency Artifact Guard Slice

Completed:

- Tightened run-output dependency availability so a graph-state-backed
  `completed_stub` output cannot silently satisfy a downstream ADaM runtime
  dependency.
- The guard also blocks run outputs whose graph execution state says
  `completed_stub`, `structural_stub_pass`, or `stubbed_r_execution: true`.
- Added a regression proving an upstream `ADSL` output with a structural stub
  graph state leaves downstream `ADAE` blocked as `found_but_unusable`.
- Kept normal completed run outputs usable when graph state records a real
  output artifact and a non-terminal execution state.

Current boundary:

- This is a dependency-quality signal guard only. It does not change mock code
  generation, legacy stub graph execution, product DatasetGraph routing, UI
  state, compare, R execution, or static-rule governance.
- The local mock/demo flow may still produce `completed_stub` outputs for UI
  smoke testing. Such outputs remain visible as demo artifacts, but they no
  longer unlock downstream runtime dependencies.
- This slice does not block every `not_real_derivation` result. Mock provider
  plus real R execution remains a separate policy question.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_run_output_dependency_artifact_wins_over_reference_adam tests.test_graph_smoke.GraphSmokeTests.test_unbacked_run_output_dependency_is_not_usable tests.test_graph_smoke.GraphSmokeTests.test_terminal_failure_run_output_dependency_does_not_satisfy_downstream tests.test_graph_smoke.GraphSmokeTests.test_completed_stub_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
```

Result: focused dependency-availability regressions passed; 241 related
graph/gateway/API/static-rule tests passed; AST syntax check covered 79 Python
files; diff check passed.

Subagent review:

- GO. No blocking findings. The review confirmed that the guard is narrow to
  current-run `run_output` dependency candidates, preserves the rule that a
  present run output takes precedence over reference ADaM, does not affect
  reference ADaM visibility or UI/demo progress paths, and does not block every
  `not_real_derivation` case.
- Follow-up from the review was addressed in the same slice by splitting the
  unusable-run-output reason text into more precise messages for terminal
  failures, missing/corrupt graph state, mismatched output paths, and structural
  stub outputs.

### 2026-05-31 - LG2.8 Not-Real Derivation Dependency Guard Slice

Completed:

- Added a code-generation quality record to GraphGateway-owned `code_state`.
  It records provider/model metadata and marks mock-provider code as
  `not_real_derivation`.
- Propagated that quality record into `execution_state` after approved-code
  execution, so the dependency resolver can make decisions from canonical graph
  state instead of UI assumptions.
- Tightened run-output dependency availability so a completed output marked
  `not_real_derivation` cannot silently satisfy a downstream runtime ADaM
  dependency.
- Added regressions proving that a completed ADSL run output with
  `generation_quality.not_real_derivation: true` leaves ADAE blocked as
  `found_but_unusable`, and that mock code generation records the quality
  signal in graph state.

Current boundary:

- This is a dependency-quality guard, not a judgment that all mock-assisted work
  is useless. Mock-generated artifacts remain visible for UI smoke testing and
  local review, but they no longer unlock downstream runtime dependencies.
- Real provider output is not marked `not_real_derivation` by this slice. Its
  clinical quality still depends on approved specs, human review, static checks,
  local R execution, validation, and future stronger rules.
- Pre-existing runs created before this quality field existed are not
  retroactively classified. Regenerate or migrate them before using their
  outputs as downstream runtime evidence.
- This does not change provider calls, R execution, UI state, compare, or static
  rule governance.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_generation_quality_marks_only_mock_signals_as_not_real tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_execution_preserves_generation_quality_signal tests.test_graph_smoke.GraphSmokeTests.test_not_real_derivation_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_not_real_derivation_run_output_dependency_does_not_satisfy_downstream tests.test_graph_smoke.GraphSmokeTests.test_completed_stub_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
```

### 2026-05-31 - LG2.8 Output Quality Read Model Slice

Completed:

- Added a shared output-quality read model for UI/API display. It classifies
  generated outputs as `real_runtime_output`, `not_real_derivation`,
  `structural_stub`, `terminal_failure`, or `not_completed`.
- Exposed that signal through GraphGateway progress items and review-summary
  dataset reviews.
- Added user-facing warnings so mock/offline and structural demo outputs remain
  visible for review, but are clearly marked as not downstream runtime evidence.
- Updated the browser UI to show a warning banner on result review and to label
  dataset cards as `review only` or `demo output` when graph state says so.

Current boundary:

- This is a read-model and UI clarity slice. It does not change LLM calls,
  R execution, dependency resolution, compare, or static-rule behavior.
- The review-summary path reads quality details from the existing
  `workflow_state.json` compatibility projection. It does not load or mutate
  canonical graph state.
- The quality signal is not a clinical correctness score. It only explains
  whether an output is fit to be treated as downstream runtime evidence.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_not_real_outputs_as_review_only tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_recovers_multiple_outputs_from_same_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_read_model_helpers_do_not_record_compare -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_output_quality_classification_matrix tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_not_real_outputs_as_review_only tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_output_is_not_previewed_or_downloadable tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway tests.test_graph_smoke tests.test_static_rules -v
git diff --check
```

Result: focused progress/review-summary and quality-matrix tests passed; 247
related API/gateway/graph/static-rule tests passed; AST syntax check covered 80
Python files; diff check passed.

Subagent review:

- GO. No blocking findings.
- The review confirmed this slice stays in read-model/UI display code and does
  not mutate dependency resolution, R execution, compare, or canonical graph
  state.
- The suggested minimal fix was applied: `dataset_output_quality()` now also
  reads terminal-failure and partial-output-usability flags from validation
  reports, and the classification matrix plus terminal-failure review-summary
  regression cover that path.

### 2026-05-31 - LG2.8 Study Output Quality Rollup Slice

Completed:

- Added a study-level output-quality rollup read model on top of dataset
  `output_quality` signals.
- Exposed `output_quality_rollup` through the run progress API response.
- Changed study-level next-action wording so a run whose planned targets only
  have review-only/demo outputs is not described as real completion.
- Changed mixed completion wording so real runtime outputs and review-only/demo
  outputs are visible as different quality classes.
- Updated dataset-level completed labels so review-only/demo outputs do not use
  the same user-facing text as real runtime outputs.
- Updated the browser progress header to show `review only` or `mixed output`
  when the run-level rollup says so.

Current boundary:

- This is still a read-model and UI clarity slice. It does not mutate canonical
  graph state, dependency planning, dependency resolution, LLM generation,
  R execution, compare, or static-rule behavior.
- The underlying dataset status can remain `completed` for review/demo
  artifacts. The new rollup explains whether those outputs are real runtime
  evidence.
- This does not add a clinical correctness score. It only separates real
  runtime outputs from outputs that are visible for review but cannot satisfy
  downstream runtime dependencies.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_not_real_outputs_as_review_only tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_mixed_completion_quality tests.test_graph_gateway.GraphGatewayTests.test_study_output_quality_rollup_distinguishes_review_only_completion -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

Result: focused rollup/API/UI regressions passed; the broader
graph-gateway/API suite passed with 151 tests; AST syntax check covered 80
Python files; `git diff --check` reported only CRLF line-ending warnings.

Subagent review:

- GO. No blocking findings.
- The review confirmed that output-quality rollup is pure read-model logic,
  that stale-plan/dependency-review/interrupt priority remains ahead of
  completed-quality wording, and that API/UI changes reduce rather than create
  user-facing ambiguity.

### 2026-05-31 - LG2.8 Review Summary Graph-State-First Slice

Completed:

- Changed `/review-summary` to prefer canonical `runs/{run_id}/graph_state.json`
  for dataset status, code state, execution state, validation summary, compare
  status, output quality, and warnings.
- Kept `workflow_state.json` as a compatibility fallback only when canonical
  graph state cannot be loaded.
- Added graph-state dataset discovery so review summaries include datasets
  known to the graph even when manifest/workflow projections are missing.
- Restricted graph-recorded output artifact paths to the current `run_dir`;
  absolute paths and `..` escapes outside the run are ignored.
- Preserved reference ADaM as preview/compare evidence only. It still does not
  decide derivation logic or output-quality eligibility.

Current boundary:

- This is a read-model alignment slice. It does not mutate canonical graph
  state, workflow projections, dependency plans, dependency resolution, LLM
  generation, R execution, compare reports, or static-rule behavior.
- Disk validation reports remain usable for legacy/fallback reads, but when
  canonical graph state is available, graph validation wins over stale files.
- This does not remove the compatibility projection yet; it only stops
  `/review-summary` from treating that projection as the primary source.

Verification:

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

Result: the broader graph-gateway/API suite passed with 155 tests; AST syntax
check covered 80 Python files; `git diff --check` reported only CRLF
line-ending warnings.

Subagent review:

- First review: NO-GO. It found that stale disk validation reports could still
  override canonical graph validation in output-quality classification.
- Fix applied: review summary now builds `review_validation` with graph
  validation first, and uses it consistently for output quality, validation
  status/report, warnings, and errors.
- Final review: GO. No blocking findings.

### 2026-05-31 - LG2.8 Review Summary Source Metadata Slice

Completed:

- Added explicit read-model source metadata to `RunReviewSummary`:
  `read_model_source`, `graph_state_path`, and `workflow_state_path`.
- Made run-level `study_id` and `status` follow the same graph-state-first rule
  as dataset reviews.
- Split fallback labels into:
  - `graph_state` when canonical graph state is loaded.
  - `workflow_state_fallback` when graph state is unavailable but the legacy
    projection is available.
  - `artifact_fallback` when the read model is reconstructed only from run
    artifacts such as outputs, validation files, and manifests.

Current boundary:

- This is source visibility for a read model only. It does not mutate graph
  state, workflow projections, dependency resolution, LLM generation,
  R execution, compare, or static-rule behavior.
- The fields are intended to remove UI/API ambiguity while legacy fallback
  remains available.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_review_summary_recovers_multiple_outputs_from_same_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_state_without_workflow_projection -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

Result: focused source-metadata tests passed; broader graph-gateway/API suite
passed with 155 tests; AST syntax check covered 80 Python files; `git diff
--check` reported only CRLF line-ending warnings.

Subagent review:

- First review: GO, with a non-blocking suggestion to distinguish artifact-only
  fallback from workflow fallback.
- Fix applied: `artifact_fallback` is now explicit and covered by regression.
- Final review: GO.

### 2026-05-31 - LG2.8 Review Summary Source Advanced UI Slice

Completed:

- Wired review-summary source metadata into the browser Advanced/Audit panel:
  `review_summary_source`, `graph_state`, and `workflow_state`.
- Kept technical paths out of the main workflow panes. Draft-spec notices,
  result summaries, and action areas still refer users to Advanced settings and
  audit files instead of printing paths inline.
- Updated UI regression tests so source/path metadata appears in Advanced while
  draft-spec and result-facing areas keep hiding direct technical paths.

Current boundary:

- This is UI read-model display only. It does not change API state transitions,
  dependency planning/resolution, LLM generation, R execution, compare, or
  static-rule behavior.
- The purpose is audit clarity: users can tell whether `/review-summary` was
  read from canonical graph state, workflow fallback, or artifact fallback
  without cluttering the normal product flow.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_state_without_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_recovers_multiple_outputs_from_same_run -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

Result: focused UI/review-summary tests passed; broader graph-gateway/API suite
passed with 155 tests; AST syntax check covered 80 Python files; `git diff
--check` reported only CRLF line-ending warnings.

Subagent review:

- GO. No blocking findings.
- The review confirmed this slice only changes Advanced UI display, keeps
  technical paths out of the main product panes, and adds no backend state
  writes or workflow behavior.

### 2026-05-31 - LG2.8 Target Planning vs Active Detail UI Slice

Completed:

- Added a browser UI summary that separates the ADaM datasets selected for
  joint dependency planning from the single active dataset shown in the
  review/code/result panels.
- Added per-dataset card context labels so users can see whether a dataset is
  planned in the current run or only visible as history/candidate context.
- Updated UI contract tests to lock the distinction between planned targets and
  active detail target.

Current boundary:

- This is a UI clarity slice only. It does not change canonical graph state,
  dependency planning, dependency resolution, LLM generation, R execution,
  compare, or static-rule behavior.
- Code generation and R execution remain one active dataset at a time. The
  multi-target selection only controls dependency planning and dashboard
  context for now.
- No new clinical or demo-specific static rule is introduced.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

Result: focused UI contract tests passed; broader graph-gateway/API suite
passed with 155 tests; AST syntax check covered 80 Python files; `git diff
--check` reported only CRLF line-ending warnings.

### 2026-05-31 - LG2.8 Dependency Map Readability UI Slice

Completed:

- Reworked the browser Dependency Map from a numbered list into a user-facing
  explanation card for each target:
  - source evidence recognized from uploaded SDTM files
  - dependency decision from the graph plan
  - runtime meaning of the target status
  - direct next action
- Kept the reference ADaM warning in the dependency flow so uploaded reference
  outputs remain comparison/output-shape evidence only, not derivation authority
  or runtime dependency evidence.
- Updated UI contract tests to check the new readable dependency-map structure
  and to keep guarding against treating reference ADaM as runtime dependency.

Current boundary:

- This is a browser UI presentation slice only. It does not change dependency
  planning, dependency resolution, canonical graph state, LLM generation,
  R execution, compare, or static-rule behavior.
- The map still reflects the existing graph read models. It does not add new
  clinical rules or infer new dependencies.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

### 2026-05-31 - LG2.8 Top Status Readability UI Slice

Completed:

- Reworked the browser header status area into a compact current-status read
  model:
  - current operation;
  - loaded study;
  - active detail target;
  - next action from the graph progress/read model;
  - visible operation progress state for running, done, and failed operations.
- Wired the header overview to refresh from existing UI/graph state after:
  - API health checks;
  - study-progress rendering;
  - target rendering or target selection changes;
  - initial page setup.
- Added UI contract coverage so future changes keep the top status fields and
  progress hooks present.

Current boundary:

- This is a browser UI read-model slice only.
- It does not change canonical graph state, dependency planning, dependency
  resolution, LLM generation, R execution, compare, or static-rule behavior.
- It does not add clinical rules, demo-specific rules, or new blocking
  validation.
- The header reads from existing state (`runProgress`, selected target, plan,
  and input summary); it does not become a second workflow state machine.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check -- docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md src\adam_agent\api\web.py tests\test_api_phase8.py
```

### 2026-05-31 - LG2.4 Agent Node IO Contract Slice

Completed:

- Extended the bounded agent contract layer with explicit node input/output
  packages:
  - `AgentNodeInput`
  - `AgentNodeOutput`
  - `build_agent_node_input()`
  - `build_agent_node_output()`
- The input package makes the agent boundary explicit: agent role, graph node,
  study/run id, optional dataset, task text, declared inputs, artifact ids,
  risk flags, evidence bundle id, and reference query ids.
- The output package makes the returned boundary explicit: agent role, graph
  node, study/run id, optional dataset, status, decision, reason, declared
  outputs, artifact ids, risk flags, and matching audit decisions.
- Added validation so an output cannot quietly include audit decisions from a
  different agent, node, or dataset.
- Tightened the audit-decision timestamp contract to UTC `Z` timestamps and
  normalized decision datasets to uppercase.
- Exported the new contracts from `adam_agent.agents`.
- Added focused tests for JSON-safe input packages, output packages, automatic
  matching audit-decision creation, timestamp validation, dataset
  normalization, and cross-agent/cross-node/cross-dataset rejection.

Current boundary:

- This is a contract slice for future multi-agent graph nodes.
- It does not add new autonomous behavior, clinical rules, dependency
  inference, LLM prompts, R execution behavior, compare behavior, or static
  rule behavior.
- Existing graph/product paths continue to use the prior `AgentDecision`
  records. Future slices can migrate concrete nodes to these IO packages one
  role at a time.
- Reference ADaM remains compare/output-shape evidence only. No derivation
  authority is added here.

Verification:

```text
python -B -m unittest tests.test_agents_contract -v
python -B -m unittest tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/agents/contracts.py'), pathlib.Path('src/adam_agent/agents/__init__.py'), pathlib.Path('tests/test_agents_contract.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Evidence Agent IO Migration Slice

Completed:

- Migrated the existing DatasetGraph `prepare_product_context` evidence-agent
  node to emit the new typed agent IO packages:
  - `AgentNodeInput`
  - `AgentNodeOutput`
- The evidence-agent input records the explicit task, execution mode,
  dependency-resolution count, forced draft-spec flag, risk flags, evidence
  bundle id, and reference query ids.
- The evidence-agent output now wraps the existing evidence decisions:
  - `input_spec_ready`
  - `approved_draft_spec_ready`
  - `draft_spec_required`
- Existing `agent_decisions` are now derived from the typed evidence-agent
  output for this node, instead of being hand-built separately.
- Dataset summaries expose `agent_node_inputs` and `agent_node_outputs` in
  metadata, and StudyGraph batch execution carries these packages into the
  study audit manifest metadata.
- Added focused smoke tests proving both the direct DatasetGraph path and the
  StudyGraph batch path preserve the evidence-agent IO package.

Current boundary:

- This slice migrates only the evidence/product-context node.
- It does not change dependency planning, spec generation, code generation,
  code review, R execution, compare, static rules, or repair routing.
- The IO packages are audit/read-model data. They do not unlock workflow gates
  and do not become a second state machine.
- Later slices can migrate spec/code/static/execution/validation/repair nodes
  one at a time.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_uses_input_spec_without_stub_code tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('src/adam_agent/graph/study_graph.py'), pathlib.Path('src/adam_agent/graph/state.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Draft Spec Agent IO Migration Slice

Completed:

- Migrated the existing DatasetGraph `draft_spec_agent` success path to emit
  typed agent IO packages:
  - `AgentNodeInput`
  - `AgentNodeOutput`
- The spec-agent input records the explicit task, current spec source, prepared
  context keys, warning count, context artifact id, risk flags, evidence bundle
  id, and reference query ids.
- The spec-agent output wraps the existing `draft_spec_generated` decision and
  exposes the draft-spec path, variable count, next action, risk flag, and
  prompt/response/spec artifact ids.
- Existing `agent_decisions` for this node now come from the typed
  `AgentNodeOutput`, avoiding parallel hand-built audit records.
- Added focused smoke assertions proving the no-input-spec path now carries the
  `spec_agent` IO package through the DatasetGraph result and summary metadata.

Current boundary:

- This slice changes only the successful draft-spec generation audit packaging.
- It does not change evidence preparation, dependency planning, input-spec
  authority, code generation, R execution, compare, static rules, repair, UI, or
  provider behavior.
- The draft spec remains review-required and is not treated as approved until
  the graph approval gate records it.
- Reference ADaM remains compare/output-shape evidence only, not derivation
  authority.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Code And Static Review Agent IO Migration Slice

Completed:

- Migrated the successful DatasetGraph `generate_r_code_agent` path to emit
  typed IO packages for two bounded roles:
  - `code_agent`
  - `static_review_agent`
- The code-agent input records the approved spec source, prepared context keys,
  included datasets, variable-count summary, product-context artifact id, risk
  flags, evidence bundle id, and reference query ids.
- The code-agent output wraps the existing `r_code_generated` audit decision
  and records generated-code artifact ids plus the next human action.
- The static-review input records the generated-code artifact id, required
  identifier count, required identifier source id, and its limited-check scope.
- The static-review output wraps the existing
  `static_check_passed_for_review` warning decision and records the static-check
  artifact id.
- Added focused smoke assertions proving direct DatasetGraph code generation
  and StudyGraph batch execution retain both code/static IO packages.

Current boundary:

- This slice changes only success-path audit packaging for code generation and
  limited static review.
- It does not change prompt construction, provider calls, approved-spec gates,
  generated R code parsing, static-rule semantics, code review, R execution,
  compare, repair, dependency planning, or UI behavior.
- Static review remains explicitly limited scope. No clinical, dataset-specific,
  study-specific, demo-specific, or variable-specific static rule was added.
- Reference ADaM remains compare/output-shape evidence only, not derivation
  authority.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_generate_code_uses_input_spec_and_stops_for_review tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Execution Agent IO Migration Slice

Completed:

- Migrated the DatasetGraph `execute_approved_code` result packaging to emit
  typed IO packages for `execution_agent`.
- The execution-agent input records the explicit execution task, code path,
  static-check path, whether an Rscript path was provided, and available
  upstream artifact ids.
- The execution-agent output wraps the existing execution audit decision:
  - `r_execution_completed`
  - `r_execution_terminal_failure`
- The output records validation status, terminal-failure status, output path,
  risk flags, and execution/validation/failure artifact ids.
- Added focused smoke tests for both successful execution and terminal-failure
  execution using a patched execution boundary, so the tests validate graph
  state packaging without depending on local R availability.

Current boundary:

- This slice changes only result audit packaging after the existing execution
  boundary returns.
- It does not change code-review validation, stale-input checks, static-rule
  validation, R sandbox behavior, output validation, terminal-failure routing,
  repair policy, compare, dependency planning, provider calls, or UI behavior.
- This is not a sandbox-hardening slice. Stronger OS/container isolation remains
  separate production work.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_execute_records_execution_agent_io tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_execute_records_terminal_failure_agent_io -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Audit Agent IO Migration Slice

Completed:

- Migrated the StudyGraph `write_audit_manifest` audit-summary step to emit
  typed IO packages for `audit_agent`.
- The audit-agent input records study status, target datasets, dataset-result
  count, agent-decision count, risk-flag count, and upstream audit artifact ids.
- The audit-agent output wraps a new study-level
  `agent_audit_summary_written` decision and records the agent-summary artifact
  id, summary type, decision count, and dataset count.
- The final study manifest metadata now includes the audit-agent IO package and
  audit-agent decision alongside prior dataset-node IO packages.
- Added a StudyGraph smoke assertion proving the audit-agent IO package appears
  in the final graph result and audit manifest metadata.

Current boundary:

- This slice changes only final audit packaging.
- It does not change dependency planning, dataset dispatch, dataset generation,
  human gates, provider calls, R execution, compare, repair, static rules, or UI.
- The agent summary remains a derived read model. It does not become workflow
  truth and does not change dataset state.
- The audit-agent decision is appended after the summary is written, so the
  summary does not count itself.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/study_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Gateway Agent IO Persistence Slice

Completed:

- Extended canonical `DatasetRunState` and `StudyRunState` with
  `agent_node_inputs` and `agent_node_outputs`.
- `GraphGateway.finalize_inputs()`, `generate_code()`, and
  `execute_approved_code()` now preserve typed IO packages returned by
  DatasetGraph product nodes.
- Gateway recorder methods validate IO packages with the existing
  `AgentNodeInput` / `AgentNodeOutput` contracts before persisting them.
- Every canonical graph-state write now syncs dataset-level agent IO up to the
  study-level read model, matching the existing agent-decision rollup pattern.
- Added focused Gateway regression coverage proving input-spec finalization and
  code generation persist agent IO at dataset level, study level, and in
  `graph_state.json`.
- Follow-up coverage also proves approved-code execution persists
  `execution_agent` IO at dataset level, study level, and in `graph_state.json`.

Current boundary:

- This slice only closes the persistence gap between DatasetGraph product-node
  IO packages and Gateway-owned canonical state.
- It does not change workflow routing, provider calls, dependency planning,
  draft-spec authority, code generation prompts, static-rule semantics,
  R execution, compare, repair, terminal-failure handling, or UI behavior.
- Static rules remain generic and limited-scope. No dataset-specific,
  demo-specific, variable-specific, or PSY201-specific rule was added.
- Reference ADaM remains compare/output-shape evidence only, not derivation
  authority.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('src/adam_agent/schemas/graph_state.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
```

### 2026-05-31 - LG2.7 Agent Node Trace UI Slice

Completed:

- Extended the existing Agent Audit panel to show a plain-language agent node
  handoff trace from canonical graph state.
- The UI now reads `agent_node_inputs` and `agent_node_outputs` from the active
  dataset first, then falls back to study-level graph state.
- Trace cards show the bounded agent role, node, task, decision/status, dataset
  scope, risk-flag count, and artifact-reference count.
- The panel still avoids raw JSON and keeps technical paths out of the main UI.
- Added a focused UI contract test proving the panel uses graph-state agent IO
  fields and continues to avoid `JSON.stringify`.

Current boundary:

- This slice changes only the browser read model and display.
- It does not change FastAPI endpoints, GraphGateway persistence, workflow
  routing, provider calls, dependency planning, static rules, R execution,
  compare, repair, or Reference ADaM authority.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Initial Compare Action UI Slice

Completed:

- Updated the result Compare tab so the user can run the first compare when
  both generated output preview and reference ADaM preview are available.
- Kept the existing `Run Compare Again` action after a compare summary exists.
- Added explicit empty-state wording when compare cannot run because either the
  generated output or reference ADaM table is missing.
- Repeated the product rule in the empty state: reference ADaM is comparison
  evidence only, not derivation authority.
- Added a focused UI contract test for initial compare availability and the
  unavailable-reference state.

Current boundary:

- This slice changes only the browser result-panel action wiring and wording.
- It does not change the `/compare` API endpoint, GraphGateway compare
  persistence, dependency planning, dependency resolution, provider calls,
  static rules, R execution, repair, or Reference ADaM authority.
- It does not auto-run compare; the human still clicks `Run Compare`.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_allows_initial_compare_when_reference_preview_exists tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway -v
python -B -m unittest tests.test_api_phase8 -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Review-Only Output UI Wording Slice

Completed:

- Added a browser helper that reads dataset output quality from graph progress
  or the review-summary read model before choosing user-facing output wording.
- Changed Dependency Map runtime text so structural demo and mock/offline
  outputs are explicitly described as review-only and unable to satisfy
  downstream runtime dependencies.
- Changed Dataset Execution Cards so review-only/demo outputs use a warning
  stage style instead of the same completed-run visual state as real local R
  runtime outputs.
- Updated next-action wording after completed execution so review-only/demo
  outputs are inspected as evidence, not treated as runtime inputs for another
  dataset.
- Added a focused UI contract test for the shared output-quality wording path.

Current boundary:

- This slice changes only browser read-model wording and stage styling.
- It does not change canonical graph state, dependency planning, dependency
  resolution, provider calls, static rules, R execution, compare, repair, or
  Reference ADaM authority.
- It does not block mock/demo outputs from being displayed for review. It only
  prevents the UI from presenting them as real runtime dependency evidence.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -m unittest tests.test_api_phase8 -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Input-Change Target Selection Reset UI Slice

Completed:

- Cleared browser `selectedTargetsForPlan` when uploaded inputs change and the
  previous plan/code/review state is invalidated.
- Updated the stale-plan message to tell the user to re-check output selection
  before refreshing dependency planning.
- Added a focused UI contract test that proves input-change invalidation clears
  stale planned targets alongside plan/generated/review state.

Current boundary:

- This slice changes only browser invalidation state and explanatory text.
- It does not change upload/scanning behavior, `/study-inputs`, dependency
  planning semantics, GraphGateway invalidation, provider calls, static rules,
  R execution, compare, repair, or Reference ADaM authority.
- The target candidates are still recomputed from the latest input summary by
  the existing render path; only the old planned selection is cleared.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_input_change_clears_stale_planned_targets tests.test_api_phase8.Phase8ApiTests.test_index_keeps_reference_only_targets_unplanned_until_explicitly_selected tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 Reference-Only Target Selection UI Slice

Completed:

- Added a browser-side target evidence read model that records whether each
  target candidate came from input specs, legacy code, reference ADaM, manual
  entry, graph state, or progress state.
- Kept reference-only targets visible in Choose Output and Dataset Execution
  Cards, but stopped auto-planning them.
- Demo/autoselect flow now prepares a dependency plan only when at least one
  non-reference-only target can be auto-planned.
- Added source hints on target chips, such as `spec evidence`,
  `legacy evidence`, `spec + reference`, and `reference only`.
- Finalize/generate buttons now tell the user to explicitly select a
  reference-only target before trying to generate it.
- Added focused UI contract tests for the reference-only target-selection
  behavior.

Current boundary:

- This slice changes only browser target-selection state and explanatory text.
- It does not change `/study-inputs`, target discovery payloads, dependency
  planning semantics, GraphGateway state transitions, provider calls, static
  rules, R execution, compare, repair, or Reference ADaM authority.
- A user can still explicitly select a reference-only target. The change only
  prevents the UI from silently turning Reference ADaM evidence into a default
  generation request.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_does_not_default_target_selection_to_adae tests.test_api_phase8.Phase8ApiTests.test_index_keeps_reference_only_targets_unplanned_until_explicitly_selected tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 Advanced Setup Wording UI Slice

Completed:

- Renamed the advanced collapsible panel to `Advanced setup and audit files
  (usually not needed)`.
- Added plain-language guidance that the normal flow does not require editing
  technical fields.
- Reworded visible labels/help text for run id, config file, provider/model,
  API key, and local Rscript so they read as troubleshooting/audit settings
  rather than required product inputs.
- Kept the same field ids and request payload behavior.
- Added focused UI contract checks for the new wording.

Current boundary:

- This slice changes only static HTML/CSS text in the browser UI and the
  matching UI contract tests.
- It does not change provider resolution, API key handling, run id generation,
  config loading, Rscript invocation, GraphGateway state, dependency planning,
  static rules, R execution, compare, or repair.
- It does not remove real LLM provider controls; it only makes clear that they
  are advanced setup controls.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 Reference-Only Dataset Card UI Slice

Completed:

- Updated Dataset Execution Cards so a target that appears only because a
  Reference ADaM file was uploaded no longer looks like it can enter the code
  generation stage.
- Reference-only cards now keep the `code` stage inactive unless the target is
  actually planned in the current run or has generated code/review history.
- Added explicit card context text: Reference ADaM is compare/output-shape
  evidence only, not generation input.
- Added a focused UI contract test for the reference-only card behavior.

Current boundary:

- This slice changes only browser read-model rendering and explanatory text.
- It does not change target discovery, `/study-inputs`, dependency planning,
  GraphGateway state transitions, provider calls, static rules, R execution,
  compare, repair, or Reference ADaM authority.
- It does not remove Reference ADaM visibility. It only prevents the card stage
  strip from presenting reference-only evidence as runnable generation progress.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 SAS7BDAT Preview Status UI Slice

Completed:

- Changed browser file-card status rendering for `.sas7bdat` files whose API
  preview status is `not_previewed`.
- The UI now labels those files as `runtime input` and explains that the SAS
  dataset is recognized for R execution even when browser preview is unavailable.
- Added a focused UI contract test for the status label and summary text.

Current boundary:

- This slice changes only browser file-card wording and pill styling.
- It does not change `/study-inputs` API payloads, file scanning, upload
  handling, sas7bdat runtime support, R package requirements, dependency
  planning, GraphGateway state transitions, provider calls, static rules,
  R execution, compare, repair, or Reference ADaM authority.
- The message does not claim successful preview. It separates runtime input
  support from browser preview availability.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_explains_sas7bdat_not_previewed_as_runtime_input tests.test_api_phase8.Phase8ApiTests.test_study_inputs_marks_sas7bdat_as_runtime_supported_when_preview_unavailable -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Input Warning Path Hiding UI Slice

Completed:

- Changed the browser input-warning rendering so invalid input files are shown
  as `Skipped filename: reason` instead of exposing the full local path.
- Added `inputWarningText()` as a small UI formatter used only by
  `renderInputSummary()`.
- Added a focused UI contract test proving `inputWarnings` no longer builds
  messages from `item.path` directly.

Current boundary:

- This slice changes only browser warning text.
- It does not change `/study-inputs` API payloads, file scanning, upload
  handling, dependency planning, GraphGateway state transitions, provider calls,
  static rules, R execution, compare, repair, or Reference ADaM authority.
- Full technical paths remain available only through API/audit artifacts and
  advanced surfaces where explicitly intended.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_hides_invalid_file_paths_from_input_warnings tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Dataset-Neutral Target Selection UI Slice

Completed:

- Removed the browser fallback that invented `ADAE` when no ADaM target was
  inferred from uploaded evidence.
- Removed browser target-selection preference for `ADAE`; the UI now selects the
  first inferred/planned target from the actual read model instead of silently
  prioritizing one dataset.
- Added a focused UI contract test covering inference, demo auto-selection,
  progress recovery, and graph-state recovery.

Current boundary:

- This slice changes only browser target-selection defaults.
- It does not change target inference rules, manual target entry, dependency
  planning, GraphGateway state transitions, provider calls, static rules, R
  execution, compare, repair, or Reference ADaM authority.
- Demo data will still show ADAE when ADAE is present in the uploaded demo
  specs/reference evidence. The UI no longer invents ADAE when evidence is
  absent.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_does_not_default_target_selection_to_adae -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Approve-Run Dependency Gate UI Slice

Completed:

- Tightened the browser `Approve And Run Locally` availability check so it uses
  the same active dependency block gate as finalize and code generation.
- If graph progress is temporarily unavailable but the recovered dependency plan
  marks the active target as blocked, the UI now keeps local execution disabled
  and explains that dependency review must be resolved first.
- Added a focused UI contract assertion for the approve/run gate.

Current boundary:

- This slice changes only browser action availability and user-facing gate text.
- It does not change backend execution preflight, dependency planning,
  GraphGateway state transitions, provider calls, static rules, R execution,
  compare, repair, or Reference ADaM authority.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_explains_disabled_actions_from_existing_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Demo Load Graph Read Model Refresh Slice

Completed:

- Removed the direct progress-only refresh from the browser demo-load path.
- Demo load still calls `scanInputs()`, and `scanInputs()` now refreshes graph
  read models, which includes the graph-owned progress read model.
- Added a focused UI contract test proving `createDemoStudy()` no longer calls
  `refreshRunProgress()` directly after scanning inputs.

Current boundary:

- This slice changes only browser read-model refresh sequencing after demo load.
- It does not change demo file copying, input scanning, target inference,
  dependency planning, GraphGateway state transitions, provider calls, static
  rules, R execution, compare, repair, or Reference ADaM authority.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_demo_load_does_not_refresh_progress_directly tests.test_api_phase8.Phase8ApiTests.test_index_scan_inputs_refreshes_graph_read_models -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Scan Refresh Graph Read Models Slice

Completed:

- Updated the browser `scanInputs()` path to refresh graph read models after
  rescanning study files.
- This keeps Agent Audit, agent-node traces, dependency progress, and review
  gates aligned with canonical graph state after demo load or manual input
  refresh.
- Added a focused UI contract test proving `scanInputs()` calls
  `refreshGraphReadModels()` instead of refreshing progress alone.

Current boundary:

- This slice changes only browser read-model refresh order.
- It does not change upload handling, input scanning, dependency planning,
  GraphGateway state transitions, provider calls, static rules, R execution,
  compare, repair, or Reference ADaM authority.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_scan_inputs_refreshes_graph_read_models tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.4 Validation Agent IO Compare Slice

Completed:

- Extended graph-owned compare recording so `validation_agent` now writes typed
  `AgentNodeInput` and `AgentNodeOutput` records for
  `compare_reference_output`.
- Kept the existing `validation_agent` decision payload shape while deriving it
  through the typed node output, so existing audit summaries and UI projections
  remain compatible.
- Study-level `agent_node_inputs` and `agent_node_outputs` now roll up compare
  IO from the dataset state, matching evidence/spec/code/static/execution agent
  behavior.
- Tightened generic agent-record deduplication so same-second reruns with
  different payloads are preserved in the audit trail.
- Added gateway tests proving compare IO is persisted at both dataset and study
  level, same-second compare reruns keep separate audit records, and compare
  status/result-summary behavior is unchanged.

Current boundary:

- This slice changes only canonical agent IO persistence for compare.
- It does not change compare calculation, reference ADaM authority, dependency
  planning, provider calls, static rules, R execution, repair, terminal-failure
  routing, or UI behavior.
- Reference ADaM remains comparison/output-shape evidence only and is not used
  as derivation authority.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_keeps_same_second_rerun_audit_records tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_reference_output_computes_and_records_compare -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
```

### 2026-05-31 - LG2.4 Diagnosis/Repair Agent IO Terminal-Failure Slice

Completed:

- Extended `GraphGateway.record_terminal_failure_review()` so
  `diagnosis_repair_agent` writes typed `AgentNodeInput` and `AgentNodeOutput`
  records for `terminal_failure_review`.
- Kept existing terminal-failure triage semantics:
  - records the human triage decision
  - records the next controlled product action
  - does not automatically repair code, revise specs, retry R, or produce
    output
- Study-level `agent_node_inputs` and `agent_node_outputs` now roll up
  terminal-failure triage IO from the dataset state.
- Added focused gateway assertions proving the diagnosis/repair IO is present
  at dataset level, study level, and in persisted `graph_state.json`.

Current boundary:

- This slice only changes canonical agent IO persistence for terminal-failure
  review.
- It does not implement a dedicated LLM repair prompt, spec-revision subgraph,
  retry execution, dependency planning, static rules, compare, R execution, or
  UI behavior.
- Terminal-failure actions remain human-controlled gates.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
```

### 2026-05-31 - LG2.4 Dependency Agent IO Planning Slice

Completed:

- Extended canonical dependency planning so the study-level
  `dependency_agent` writes typed `AgentNodeInput` and `AgentNodeOutput`
  records for `dependency_plan`.
- Preserved the existing `dependency_plan_prepared` decision payload and bound
  it to the typed output package, so agent audit summaries still count the
  same decision.
- Updated study-level agent IO synchronization to keep study-scoped agent
  records, then append dataset-scoped records from each dataset state. This
  prevents later dataset actions from erasing the dependency-agent handoff.
- Added gateway regression coverage proving dependency-agent IO is present in
  canonical graph state and persisted `graph_state.json`, while existing
  dataset-level IO remains available.

Current boundary:

- This slice only changes canonical agent IO persistence for dependency
  planning.
- It does not change dependency planning logic, dependency review routing,
  provider calls, draft spec generation, code generation, static rules, R
  execution, compare, repair, or UI behavior.
- The dependency agent remains a bounded planning/audit role. It does not
  invent clinical derivation logic and does not use Reference ADaM as
  derivation authority.

Verification:

```text
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_agents_contract -v
```

### 2026-05-31 - LG2.4 Gateway Fallback Agent IO Slice

Completed:

- Added default typed `AgentNodeInput` / `AgentNodeOutput` packages for
  Gateway fallback recorder paths when the caller does not provide DatasetGraph
  product-node IO.
- Covered fallback IO for:
  - draft spec recording
  - input spec readiness
  - approved draft spec readiness
  - generated code and limited static-check recording
  - execution result recording
- Kept the existing fallback `AgentDecision` payloads and bound those exact
  decisions into each typed output package.
- Added focused gateway assertions proving direct Gateway recorder calls now
  leave typed IO in dataset state and study-level rollups.

Current boundary:

- This slice only closes an audit packaging gap in fallback recorder paths.
- It does not change DatasetGraph product-node IO, dependency planning,
  provider calls, static rules, R execution, compare, repair, or UI behavior.
- The fallback IO is a compatibility/audit safety net. Normal product flow
  still prefers the richer IO emitted by DatasetGraph product nodes.

Verification:

```text
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_agents_contract -v
```

### 2026-05-31 - LG2.4 Agent Audit Node IO Summary Slice

Completed:

- Extended the derived `agent_audit_summary` read model so it summarizes typed
  `AgentNodeInput` / `AgentNodeOutput` handoffs in addition to agent decisions.
- Added study-level node IO counts, invalid node IO counts, per-agent node
  counts, and latest node-output summaries.
- Added dataset-level node IO counts and latest node-output summaries so audit
  review can show which bounded agent nodes actually handled a dataset.
- Passed StudyGraph-collected node IO into the audit summary writer, matching
  the canonical GraphGateway path.

Current boundary:

- This slice only enriches the derived audit read model.
- It does not change graph routing, dependency planning, provider calls,
  static rules, R execution, compare, repair, UI behavior, or Reference ADaM
  authority.
- Canonical truth remains `graph_state.json`; the summary is still a derived
  human review artifact.

Verification:

```text
python -B -m unittest tests.test_agents_contract -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
```

### 2026-05-31 - LG2.7 Terminal-Failure Triage UI Slice

Completed:

- Added a browser terminal-failure triage panel for the active dataset when
  graph state reports `execution.status == "terminal_failure"`.
- Exposed the controlled human actions already supported by GraphGateway:
  - `retry_execution`
  - `repair_code`
  - `revise_spec`
  - `request_new_input`
  - `skip_dataset`
  - `continue_other_datasets`
- Wired those actions to the existing
  `/runs/{run_id}/datasets/{dataset}/terminal-failure-review` endpoint.
- After recording the decision, the UI refreshes canonical graph read models
  and shows the graph-owned next action instead of inventing a browser-side
  workflow path.
- Added UI contract coverage proving the panel, action buttons, endpoint call,
  and graph-state refresh hook are present.

Current boundary:

- This slice changes only the browser UI. It does not change GraphGateway
  terminal-failure semantics, repair behavior, R execution, static rules,
  compare, dependency planning, or Reference ADaM authority.
- The action remains a human-controlled graph gate. The UI records the choice;
  it does not automatically retry, repair, revise specs, request inputs, skip,
  or continue downstream datasets on its own.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_terminal_failure_triage_actions tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-05-31 - LG2.7 Graph-Owned Terminal-Failure Actions Slice

Completed:

- Moved the terminal-failure action list into the graph progress read model.
  `GraphGateway` now exposes `available_actions` for each dataset when that
  dataset has an open `terminal_failure` interrupt.
- Kept the same six supported terminal-failure decisions:
  `retry_execution`, `repair_code`, `revise_spec`, `request_new_input`,
  `skip_dataset`, and `continue_other_datasets`.
- Updated the browser triage panel to read actions from dataset progress first.
  The local browser constant is now only a compatibility fallback for older
  read models.
- After a terminal-failure decision is recorded, graph progress no longer
  exposes stale triage actions for that dataset, so the UI hides the old action
  buttons and shows the graph-owned next action.
- Added API/model and gateway tests proving terminal-failure actions are visible
  only while the graph-owned interrupt is open.

Current boundary:

- This slice is a read-model ownership fix. It does not change R execution,
  repair routing, static rules, compare behavior, dependency planning, provider
  calls, or Reference ADaM authority.
- UI still records a human decision through the existing
  `/runs/{run_id}/datasets/{dataset}/terminal-failure-review` endpoint; it does
  not perform the follow-up action by itself.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_terminal_failure_triage_actions tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry -v
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
node --check .tmp_tests\ui_script_check.js
```

### 2026-06-01 - LG2.8 Progress Compatibility Projection Path Slice

Completed:

- Tightened `GraphGateway.progress_summary()` metadata so
  `workflow_state_path` is reported only when the compatibility projection file
  actually exists.
- Made `RunProgressResponse.workflow_state_path` optional.
- Preserved graph-owned progress behavior: `graph_state.json` remains the
  source of truth and `/progress` still works when `workflow_state.json` is
  absent.
- Added gateway-level and FastAPI endpoint tests proving missing
  `workflow_state.json` is reported as `None` / `null`, not as a path to a file
  that does not exist.

Current boundary:

- This is read-model metadata truthfulness only.
- It does not remove `workflow_state.json`, change projection writes, alter
  product transitions, or change UI behavior beyond exposing a truthful null
  metadata value.

Review:

- Subagent review returned GO.
- The review noted that other response models still require
  `workflow_state_path`, but those responses correspond to operations that
  create or depend on the compatibility projection; this slice is scoped only to
  the progress read model.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_missing_workflow_projection_as_none -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_missing_workflow_projection_as_null tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/models.py'), pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('tests/test_api_phase8.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.7 Graph-Owned Dependency Next-Action Text Slice

Completed:

- Tightened `nextActionText(...)` for the dependency map.
- If graph progress is loaded but a target has no dataset progress item, the UI
  no longer falls back to local generated/review/execution caches to say
  "review code" or "inspect output".
- The dependency map now shows refresh/reprepare guidance for missing graph
  progress, while preserving Reference ADaM wording as compare/output-shape
  evidence only.
- Added a Node-executed UI test proving stale local generated/review/execution
  cache is ignored when graph progress is loaded and missing the target.

Current boundary:

- This is UI guidance text only.
- It does not change action gating, GraphGateway progress generation, dependency
  planning, provider calls, R execution, compare, repair, or Reference ADaM
  authority.

Review:

- Subagent review returned GO and confirmed the guard runs only after real graph
  target actions (`blocked_reason` / `action_label`) are honored.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_next_action_text_ignores_local_cache_when_progress_loaded tests.test_api_phase8.Phase8ApiTests.test_index_dataset_status_ignores_local_completion_cache_when_progress_loaded tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.7 Graph-Owned Dataset Status Fallback Slice

Completed:

- Tightened `datasetStatus(...)` so local generated/review/execution caches are
  used only when the graph progress read model is unavailable.
- When `state.runProgress` exists but a target lacks a dataset progress item,
  the UI now falls back only to plan/reference/candidate status:
  `ready`, `reference evidence`, `waiting`, or `candidate`.
- Tightened `datasetOutputQualityStatus(...)` the same way. Persisted
  review-summary quality no longer drives dataset status when graph progress is
  already loaded.
- Added a Node-executed UI test proving stale local generated/execution/review
  and output-quality caches cannot mark a dataset as completed or review-only
  while graph progress is present and lacks that target.

Current boundary:

- This is UI read-model fallback behavior only.
- It does not change GraphGateway progress generation, product state, provider
  calls, R execution, compare, repair, dependency planning, or Reference ADaM
  authority.

Review:

- Subagent first returned NO-GO because output-quality fallback still read
  persisted review summary before the graph-progress guard.
- Fixed by making `datasetOutputQualityStatus(...)` return empty when
  `state.runProgress` is present and no progress quality exists for the target.
- Subagent re-review returned GO.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dataset_status_ignores_local_completion_cache_when_progress_loaded tests.test_api_phase8.Phase8ApiTests.test_index_dataset_card_stages_prefer_graph_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.7 Graph-Owned Generate Gate Slice

Completed:

- Added `graphAllowsCodeGeneration(progress)` so the browser treats graph
  progress `next_action` values as the authority for code-generation readiness.
- When dataset progress says `generate_code`, `repair_generated_code`, or
  `revise_approved_spec`, the UI no longer blocks the Generate action only
  because local browser spec-gate cache is missing or stale.
- Applied the same graph-owned guard inside `generateCode()`, so the direct
  button path and the visible availability state stay consistent.
- Preserved the human-review boundary:
  - `review_draft_spec` still routes to draft-spec review;
  - `revise_approved_spec` still calls `/draft-spec`;
  - generated code still requires code review before execution.
- Added UI behavior coverage for the stale-local-spec-cache case.

Current boundary:

- This is a UI gate/read-model correction only.
- Backend GraphGateway/product endpoints remain the hard validation boundary.
- It does not change dependency planning, draft-spec approval semantics, provider
  calls, R execution, compare, repair, static rules, or Reference ADaM authority.

Review:

- Subagent review returned GO.
- The review confirmed this does not bypass draft-spec/code-review gates because
  graph progress remains the source of truth and backend validation remains in
  place.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_draft_review_gate_overrides_local_input_spec_shortcuts -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-06-01 - LG2.7 Graph-Owned Dataset Card Stage Slice

Completed:

- Moved the dataset-card stage strip (`code`, `review`, `run`) toward the graph
  progress read model.
- Added focused browser helpers:
  - `codeStageClassFor(...)`
  - `reviewStageClassFor(...)`
  - `runStageClassFor(...)`
- The helpers now treat `GET /runs/{run_id}/progress` dataset fields
  (`code_status`, `execution_status`, `next_action`) as the source for completed
  stage display.
- Removed local browser/review-summary fallback from stage completion:
  `generatedFor(...)`, `reviewFor(...)`, `executionFor(...)`, and
  `datasetReviewFor(...)` can no longer mark the card stages as `done` when
  graph progress does not say so.
- Kept Reference ADaM protection: reference-only targets do not enter the code
  stage, and review-only/mock/stub outputs still show as review-only rather than
  runtime-complete.
- Added a Node-executed UI matrix test proving graph progress drives stage
  classes and that local cached generated/review/execution data without graph
  progress cannot mark stages complete.

Current boundary:

- This is UI read-model display only.
- It does not change GraphGateway, dependency planning, provider calls, static
  rules, R execution, compare, repair, or terminal-failure semantics.
- The stage strip is a viewer of graph progress. It does not become a workflow
  controller and does not make API decisions.

Review:

- Subagent first returned NO-GO because stage helpers still used local browser
  caches and persisted review summary as completion fallback.
- Fixed by removing those fallbacks from completed-stage logic and adding a
  regression case where local caches exist but graph progress is empty.
- Subagent re-review returned GO.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage tests.test_api_phase8.Phase8ApiTests.test_index_dataset_card_stages_prefer_graph_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_dataset_card_stages_prefer_graph_progress_read_model -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-05-31 - LG2.7 Graph-Owned Primary Action Gates Slice

Completed:

- Added a browser `graphActionGate()` helper so the four primary dataset
  actions are driven first by `GET /runs/{run_id}/progress` dataset
  `next_action` values.
- Mapped graph-owned next actions to UI actions:
  - `finalize_inputs` / `reconfirm_inputs` -> finalize inputs
  - `review_draft_spec` -> approve draft spec
  - `generate_code` / `repair_generated_code` -> generate R code
  - `revise_approved_spec` -> generate a revised draft spec through
    `/draft-spec`, not through normal finalize
  - `review_code` / `execute_approved_code` / `retry_approved_execution` ->
    code approval or local execution
- Prevented duplicate code-review writes when graph progress already says the
  approved code should execute or retry execution.
- Fixed the revised-spec path so a terminal-failure `revise_spec` decision
  cannot silently reuse an uploaded input spec or old approved draft:
  `DatasetGraph` now evaluates `force_new_draft_spec` before the input-spec
  shortcut, and the UI calls `/draft-spec` for `revise_approved_spec`.
- Fixed the draft-review UI gate so graph progress `review_draft_spec` wins
  over local browser shortcuts that see uploaded input specs or old approved
  drafts.
- Added Node-executed UI behavior coverage for the next-action/button matrix,
  plus API regression coverage for terminal-failure revise-spec flow.

Current boundary:

- This slice changes UI action gating and one DatasetGraph control-flow guard.
- It does not change dependency planning, provider calls, static rules, R
  execution, compare, repair implementation, terminal-failure triage semantics,
  or Reference ADaM authority.
- The browser still sends existing split-flow endpoint requests; it does not
  become the workflow controller. GraphGateway progress remains the UI action
  source of truth.

Review:

- Subagent review first returned NO-GO because `revise_approved_spec` could
  route through normal finalize and reuse stale spec evidence. Fixed.
- Subagent re-review returned NO-GO because `review_draft_spec` could still be
  blocked by local input-spec shortcuts. Fixed.
- Final subagent review returned GO. A minor copy issue was fixed before commit.

Verification:

```text
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_graph_smoke -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-05-31 - LG2.7 Graph-Owned Human Review Queue Slice

Completed:

- Added `review_queue` to the graph progress read model. `GraphGateway` now
  derives open human gates from canonical study/dataset interrupts and
  graph-owned next actions.
- The read model includes review scope, dataset, interrupt name, source, reason,
  action, and action label, so the browser does not need to infer the queue from
  raw graph state when progress data is current.
- Updated the browser human-review queue to prefer `progress.review_queue`.
  The old local inference path remains only as a compatibility fallback for
  older progress payloads.
- Fixed review-queue semantics after subagent review:
  - terminal-failure follow-up states after triage no longer appear as open
    triage review gates;
  - study-level dependency review is still shown when the dependency status
    requires review but no interrupt payload is present;
  - dataset-level interrupts are handled from dataset progress, not mistaken for
    study-level interrupts.
- Added focused gateway and UI contract tests proving the queue is exposed from
  graph progress and consumed by the UI.

Current boundary:

- This slice changes only read-model ownership and UI consumption.
- It does not change approval semantics, interrupt resume behavior, dependency
  planning, provider calls, static rules, R execution, compare, repair, or
  Reference ADaM authority.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_blocks_review_required_dependency_sources tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state -v
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
node --check .tmp_tests\ui_script_check.js
```

### 2026-06-01 - LG2.8 Gateway-Owned Compatibility Metadata Slice

Completed:

- Moved graph compatibility response metadata closer to the graph boundary:
  `project_graph_state_to_workflow()` now writes `workflow_control`,
  `graph_state_path`, and `workflow_state_path` into the compatibility
  `workflow_state.json` projection.
- Removed the old FastAPI service helper that reconstructed graph/workflow
  paths from `study_dir` and `run_id`.
- Product split-flow responses now copy compatibility metadata from
  `GraphGateway` result projections.
- Responses whose schemas do not expose `workflow_control`
  (`RunPlanResponse`, `DependencyReviewResponse`,
  `TerminalFailureReviewResponse`) copy only the two path fields from the same
  gateway projection.
- Added regression tests proving:
  - graph projections carry the compatibility metadata they advertise;
  - service no longer has the old path-synthesizing helper;
  - dependency-review and terminal-failure-review responses forward gateway
    projection paths, rather than rebuilding correct-looking paths locally.

Current boundary:

- This is read-model/compatibility metadata cleanup only.
- It does not change canonical `graph_state.json`, dependency planning,
  interrupt semantics, provider calls, static rules, R execution, compare, or
  legacy `/runs` behavior.
- `GET /runs/{run_id}/progress` remains a graph-state read model and still
  reports `workflow_state_path: null` when the compatibility projection file is
  absent.

Review:

- Subagent review returned GO.
- Follow-up test gap from the review was closed by adding direct API-level tests
  for dependency-review and terminal-failure-review projection paths.

Verification:

```text
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_api_phase8 -v
python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Checkpointer Boundary Extraction Slice

Completed:

- Added `src/adam_agent/graph/checkpointing.py` as the explicit LangGraph
  checkpointer boundary.
- `GraphGateway` now obtains the default checkpointer through this boundary and
  uses the same module to describe runtime persistence metadata.
- The boundary currently exposes only the in-memory backend and rejects unknown
  backends fail-closed.
- Custom checkpointers are reported as `custom` without claiming persistence.

Current boundary:

- This slice does not install or enable `langgraph-checkpoint-sqlite`.
- `langgraph_checkpointer_persistent` remains `false` for the default product
  path.
- Restart recovery remains `graph_state.json`.
- `graph_checkpoints.sqlite` remains a product audit ledger, not a LangGraph
  checkpointer.

Review:

- Subagent review returned GO.
- The reviewer confirmed the checkpointer boundary centralizes creation and
  metadata without enabling unsupported persistence, keeps `GraphGateway`
  defaults aligned with prior InMemory behavior, fails closed for unknown
  backends, reports custom checkpointers without claiming persistence, and does
  not change public API/UI defaults.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_defaults_to_nonpersistent_memory tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_backend tests.test_graph_gateway.GraphGatewayTests.test_custom_checkpointer_is_reported_without_persistence_claim -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 267 tests in 28.419s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Native Dependency-Review Interrupt Pilot Slice

Completed:

- Added a narrow internal `StudyGraph` mode, `native_dependency_review`, that
  uses LangGraph `interrupt()` at the study-level dependency-review gate.
- Added `GraphGateway.start_native_dependency_review()` and
  `GraphGateway.resume_native_dependency_review()` as pilot-only entry points.
- The pilot writes native interrupt metadata into `runtime_persistence` while
  preserving `graph_state.json` as the product source of truth.
- Added regression tests proving the StudyGraph can pause at
  `wait_for_dependency_review`, resume with `Command(resume=...)`, and persist
  the reviewed state through GraphGateway.

Current boundary:

- This is not wired into the public UI/API default path yet.
- It only covers the study-level `dependency_review` gate, not draft-spec,
  code-review, terminal-failure, or multi-dataset product execution gates.
- It still uses the existing default `InMemorySaver`, so process-restart
  recovery remains based on canonical `graph_state.json` until a persistent
  LangGraph checkpointer is introduced.
- Existing split-flow endpoints remain unchanged.

Review:

- Subagent review returned GO.
- The reviewer confirmed the pilot uses real LangGraph `interrupt()` and
  `Command(resume=...)`, remains isolated from public/default split-flow
  endpoints, keeps persistence wording conservative, and does not introduce a
  reducer/schema issue likely to corrupt existing behavior.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_native_dependency_review_interrupt_can_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dependency_review_interrupt_roundtrip_persists_state -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke -v
Ran 155 tests in 12.515s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 109 tests in 18.032s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Dataset Draft-Spec Native Interrupt Pilot Slice

Completed:

- Added an internal `DatasetGraph` pilot node, `wait_for_draft_spec_review`.
- When dataset state explicitly sets `native_draft_spec_review=True` and
  `draft_spec_agent` has generated a review-required draft spec, DatasetGraph
  now pauses at a dataset-level `draft_spec_review` using LangGraph
  `interrupt()`.
- Resuming with `Command(resume=...)` now:
  - closes the native interrupt and marks the next action as
    `persist_draft_spec_review` on `approve`;
  - closes the native interrupt and marks the dataset failed with
    `human_rejected_draft_spec` on `reject`.
- `compile_dataset_graph()` now accepts an optional checkpointer for internal
  native interrupt tests while preserving the default call shape.

Current boundary:

- This is a dataset-level native interrupt pilot only. It is not wired into the
  public UI/API default path.
- The public product flow still records draft-spec review artifacts, hash and
  fingerprint checks, and `workflow_state.json` projections through the
  `GraphGateway` split-flow endpoint.
- This slice does not implement a persistent LangGraph SQLite/Postgres
  checkpointer and does not change LLM generation, R execution, compare, static
  rules, repair, or UI behavior.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_draft_spec_review_interrupt_can_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_draft_spec_review_reject_closes_interrupt_as_failed tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
```

### 2026-06-01 - LG2.8 Legacy Run Projection Metadata Slice

Completed:

- Tightened the legacy successful `POST /runs` response path so it no longer
  reconstructs compatibility metadata in the FastAPI service layer.
- `run_study_from_request()` now passes the full `GraphGatewayLegacyRunResult`
  to response construction.
- `_response_from_legacy_graph_result()` copies `workflow_control`,
  `graph_state_path`, and `workflow_state_path` from the gateway-owned legacy
  `workflow_projection`.
- Removed the now-unused legacy compatibility constant import from the service
  layer.
- Added a sentinel API regression test proving the legacy response forwards the
  gateway projection path instead of rebuilding a local path.

Current boundary:

- This only changes legacy `/runs` response metadata ownership.
- It does not change the legacy stub run behavior, blocked LLM `/runs` split-flow
  guard, graph state, dependency planning, product split-flow endpoints, review
  summary read models, or UI behavior.

Review:

- Subagent review returned GO.
- The reviewer confirmed blocked LLM `/runs` does not enter the new response
  constructor and `RunStudyResponse` accepts the copied fields.

Verification:

```text
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.8 Current API Contract Split-Flow Documentation Slice

Completed:

- Rewrote `docs/phase8_1_api_contract.md` around the current product flow:
  workspace/input upload -> graph-owned dependency planning -> dependency
  review when needed -> dataset finalize/draft-spec/code-review/execute gates
  -> progress/review/compare/download read models.
- Made `GraphGateway` state ownership explicit in the API contract:
  `graph_state.json` is the product source of truth, while
  `workflow_state.json` is a compatibility projection for current UI/API read
  paths.
- Documented upload invalidation through `GraphGateway`, including
  `touched_graph_runs`, `touched_runs`, and `skipped_graph_runs`.
- Moved `POST /runs` into a dedicated Legacy Compatibility section and kept the
  `graph_state_path: null` explanation so callers do not mistake legacy smoke
  output for graph-owned product state.
- Added current endpoint coverage for `/product-workspace`, `/studies/files`,
  `/runs/prepare`, `/dependency-review`, dataset split-flow endpoints,
  `/progress`, `/graph-state`, `/review-summary`, table preview, compare,
  download, artifact read, and LLM connection testing.

Current boundary:

- This is documentation alignment only.
- It does not change FastAPI behavior, GraphGateway state transitions,
  provider calls, static rules, R execution, compare, UI behavior, or legacy
  `/runs` behavior.
- The contract intentionally does not claim production-grade CDISC/P21 rules or
  hardened sandboxing.

Review:

- Subagent review returned GO.
- The non-blocking suggestion to list still-existing compatibility artifact
  read endpoints was accepted by adding a short compatibility read section.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion tests.test_api_phase8.Phase8ApiTests.test_upload_endpoint_delegates_input_invalidation_to_gateway tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model -v
git diff --check -- docs/phase8_1_api_contract.md docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
```

### 2026-06-01 - LG2.8 Early Phase Status Summary Calibration Slice

Completed:

- Updated the LG2.0/LG2.1/LG2.2 current-status summaries so they no longer
  imply product split-flow state transitions are still owned by FastAPI service
  wrappers.
- Clarified the current boundary:
  - product split-flow state changes are now `GraphGateway`-owned;
  - FastAPI service wrappers still validate requests, resolve config/provider
    settings, and shape responses;
  - the whole product loop is still not a single native LangGraph
    interrupt/checkpointer run.
- Kept open items focused on the real remaining architecture work:
  native full-loop interrupt/resume, checkpointer-backed restart recovery,
  automatic repair/spec-revision routing, and eventual legacy stub removal.

Current boundary:

- This is documentation/status calibration only.
- It does not change code, tests, API behavior, GraphGateway behavior, UI
  behavior, static rules, R execution, repair, or compare.

Review:

- Subagent review returned GO.
- The reviewer confirmed the wording matches current service-to-gateway
  delegation and does not overstate the native LangGraph/checkpointer maturity.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods tests.test_api_phase8.Phase8ApiTests.test_service_layer_no_longer_writes_workflow_state_directly tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway -v
git diff --check -- docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
```

### 2026-06-01 - LG2.8 Closeout Verification Slice

Completed:

- Ran a closeout-level regression check for the current LangGraph-v2 split-flow
  architecture.
- The verification covered:
  - FastAPI split-flow API and `GraphGateway` state delegation;
  - graph progress, dependency review, draft-spec/code review, approved
    execution, terminal-failure review, compare, and upload invalidation;
  - DatasetGraph/StudyGraph product-mode guards;
  - bounded agent IO/audit contracts;
  - generic static-rule policy and rule-pack admission;
  - local Rscript sandbox boundaries.
- The evidence supports the current phase conclusion: covered product
  split-flow state transitions are now owned by `GraphGateway` and canonical
  `graph_state.json`;
  `workflow_state.json` remains a compatibility projection; UI/API still drives
  the product through split-flow endpoints rather than one complete native
  LangGraph interrupt/checkpointer run.

Current boundary:

- This slice records closeout evidence only. It does not change code or product
  behavior.
- The following remain explicitly unfinished:
  - native LangGraph full-loop interrupt/resume;
  - full human-review recovery after process restart through a checkpointer;
  - automatic multi-dataset generation/review/execution;
  - automatic repair/spec-revision closed loop;
  - production-grade CDISC/P21/company standards checks;
  - container/OS-level hardened R sandboxing;
  - final removal of legacy stub/test-mode paths.
- The current local Rscript runner is still a development execution boundary,
  although it has forbidden-call, path, argument, and environment guards.
- The current static-rule layer is still a generic contract/policy gate, not a
  full clinical compliance proof.

Review:

- Subagent review returned GO.
- The reviewer confirmed the closeout record does not overstate native
  LangGraph/checkpointer maturity and does not omit the unfinished boundaries
  around multi-dataset batch execution, repair/spec-revision, CDISC/P21,
  hardened sandboxing, and legacy stub removal.

Verification:

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
Ran 179 tests in 19.112s - OK

python -B -m unittest tests.test_graph_smoke tests.test_agents_contract tests.test_static_rules tests.test_sandbox -v
Ran 126 tests in 8.109s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1/LG2.8 Runtime Persistence Boundary Slice

Completed:

- Added `runtime_persistence` metadata to canonical `StudyRunState`.
- `GraphGateway._persist_graph_state()` now refreshes this metadata before each
  canonical graph-state write.
- `project_graph_state_to_workflow()` projects the same metadata into
  `workflow_state.json`.
- `GraphGateway.progress_summary()` and `RunProgressResponse` now expose
  `runtime_persistence` so UI/API/reviewers can see the current recovery
  boundary.
- The metadata explicitly states that:
  - the product source of truth is `graph_state.json`;
  - `graph_checkpoints.sqlite` is a local product audit ledger, not a LangGraph
    SQLite checkpointer;
  - the default LangGraph checkpointer is currently `InMemorySaver`;
  - `langgraph_checkpointer_persistent` is conservatively `false` until a real
    persistent LangGraph checkpointer is integrated;
  - `native_interrupt_resume` remains `false`;
  - restart recovery currently comes from `graph_state.json`.

Current boundary:

- This slice does not implement a real LangGraph SQLite/Postgres checkpointer.
- It does not change workflow routing, review semantics, LLM generation, R
  execution, compare, static rules, UI behavior, or legacy `/runs` behavior.
- It only makes the current persistence capability explicit so future work does
  not mistake the local `graph_checkpoints.sqlite` ledger for a native
  LangGraph checkpoint store.

Review:

- Subagent review returned GO.
- The reviewer confirmed the slice does not make `workflow_state.json` a source
  of truth again and does not overstate native LangGraph/checkpointer resume
  maturity.
- Accepted one non-blocking suggestion: `langgraph_checkpointer_persistent` no
  longer infers persistence optimistically from the checkpointer class name and
  is conservative `false` in this slice.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_gateway_checkpoint_can_be_read_from_same_graph_instance tests.test_graph_gateway.GraphGatewayTests.test_gateway_persists_canonical_state_for_process_restart_resume -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('src/adam_agent/schemas/graph_state.py'), pathlib.Path('src/adam_agent/api/models.py'), pathlib.Path('src/adam_agent/graph/workflow_state.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.1 Strict Human Resume Gate Slice

Completed:

- Tightened `GraphGateway.resume()` so a human command must match the current
  open graph interrupt or fail closed.
- Study-level commands can only resume the current open study interrupt, such as
  `dependency_review`.
- Dataset-level commands can only resume the current open interrupt on the same
  dataset, such as `ADAE/draft_spec_review`.
- Resolved interrupts, missing dataset interrupts, and wrong interrupt names on
  a dataset are no longer silently recorded into canonical graph state.
- Added regressions proving mismatched commands do not write `graph_state.json`.

Current boundary:

- This slice only tightens human command -> interrupt matching.
- It does not implement full native LangGraph interrupt/checkpointer resume.
- It does not change dependency planning, draft/spec/code review semantics, LLM
  generation, R execution, compare, static rules, UI behavior, or legacy
  `/runs` behavior.
- It is a safety precondition for later native interrupt/resume work: a human
  command must first prove it belongs to the current open gate before it can
  enter canonical state.

Review:

- Subagent review returned GO.
- The reviewer confirmed the resume gate runs before canonical state
  persistence, separates study-level and dataset-level interrupts correctly,
  preserves unrelated open dataset interrupts, and does not block the existing
  split-flow review endpoints.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_dependency_owns_interrupt_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_preserves_other_dataset_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_rejects_study_command_without_matching_open_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_rejects_dataset_command_for_other_open_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_rejects_dataset_command_without_dataset_interrupt -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 260 tests in 29.362s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Dataset Code Review Native Interrupt Pilot Slice

Completed:

- Added a dataset-level `wait_for_code_review` graph node.
- When dataset state explicitly sets `native_code_review=True` and
  `generate_r_code_agent` has generated review-required R code, DatasetGraph
  now pauses at `code_review` with native LangGraph `interrupt()`.
- On `Command(resume=...)`:
  - `approve` closes the native interrupt and marks the next action as
    `persist_code_review`;
  - `reject` closes the native interrupt, keeps the dataset in `needs_review`,
    and marks the next action as `regenerate_code`.
- Added `native_code_review_status` and `native_code_review_resume` state fields
  to record the internal pilot resume result.

Current boundary:

- This is an internal dataset-level native interrupt pilot. It is not connected
  to the public UI/API default path.
- This slice does not write `review/{dataset}_code_review.json` and does not
  replace `GraphGateway.review_code()` code hash, static-check hash, input
  fingerprint, or approved spec checks.
- Approve does not directly execute R code. The public product flow must still
  persist the code-review artifact through the gateway before approved-code
  execution can run.
- This slice does not implement a persistent LangGraph SQLite/Postgres
  checkpointer and does not change LLM generation, R execution, compare, static
  rules, repair, or UI behavior.

Review:

- Subagent review returned GO.
- The reviewer confirmed `native_code_review` is explicit opt-in, the default
  `GraphGateway`/FastAPI/UI split-flow does not enter this node, the pilot does
  not bypass `GraphGateway.review_code()` review artifact, code hash,
  static-check hash, input fingerprint, or approved spec checks, and approve
  does not directly execute R.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_code_review_interrupt_can_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_code_review_reject_closes_interrupt_for_regeneration tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 -v
Ran 271 tests in 27.177s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Native Code Review Command Bridge Slice

Completed:

- Added `GraphGateway.review_code_from_command()`.
- The entry point accepts a graph-native `HumanCommand`, loads the current
  canonical `graph_state.json`, and uses the existing resume gate to verify the
  command matches the current open dataset-level `code_review` interrupt.
- After validation, it delegates to the existing `GraphGateway.review_code()` to
  write `review/{dataset}_code_review.json` and reuse existing code hash,
  static-check hash, input fingerprint, approved spec checks, and canonical
  state persistence.
- Added tests proving:
  - a matching `code_review` command enters the formal code-review
    artifact/state flow;
  - a command that does not match the current open interrupt fails closed and
    does not write a review artifact.

Current boundary:

- This slice only provides a safe bridge from native command to gateway artifact
  flow.
- It does not change the public UI/API default path and does not automatically
  connect the DatasetGraph native interrupt to the browser.
- It does not duplicate `review_code()` validation and does not bypass existing
  code-review artifact/hash/fingerprint semantics.
- It does not implement a persistent LangGraph SQLite/Postgres checkpointer and
  does not change LLM generation, R execution, compare, static rules, repair, or
  UI behavior.

Review:

- Subagent review returned GO.
- The reviewer confirmed the bridge first loads canonical graph state and uses
  the open interrupt gate to validate the command before delegating to the
  existing `review_code()`; it does not duplicate or weaken code hash,
  static-check hash, input fingerprint, or approved spec checks; mismatched
  commands fail closed before any review artifact is written.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_bridges_native_interrupt_to_artifact_flow tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_rejects_mismatched_interrupt_without_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_writes_artifact_and_records_canonical_state -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 273 tests in 29.084s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Native Code Review Command Guard Slice

Completed:

- Tightened `GraphGateway.review_code_from_command()`:
  - if a study-level interrupt is still open, such as `dependency_review`, a
    dataset-level `code_review` command fails closed first;
  - the existing dataset-level open interrupt matching remains in place.
- Added a reject bridge test proving a native `reject` command enters the formal
  code-review artifact/state flow without unlocking execution.
- Added a study-level gate test proving a code-review artifact is not written
  while a study-level interrupt is unresolved, and generated code is not
  accidentally marked approved/rejected.
- Added a dataset-level rollup test proving a top-level `current_interrupt` with
  a dataset-bound `code_review` interrupt is not mistaken for a study-level gate.

Current boundary:

- This slice only tightens native command bridge safety semantics and test
  coverage.
- It does not change the public UI/API default path and does not automatically
  connect DatasetGraph native interrupts.
- It does not change `GraphGateway.review_code()` artifact/hash/fingerprint
  checks and does not change R execution, LLM generation, compare, static rules,
  repair, or UI behavior.
- It does not implement a persistent LangGraph SQLite/Postgres checkpointer.

Review:

- Subagent review returned GO.
- The review confirmed the native command bridge still loads canonical graph
  state first, the study-level gate fails closed before artifact writes,
  dataset-level interrupts are not mistaken for study-level gates, the reject
  bridge continues to reuse `review_code()` without forking
  artifact/hash/fingerprint/approval semantics, and the public FastAPI/UI
  split-flow default path is unchanged.
- Accepted one non-blocking suggestion and added the dataset-level rollup
  regression test.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_bridges_native_interrupt_to_artifact_flow tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_bridges_reject_without_execution_unlock tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_rejects_mismatched_interrupt_without_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_rejects_dataset_command_behind_study_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_allows_dataset_interrupt_as_current_rollup -v
Ran 5 tests in 0.258s - OK

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 276 tests in 28.657s - OK

python -B -m compileall -q src tests
git diff --check
Exited 0; CRLF warnings only.
```

### 2026-06-01 - LG2.1 Terminal-Failure Command Bridge Slice

Completed:

- Added `GraphGateway.review_terminal_failure_from_command()` as a graph-native
  command bridge for terminal-failure triage.
- The bridge:
  - loads canonical `graph_state.json`;
  - rejects any open study-level interrupt before accepting a dataset-level
    terminal-failure command;
  - uses `_assert_resume_command_matches_open_interrupt()` to prove the command
    matches the current dataset `terminal_failure` interrupt;
  - validates the action against `TERMINAL_FAILURE_REVIEW_ACTIONS`;
  - delegates to existing `review_terminal_failure()` and therefore keeps
    terminal-failure triage rules, next-action mapping, agent audit writes, and
    input fingerprint handling centralized in `record_terminal_failure_review()`.
- Added regressions proving:
  - a matching `terminal_failure` command enters the formal triage flow and
    records diagnosis/repair agent audit output;
  - a mismatched dataset interrupt fails closed without writing
    `terminal_failure_review`;
  - an unsupported command action fails closed without triage mutation;
  - a dataset terminal-failure command behind an open study-level interrupt
    fails closed before mutating dataset triage state.

Current boundary:

- This is a command bridge only. It does not add a native DatasetGraph
  `terminal_failure` interrupt roundtrip yet.
- It does not change the public FastAPI/UI split-flow default path.
- It does not implement automatic repair, spec revision, retry execution, or a
  persistent LangGraph SQLite/Postgres checkpointer.

Review:

- Subagent review returned GO.
- The review confirmed the bridge loads canonical graph state, blocks
  study-level gates first, validates the current dataset interrupt, and then
  delegates to the existing terminal-failure triage flow without duplicating or
  bypassing its rules.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_bridges_to_triage_flow tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_rejects_mismatched_interrupt_without_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_rejects_invalid_action_without_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_rejects_dataset_command_behind_study_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed -v
Ran 7 tests in 0.313s - OK
```

### 2026-06-01 - LG2.1 Optional Checkpointer Backend Guard Slice

Completed:

- Extended the explicit checkpointer backend names to `memory`, `sqlite`, and
  `postgres`.
- Preserved the default `memory` backend:
  - `build_checkpointer()` still returns `InMemorySaver`;
  - `describe_checkpointer()` still reports
    `langgraph_checkpointer_persistent=false`,
    `native_interrupt_resume=false`, and
    `restart_recovery_source=graph_state_json`.
- Added fail-closed optional backend guards:
  - `sqlite` now requires the optional `langgraph-checkpoint-sqlite` package;
  - `postgres` now requires the optional `langgraph-checkpoint-postgres`
    package;
  - even when an optional package is present, this build still rejects it until
    database lifecycle management is wired.
- Accepted the subagent's low-severity wording suggestion: optional package
  detection now uses `importlib.util.find_spec()` before import, so missing
  transitive dependencies are not mislabeled as a missing LangGraph package.
- Added regressions for default memory, unavailable SQLite, unavailable
  Postgres, custom checkpointer metadata, and runtime persistence projection.

Current boundary:

- This slice does not enable persistent LangGraph checkpointing.
- It does not change public FastAPI/UI behavior, graph invocation semantics, or
  native interrupt behavior.
- It is a safer configuration boundary for the later SQLite/Postgres
  checkpointer implementation.

Review:

- Subagent review returned GO.
- The review confirmed SQLite/Postgres fail closed, do not fall back to memory,
  and do not claim persistence. The default product path remains memory-backed.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_defaults_to_nonpersistent_memory tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_sqlite_backend tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_postgres_backend tests.test_graph_gateway.GraphGatewayTests.test_custom_checkpointer_is_reported_without_persistence_claim tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary -v
Ran 5 tests in 0.039s - OK
```

### 2026-06-01 - LG2.1 Native Draft-Spec Gateway Roundtrip Slice

Completed:

- Added internal `GraphGateway.start_native_draft_spec_review()`:
  - uses the DatasetGraph native `draft_spec_review` interrupt to actually
    pause;
  - after the pause, still records through shared
    `_record_draft_spec_generation_from_dataset_result()` into existing
    canonical `record_draft_spec_generation()` state and the formal
    draft-spec review interrupt;
  - records `native_draft_spec_review_interrupt` metadata with an explicit
    `draft_spec_review_pilot_only` boundary.
- Added internal `GraphGateway.resume_native_draft_spec_review()`:
  - checks canonical `graph_state.json` before native resume;
  - rejects an open study-level interrupt before consuming the native
    DatasetGraph checkpoint;
  - resumes the DatasetGraph native interrupt only after the canonical
    dataset-level `draft_spec_review` interrupt still matches;
  - passes the returned human command into `review_draft_spec_from_command()`;
  - therefore formal review artifacts, draft-spec hash, approved-spec hash, and
    input fingerprint validation continue to reuse the existing gateway checks.
- Added `GraphGateway.review_draft_spec_from_command()` as the native command
  bridge for draft-spec review. It loads canonical graph state, rejects
  unresolved study-level gates, validates the dataset interrupt, and then
  delegates to the existing `review_draft_spec()` artifact flow.
- Extracted DatasetGraph draft-spec recording from `finalize_inputs()` into a
  shared helper so the public split-flow and native pilot use the same
  canonical draft-spec generation record path.
- Added regressions proving:
  - native approve roundtrip writes formal
    `review/{dataset}_draft_spec_review.json` and closes the
    `draft_spec_review` interrupt;
  - native reject roundtrip writes a formal reject artifact while keeping the
    dataset locked at draft-spec review;
  - native resume checks the canonical dataset open interrupt before resuming
    the DatasetGraph checkpoint;
  - native resume also rejects an open study-level interrupt before native
    checkpoint resume, so the checkpoint is not consumed and no review artifact
    is written.

Current boundary:

- This remains an internal pilot and does not change the public FastAPI/UI
  split-flow default path.
- It does not implement a persistent LangGraph SQLite/Postgres checkpointer.
- It does not change LLM draft-spec generation quality, R execution, compare,
  repair, static rules, or Reference ADaM authority.

Review:

- First subagent review returned NO-GO: study-level interrupt validation happened
  after native DatasetGraph resume, which could consume the in-memory native
  checkpoint before formal artifact flow failed closed.
- Fixed by moving the study-level interrupt guard ahead of native resume in
  `resume_native_draft_spec_review()` and adding a regression that patches
  `compile_dataset_graph` to prove it is not called while a study-level gate is
  open.
- Follow-up subagent review returned GO.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_roundtrip_persists_formal_review_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_reject_roundtrip_keeps_review_locked tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_resume_requires_canonical_draft_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_resume_rejects_study_interrupt_before_native_resume -v
Ran 4 tests in 0.539s - OK
```

### 2026-06-01 - LG2.1 Native Code Review Gateway Roundtrip Slice

Completed:

- Added internal `GraphGateway.start_native_code_review()`:
  - uses the DatasetGraph native `code_review` interrupt to actually pause;
  - after the pause, still records through shared
    `_record_code_generation_from_dataset_result()` into existing canonical
    `record_code_generation()` state and the formal code-review interrupt;
  - records `native_code_review_interrupt` metadata while still reporting that
    the default LangGraph checkpointer is not persistent.
- Added internal `GraphGateway.resume_native_code_review()`:
  - resumes the DatasetGraph native interrupt first;
  - passes the returned human command into `review_code_from_command()`;
  - therefore formal review artifacts, code hash, static-check hash, spec hash,
    and input fingerprint validation continue to reuse the existing gateway
    checks.
- Extracted DatasetGraph result recording from `generate_code()` into a shared
  helper so the public split-flow and native pilot use the same canonical
  code-generation record path.
- Added regressions proving:
  - native approve roundtrip writes formal `review/{dataset}_code_review.json`
    and closes the `code_review` interrupt;
  - native reject roundtrip writes a formal reject artifact while keeping
    execution locked;
  - native resume checks the canonical open `code_review` interrupt before
    resuming the DatasetGraph checkpoint, so a changed canonical state fails
    closed first.

Current boundary:

- This remains an internal pilot and does not change the public FastAPI/UI
  split-flow default path.
- It does not implement a persistent LangGraph SQLite/Postgres checkpointer.
- It does not change R execution, compare, repair, static rules, or Reference
  ADaM authority.

Review:

- Subagent review returned GO.
- The review confirmed this does not introduce a second product code-review
  state; the native interrupt is a transient pause that still bridges back
  through `review_code_from_command()` -> `review_code()` ->
  `record_code_review()`.
- The review confirmed `generate_code()` keeps the public default path and does
  not enable `native_code_review=True`, so the FastAPI/UI split-flow is not
  flipped.
- Accepted and fixed two non-blocking notes:
  - native interrupt metadata boundary is now labeled per pilot;
  - `resume_native_code_review()` performs a canonical open-interrupt preflight
    before resuming the native checkpoint.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_roundtrip_persists_formal_review_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_reject_roundtrip_keeps_execution_locked tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_resume_requires_canonical_code_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dependency_review_interrupt_roundtrip_persists_state -v
Ran 4 tests in 0.546s - OK

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 279 tests in 27.897s - OK

python -B -m compileall -q src tests
git diff --check
Exited 0; CRLF warnings only.
```

### 2026-06-01 - LG2.1 Local SQLite Checkpointer Wiring Slice

Completed:

- Extended `CheckpointerBundle` with explicit resource lifecycle metadata:
  `checkpoint_path`, `close_callback`, and `close()`.
- Added `default_sqlite_checkpointer_path(study_dir, run_id)` so the LangGraph
  runtime checkpoint store has a separate, predictable path:
  `runs/{run_id}/langgraph_checkpoints.sqlite`.
- `build_checkpointer("sqlite", sqlite_path=...)` now:
  - requires an explicit SQLite path;
  - fails closed when `langgraph-checkpoint-sqlite` is not installed;
  - builds `langgraph.checkpoint.sqlite.SqliteSaver` when the optional package
    is installed;
  - marks `langgraph_checkpointer_persistent=true` and
    `native_interrupt_resume=true` only for that real SQLite saver;
  - reports `native_interrupt_resume_scope=native_pilot_interrupts_only` so the
    metadata cannot be read as full product-loop restart recovery.
- `GraphGateway` can now be constructed with
  `checkpointer_backend="sqlite"` and `sqlite_checkpointer_path=...`, while the
  public default remains `memory`.
- Added optional packaging metadata:
  `adam-agent-studio[sqlite-checkpoint]` installs
  `langgraph-checkpoint-sqlite>=3.0.3,<3.1`.
- Added tests for:
  - required SQLite path;
  - missing optional package fail-closed behavior;
  - separation between `langgraph_checkpoints.sqlite` and the product audit
    ledger `graph_checkpoints.sqlite`;
  - SQLite metadata when the optional package is available;
  - restart-style readback where a new `GraphGateway` reads a native
    dependency-review interrupt from the same SQLite checkpoint.

Current boundary:

- The public FastAPI/UI product path still defaults to the in-memory
  checkpointer.
- The optional SQLite checkpointer has been wired and smoke-tested, but the full
  product loop is not yet one native LangGraph run that resumes every gate after
  process restart.
- This local SQLite saver is suitable for local single-process recovery testing,
  not production multi-worker deployment.

Review:

- Subagent review returned GO.
- Accepted both medium-risk suggestions:
  - added `native_interrupt_resume_scope` to avoid implying full product-loop
    native resume;
  - changed the unavailable-package test to patch `find_spec`, so it remains
    stable even if CI installs the optional SQLite extra.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_defaults_to_nonpersistent_memory tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_requires_sqlite_path tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_sqlite_backend tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_postgres_backend tests.test_graph_gateway.GraphGatewayTests.test_default_sqlite_checkpointer_path_is_separate_from_product_ledger tests.test_graph_gateway.GraphGatewayTests.test_sqlite_checkpointer_metadata_marks_native_resume_when_package_available tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary -v
Ran 7 tests in 0.044s - OK (skipped=1)

$env:PYTHONPATH = ".tmp_tests\sqlite_pkg;src"
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_sqlite_checkpointer_metadata_marks_native_resume_when_package_available tests.test_graph_gateway.GraphGatewayTests.test_sqlite_checkpointer_can_read_interrupt_after_new_gateway_when_package_available -v
Ran 2 tests in 0.078s - OK
```

### 2026-06-01 - LG2.1 Native Terminal-Failure Gateway Roundtrip Slice

Completed:

- Added a DatasetGraph native terminal-failure review pilot:
  - new `wait_for_terminal_failure_review` node;
  - new `native_terminal_failure_review`,
    `native_terminal_failure_review_status`, and
    `native_terminal_failure_review_resume` state fields;
  - `execute_approved_code` now routes to this native interrupt only when the
    execution failed and the pilot flag is explicitly enabled.
- Added `GraphGateway.start_native_terminal_failure_review()`:
  - reuses the existing dependency gate and approved-code preflight;
  - runs DatasetGraph with `native_terminal_failure_review=True`;
  - records the failed execution through the existing canonical
    `record_execution()` path;
  - stores `native_terminal_failure_review_interrupt` metadata with boundary
    `terminal_failure_review_pilot_only`.
- Added `GraphGateway.resume_native_terminal_failure_review()`:
  - checks canonical `graph_state.json` before resuming the native checkpoint;
  - fails closed if a study-level interrupt is open or the dataset is no longer
    waiting at `terminal_failure`;
  - resumes the DatasetGraph checkpoint only after that canonical guard passes;
  - bridges the returned native human command into the existing
    `review_terminal_failure_from_command()` -> `review_terminal_failure()` ->
    `record_terminal_failure_review()` flow.
- Added regressions proving:
  - DatasetGraph can pause and resume at a native `terminal_failure` interrupt;
  - the gateway roundtrip writes the formal terminal-failure triage state and
    diagnosis/repair agent audit;
  - a changed canonical dataset interrupt blocks native resume before the
    DatasetGraph checkpoint is consumed;
  - an open study-level interrupt also blocks native resume before the
    DatasetGraph checkpoint is consumed.

Current boundary:

- This is still an internal pilot. It does not flip public FastAPI/UI execution
  to native terminal-failure resume.
- It does not run automatic repair, revise-spec, retry execution, or continue
  other datasets. It only records the human triage decision and next controlled
  action through the existing gateway flow.
- It does not change R execution quality, LLM generation, compare, static rules,
  or production sandboxing.

Review:

- Subagent review returned GO with no must-fix items.
- The review confirmed the intended boundary: DatasetGraph owns only the native
  pause/resume pilot, while GraphGateway remains the canonical product state
  transition boundary.
- Non-blocking future hardening suggestions:
  - add a guard test for the case where the pilot execution unexpectedly does
    not interrupt;
  - derive the native interrupt action list from the shared terminal-failure
    action contract to reduce drift risk.

Verification:

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_terminal_failure_review_interrupt_can_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_roundtrip_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_requires_canonical_terminal_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_rejects_study_interrupt_before_native_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes -v
Ran 5 tests in 0.474s - OK

python -B -m compileall -q src tests

git diff --check
Exited 0; CRLF warnings only.

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 296 tests in 28.761s - OK (skipped=2)
```

### 2026-06-01 - LG2.1 Native Terminal-Failure Hardening Slice

Completed:

- Followed the previous review's non-blocking hardening suggestions.
- Moved the terminal-failure human action contract into
  `src/adam_agent/graph/terminal_failure_actions.py` so DatasetGraph native
  interrupts and GraphGateway validation use the same action names.
- Added a fail-closed regression for the internal pilot start path:
  - if `start_native_terminal_failure_review()` invokes DatasetGraph but the
    graph does not stop at a native `terminal_failure` interrupt, the gateway
    raises before reading a native snapshot;
  - canonical `graph_state.json` is not polluted with terminal-failure triage or
    native interrupt metadata.

Current boundary:

- The action contract extraction does not make DatasetGraph a product state
  writer. GraphGateway still records failed execution and formal triage in
  canonical state.
- This remains an internal pilot guard, not a public UI/API behavior change.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_start_fails_closed_without_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_roundtrip_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_requires_canonical_terminal_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_rejects_study_interrupt_before_native_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_terminal_failure_review_interrupt_can_resume -v
Ran 5 tests in 0.452s - OK

python -B -m compileall -q src tests

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 297 tests in 28.550s - OK (skipped=2)

git diff --check
Exited 0; CRLF warnings only.
```

### 2026-06-01 - LG2.2 Native Dataset Product Loop Pilot Slice

Completed:

- Added internal execution mode `graph_product_full_loop`.
- Added `GraphGateway.start_native_dataset_product_loop()` for the shortest
  safe native dataset path:
  - prepare product context;
  - use an approved `input_spec` or already approved draft spec;
  - generate review-required R code;
  - stop at a native DatasetGraph `code_review` interrupt.
- Added `GraphGateway.resume_native_dataset_product_loop()`:
  - resumes the native code-review checkpoint;
  - bridges the native human command through the existing formal
    `review_code_from_command()` path so code-review JSON/hash/canonical state
    checks still run;
  - optionally calls the existing `execute_approved_code()` after approval.
- Added regressions for:
  - approve path: native loop writes formal code review and then records
    execution through the existing Gateway execution path;
  - reject path: rejected code review does not execute and leaves the dataset at
    the code-review gate.

Current boundary:

- This is an internal pilot, not a public UI/API default.
- It covers the input-spec-ready path only. Missing-spec draft-spec review,
  approved-draft-spec continuation, repair, revise-spec, and multi-dataset
  StudyGraph orchestration still remain future slices.
- DatasetGraph still does not write product state directly. GraphGateway remains
  the formal state transition boundary for review artifacts and execution.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_input_spec_executes_after_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_roundtrip_persists_formal_review_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_reject_roundtrip_keeps_execution_locked -v
Ran 4 tests in 0.794s - OK

python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_generate_code_uses_input_spec_and_stops_for_review tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_code_review_interrupt_can_resume -v
Ran 3 tests in 0.141s - OK

python -B -m compileall -q src tests
```

### 2026-06-01 - LG2.2 Native Dataset Loop Missing-Spec Slice

Completed:

- Extended the internal `graph_product_full_loop` pilot so it respects the spec
  gate before the code gate:
  - if an approved input spec or approved draft spec exists, it continues to
    native `code_review`;
  - if no approved spec exists, it generates a review-required draft spec and
    stops at native `draft_spec_review`.
- Added `GraphGateway.resume_native_dataset_product_loop_draft_spec()`:
  - resumes the native draft-spec interrupt through the existing formal
    draft-spec review artifact flow;
  - on approval, restarts the Gateway-owned native dataset product path and
    continues to native `code_review`;
  - on rejection, it does not generate R code.
- Kept the boundary unchanged: this remains an internal pilot and GraphGateway
  remains the canonical owner of draft review, code generation review, and
  execution state.

Current boundary:

- Public FastAPI/UI still uses the existing split-flow endpoints.
- The new continuation covers draft-spec approval to code review only. Code
  review approval and optional execution continue through
  `resume_native_dataset_product_loop()`.
- Repair/revise-spec automatic routing and StudyGraph multi-dataset native
  orchestration remain future slices.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_missing_spec_stops_at_draft_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_draft_approval_continues_to_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_draft_reject_does_not_generate_code -v
Ran 3 tests in 0.533s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_input_spec_executes_after_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_reject_does_not_execute -v
Ran 2 tests in 0.413s - OK
```

### 2026-06-01 - LG2.2 Native Loop Terminal-Failure Follow-Up Hardening Slice

Completed:

- Added focused coverage for terminal-failure follow-up actions through the
  internal native dataset loop:
  - `repair_code` allows the loop to regenerate code and stop at native
    `code_review`;
  - `revise_spec` routes back to draft-spec generation/review before code can
    be reused.
- Tightened `record_draft_spec_generation()` so a terminal-failure
  `revise_spec` follow-up marks the previous code state as `stale` instead of
  leaving old approved code looking current.

Current boundary:

- This is still graph-state hardening only. It does not add public UI/API
  actions or automatic repair routing.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_respects_repair_code_terminal_followup tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_routes_revise_spec_followup_to_draft_review tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_with_approved_draft_spec_generates_new_draft_and_clears_old_review tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_requires_finalize_before_regenerating_code -v
Ran 4 tests in 1.550s - OK
```

### 2026-06-01 - LG2.3 Native Study Product Loop API/UI Wiring Slice

Completed:

- Added `POST /runs/native-study-loop` as the local UI/API entrypoint for the
  internal `GraphGateway.start_native_study_product_loop()` pilot.
- Added the UI `Start Runnable Datasets` action. It uses planning selection,
  not the active detail target, then refreshes graph state, progress, and review
  read models after the call.
- Kept the product boundary explicit:
  - the study-level start only advances runnable selected datasets to
    `draft_spec_review` or `code_review`;
  - it does not approve draft specs;
  - it does not approve generated code;
  - it does not execute R.
- Fixed the checkpoint state boundary:
  - removed `llm_client_builder` and `target_context_builder` from
    `DatasetGraphState`;
  - stopped passing Python function objects through LangGraph state payloads;
  - moved runtime dependency injection to the `compile_dataset_graph(...)`
    boundary, so fake/test LLM injection still works without polluting
    checkpoint state.

Current boundary:

- This is the first local product entrypoint for the native study loop, not a
  full automatic batch executor.
- Review-required `no_dependency_evidence` can still be carried into dataset
  review gates under the current Gateway design; entering the review gate is not
  a production-grade dependency proof.
- R execution remains per-dataset and requires explicit human approval after
  code review.

Subagent review:

- 2026-06-01, Rawls, `gpt-5.5`, read-only review: GO.
- Verified no review bypass, no R execution in the endpoint, dependency-blocked
  targets not dispatched, UI uses graph-owned read models, and builder functions
  no longer enter checkpoint state.

Verification:

```text
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 312 tests in 32.118s - OK (skipped=2)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Native Resume UI Boundary Guard Slice

Completed:

- Added local UI contract coverage confirming the index page does not expose
  `native_resume.explicit_resume_endpoint` and does not include a
  `/native-resume` call path.
- Added a render harness test confirming that when
  `native_resume.available == true`, the UI only displays durable native resume
  as status text and does not create a button or endpoint call.
- Locked the current product boundary:
  - the UI may show the `Recovery` explanation;
  - default product actions still use visible split-flow review buttons;
  - the native resume endpoint remains an explicit API, not a default UI
    action.

Current boundary:

- This slice adds no button, no API, and no graph state transition.
- A UI entry for native resume should only be designed after durable
  checkpointer/native resume becomes a default product capability.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_resume_available_as_status_not_action -v
```

### 2026-06-01 - LG2.1 Service Checkpointer Backend Fail-Closed API Slice

Completed:

- Added API-level regression coverage for explicit service checkpointer backend
  configuration failures.
- When `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND=sqlite` and the optional
  `langgraph.checkpoint.sqlite` package is unavailable, `POST /runs/prepare`
  now has a test proving it fails with HTTP 400 before any graph state or
  workflow projection is written.
- When `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND=postgres` and the optional
  `langgraph.checkpoint.postgres` package is unavailable, `POST /runs/prepare`
  now has the same fail-closed API coverage.
- The tests assert that no partial product state is created:
  - no `graph_state.json`;
  - no `workflow_state.json`;
  - no `langgraph_checkpoints.sqlite`.
- The dependency probe mock is scoped to only the relevant optional
  checkpointer package names, so it does not hide unrelated imports.

Current boundary:

- This slice does not add SQLite/Postgres support to the current environment.
- It does not claim persistent native resume is available by default.
- It only locks down the API behavior when an operator explicitly enables a
  checkpointer backend that is not available: fail early, explain the missing
  dependency, and leave no half-created product run.

Subagent review:

- 2026-06-01, Fermat, `gpt-5.5`, read-only review: GO.
- Confirmed the tests exercise the real `/runs/prepare` route through
  `prepare_run_plan()`, `_open_graph_gateway()`, `_new_graph_gateway()`, and
  `GraphGateway(... checkpointer_backend=...)`.
- Confirmed no production-code change is required for this slice.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_sqlite_checkpointer_unavailable tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_postgres_checkpointer_unavailable tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_uses_run_scoped_sqlite_path_when_enabled tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_rejects_unknown_backend -v
Ran 4 tests in 0.079s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_sqlite_checkpointer_unavailable tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_postgres_checkpointer_unavailable -v
Ran 2 tests in 0.075s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 126 tests in 16.288s - OK

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.1 Service Gateway Lifecycle Boundary Slice

Completed:

- Added `_open_graph_gateway(study_dir=None, run_id=None)` as the
  service-scoped lifecycle boundary for `GraphGateway`.
- The helper still delegates construction to `_new_graph_gateway()`, but now
  guarantees `gateway.close()` in a `finally` block.
- Migrated service-layer gateway use to the context helper across:
  - upload invalidation;
  - legacy `/runs` shim;
  - dependency planning;
  - native study product loop start;
  - dependency, draft-spec, code, and terminal-failure review gates;
  - finalize inputs, draft spec generation, code generation, approved-code
    execution;
  - graph-state, progress, compare, and review-summary read models.
- Added service-layer contract tests:
  - service helpers cannot call `_new_graph_gateway()` directly except through
    `_open_graph_gateway()`;
  - `_open_graph_gateway()` closes the gateway after use.

Current boundary:

- This slice does not change the default public API/UI backend. Memory remains
  the default unless `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND` is explicitly set.
- It does not enable full graph-native product resume by itself.
- The purpose is to make the service layer safe for the already-added
  run-scoped SQLite checkpointer path by closing per-request gateway resources.
- `GraphGateway` still owns workflow state transitions; FastAPI service code
  remains request validation, response shaping, and read-model plumbing.

Subagent review:

- 2026-06-01, Dewey, `gpt-5.5`, read-only review: GO.
- Confirmed all service-layer gateway use flows through `_open_graph_gateway()`;
  default memory behavior and run-scoped sqlite behavior remain unchanged;
  exception paths close resources; no double-close or lazy-result lifetime risk
  was found.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_service_layer_constructs_graph_gateway_only_through_factory tests.test_api_phase8.Phase8ApiTests.test_service_layer_opens_graph_gateway_through_context_helper tests.test_api_phase8.Phase8ApiTests.test_open_graph_gateway_closes_gateway_after_use tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_defaults_to_memory_backend tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_uses_run_scoped_sqlite_path_when_enabled tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_falls_back_to_memory_without_run_context tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_rejects_unknown_backend -v
Ran 7 tests in 0.096s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 124 tests in 15.848s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.1 Service Checkpointer Backend Config Slice

Completed:

- Extended `_new_graph_gateway(study_dir=None, run_id=None)` into the single
  service-layer runtime checkpointer/backend configuration point.
- Default behavior is unchanged:
  - no environment variable means plain `GraphGateway()`;
  - public API/UI still uses the memory checkpointer unless explicitly
    configured.
- Added opt-in `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND` handling:
  - `memory` returns the default gateway;
  - `sqlite` uses the run-scoped
    `runs/{run_id}/langgraph_checkpoints.sqlite` path when a run context is
    available;
  - `sqlite` without `study_dir` or `run_id` falls back to memory for
    study-wide operations such as upload invalidation;
  - `postgres` delegates to `GraphGateway` and fails closed until lifecycle
    management is wired;
  - unknown backend names fail closed with `ApiServiceError`.
- Kept the runtime checkpoint store separate from the product audit ledger:
  `langgraph_checkpoints.sqlite` is not `graph_checkpoints.sqlite`.
- Added tests for default memory behavior, run-scoped sqlite routing,
  no-context memory fallback, and unknown backend rejection.

Current boundary:

- This slice provides a configuration entry point only. It does not claim the
  complete product flow is now a persistent native LangGraph run.
- SQLite still requires the optional `langgraph-checkpoint-sqlite` dependency.
- Full product interrupt/resume, automatic repair loops, and production
  Postgres lifecycle management remain later work.

Subagent review:

- 2026-06-01, Peirce, `gpt-5.5`, first read-only review: NO-GO.
- Blocking issue: an environment override for the sqlite path could have made
  multiple runs share one checkpoint database, and could have forced sqlite on
  study-wide operations without run context.
- Fix applied:
  - removed the service-level sqlite path override;
  - sqlite now always derives from `default_sqlite_checkpointer_path(study_dir,
    run_id)` when run context exists;
  - sqlite without run context falls back to memory.
- 2026-06-01, Einstein, `gpt-5.5`, read-only re-review: GO.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_defaults_to_memory_backend tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_uses_run_scoped_sqlite_path_when_enabled tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_falls_back_to_memory_without_run_context tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_rejects_unknown_backend tests.test_api_phase8.Phase8ApiTests.test_service_layer_constructs_graph_gateway_only_through_factory -v
Ran 5 tests in 0.040s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 122 tests in 15.958s - OK

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.1 Service Gateway Factory Boundary Slice

Completed:

- Added `_new_graph_gateway()` in `api/service.py` as the single service-owned
  construction boundary for `GraphGateway`.
- Replaced direct `GraphGateway()` construction inside service helpers with the
  new factory, including upload invalidation, legacy run shim, prepare,
  native study loop, review gates, execution, progress, graph-state, compare,
  and review-summary helpers.
- Added a service-layer contract test that fails if any service helper other
  than `_new_graph_gateway()` directly constructs `GraphGateway`.
- Kept the current runtime behavior unchanged: the factory still returns
  `GraphGateway()` with default arguments and therefore keeps the current memory
  checkpointer default.

Current boundary:

- This slice does not enable SQLite/Postgres checkpointer persistence.
- It does not change FastAPI routes, workflow state transitions, review gates,
  LLM generation, R execution, or artifact writing.
- The purpose is to keep future checkpointer/backend configuration behind one
  service boundary instead of letting each endpoint choose separately.

Subagent review:

- 2026-06-01, Averroes, `gpt-5.5`, read-only review: GO.
- Confirmed no business behavior change, no direct service-layer
  `GraphGateway()` call remains outside the factory, and existing tests can
  still patch `adam_agent.api.service.GraphGateway`.
- Non-blocking suggestion: the AST guard does not catch alias tricks such as
  assigning `Gateway = GraphGateway`; a convention comment was added near the
  factory.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_service_layer_constructs_graph_gateway_only_through_factory tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods tests.test_api_phase8.Phase8ApiTests.test_service_layer_no_longer_writes_workflow_state_directly tests.test_api_phase8.Phase8ApiTests.test_service_layer_reads_graph_state_only_for_explicit_read_models tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model -v
Ran 5 tests in 0.120s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 118 tests in 15.949s - OK

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.7 Split Code Approval And Execution UI Slice

Completed:

- Split the browser's old combined code-review action into two explicit graph
  actions:
  - `Approve Code` records the human code-review decision only.
  - `Run Approved Code` executes already approved code through the local R
    boundary only.
- Updated the graph-aware UI gate mapping:
  - `review_code` enables `Approve Code`;
  - `execute_approved_code` and `retry_approved_execution` enable
    `Run Approved Code`.
- Removed the hidden approve-then-execute path from the browser handler. The UI
  no longer calls `/code-review` and `/execute-approved-code` from the same
  button.
- Kept the backend boundary unchanged: service and Gateway gates still own the
  actual approval and execution checks.
- Added UI contract and Node harness coverage proving:
  - the old combined label is absent;
  - approval calls `/code-review` only;
  - execution calls `/execute-approved-code` only;
  - the next-action matrix separates approval from execution.

Current boundary:

- This is a UI/gate-alignment slice. It does not change dependency planning,
  LLM generation, generated R content, R execution internals, or compare
  behavior.
- The graph progress read model remains the authority for next action. The
  browser review cache is display state only; the backend still rejects
  execution without graph-owned code approval.

Subagent review:

- 2026-06-01, Lovelace, `gpt-5.5`, read-only review: GO.
- Confirmed that no hidden approve+execute UI path remains and that backend
  review/execution gates remain separate.
- Non-blocking suggestion: execution availability could also consider the
  browser's local review cache for clearer disabled messaging. This was not
  applied because graph `next_action` is the canonical workflow state and the
  backend already fail-closes if approval is missing.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_explains_disabled_actions_from_existing_state tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_code_approval_and_execution_are_separate_ui_actions -v
Ran 5 tests in 0.292s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 117 tests in 15.741s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Review Gate Action Read-Model Slice

Completed:

- `GraphGateway.progress_summary()` now exposes graph-owned
  `available_actions` in dataset progress:
  - `draft_spec_review`: `approve` / `reject`;
  - `code_review`: `approve` / `reject`;
  - `terminal_failure`: the existing triage actions.
- `available_actions` is a read-model hint only:
  - draft-spec approval still goes through the existing draft-spec review
    Gateway flow;
  - code approval still goes through the existing code-review Gateway flow;
  - R execution still requires explicit approved-code execution.
- Dataset `available_actions` is cleared when dependency, stale-plan, or
  study-level gates block the dataset, so the UI does not imply a bypass.
- Terminal-failure triage actions are no longer shown after a human triage
  decision has already been recorded.
- The human review queue now projects the same `available_actions` to the UI
  and renders them as text-only "Available graph actions" hints, not clickable
  commands.
- A draft-spec review action-hint regression test was added after subagent
  review noted the gap.

Current boundary:

- This slice adds no approve/reject endpoints.
- It does not change dependency planning, draft/code review validation, LLM
  generation, R execution, compare, repair/spec-revision, or native
  checkpointer behavior.
- The UI renders graph read-model state; it does not become a browser-side
  workflow state machine.

Subagent review:

- 2026-06-01, Poincare, `gpt-5.5`, read-only review: GO.
- Confirmed:
  - `available_actions` remains read-model data and does not add an execution
    path;
  - blocked/stale/study-level dependency gate cases clear dataset actions;
  - terminal-failure triage actions are hidden after triage review;
  - UI action hints are escaped text only, not buttons and not wired to fetch or
    click handlers;
  - dataset progress and review queue stay consistent for the covered
    code-review path.
- Non-blocking suggestion: add explicit `draft_spec_review` action-hint
  coverage. This was implemented before commit.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_review_gate_actions_when_unblocked tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_draft_spec_review_actions tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed -v
Ran 3 tests in 0.161s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_exposes_terminal_failure_triage_actions -v
Ran 2 tests in 0.078s - OK

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 236 tests in 25.074s - OK (skipped=2)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Stale Study Loop Result Guard Slice

Completed:

- Tightened the graph-owned progress read model so `study_loop_result` is hidden
  whenever the dependency plan is stale:
  - `dependency_plan.plan_stale == true`; or
  - `dependency_review_status == "stale"`.
- This prevents the UI from showing a previous native study-loop start after
  study input files changed and the graph requires re-planning.
- The change is intentionally a read-model guard. It does not delete
  `runtime_persistence.native_study_product_loop`, mutate historical metadata,
  change dependency planning, approve reviews, generate code, or run R.
- Added a gateway regression test:
  - start a native study loop for ADAE/ADCM;
  - verify `study_loop_result` is projected;
  - mutate `input_sdtm/ae.csv`;
  - mark inputs changed;
  - verify progress is stale and `study_loop_result == {}`.

Subagent review:

- 2026-06-01, Linnaeus, `gpt-5.5`, read-only review: GO.
- It confirmed the guard does not hide normal dataset review queues because
  `review_queue` remains independently exposed, and agreed the read-model
  boundary is safer than deleting runtime persistence metadata.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_hides_study_loop_result_after_inputs_change tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_prioritizes_stale_plan_replan -v
Ran 4 tests in 0.675s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_clears_stale_study_loop_result_when_progress_has_none tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation -v
Ran 4 tests in 0.384s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 118 tests in 8.436s - OK (skipped=2)
```

### 2026-06-01 - LG2.7 Study Loop Result Progress Read-Model Slice

Completed:

- Added `study_loop_result` to the graph-owned run progress read model.
- `GraphGateway.progress_summary()` now projects the latest native study-loop
  summary from canonical graph state metadata:
  - `runtime_persistence.native_study_product_loop.started_datasets`;
  - `runtime_persistence.native_study_product_loop.blocked_datasets`;
  - graph-owned `review_queue`.
- `RunProgressResponse` now exposes `study_loop_result`.
- The UI now restores the Study Loop Result panel from
  `progress.study_loop_result` after refresh or progress reload.
- The previous command response remains only a display fallback. It cannot
  override `source: graph_progress` after `refreshGraphReadModels()`.
- When a study-loop result is restored from progress without fresh
  `dataset_results`, the UI uses graph `review_queue` to recover the concrete
  review gate label for started datasets.

Current boundary:

- `study_loop_result` is a read model only. It does not participate in
  dependency planning, dataset dispatch, draft/code approval, LLM generation,
  or R execution.
- `runtime_persistence` remains metadata about native loop/checkpoint
  boundaries; the UI does not write it.

Subagent review:

- 2026-06-01, Erdos, `gpt-5.5`, first read-only review: NO-GO.
- Required fix: `startNativeStudyLoop()` refreshed graph progress and then
  overwrote the restored `graph_progress` result with the command response.
- Fix applied: command response is now written only when there is no
  graph-progress result; added a Node harness test covering this exact
  priority.
- 2026-06-01, Erdos, `gpt-5.5`, second read-only review: GO.
- 2026-06-01, Euclid, `gpt-5.5`, read-only review after Chinese-doc sync: GO.
  Its non-blocking suggestions were absorbed before commit:
  - empty `progress.study_loop_result` now clears stale browser display state
    instead of leaving the previous run's panel visible;
  - API coverage now verifies both ADAE and ADCM are present in the nested
    `study_loop_result.review_queue`.
- 2026-06-01, Raman, `gpt-5.5`, final read-only review of the absorbed
  suggestions: GO.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress tests.test_api_phase8.Phase8ApiTests.test_index_start_study_loop_keeps_graph_progress_result_over_command_response -v
Ran 7 tests in 0.758s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress -v
Ran 5 tests in 0.823s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 116 tests in 15.760s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 117 tests in 8.797s - OK (skipped=2)

python -B -m unittest tests.test_graph_smoke -v
Ran 84 tests in 7.517s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Study Loop Result Read-Model Slice

Completed:

- Added a `Study Loop Result` panel to the local UI dashboard.
- The panel renders only data returned by `/runs/native-study-loop` and the
  graph progress read model:
  - datasets started by the study-level command;
  - datasets that stayed blocked by dependency/user action;
  - review gates still open in the graph review queue.
- Kept the product boundary explicit in the UI:
  - `Start Runnable Datasets` moves datasets to review gates only;
  - it does not approve draft specs;
  - it does not approve generated code;
  - it does not run R.
- The browser stores the last command response as `lastStudyLoopResult` only
  for display. It is not sent back as workflow state and does not affect graph
  decisions.
- Added a Node harness test that executes `renderStudyLoopResult()` with
  started, blocked, and queued-review data, so the panel is tested as rendered
  UI, not only as static HTML text.

Current boundary:

- This is a UI read-model improvement only. It does not change dependency
  planning, Gateway dispatch rules, approval gates, LLM generation, or R
  execution.
- The panel summarizes the latest study-loop start in the current browser
  session. Canonical workflow truth remains graph state and graph progress.

Subagent review:

- 2026-06-01, Anscombe, `gpt-5.5`, read-only review: GO.
- Verified that the UI does not add hidden approve/run paths and does not
  replace graph state with a local state machine.
- Non-blocking suggestion was to add a Node harness render test; this was
  implemented before commit.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary -v
Ran 2 tests in 0.120s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_does_not_start_dependency_blocked_targets -v
Ran 4 tests in 0.407s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Dependency Assumption UI Wording Slice

Completed:

- Tightened UI wording around `no_dependency_evidence` and Start Runnable
  Datasets:
  - entering a review gate is not described as dependency proof;
  - missing upstream ADaM evidence is explicitly something to confirm during
    spec/code review;
  - both the action reason and transient operation message now say:
    `This is not dependency proof. R will not run.`
- This slice only changes user-facing copy and UI contract tests. It does not
  change dependency planning, graph state, Gateway gates, review gates, or R
  execution behavior.

Subagent review:

- 2026-06-01, Tesla, `gpt-5.5`, read-only review: GO.
- Confirmed that the change is copy/test only and stays aligned with the
  Reference ADaM policy and explicit per-dataset human-approved execution
  boundary.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
Ran 3 tests in 0.119s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 111 tests in 15.240s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Native Resume Progress Read-Model Slice

Completed:

- Added a stable `native_resume` object to `GET /runs/{run_id}/progress`.
- The field is derived from canonical `graph_state.runtime_persistence`:
  - `available`
  - `scope`
  - `boundary`
  - `explicit_resume_endpoint`
  - `default_review_path`
  - `restart_recovery_source`
  - `message`
- The default memory checkpointer path reports:
  - `available == false`
  - `scope == "none"`
  - `boundary == "graph_state_projection_only"`
  - `default_review_path == "split_flow_review_endpoints"`
- The UI Study Progress panel now adds a plain `Recovery` step:
  - when native resume is unavailable, it tells users to use the visible review
    buttons and says restart recovery reads saved graph state;
  - when durable native resume is available, it says the scope in
    human-readable wording.

Current boundary:

- This is only a read-model and UI clarity slice.
- It adds no button, no new workflow command, and no state transition path.
- The explicit native resume endpoint still fails closed with the default
  memory checkpointer.

Follow-up cleanup:

- `study_loop_result.native_resume_available/native_resume_scope/resume_boundary`
  now reuse the same `native_resume` helper instead of recomputing the fields.
- Renamed `native_resume.endpoint` to `native_resume.explicit_resume_endpoint`
  so automated clients do not mistake it for the default review path.
- Added assertions that `study_loop_result` and top-level `native_resume`
  remain consistent.

Subagent review:

- 2026-06-01, Harvey, `gpt-5.5`, read-only review: GO.
- Confirmed:
  - `native_resume` stays consistent with `runtime_persistence`;
  - UI wording does not imply default native resume is available;
  - no product workflow path or GraphGateway bypass was introduced;
  - tests cover the default unavailable path.
- Non-blocking suggestion: add a positive progress read-model test for the
  optional SQLite checkpointer path. This was absorbed as a skip-if-unavailable
  test and does not fake local durable resume support.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_sqlite_progress_marks_native_resume_when_package_available tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions -v
Ran 4 tests in 0.253s - OK (skipped=1)

python -B -m unittest tests.test_graph_gateway -v
Ran 122 tests in 9.583s - OK (skipped=3)

python -B -m unittest tests.test_api_phase8 -v
Ran 127 tests in 17.797s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Native Resume Boundary Read-Model Slice

Completed:

- Added native resume boundary fields to graph-owned
  `study_loop_result` progress:
  - `native_resume_available`
  - `native_resume_scope`
  - `resume_boundary`
- These fields are projected only from `runtime_persistence`. They do not
  participate in dependency planning, dataset dispatch, draft/code review, LLM
  generation, or R execution.
- With the default memory checkpointer, `study_loop_result` now explicitly
  reports:
  - `native_resume_available == false`
  - `native_resume_scope == "none"`
  - `resume_boundary == "graph_state_projection_only"`
- The UI Study Loop Result detail now tells users:
  - default recovery uses saved graph state;
  - durable LangGraph checkpoint resume is not enabled for the run;
  - Start Runnable Datasets still does not approve draft specs, approve code,
    or run R.
- After subagent review, the UI no longer displays internal scope values such
  as `native_pilot_interrupts_only` directly. It maps that value to the
  user-facing phrase “pilot graph interrupts only”.

Current boundary:

- This is a read-model/UX boundary slice. It adds no approve/reject endpoint.
- It does not claim the default product flow is full native LangGraph
  interrupt/checkpointer resume.
- Public split-flow review/execute endpoints remain the current product path;
  the native study loop is still a pilot that dispatches datasets to review
  gates.

Subagent review:

- 2026-06-01, Hubble, `gpt-5.5`, read-only review: GO.
- Confirmed:
  - the new fields accurately come from `runtime_persistence`;
  - no new workflow state machine, approve/reject path, service state fork, or
    execution-path change was introduced;
  - the UI default-path wording clearly says this is saved graph state recovery,
    not durable LangGraph checkpoint resume;
  - API progress and UI contract tests cover the key boundary.
- Non-blocking suggestion: avoid rendering internal scope enum values in the
  true-case UI text. This was absorbed before commit.

Verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets -v
Ran 5 tests in 0.717s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 120 tests in 9.482s - OK (skipped=2)

python -B -m unittest tests.test_api_phase8 -v
Ran 126 tests in 17.299s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.1/LG2.8 Native Resume Endpoint Fail-Closed Slice

Completed:

- Added explicit API:
  `POST /runs/{run_id}/datasets/{dataset}/native-resume`.
- Added `NativeDatasetResumeRequest` / `NativeDatasetResumeResponse`. The
  request model only keeps fields actually needed by native resume:
  - `study_dir`
  - `decision`
  - `reviewer`
  - `notes`
  - `execute_after_approval`
  - `rscript_path`
- `GraphGateway.resume_native_dataset_interrupt()` is now the single native
  dataset interrupt resume entry point, but it first checks whether durable
  native resume is available.
- With the default memory checkpointer, the endpoint fails closed:
  - HTTP 400;
  - clear message that native LangGraph interrupt resume is not enabled;
  - no fallback to ordinary code-review/draft-spec-review paths;
  - no `*_code_review.json` review artifact is written.
- The entry point routes by interrupt type to the existing native resume
  methods only after the durable check passes:
  - `draft_spec_review`
  - `code_review`
  - `terminal_failure`

Current boundary:

- This is a public interface reserved for future durable checkpointer/native
  resume. It does not change the current default split-flow product path.
- The current local environment does not have `langgraph-checkpoint-sqlite`, so
  the positive durable resume path is not enabled or faked.
- This slice adds no UI button and does not imply that the default product flow
  is already complete native resume.

Subagent review:

- 2026-06-01, Sartre, `gpt-5.5`, read-only review: GO.
- Confirmed:
  - the default API path uses memory backend and returns 400;
  - `GraphGateway.resume_native_dataset_interrupt()` checks durable native
    resume before loading state or calling specific resume methods;
  - unsupported native resume cannot write `*_code_review.json`;
  - the service layer delegates to gateway and formats the result without adding
    a new state fork.
- Non-blocking suggestion: remove unused LLM/config fields from the request
  model so the endpoint does not imply draft-spec-to-code continuation. This was
  absorbed before commit.

Verification:

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer tests.test_api_phase8.Phase8ApiTests.test_native_resume_endpoint_fails_closed_without_durable_checkpointer tests.test_api_phase8.Phase8ApiTests.test_service_layer_reads_graph_state_only_for_explicit_read_models -v
Ran 3 tests in 0.319s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 121 tests in 9.261s - OK (skipped=2)

python -B -m unittest tests.test_api_phase8 -v
Ran 127 tests in 17.334s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```
