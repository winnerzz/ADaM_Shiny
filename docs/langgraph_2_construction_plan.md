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
- `src/adam_agent/graph/workflow_state.py` still persists the compatibility
  `workflow_state.json` read model and a SQLite sidecar history for the current
  UI/API flow.
- `src/adam_agent/llm/`, `src/adam_agent/downstream/`, and
  `src/adam_agent/tools/` already provide useful tool boundaries.
- ADSL has been corrected to follow the same ADaM product flow as other AD
  targets. The old `src/adam_agent/adsl/` package is legacy/regression only.

Current architectural deviations:

- The real human-in-the-loop workflow is mostly in FastAPI service functions,
  not in LangGraph `interrupt` nodes.
- `workflow_state.json` still exists as a UI/API compatibility read model, but
  product truth is converging on canonical `graph_state.json`; remaining direct
  compatibility writes must be treated as legacy surfaces to remove or isolate.
- `DatasetGraph` still contains early `*_stub` nodes. The graph is not yet the
  full product workflow.
- The current product is a controlled pipeline with LLM calls, not yet a true
  multi-agent graph with explicit specialist roles.
- UI dataset cards and target switching still behave more like a single-target
  controller than a graph view over multiple persistent dataset runs.
- Static ADaM/CDISC checks now have a limited policy-driven gate, but they are
  not a full rules engine.
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
  - Service-level wrappers still map graph state back to the existing UI
    response models and `workflow_state.json` projection for compatibility.
- Still open:
  - Graph-native resume from `code_review` into R execution.
  - Graph-native validation, compare, terminal failure routing, and repair.
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

Add standards-aware check boundaries without pretending to be production
complete. Static checks must be generic policy checks, not demo-specific ADaM
derivation rules.

Design principle:

- Static checks are a policy/rule-pack layer, not a growing list of one-off
  patches for PSY201, ADAE, ADSL, or any single demo variable.
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
- There is no "exception registry" inside the generic engine. If a future issue
  seems to require an exception, the engineering response must be one of:
  revise the approved spec contract, add a source-backed rule-pack item, or keep
  the issue as a non-blocking reviewer note until it has proper authority.
- When a check is heuristic or incomplete, it must be warning/informational and
  must record that it does not prove clinical correctness.
- Every static finding must carry rule-governance metadata:
  - `category`: one of `artifact_contract`, `execution_boundary`,
    `spec_contract`, or `standards_pack`
  - `source_type`: one of `system_contract`, `approved_spec`,
    `standards_pack`, or `user_policy`
  - optional `source_id`: spec artifact id, rule-pack id, or policy id
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
- Add a rule-design review checklist for every future static-check PR:
  - What declared contract is being checked?
  - Where does the rule authority come from: system contract, approved spec,
    user policy, or versioned rule pack?
  - Is the rule independent of demo-study names and file-specific observations?
  - If it blocks a run, where are authority_type, source, version, scope,
    severity, and evidence recorded?
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
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_graph_gateway.GraphGatewayTests.test_gateway_generate_code_does_not_accept_external_dependency_artifacts tests.test_api_phase8.Phase8ApiTests.test_execute_rejects_changed_runtime_dependency_artifact -v
```

Result: 5 focused tests passed.

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_starts_plan_and_blocks_unresolved_dependency tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_returns_plan_when_open tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_generate_code_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_draft_spec_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry -v
python -m compileall -q src\adam_agent
```

Result: 6 gateway tests passed; 5 API tests passed; compileall passed.

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

Current boundary:

- This does not change the compare algorithm. It is still an initial CSV
  structural and sampled-cell comparison, not a clinical conformance validator.
- Reference ADaM remains comparison/output-shape evidence only. This slice does
  not make reference ADaM a derivation authority.
- Static checks remain generic contract/rule-pack checks. This slice does not
  add clinical, dataset-specific, study-specific, or demo-specific rules.

Focused verification:

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_does_not_create_graph_state_without_prepared_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_updates_graph_compare_when_reference_disappears tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_writes_compare_report_artifact_when_requested tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_requires_existing_graph_state -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
```

Result: 6 focused compare tests passed; 178 related gateway/API/graph/static/
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
- Dependency-plan gating still lives in the service compatibility wrapper for
  this slice.
- `finalize-inputs` still calls `DatasetGraph` directly from the service and is
  the remaining large dataset product step to migrate.
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
- Dependency-plan gating still lives in the service compatibility wrapper for
  this slice.
- The separate explicit `/draft-spec` endpoint still performs its own draft
  generation service flow and records the result through `GraphGateway`; this
  was left out of scope to keep the migration slice bounded.
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
- Added a positive terminal-failure retry regression: after a human
  `retry_execution` review, `/execute-approved-code` is allowed to enter the
  graph-owned `graph_product_execute` path and records the retry follow-up as
  consumed by execution.

Current boundary:

- The static-rule guard is intentionally about the generic engine. Dataset names
  and standards terms may still appear in tests or future versioned rule-pack
  fixtures.
- The retry regression uses a mocked DatasetGraph return value. It proves the
  compatibility wrapper gate and graph-state recording path, not real R
  execution.

Verification:

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_generated_code tests.test_static_rules tests.test_sandbox -v
```

Result: 14 static-rule tests passed; 3 focused retry tests passed; 205 core
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
