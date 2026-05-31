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
  old synthetic `*_stub` nodes.
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
