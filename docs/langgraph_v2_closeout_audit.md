# LangGraph-v2 Closeout Audit

Snapshot date: 2026-06-01

Branch at audit time: `LangGraph-v2`

Purpose:

This document records where LangGraph-v2 stands after the recent graph-state,
native study-loop, native-resume read-model, UI, and compatibility hardening
slices. It is a handoff document for the next implementation phase.

The main conclusion is deliberately narrow:

> LangGraph-v2 has moved product state ownership behind `GraphGateway` and
> canonical `graph_state.json`. It has not yet turned the whole product into one
> complete native LangGraph run with durable interrupt resume.

## 1. What Counts As Done

A phase is counted as done only when current code and tests show the product
behavior exists, not merely when the idea appears in documentation.

For this audit, "done" means:

- the behavior is implemented in `src/`
- the behavior is exercised by tests
- the construction plan records the boundary
- a subagent review did not find a major blocker
- remaining limits are stated without claiming production readiness

## 2. Phase Status

| Phase | Current Status | Evidence | Remaining Boundary |
|---|---|---|---|
| LG2.0 State contract | Mostly complete | `StudyRunState`, `DatasetRunState`, `InterruptState`, `HumanCommand`; state schema tests | Full product execution is still not a single native run |
| LG2.1 GraphGateway/checkpointing | Partly complete | Product state-changing endpoints now delegate to `GraphGateway`; service construction/lifecycle tests; guarded optional SQLite checkpointer | Default product path still uses memory checkpointer and split-flow endpoints; full durable native resume is not product-default |
| LG2.2 DatasetGraph product nodes | Mostly complete | Product prepare/code/execute paths use DatasetGraph/Gateway gates; ADSL is no longer special; legacy stubs isolated | Continuous native graph loop from spec/code review to execution/repair is still incomplete |
| LG2.3 StudyGraph multi-dataset orchestration | Partly complete | Multi-target planning, dependency gates, persisted dataset states, native study-loop pilot, runtime dependency output gate | Study loop starts runnable datasets to review gates; it does not automatically approve, execute, or complete all targets |
| LG2.4 Multi-agent node model | Partly complete | Typed agent IO, agent decisions, agent audit summary, bounded roles in graph/audit records | Agents are packaging/audit roles, not full specialist autonomous nodes with all reference/repair behavior |
| LG2.5 Reference/static rule layer | Partly complete | Generic static-rule engine, policy/report validation, rule-pack admission metadata, no demo-specific rule authority | CDISC/P21/company standards retrieval and production-grade rules are not complete |
| LG2.6 R sandbox boundary | Partly complete | `SandboxRunner` protocol, local Rscript sandbox wrapper, path/env/argument/preflight guards | Local Rscript is not OS/container isolation and must stay labeled developer mode |
| LG2.7 UI as graph-state viewer | Mostly complete for MVP | UI reads graph progress, persistent dataset cards, graph-owned actions, dependency map wording, native resume status-only panels | UI still drives split-flow actions; it is not yet a native LangGraph interrupt client for all gates |
| LG2.8 compatibility/deprecation | Mostly complete for MVP | Legacy `/runs` blocked for real LLM flow, compatibility metadata, graph-first read paths, fail-closed corrupt graph reads | Compatibility shims remain; legacy stub/test paths have not been removed |

## 3. Most Important Completed Architecture Changes

1. Canonical product state moved to `graph_state.json`.

   `workflow_state.json` still exists, but it is now a compatibility projection.
   Product transitions should enter through `GraphGateway`, not through service
   helpers writing their own workflow state.

2. ADSL is no longer a special deterministic R-template product path.

   ADSL, ADAE, ADCM, ADLB, and future ADxx targets share the same product idea:
   approved input spec or approved draft spec -> LLM-generated R -> review gate
   -> approved local execution -> validation/audit.

3. Reference ADaM has been constrained to evidence use.

   Reference ADaM can support output-shape comparison and dependency
   availability evidence. It cannot silently define derivation logic or satisfy
   runtime dependency requirements unless a graph-owned real output artifact
   exists.

4. Native study-loop pilot exists.

   `/runs/native-study-loop` can dispatch multiple runnable datasets to review
   gates and preserve existing dataset progress. It exposes study-loop result
   and native resume queue status through graph progress/read-model fields.

5. Native resume is explicit and fail-closed.

   The explicit native resume endpoint exists for future durable checkpointer
   use. Under the default memory checkpointer it fails closed, and the UI shows
   native resume as status only.

6. Static rules are governed, not demo patches.

   The current static layer blocks generic execution/artifact contract failures
   and admits standards/company rules only through rule-pack metadata. It does
   not claim full CDISC/P21 compliance.

7. R execution has a replaceable boundary.

   Generated code execution now passes through a sandbox interface. The current
   local backend has safety guards, but it is not production isolation.

## 4. Main Remaining Work

These are not small UI polish tasks. They are the real remaining architecture
items.

### R1. Full Native Product Run

Current state:

- Product actions are graph-owned through `GraphGateway`.
- The browser still drives the workflow by calling split-flow endpoints:
  prepare, finalize, draft-spec review, generate code, code review, execute,
  terminal-failure review.

Needed:

- One graph-native product run should be able to pause at each human gate and
  resume through LangGraph interrupts.
- Dependency review, draft-spec review, code review, and terminal-failure
  review should become product-default, end-to-end native graph interrupts.
  Existing pilot/native-resume work proves parts of the boundary, but the full
  runtime still falls back to gateway-recorded split-flow state transitions.

### R2. Durable Checkpointer As Product Capability

Current state:

- The code has a checkpointer boundary and optional SQLite wiring.
- Default local product flow still uses memory checkpointer behavior.
- `graph_state.json` provides restart read-model recovery, but not full native
  LangGraph checkpoint resume for every gate.

Needed:

- Decide the product-default local durable backend.
- Add restart tests for full product gates, not only pilot interrupts.
- Make UI resume commands use durable native resume only when the run proves it
  is supported.

### R3. Study-Level Multi-Dataset Execution Loop

Current state:

- Multi-target planning and dependency gating exist.
- Native study-loop starts runnable datasets to review gates.
- Runtime dependencies require real graph-owned output artifacts.

Needed:

- A study loop that can continue after approvals and run batches in dependency
  order.
- No silent upstream ADaM auto-run. The user must still approve dependency
  generation or provide the dependency output.
- Per-dataset cards must remain stable as datasets move through different gates.

### R4. Repair And Spec-Revision Closed Loop

Current state:

- Terminal failure triage is controlled and auditable.
- The system can record next actions such as retry execution, repair code, or
  revise spec.

Needed:

- Actual native routing from failure diagnosis into repair-code or revise-spec
  nodes.
- Bounded repair attempts per dataset.
- A clear terminal failure when repair/spec revision fails.

### R5. Real Specialist Agent Behavior

Current state:

- Agent IO and audit packaging exist.
- Roles are bounded and visible.

Needed:

- Evidence, spec, code, static review, execution, validation, repair, reference,
  and audit should become deeper graph nodes with typed inputs/outputs.
- These agents must still be controlled tools, not free-form processes.

### R6. Standards And Reference Retrieval

Current state:

- Local reference store and rule-pack admission boundaries exist.
- Static checks are generic and limited.

Needed:

- Real CDISC/P21/company-standard reference assets.
- Search/lookup tools that LLM nodes can call.
- Clear authority metadata for every blocking standards rule.

### R7. Production Sandbox And Deployment Controls

Current state:

- Local Rscript sandbox wrapper exists and has path/env/argument guards.

Needed:

- Container or OS-level isolation before any production safety claim.
- Network and filesystem controls enforced below the application layer.
- Resource limits and operational logging.

## 5. What Should Not Be Done Next

Do not:

- add more UI buttons that bypass graph gates
- reintroduce an ADSL-specific template path into product flow
- treat reference ADaM as derivation authority
- add demo-shaped static rules inside the generic engine
- claim CDISC/P21 compliance from current static checks
- make native resume appear callable under memory checkpointer mode
- let FastAPI service helpers write independent workflow state

## 6. Recommended Next Stage

Use a new stage called:

`LG3.0 Native Product Run And Durable Resume`

Suggested first slice:

1. Define the native full-run state machine for one dataset.
2. Pick one gate sequence:
   `input_spec -> generate_code -> code_review interrupt -> execute`.
3. Implement it behind `GraphGateway` without changing the browser UI first.
4. Add restart/resume tests with the configured durable checkpointer.
5. Only then expose the result through the UI.

Why this order:

- If UI is changed first, it will become another split-flow controller.
- If rules are expanded first, they will attach to an unfinished orchestration
  model.
- If repair is expanded first, it will still need a durable native run to route
  correctly.

## 7. Verification Snapshot

Recent verification already run in the latest slices:

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 283 tests in 29.848s - OK (skipped=4)

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets -v
Ran 2 tests in 0.495s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

This evidence supports the current split-flow and read-model state. It does not
prove the unfinished native full-run capability.

## 8. Closeout Decision

LangGraph-v2 should be considered:

- complete enough as a state-ownership and compatibility-migration milestone
- incomplete as a full native LangGraph product runtime
- not production-ready for clinical submission use

The next implementation should stop adding read-model polishing unless it
unblocks native full-run behavior. The main engineering target should now move
from "make split-flow graph-owned" to "make the graph itself run and resume the
product flow."

## 9. Subagent Review

Review:

- 2026-06-01, Gibbs, `gpt-5.5`, read-only review: GO.

Confirmed:

- The document does not overstate LangGraph-v2 completion.
- It correctly distinguishes graph-owned split flow from a true durable native
  LangGraph product runtime.
- It includes the major remaining work: durable full-run resume, multi-dataset
  execution after approvals, repair/spec-revision routing, real specialist
  agents, CDISC/P21/company rule assets, production sandboxing, and legacy stub
  cleanup.
- The LG3.0 first slice is aimed at backend/native runtime behavior, not more
  UI/read-model polish.

Adjustment made after review:

- Clarified R1 wording so it acknowledges existing pilot/native-resume work
  while still marking product-default, end-to-end native interrupts as
  unfinished.
