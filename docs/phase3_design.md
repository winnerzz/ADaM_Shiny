# Phase 3 Design - LangGraph Skeleton

Last updated: 2026-05-22

Phase status: skeleton implemented and ready for review

## Purpose

Phase 3 should prove that the orchestration architecture works before we add
real ADaM derivation logic.

The goal is not to generate ADSL yet. The goal is to prove that:

- a study-level graph can coordinate multiple target ADaM datasets
- each ADaM dataset can run through its own isolated dataset-level graph call
- dataset results can be reduced back into a study-level summary
- checkpointing can record the run history
- one dataset failure does not corrupt another dataset state

## Plain-Language Idea

`StudyGraph` is the project manager.

`DatasetGraph` is the worker for one ADaM dataset.

For example:

```text
StudyGraph
  -> plans dependency order
  -> runs ADSL first
  -> if ADSL succeeds, sends ADAE / ADLB / ADCM to dataset tasks
  -> if ADSL fails, marks dependent datasets as blocked
  -> collects dataset summaries
  -> writes a study-level audit manifest
```

Each dataset gets its own `DatasetState`.

That means ADSL has its own repair count, failures, routing decisions, and
approval records. ADAE has a separate copy. If ADAE fails, ADSL should remain
clean.

## Non-Goals

Phase 3 should not implement:

- real LLM calls
- real R code generation
- real R sandbox execution
- real SDTM parsing
- real define.xml parsing
- production UI
- production-grade persistence backend

Those belong to later phases.

## Required Dependency

Phase 3 cannot be implemented with the current dependency set alone.

`pyproject.toml` currently declares Pydantic, but Phase 3 needs LangGraph.
The implementation step should add a bounded LangGraph dependency before graph
code is written, for example:

```toml
dependencies = [
  "pydantic>=2,<3",
  "langgraph>=0.6,<1"
]
```

The exact lower bound can be adjusted after local import/API verification, but
Phase 3 tests must run against one known LangGraph API family rather than an
unspecified version.

Implementation note:

`python -m pip install -e .` resolved LangGraph `0.6.11` in the local
development environment.

## Proposed Minimal Flow

### StudyGraph

```text
START
  -> initialize_study
  -> plan_datasets
  -> run_foundation_datasets
  -> route_after_foundation
       foundation_ready   -> dispatch_downstream_dataset_tasks
       foundation_failed  -> mark_downstream_blocked
  -> reduce_dataset_results
  -> write_audit_manifest_stub
  -> END
```

Phase 3 should not pretend all ADaM datasets are peers. The MVP dependency DAG
is:

```text
ADSL
  -> ADAE
  -> ADLB
  -> ADCM
```

Only `ADSL` and `ADAE` need to run in the first smoke test. Additional
downstream datasets such as `ADCM` can be used to prove the reducer is not
hard-coded to two datasets.

Dependency rule:

If the user requests a downstream dataset only, for example `["ADAE"]`, the
Phase 3 skeleton automatically inserts `ADSL` as the foundation task. A future
phase may support an explicit "externally verified ADSL already exists" mode,
but Phase 3 does not.

### DatasetGraph

```text
START
  -> prepare_dataset
  -> draft_lineage_stub
  -> draft_spec_stub
  -> route_risk_stub
       low_risk  -> generate_code_stub
       high_risk -> human_review_stub -> generate_code_stub
  -> run_sandbox_stub
  -> classify_result_stub
       success    -> summarize_dataset
       code_error -> repair_code_stub -> run_sandbox_stub
       spec_error -> revise_spec_stub -> generate_code_stub
  -> summarize_dataset
  -> END
```

The first implementation can simulate:

- `ADSL` succeeds without repair
- `ADAE` triggers one code repair and then succeeds
- a second scenario where `ADSL` fails and downstream datasets are marked
  `blocked_by_adsl`

This is enough to prove state isolation.

## Design Claims To Audit

This phase should be reviewed claim by claim.

| Claim | Evidence | Local test | Business meaning | Failure condition |
|---|---|---|---|---|
| LangGraph workflows are made from state, nodes, and edges. | Official Graph API says these are the three key components. | Build a tiny graph that mutates state through nodes and edges. | We can model clinical generation as explicit workflow steps. | Nodes mutate hidden global state or graph behavior is not visible from state. |
| `StudyGraph` and `DatasetGraph` should be separated. | Official subgraph docs support graph composition, but Phase 3 uses explicit graph invocation inside parent nodes for simplicity. | Parent graph invokes dataset graph for ADSL and ADAE. | Study orchestration stays separate from dataset derivation logic. | Dataset internals leak into study-level orchestration. |
| Dataset dispatch should use `Send` or an equivalent map-reduce pattern for downstream batches. | Official Graph API says `Send` supports cases where the number of downstream tasks is not known ahead of time and each task needs its own state. | After ADSL succeeds, downstream targets produce separate dataset runs. | A future study can add more ADaM datasets without rewriting static edges. | Adding ADCM requires hard-coding a new edge in the graph skeleton. |
| ADSL should be treated as the first foundation dataset in the MVP. | ADaM downstream datasets commonly depend on subject-level attributes from ADSL. | If ADSL fails, downstream datasets are marked `blocked_by_adsl` and are not run. | Dependency order is clinically plausible. | ADAE runs after failed ADSL as if nothing happened. |
| Dataset state must be isolated. | Phase 2 state model requires retry counts inside `DatasetState`, not `StudyState`. | ADAE repair count becomes 1 while ADSL remains 0. | A failed ADAE does not corrupt ADSL. | One global repair count is shared across datasets. |
| Checkpointing needs a `thread_id`. | Official persistence docs say checkpointers use `thread_id` as the primary key. | Run graph with `thread_id = "{study_id}:{run_id}"` and assert state history exists. | A run can be inspected, resumed, or debugged later. | `get_state_history()` returns nothing or cannot distinguish runs. |
| We should not hard-code dataset name into `checkpoint_ns`. | Official persistence docs describe root namespace as `""` and subgraph namespace as `node_name:uuid`. | Test should inspect checkpoint history without relying on a fixed dataset namespace. | Dataset identity belongs in state and audit, not in LangGraph internals. | Tests assume `checkpoint_ns == "ADSL"`. |
| Dynamic routing and static edges should not be mixed from the same node. | Official Graph API warns both paths can execute and make behavior harder to reason about. | Routing node has either conditional edges or `Command`, not both. | Failure repair paths remain predictable. | A node routes to both repair and success accidentally. |
| Parallel dataset results need an explicit reducer. | LangGraph parallel branches merge state updates through reducers. | `dataset_results` collects ADSL and ADAE instead of overwriting one with the other. | StudyGraph can summarize all dataset tasks. | Only the last dataset result remains after reduce. |

## Proposed Files

```text
src/adam_agent/graph/
  __init__.py
  state.py
  dataset_graph.py
  study_graph.py
  routing.py

tests/
  test_graph_smoke.py

docs/
  phase3_design.md
  phase3_review_temp.html
```

## Runtime State Strategy

Phase 2 Pydantic schemas remain the contract and validation layer.

Phase 3 LangGraph runtime state should be a thin `TypedDict` layer because
LangGraph reducers are easiest to express there.

Use explicit names to avoid confusing the two layers:

- `StudyState`: durable Pydantic contract from Phase 2
- `DatasetState`: durable Pydantic contract from Phase 2
- `StudyGraphState`: LangGraph runtime `TypedDict`
- `DatasetGraphState`: LangGraph runtime `TypedDict`

Suggested split:

```text
Pydantic schemas
  -> validate durable objects
  -> serialize fixtures and audit records

TypedDict graph state
  -> carry in-flight graph updates
  -> use reducers for map-reduce result collection
```

This avoids forcing every graph update through expensive recursive validation
while keeping persisted objects structured.

Do not copy every Pydantic field into the graph `TypedDict` layer. The runtime
state should carry only the fields needed to route, dispatch, reduce, and test
the skeleton.

Minimal `StudyGraphState` fields:

```text
study_id
run_id
target_datasets
dependency_graph
foundation_datasets
downstream_datasets
dataset_results  # reducer-managed
blocked_datasets
audit_artifacts
status
```

Minimal `DatasetGraphState` fields:

```text
study_id
run_id
dataset
stub_scenario
status
repair_attempts
max_repair_attempts
route
failure_type
summary
audit_artifacts
```

Runtime state should keep map-reduce payloads small. A `Send` branch should
receive only the dataset task payload it needs:

```text
dataset
study_id
run_id
dependency status
stub scenario
small input/reference artifact refs
llm exposure policy snapshot
```

It should not receive the full `StudyState`. The full study state stays with the
parent graph.

`dataset_results` must have an explicit reducer. For Phase 3, a list-append
reducer is acceptable because it makes it easy to inspect every returned
`DatasetResultSummary`. A later phase can normalize this into a dictionary by
dataset name if needed.

For audit manifest stubs, Phase 3 can keep the artifact in state as an
`ArtifactRef(kind="report", role="audit_manifest", ...)` without requiring
production-grade file persistence. Writing a small JSON file is acceptable if it
stays deterministic and test-local, but the behavior under test should be the
presence of the audit artifact and dataset summaries.

## Checkpoint Strategy

MVP:

- use in-memory checkpointer for smoke tests
- config uses `thread_id = "{study_id}:{run_id}"`
- do not assume fixed `checkpoint_ns`
- parent graph owns the checkpointer
- dataset graph is invoked inside parent graph nodes in Phase 3
- assert there is a non-empty state history and that the history is consistent
  with the final state

Important boundary:

`checkpoint_ns` is a LangGraph execution namespace, not a business dataset id.
Dataset identity must live in state, artifact refs, and audit records.

Later:

- move from in-memory to SQLite or Postgres checkpointer
- expose state history in the product UI
- support human interrupt/resume

Checkpoint tests should be behavior tests, not tests of internal storage
details. They should check that:

- a configured `thread_id` is accepted
- state history is non-empty
- final state contains the expected dataset summaries
- the history is consistent with the final state

They should not depend on fixed internal `checkpoint_ns` values.

Important limitation:

Phase 3 verifies study-level checkpoint history. It does not yet verify nested
LangGraph subgraph checkpoint inheritance or dataset-internal node history
inside the parent checkpoint. That can be revisited when human interrupt/resume
and production checkpointing become real requirements.

## Test Plan

Minimum tests:

1. `test_study_graph_runs_stub_datasets`
   - run ADSL and ADAE through the study graph
   - assert both appear in `dataset_results`

2. `test_dataset_state_isolation`
   - simulate ADSL success
   - simulate ADAE one repair
   - assert ADSL repair attempts stay 0 and ADAE repair attempts become 1

3. `test_adsl_failure_blocks_downstream`
   - simulate ADSL failure
   - assert ADAE is marked `blocked_by_adsl`
   - assert ADAE dataset graph is not run

4. `test_downstream_only_request_still_runs_adsl_foundation_first`
   - request `["ADAE"]`
   - assert ADSL is automatically inserted as the foundation dataset
   - assert both ADSL and ADAE summaries exist

5. `test_checkpoint_history_exists`
   - run graph with a checkpointer and `thread_id`
   - assert `get_state_history(config)` returns snapshots
   - assert final state and history agree on key fields

6. `test_audit_manifest_stub_written`
   - assert a stub audit manifest exists or is represented as an `ArtifactRef`
   - assert study-level audit includes dataset-level summaries

7. `test_routing_does_not_mix_static_and_dynamic_paths`
   - assert failure classification chooses one route
   - no node should accidentally continue down success and repair paths

8. `test_routing_respects_max_repair_attempts`
   - assert repair/spec routes fail when max attempts are exhausted

9. `test_dataset_result_reducer_keeps_multiple_results`
   - assert both ADSL and ADAE results remain after downstream reduce

## Sub-Agent Review Plan

Ask three reviewers to critique this design before implementation:

1. LangGraph architecture reviewer
   - check whether subgraph, `Send`, reducers, checkpoint assumptions match the
     official LangGraph model

2. Clinical workflow reviewer
   - check whether study-level versus dataset-level split fits ADaM reality

3. Engineering reviewer
   - check whether the proposed files, tests, and state split are implementable
     without over-engineering Phase 3

The parent agent should then judge whether each finding is:

- valid and must fix before coding
- valid but can be deferred
- misunderstanding or not applicable

## Sources To Verify

- LangGraph Graph API:
  https://docs.langchain.com/oss/python/langgraph/graph-api
- LangGraph Subgraphs:
  https://docs.langchain.com/oss/python/langgraph/use-subgraphs
- LangGraph Persistence:
  https://docs.langchain.com/oss/python/langgraph/persistence

## Proposed Phase 3 Exit Criteria

Phase 3 can be considered complete when:

- a local stub study graph runs
- ADSL runs first as the MVP foundation dataset
- downstream dataset tasks are dispatched only after ADSL succeeds
- downstream-only requests automatically include ADSL as the foundation dataset
- if ADSL fails, downstream datasets are marked blocked/skipped
- dataset result summaries are reduced back to study state
- ADSL and ADAE repair attempts are isolated
- checkpoint history can be inspected
- a study-level audit manifest stub is produced from dataset-level summaries
- smoke tests pass
- a review HTML explains the graph behavior to a non-specialist

## Implementation Result

Implemented files:

```text
src/adam_agent/graph/state.py
src/adam_agent/graph/routing.py
src/adam_agent/graph/dataset_graph.py
src/adam_agent/graph/study_graph.py
tests/test_graph_smoke.py
```

Verification:

```text
python -m unittest discover -s tests -p "test_*.py"
Ran 20 tests ... OK

python -m unittest tests.test_graph_smoke -v
Ran 8 tests ... OK
```
