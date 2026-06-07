# State Ownership

This note defines which state object is allowed to decide product behavior.
The goal is to stop the UI, API compatibility files, and graph runtime from
each inventing their own answer to "what should happen next?"

## Plain Rule

`graph_state.json` is the only durable source of truth for a run.

Everything else is either:

- temporary graph runtime state,
- a read-only projection for the UI,
- a legacy compatibility file, or
- browser-only display state.

## State Layers

| Layer | File | Purpose | May Decide Workflow? |
| --- | --- | --- | --- |
| LangGraph runtime state | `src/adam_agent/graph/state.py` | In-flight values while graph nodes run. | No. It must be persisted into canonical state before the product relies on it. |
| Canonical durable state | `src/adam_agent/schemas/graph_state.py` and `runs/{run_id}/graph_state.json` | The product record for study status, dataset status, interrupts, human commands, artifacts, and failures. | Yes. This is the source of truth. |
| Gateway write boundary | `src/adam_agent/graph/gateway.py` | The only normal place that mutates canonical product state and writes projections. | Yes, by updating canonical state first. |
| Workflow projection | `runs/{run_id}/workflow_state.json` via `src/adam_agent/graph/workflow_state.py` | Compatibility read model for older UI/API expectations. | No. It must not override canonical graph state. |
| API progress read model | `GraphGateway.progress_summary()` | UI-facing summary: next action, review queue, blocked datasets, available actions. | No mutation. It may guide UI because it is derived from canonical state. |
| Browser state | `src/adam_agent/api/web.py` local `state` object | Selected study, selected dataset, open panels, latest fetched progress. | No. It must not maintain independent dataset lifecycle truth. |

## Write Rules

1. Product state changes go through `GraphGateway`.
2. `GraphGateway` writes `graph_state.json` first.
3. `workflow_state.json` may be written only as a projection from canonical
   state or as a clearly marked legacy compatibility path.
4. API service helpers should delegate product state changes to `GraphGateway`.
5. The frontend must refresh `progress_summary()` after a write instead of
   guessing the next lifecycle state locally.

## Read Rules

1. The UI should prefer `progress_summary()` for:
   - the current next action,
   - the review queue,
   - whether a dataset is blocked,
   - which action buttons are valid.
2. `graph_state.json` can be shown in advanced audit views.
3. `workflow_state.json` can be shown as an audit artifact, but should not drive
   buttons or workflow routing.
4. If `graph_state.json` exists but cannot be read, product endpoints must fail
   closed instead of falling back to artifacts or `workflow_state.json`.

## Required Invariants

These are product rules, not UI preferences.

1. A study-level interrupt blocks dataset-level commands until it is resolved.
2. Dataset actions come from canonical dataset state, not browser caches.
3. A completed dataset cannot be made actionable again by an old open failure
   projection.
4. A terminal failure must expose an explicit human choice:
   - repair generated code,
   - revise approved spec,
   - request new input,
   - skip or stop the dataset.
5. Uploaded evidence changes must mark existing plans or dataset work stale
   before the run continues.
6. `workflow_state.json` must stay consistent with `graph_state.json` when it is
   generated from canonical state.

## Frontend Target Shape

The browser should eventually keep only UI state:

- selected study/run,
- selected ADaM targets,
- active dataset detail,
- expanded or collapsed panels,
- latest fetched `runProgress`.

The browser should not keep independent lifecycle maps such as generated code,
review, execution, draft spec, or terminal failure state as product truth. Those
can exist temporarily while rendering a response, but must be refreshed from
`progress_summary()` after every write.

## Why This Matters

Clinical data generation can fail silently if two layers disagree. For example,
the UI may believe code is ready to repair while canonical state still has an
open dependency review. In that case the correct behavior is to stop at the
canonical review gate, not to let a cached browser state continue the dataset.
