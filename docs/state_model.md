# State Model

This document defines the Phase 2 state design for ADaM Agent Studio.

Phase:

`Phase 2 - State model`

Last updated:

2026-05-22

## Purpose

State is the memory of a run.

It must answer:

- What study is running?
- Which ADaM datasets are being generated?
- What evidence supports each draft spec decision?
- What was approved, by whom, and under what mode?
- What files were read or written?
- What data was exposed to LLM providers?
- Where did the graph pause, fail, repair, or route backward?

The state model should be stable before the LangGraph skeleton is implemented.

## Design Principles

1. Keep study-level and dataset-level state separate.
2. Keep per-dataset retry counts inside `DatasetState`, never in `StudyState`.
3. Put small structured objects in state.
4. Put large files behind `ArtifactRef` path/hash references.
5. Every draft spec variable should be traceable to evidence.
6. Every approved spec should be traceable to approval metadata.
7. LLM exposure mode must be explicit and auditable.
8. Reference ADaM may support output shape and validation, but should not be the
   only source of derivation logic.
9. `.sas7bdat` is data; `.sas` is text evidence in the MVP.

## State Objects

Phase 2 defines these core objects:

| Object | Role |
|---|---|
| `StudyState` | Study-level run orchestration state |
| `DatasetState` | State for one target ADaM dataset |
| `ArtifactRef` | Path/hash reference to files and large outputs |
| `EvidenceRecord` | Evidence supporting lineage/spec decisions |
| `ApprovalRecord` | Human, silent-pass, or demo-bypass approval metadata |
| `SpecVariable` | Variable-level draft/approved spec record |
| `SpecDocument` | Dataset-level spec document containing spec variables |
| `LLMExposureConfig` | Run-level policy for what can be sent to LLMs |
| `LLMCallRecord` | Per-call record of what was sent and received |
| `DatasetResultSummary` | Study-level summary of a dataset subgraph result |
| `RouteDecision` | Routing result from risk/failure diagnosis |
| `FailureRecord` | Structured failure and diagnosis information |

## What Belongs in State

Put these in state:

- ids and statuses
- dependency graph
- small lineage JSON
- draft spec JSON
- approved spec JSON
- evidence records
- approval records
- LLM exposure configuration
- LLM call metadata
- generated code string for current dataset
- summarized validation and compare reports
- routing decisions
- failure summaries

Do not put these directly in state:

- full SDTM tables
- full ADaM output tables
- reference ADaM tables
- full compare detail tables
- CDISC PDF contents
- large logs

Use `ArtifactRef` for large files.

## StudyState

`StudyState` answers:

> Where is the whole study run?

It should contain study-level orchestration data, not the full details of every
dataset.

Suggested shape:

```json
{
  "study_id": "PSY201",
  "run_id": "20260522_001",
  "status": "running",
  "current_phase": "study_orchestration",
  "target_datasets": ["ADSL"],
  "dependency_graph": {
    "ADSL": []
  },
  "dataset_status": {
    "ADSL": "pending"
  },
  "dataset_results": {},
  "study_artifacts": [],
  "llm_exposure": {},
  "global_decisions": [],
  "errors": [],
  "created_at": "2026-05-22T00:00:00+08:00",
  "updated_at": "2026-05-22T00:00:00+08:00"
}
```

Required fields:

- `study_id`
- `run_id`
- `status`
- `target_datasets`
- `dependency_graph`
- `dataset_status`
- `dataset_results`
- `study_artifacts`
- `llm_exposure`

Allowed statuses:

```text
pending
running
needs_review
completed
failed
cancelled
```

Study-level rule:

`StudyState` may summarize a dataset result, but it should not embed full
`DatasetState` for every dataset. Dataset-specific details belong to
`DatasetState` and checkpointed subgraph state.

`dataset_results` should contain `DatasetResultSummary`, not arbitrary dicts.

## DatasetState

`DatasetState` answers:

> Where is this one ADaM dataset in its own workflow?

Suggested shape:

```json
{
  "study_id": "PSY201",
  "run_id": "20260522_001",
  "dataset": "ADSL",
  "dataset_role": "subject_level",
  "status": "running",
  "input_domains": ["DM", "EX"],
  "input_artifacts": [],
  "reference_artifacts": [],
  "lineage": {},
  "draft_spec": {},
  "approved_spec": null,
  "evidence_records": [],
  "approval_records": [],
  "generated_code": "",
  "code_artifact": null,
  "output_artifacts": [],
  "validation_summary": {},
  "compare_summary": {},
  "llm_exposure": {},
  "llm_calls": [],
  "route_decisions": [],
  "failures": [],
  "repair_attempts": 0,
  "max_repair_attempts": 3,
  "human_decisions": [],
  "created_at": "2026-05-22T00:00:00+08:00",
  "updated_at": "2026-05-22T00:00:00+08:00"
}
```

Required fields:

- `study_id`
- `run_id`
- `dataset`
- `dataset_role`
- `status`
- `input_domains`
- `input_artifacts`
- `lineage`
- `draft_spec`
- `approved_spec`
- `evidence_records`
- `approval_records`
- `repair_attempts`
- `max_repair_attempts`

Dataset-level rule:

`repair_attempts` belongs here. It must not be global. ADSL, ADAE, and ADCM each
need their own retry count, failure records, and approval records.

`llm_exposure` in `DatasetState` is a dataset-level snapshot or narrowing of the
study-level policy. A dataset subgraph must not silently escalate exposure mode
beyond the study-level `LLMExposureConfig`.

Checkpoint configuration note for Phase 3:

```text
thread_id = "{study_id}:{run_id}"
```

Do not use the business dataset name as a manually assigned `checkpoint_ns`.
In LangGraph, `checkpoint_ns` is an execution namespace managed by the graph and
subgraph runtime. Dataset identity should live in `DatasetState.dataset`,
artifact references, and audit records. This keeps the business model separate
from LangGraph internals.

## SpecVariable and SpecDocument

`SpecVariable` is the minimum variable-level contract for draft and approved
specs.

Suggested shape:

```json
{
  "variable": "TRTSDT",
  "label": "Date of First Exposure to Treatment",
  "type": "date",
  "source_domains": ["EX"],
  "source_variables": ["EXSTDTC"],
  "derivation": "Minimum non-missing EX.EXSTDTC per USUBJID",
  "evidence_ids": ["ev_adsl_trtsdt_001"],
  "approval_ids": [],
  "confidence": 0.82,
  "review_required": true,
  "review_reasons": ["Treatment date derivation should be reviewed"],
  "risk_level": "high",
  "approval_status": "draft",
  "assumptions": []
}
```

Rules:

- Every spec variable needs at least one `evidence_id`.
- Approved variables need at least one `approval_id`.
- High-risk variables require review or approval evidence.
- `draft_spec` and `approved_spec` should use `SpecDocument`, not free-form dicts.

`SpecDocument` groups variables for one dataset and enforces that approved
documents cannot contain unapproved variables.

## ArtifactRef

`ArtifactRef` represents a file without embedding its full contents in state.

Suggested shape:

```json
{
  "artifact_id": "art_dm_csv_001",
  "kind": "input_sdtm",
  "path": "studies/PSY201/input_sdtm/dm.csv",
  "sha256": "sha256:...",
  "dataset": "DM",
  "format": "csv",
  "role": "source",
  "created_at": "2026-05-22T00:00:00+08:00",
  "metadata": {
    "rows": 100,
    "columns": 20,
    "column_names": ["STUDYID", "USUBJID", "AGE", "SEX"]
  }
}
```

Common `kind` values:

```text
input_sdtm
input_define
input_spec
reference_adam
legacy_code
draft_spec
approved_spec
lineage
generated_code
output_adam
validation_report
compare_report
audit_manifest
llm_prompt
llm_response
tool_log
```

SAS boundary:

- `.sas7bdat` files are `input_sdtm` or `reference_adam` data artifacts.
- `.sas` files are `legacy_code` artifacts and text evidence in the MVP.

## EvidenceRecord

`EvidenceRecord` explains why the system believes a lineage/spec decision is
reasonable.

Suggested shape:

```json
{
  "evidence_id": "ev_adsl_trtsdt_001",
  "dataset": "ADSL",
  "variable": "TRTSDT",
  "source_type": "sdtm_profile",
  "source_ref": "EX.EXSTDTC",
  "artifact_id": "art_ex_csv_001",
  "summary": "EX.EXSTDTC exists and is date-like",
  "supports": ["source_variable", "derivation_candidate"],
  "confidence_delta": 0.2,
  "created_at": "2026-05-22T00:00:00+08:00"
}
```

Allowed `source_type` values:

```text
existing_spec
legacy_sas
sap_protocol_tfl
define_xml
reference_adam
sdtm_profile
cdisc_standard
company_standard
built_in_template
human_note
```

Allowed `supports` values:

```text
target_variable
label
type
source_domain
source_variable
derivation_logic
derivation_candidate
output_shape
validation
review_requirement
```

Rule for `reference_adam` evidence:

Reference ADaM can support:

- `target_variable`
- `label`
- `type`
- `output_shape`
- `validation`

It should not be the only evidence for:

- `derivation_logic`
- population flags
- treatment windows
- baseline rules
- analysis periods

If reference ADaM is the only clue for a derivation, the variable should be
marked `review_required = true`.

## ApprovalRecord

`ApprovalRecord` prevents a draft spec from silently becoming an approved spec.

Suggested shape:

```json
{
  "approval_id": "appr_adsl_001",
  "dataset": "ADSL",
  "reviewed_variables": ["USUBJID", "TRTSDT"],
  "approval_mode": "human",
  "approved_by": "user",
  "approved_at": "2026-05-22T00:00:00+08:00",
  "source_draft_spec_hash": "sha256:...",
  "unresolved_assumptions": [],
  "rule_id": null,
  "notes": ""
}
```

Allowed `approval_mode` values:

```text
human
silent_pass
demo_only_no_review
```

Rules:

- `human` means a user approved the variables or spec.
- `silent_pass` means system rules approved high-confidence low-risk variables.
- `demo_only_no_review` means the run is explicitly configured to bypass review
  for demo development.
- `reviewed_variables` must not be empty.
- `source_draft_spec_hash` is required.
- `silent_pass` requires a `rule_id`.
- Code generation must check for approved spec and approval metadata.

## LLMExposureConfig

`LLMExposureConfig` records what a run is allowed to send to an LLM.

Suggested shape:

```json
{
  "mode": "demo_rich_context",
  "data_classification": "processed_demo",
  "external_api_allowed": true,
  "approved_by": "user",
  "approval_note": "User accepts responsibility for this debugging run",
  "sample_rows_per_dataset": 20,
  "max_unique_values_per_variable": 30,
  "max_subject_summaries": 20,
  "include_reference_rows": true,
  "created_at": "2026-05-22T00:00:00+08:00"
}
```

Allowed `mode` values:

```text
metadata_only
demo_rich_context
full_data_allowed
```

Allowed `data_classification` values:

```text
unknown
real_clinical
processed_demo
synthetic
public
```

Rules:

- Unknown or real clinical data defaults to `metadata_only`.
- `demo_rich_context` requires `data_classification = processed_demo` and
  `external_api_allowed = true`.
- `full_data_allowed` is allowed for user-approved debugging sessions and must
  be recorded in audit.
- The graph should not silently escalate exposure mode.
- `metadata_only` calls must not include sample rows or full data.
- `full_data_included = true` requires `full_data_allowed`.
- Rich/full-data calls require prompt and response artifact ids plus a
  redaction policy.

## LLMCallRecord

`LLMCallRecord` records what actually happened for one model call.

Suggested shape:

```json
{
  "call_id": "llm_adsl_draft_spec_001",
  "node": "draft_spec",
  "provider": "openai_compatible",
  "model": "gpt-5.5",
  "exposure_mode": "demo_rich_context",
  "datasets_included": ["DM", "EX"],
  "variables_included": ["USUBJID", "AGE", "SEX", "EXSTDTC"],
  "sample_row_counts": {
    "DM": 20,
    "EX": 20
  },
  "full_data_included": false,
  "prompt_artifact_id": "art_prompt_001",
  "response_artifact_id": "art_response_001",
  "prompt_hash": "sha256:...",
  "response_hash": "sha256:...",
  "redaction_policy": "none_for_processed_demo",
  "provider_locality": "external_api",
  "created_at": "2026-05-22T00:00:00+08:00"
}
```

This record should not store the full prompt or response body if they are large.
Store large prompt/response payloads as artifacts.

## RouteDecision

`RouteDecision` records why the graph chose the next step.

Suggested shape:

```json
{
  "decision_id": "route_adsl_001",
  "node": "route_spec_risk",
  "dataset": "ADSL",
  "decision": "human_review",
  "reason": "TRTSDT derivation inferred from reference ADaM without legacy code",
  "confidence": 0.82,
  "created_at": "2026-05-22T00:00:00+08:00"
}
```

Common decisions:

```text
continue
silent_pass
human_review
repair_code
revise_spec
revise_lineage
request_reference
fail
```

## FailureRecord

`FailureRecord` captures execution, validation, or comparison failures.

Suggested shape:

```json
{
  "failure_id": "fail_adsl_run_001",
  "dataset": "ADSL",
  "node": "run_sandbox",
  "failure_type": "code_error",
  "message": "Object EXSTDTC not found",
  "artifact_ids": ["art_run_log_001"],
  "root_cause": "source_variable_missing",
  "recommended_route": "revise_spec",
  "repair_attempt": 1,
  "created_at": "2026-05-22T00:00:00+08:00"
}
```

Allowed `failure_type` values:

```text
input_error
spec_error
lineage_error
code_error
sandbox_error
validation_error
compare_error
llm_error
unknown
```

Rules:

- Do not always route failures to `repair_code`.
- If root cause is spec or lineage, route backward to spec or lineage revision.
- Stop or escalate when repair attempts exceed `max_repair_attempts`.

## ADSL MVP State Requirements

For the first ADSL loop, `DatasetState` must be able to represent:

- `USUBJID`
- direct demographic mappings from `DM`
- treatment assignment or treatment label from `DM` when available
- candidate `TRTSDT`
- candidate `TRTEDT`
- candidate demo/MVP exposure-derived `SAFFL`

These are starter variables for the first runnable ADSL loop. They are not
production default ADaM rules. Each variable should carry evidence and review
status.

Treatment dates and population flags should default to `review_required = true`
when they are inferred from SDTM profile patterns, reference ADaM values, or a
built-in starter template without stronger study evidence. A simple
exposure-derived `SAFFL` is a demo/MVP candidate only; production safety
population logic must come from a spec, SAP/protocol, legacy program, define.xml,
or explicit human approval.

Do not force these into automatic approval without stronger evidence:

- `ITTFL`
- `EFFFL`
- baseline flags
- analysis periods
- endpoint-related flags
- complex visit/window logic

## Serialization and Checkpointing

All state objects should be JSON-serializable.

Avoid:

- Python class instances that cannot serialize cleanly
- open file handles
- data frames
- binary payloads
- raw PDF text blobs

Preferred implementation later:

- Python `dataclass` or Pydantic model for validation
- JSON fixtures for test examples
- LangGraph checkpointer for runtime persistence

## Phase 2 Exit Criteria

Phase 2 is complete when:

- `docs/state_model.md` is reviewed and accepted.
- Python schema files exist for the core state objects.
- Minimal JSON fixtures exist for ADSL study and dataset state.
- Tests verify the state is JSON-serializable.
- Tests verify dataset state isolation, especially `repair_attempts`.
