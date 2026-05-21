# Output Contract

This document defines the expected output of an MVP run.

Phase:

`Phase 1 - Product and data contracts`

Last updated:

2026-05-21

## Run Folder Shape

Each run should create a unique folder:

```text
studies/{study_id}/runs/{run_id}/
  lineage/
  specs/
  code/
  outputs/
  validation/
  compare/
  audit/
```

The exact `run_id` format can be decided later. It should be stable, sortable,
and unique.

Suggested format:

```text
YYYYMMDD_HHMMSS_{short_uuid}
```

## ADSL MVP Outputs

For the first real ADSL loop:

```text
runs/{run_id}/
  lineage/
    adsl_lineage.json

  specs/
    adsl_draft_spec.json
    adsl_approved_spec.json

  code/
    build_adsl.R

  outputs/
    adsl.csv

  validation/
    adsl_validation_report.json

  compare/
    adsl_compare_report.json

  audit/
    manifest.json
    decisions.json
    llm_calls.jsonl
    tool_calls.jsonl
```

`adsl_compare_report.json` should always exist. If no reference ADSL is provided,
write it with `status: "skipped"` and a clear reason. This keeps downstream UI
and audit logic simple while avoiding a false comparison claim.

## Required Output Categories

| Category | Purpose |
|---|---|
| `lineage/` | Shows how source domains and variables map to target variables |
| `specs/` | Stores draft and approved machine-readable ADaM contracts |
| `code/` | Stores generated R derivation code |
| `outputs/` | Stores generated ADaM dataset outputs |
| `validation/` | Stores structural and rule-based validation results |
| `compare/` | Stores reference comparison results when reference ADaM exists |
| `audit/` | Stores full run trace, decisions, hashes, and LLM/tool logs |

## Canonical Path Rule

The canonical output location is always:

```text
studies/{study_id}/runs/{run_id}/
```

Top-level study folders may later contain indexes or user-facing exports, but
the MVP should not treat top-level `outputs/` or `audit/` as canonical.

## Approved Spec Output

`adsl_approved_spec.json` must not be a silent copy of `adsl_draft_spec.json`.

It must include one of these approval records:

- human approval
- rule-based silent approval for high-confidence direct mappings
- explicit `demo_only_no_review` bypass

Minimum approval metadata:

```json
{
  "approval_mode": "human | silent_pass | demo_only_no_review",
  "approved_by": "user id, system rule, or demo config",
  "approved_at": "ISO-8601 timestamp",
  "reviewed_variables": ["USUBJID", "TRTSDT"],
  "unresolved_assumptions": [],
  "source_draft_spec_hash": "sha256:..."
}
```

If approval cannot be established, code generation must stop or remain in an
explicit demo-only mode.

## Artifact Manifest

Every run must write:

```text
audit/manifest.json
```

The manifest should include:

- study id
- run id
- target dataset
- input file paths
- input file hashes
- draft spec path and hash
- approved spec path and hash
- lineage path and hash
- output file paths
- output file hashes
- generated code path
- generated code hash
- validation report path
- validation rule version
- compare report path when available
- LLM mode used
- data exposure mode
- R version and package snapshot when available
- sandbox configuration or image id when available
- graph node trace path
- evidence ids used for draft spec
- decision ids linked to variables
- timestamp
- graph version or commit hash when available

## Validation Output

The validation report should not be only prose.

It should be structured JSON with fields such as:

```json
{
  "dataset": "ADSL",
  "status": "pass",
  "checks": [
    {
      "name": "required_columns_present",
      "status": "pass",
      "details": {}
    }
  ],
  "warnings": [],
  "errors": []
}
```

## Comparison Output

`adsl_compare_report.json` should be produced for every ADSL run.

If a reference ADSL exists, comparison should report:

- row count differences
- column differences
- key differences by `USUBJID`
- variable-level mismatch counts
- type differences
- selected high-impact discrepancies

Full large compare tables can be stored as artifacts. Summaries should be kept
in state.

If no reference ADSL exists, the report should look like:

```json
{
  "dataset": "ADSL",
  "status": "skipped",
  "reason": "No reference ADSL was provided"
}
```

## Audit Principle

The output is not only the ADaM dataset.

The real product output is:

```text
ADaM dataset + code + evidence + validation + audit trail
```
