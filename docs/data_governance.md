# Data Governance Contract

This document defines what information may be sent to LLM providers.

Phase:

`Phase 1 - Product and data contracts`

Last updated:

2026-05-21

## Core Policy

Clinical data should remain local by default.

For current demo/MVP development, the user confirmed that the demo data under
discussion has been processed and may be sent more freely to external APIs. This
is a development allowance for explicitly approved demo runs, not the default
policy for unknown or real clinical data.

## LLM Exposure Modes

The product should support explicit exposure modes.

| Mode | What LLM Can See | Intended Use |
|---|---|---|
| `metadata_only` | column names, labels, types, missingness, unique-value summaries, date ranges | default for unknown or real clinical data |
| `demo_rich_context` | metadata plus sample rows, richer value distributions, selected reference rows | explicitly approved processed demo data |
| `full_data_allowed` | larger data slices or full small demo tables | only when explicitly enabled |

## Default Selection Rule

The implementation must not use one global exposure default for every study.

Use this rule:

```text
if study/run config explicitly declares processed demo data:
    exposure_mode = demo_rich_context
else:
    exposure_mode = metadata_only
```

For real clinical data, unknown data, or unclassified uploaded data:

```text
default_llm_exposure_mode = metadata_only
```

For the current approved demo build only:

```text
exposure_mode = demo_rich_context
```

Reason:

- The current demo data is processed and explicitly approved by the user for MVP
  development.
- More context can help build and debug the first ADSL loop.
- The system is still local-first and must record what was sent.

## `metadata_only`

Allowed:

- dataset names
- variable names
- labels when available
- data types
- missing counts or percentages
- distinct counts
- date ranges
- controlled terminology summaries
- small aggregated summaries

Not allowed by default:

- full subject-level rows
- direct identifiers
- free-text adverse event narratives
- raw medical history text
- unapproved patient-level records

## `demo_rich_context`

Allowed:

- everything in `metadata_only`
- first N rows from processed demo data
- selected sample rows per domain
- unique values for important categorical variables
- small subject-level summaries
- reference ADSL sample rows when available

Suggested initial limits:

```text
sample_rows_per_dataset = 20
max_unique_values_per_variable = 30
max_subject_summaries = 20
```

These are initial development limits, not production defaults.

Required precondition:

```text
demo_rich_context requires explicit study/run configuration:
data_classification = processed_demo
external_api_allowed = true
```

## `full_data_allowed`

Allowed only when explicitly configured.

Use cases:

- tiny synthetic datasets
- fully public toy datasets
- local-only model calls
- user-approved debugging sessions

For `user-approved debugging sessions`, the approving user is responsible for
the data exposure decision for that run. The approval must be explicit in run
configuration and recorded in the audit manifest.

Every `full_data_allowed` run must be recorded in the audit manifest.

## Required Audit Fields

Every LLM call should record:

- provider
- model
- exposure mode
- datasets included
- variables included
- sample row counts
- whether full data was included
- prompt artifact path
- prompt hash
- response hash
- redaction policy and version
- provider locality, such as external_api or local_model
- timestamp

## Safety Rule

The graph should not decide by itself to escalate from `metadata_only` to
`demo_rich_context` or `full_data_allowed`.

Exposure mode is a run configuration decision.
