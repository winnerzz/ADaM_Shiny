# Spec Draft Contract

This document defines how the system may create a draft ADaM spec when a formal
spec is missing.

Phase:

`Phase 1 - Product and data contracts`

Last updated:

2026-05-21

## Core Rule

If the ADaM spec is missing, the system may generate a draft spec.

But the draft spec must be evidence-based and reviewable.

The system must not do this:

```text
ask LLM to invent an ADSL spec from memory
```

It should do this:

```text
collect evidence
  -> extract candidate target variables
  -> infer source variables and derivations
  -> attach evidence records
  -> assign confidence
  -> mark review-required items
  -> write draft spec
```

## Evidence Priority

Use evidence in this order when available:

| Priority | Evidence | What It Helps Infer |
|---|---|---|
| 1 | Existing ADaM spec | Target variables, labels, types, derivations |
| 2 | Legacy SAS AD program | Derivation logic, joins, filters, ordering, edge cases |
| 3 | SAP / protocol / TFL shells | Analysis populations, endpoints, windows, treatment periods |
| 4 | define.xml | Metadata, labels, codelists, dataset definitions |
| 5 | Reference ADaM dataset | Target variables, types, labels if available, value patterns, comparison target |
| 6 | SDTM profiles | Available source variables, date ranges, missingness, keys |
| 7 | CDISC ADaM IG / SDTM IG | Standard variable expectations and conventions |
| 8 | Company standards | Local implementation conventions |
| 9 | Built-in templates | Common ADSL patterns when stronger evidence is missing |

Lower-priority evidence should not override higher-priority evidence without a
recorded reason.

Reference ADaM deserves special caution. It is strong evidence for what the
output looks like, but it is weaker evidence for why a variable was derived that
way. Do not infer population flags, treatment windows, baseline rules, or
analysis-period logic from reference values alone without review.

Legacy SAS AD programs are also evidence, not executable runtime dependencies in
the MVP. The system may read `.sas` files as text to infer derivation logic, but
R sandbox execution should use generated R code.

## Minimum Draft Spec Fields

Each variable in the draft spec should include:

```json
{
  "dataset": "ADSL",
  "variable": "TRTSDT",
  "label": "Date of First Exposure to Treatment",
  "type": "date",
  "source_domains": ["EX"],
  "source_variables": ["EXSTDTC"],
  "derivation": "Minimum non-missing EX.EXSTDTC per USUBJID",
  "evidence": [
    {
      "type": "sdtm_profile",
      "source": "EX.EXSTDTC exists and is date-like"
    }
  ],
  "confidence": 0.82,
  "review_required": true,
  "approval_status": "draft",
  "assumptions": []
}
```

Minimum required fields:

- `dataset`
- `variable`
- `label`
- `type`
- `source_domains`
- `source_variables`
- `derivation`
- `evidence`
- `confidence`
- `review_required`
- `approval_status`
- `assumptions`

## Review Rules

Mark `review_required = true` when:

- confidence is below threshold
- multiple source variables are plausible
- derivation affects treatment dates
- derivation affects population flags
- derivation affects analysis period or baseline logic
- evidence sources conflict
- variable is not supported by any strong evidence
- LLM made a non-trivial inference
- derivation is inferred from reference ADaM values without supporting spec,
  legacy code, SAP/protocol, or define.xml evidence

Suggested initial threshold:

```text
confidence < 0.90 -> review_required = true
```

This threshold can be tuned later.

## ADSL First Draft Sources

For the first ADSL MVP, likely evidence sources are:

- `DM` for subject id, demographics, planned treatment, treatment arm
- `EX` for actual exposure dates and safety-related derivations
- reference `ADSL` when available
- old SAS `ADSL.sas` when available
- define.xml when available
- built-in ADSL starter template

Do not expand the first ADSL loop beyond the minimal ADSL skeleton unless
stronger study evidence is available.

## Important Distinction

`draft_spec` is not the same as `approved_spec`.

Suggested lifecycle:

```text
draft_spec
  -> silent pass for high-confidence direct mappings
  -> human review for high-risk/low-confidence variables
  -> record approval metadata or demo-only bypass
  -> approved_spec
  -> code generation
```

Generated code should use `approved_spec`, not raw `draft_spec`, unless the run
is explicitly configured as a demo-only no-review mode.
