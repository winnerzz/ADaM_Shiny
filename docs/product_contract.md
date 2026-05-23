# Product Contract

This document defines the MVP product boundary for ADaM Agent Studio.

Phase:

`Phase 1 - Product and data contracts`

Last updated:

2026-05-21

## Product Goal

ADaM Agent Studio helps generate auditable ADaM datasets from SDTM inputs and
study evidence.

The product is not a one-shot prompt that directly turns raw data into final
ADaM. It is a controlled workflow:

```text
scan inputs
  -> draft lineage
  -> draft or read spec
  -> review risky assumptions
  -> generate R code
  -> run in R sandbox
  -> validate output
  -> compare against reference ADaM when available
  -> write audit package
```

## Phase 1 Decisions

Accepted decisions:

1. The first real end-to-end dataset target is `ADSL`.
2. The first supported input formats are `csv` and `sas7bdat`.
3. ADaM spec may be missing, but the system must generate a reviewable draft
   spec from explicit evidence.
4. During current demo/MVP development, processed and explicitly approved demo
   data may use `demo_rich_context` for LLM calls. Unknown or real clinical data
   must default to the more conservative `metadata_only` mode.

## MVP Scope

The first real loop should support:

- local study folder
- SDTM `DM` and `EX` inputs for ADSL
- optional ADSL spec
- optional reference ADSL
- optional define.xml metadata
- optional legacy ADSL SAS program as text evidence
- draft ADSL lineage
- draft ADSL spec when spec is missing
- review or explicit demo bypass before code generation
- R code generation through an isolated LLM interface
- local R sandbox execution
- validation report
- comparison report when reference ADSL exists
- audit manifest

## Out of Scope for the First Real Loop

Do not include these in the first real loop:

- generating every ADaM dataset at once
- full P21 validation
- full CDISC RAG infrastructure
- production user management
- production deployment
- complete UI
- automatic claim of regulatory-grade validation

These can be added after the graph skeleton, tool boundaries, and ADSL loop are
stable.

## First Dataset Target

Use `ADSL` first.

Reason:

- ADSL is subject-level and foundational.
- Many downstream ADaM datasets depend on ADSL variables.
- It is simpler than event-level datasets such as ADAE.
- It is a better test of architecture because failures are easier to interpret.

`ADAE` should be treated as a later dependent dataset after the ADSL loop works.

## Minimum Product Promise

For the first real loop, the system should be able to say:

> Given local DM/EX inputs and optional study evidence, the system can create a
> draft ADSL contract, generate auditable R code, run it locally, validate the
> output, and record all assumptions and artifacts.

This is the minimum promise. Anything stronger requires later validation work.

## ADSL MVP Boundary

The first ADSL loop is intentionally narrow.

It should produce a minimal, evidence-backed ADSL skeleton, not a complete
submission-ready ADSL. The starter variables below are an MVP implementation
target for the current demo-style ADSL loop. They are not production defaults
and must not be presented as universal ADaM rules:

- subject identifier, normally `USUBJID`, when present in `DM`
- direct demographic mappings from `DM` when present and clearly labeled
- treatment assignment or treatment label from `DM` when available
- candidate actual treatment start date from `EX`
- candidate actual treatment end date from `EX`
- candidate demo/MVP safety flag derived from exposure presence

Anything involving baseline windows, analysis periods, complex population flags,
efficacy endpoints, or study-specific visit logic is outside the first real loop
unless a strong spec, legacy SAS program, or human decision supplies the rule.

Treatment dates and population flags must be treated as review-required
derivation candidates unless supported by a study spec, SAP/protocol, legacy
program, define.xml, or explicit human approval. For example, `SAFFL = has any
EX record` is acceptable as a demo/MVP candidate, but it is not a production
safety-population rule.

## Non-Negotiable Boundaries

- The system must not silently invent clinical rules.
- A generated draft spec is not an approved spec until reviewed.
- Every derived variable must have traceable evidence or a recorded assumption.
- LLM calls must go through the internal LLM layer, not directly from graph nodes.
- Generated code must run in the R sandbox, not in the main process.
- The R sandbox can read `.sas7bdat` data files through R packages, but it does
  not execute `.sas` programs in the MVP.
- Large data files should be stored as artifacts with paths and hashes.
- Dataset states must stay isolated from one another.
- `demo_rich_context` must be an explicit study/run configuration, not a global
  default for all data.
