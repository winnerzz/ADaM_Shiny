# Phase 7 Design - Multi-Dataset Study Orchestration MVP

Last updated: 2026-05-24

Phase status: implementation in progress

## Purpose

Phase 7 moves from one real dataset to study-level orchestration.

The first target is not to generate every ADaM dataset. The first target is to
make dataset dependency planning explicit and auditable.

Plain language:

```text
requested datasets
  -> decide which foundation datasets are needed
  -> run foundation datasets first
  -> run downstream datasets only if dependencies pass
  -> block downstream datasets if ADSL fails
  -> summarize the study result
```

## Phase 7.1 Scope

Phase 7.1 implements a dependency orchestration MVP.

It does not implement real ADAE/ADCM/ADLB generation yet. Downstream datasets
can still use stub dataset graphs while ADSL remains the first real executable
dataset.

## Dependency Principle

A dataset depends on another dataset when it needs variables or outputs from
that dataset.

For ADaM, `ADSL` is usually the subject-level foundation because downstream
analysis datasets often need subject-level treatment, date, population, and
demographic information.

Example:

```text
ADAE = AE event records + ADSL subject-level variables
ADCM = CM records + ADSL subject-level variables
ADLB = LB records + ADSL subject-level variables
```

## Evidence Priority for Dependencies

Dependency planning follows a spec-first rule:

1. If `input_spec` is provided, use it as the authoritative dependency source.
2. When `input_spec` exists, scan legacy SAS and define.xml only as consistency
   checks. Do not let them change the dependency plan. Report only conflicts.
3. If `input_spec` is missing, use legacy SAS and define.xml as draft
   dependency evidence.
4. If no usable dependency evidence exists, use the MVP fallback default.

The fallback default is recorded as:

```text
dependency_evidence = phase7_mvp_fallback_adsl_foundation
```

This is an orchestration default, not a clinical derivation rule.

This fallback is intentionally not a complete ADaM dependency graph. It only
keeps the study run order safe enough for the current MVP. Real cross-ADaM
dependencies, such as an endpoint dataset depending on another analysis dataset,
must come from spec/SAS/define/human evidence in later phases.

## MVP Default

Fallback rule:

```text
ADSL has no dependencies.
ADAE, ADCM, ADLB, ADEX, ADEG depend on ADSL.
Unknown AD* downstream datasets also depend on ADSL by default.
```

Each dependency decision must carry:

- `dataset`
- `dependencies`
- `source`
- `confidence`
- `review_required`
- `reason`

Common downstream fallback examples such as `ADAE -> ADSL` are still
`review_required = true` because the exact dependency should be checked against
study evidence.

Unknown `AD*` datasets get lower confidence:

```text
source = mvp_unknown_adam_fallback
confidence = 0.4
review_required = true
```

That means the system may use the fallback to keep ordering safe, but it must
not present the dependency as a verified production plan.

Non-AD targets such as `LB` or `DM` are not ADaM outputs in this MVP. They should
not be sent through the dataset graph and marked completed by a stub. They are
recorded as:

```text
unsupported_datasets = ["LB"]
validation_status = unsupported_dataset
```

If the user requests only a downstream dataset:

```text
requested = ["ADAE"]
target = ["ADSL", "ADAE"]
auto_added = ["ADSL"]
```

If the user requests multiple downstream datasets:

```text
requested = ["ADAE", "ADCM"]
target = ["ADSL", "ADAE", "ADCM"]
auto_added = ["ADSL"]
```

ADSL should run only once.

## State Additions

Study graph state should expose:

- `requested_datasets`
- `target_datasets`
- `auto_added_datasets`
- `foundation_datasets`
- `downstream_datasets`
- `unsupported_datasets`
- `dataset_dependencies`
- `dependency_decisions`
- `dependency_graph`
- `dependency_evidence`

The study-level audit stub should include the same fields so another tool can
explain why ADSL was inserted.

## Exit Criteria

Phase 7.1 is complete when:

- dependency planning lives in a dedicated module
- downstream-only requests auto-add ADSL
- multiple downstream requests auto-add ADSL only once
- ADSL failure blocks downstream datasets
- downstream stub failure does not change completed ADSL status
- audit manifest metadata records requested/auto-added/dependency evidence
- full test suite passes

## Follow-Ups

- production-grade define.xml parsing
- production-grade spec dependency parsing beyond lightweight ADaM token
  detection
- production-grade legacy SAS dependency parsing beyond simple merge/set/join
  lines
- real ADAE minimal loop
