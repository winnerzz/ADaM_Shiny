# Phase 7 Design - Multi-Dataset Study Orchestration MVP

Last updated: 2026-05-24

Phase status: Phase 7.3 implemented; Phase 7 still in progress overall

## Purpose

Phase 7 moves from one real dataset to study-level orchestration.

The first target is not to generate every ADaM dataset. The first target is to
make dataset dependency planning explicit and auditable.

Plain language for Phase 7.1-7.3:

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

Phase 7.4 refines this behavior. Dependency discovery is still automatic, but
discovered dependencies are not automatically approved for execution. In
user-facing language, `auto_added_datasets` should be treated as:

```text
required by dependency planning
```

not:

```text
approved for automatic execution
```

See `docs/phase7_4_design.md` for the general dependency resolution rule.

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
- `dependency_review_status`
- `dependency_plan_artifact`
- `dependency_review_artifact`

The study-level audit stub should include the same fields so another tool can
explain why ADSL was inserted.

## Phase 7.3 Review Contract

Phase 7.3 makes the dependency plan reviewable as files, not only as transient
LangGraph state.

When `study_dir` is available, StudyGraph writes:

```text
runs/{run_id}/planning/dependency_plan.json
runs/{run_id}/planning/dependency_review.md
```

Plain language:

- `dependency_plan.json` is for software tools and later UI screens.
- `dependency_review.md` is for a human reviewer.
- Both files say what the user requested, what the system added, what will run
  first, what depends on what, and what warnings exist.

Review status values:

```text
accepted
  no warnings and no review-required dependency decision

review_required
  the plan can run, but at least one decision uses MVP fallback or file-derived
  evidence that should be checked

warning
  the plan can run, but there is a dependency warning such as input_spec versus
  SAS/define conflict

blocked
  unsupported non-ADaM targets were requested
```

Important boundary:

If `input_spec` is present, it is treated as the user-provided spec. It can
drive the dependency plan directly. Legacy SAS and define.xml are scanned only
to find conflicts. A consistent secondary scan should stay quiet.

If `input_spec` is present but does not cover a requested or auto-added target
dataset, the system may still use MVP fallback ordering, but it must record a
planning warning. This prevents the UI from hiding the difference between:

```text
spec explicitly supports this dependency
```

and:

```text
spec folder exists, but no dependency evidence was extracted for this dataset
```

Phase 7.3 also writes a real study-level manifest when `study_dir` is available:

```text
runs/{run_id}/audit/manifest.json
```

That manifest links the dependency plan/review artifacts and the dataset-level
audit artifacts returned by dataset subgraphs.

When StudyGraph invokes the real ADSL minimal dataset subgraph, the dataset
manifest is written as:

```text
runs/{run_id}/audit/adsl_manifest.json
```

This avoids overwriting the study-level `audit/manifest.json`. When the ADSL
service runs alone through its CLI/service boundary, it still writes the
canonical `audit/manifest.json`.

## Exit Criteria

Phase 7.1 is complete when:

- dependency planning lives in a dedicated module
- downstream-only requests auto-add ADSL
- multiple downstream requests auto-add ADSL only once
- ADSL failure blocks downstream datasets
- downstream stub failure does not change completed ADSL status
- audit manifest metadata records requested/auto-added/dependency evidence
- dependency plan and dependency review artifacts are written under
  `runs/{run_id}/planning/` when a study directory is available
- full test suite passes

## Follow-Ups

- implement Phase 7.4 general dependency resolution and LLM-driven downstream
  generation
- production-grade define.xml parsing
- production-grade spec dependency parsing beyond lightweight ADaM token
  detection
- production-grade legacy SAS dependency parsing beyond simple merge/set/join
  lines
