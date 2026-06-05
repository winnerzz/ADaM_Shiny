# Phase 8 UI Simplification Plan

Last updated: 2026-06-04

Status: draft for review

Review status:

- Primary review: completed by Codex from the current architecture and recent
  PSY201 run behavior.
- Sub-agent review: attempted, but unavailable in this session because the
  local Codex/sub2api authentication path returned 401 errors. Do not treat this
  as an independent sub-agent sign-off yet.

## Goal

The UI should let a clinical programmer understand the run without reading raw
LangGraph state or JSON.

The page must always answer three questions:

1. Where am I in the workflow?
2. Why is the selected dataset ready, waiting, blocked, or done?
3. What is the next safe action?

The UI should be simple, but not incomplete. It must still show the full
controlled workflow:

```text
Inputs -> Plan -> Draft/Spec Review -> Generate R Code -> Code Review -> Run R -> Review Output -> Audit
```

## Product Rules

- Keep LangGraph as the source of truth for workflow state.
- Do not create UI-only shortcuts that bypass graph review gates.
- Do not expose raw paths, run ids, JSON, or graph internals in the main view.
- Keep raw artifacts in Advanced Audit.
- Reference ADaM must be labeled as comparison/output-shape evidence only.
- Uploaded input spec is primary. Draft spec is used only when no input spec is provided and must be reviewed.
- Generated R code must be reviewed before local execution.
- A completed runtime output is not clinical proof. It is only evidence for review.

## Proposed Page Shape

Use one screen with four visible areas.

```text
Header
  product name | API status | current study | active target | next action

Left rail
  1 Inputs
  2 Plan
  3 Spec
  4 Code
  5 Run
  6 Output

Main work area
  Next action card
  Active dataset panel
  Review panel / code panel / output panel

Right work queue
  Dataset queue grouped by status
  Human review queue
  Dependency explanation
```

This is still one page. It should not become a dashboard with many competing
cards. The page hierarchy is:

1. Next safe action.
2. Active dataset details.
3. Other dataset statuses.
4. Audit details only when opened.

Rationale:

- The left rail tells the user the general phase.
- The next action card tells the user what to do now.
- The active dataset panel keeps focus on one dataset.
- The right queue preserves multi-dataset context.

## Main Screen Components

### 1. Header

Visible:

- API status
- Study name
- Active target
- Next action
- Small progress bar for long operations
- one clear run mode badge: `real LLM`, `mock`, or `review-only`

Hidden from main view:

- raw run id
- backend folder path
- checkpoint path

These go under Advanced Setup or Advanced Audit.

### 2. Next Action Card

This is the most important UI component.

It should show:

- one title
- one short explanation
- one primary button
- optional secondary button

Examples:

```text
Review the draft spec for ADCM
No uploaded spec was found. Review this generated draft before code generation.
[Review Draft Spec] [Reject Draft Spec]
```

```text
ADDM finished. Continue with ADCM
ADDM has a generated output in this run. ADCM is waiting for code review.
[Open ADCM] [Inspect ADDM]
```

```text
ADLB is waiting for ADSL
ADLB needs real local runtime output from ADSL. Reference ADaM does not satisfy this dependency.
[Open ADSL] [Refresh Progress]
```

Rules:

- Never show more than one primary action.
- If the graph has an open human gate, the primary action must point to that gate.
- If the active dataset is done and another selected dataset needs work, guide the user to the next dataset.
- If nothing is actionable, show Refresh Progress.
- The card must say whether the action will only open a review screen or will
  actually resume the graph.
- The card must not invent an action from frontend state. It must come from the
  graph progress API or graph-state-derived command list.

### 3. Study Inputs Panel

Keep five input groups:

- SDTM
- ADaM Specs
- Reference ADaM
- Define
- Legacy Code

Each group should show:

- file count
- dataset/domain names
- preview status
- remove button
- warning if unreadable
- whether this file group can affect derivation logic

Do not show internal folder paths by default.

Input authority labels:

| Input group | UI authority label |
|---|---|
| Uploaded ADaM spec | Primary derivation instruction |
| Draft spec | Review-required generated instruction |
| SDTM | Source data |
| Define | Metadata evidence |
| Legacy code | Derivation evidence |
| Reference ADaM | Output-shape / comparison evidence only |

The UI must make this clear before planning. Reference ADaM must not be grouped
visually with specs.

For `.sas7bdat`, use this wording:

```text
SAS dataset recognized. It can be used by the R runner when the required R package is available. Browser preview may be limited.
```

### 4. Output Selection

The output selector should support multi-target planning.

It should show:

- selected targets
- active target
- source of each target suggestion
- whether the target is selected, generated, waiting, blocked, or reference-only

Target source labels:

- `uploaded spec`
- `legacy code`
- `run history`
- `manual`
- `reference only`

Reference-only targets must not be auto-planned. They can be selected manually,
but the UI must say:

```text
Reference-only candidate. Selecting it requests generation, but the reference file is not derivation authority.
```

The active target is only the dataset currently shown in the main panel. It must
not mean "the only dataset in the run." Multi-target progress must remain
visible after switching active target.

### 5. Dependency Explanation

Rename the visual purpose from "dependency graph" to:

```text
Why this dataset is waiting
```

For each selected dataset, show:

- What it needs
- Why the system thinks so
- What evidence was used
- What the next action is
- Trust boundary
- whether the dependency can be satisfied by a generated runtime output,
  uploaded runtime output, or user decision

Example:

```text
ADLB
What it needs: real local output from ADSL
Why: uploaded spec or legacy code uses ADSL variables
Evidence: legacy ADLB.sas, uploaded SDTM LB
Next action: complete ADSL, then refresh progress
Boundary: reference ADSL cannot satisfy this runtime dependency
```

Dependency text must avoid absolute claims when the source is weak. Use wording
like "the system thinks this is needed because..." and show the evidence. This
keeps dependency planning reviewable instead of hiding it as a fixed truth.

### 6. Dataset Work Queue

Dataset cards should be grouped, not shown as one flat list.

Groups:

1. Needs review
2. Ready to continue
3. Waiting upstream
4. Blocked
5. Finished or reference-only

Each card should show:

- dataset name
- status
- one-line reason
- next action
- mini stage strip: inputs / plan / spec / code / run

Rules:

- Clicking a card changes the active dataset only.
- It must not start generation, approval, or execution.
- Previous dataset progress must stay visible when active target changes.

### 7. Draft Spec Review

Shown only when:

- no uploaded input spec exists for the active target, and
- a draft spec has been generated, or
- graph progress says draft-spec review is open

Show:

- dataset
- variables table
- source domains
- derivation text
- risk level
- draft warnings
- evidence used to create the draft
- evidence not used as authority
- Approve Draft Spec
- Reject Draft Spec

Reject requires review notes.

Approval rule:

- Approving a draft spec records a graph decision for this run only.
- Approval must be tied to the current run and current draft fingerprint.
- If inputs change after approval, the UI must show that the draft needs a new
  review if the backend reports invalidation.

### 8. R Code Review

Show:

- generated R code
- assumptions
- risk points
- static check result
- provider mode: mock or real LLM
- exposure mode
- Approve Code
- Reject Code

Do not run R from this panel unless code approval is already recorded.

If static checks are not implemented or are partial, the UI must say so plainly.
Do not show a green "passed" style for checks that are only syntax or structural
checks.

### 9. R Execution and Output

Show:

- execution status
- output row count and column count
- preview table
- download button
- validation summary
- diagnostics if failed
- repair/retry action when graph exposes terminal failure review
- whether the output can satisfy downstream runtime dependencies

If output is mock or structural stub, show:

```text
Review-only output. This cannot satisfy downstream runtime dependencies.
```

### 10. Advanced Audit

Collapsed by default.

Contains:

- run id
- study folder
- Rscript path
- LLM provider settings
- graph state links
- raw JSON artifacts
- logs
- diagnostics

Main UI should link to audit records, not render raw JSON.

## State Translation

The UI should translate graph state into plain statuses.

| Graph state / signal | User status |
|---|---|
| no inputs | Load inputs |
| plan missing | Prepare plan |
| dependency review open | Review dependency plan |
| waiting_for_runtime_dependencies | Waiting upstream |
| no input spec and draft missing | Generate/review draft spec |
| draft_spec_review open | Review draft spec |
| code generated | Review code |
| code approved | Run approved code |
| terminal_failure | Repair or retry |
| execution completed | Inspect output |
| compare mismatch | Review differences |

If multiple signals apply, human review gates win over ordinary progress states.
For example, if a dataset has generated code and an open code-review interrupt,
the user status is `Review code`, not `Ready`.

## First Implementation Scope

Keep this as a frontend-focused cleanup.

Implement or keep:

- main next-action card
- grouped dataset work queue
- dependency explanation in plain language
- active dataset preservation across target changes
- clearer `.sas7bdat` wording
- Advanced Audit collapse

Do not implement yet:

- compare as a hard quality gate
- production CDISC/P21 validation
- container sandbox
- file role reassignment metadata
- full frontend framework split

## Review Findings

### P0 Must Fix Before UI Implementation

- The plan is directionally correct, but the UI must define one source for
  "next action." That source should be graph progress plus graph command
  availability, not frontend inference alone.
- Draft spec approval must show run/fingerprint safety indirectly in plain
  language, and the backend must enforce it. The UI should display stale review
  states when inputs change.
- Reference ADaM needs a stronger visual boundary from uploaded specs. It should
  be in a separate "comparison evidence" group, not only labeled in text.

### P1 Should Simplify

- Keep the first screen to three main blocks: next action, active dataset, work
  queue. Inputs and audit can be collapsible after the run is prepared.
- Avoid showing both a left rail and many stage strips if the page feels busy.
  Keep the left rail for global phase and use small stage chips only inside
  dataset cards.
- Do not show provider settings, paths, run id, Rscript path, raw logs, or JSON
  unless the user opens Advanced Audit.

### P1 Must Add For Clinical Programmer Clarity

- Add a "why this action is required" sentence to every gate:
  dependency review, draft spec review, code review, terminal failure review.
- Show whether a generated output is usable runtime evidence for downstream
  datasets.
- For failed runs, distinguish "repair generated code", "rerun approved code",
  and "reject and go back to spec/code review."

### P2 Keep As Later Work

- Compare should remain outside this cleanup unless shown only as optional
  output inspection.
- Production CDISC/P21 validation and container sandbox should stay in Advanced
  Audit / future phases, not in the main workflow UI.

## Acceptance Criteria

A user should be able to run PSY201-like data without guessing:

1. Upload SDTM `.sas7bdat`, reference ADaM `.sas7bdat`, and legacy SAS programs.
2. Select multiple ADaM targets.
3. See which datasets are ready, waiting, blocked, or done.
4. See why a dataset is waiting for an upstream ADaM.
5. Generate or review draft specs when no input spec exists.
6. Review generated R code before running.
7. Run approved R code.
8. Move from completed ADDM to the remaining datasets without losing progress.

The UI is not acceptable if the user must inspect JSON or raw paths to know the
next action.

## Current Open Risks

- Current UI is still one large `web.py` file. This is acceptable for this
  cleanup, but should be split later.
- Browser visual QA was not completed because the in-app browser was unavailable
  and Playwright is not installed.
- The latest UI simplification patch is not committed yet.
- Compare review is intentionally not included in this cleanup.
