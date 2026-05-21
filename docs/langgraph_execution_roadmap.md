# LangGraph ADaM Agent Execution Roadmap

This file is the project handoff ledger for the LangGraph ADaM Agent build.

Purpose:

- keep the staged execution plan in the repository
- record what was actually completed at the end of each phase
- preserve verification commands, file changes, blockers, and next actions
- make context handoff easier after chat interruption or when another AI tool
  continues the work

Update rule:

- Update this file at the end of every phase.
- Do not only mark a phase as done. Record evidence: files changed, commands run,
  results, assumptions, and open issues.
- If a phase is partially done, mark it as `partial` and explain the missing part.
- Keep this file factual. Do not use it as a brainstorming document.

## Current Status

| Field | Value |
|---|---|
| Current phase | Phase 0 - Engineering foundation |
| Current branch observed | `experimental-v3` |
| Active LangGraph worktree | `D:\Archive\Research\Projects\ADaM_Shiny_LangGraph` |
| Target architecture branch | `LangGraph` |
| Product direction | Local-first ADaM Agent Studio using LangGraph orchestration and R sandbox execution |
| Current implementation status | Architecture foundation and empty project skeleton created |
| Last roadmap update | 2026-05-21 |

Known workspace notes:

- The existing Shiny prototype remains in the repository.
- A local empty `LangGraph` branch exists.
- A separate worktree has been created for `LangGraph` at
  `D:\Archive\Research\Projects\ADaM_Shiny_LangGraph`.
- Remote push previously failed because GitHub connectivity failed.
- The current worktree may contain unrelated local changes such as `.Rhistory`, `demo-data/PSY201/`, and `docs/`.
- Preserve user work before switching branches or restructuring files.

## Phase Map

```text
Phase 0  Engineering foundation
  -> Phase 1  Product and data contracts
  -> Phase 2  State model
  -> Phase 3  LangGraph skeleton
  -> Phase 4  Tool layer MVP
  -> Phase 5  Single-dataset real loop
  -> Phase 6  Failure diagnosis and rollback
  -> Phase 7  Multi-dataset study orchestration
  -> Phase 8  Product UI and audit workflow
  -> Phase 9  Standards and production hardening
```

## Phase Summary

| Phase | Goal | Main Outputs | Exit Criteria | Status |
|---|---|---|---|---|
| 0. Engineering foundation | Create a safe project base for the LangGraph build | branch/worktree decision, `CODEX.md`, roadmap, initial docs | New work can proceed without damaging the Shiny prototype | in progress |
| 1. Product and data contracts | Define exactly what the MVP accepts and produces | input/output contract, study folder convention, data governance rules | The first MVP scope is unambiguous | not started |
| 2. State model | Define `StudyState` and `DatasetState` | schema files and examples | Dataset-level state isolation is explicit | not started |
| 3. LangGraph skeleton | Prove main graph and dataset subgraph orchestration | stub `StudyGraph`, stub `DatasetGraph`, checkpoint stub | ADSL/ADAE stubs can be dispatched and collected | not started |
| 4. Tool layer MVP | Add deterministic interfaces around data, artifacts, LLM, and R | SDTM reader stub, artifact manifest, LLM client interface, R runner stub | Graph nodes call tools through stable interfaces | not started |
| 5. Single-dataset real loop | Run one real ADaM dataset end to end, likely ADSL first | lineage/spec/code/run/validate path for ADSL | One dataset can run from inputs to validated output | not started |
| 6. Failure diagnosis and rollback | Add controlled repair and backward routing | `diagnose_failure`, `repair_code`, `revise_spec`, `revise_lineage` | Failures are classified instead of blindly repairing code | not started |
| 7. Multi-dataset study orchestration | Coordinate multiple ADaM datasets with dependencies | dependency graph, dataset dispatch/reduce logic | ADSL can complete before dependent datasets run | not started |
| 8. Product UI and audit workflow | Make the system usable by a human reviewer | FastAPI/UI, run history, review views, audit report | User can upload, run, review, and export | not started |
| 9. Standards and production hardening | Add reference standards and production controls | CDISC/P21 tools, provider expansion, security/deployment strategy | System is extensible beyond demo data | not started |

## Phase 0 - Engineering Foundation

Goal:

Create a safe project base for the LangGraph build while preserving the existing
Shiny prototype and user data.

Planned work:

- Confirm current git branch and dirty worktree.
- Decide whether to work directly in this worktree or create a separate worktree
  for `LangGraph`.
- Preserve existing Shiny prototype files.
- Add project-level Codex guidance.
- Add this roadmap/handoff file.
- Keep architecture documentation in `docs/`.
- Commit the architecture starting point when branch/worktree state is clean
  enough.

Completed work:

- Added project-level Codex guidance at `CODEX.md`.
- Added human-readable LangGraph architecture document at
  `docs/langgraph_adam_architecture.html`.
- Added this execution roadmap at `docs/langgraph_execution_roadmap.md`.
- Confirmed current Shiny prototype worktree remains on `experimental-v3`.
- Confirmed `LangGraph` branch exists at empty-tree commit
  `74a6ac7 chore: initialize LangGraph architecture branch`.
- Created separate `LangGraph` worktree at
  `D:\Archive\Research\Projects\ADaM_Shiny_LangGraph`.
- Copied `CODEX.md`, `docs/langgraph_adam_architecture.html`, and this roadmap
  into the separate `LangGraph` worktree.
- Created the initial empty project skeleton:
  - `src/adam_agent/graph/`
  - `src/adam_agent/schemas/`
  - `src/adam_agent/nodes/study/`
  - `src/adam_agent/nodes/dataset/`
  - `src/adam_agent/tools/`
  - `src/adam_agent/llm/`
  - `references/CDISC/`
  - `references/company_standards/`
  - `studies/_template/`
  - `tests/fixtures/`
- Added small README files and package marker files so the skeleton is visible
  and trackable in git.

Files created or changed:

- `CODEX.md`
- `README.md`
- `docs/langgraph_adam_architecture.html`
- `docs/langgraph_execution_roadmap.md`
- `references/README.md`
- `studies/_template/README.md`
- `tests/README.md`
- `src/adam_agent/__init__.py`
- `src/adam_agent/graph/__init__.py`
- `src/adam_agent/schemas/__init__.py`
- `src/adam_agent/nodes/__init__.py`
- `src/adam_agent/nodes/study/__init__.py`
- `src/adam_agent/nodes/dataset/__init__.py`
- `src/adam_agent/tools/__init__.py`
- `src/adam_agent/llm/__init__.py`
- `.gitkeep` files under empty reference/study/test fixture directories

Verification:

```powershell
git status --short --branch
git branch --list
git worktree list
git log --oneline -1 LangGraph
git ls-tree -r --name-only LangGraph
rg --files
Get-Content -Raw CODEX.md
Get-Content -Raw docs\langgraph_execution_roadmap.md
```

Observed verification notes:

- `git worktree list` shows:
  - `D:/Archive/Research/Projects/ADaM_Shiny-ADaM_Shiny_experimental` on
    `experimental-v3`
  - `D:/Archive/Research/Projects/ADaM_Shiny_LangGraph` on `LangGraph`
- `rg --files` in the LangGraph worktree shows only new architecture files and
  skeleton directories, not old Shiny prototype code.

Current blockers or risks:

- The original Shiny prototype worktree is not clean, but it is isolated from
  the new `LangGraph` worktree.
- `LangGraph` is checked out in the separate worktree.
- Remote push to GitHub previously failed because of network connectivity.
- `demo-data/PSY201/` is untracked and may be user-supplied data; do not delete
  or move it without explicit confirmation.

Recommended next action:

Commit the architecture starting point in the clean `LangGraph` worktree, then
push `LangGraph` when GitHub connectivity allows.

Phase 0 exit criteria:

- Clear decision on active LangGraph worktree.
- Architecture docs and roadmap are present in the LangGraph worktree.
- No user data is lost or overwritten.
- A first architecture commit exists on `LangGraph`.
- Remote branch is pushed when GitHub connectivity allows.

Phase 0 status:

`in progress - ready to commit`

## Phase 0 Handoff Record - 2026-05-21

Date:

2026-05-21

Phase:

Phase 0 - Engineering foundation

Status:

`in progress - ready to commit`

What changed:

- Created an isolated `LangGraph` worktree at
  `D:\Archive\Research\Projects\ADaM_Shiny_LangGraph`.
- Kept the existing Shiny prototype worktree on `experimental-v3`.
- Added Codex project guidance and architecture docs to the LangGraph worktree.
- Created the first new-project skeleton for future LangGraph development.

Files changed:

- `CODEX.md`
- `README.md`
- `docs/langgraph_adam_architecture.html`
- `docs/langgraph_execution_roadmap.md`
- `references/README.md`
- `studies/_template/README.md`
- `tests/README.md`
- `src/adam_agent/**/__init__.py`
- `.gitkeep` files in empty placeholder directories

Commands run:

- `git status --short --branch`
- `git branch --list`
- `git worktree list`
- `git remote -v`
- `git log --oneline -1 LangGraph`
- `git ls-tree -r --name-only LangGraph`
- `git worktree add ..\ADaM_Shiny_LangGraph LangGraph`
- `rg --files`

Verification result:

- `LangGraph` is checked out in its own worktree.
- The new worktree contains only the new architecture docs and skeleton.
- The old Shiny prototype worktree still has its previous local changes and was
  not modified by branch switching.

Decisions made:

- Use a separate worktree instead of switching the dirty `experimental-v3`
  worktree.
- Start `LangGraph` from an empty tree rather than carrying over the Shiny
  prototype.
- Keep only architecture docs and project skeleton in the first commit.

Open issues:

- Remote push has not been retried in this phase.
- Phase 1 product/data contracts are not started.

Recommended next action:

- Commit this architecture starting point.
- Then begin Phase 1 by writing the MVP input/output contract and study folder
  convention.

## Phase 1 - Product and Data Contracts

Goal:

Define the product boundary before implementation. This prevents the MVP from
expanding into a vague "generate all ADaM" promise.

Planned work:

- Define accepted input folders and file types.
- Define output artifacts.
- Define the first supported study layout.
- Define data privacy rules for LLM calls.
- Define the minimum spec-equivalent contract.
- Decide MVP target datasets.

Likely MVP scope:

- Study-level input scan.
- SDTM metadata/profile ingestion.
- Optional reference ADaM comparison.
- ADSL first as the first real dataset.
- ADAE as the first dependent dataset later.
- Metadata-first LLM calls by default.

Expected outputs:

- `docs/product_contract.md`
- `docs/data_governance.md`
- `studies/_template/`

Exit criteria:

- Another developer or AI tool can tell exactly what files to provide for a test
  run and what outputs to expect.

Status:

`not started`

## Phase 2 - State Model

Goal:

Define the state objects before graph implementation.

Planned work:

- Create `StudyState`.
- Create `DatasetState`.
- Define artifact references.
- Define risk/confidence routing fields.
- Define human decision records.
- Define failure diagnosis records.

Expected outputs:

- `src/adam_agent/schemas/study_state.py`
- `src/adam_agent/schemas/dataset_state.py`
- `src/adam_agent/schemas/artifacts.py`
- `src/adam_agent/schemas/audit.py`
- state example fixtures under `tests/fixtures/`

Exit criteria:

- Per-dataset repair attempts and decisions cannot leak into another dataset.
- Large files are represented by path/hash, not embedded in state.

Status:

`not started`

## Phase 3 - LangGraph Skeleton

Goal:

Prove that the graph architecture runs before adding real ADaM logic.

Planned work:

- Implement stub `StudyGraph`.
- Implement stub `DatasetGraph`.
- Add checkpointing.
- Add dataset dispatch and reduce behavior.
- Add one risk-based routing branch.
- Add smoke tests.

Expected outputs:

- `src/adam_agent/graph/study_graph.py`
- `src/adam_agent/graph/dataset_graph.py`
- `src/adam_agent/graph/routing.py`
- `tests/test_graph_smoke.py`

Minimal target flow:

```text
scan_study_inputs
  -> build_dataset_dependency_graph
  -> dispatch DatasetGraph(ADSL)
  -> dispatch DatasetGraph(ADAE)
  -> reduce_dataset_results
  -> write audit manifest
```

Exit criteria:

- The graph can run locally with stub datasets.
- ADSL and ADAE state objects are separate.
- A checkpoint is created.
- An audit manifest is written.

Status:

`not started`

## Phase 4 - Tool Layer MVP

Goal:

Keep graph nodes clean by moving deterministic operations behind stable tools.

Planned work:

- Add SDTM reader/profile tool.
- Add artifact store/manifest tool.
- Add simple LLM client abstraction.
- Add R runner stub.
- Add config loading.

Expected outputs:

- `src/adam_agent/tools/sdtm_reader.py`
- `src/adam_agent/tools/artifacts.py`
- `src/adam_agent/tools/r_runner.py`
- `src/adam_agent/llm/clients.py`
- `src/adam_agent/llm/model_registry.py`

Exit criteria:

- Graph nodes do not call providers directly.
- Graph nodes do not manually scatter file I/O.
- Mock mode can run without real API keys.

Status:

`not started`

## Phase 5 - Single-Dataset Real Loop

Goal:

Run one real dataset through a real lineage/spec/code/run/validate loop.

Recommended target:

- Start with ADSL.

Reason:

- ADSL is the subject-level foundation for many later ADaM datasets.
- ADAE is more complex and should not be the first real target.

Planned work:

- Read SDTM DM/EX and relevant inputs.
- Draft or load a spec-equivalent contract.
- Generate R code through the isolated LLM interface.
- Execute through R sandbox.
- Validate output structure and key variables.
- Compare to reference ADaM if available.

Exit criteria:

- One ADSL run produces a concrete output artifact.
- The run has an audit trail.
- Validation results are structured.

Status:

`not started`

## Phase 6 - Failure Diagnosis and Rollback

Goal:

Prevent blind repair loops.

Planned work:

- Add failure classification.
- Route code errors to `repair_code`.
- Route spec errors to `revise_spec`.
- Route lineage errors to `revise_lineage`.
- Escalate repeated or ambiguous failures to human review.

Exit criteria:

- The graph can intentionally route backward from execution failure to spec or
  lineage revision.
- Repair attempts are bounded per dataset.

Status:

`not started`

## Phase 7 - Multi-Dataset Study Orchestration

Goal:

Move from one dataset to study-level orchestration.

Planned work:

- Build dataset dependency graph.
- Run independent datasets in parallel where safe.
- Enforce ADSL-before-dependent-datasets behavior.
- Reduce dataset results into a study-level summary.

Exit criteria:

- ADSL can feed downstream datasets.
- A downstream dataset failure does not corrupt unrelated dataset state.

Status:

`not started`

## Phase 8 - Product UI and Audit Workflow

Goal:

Expose the workflow to a user.

Planned work:

- Decide UI path: keep Shiny temporarily or move to FastAPI + browser UI.
- Add run creation.
- Add upload/selection screens.
- Add human review screens for high-risk decisions.
- Add audit report download.

Exit criteria:

- A user can create a run, inspect status, approve high-risk items, and download
  outputs/audit files.

Status:

`not started`

## Phase 9 - Standards and Production Hardening

Goal:

Move from MVP to extensible product.

Planned work:

- Add CDISC reference tools.
- Add P21 rule lookup.
- Add define.xml parsing.
- Add company-standard lookup.
- Add more model providers and modern model IDs.
- Add deployment/security strategy.
- Add stronger test fixtures.

Exit criteria:

- The system can support new studies and new target datasets without rewriting
  the orchestration layer.

Status:

`not started`

## Handoff Template

Use this template at the end of each phase or major work session.

```text
Date:
Phase:
Status: not started / in progress / partial / complete

What changed:
- 

Files changed:
- 

Commands run:
- 

Verification result:
- 

Decisions made:
- 

Open issues:
- 

Recommended next action:
- 
```
