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
| Current phase | Phase 5 - Single-dataset real loop |
| Original Shiny worktree branch | `experimental-v3` |
| Active worktree branch | `LangGraph` |
| Active LangGraph worktree | `D:\Archive\Research\Projects\ADaM_Shiny_LangGraph` |
| Target architecture branch | `LangGraph` |
| Product direction | Local-first ADaM Agent Studio using LangGraph orchestration and R sandbox execution |
| Current implementation status | Phase 4 complete; ready for Phase 5 single-dataset ADSL loop |
| Last roadmap update | 2026-05-23 |

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
| 0. Engineering foundation | Create a safe project base for the LangGraph build | branch/worktree decision, `CODEX.md`, roadmap, initial docs | New work can proceed without damaging the Shiny prototype | complete |
| 1. Product and data contracts | Define exactly what the MVP accepts and produces | input/output contract, study folder convention, data governance rules | The first MVP scope is unambiguous | complete |
| 2. State model | Define `StudyState` and `DatasetState` | schema files and examples | Dataset-level state isolation is explicit | complete |
| 3. LangGraph skeleton | Prove main graph and dataset graph orchestration | stub `StudyGraph`, stub `DatasetGraph`, checkpoint stub | ADSL/ADAE stubs can be dispatched and collected | complete |
| 4. Tool layer MVP | Add deterministic interfaces around data, artifacts, LLM, and R | SDTM reader stub, artifact manifest, LLM client interface, R runner stub | Graph nodes call tools through stable interfaces | complete |
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
- Remote push now succeeds; `LangGraph` tracks `origin/LangGraph`.
- `demo-data/PSY201/` is untracked and may be user-supplied data; do not delete
  or move it without explicit confirmation.

Recommended next action:

Begin Phase 1 by writing the MVP input/output contract and study folder
convention.

Phase 0 exit criteria:

- Clear decision on active LangGraph worktree.
- Architecture docs and roadmap are present in the LangGraph worktree.
- No user data is lost or overwritten.
- A first architecture commit exists on `LangGraph`.
- Remote branch is pushed when GitHub connectivity allows.

Phase 0 status:

`complete`

## Phase 0 Handoff Record - 2026-05-21

Date:

2026-05-21

Phase:

Phase 0 - Engineering foundation

Status:

`complete`

What changed:

- Created an isolated `LangGraph` worktree at
  `D:\Archive\Research\Projects\ADaM_Shiny_LangGraph`.
- Kept the existing Shiny prototype worktree on `experimental-v3`.
- Added Codex project guidance and architecture docs to the LangGraph worktree.
- Created the first new-project skeleton for future LangGraph development.
- Committed the architecture starting point.
- Pushed `LangGraph` to GitHub and set upstream tracking.

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
- `git add CODEX.md README.md docs references src studies tests`
- `git commit -m "chore: add LangGraph architecture foundation"`
- `git push -u origin LangGraph`

Verification result:

- `LangGraph` is checked out in its own worktree.
- The new worktree contains only the new architecture docs and skeleton.
- The old Shiny prototype worktree still has its previous local changes and was
  not modified by branch switching.
- Latest local and remote branch:
  `1d92c71 chore: add LangGraph architecture foundation`.
- `git status --short --branch` in the LangGraph worktree was clean after the
  architecture commit.

Decisions made:

- Use a separate worktree instead of switching the dirty `experimental-v3`
  worktree.
- Start `LangGraph` from an empty tree rather than carrying over the Shiny
  prototype.
- Keep only architecture docs and project skeleton in the first commit.

Open issues:

- Phase 1 product/data contracts are not started.

Recommended next action:

- Begin Phase 1 by writing the MVP input/output contract and study folder
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
- `docs/input_contract.md`
- `docs/output_contract.md`
- `docs/spec_draft_contract.md`
- `docs/data_governance.md`
- `studies/_template/`

Completed work:

- Added `docs/product_contract.md`.
- Added `docs/input_contract.md`.
- Added `docs/output_contract.md`.
- Added `docs/spec_draft_contract.md`.
- Added `docs/data_governance.md`.
- Updated `studies/_template/README.md`.
- Added `studies/_template/runs/.gitkeep`.

Decisions recorded:

- First real end-to-end target is `ADSL`.
- First supported formats are `csv` and `sas7bdat`.
- If spec is missing, generate a draft spec from evidence instead of inventing
  one from model memory.
- Draft spec evidence priority now separates derivation-intent evidence from
  output-shape evidence. Existing spec, legacy SAS, SAP/protocol/TFL shells, and
  define.xml should carry derivation intent before reference ADaM values.
- Current processed demo data may use `demo_rich_context` for LLM calls only
  when the study/run explicitly declares `data_classification = processed_demo`
  and `external_api_allowed = true`.
- Unknown or real clinical data defaults to `metadata_only`.
- Canonical generated outputs live under `studies/{study_id}/runs/{run_id}/`.
- `adsl_approved_spec.json` must contain approval metadata or an explicit
  `demo_only_no_review` bypass; it must not be a silent copy of draft spec.
- `.sas7bdat` is treated as a data table readable by R packages.
- `.sas` programs are text evidence in the MVP and are not executed by the R
  sandbox.

Exit criteria:

- Another developer or AI tool can tell exactly what files to provide for a test
  run and what outputs to expect.

Status:

`complete`

## Phase 1 Handoff Record - 2026-05-21

Date:

2026-05-21

Phase:

Phase 1 - Product and data contracts

Status:

`complete`

What changed:

- Converted product-scope discussion into repository documents.
- Defined ADSL as the first real dataset target.
- Defined `csv` and `sas7bdat` as first supported formats.
- Defined how draft specs can be generated when formal specs are missing.
- Defined LLM exposure modes and accepted `demo_rich_context` for current demo
  development only with explicit run configuration.
- Sub-agents reviewed the contracts for handoff consistency and clinical-data
  product risk.
- Fixed high-priority review findings around exposure defaults, canonical output
  paths, approval gate, evidence priority, and ADSL MVP scope.
- User reviewed and accepted the remaining Phase 1 decisions:
  - ADSL first-version minimal variable boundary is enough.
  - Spec-draft evidence priority is accepted.
  - `full_data_allowed` responsibility model is accepted.
  - Phase 1 may be completed.
- Added SAS boundary clarification:
  - `.sas7bdat` files are data tables readable through R packages.
  - `.sas` files are code/text evidence and are not executed by the R sandbox in
    the MVP.

Files changed:

- `docs/product_contract.md`
- `docs/input_contract.md`
- `docs/output_contract.md`
- `docs/spec_draft_contract.md`
- `docs/data_governance.md`
- `studies/_template/README.md`
- `studies/_template/runs/.gitkeep`
- `studies/_template/legacy_code/.gitkeep`
- `docs/phase1_review_temp.html`
- `docs/langgraph_execution_roadmap.md`

Commands run:

- `git status --short --branch`
- `rg --files`
- sub-agent review for documentation consistency
- sub-agent review for clinical-data/ADaM product risk
- user review of Phase 1 decisions

Verification result:

- Files were created in the clean `LangGraph` worktree.
- No code implementation was added in Phase 1 yet.
- Two sub-agent reviews completed and identified contract-level issues.
- Findings were addressed in the contract documents before commit.
- User approved Phase 1 completion.

Decisions made:

- Spec can be missing, but generated draft spec must carry evidence,
  confidence, assumptions, and review flags.
- Reference ADaM is useful for output shape and comparison, but should not drive
  derivation intent by itself.
- Demo data can expose richer context to external APIs during the MVP build only
  when explicitly classified and allowed.
- `full_data_allowed` remains available for user-approved debugging sessions;
  the approving user is responsible for that exposure decision and the run must
  record it in audit.
- Exposure mode remains explicit and auditable.
- Canonical run artifacts belong under `runs/{run_id}`.
- Approved spec requires approval metadata or explicit demo bypass.
- R sandbox reads `.sas7bdat` data but does not execute `.sas` programs in the
  MVP.

Open issues:

- Exact ADSL MVP variable schema still needs to be represented in Phase 2 state
  model or a Phase 2 fixture.

Recommended next action:

- Begin Phase 2 by defining `StudyState`, `DatasetState`, artifact references,
  approval records, evidence records, and LLM exposure mode fields.

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

- `docs/state_model.md`
- `src/adam_agent/schemas/study_state.py`
- `src/adam_agent/schemas/dataset_state.py`
- `src/adam_agent/schemas/artifacts.py`
- `src/adam_agent/schemas/audit.py`
- state example fixtures under `tests/fixtures/`

Completed work:

- Added first draft of `docs/state_model.md`.
- Defined proposed `StudyState`.
- Defined proposed `DatasetState`.
- Defined supporting state objects:
  - `ArtifactRef`
  - `EvidenceRecord`
  - `ApprovalRecord`
  - `LLMExposureConfig`
  - `LLMCallRecord`
  - `RouteDecision`
  - `FailureRecord`
- Captured Phase 1 decisions in state form:
  - reference ADaM is output-shape/validation evidence, not sole derivation
    logic
  - `.sas7bdat` is data and `.sas` is text evidence in the MVP
  - `full_data_allowed` is allowed for user-approved debugging sessions and must
    be audited
  - dataset retry counts must be isolated in `DatasetState`
- Implemented Pydantic v2 schema modules:
  - `src/adam_agent/schemas/base.py`
  - `src/adam_agent/schemas/artifacts.py`
  - `src/adam_agent/schemas/evidence.py`
  - `src/adam_agent/schemas/approval.py`
  - `src/adam_agent/schemas/llm.py`
  - `src/adam_agent/schemas/routing.py`
  - `src/adam_agent/schemas/specs.py`
  - `src/adam_agent/schemas/states.py`
- Added `pyproject.toml` with the package and Pydantic dependency declaration.
- Added minimal ADSL fixtures:
  - `tests/fixtures/study_state_adsl_minimal.json`
  - `tests/fixtures/dataset_state_adsl_minimal.json`
- Added `tests/test_state_schemas.py`.
- Verified JSON round-trip, strict extra-field rejection, dataset retry isolation,
  LLM exposure validation, LLM call policy validation, artifact role/SAS boundary
  checks, spec variable evidence/approval checks, and reference ADaM evidence
  guard.

Exit criteria:

- Per-dataset repair attempts and decisions cannot leak into another dataset.
- Large files are represented by path/hash, not embedded in state.
- State objects are JSON-serializable.
- Minimal fixtures exist for an ADSL run.

Status:

`complete`

## Phase 2 Handoff Record - 2026-05-22

Date:

2026-05-22

Phase:

Phase 2 - State model

Status:

`complete`

What changed:

- Started Phase 2 with a documentation-first state model.
- Added `docs/state_model.md`.
- Implemented Pydantic v2 schema modules after user asked to start writing.
- Added minimal JSON fixtures and standard-library tests.

Files changed:

- `docs/state_model.md`
- `docs/langgraph_execution_roadmap.md`
- `src/adam_agent/schemas/base.py`
- `src/adam_agent/schemas/artifacts.py`
- `src/adam_agent/schemas/evidence.py`
- `src/adam_agent/schemas/approval.py`
- `src/adam_agent/schemas/llm.py`
- `src/adam_agent/schemas/routing.py`
- `src/adam_agent/schemas/specs.py`
- `src/adam_agent/schemas/states.py`
- `src/adam_agent/schemas/__init__.py`
- `pyproject.toml`
- `tests/fixtures/study_state_adsl_minimal.json`
- `tests/fixtures/dataset_state_adsl_minimal.json`
- `tests/test_state_schemas.py`

Commands run:

- `git status --short --branch`
- `rg --files`
- `Get-Content -Raw docs\langgraph_execution_roadmap.md`
- `python --version`
- `python -c "import pydantic; print(pydantic.__version__)"`
- `python -c "import pytest; print(pytest.__version__)"`
- `python -m unittest discover -s tests -p "test_*.py"`

Verification result:

- Worktree was clean before Phase 2 draft work.
- Python version observed: `3.14.4`.
- Pydantic version observed: `2.13.2`.
- `pytest` was not installed, so tests use standard-library `unittest`.
- Test result after schema hardening: `Ran 12 tests ... OK`.

Decisions made:

- Start Phase 2 with a reviewable state contract before coding schema classes.
- Keep large tables and reports out of state and behind `ArtifactRef`.
- Keep repair attempts, failures, route decisions, approvals, and evidence inside
  each dataset state.
- Use Pydantic v2 for checkpoint/API/audit-facing schemas.
- Keep `RouteDecision` and `FailureRecord` lightweight in the first
  implementation.
- Add minimal `SpecVariable` and `SpecDocument` schemas so draft/approved specs
  are not arbitrary dicts.
- Add `DatasetResultSummary` so `StudyState` cannot accidentally embed full
  dataset state.
- Use UTC timestamps for schema defaults.
- Keep dataset-level LLM exposure as a snapshot/narrowing of study-level policy;
  dataset subgraphs must not silently escalate exposure mode.

Open issues:

- Future graph implementation must confirm these schemas work with LangGraph
  checkpointing.
- Optional editable-install verification can be run if packaging behavior needs
  to be checked before Phase 3:
  `python -m pip install -e .`

Recommended next action:

- Begin Phase 3 by building a minimal LangGraph main graph and dataset subgraph
  skeleton around these state objects.

## Phase 3 - LangGraph Skeleton

Goal:

Prove that the graph architecture runs before adding real ADaM logic.

Planned work:

- Add bounded `langgraph` dependency and verify local import/API behavior.
- Implement stub `StudyGraph`.
- Implement stub `DatasetGraph`.
- Add checkpointing through the parent graph.
- Add dependency-aware dataset dispatch and reduce behavior.
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
  -> run DatasetGraph(ADSL)
  -> if ADSL succeeds, dispatch downstream DatasetGraph(ADAE)
  -> if ADSL fails, mark downstream blocked
  -> reduce_dataset_results
  -> write audit manifest
```

Exit criteria:

- The graph can run locally with stub datasets.
- ADSL runs before downstream datasets in the MVP skeleton.
- ADSL failure marks downstream datasets blocked/skipped.
- ADSL and ADAE state objects are separate.
- A checkpoint is created.
- An audit manifest is written.

Status:

`complete`

## Phase 3 Handoff Record - 2026-05-22

Date:

2026-05-22

Phase:

Phase 3 - LangGraph skeleton

Status:

`complete`

What changed:

- Added Phase 3 design and review documents.
- Added `langgraph>=0.6,<1` dependency and installed local editable package for
  implementation verification.
- Implemented runtime graph state with `StudyGraphState` and
  `DatasetGraphState`.
- Implemented dataset-level stub graph.
- Implemented study-level graph with ADSL foundation-first behavior.
- Implemented downstream dataset dispatch through `Send`.
- Added reducer-backed dataset result collection.
- Added study-level checkpoint smoke coverage with `InMemorySaver`.
- Added blocked-downstream behavior when ADSL fails.
- Added automatic ADSL foundation insertion for downstream-only requests.
- Added max repair attempt routing guard for code/spec repair stubs.
- Added study-level audit manifest stub represented as an `ArtifactRef`.

Files changed:

- `docs/phase3_design.md`
- `docs/phase3_review_temp.html`
- `docs/langgraph_execution_roadmap.md`
- `docs/state_model.md`
- `pyproject.toml`
- `src/adam_agent/graph/state.py`
- `src/adam_agent/graph/routing.py`
- `src/adam_agent/graph/dataset_graph.py`
- `src/adam_agent/graph/study_graph.py`
- `tests/test_graph_smoke.py`

Commands run:

- `python -m pip install -e .`
- `python -m unittest discover -s tests -p "test_*.py"`
- `python -m unittest tests.test_graph_smoke -v`

Verification result:

- LangGraph installed version observed through pip resolution: `0.6.11`.
- Full test suite result after graph implementation hardening: `Ran 20 tests ... OK`.
- Graph smoke test result after hardening: `Ran 8 tests ... OK`.

Decisions made:

- Treat ADSL as the MVP foundation dataset.
- Dispatch downstream datasets only after ADSL succeeds.
- Automatically include ADSL when a downstream-only request is made.
- Mark downstream datasets `blocked_by_adsl` when ADSL fails.
- Keep dataset identity in state and audit artifacts, not in `checkpoint_ns`.
- Let the parent study graph own the checkpointer; Phase 3 verifies study-level
  checkpoint history, not nested subgraph checkpoint inheritance.
- Keep Phase 3 runtime state thin and separate from Phase 2 Pydantic schemas.

Open issues:

- Phase 3 still uses stub lineage/spec/code/sandbox logic.
- Production persistence is not implemented.
- Real LLM and R sandbox tool interfaces belong to Phase 4/5.

Recommended next action:

- Begin Phase 4 by adding deterministic tool interfaces around artifacts,
  study input reading, LLM calls, and R sandbox execution stubs.

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

`complete`

## Phase 4 Handoff Record - 2026-05-23

Date:

2026-05-23

Phase:

Phase 4 - Tool layer MVP

Status:

`complete`

What changed:

- Added a deterministic tool layer so future graph nodes do not directly handle
  scattered file I/O, model-provider calls, or R execution details.
- Added `ArtifactStore` for file hashing and canonical run manifest writing.
- Added `StudyInputScanner` for structured study-folder scanning.
- Added `SDTMReader` for lightweight CSV profiling and explicit sas7bdat
  not-implemented behavior.
- Added `ConfigLoader` for default and demo LLM exposure policies, reusing the
  existing `LLMExposureConfig` schema.
- Added `ModelRegistry` with Phase 4 mock-only support. Real providers such as
  OpenAI, Anthropic, and OpenAI-compatible endpoints intentionally raise
  `ModelNotImplementedError` for now.
- Added `MockLLMClient` that runs without API keys or network but still returns
  an auditable `LLMCallRecord`.
- Added `StubRRunner` with structured success/failure results and no real R
  subprocess execution.
- Added Phase 4 tests covering the boundaries above.
- Added `.tmp_tests/` to `.gitignore` because Windows/Python temporary
  directory permissions blocked `tempfile.TemporaryDirectory()` in this
  environment.

Files changed:

- `.gitignore`
- `docs/phase4_design.md`
- `docs/langgraph_execution_roadmap.md`
- `docs/phase4_review_temp.html`
- `src/adam_agent/tools/artifacts.py`
- `src/adam_agent/tools/study_inputs.py`
- `src/adam_agent/tools/sdtm_reader.py`
- `src/adam_agent/tools/config.py`
- `src/adam_agent/tools/r_runner.py`
- `src/adam_agent/llm/model_registry.py`
- `src/adam_agent/llm/clients.py`
- `tests/test_tools_phase4.py`

Commands run:

- `git status --short --branch`
- `python -m unittest discover -s tests -p "test_*.py"`

Verification result:

- Full test suite passes:
  `Ran 31 tests ... OK`.
- Test output still includes LangGraph/Python dependency deprecation warnings;
  they do not fail the suite.
- No API key, network call, or real R installation is required for Phase 4 tests.

Decisions made:

- Keep Phase 4 mock-only for provider calls. This avoids pretending that
  OpenAI/Anthropic models such as `gpt-5.5` are already wired into the product.
- Preserve the clean boundary for future real provider clients: they must
  implement the same `LLMClient.generate()` contract and return both response
  text and `LLMCallRecord`.
- Treat `external_api_allowed=true` in demo config as policy rehearsal only
  until a real provider client is implemented.
- Keep `.sas7bdat` as a recognized data format but return
  `not_implemented_yet` for profiling in Phase 4.
- Keep `.sas` under `legacy_code` as text evidence and flag `.sas` under
  `input_sdtm` as invalid data input.

Open issues:

- Phase 4 tools are not yet wired into the LangGraph nodes.
- Real R execution belongs to Phase 5.
- Real OpenAI/Anthropic/OpenAI-compatible clients are intentionally not
  implemented yet.
- `.sas7bdat` profiling needs an optional reader in a later phase.
- User reviewed and approved Phase 4.

Recommended next action:

- Commit Phase 4 as the tool-layer MVP.
- Then begin Phase 5 by wiring tools into a single-dataset ADSL loop.

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
- Review high-risk variables or record explicit demo-only bypass.
- Produce `adsl_approved_spec.json` with approval metadata.
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
