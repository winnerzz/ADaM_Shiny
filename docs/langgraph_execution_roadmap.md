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
| Current phase | Phase 7 - Multi-dataset study orchestration |
| Original Shiny worktree branch | `experimental-v3` |
| Active worktree branch | `LangGraph` |
| Active LangGraph worktree | `D:\Archive\Research\Projects\ADaM_Shiny_LangGraph` |
| Target architecture branch | `LangGraph` |
| Product direction | Local-first ADaM Agent Studio using LangGraph orchestration and R sandbox execution |
| Current implementation status | Phase 7.4 design started; general dependency resolution and LLM-driven downstream generation planned |
| Last roadmap update | 2026-05-24 |

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
| 5. Single-dataset real loop | Run one real ADaM dataset end to end, likely ADSL first | lineage/spec/code/run/validate path for ADSL | One dataset can run from inputs to validated output | in progress |
| 6. Failure diagnosis and rollback | Add controlled repair and backward routing | `diagnose_failure`, `repair_code`, `revise_spec`, `revise_lineage` | Failures are classified instead of blindly repairing code | in progress |
| 7. Multi-dataset study orchestration | Coordinate multiple ADaM datasets with dependencies | dependency graph, dataset dispatch/reduce logic | ADSL can complete before dependent datasets run | in progress |
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
- Added `ModelRegistry` with Phase 4 mock-only support. In later Phase 7.4 work,
  real provider metadata was added for OpenAI-compatible providers and
  Anthropic, while preserving the rule that real providers must never fall back
  to mock.
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
- Real provider clients were intentionally not implemented in Phase 4. Later
  Phase 7.4 work added OpenAI-compatible and Anthropic client boundaries while
  keeping graph defaults on mock/stub mode.
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

`in progress`

Phase 5 design notes:

- Added `docs/phase5_design.md`.
- Confirmed that Phase 5 starter variables are MVP candidates for a runnable
  ADSL loop, not production default ADaM rules.
- Clarified that treatment dates and population flags such as `TRTSDT`,
  `TRTEDT`, and `SAFFL` must be review-required unless supported by stronger
  study evidence or explicit human/demo approval.
- Started with CSV execution, then added `.sas7bdat` support on the R runtime
  path through `haven::read_sas()`. Python still does not profile `.sas7bdat`
  contents in Phase 5.
- Recommended deterministic R generation before real LLM code generation.

Phase 5 implementation notes:

- Added ADSL-specific helper package under `src/adam_agent/adsl/`.
- Added starter ADSL spec builder that creates `SpecDocument` and
  `EvidenceRecord` objects from DM/EX profiles.
- Added demo-only approval helper that records
  `demo_only_no_review` and explicitly says starter rules are not
  production-approved.
- Added deterministic base-R template renderer for `build_adsl.R`.
- Updated the generated CSV reader to keep all columns as character so
  identifiers such as `USUBJID = 01` are not silently converted to `1`.
- Added minimal ADSL CSV validator for required columns, `USUBJID` uniqueness,
  treatment date order, and `SAFFL` value shape.
- Added `LocalRRunner` behind the existing R runner boundary. It reports a
  structured failure when `Rscript` is unavailable instead of pretending success.
- Current environment check: `Rscript` is available at
  `C:\Dev\R-4.5.2\bin\Rscript.exe`.
- Added a real local R smoke test using that `Rscript.exe`: synthetic DM/EX are
  rendered into `build_adsl.R`, executed locally, and the generated `adsl.csv`
  passes the minimal validator.
- Added a `.sas7bdat` smoke test that creates synthetic DM/EX with R
  `haven::write_sas()`, then runs the same Phase 5 ADSL loop. This verifies the
  current product direction: R reads `.sas7bdat`; Python only orchestrates and
  records artifacts.
- Tightened `.sas7bdat` profiling after sub-agent review: Phase 5 now runs a
  short R/haven profile script before spec drafting so missing date columns do
  not create fake `TRTSDT` or `TRTEDT` evidence.
- Added `run_adsl_minimal()` service in `src/adam_agent/adsl/runner.py`.
  It scans a study folder, profiles DM/EX, drafts and demo-approves the starter
  spec, writes `build_adsl.R`, executes local R, validates `adsl.csv`, writes a
  skipped compare report, and writes the run manifest.
- Added an end-to-end service test that verifies the run creates:
  draft spec, approved spec, approval record, generated R code, `adsl.csv`,
  validation report, compare report, and audit manifest.
- Wired the Phase 5 ADSL minimal service into `DatasetGraph` behind the explicit
  execution mode `real_adsl_minimal`.
- `StudyGraph` now passes `execution_mode`, `study_dir`, and `rscript_path` into
  dataset tasks while preserving the default stub behavior.
- Added a graph-level real ADSL smoke test: `StudyGraph` can run ADSL as the
  foundation dataset through the real Phase 5 service and returns a completed
  `DatasetResultSummary` with `validation_status = pass`.
- Added a small local CLI entry point:
  `python -m adam_agent.cli run-adsl-minimal --study-dir ... --run-id ... --rscript-path ...`.
  The CLI returns a JSON summary with run status, validation status, run folder,
  and manifest path.
- Added structured failure handling for real ADSL graph/CLI paths, so expected
  missing-input failures return `status = failed` instead of raising through the
  caller.
- Full test suite after Phase 5 starter implementation:
  `Ran 46 tests ... OK`.

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

`in progress`

Phase 6 design notes:

- Added `docs/phase6_design.md`.
- Scoped Phase 6 MVP to failure diagnosis and recommended routing, not real
  automatic repair.
- Defined the first failure report artifact:
  `runs/{run_id}/diagnostics/adsl_failure_report.json`.
- Kept the failure report as `kind = tool_log`, `role = audit` to avoid schema
  churn before a dedicated failure-report artifact kind is needed.

Phase 6 implementation notes:

- Added `src/adam_agent/adsl/diagnostics.py`.
- Implemented `diagnose_adsl_failure()` to classify:
  - missing required input
  - unsupported or unreadable `.sas7bdat` profile path
  - source-variable/spec-style R errors
  - R runtime failures
  - validation failures
  - key integrity failures requiring human review
- Implemented `write_failure_report()`.
- Failure ids are root-cause-specific, for example
  `failure_adsl_missing_required_input`, so later retry/repair history can
  distinguish different failure causes.
- Wired diagnosis into `run_adsl_minimal()`:
  - missing DM/EX now returns a structured failed result and writes
    `adsl_failure_report.json`
  - profile failures now write a failure report
  - R or validation failures now write a failure report and register it in the
    manifest
- Added `failure_record` to `AdslRunResult`.
- Updated CLI JSON output to include `failure_id`, `failure_type`, and
  `recommended_route`.
- Updated real ADSL graph state to carry `failure_records` and
  `recommended_route`.
- Updated real ADSL graph summary to use the diagnosis `failure_id`.
- Ensured failed real ADSL runs summarize directly after diagnosis instead of
  passing through stub `repair_code` or `revise_spec` nodes.
- Manifest and CLI output now include `failure_id`, `failure_type`,
  `root_cause`, and `recommended_route`.
- Updated `docs/output_contract.md` to include `diagnostics/`.

Verification:

```text
python -m unittest discover -s tests -p "test_*.py"
Ran 54 tests ... OK
```

Open issues:

- Phase 6 still does not implement real `repair_code` or `revise_spec` nodes
  for the real ADSL path.
- Recommended routes are recorded and surfaced, but the real path does not yet
  loop backward based on them.
- Human review is represented as a route, not a UI checkpoint.

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

`in progress`

Phase 7.1 design notes:

- Added `docs/phase7_design.md`.
- Scoped Phase 7.1 to dependency orchestration, not real ADAE/ADCM generation.
- Recorded that `ADSL` is an MVP foundation default, not a production clinical
  derivation rule.
- Recorded future evidence priority for dependency decisions:
  approved spec, legacy SAS, define.xml, study notes, MVP fallback, human
  decision.

Phase 7.1 implementation notes:

- Added `src/adam_agent/graph/dependencies.py`.
- Added explicit `DatasetDependencyPlan`.
- Added MVP fallback evidence marker:
  `phase7_mvp_fallback_adsl_foundation`.
- Added per-dataset dependency decisions with source, confidence,
  review-required flag, and reason.
- Common downstream fallback dependencies such as `ADAE -> ADSL` are still
  marked review-required.
- Unknown `AD*` datasets are low-confidence fallback dependencies and require
  review.
- Non-AD targets such as `LB` are marked unsupported instead of being sent
  through the dataset stub and shown as completed.
- StudyGraph now records:
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
- Study-level audit stub metadata now records requested datasets, auto-added
  datasets, dependency map, and dependency evidence.
- Downstream-only requests such as `["ADAE"]` auto-add `ADSL`.
- Multiple downstream requests such as `["ADAE", "ADCM"]` auto-add `ADSL` only
  once.

Phase 7.2 implementation notes:

- Added conservative dependency evidence scanning from:
  - `input_spec`
  - `legacy_code`
  - `input_define`
- Dependency planning is spec-first:
  - if `input_spec` is present, it is the authoritative dependency source
  - legacy SAS and define.xml are scanned only to validate the spec for
    conflicts
  - consistent secondary evidence stays quiet
  - conflicting secondary evidence is recorded as a planning warning
  - if `input_spec` is missing, legacy SAS and define.xml can provide draft
    dependency evidence before MVP fallback
- StudyGraph now builds execution batches from the dependency plan, for
  example:
  `ADSL -> ADLB -> ADTTE`.
- Datasets in the same dependency batch can run together through a bounded
  thread pool.
- If an intermediate dataset fails, only datasets that depend on it are
  blocked. Unrelated datasets in the same study can still complete.
- Study-level audit metadata now records dependency evidence records,
  dependency planning warnings, and execution batches.

Phase 7.3 implementation notes:

- Added a file-backed dependency planning review contract.
- StudyGraph now writes the following files when `study_dir` is present:
  - `runs/{run_id}/planning/dependency_plan.json`
  - `runs/{run_id}/planning/dependency_review.md`
- `dependency_plan.json` records requested datasets, final target datasets,
  auto-added datasets, unsupported datasets, dataset dependencies, dependency
  decisions, evidence records, planning warnings, execution batches, blocked
  datasets, and `review_status`.
- `dependency_review.md` gives the same information in a human-readable form
  for quick review.
- Added review status levels:
  - `accepted`
  - `review_required`
  - `warning`
  - `blocked`
- Tightened the user-provided spec rule:
  - if `input_spec` exists, it drives the dependency plan
  - consistent secondary SAS/define evidence stays quiet
  - conflicting secondary evidence becomes a warning
  - `input_spec_dependency` by itself is not marked `review_required` because
    it represents the user-provided spec in the current contract
- Added an `input_spec` coverage-gap warning:
  - if the spec folder exists but no dependency evidence is extracted for a
    requested or auto-added target dataset, the plan records that it is falling
    back to MVP ordering for that dataset
- StudyGraph now writes the study-level audit manifest when `study_dir` is
  present:
  - `runs/{run_id}/audit/manifest.json`
  - the manifest links the dependency plan/review artifacts and dataset-level
    audit artifacts
- Real ADSL runs invoked from StudyGraph write their dataset manifest as
  `runs/{run_id}/audit/adsl_manifest.json` so they do not overwrite the
  study-level `audit/manifest.json`. Direct CLI/service ADSL runs still write
  the canonical `audit/manifest.json`.
- StudyGraph state and audit metadata now carry dependency plan/review artifact
  references.
- Updated `CODEX.md` so the repository guidance describes the current
  LangGraph worktree instead of the older Shiny prototype file layout.

Verification so far:

```text
python -m unittest discover -s tests -p "test_*.py"
Ran 69 tests ... OK
```

Phase 7.4 design notes:

- Added `docs/phase7_4_design.md`.
- Phase 7.4 is not a UI phase. UI remains Phase 8.
- Phase 7.4 should implement general dependency resolution and LLM-driven
  target generation.
- The key product rule is now:
  - dependency discovery can be automatic
  - dependency availability must be checked
  - missing dependency execution must not be automatic unless the user or run
    configuration explicitly approves it
- This rule must be generic. Do not hard-code `ADAE -> ADSL` behavior.
- ADAE can be the first test fixture, but code should work for any dependency
  relationship discovered by the dependency planner.
- The current `auto_added_datasets` field should be treated carefully in UI and
  review language. It means "required by dependency planning", not "approved for
  automatic execution".

Phase 7.4 implementation notes:

- Added generic dependency availability and decision records.
- StudyGraph now checks dependency availability for requested targets before
  executing them.
- Available dependency artifacts can satisfy requirements without running the
  dependency dataset in the current run.
- First MVP availability sources:
  - `reference_adam/{dataset}.csv`
  - `reference_adam/{dataset}.sas7bdat`
  - `runs/{run_id}/outputs/{dataset}.csv`
  - `runs/{run_id}/outputs/{dataset}.sas7bdat`
- Availability now separates a file that is merely found from a file that is
  usable by the current dependency gate. CSV dependencies must be minimally
  readable; `.sas7bdat` dependency artifacts are recorded as
  `found_but_unusable` in Phase 7.4 because Python-side dependency profiling is
  not implemented.
- Missing dependency datasets now create a structured block:
  `dependency_user_action_required`.
- StudyGraph no longer silently runs discovered dependencies. It executes only:
  - user-requested datasets
  - dependency datasets explicitly listed in `approved_dependency_datasets`
- StudyGraph also checks dependency-chain readiness before execution. If a user
  approves a middle dependency but its own required parent is still missing, the
  middle dependency and final requested target both remain blocked with
  `dependency_user_action_required`.
- Existing tests were updated to reflect the new dependency-resolution
  semantics.
- Full test suite after this step:
  `Ran 71 tests ... OK`.
- Added `src/adam_agent/llm/context.py` as the first LLM-generation preparation
  component.
- The context builder creates an auditable target package without calling a real
  provider:
  - target input spec payload
  - source SDTM profiles
  - resolved dependency profiles
  - runtime contract for generated R code/output paths
  - exposure-policy summary
  - warnings for missing/unreadable artifacts
- Added `tests/test_llm_context.py`.
- Full test suite after context package builder:
  `Ran 75 tests ... OK`.
- Added `src/adam_agent/llm/generated_code.py`.
- The generated-code parser enforces the first strict LLM JSON output contract:
  `dataset`, `r_code`, `assumptions`, `risk_points`, `used_inputs`, and
  `expected_outputs`.
- The writer records the raw response, parsed response, and generated R script
  under the run directory.
- Added a small pre-sandbox guardrail for obvious unsafe generated R calls such
  as `system()`, `shell()`, `unlink()`, `download.file()`, and
  `install.packages()`.
- Added `tests/test_llm_generated_code.py`.
- Full test suite after generated-code parser/writer:
  `Ran 79 tests ... OK`.
- Added `src/adam_agent/downstream/runner.py`.
- The generic downstream runner now wires the Phase 7.4 pieces into one
  testable service boundary:
  `context package -> LLM client -> generated-code parser/writer -> R runner ->
  structural output validation`.
- The runner is dataset-generic and uses ADAE only as the first fixture.
- It can run with a fixed mock LLM response and stub R runner, so no API key or
  local R installation is required for these tests.
- The default stub runner now reports `structural_stub_pass`,
  `stubbed_r_execution = true`, and `not_real_derivation = true`, rather than a
  plain validation `pass`.
- Added `tests/test_downstream_runner.py`.
- Full test suite after generic downstream runner:
  `Ran 83 tests ... OK`.
- Wired the generic downstream runner into `DatasetGraph` behind
  `execution_mode = "llm_downstream_stubbed"`.
- `StudyGraph` now passes dependency-resolution records into dataset tasks so a
  downstream dataset can build its dependency-aware LLM context package.
- Added a graph-level smoke test showing ADAE can run through the explicit
  LLM-downstream stubbed mode when ADSL is available as a dependency artifact.
- Full test suite after StudyGraph/DatasetGraph integration:
  `Ran 84 tests ... OK`.
- Sub-agent review then identified two boundary risks:
  - `.sas7bdat` dependency artifacts were too easy to misread as available
  - stubbed downstream execution could be mistaken for real generation
- Fixed both boundaries:
  - dependency records now support `found_but_unusable`
  - dataset summaries now use `status = completed_stub` and
    `validation_status = structural_stub_pass` for stubbed downstream runs
  - dependency blocks now use the generic `blocked_by_dependency` reason
    instead of the older `blocked_by_adsl` wording
- Full test suite after boundary fixes:
  `Ran 88 tests ... OK`.
- Added `OpenAICompatibleLLMClient` behind the existing `LLMClient.generate()`
  interface.
- The OpenAI-compatible client uses a configurable base URL, API key, and model
  string. It does not hard-code a small model allowlist, so newer model IDs can
  be supplied by run configuration.
- External provider calls require explicit `external_api_allowed=True` and a
  configured API key. There is still no fallback from a real provider to mock.
- Tests use a fake transport and do not call the network.
- StudyGraph/DatasetGraph still default to mock/stub mode; real provider use
  needs an explicit later wiring step.
- Full test suite after OpenAI-compatible client boundary:
  `Ran 91 tests ... OK`.
- Sub-agent review of provider expansion found no P0 blocker, but required
  stronger P1 audit/safety controls around arbitrary base URLs, relay risk,
  provider identity, and fail-closed behavior.
- Added `LLMProviderConfig` and `build_llm_client()` so provider selection is a
  configuration/factory concern, not graph business logic.
- Added provider aliases:
  - `deepseek` and `qwen` route through the OpenAI-compatible transport
  - `anthropic` and `claude` route through a separate Anthropic Messages client
- Added `AnthropicMessagesLLMClient` for official Claude Messages API shape:
  `POST /v1/messages`, `x-api-key`, `anthropic-version`, optional `system`,
  and response text extraction across all text content blocks.
- Added audit fields to `LLMCallRecord`:
  `provider_alias`, `transport`, `provider_base_url`,
  `subject_level_data_included`, `external_relay`, and `risk_flags`.
- Added fail-closed checks:
  - external providers require explicit external API approval
  - providers requiring API keys fail when the key is absent
  - custom base URLs require explicit approval
  - response payloads without usable text fail instead of falling back to mock
- A generic `custom-http` relay client remains out of scope. Nonstandard relay
  formats can be added later, but standard relay services should use
  OpenAI-compatible mode first.
- Full test suite after provider factory and Anthropic client:
  `Ran 96 tests ... OK`.

Open issues:

- The fallback is not a complete ADaM dependency graph and must not be treated
  as production evidence.
- Downstream datasets still use stubs unless a later phase implements real
  dataset loops.
- A downstream `structural_stub_pass` is only a smoke-test success for
  orchestration and artifacts, not a real ADaM derivation.
- Dependency extraction is conservative and lightweight; production-grade
  define.xml/spec parsing still belongs in a later standards-hardening phase.
- Phase 7.4 has not yet implemented real LLM provider calls or real downstream
  R execution for a non-ADSL target.
- Downstream validation is still structural and must not be described as
  regulatory-grade ADaM compliance validation.

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
