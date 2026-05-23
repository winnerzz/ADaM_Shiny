# CODEX.md

Project-level instructions for Codex when working in this repository.

These rules are adapted from the referenced Karpathy-style coding guidance:
think before coding, keep changes simple, edit surgically, and make every task
verifiable. This project adds stricter clinical-data and auditability constraints.

Source guidance:

- https://github.com/multica-ai/andrej-karpathy-skills/blob/main/CLAUDE.md

## 0. General Coding Guardrails

Use the referenced Karpathy-style guidance as one of the operating baselines for
this project. The goal is to reduce common LLM coding mistakes: assuming too
much, adding speculative complexity, touching unrelated files, and claiming
success without verification.

### 0.1 Think Before Coding

Before implementing:

- State material assumptions when they affect the design.
- If a request has multiple plausible meanings, surface the tradeoff instead of
  silently choosing one.
- Ask a concise question when missing context would make the implementation
  risky.
- Push back on complexity that does not serve the current milestone.

In this ADaM project, this especially applies to clinical rules. Do not silently
turn demo logic, starter variables, or reference-output patterns into production
derivation rules.

### 0.2 Simplicity First

Write the smallest implementation that satisfies the current phase goal.

- Do not add features beyond the current phase.
- Do not create abstractions for one-off code.
- Do not add configurability merely because it might be useful later.
- If a simpler deterministic tool can verify the architecture, use it before
  involving a real LLM provider.

For this project, prefer:

- deterministic templates before provider-dependent code generation
- focused validators before broad compliance claims
- explicit artifact manifests before complex persistence layers

### 0.3 Surgical Changes

Every changed line should trace back to the user's current request or the active
phase objective.

- Touch only the files needed for the task.
- Match the surrounding style.
- Do not refactor neighboring code just because it could be cleaner.
- Do not delete pre-existing dead code unless asked.
- Clean up only imports, variables, or helpers made obsolete by your own change.

If unrelated issues are found, mention them separately and keep moving on the
requested work.

### 0.4 Goal-Driven Execution

Turn implementation requests into verifiable goals.

For multi-step work, define:

```text
1. change to make
2. check that proves it
3. next integration point
```

Examples for this project:

- "Add a runner" means: add the runner, add a structured failure path, run a
  focused test.
- "Run ADSL" means: produce `adsl.csv`, write validation/audit artifacts, and
  verify the generated files exist.
- "Connect to Graph" means: preserve existing stub behavior, add a real-mode
  test, and run the full test suite.

Do not report success without a file, command, test, or explicit limitation.

## 1. Product Direction

This repository currently contains a Shiny ADaM Builder prototype and early
architecture material for a LangGraph-based ADaM agent product.

The target product is **ADaM Agent Studio**:

- Local-first clinical-data automation for converting SDTM inputs into ADaM outputs.
- A controlled workflow, not a one-shot prompt.
- LangGraph for orchestration, checkpointing, recovery, and human review.
- R as the execution/sandbox layer for derivation code.
- Python/FastAPI/LangGraph as the future orchestration layer.
- CDISC, P21, define.xml, company standards, and study artifacts as explicit
  reference assets, not hidden model memory.

Do not frame this project as "vibe coding ADaM". The correct framing is:

> LLMs draft lineage, specs, and code; deterministic tools check and execute;
> LangGraph controls the process; humans review high-risk decisions; the system
> keeps an audit trail.

## 2. Current Repository Reality

Before changing code, remember the current codebase is still a prototype.

Important existing files:

- `app.R`: Shiny launcher. Loads packages, sources modules, starts `shinyApp()`.
- `server.R`: Main Shiny orchestration layer. Contains much of the current runtime flow.
- `ui.R`: Shiny interface.
- `data_utils.R`: SDTM/spec loading, profiling, and sandbox helper functions.
- `derivation_plan_utils.R`: Converts spec-like inputs into derivation plans.
- `llm_api.R`: Current provider calls, prompt construction, JSON parsing, mock/repair logic.
- `code_static_checks.R`: Regex/rule-based pre-execution checks.
- `validation_utils.R`: Post-execution validation helpers.
- `provider_registry.R`: Current model/provider registry. Treat it as stale-prone.
- `domain_registry.R`: Domain-specific assumptions and registry logic.
- `tests/`: Existing R tests for pipeline and static checks.
- `docs/langgraph_adam_architecture.html`: Human-readable architecture proposal.

The current app contains demo-specific assumptions around ADSL/ADAE, DM/EX/AE,
`TRTSDT`, `TRTEDT`, `TRTEMFL`, `RELGR1`, mock paths, and hard-coded prompt shapes.
Separate reusable framework logic from demo scaffolding.

## 3. Architecture Principles

Use these principles for all new architecture work.

### 3.1 Two-Level Graph

Do not build one global workflow state for all datasets.

Use:

- `StudyGraph`: study-level orchestration, input scan, dependency graph,
  dataset dispatch, result reduction, final audit packaging.
- `DatasetGraph`: one isolated subgraph per target ADaM dataset.

Each dataset must own its own:

- status
- repair attempts
- generated code
- validation result
- compare result
- human decisions
- risk/confidence routing

ADAE failing must not mutate ADCM's retry count or state.

### 3.2 Failure Routing

Do not assume every failure is a code bug.

The graph must be able to route failures to:

- `repair_code`
- `revise_spec`
- `revise_lineage`
- `request_reference`
- `human_escalation`

If sandbox execution fails because the spec or lineage is wrong, the system must
route backward to spec/lineage review instead of repeatedly patching bad code.

### 3.3 Reference Access

Do not stuff all CDISC/P21 content into a prompt.

Represent references as tools or explicit assets:

- ADaM IG
- SDTM IG
- Controlled Terminology
- Define-XML metadata
- TAUG documents
- P21 rules
- company standards
- prior study code and validated ADaM outputs

LLM nodes should be able to query references when drafting lineage, drafting
specs, generating code, diagnosing failures, and repairing code.

### 3.4 Human Review

Do not create approval fatigue.

Use risk-driven review:

- silent pass for high-confidence, low-risk direct mappings
- human review for ambiguous sources, critical variables, rule conflicts,
  low confidence, or repeated repair failure
- always record silent decisions in the audit trail

### 3.5 State and Artifacts

Use mixed state storage.

Keep small structured objects in LangGraph state:

- lineage JSON
- draft/approved spec JSON
- generated code strings
- risk scores
- routing decisions
- summarized validation reports
- human decisions

Keep large or regulated files as artifact paths plus hashes:

- SDTM matrices
- reference ADaM datasets
- generated XPT/CSV/SAS7BDAT files
- large compare outputs
- CDISC PDFs
- final audit reports

## 4. First Implementation Target

The first LangGraph implementation should prove the architecture, not full ADaM
correctness.

Minimum useful milestone:

1. Define `StudyState` and `DatasetState`.
2. Build a minimal `StudyGraph`.
3. Build a minimal `DatasetGraph`.
4. Dispatch at least ADSL and ADAE dataset stubs.
5. Add one confidence/risk-driven human review route.
6. Add checkpointing.
7. Produce an artifact manifest.
8. Run a small local test study through the stubbed flow.

Success means:

- the graph runs
- state is isolated per dataset
- checkpoint/recovery works
- routing decisions are visible
- artifacts are recorded
- failures can be explained

Generating production-quality ADaM data is a later milestone.

## 5. LLM Layer Rules

For the first build, keep the LLM layer simple but isolated.

Acceptable now:

- OpenAI-compatible chat/completions-style calls
- mock mode
- provider registry
- JSON extraction/validation wrappers

Required boundary:

- no direct provider calls from graph nodes
- graph nodes call an internal LLM interface
- prompts live outside orchestration code
- structured outputs are validated before entering state

Future upgrades must be possible without rewriting graph logic:

- OpenAI Responses API
- Anthropic
- local models
- tool calling
- schema-constrained outputs
- tracing and evaluation

Suggested interface shape:

```text
LLMRequest
  provider
  model
  system_prompt
  user_prompt
  tools
  response_schema
  temperature

LLMResult
  content
  parsed_json
  tool_calls
  usage
  raw_response
  warnings
```

## 6. R Sandbox Rules

Generated derivation code must not execute in the main app process.

The R runner/sandbox should:

- execute in an isolated working directory
- receive explicit input paths and output paths
- block dangerous file/network/system operations where possible
- capture stdout, stderr, warnings, errors, and session info
- return structured execution results
- write outputs into the artifact store

R code generation should prefer readable, auditable code over clever code.
Every generated script should be reproducible from its spec, inputs, and run
manifest.

## 7. Clinical Data Rules

Clinical data work needs stronger standards than ordinary app code.

Do:

- preserve provenance for every derived variable
- distinguish source SDTM domains from target ADaM datasets
- keep dataset-level and variable-level assumptions explicit
- hash important input/output artifacts
- keep audit logs for model outputs, tool outputs, human decisions, and repairs
- compare against reference ADaM when available
- surface uncertainty instead of hiding it

Do not:

- silently invent clinical rules
- overwrite user data
- treat SAP alone as sufficient variable-level specification
- assume metadata-only generation is enough
- rely on model memory for CDISC rules
- call ambiguous output "validated" without deterministic checks

## 8. Coding Discipline

Follow these rules for every code change.

Think before coding:

- State assumptions when they matter.
- If the requirement has multiple plausible meanings, ask or present the tradeoff.
- Push back on unnecessary complexity.

Simplicity first:

- Write the minimum code that satisfies the current task.
- Do not add speculative abstractions.
- Do not build future flexibility unless it protects a known boundary.

Surgical edits:

- Touch only files needed for the task.
- Match the existing style.
- Do not refactor unrelated code.
- Do not delete existing dead code unless the user asks.
- If you notice unrelated problems, mention them separately.

Verification:

- Every meaningful change needs a way to check it.
- Prefer focused tests over broad rewrites.
- If tests cannot run, say exactly why.
- Do not claim success without a command, test, or file-level verification.

## 9. Git and Workspace Safety

The user may have uncommitted work.

Before edits:

- check `git status --short --branch`
- identify untracked or modified files
- do not reset, checkout, or delete user changes without explicit permission

When switching branches:

- preserve dirty work first
- prefer a separate worktree for the empty `LangGraph` branch if the current
  worktree contains user files

Known context:

- A local empty branch named `LangGraph` may exist.
- It may not be pushed to GitHub if network access failed.
- The current Shiny prototype branch may still contain uncommitted local data
  and documentation.

## 10. Suggested Future Directory Shape

For the LangGraph build, prefer this shape unless the project chooses otherwise:

```text
docs/
  architecture.html
  agent_graph.md
  module_contracts.md
  cdisc_reference_strategy.md

references/
  CDISC/
    ADaM_IG/
    SDTM_IG/
    Controlled_Terminology/
    Define_XML/
    TAUG/
    P21_Rules/
  company_standards/
  reference_manifest.yaml

studies/
  _template/
    input_sdtm/
    input_define/
    input_spec/
    reference_adam/
    outputs/
    audit/

src/adam_agent/
  graph/
    study_graph.py
    dataset_graph.py
    routing.py
    checkpoints.py
  schemas/
    study_state.py
    dataset_state.py
    lineage.py
    adam_spec.py
    validation.py
    audit.py
  nodes/
    study/
    dataset/
  tools/
    sdtm_reader.py
    define_reader.py
    cdisc_reference_tools.py
    p21_tools.py
    r_runner.py
    dataset_compare.py
  llm/
    model_registry.py
    clients.py
    prompts.py
    structured_outputs.py
```

## 11. Communication Style for This Project

The user often wants beginner-friendly but rigorous explanations in Chinese.

When explaining:

- use simple terms first
- explain what a file does and why it exists
- distinguish "core necessary logic" from "demo/legacy scaffolding"
- avoid defending complexity just because it exists
- give a concrete next file or next engineering step

When reviewing architecture:

- be direct about risks
- identify where a design will fail in real clinical-data work
- propose a smaller verifiable next step
- keep auditability and rollback in view

## 12. Non-Negotiables

- Do not turn the product back into one giant prompt.
- Do not share mutable global state across datasets.
- Do not repair code forever when the root cause is spec or lineage.
- Do not hide model assumptions.
- Do not use CDISC/P21 rules from memory when a reference source is available.
- Do not overwrite user data or uncommitted work.
- Do not claim regulatory-grade validation from mock checks.
- Do not add complex infrastructure before proving the graph skeleton.
