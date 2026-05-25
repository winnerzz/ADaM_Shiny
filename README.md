# ADaM Agent Studio

This branch contains the LangGraph-based ADaM Agent Studio prototype.

The old Shiny prototype is not copied here. This branch was started from an
empty tree so the new architecture can be built without carrying prototype
coupling forward.

Primary documents:

- `CODEX.md`: project-level instructions for Codex and other AI coding tools
- `docs/langgraph_adam_architecture.html`: readable architecture explanation
- `docs/langgraph_execution_roadmap.md`: staged execution and handoff ledger

Initial direction:

- local-first product
- LangGraph orchestration
- per-dataset isolated state
- deterministic tools around LLM calls
- R sandbox execution
- audit-first clinical-data workflow

## Current Prototype Boundary

Current working capabilities:

- study-level orchestration through `StudyGraph`
- per-dataset execution through isolated `DatasetGraph` state
- dependency planning and dependency availability checks
- deterministic minimal ADSL generation through local R
- downstream LLM-generated R code path for non-ADSL ADaM targets
- configurable LLM provider boundary, including OpenAI-compatible providers,
  DeepSeek/Qwen aliases, Anthropic/Claude, and mock mode
- optional local Rscript execution for downstream generated code
- structured validation, audit artifacts, and failure diagnosis
- one bounded downstream repair attempt for code/contract/runtime failures

Important limitations:

- This is not regulatory-grade ADaM validation.
- ADSL has a minimal deterministic implementation.
- Downstream datasets such as ADAE/ADCM/ADLB depend on the provided spec,
  source data, LLM output, and R sandbox execution.
- `structural_stub_pass` only proves orchestration and artifact wiring.
- Real clinical derivation quality still depends on human-reviewed specs,
  standards references, and validation beyond this prototype.

## Quick Start

Run the test suite:

```powershell
python -m unittest discover -s tests -p "test_*.py"
```

Run a study-level mock downstream smoke test:

```powershell
python -m adam_agent.cli run-study `
  --study-dir "D:\path\to\PSY201" `
  --run-id run_demo_mock `
  --target ADAE `
  --config studies\_template\configs\mock_downstream.json `
  --execution-mode llm_downstream_provider
```

Run downstream LLM-generated R code through local Rscript:

```powershell
python -m adam_agent.cli run-study `
  --study-dir "D:\path\to\PSY201" `
  --run-id run_demo_r `
  --target ADAE `
  --config studies\_template\configs\mock_downstream.json `
  --execution-mode llm_downstream_r_sandbox `
  --rscript-path "C:\Dev\R-4.5.2\bin\Rscript.exe"
```

Expected study input shape:

```text
PSY201/
  input_sdtm/
    ae.csv
  input_spec/
    adae.json
  reference_adam/
    adsl.csv
  runs/
```

Typical run outputs:

```text
runs/{run_id}/
  planning/dependency_plan.json
  planning/dependency_review.md
  llm/{dataset}_context.json
  llm/{dataset}_response.json
  code/build_{dataset}.R
  outputs/{dataset}.csv
  validation/{dataset}_validation_report.json
  diagnostics/{dataset}_failure_report.json
  audit/manifest.json
```

`diagnostics/` appears when a run fails or when an initial failure was repaired.

## Data and API Safety

Mock mode requires no API key and no network call.

External provider mode must be explicitly configured. Do not put real API keys
in repository files. Use environment variables such as `OPENAI_API_KEY`,
`DEEPSEEK_API_KEY`, `DASHSCOPE_API_KEY`, or `ANTHROPIC_API_KEY`.

For a custom OpenAI-compatible relay, set a custom `base_url` in a private local
config and approve it explicitly with:

```json
{
  "llm_provider": {
    "provider": "openai-compatible",
    "model": "gpt-5.5",
    "base_url": "https://example-relay/v1",
    "api_key_env": "YOUR_PRIVATE_ENV_VAR",
    "allow_custom_base_url": true,
    "custom_base_url_approved_by": "local_user"
  }
}
```

Do not commit that private config if it contains a real endpoint, approval
identity, or study-specific data policy.
