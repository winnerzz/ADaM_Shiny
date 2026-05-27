# Tests

Initial tests should prove architecture behavior before ADaM correctness:

- graph smoke tests
- state isolation tests
- checkpoint tests
- artifact manifest tests
- mock LLM client tests

Real ADaM derivation tests should be added after the graph skeleton and tool
interfaces are stable.

## Optional Live LLM Smoke Test

`test_live_llm_smoke.py` is skipped by default. It is only for manual provider
connectivity checks and requires private environment variables.

Example for an OpenAI-compatible relay:

```powershell
$env:ADAM_AGENT_RUN_LIVE_LLM = "1"
$env:ADAM_AGENT_LIVE_LLM_PROVIDER = "openai-compatible"
$env:ADAM_AGENT_LIVE_LLM_MODEL = "gpt-5.5"
$env:ADAM_AGENT_LIVE_LLM_BASE_URL = "https://your-relay.example/v1"
$env:ADAM_AGENT_LIVE_LLM_API_KEY_ENV = "ADAM_AGENT_LIVE_LLM_API_KEY"
$env:ADAM_AGENT_LIVE_LLM_API_KEY = "<private key>"
python -m unittest tests.test_live_llm_smoke
```

Do not commit API keys or private relay configs.

## Optional Live UI/API Workflow Smoke Test

`test_live_ui_smoke.py` is also skipped by default. It validates the Phase 8 UI
backend workflow against a real provider:

- prepare demo study files
- prepare the dependency plan
- call `/llm/test-connection`
- call `/runs/{run_id}/datasets/ADAE/generate-code`
- optionally approve and execute generated R locally

Example:

```powershell
$env:ADAM_AGENT_RUN_LIVE_UI_SMOKE = "1"
$env:ADAM_AGENT_LIVE_LLM_PROVIDER = "openai-compatible"
$env:ADAM_AGENT_LIVE_LLM_MODEL = "gpt-5.5"
$env:ADAM_AGENT_LIVE_LLM_BASE_URL = "https://your-relay.example/v1"
$env:ADAM_AGENT_LIVE_LLM_API_KEY_ENV = "ADAM_AGENT_LIVE_LLM_API_KEY"
$env:ADAM_AGENT_LIVE_LLM_API_KEY = "<private key>"
python -m unittest tests.test_live_ui_smoke
```

Add these only if you want the smoke test to run generated R locally:

```powershell
$env:ADAM_AGENT_LIVE_UI_EXECUTE_R = "1"
$env:ADAM_AGENT_LIVE_UI_RSCRIPT_PATH = "C:\Dev\R-4.5.2\bin\Rscript.exe"
```
