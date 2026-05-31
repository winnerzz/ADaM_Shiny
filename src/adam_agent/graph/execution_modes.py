"""Execution-mode contracts for graph entry points."""

from __future__ import annotations


LEGACY_STUB_MODE = "stub"
LLM_DOWNSTREAM_PROVIDER_MODE = "llm_downstream_provider"
LLM_DOWNSTREAM_R_SANDBOX_MODE = "llm_downstream_r_sandbox"
LLM_DOWNSTREAM_STUBBED_MODE = "llm_downstream_stubbed"
GRAPH_PRODUCT_PREPARE_MODE = "graph_product_prepare"
GRAPH_PRODUCT_GENERATE_CODE_MODE = "graph_product_generate_code"
GRAPH_PRODUCT_EXECUTE_MODE = "graph_product_execute"
RETIRED_ADSL_TEMPLATE_MODE = "real_adsl_minimal"

LLM_DOWNSTREAM_MODES = frozenset(
    {
        LLM_DOWNSTREAM_STUBBED_MODE,
        LLM_DOWNSTREAM_PROVIDER_MODE,
        LLM_DOWNSTREAM_R_SANDBOX_MODE,
    }
)
GRAPH_PRODUCT_MODES = frozenset(
    {
        GRAPH_PRODUCT_PREPARE_MODE,
        GRAPH_PRODUCT_GENERATE_CODE_MODE,
        GRAPH_PRODUCT_EXECUTE_MODE,
    }
)
LEGACY_RUN_BLOCKED_LLM_MODES = frozenset(
    {
        LLM_DOWNSTREAM_PROVIDER_MODE,
        LLM_DOWNSTREAM_R_SANDBOX_MODE,
    }
)
LEGACY_RUN_ENDPOINT_MODES = frozenset({LEGACY_STUB_MODE, *LEGACY_RUN_BLOCKED_LLM_MODES})
CLI_RUN_STUDY_MODES = frozenset({*LEGACY_RUN_ENDPOINT_MODES, LLM_DOWNSTREAM_STUBBED_MODE})


def format_execution_modes(modes: frozenset[str]) -> str:
    """Return a stable human-readable mode list for errors and docs."""

    return ", ".join(sorted(modes))
