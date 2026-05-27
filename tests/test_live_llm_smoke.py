"""Opt-in live LLM smoke tests.

These tests are skipped by default because they call external APIs and require
private credentials. Enable them explicitly when validating a real provider or
OpenAI-compatible relay.
"""

from __future__ import annotations

import os
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.llm.clients import LLMProviderConfig, build_llm_client
    from adam_agent.schemas.llm import LLMExposureConfig
    from adam_agent.downstream.runner import _llm_request
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.llm.clients import LLMProviderConfig, build_llm_client
    from adam_agent.schemas.llm import LLMExposureConfig
    from adam_agent.downstream.runner import _llm_request


def _live_llm_enabled() -> bool:
    return os.environ.get("ADAM_AGENT_RUN_LIVE_LLM", "").strip() == "1"


@unittest.skipUnless(_live_llm_enabled(), "Set ADAM_AGENT_RUN_LIVE_LLM=1 to call a real LLM provider.")
class LiveLLMSmokeTests(unittest.TestCase):
    def test_openai_compatible_provider_returns_adam_code_contract(self) -> None:
        provider = os.environ.get("ADAM_AGENT_LIVE_LLM_PROVIDER", "openai-compatible")
        model = os.environ.get("ADAM_AGENT_LIVE_LLM_MODEL", "gpt-5.5")
        base_url = os.environ.get("ADAM_AGENT_LIVE_LLM_BASE_URL")
        api_key_env = os.environ.get("ADAM_AGENT_LIVE_LLM_API_KEY_ENV", "ADAM_AGENT_LIVE_LLM_API_KEY")
        approved_by = os.environ.get("ADAM_AGENT_LIVE_LLM_APPROVED_BY", "local_live_smoke")
        api_key = os.environ.get(api_key_env)
        if not api_key:
            self.skipTest(f"Set {api_key_env} before running the live LLM smoke test.")

        config = LLMProviderConfig(
            provider=provider,
            model=model,
            base_url=base_url,
            api_key_env=api_key_env,
            timeout_seconds=float(os.environ.get("ADAM_AGENT_LIVE_LLM_TIMEOUT", "90")),
            max_tokens=int(os.environ.get("ADAM_AGENT_LIVE_LLM_MAX_TOKENS", "2048")),
            allow_custom_base_url=bool(base_url),
            custom_base_url_approved_by=approved_by if base_url else None,
        )
        exposure = LLMExposureConfig(
            mode="demo_rich_context",
            data_classification="processed_demo",
            external_api_allowed=True,
            approved_by=approved_by,
            approval_note="Opt-in live smoke test with synthetic/demo-sized prompt.",
            sample_rows_per_dataset=1,
        )
        context = {
            "study_id": "LIVE_SMOKE",
            "run_id": "run_live_llm_smoke",
            "target_dataset": "ADAE",
            "source_dataset_profiles": {
                "AE": {
                    "columns": ["USUBJID", "AESEQ", "AETERM", "AESTDTC"],
                    "row_count": 1,
                    "sample_rows": [
                        {"USUBJID": "01", "AESEQ": "1", "AETERM": "HEADACHE", "AESTDTC": "2024-01-02"}
                    ],
                }
            },
            "spec_sources": [
                {
                    "file_name": "adae_minimal.csv",
                    "columns": ["Dataset", "Variable", "Source", "Derivation"],
                    "sample_rows": [
                        {
                            "Dataset": "ADAE",
                            "Variable": "AETERM",
                            "Source": "AE.AETERM",
                            "Derivation": "Copy from AE.",
                        }
                    ],
                }
            ],
            "resolved_dependencies": {},
            "warnings": [],
        }
        client = build_llm_client(config)

        response = client.generate(
            _llm_request(
                prompt=(
                    "Return strict JSON for an ADAE generation package. "
                    "The r_code can be minimal, but it must write outputs/adae.csv."
                ),
                target="ADAE",
                study_id="LIVE_SMOKE",
                run_id="run_live_llm_smoke",
                provider=provider,
                model=model,
                exposure=exposure,
                context_dict=context,
                context_artifact_id="live_smoke_context",
                node="live_smoke_generate_downstream_code",
                call_id="live_smoke_llm_call",
                response_artifact_id="live_smoke_response",
            )
        )

        lowered = response.response_text.lower()
        self.assertIn("r_code", lowered)
        self.assertIn("adae", lowered)
        self.assertEqual(response.call_record.model, model)
        self.assertTrue(response.call_record.external_relay if base_url else True)
        self.assertNotIn(api_key, response.call_record.model_dump_json())


if __name__ == "__main__":
    unittest.main()
