"""Tests for Phase 4 tool-layer contracts."""

from __future__ import annotations

import json
import os
import sys
import unittest
import uuid
from http.client import RemoteDisconnected
from pathlib import Path

from tests.temp_workspace import test_session_root
from unittest.mock import patch

from pydantic import ValidationError

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()


def workspace_tempdir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path

try:
    from adam_agent.llm.clients import (
        AnthropicMessagesLLMClient,
        LLMClientConfigError,
        LLMProviderConfig,
        LLMProviderResponseError,
        LLMRequest,
        LLMResponse,
        MockLLMClient,
        OpenAICompatibleConfig,
        OpenAICompatibleLLMClient,
        build_llm_client,
    )
    from adam_agent.llm.model_registry import ModelNotImplementedError, ModelRegistry
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.llm import LLMCallRecord, LLMExposureConfig
    from adam_agent.tools.artifacts import ArtifactStore
    from adam_agent.tools.config import ConfigLoader
    from adam_agent.tools.compare import compare_dataset_files
    from adam_agent.tools.r_runner import RRunRequest, StubRRunner
    from adam_agent.tools.sdtm_reader import SDTMReader
    from adam_agent.tools.study_inputs import StudyInputScanner
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.llm.clients import (
        AnthropicMessagesLLMClient,
        LLMClientConfigError,
        LLMProviderConfig,
        LLMProviderResponseError,
        LLMRequest,
        LLMResponse,
        MockLLMClient,
        OpenAICompatibleConfig,
        OpenAICompatibleLLMClient,
        build_llm_client,
    )
    from adam_agent.llm.model_registry import ModelNotImplementedError, ModelRegistry
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.llm import LLMCallRecord, LLMExposureConfig
    from adam_agent.tools.artifacts import ArtifactStore
    from adam_agent.tools.config import ConfigLoader
    from adam_agent.tools.compare import compare_dataset_files
    from adam_agent.tools.r_runner import RRunRequest, StubRRunner
    from adam_agent.tools.sdtm_reader import SDTMReader
    from adam_agent.tools.study_inputs import StudyInputScanner


class Phase4ToolTests(unittest.TestCase):
    def test_artifact_store_registers_file_and_writes_canonical_manifest(self) -> None:
        tmp = workspace_tempdir("artifact_store_manifest")
        study_dir = tmp / "PSY201"
        source = study_dir / "input_sdtm" / "dm.csv"
        source.parent.mkdir(parents=True)
        source.write_text("USUBJID,AGE\n01,34\n", encoding="utf-8")

        store = ArtifactStore(study_dir, study_id="PSY201", run_id="run_001")
        artifact = store.register_existing(
            source,
            artifact_id="input_sdtm_dm",
            kind="input_sdtm",
            role="source",
            dataset="DM",
        )
        manifest = store.write_manifest()

        self.assertIsInstance(artifact, ArtifactRef)
        self.assertTrue(artifact.sha256.startswith("sha256:"))
        self.assertEqual(manifest.path, (study_dir / "runs" / "run_001" / "audit" / "manifest.json").as_posix())
        payload = json.loads(Path(manifest.path).read_text(encoding="utf-8"))
        self.assertEqual(payload["study_id"], "PSY201")
        self.assertEqual(payload["run_id"], "run_001")
        self.assertEqual(payload["artifacts"][0]["artifact_id"], "input_sdtm_dm")

    def test_artifact_store_missing_file_raises_clear_error(self) -> None:
        tmp = workspace_tempdir("artifact_store_missing")
        store = ArtifactStore(tmp / "PSY201", study_id="PSY201", run_id="run_001")
        with self.assertRaises(FileNotFoundError):
            store.register_existing(
                tmp / "missing.csv",
                artifact_id="missing",
                kind="input_sdtm",
                role="source",
            )

    def test_study_input_scanner_returns_structured_index(self) -> None:
        tmp = workspace_tempdir("study_input_scanner")
        study_dir = tmp / "PSY201"
        (study_dir / "input_sdtm").mkdir(parents=True)
        (study_dir / "reference_adam").mkdir()
        (study_dir / "legacy_code").mkdir()
        (study_dir / "input_sdtm" / "dm.csv").write_text("USUBJID\n01\n", encoding="utf-8")
        (study_dir / "input_sdtm" / "bad.sas").write_text("data dm; run;", encoding="utf-8")
        (study_dir / "reference_adam" / "adsl.sas7bdat").write_text("stub", encoding="utf-8")
        (study_dir / "legacy_code" / "ADSL.sas").write_text("data adsl; run;", encoding="utf-8")

        index = StudyInputScanner(study_dir).scan()

        self.assertIsInstance(index.input_sdtm["DM"], ArtifactRef)
        self.assertEqual(index.input_sdtm["DM"].kind, "input_sdtm")
        self.assertEqual(index.reference_adam["ADSL"].kind, "reference_adam")
        self.assertEqual(index.legacy_code["adsl"].kind, "legacy_code")
        self.assertEqual(index.invalid_files[0].reason, "input_sdtm supports only csv and sas7bdat in the MVP")

    def test_sdtm_reader_profiles_csv_and_limits_samples(self) -> None:
        tmp = workspace_tempdir("sdtm_reader_csv")
        path = tmp / "dm.csv"
        path.write_text("USUBJID,AGE\n01,34\n02,41\n", encoding="utf-8")

        profile = SDTMReader().profile(path, sample_rows=1)

        self.assertEqual(profile.status, "ok")
        self.assertEqual(profile.columns, ["USUBJID", "AGE"])
        self.assertEqual(profile.row_count, 2)
        self.assertEqual(profile.sample_rows, [{"USUBJID": "01", "AGE": "34"}])

    def test_sdtm_reader_sas7bdat_is_explicit_not_implemented(self) -> None:
        tmp = workspace_tempdir("sdtm_reader_sas7bdat")
        path = tmp / "dm.sas7bdat"
        path.write_text("stub", encoding="utf-8")

        profile = SDTMReader().profile(path)

        self.assertEqual(profile.status, "not_implemented_yet")
        self.assertIn("not implemented", profile.message)

    def test_compare_supports_sas7bdat_reference_through_injected_reader(self) -> None:
        tmp = workspace_tempdir("compare_sas7bdat_reference")
        generated = tmp / "addm.csv"
        reference = tmp / "addm.sas7bdat"
        generated.write_text(
            "USUBJID,SITEID,AGE\n01,01,21\n02,01,60\n",
            encoding="utf-8",
        )
        reference.write_text("binary placeholder", encoding="utf-8")

        def fake_reader(path: Path) -> dict:
            self.assertEqual(path, reference)
            return {
                "status": "ok",
                "columns": ["usubjid", "siteid", "age"],
                "rows": [
                    {"usubjid": "01", "siteid": "01", "age": "21"},
                    {"usubjid": "03", "siteid": "01", "age": "70"},
                ],
            }

        result = compare_dataset_files("ADDM", generated, reference, table_reader=fake_reader)

        self.assertEqual(result["status"], "differences")
        self.assertEqual(result["generated_file"], "addm.csv")
        self.assertEqual(result["reference_file"], "addm.sas7bdat")
        self.assertEqual(result["generated_only_columns"], [])
        self.assertEqual(result["reference_only_columns"], [])
        self.assertEqual(result["key_columns"], ["USUBJID"])
        self.assertEqual(result["generated_only_keys"], ["02"])
        self.assertEqual(result["reference_only_keys"], ["03"])

    def test_config_loader_defaults_and_demo_alias_use_existing_schema(self) -> None:
        default_config = ConfigLoader().load(study_id="PSY201", run_id="run_001")
        self.assertIsInstance(default_config.llm_exposure, LLMExposureConfig)
        self.assertEqual(default_config.llm_exposure.mode, "metadata_only")
        self.assertEqual(default_config.llm_provider.provider, "mock")
        self.assertEqual(default_config.llm_provider.model, "mock-model")

        demo_config = ConfigLoader().from_dict(
            {
                "study_id": "PSY201",
                "run_id": "run_002",
                "llm_exposure": {
                    "llm_exposure_mode": "demo_rich_context",
                    "data_classification": "processed_demo",
                    "external_api_allowed": True,
                    "sample_rows_per_dataset": 20,
                },
                "llm_provider": {
                    "provider": "deepseek",
                    "model": "deepseek-chat",
                    "api_key_env": "DEEPSEEK_API_KEY",
                },
            }
        )
        self.assertEqual(demo_config.llm_exposure.mode, "demo_rich_context")
        self.assertEqual(demo_config.llm_exposure.sample_rows_per_dataset, 20)
        self.assertEqual(demo_config.llm_provider.provider, "deepseek")
        self.assertEqual(demo_config.llm_provider.model, "deepseek-chat")

    def test_config_loader_rejects_full_data_without_approver(self) -> None:
        with self.assertRaises(ValidationError):
            ConfigLoader().from_dict(
                {
                    "mode": "full_data_allowed",
                    "data_classification": "processed_demo",
                }
            )

    def test_mock_llm_client_runs_without_api_key_and_returns_audit_record(self) -> None:
        old_key = os.environ.pop("OPENAI_API_KEY", None)
        try:
            response = MockLLMClient().generate(
                LLMRequest(
                    prompt="Draft ADSL spec",
                    provider="mock",
                    model="mock-model",
                    exposure=LLMExposureConfig(),
                    node="draft_spec_stub",
                    call_id="llm_001",
                )
            )
        finally:
            if old_key is not None:
                os.environ["OPENAI_API_KEY"] = old_key

        self.assertIsInstance(response, LLMResponse)
        self.assertIsInstance(response.call_record, LLMCallRecord)
        self.assertEqual(response.call_record.provider, "mock")
        self.assertEqual(response.call_record.provider_locality, "local_model")
        self.assertIsNotNone(response.call_record.prompt_artifact_id)
        self.assertIsNotNone(response.call_record.response_artifact_id)
        self.assertTrue(response.call_record.prompt_hash.startswith("sha256:"))
        self.assertTrue(response.call_record.response_hash.startswith("sha256:"))

    def test_mock_llm_enforces_metadata_only_policy(self) -> None:
        with self.assertRaises(ValidationError):
            MockLLMClient().generate(
                LLMRequest(
                    prompt="Do not include rows",
                    provider="mock",
                    model="mock-model",
                    exposure=LLMExposureConfig(),
                    node="draft_spec_stub",
                    call_id="llm_002",
                    sample_row_counts={"DM": 1},
                )
            )

    def test_model_registry_does_not_fallback_real_providers_to_mock(self) -> None:
        registry = ModelRegistry()

        model = registry.lookup("mock", "mock-model")
        self.assertFalse(model.requires_api_key)

        openai_model = registry.lookup("openai", "gpt-5.5")
        self.assertEqual(openai_model.provider, "openai")
        self.assertEqual(openai_model.model, "gpt-5.5")
        self.assertTrue(openai_model.requires_api_key)
        self.assertEqual(openai_model.provider_locality, "external_api")

        anthropic_model = registry.lookup("anthropic", "claude-opus-4.5")
        self.assertEqual(anthropic_model.provider, "anthropic")
        self.assertEqual(anthropic_model.model, "claude-opus-4.5")
        self.assertTrue(anthropic_model.requires_api_key)

    def test_openai_compatible_client_requires_explicit_external_api_approval(self) -> None:
        client = OpenAICompatibleLLMClient(
            OpenAICompatibleConfig(base_url="https://example.test/v1", api_key="test-key"),
            transport=lambda _url, _headers, _payload, _timeout: {
                "choices": [{"message": {"content": "not used"}}]
            },
        )

        with self.assertRaises(LLMClientConfigError):
            client.generate(
                LLMRequest(
                    prompt="Draft ADAE code",
                    provider="openai-compatible",
                    model="gpt-5.5",
                    exposure=LLMExposureConfig(),
                    node="generate_downstream_code",
                    call_id="llm_external_blocked",
                )
            )

    def test_openai_compatible_client_requires_api_key(self) -> None:
        old_key = os.environ.pop("OPENAI_API_KEY", None)
        try:
            client = OpenAICompatibleLLMClient(
                OpenAICompatibleConfig(base_url="https://example.test/v1", api_key=None),
                transport=lambda _url, _headers, _payload, _timeout: {
                    "choices": [{"message": {"content": "not used"}}]
                },
            )
            with self.assertRaises(LLMClientConfigError):
                client.generate(
                    LLMRequest(
                        prompt="Draft ADAE code",
                        provider="openai-compatible",
                        model="gpt-5.5",
                        exposure=LLMExposureConfig(
                            mode="demo_rich_context",
                            data_classification="processed_demo",
                            external_api_allowed=True,
                        ),
                        node="generate_downstream_code",
                        call_id="llm_missing_key",
                        prompt_artifact_id="prompt_missing_key",
                        response_artifact_id="response_missing_key",
                        redaction_policy="processed_demo_test",
                    )
                )
        finally:
            if old_key is not None:
                os.environ["OPENAI_API_KEY"] = old_key

    def test_openai_compatible_client_uses_transport_and_returns_audit_record(self) -> None:
        calls = []

        def fake_transport(url, headers, payload, timeout):
            calls.append((url, headers, payload, timeout))
            return {
                "choices": [
                    {
                        "message": {
                            "content": json.dumps(
                                {
                                    "dataset": "ADAE",
                                    "r_code": "write.csv(data.frame(), 'outputs/adae.csv')",
                                    "assumptions": [],
                                    "risk_points": [],
                                    "used_inputs": ["AE"],
                                    "expected_outputs": ["adae.csv"],
                                }
                            )
                        }
                    }
                ]
            }

        client = OpenAICompatibleLLMClient(
            OpenAICompatibleConfig(base_url="https://api.openai.com/v1", api_key="test-key"),
            transport=fake_transport,
        )
        response = client.generate(
            LLMRequest(
                prompt="Generate ADAE",
                provider="openai-compatible",
                model="gpt-5.5",
                exposure=LLMExposureConfig(
                    mode="demo_rich_context",
                    data_classification="processed_demo",
                    external_api_allowed=True,
                    sample_rows_per_dataset=1,
                ),
                node="generate_downstream_code",
                call_id="llm_openai_compatible_001",
                datasets_included=["AE"],
                sample_row_counts={"AE": 1},
                prompt_artifact_id="prompt_001",
                response_artifact_id="response_001",
                redaction_policy="processed_demo_test",
            )
        )

        self.assertIsInstance(response, LLMResponse)
        self.assertIn('"dataset": "ADAE"', response.response_text)
        self.assertEqual(response.call_record.provider, "openai-compatible")
        self.assertEqual(response.call_record.model, "gpt-5.5")
        self.assertEqual(response.call_record.provider_locality, "external_api")
        self.assertEqual(response.call_record.sample_row_counts, {"AE": 1})
        self.assertEqual(calls[0][0], "https://api.openai.com/v1/chat/completions")
        self.assertEqual(calls[0][1]["Authorization"], "Bearer test-key")
        self.assertEqual(calls[0][2]["model"], "gpt-5.5")
        self.assertNotIn("test-key", response.call_record.model_dump_json())

    def test_openai_compatible_http_client_wraps_remote_disconnect(self) -> None:
        client = OpenAICompatibleLLMClient(
            OpenAICompatibleConfig(base_url="https://api.openai.com/v1", api_key="test-key")
        )

        with patch("adam_agent.llm.clients.request.urlopen", side_effect=RemoteDisconnected("closed")):
            with self.assertRaisesRegex(LLMProviderResponseError, "closed the connection"):
                client.generate(
                    LLMRequest(
                        prompt="Generate ADAE",
                        provider="openai-compatible",
                        model="gpt-5.5",
                        exposure=LLMExposureConfig(
                            mode="demo_rich_context",
                            data_classification="processed_demo",
                            external_api_allowed=True,
                        ),
                        node="generate_downstream_code",
                        call_id="llm_remote_disconnect",
                        prompt_artifact_id="prompt_disconnect",
                        response_artifact_id="response_disconnect",
                        redaction_policy="processed_demo_test",
                    )
                )

    def test_client_factory_routes_deepseek_and_qwen_through_openai_compatible_transport(self) -> None:
        def fake_transport(_url, _headers, _payload, _timeout):
            return {"choices": [{"message": {"content": "ok"}}]}

        deepseek = build_llm_client(
            LLMProviderConfig(provider="deepseek", model="deepseek-chat", api_key="deepseek-key"),
            transport=fake_transport,
        )
        qwen = build_llm_client(
            LLMProviderConfig(provider="qwen", model="qwen-plus", api_key="qwen-key"),
            transport=fake_transport,
        )

        self.assertIsInstance(deepseek, OpenAICompatibleLLMClient)
        self.assertIsInstance(qwen, OpenAICompatibleLLMClient)
        deepseek_response = deepseek.generate(
            LLMRequest(
                prompt="Generate ADAE",
                provider="deepseek",
                model="deepseek-chat",
                exposure=LLMExposureConfig(
                    mode="demo_rich_context",
                    data_classification="processed_demo",
                    external_api_allowed=True,
                ),
                node="generate_downstream_code",
                call_id="llm_deepseek_alias",
                prompt_artifact_id="prompt_deepseek",
                response_artifact_id="response_deepseek",
                redaction_policy="processed_demo_test",
            )
        )

        self.assertEqual(deepseek_response.call_record.provider, "deepseek")
        self.assertEqual(deepseek_response.call_record.provider_alias, "deepseek")
        self.assertEqual(deepseek_response.call_record.transport, "openai-compatible")
        self.assertEqual(deepseek_response.call_record.provider_base_url, "https://api.deepseek.com/v1")

    def test_custom_openai_compatible_base_url_requires_explicit_approval(self) -> None:
        client = build_llm_client(
            LLMProviderConfig(
                provider="deepseek",
                model="deepseek-chat",
                base_url="https://relay.example/v1",
                api_key="relay-key",
            ),
            transport=lambda _url, _headers, _payload, _timeout: {
                "choices": [{"message": {"content": "not used"}}]
            },
        )

        with self.assertRaises(LLMClientConfigError):
            client.generate(
                LLMRequest(
                    prompt="Generate ADAE",
                    provider="deepseek",
                    model="deepseek-chat",
                    exposure=LLMExposureConfig(
                        mode="demo_rich_context",
                        data_classification="processed_demo",
                        external_api_allowed=True,
                    ),
                    node="generate_downstream_code",
                    call_id="llm_custom_base_denied",
                    prompt_artifact_id="prompt_custom_base_denied",
                    response_artifact_id="response_custom_base_denied",
                    redaction_policy="processed_demo_test",
                )
            )

    def test_custom_openai_compatible_base_url_records_relay_risk_when_approved(self) -> None:
        client = build_llm_client(
            LLMProviderConfig(
                provider="deepseek",
                model="deepseek-chat",
                base_url="https://relay.example/v1",
                api_key="relay-key",
                allow_custom_base_url=True,
                custom_base_url_approved_by="tester",
            ),
            transport=lambda _url, _headers, _payload, _timeout: {
                "choices": [{"message": {"content": "ok"}}]
            },
        )

        response = client.generate(
            LLMRequest(
                prompt="Generate ADAE",
                provider="deepseek",
                model="deepseek-chat",
                exposure=LLMExposureConfig(
                    mode="demo_rich_context",
                    data_classification="processed_demo",
                    external_api_allowed=True,
                ),
                node="generate_downstream_code",
                call_id="llm_custom_base_allowed",
                prompt_artifact_id="prompt_custom_base_allowed",
                response_artifact_id="response_custom_base_allowed",
                redaction_policy="processed_demo_test",
                subject_level_data_included=True,
            )
        )

        self.assertTrue(response.call_record.external_relay)
        self.assertIn("external_relay", response.call_record.risk_flags)
        self.assertIn("custom_base_url_approved", response.call_record.risk_flags)
        self.assertIn("subject_level_data_sent", response.call_record.risk_flags)

    def test_anthropic_messages_client_uses_official_message_shape_and_text_blocks(self) -> None:
        calls = []

        def fake_transport(url, headers, payload, timeout):
            calls.append((url, headers, payload, timeout))
            return {
                "content": [
                    {"type": "text", "text": "part one"},
                    {"type": "tool_use", "name": "ignored"},
                    {"type": "text", "text": "part two"},
                ]
            }

        client = build_llm_client(
            LLMProviderConfig(provider="anthropic", model="claude-sonnet-4-5", api_key="anthropic-key"),
            transport=fake_transport,
        )
        response = client.generate(
            LLMRequest(
                prompt="Generate ADAE",
                system_prompt="Return strict JSON.",
                provider="anthropic",
                model="claude-sonnet-4-5",
                exposure=LLMExposureConfig(
                    mode="demo_rich_context",
                    data_classification="processed_demo",
                    external_api_allowed=True,
                ),
                node="generate_downstream_code",
                call_id="llm_anthropic_001",
                prompt_artifact_id="prompt_anthropic",
                response_artifact_id="response_anthropic",
                redaction_policy="processed_demo_test",
            )
        )

        self.assertIsInstance(client, AnthropicMessagesLLMClient)
        self.assertEqual(response.response_text, "part one\npart two")
        self.assertEqual(response.call_record.provider_alias, "anthropic")
        self.assertEqual(response.call_record.transport, "anthropic-messages")
        self.assertEqual(calls[0][0], "https://api.anthropic.com/v1/messages")
        self.assertEqual(calls[0][1]["x-api-key"], "anthropic-key")
        self.assertEqual(calls[0][1]["anthropic-version"], "2023-06-01")
        self.assertEqual(calls[0][2]["system"], "Return strict JSON.")
        self.assertEqual(calls[0][2]["messages"], [{"role": "user", "content": "Generate ADAE"}])
        self.assertNotIn("anthropic-key", response.call_record.model_dump_json())

    def test_anthropic_messages_client_fails_closed_on_missing_text(self) -> None:
        client = build_llm_client(
            LLMProviderConfig(provider="anthropic", model="claude-sonnet-4-5", api_key="anthropic-key"),
            transport=lambda _url, _headers, _payload, _timeout: {"content": [{"type": "tool_use"}]},
        )

        with self.assertRaises(LLMProviderResponseError):
            client.generate(
                LLMRequest(
                    prompt="Generate ADAE",
                    provider="anthropic",
                    model="claude-sonnet-4-5",
                    exposure=LLMExposureConfig(
                        mode="demo_rich_context",
                        data_classification="processed_demo",
                        external_api_allowed=True,
                    ),
                    node="generate_downstream_code",
                    call_id="llm_anthropic_bad_response",
                    prompt_artifact_id="prompt_anthropic_bad",
                    response_artifact_id="response_anthropic_bad",
                    redaction_policy="processed_demo_test",
                )
            )

    def test_stub_r_runner_returns_structured_success_and_failure(self) -> None:
        runner = StubRRunner()
        success = runner.run(
            RRunRequest(
                code="print('ok')",
                dataset="ADSL",
                run_id="run_001",
                working_dir="runs/run_001",
            )
        )
        failure = runner.run(
            RRunRequest(
                code="stop('bad')",
                dataset="ADSL",
                run_id="run_001",
                working_dir="runs/run_001",
                scenario="failure",
            )
        )

        self.assertTrue(success.success)
        self.assertEqual(success.exit_code, 0)
        self.assertFalse(failure.success)
        self.assertNotEqual(failure.exit_code, 0)
        self.assertIn("stub R failure", failure.stderr)


if __name__ == "__main__":
    unittest.main()
