"""Tests for the generic downstream ADaM runner."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

from tests.temp_workspace import test_session_root
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()
LOCAL_RSCRIPT = Path(r"C:\Dev\R-4.5.2\bin\Rscript.exe")

try:
    from adam_agent.downstream.runner import run_downstream_adam
    from adam_agent.llm.clients import LLMProviderConfig, MockLLMClient, build_llm_client
    from adam_agent.schemas.llm import LLMExposureConfig
    from adam_agent.tools.r_runner import LocalRRunner, RRunRequest, RRunResult
    from adam_agent.tools.sandbox import LocalRscriptSandboxRunner
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.downstream.runner import run_downstream_adam
    from adam_agent.llm.clients import LLMProviderConfig, MockLLMClient, build_llm_client
    from adam_agent.schemas.llm import LLMExposureConfig
    from adam_agent.tools.r_runner import LocalRRunner, RRunRequest, RRunResult
    from adam_agent.tools.sandbox import LocalRscriptSandboxRunner


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class FileWritingStubRRunner:
    """Stub runner that writes the canonical output without invoking R."""

    def run(self, request: RRunRequest) -> RRunResult:
        output_path = Path(request.working_dir) / "outputs" / f"{request.dataset.lower()}.csv"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text("USUBJID\n01\n", encoding="utf-8")
        return RRunResult(dataset=request.dataset, exit_code=0, stdout="stub wrote output", stderr="")


class FailingStubRRunner:
    def run(self, request: RRunRequest) -> RRunResult:
        return RRunResult(dataset=request.dataset, exit_code=1, stdout="", stderr="stub downstream failure")


class SequentialMockLLMClient:
    """Mock client that returns one response per call."""

    def __init__(self, responses: list[str]) -> None:
        self.responses = list(responses)
        self.calls = []

    def generate(self, request):
        self.calls.append(request)
        index = min(len(self.calls) - 1, len(self.responses) - 1)
        return MockLLMClient(fixed_response_text=self.responses[index]).generate(request)


class FailsThenWritesStubRRunner:
    """Fail once, then write the canonical output on the repair attempt."""

    def __init__(self) -> None:
        self.calls = 0

    def run(self, request: RRunRequest) -> RRunResult:
        self.calls += 1
        if self.calls == 1:
            return RRunResult(dataset=request.dataset, exit_code=1, stdout="", stderr="unexpected symbol in generated code")
        output_path = Path(request.working_dir) / "outputs" / f"{request.dataset.lower()}.csv"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text("USUBJID\n01\n", encoding="utf-8")
        return RRunResult(dataset=request.dataset, exit_code=0, stdout="repair wrote output", stderr="")


class MissingSourceVariableRRunner:
    """Simulate an R error caused by a missing source variable."""

    def run(self, request: RRunRequest) -> RRunResult:
        return RRunResult(dataset=request.dataset, exit_code=1, stdout="", stderr="object 'AETERM' not found")


class DownstreamRunnerTests(unittest.TestCase):
    def test_downstream_runner_wires_context_llm_code_writer_r_and_validation(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_success")
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["Demo response."],
                "risk_points": [],
                "used_inputs": ["AE", "ADSL"],
                "expected_outputs": ["adae.csv"],
            }
        )

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_success",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_success"),
            llm_client=MockLLMClient(fixed_response_text=response),
            r_runner=FileWritingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(result.validation_status, "pass")
        self.assertIn("llm_context", result.artifacts)
        self.assertIn("llm_prompt", result.artifacts)
        self.assertIn("llm_response", result.artifacts)
        self.assertIn("generated_code", result.artifacts)
        self.assertIn("validation_report", result.artifacts)
        self.assertIn("output_adam", result.artifacts)
        self.assertFalse(result.validation_report["stubbed_r_execution"])
        self.assertEqual(result.llm_call_record.provider, "mock")
        self.assertEqual(result.llm_call_record.datasets_included, ["AE", "ADSL"])
        self.assertTrue(result.llm_call_record.prompt_artifact_id.startswith("llm_prompt_compact_"))
        self.assertTrue((study_dir / "runs" / "run_downstream_success" / "llm" / "adae_context.json").exists())
        compact_prompt_path = study_dir / "runs" / "run_downstream_success" / "llm" / "adae_compact_prompt.txt"
        self.assertTrue(compact_prompt_path.exists())
        compact_prompt = compact_prompt_path.read_text(encoding="utf-8")
        self.assertIn("## Target Spec", compact_prompt)
        self.assertNotIn('"target_spec"', compact_prompt)
        self.assertTrue((study_dir / "runs" / "run_downstream_success" / "code" / "build_adae.R").exists())
        self.assertTrue((study_dir / "runs" / "run_downstream_success" / "outputs" / "adae.csv").exists())

    def test_downstream_runner_reports_llm_parse_failure_without_running_r(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_bad_llm")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_bad_llm",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_bad_llm"),
            llm_client=MockLLMClient(fixed_response_text="not json"),
            r_runner=FailingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "llm_output_parse_error")
        self.assertIsNone(result.r_result)
        self.assertIn("validation_report", result.artifacts)
        self.assertIn("failure_report", result.artifacts)
        self.assertNotIn("generated_code", result.artifacts)
        self.assertIn("not valid JSON", result.error)
        failure_report = json.loads(Path(result.artifacts["failure_report"].path).read_text(encoding="utf-8"))
        self.assertEqual(failure_report["latest_root_cause"], "code_contract_error")
        self.assertEqual(failure_report["latest_recommended_route"], "repair_code")

    def test_downstream_runner_blocks_static_rule_failure_before_r_execution(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_static_block")
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nsystem('whoami')\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["Unsafe code response."],
                "risk_points": [],
                "used_inputs": ["AE"],
                "expected_outputs": ["adae.csv"],
            }
        )

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_static_block",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_static_block"),
            llm_client=MockLLMClient(fixed_response_text=response),
            r_runner=FailingStubRRunner(),
            source_datasets=["AE"],
            max_repair_attempts=0,
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "static_rule_error")
        self.assertIsNone(result.r_result)
        self.assertIn("static_check", result.artifacts)
        self.assertEqual(result.failure_records[0].root_cause, "static_rule_violation")
        self.assertEqual(result.failure_records[0].recommended_route, "repair_code")
        static_report = json.loads(Path(result.artifacts["static_check"].path).read_text(encoding="utf-8"))
        self.assertEqual(static_report["status"], "blocked")
        self.assertTrue(any(item["rule_id"] == "R_FORBIDDEN_CALL" for item in static_report["blocking_errors"]))

    def test_downstream_preflight_failure_records_sandbox_boundary(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_preflight_sandbox")
        outside_code = study_dir / "outside_code" / "build_adae.R"
        outside_code.parent.mkdir(parents=True)
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["Preflight failure test."],
                "risk_points": [],
                "used_inputs": ["AE"],
                "expected_outputs": ["outputs/adae.csv"],
            }
        )

        from adam_agent.downstream import runner as downstream_runner
        from adam_agent.llm.generated_code import write_generated_code_artifacts as real_writer

        def write_artifacts_outside_code(**kwargs):
            artifacts = real_writer(**kwargs)
            outside_code.write_text(Path(artifacts.code_artifact.path).read_text(encoding="utf-8"), encoding="utf-8")
            return artifacts.__class__(
                response_artifact=artifacts.response_artifact,
                code_artifact=artifacts.code_artifact.model_copy(update={"path": str(outside_code.as_posix())}),
                package_artifact=artifacts.package_artifact,
            )

        with patch.object(downstream_runner, "write_generated_code_artifacts", side_effect=write_artifacts_outside_code):
            result = run_downstream_adam(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_downstream_preflight_sandbox",
                target_dataset="ADAE",
                dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_preflight_sandbox"),
                llm_client=MockLLMClient(fixed_response_text=response),
                r_runner=LocalRscriptSandboxRunner(
                    run_dir=study_dir / "runs" / "run_downstream_preflight_sandbox",
                    rscript_path=None,
                    allowed_output_paths=[
                        study_dir / "runs" / "run_downstream_preflight_sandbox" / "outputs" / "adae.csv"
                    ],
                ),
                source_datasets=["AE"],
                max_repair_attempts=0,
            )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "r_sandbox_preflight_error")
        self.assertEqual(result.validation_report["sandbox"]["backend_name"], "local_rscript")
        self.assertFalse(result.validation_report["sandbox"]["hardened"])
        validation_payload = json.loads(Path(result.artifacts["validation_report"].path).read_text(encoding="utf-8"))
        self.assertEqual(validation_payload["sandbox"]["backend_name"], "local_rscript")
        self.assertFalse(validation_payload["sandbox"]["hardened"])

    def test_downstream_runner_repairs_r_failure_once_and_passes(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_repair_pass")
        initial_response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\nstop('bad generated code')\n",
                "assumptions": ["Initial bad code."],
                "risk_points": [],
                "used_inputs": ["AE"],
                "expected_outputs": ["adae.csv"],
            }
        )
        repair_response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["Repair only fixes runtime/output behavior."],
                "risk_points": [],
                "used_inputs": ["AE"],
                "expected_outputs": ["adae.csv"],
            }
        )
        llm_client = SequentialMockLLMClient([initial_response, repair_response])
        r_runner = FailsThenWritesStubRRunner()

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_repair_pass",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_repair_pass"),
            llm_client=llm_client,
            r_runner=r_runner,
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(result.validation_status, "pass")
        self.assertEqual(len(llm_client.calls), 2)
        self.assertEqual(r_runner.calls, 2)
        self.assertEqual(len(result.failure_records), 1)
        self.assertEqual(result.failure_records[0].root_cause, "r_runtime_error")
        self.assertIn("generated_code_repair1", result.artifacts)
        self.assertIn("failure_report", result.artifacts)
        failure_report = json.loads(Path(result.artifacts["failure_report"].path).read_text(encoding="utf-8"))
        self.assertEqual(failure_report["status"], "repaired")
        self.assertEqual(failure_report["failure_count"], 1)

    def test_downstream_runner_does_not_repair_missing_source_variable(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_missing_source")
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(AETERM = missing_source), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["Missing source variable scenario."],
                "risk_points": [],
                "used_inputs": ["AE"],
                "expected_outputs": ["adae.csv"],
            }
        )
        llm_client = SequentialMockLLMClient([response, response])

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_missing_source",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_missing_source"),
            llm_client=llm_client,
            r_runner=MissingSourceVariableRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "fail")
        self.assertEqual(len(llm_client.calls), 1)
        self.assertEqual(result.failure_records[0].failure_type, "spec_error")
        self.assertEqual(result.failure_records[0].root_cause, "source_variable_missing")
        self.assertEqual(result.failure_records[0].recommended_route, "revise_spec")
        self.assertNotIn("generated_code_repair1", result.artifacts)
        failure_report = json.loads(Path(result.artifacts["failure_report"].path).read_text(encoding="utf-8"))
        self.assertEqual(failure_report["latest_recommended_route"], "revise_spec")

    def test_downstream_runner_records_r_failure_and_missing_output(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_r_failure")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_r_failure",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_r_failure"),
            r_runner=FailingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "fail")
        self.assertFalse(result.r_result.success)
        self.assertTrue(any("Expected output file" in error for error in result.validation_report["errors"]))
        self.assertIn("generated_code", result.artifacts)
        self.assertNotIn("output_adam", result.artifacts)

    def test_downstream_runner_fails_when_available_dependency_profile_is_not_usable(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_unusable_dependency", include_reference_adsl_csv=False)
        sas7bdat_path = study_dir / "runs" / "run_downstream_unusable_dependency" / "outputs" / "adsl.sas7bdat"
        sas7bdat_path.parent.mkdir(parents=True)
        sas7bdat_path.write_text("not a real sas7bdat", encoding="utf-8")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_unusable_dependency",
            target_dataset="ADAE",
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "resolution_status": "available",
                    "artifact_path": str(sas7bdat_path.as_posix()),
                    "artifact_source": "run_output",
                }
            ],
            r_runner=FileWritingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "fail")
        self.assertTrue(any("Profile not fully available" in error for error in result.validation_report["errors"]))
        self.assertNotIn("output_adam", result.artifacts)

    def test_downstream_runner_does_not_use_reference_adam_as_runtime_dependency(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_reference_not_runtime")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_reference_not_runtime",
            target_dataset="ADAE",
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "resolution_status": "available",
                    "artifact_path": str((study_dir / "reference_adam" / "adsl.csv").as_posix()),
                    "artifact_source": "reference_adam",
                }
            ],
            r_runner=FileWritingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(result.llm_call_record.datasets_included, ["AE"])
        context_payload = json.loads(Path(result.artifacts["llm_context"].path).read_text(encoding="utf-8"))
        self.assertEqual(context_payload["resolved_dependencies"], {})
        self.assertTrue(
            any("Reference ADaM ADSL is available only for comparison" in warning for warning in context_payload["warnings"])
        )

    def test_downstream_runner_respects_demo_rich_context_sample_rows(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_demo_context")
        exposure = LLMExposureConfig(
            mode="demo_rich_context",
            data_classification="processed_demo",
            external_api_allowed=True,
            sample_rows_per_dataset=1,
        )

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_demo_context",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_demo_context"),
            exposure=exposure,
            r_runner=FileWritingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(result.llm_call_record.sample_row_counts, {"AE": 1, "ADSL": 1})
        self.assertFalse(result.validation_report["stubbed_r_execution"])

    def test_default_downstream_runner_marks_structural_stub_not_real_generation(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_default_stub")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_default_stub",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_default_stub"),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed_stub")
        self.assertEqual(result.validation_status, "structural_stub_pass")
        self.assertTrue(result.validation_report["stubbed_r_execution"])
        self.assertTrue(result.validation_report["not_real_derivation"])
        self.assertEqual(result.validation_report["llm_provider"], "mock")

    def test_downstream_runner_uses_configured_provider_client_and_records_audit(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_provider_client")
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["Provider response."],
                "risk_points": [],
                "used_inputs": ["AE", "ADSL"],
                "expected_outputs": ["adae.csv"],
            }
        )
        calls = []

        def fake_transport(url, headers, payload, timeout_seconds):
            calls.append((url, headers, payload, timeout_seconds))
            return {"choices": [{"message": {"content": response}}]}

        provider_config = LLMProviderConfig(
            provider="deepseek",
            model="deepseek-chat",
            api_key="test-key",
        )
        exposure = LLMExposureConfig(
            mode="demo_rich_context",
            data_classification="processed_demo",
            external_api_allowed=True,
            sample_rows_per_dataset=1,
        )

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_provider_client",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_provider_client"),
            llm_client=build_llm_client(provider_config, transport=fake_transport),
            exposure=exposure,
            provider=provider_config.provider,
            model=provider_config.model,
            r_runner=FileWritingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(calls[0][0], "https://api.deepseek.com/v1/chat/completions")
        self.assertEqual(result.llm_call_record.provider_alias, "deepseek")
        self.assertEqual(result.llm_call_record.transport, "openai-compatible")
        self.assertEqual(result.llm_call_record.provider_base_url, "https://api.deepseek.com/v1")
        self.assertEqual(result.validation_report["provider_alias"], "deepseek")
        self.assertEqual(result.validation_report["transport"], "openai-compatible")
        self.assertFalse(result.validation_report["not_real_derivation"])

    def test_downstream_runner_refuses_non_mock_provider_without_explicit_client(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_provider_without_client")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_provider_without_client",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_provider_without_client"),
            provider="deepseek",
            model="deepseek-chat",
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "llm_client_required")
        self.assertIn("refusing to fall back to mock", result.error)
        self.assertEqual(result.validation_report["llm_provider"], "deepseek")
        self.assertNotIn("generated_code", result.artifacts)

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_downstream_runner_can_execute_generated_code_with_local_r(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_local_r")
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE, recursive = TRUE)\nae <- read.csv('../../input_sdtm/ae.csv', stringsAsFactors = FALSE)\nwrite.csv(data.frame(USUBJID = ae[['USUBJID']], AETERM = ae[['AETERM']]), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["Local R smoke response."],
                "risk_points": ["Prototype only."],
                "used_inputs": ["AE"],
                "expected_outputs": ["outputs/adae.csv"],
            }
        )

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_local_r",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir, "run_downstream_local_r"),
            llm_client=MockLLMClient(fixed_response_text=response),
            r_runner=LocalRscriptSandboxRunner(
                run_dir=study_dir / "runs" / "run_downstream_local_r",
                rscript_path=str(LOCAL_RSCRIPT),
                allowed_output_paths=[study_dir / "runs" / "run_downstream_local_r" / "outputs" / "adae.csv"],
            ),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(result.validation_status, "pass")
        self.assertFalse(result.validation_report["stubbed_r_execution"])
        self.assertTrue(result.validation_report["not_real_derivation"])
        self.assertEqual(result.validation_report["sandbox"]["backend_name"], "local_rscript")
        self.assertFalse(result.validation_report["sandbox"]["hardened"])
        self.assertFalse(any("expected_outputs" in warning for warning in result.validation_report["warnings"]))
        self.assertTrue((study_dir / "runs" / "run_downstream_local_r" / "outputs" / "adae.csv").exists())


def _study_with_adae_inputs(name: str, *, include_reference_adsl_csv: bool = True) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    input_spec = study_dir / "input_spec"
    reference_adam = study_dir / "reference_adam"
    input_sdtm.mkdir(parents=True)
    input_spec.mkdir()
    reference_adam.mkdir()
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")
    (input_spec / "adae.json").write_text(
        json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
        encoding="utf-8",
    )
    if include_reference_adsl_csv:
        (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n02,2024-01-02\n", encoding="utf-8")
    return study_dir


def _available_adsl_resolution(study_dir: Path, run_id: str) -> list[dict[str, str]]:
    output_dir = study_dir / "runs" / run_id / "outputs"
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n02,2024-01-02\n", encoding="utf-8")
    return [
        {
            "target_dataset": "ADAE",
            "required_dataset": "ADSL",
            "resolution_status": "available",
            "artifact_path": str((output_dir / "adsl.csv").as_posix()),
            "artifact_source": "run_output",
        }
    ]


if __name__ == "__main__":
    unittest.main()
