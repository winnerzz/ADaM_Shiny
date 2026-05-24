"""Tests for the generic downstream ADaM runner."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.downstream.runner import run_downstream_adam
    from adam_agent.llm.clients import MockLLMClient
    from adam_agent.schemas.llm import LLMExposureConfig
    from adam_agent.tools.r_runner import RRunRequest, RRunResult
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.downstream.runner import run_downstream_adam
    from adam_agent.llm.clients import MockLLMClient
    from adam_agent.schemas.llm import LLMExposureConfig
    from adam_agent.tools.r_runner import RRunRequest, RRunResult


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
            dependency_resolution=_available_adsl_resolution(study_dir),
            llm_client=MockLLMClient(fixed_response_text=response),
            r_runner=FileWritingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(result.validation_status, "pass")
        self.assertIn("llm_context", result.artifacts)
        self.assertIn("llm_response", result.artifacts)
        self.assertIn("generated_code", result.artifacts)
        self.assertIn("validation_report", result.artifacts)
        self.assertIn("output_adam", result.artifacts)
        self.assertEqual(result.llm_call_record.provider, "mock")
        self.assertEqual(result.llm_call_record.datasets_included, ["AE", "ADSL"])
        self.assertTrue((study_dir / "runs" / "run_downstream_success" / "llm" / "adae_context.json").exists())
        self.assertTrue((study_dir / "runs" / "run_downstream_success" / "code" / "build_adae.R").exists())
        self.assertTrue((study_dir / "runs" / "run_downstream_success" / "outputs" / "adae.csv").exists())

    def test_downstream_runner_reports_llm_parse_failure_without_running_r(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_bad_llm")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_bad_llm",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir),
            llm_client=MockLLMClient(fixed_response_text="not json"),
            r_runner=FailingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "llm_output_parse_error")
        self.assertIsNone(result.r_result)
        self.assertIn("validation_report", result.artifacts)
        self.assertNotIn("generated_code", result.artifacts)
        self.assertIn("not valid JSON", result.error)

    def test_downstream_runner_records_r_failure_and_missing_output(self) -> None:
        study_dir = _study_with_adae_inputs("downstream_runner_r_failure")

        result = run_downstream_adam(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_downstream_r_failure",
            target_dataset="ADAE",
            dependency_resolution=_available_adsl_resolution(study_dir),
            r_runner=FailingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "failed")
        self.assertEqual(result.validation_status, "fail")
        self.assertFalse(result.r_result.success)
        self.assertTrue(any("Expected output file" in error for error in result.validation_report["errors"]))
        self.assertIn("generated_code", result.artifacts)
        self.assertNotIn("output_adam", result.artifacts)

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
            dependency_resolution=_available_adsl_resolution(study_dir),
            exposure=exposure,
            r_runner=FileWritingStubRRunner(),
            source_datasets=["AE"],
        )

        self.assertEqual(result.status, "completed")
        self.assertEqual(result.llm_call_record.sample_row_counts, {"AE": 1, "ADSL": 1})


def _study_with_adae_inputs(name: str) -> Path:
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
    (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n02,2024-01-02\n", encoding="utf-8")
    return study_dir


def _available_adsl_resolution(study_dir: Path) -> list[dict[str, str]]:
    return [
        {
            "target_dataset": "ADAE",
            "required_dataset": "ADSL",
            "resolution_status": "available",
            "artifact_path": str((study_dir / "reference_adam" / "adsl.csv").as_posix()),
            "artifact_source": "reference_adam",
        }
    ]


if __name__ == "__main__":
    unittest.main()
