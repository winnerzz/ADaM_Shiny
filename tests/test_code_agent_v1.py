"""Focused tests for Code Agent V1 package building."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

from tests.temp_workspace import test_session_root

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()

try:
    from adam_agent.agents.code_agent import build_code_package
    from adam_agent.llm.clients import LLMResponse
    from adam_agent.schemas.llm import LLMCallRecord
    from adam_agent.schemas.code_agent import CodeAgentTask
    from adam_agent.tools.r_runner import RRunResult
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.agents.code_agent import build_code_package
    from adam_agent.llm.clients import LLMResponse
    from adam_agent.schemas.llm import LLMCallRecord
    from adam_agent.schemas.code_agent import CodeAgentTask
    from adam_agent.tools.r_runner import RRunResult


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class CapturingTrialRunner:
    backend_name = "capturing_trial_runner"
    hardened = False

    def __init__(self, *, run_dir: Path, allowed_output_paths: list[Path]) -> None:
        self.run_dir = run_dir
        self.allowed_output_paths = allowed_output_paths

    def boundary(self):  # type: ignore[no-untyped-def]
        class Boundary:
            def __init__(self, run_dir: Path) -> None:
                self.run_dir = run_dir

            def as_dict(self) -> dict[str, object]:
                return {
                    "backend_name": "capturing_trial_runner",
                    "hardened": False,
                    "run_dir": str(self.run_dir.as_posix()),
                    "network_disabled": False,
                    "notes": ["test runner"],
                }

        return Boundary(self.run_dir)

    def run(self, request):  # type: ignore[no-untyped-def]
        output_path = self.allowed_output_paths[0]
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text("USUBJID\n01\n", encoding="utf-8")
        return RRunResult(dataset=request.dataset, exit_code=0, stdout="trial ok", stderr="")


class RelativeInputCheckingTrialRunner(CapturingTrialRunner):
    def run(self, request):  # type: ignore[no-untyped-def]
        working_dir = Path(request.working_dir)
        prompt_contract_path = (working_dir / ".." / ".." / "input_sdtm" / "dm.csv").resolve()
        local_compat_path = working_dir / "input_sdtm" / "dm.csv"
        if not prompt_contract_path.exists():
            return RRunResult(
                dataset=request.dataset,
                exit_code=1,
                stdout="",
                stderr=f"missing prompt-contract input path: {prompt_contract_path}",
            )
        if not local_compat_path.exists():
            return RRunResult(
                dataset=request.dataset,
                exit_code=1,
                stdout="",
                stderr=f"missing local trial input path: {local_compat_path}",
            )
        return super().run(request)


def _trial_runner_factory(*, run_dir: Path, rscript_path: str | None, allowed_output_paths: list[Path]):
    return CapturingTrialRunner(run_dir=run_dir, allowed_output_paths=allowed_output_paths)


def _relative_input_trial_runner_factory(*, run_dir: Path, rscript_path: str | None, allowed_output_paths: list[Path]):
    return RelativeInputCheckingTrialRunner(run_dir=run_dir, allowed_output_paths=allowed_output_paths)


def _context() -> dict[str, object]:
    return {
        "study_id": "PSY201",
        "run_id": "run_code_agent",
        "target_dataset": "ADAE",
        "target_spec": {
            "dataset": "ADAE",
            "path": "input_spec/adae.json",
            "variables": [{"variable": "USUBJID", "derivation": "copy AE.USUBJID"}],
        },
        "source_dataset_profiles": {
            "AE": {
                "columns": ["USUBJID", "AETERM"],
                "sample_rows": [],
            }
        },
        "resolved_dependencies": {},
        "runtime_contract": {
            "language": "R",
            "runtime_output_path": "outputs/adae.csv",
            "input_path_policy": "Use read_path values.",
        },
        "exposure": {"mode": "metadata_only"},
        "warnings": [],
    }


class FixedLLMClient:
    def __init__(self, response_text: str) -> None:
        self.response_text = response_text

    def generate(self, request):  # type: ignore[no-untyped-def]
        return LLMResponse(
            response_text=self.response_text,
            call_record=LLMCallRecord(
                call_id=request.call_id,
                node=request.node,
                provider=request.provider,
                model=request.model,
                exposure_mode=request.exposure.mode,
                datasets_included=request.datasets_included,
                variables_included=request.variables_included,
                redaction_policy=request.redaction_policy,
                provider_locality="external_api",
                provider_alias="test-openai-compatible",
                transport="test",
            ),
        )


def _fixed_llm_builder(response_text: str):
    def _builder(_config):  # type: ignore[no-untyped-def]
        return FixedLLMClient(response_text)

    return _builder


class CodeAgentV1Tests(unittest.TestCase):
    def test_mock_code_agent_builds_review_package_without_official_output(self) -> None:
        study_dir = _workspace_dir("code_agent_v1_success") / "PSY201"
        (study_dir / "input_sdtm").mkdir(parents=True)
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")

        result = build_code_package(
            CodeAgentTask(
                study_dir=str(study_dir),
                study_id="PSY201",
                run_id="run_code_agent",
                dataset="ADAE",
                context=_context(),
                spec_source="input_spec",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
                required_identifiers=["USUBJID"],
                required_identifier_source_id="input_spec/adae.json",
            ),
            sandbox_runner_factory=_trial_runner_factory,
        )

        official_output = study_dir / "runs" / "run_code_agent" / "outputs" / "adae.csv"
        package_path = Path(result.review_package_path)
        runtime_path = Path(str(result.trial_runtime_report_path))
        trial_output = Path(str(result.trial_output_path))

        self.assertEqual(result.status, "ready_for_review")
        self.assertTrue(result.ready_for_human_review)
        self.assertEqual(result.trial_run_status, "pass")
        self.assertTrue(package_path.exists())
        self.assertTrue(runtime_path.exists())
        self.assertTrue(trial_output.exists())
        self.assertFalse(official_output.exists())
        package = json.loads(package_path.read_text(encoding="utf-8"))
        self.assertFalse(package["official_output_created"])
        self.assertEqual(package["trial_run_status"], "pass")
        self.assertEqual(package["attempts"][0]["status"], "trial_passed")

    def test_trial_workspace_supports_official_relative_input_paths(self) -> None:
        study_dir = _workspace_dir("code_agent_v1_relative_paths") / "PSY201"
        (study_dir / "input_sdtm").mkdir(parents=True)
        (study_dir / "input_sdtm" / "dm.csv").write_text("USUBJID,STUDYID\n01,PSY201\n", encoding="utf-8")

        r_code = (
            "dir.create('outputs', showWarnings = FALSE, recursive = TRUE)\n"
            "dm <- read.csv('../../input_sdtm/dm.csv', stringsAsFactors = FALSE)\n"
            "write.csv(data.frame(USUBJID = dm$USUBJID), 'outputs/adae.csv', row.names = FALSE)\n"
        )
        response_text = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": r_code,
                "assumptions": [],
                "risk_points": [],
                "used_inputs": ["../../input_sdtm/dm.csv"],
                "expected_outputs": ["outputs/adae.csv"],
            }
        )

        result = build_code_package(
            CodeAgentTask(
                study_dir=str(study_dir),
                study_id="PSY201",
                run_id="run_code_agent",
                dataset="ADAE",
                context=_context(),
                spec_source="input_spec",
                llm_provider={
                    "provider": "openai-compatible",
                    "model": "test-model",
                    "api_key": "test-key",
                    "base_url": "https://api.example.invalid/v1",
                    "allow_custom_base_url": True,
                    "custom_base_url_approved_by": "unit-test",
                },
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
                required_identifiers=["USUBJID"],
                required_identifier_source_id="input_spec/adae.json",
            ),
            llm_client_builder=_fixed_llm_builder(response_text),
            sandbox_runner_factory=_relative_input_trial_runner_factory,
        )

        official_output = study_dir / "runs" / "run_code_agent" / "outputs" / "adae.csv"
        trial_output = Path(str(result.trial_output_path))
        prompt_contract_copy = study_dir / "runs" / "run_code_agent" / "_ca" / "input_sdtm" / "dm.csv"
        local_trial_copy = study_dir / "runs" / "run_code_agent" / "_ca" / "adae" / "a01" / "input_sdtm" / "dm.csv"

        self.assertEqual(result.status, "ready_for_review")
        self.assertEqual(result.trial_run_status, "pass")
        self.assertTrue(trial_output.exists())
        self.assertTrue(prompt_contract_copy.exists())
        self.assertTrue(local_trial_copy.exists())
        self.assertFalse(official_output.exists())


if __name__ == "__main__":
    unittest.main()
