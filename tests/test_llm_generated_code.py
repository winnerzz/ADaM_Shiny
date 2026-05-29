"""Tests for parsing and writing LLM-generated R code artifacts."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.llm.generated_code import (
        LLMGeneratedCodeError,
        parse_generated_code_response,
        write_generated_code_artifacts,
    )
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.llm.generated_code import (
        LLMGeneratedCodeError,
        parse_generated_code_response,
        write_generated_code_artifacts,
    )


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class LLMGeneratedCodeTests(unittest.TestCase):
    def test_parse_generated_code_response_accepts_contract(self) -> None:
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": ["AE has one record per event."],
                "risk_points": ["Demo-only derivation."],
                "used_inputs": ["AE", "ADSL"],
                "expected_outputs": ["adae.csv"],
            }
        )

        package = parse_generated_code_response(response, expected_dataset="ADAE")

        self.assertEqual(package.dataset, "ADAE")
        self.assertIn("write.csv", package.r_code)
        self.assertEqual(package.assumptions, ["AE has one record per event."])
        self.assertEqual(package.used_inputs, ["AE", "ADSL"])

    def test_parse_generated_code_response_rejects_bad_json_and_dataset_mismatch(self) -> None:
        with self.assertRaises(LLMGeneratedCodeError):
            parse_generated_code_response("not json", expected_dataset="ADAE")

        with self.assertRaises(LLMGeneratedCodeError):
            parse_generated_code_response(
                json.dumps({"dataset": "ADSL", "r_code": "print('wrong')"}),
                expected_dataset="ADAE",
            )

    def test_parse_generated_code_response_requires_r_code_without_static_policy_checks(self) -> None:
        with self.assertRaises(LLMGeneratedCodeError):
            parse_generated_code_response(json.dumps({"dataset": "ADAE"}), expected_dataset="ADAE")

        package = parse_generated_code_response(
            json.dumps({"dataset": "ADAE", "r_code": "system('whoami')"}),
            expected_dataset="ADAE",
        )
        self.assertIn("system", package.r_code)

    def test_write_generated_code_artifacts_writes_response_code_and_parsed_package(self) -> None:
        study_dir = _workspace_dir("llm_generated_code_artifacts") / "PSY201"
        response = json.dumps(
            {
                "dataset": "ADAE",
                "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
                "assumptions": [],
                "risk_points": [],
                "used_inputs": ["AE", "ADSL"],
                "expected_outputs": ["adae.csv"],
            }
        )
        package = parse_generated_code_response(response, expected_dataset="ADAE")

        artifacts = write_generated_code_artifacts(
            study_id="PSY201",
            run_id="run_generated_code",
            study_dir=study_dir,
            package=package,
            response_text=response,
        )

        response_artifact, code_artifact, package_artifact = artifacts.as_list()
        self.assertEqual(response_artifact.kind, "llm_response")
        self.assertEqual(response_artifact.role, "audit")
        self.assertEqual(code_artifact.kind, "generated_code")
        self.assertEqual(code_artifact.role, "output")
        self.assertEqual(package_artifact.kind, "tool_log")
        self.assertTrue(Path(response_artifact.path).exists())
        self.assertTrue(Path(code_artifact.path).exists())
        self.assertTrue(Path(package_artifact.path).exists())
        self.assertIn("write.csv", Path(code_artifact.path).read_text(encoding="utf-8"))
        self.assertTrue(code_artifact.sha256.startswith("sha256:"))


if __name__ == "__main__":
    unittest.main()
