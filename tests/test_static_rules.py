"""Tests for generic static-rule contracts."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.tools.reference_store import LocalReferenceStore
    from adam_agent.tools.artifacts import sha256_file
    from adam_agent.tools.static_rules import (
        StaticRuleError,
        StaticRulePolicy,
        assert_no_blocking_static_findings,
        run_generated_r_static_checks,
        write_static_rule_report,
        validate_static_rule_report_artifact,
    )
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.tools.reference_store import LocalReferenceStore
    from adam_agent.tools.artifacts import sha256_file
    from adam_agent.tools.static_rules import (
        StaticRuleError,
        StaticRulePolicy,
        assert_no_blocking_static_findings,
        run_generated_r_static_checks,
        write_static_rule_report,
        validate_static_rule_report_artifact,
    )


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class StaticRuleTests(unittest.TestCase):
    def test_static_rules_pass_generic_output_contract(self) -> None:
        workspace = _workspace_dir("static_rules_pass")
        code_path = workspace / "build_target.R"
        code_path.write_text(
            "dir.create('outputs', showWarnings = FALSE)\n"
            "output <- data.frame(SUBJECT_ID = '01')\n"
            "write.csv(output, 'outputs/custom.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )

        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="CUSTOM",
            code_path=code_path,
            expected_output_path="outputs/custom.csv",
            required_identifiers=["SUBJECT_ID"],
        )

        self.assertEqual(report.status, "pass")
        self.assertEqual(report.policy.required_output_paths, ("outputs/custom.csv",))
        self.assertEqual(report.policy.required_identifiers, ("SUBJECT_ID",))
        self.assertFalse(report.blocking_errors)

    def test_static_rules_block_forbidden_calls_even_when_not_dataset_specific(self) -> None:
        workspace = _workspace_dir("static_rules_forbidden")
        code_path = workspace / "build_any.R"
        code_path.write_text(
            "dir.create('outputs', showWarnings = FALSE)\n"
            "system('whoami')\n"
            "write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )

        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            expected_output_path="outputs/any.csv",
        )

        self.assertEqual(report.status, "blocked")
        self.assertTrue(any(finding.rule_id == "R_FORBIDDEN_CALL" for finding in report.blocking_errors))
        with self.assertRaises(StaticRuleError):
            assert_no_blocking_static_findings(report)

    def test_static_rules_block_missing_required_output_from_policy(self) -> None:
        workspace = _workspace_dir("static_rules_output")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/wrong.csv', row.names = FALSE)\n", encoding="utf-8")

        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            expected_output_path="outputs/expected.csv",
        )

        self.assertEqual(report.status, "blocked")
        self.assertTrue(any(finding.rule_id == "R_REQUIRED_OUTPUT_PATH" for finding in report.blocking_errors))

    def test_static_rules_required_identifier_is_policy_driven_warning(self) -> None:
        workspace = _workspace_dir("static_rules_identifier")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n", encoding="utf-8")

        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            policy=StaticRulePolicy(
                forbidden_calls=("system",),
                required_output_paths=("outputs/any.csv",),
                required_identifiers=("SUBJECT_ID",),
            ),
        )

        self.assertEqual(report.status, "warning")
        self.assertFalse(report.blocking_errors)
        self.assertTrue(any(finding.rule_id == "R_REQUIRED_IDENTIFIER_REFERENCE" for finding in report.findings))
        self.assertIn("do not prove full CDISC", report.as_dict()["non_compliance_disclaimer"])

    def test_static_rule_report_writes_policy_and_disclaimer(self) -> None:
        workspace = _workspace_dir("static_rules_report")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n", encoding="utf-8")
        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            expected_output_path="outputs/any.csv",
        )
        report_path = write_static_rule_report(report, path=workspace / "static_report.json")

        payload = json.loads(report_path.read_text(encoding="utf-8"))
        self.assertTrue(payload["implemented"])
        self.assertEqual(payload["policy"]["policy_id"], "generated_r_contract_v1")
        self.assertIn("CDISC", payload["non_compliance_disclaimer"])

    def test_static_rule_artifact_validation_rejects_incomplete_pass_report(self) -> None:
        workspace = _workspace_dir("static_rules_incomplete")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n", encoding="utf-8")
        report_path = workspace / "static_report.json"
        report_path.write_text(json.dumps({"status": "pass"}), encoding="utf-8")

        with self.assertRaisesRegex(StaticRuleError, "missing required fields"):
            validate_static_rule_report_artifact(
                report_path,
                dataset="ANY",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
            )

    def test_static_rule_artifact_validation_rejects_report_for_different_code(self) -> None:
        workspace = _workspace_dir("static_rules_bound_code")
        original_code = workspace / "build_original.R"
        current_code = workspace / "build_current.R"
        original_code.write_text(
            "write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )
        current_code.write_text(
            "system('whoami')\nwrite.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )
        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=original_code,
            expected_output_path="outputs/any.csv",
        )
        report_path = write_static_rule_report(report, path=workspace / "static_report.json")

        with self.assertRaisesRegex(StaticRuleError, "current generated R code"):
            validate_static_rule_report_artifact(
                report_path,
                dataset="ANY",
                code_path=current_code,
                code_sha256=f"sha256:{sha256_file(current_code)}",
            )

    def test_local_reference_store_searches_only_local_text_roots(self) -> None:
        workspace = _workspace_dir("reference_store")
        references = workspace / "references"
        references.mkdir()
        (references / "guide.md").write_text("ADaM timing variables require traceable source evidence.\n", encoding="utf-8")

        hits = LocalReferenceStore([references]).search("timing", limit=2)

        self.assertEqual(len(hits), 1)
        self.assertEqual(hits[0].source, "references")
        self.assertIn("timing", hits[0].snippet.lower())


if __name__ == "__main__":
    unittest.main()
