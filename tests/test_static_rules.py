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
        load_static_rule_pack,
        run_generated_r_static_checks,
        validate_static_rule_pack_payload,
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
        load_static_rule_pack,
        run_generated_r_static_checks,
        validate_static_rule_pack_payload,
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
            required_identifier_source_id="approved_spec:CUSTOM",
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
        blocked = [finding for finding in report.blocking_errors if finding.rule_id == "R_FORBIDDEN_CALL"]
        self.assertTrue(blocked)
        self.assertEqual(blocked[0].category, "execution_boundary")
        self.assertEqual(blocked[0].source_type, "system_contract")
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
        blocked = [finding for finding in report.blocking_errors if finding.rule_id == "R_REQUIRED_OUTPUT_PATH"]
        self.assertTrue(blocked)
        self.assertEqual(blocked[0].category, "artifact_contract")
        self.assertEqual(blocked[0].source_type, "system_contract")

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
                required_identifier_source_id="approved_spec:ANY",
            ),
        )

        self.assertEqual(report.status, "warning")
        self.assertFalse(report.blocking_errors)
        identifier_findings = [
            finding for finding in report.findings if finding.rule_id == "R_REQUIRED_IDENTIFIER_REFERENCE"
        ]
        self.assertTrue(identifier_findings)
        self.assertEqual(identifier_findings[0].category, "spec_contract")
        self.assertEqual(identifier_findings[0].source_type, "approved_spec")
        self.assertEqual(identifier_findings[0].source_id, "approved_spec:ANY")
        self.assertIn("do not prove full CDISC", report.as_dict()["non_compliance_disclaimer"])

    def test_static_rules_require_source_id_for_approved_spec_identifier_policy(self) -> None:
        workspace = _workspace_dir("static_rules_identifier_source")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n", encoding="utf-8")

        with self.assertRaisesRegex(StaticRuleError, "must include source_id"):
            run_generated_r_static_checks(
                study_id="STUDY",
                run_id="run_static",
                dataset="ANY",
                code_path=code_path,
                expected_output_path="outputs/any.csv",
                required_identifiers=["SUBJECT_ID"],
            )

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
        self.assertEqual(payload["policy"]["rule_governance"]["engine_scope"], "generic_contracts_only")
        self.assertIn("source-backed rule packs", payload["policy"]["rule_governance"]["demo_observation_policy"])
        self.assertIn("CDISC", payload["non_compliance_disclaimer"])

    def test_static_rule_report_finding_metadata_prevents_anonymous_rules(self) -> None:
        workspace = _workspace_dir("static_rules_metadata")
        code_path = workspace / "build_any.R"
        code_path.write_text("system('whoami')\n", encoding="utf-8")

        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            expected_output_path="outputs/expected.csv",
        )
        payload = report.as_dict()

        self.assertTrue(payload["findings"])
        for finding in payload["findings"]:
            self.assertIn(finding["category"], {"artifact_contract", "execution_boundary", "spec_contract", "standards_pack"})
            self.assertIn(finding["source_type"], {"system_contract", "approved_spec", "standards_pack", "user_policy"})
        self.assertFalse(any("ADAE" in json.dumps(finding) for finding in payload["findings"]))

    def test_static_rule_engine_does_not_embed_demo_or_dataset_specific_logic(self) -> None:
        engine_source = (ROOT / "src" / "adam_agent" / "tools" / "static_rules.py").read_text(encoding="utf-8")

        forbidden_engine_tokens = {
            "PSY201",
            "ADAE",
            "ADSL",
            "TRTEMFL",
            "RELGR1",
            "AETERM",
            "TRTSDT",
            "TRTEDT",
            "USUBJID",
        }
        found = sorted(token for token in forbidden_engine_tokens if token in engine_source)
        self.assertEqual(found, [], "Generic static-rule engine must not embed demo or dataset-specific ADaM logic.")

    def test_static_rule_pack_admission_accepts_source_backed_rules(self) -> None:
        payload = {
            "pack_id": "company_standards_v1",
            "source": "company-standard",
            "version": "2026.05",
            "scope": ["all-adam"],
            "rules": [
                {
                    "rule_id": "COMPANY_TRACEABILITY_001",
                    "description": "Generated outputs must be traceable to an approved contract.",
                    "severity": "warning",
                    "source": "company-standard",
                    "version": "2026.05",
                    "scope": ["all-adam"],
                    "evidence": "references/company/traceability.md#rule-001",
                }
            ],
        }

        rule_pack = validate_static_rule_pack_payload(payload)

        self.assertEqual(rule_pack.pack_id, "company_standards_v1")
        self.assertEqual(rule_pack.rules[0].source_id, "company-standard:2026.05:COMPANY_TRACEABILITY_001")

    def test_static_rule_pack_admission_rejects_rules_without_evidence(self) -> None:
        payload = {
            "pack_id": "candidate_rules",
            "source": "implementation-note",
            "version": "draft",
            "scope": ["all-adam"],
            "rules": [
                {
                    "rule_id": "CANDIDATE_RULE",
                    "description": "A demo observation that has not been sourced.",
                    "severity": "warning",
                    "source": "implementation-note",
                    "version": "draft",
                    "scope": ["all-adam"],
                }
            ],
        }

        with self.assertRaisesRegex(StaticRuleError, "must include evidence"):
            validate_static_rule_pack_payload(payload)

    def test_static_rule_pack_admission_rejects_non_string_provenance_fields(self) -> None:
        payload = {
            "pack_id": "standards_pack",
            "source": "company-standard",
            "version": "1",
            "scope": ["all-adam"],
            "rules": [
                {
                    "rule_id": "RULE_001",
                    "description": "Non-string evidence should not be stringified.",
                    "severity": "warning",
                    "source": "company-standard",
                    "version": "1",
                    "scope": ["all-adam"],
                    "evidence": {"path": "references/company/rules.md", "anchor": "rule-001"},
                }
            ],
        }

        with self.assertRaisesRegex(StaticRuleError, "evidence as a non-empty string"):
            validate_static_rule_pack_payload(payload)

    def test_static_rule_pack_admission_rejects_invalid_severity_and_scope(self) -> None:
        missing_scope = {
            "pack_id": "standards_pack",
            "source": "company-standard",
            "version": "1",
            "rules": [],
        }
        with self.assertRaisesRegex(StaticRuleError, "scope"):
            validate_static_rule_pack_payload(missing_scope)

        invalid_severity = {
            "pack_id": "standards_pack",
            "source": "company-standard",
            "version": "1",
            "scope": ["all-adam"],
            "rules": [
                {
                    "rule_id": "RULE_001",
                    "description": "Invalid severity should not enter the rule engine.",
                    "severity": "critical",
                    "source": "company-standard",
                    "version": "1",
                    "scope": ["all-adam"],
                    "evidence": "references/company/rules.md#rule-001",
                }
            ],
        }
        with self.assertRaisesRegex(StaticRuleError, "invalid severity"):
            validate_static_rule_pack_payload(invalid_severity)

    def test_static_rule_pack_admission_rejects_duplicate_rules_and_non_string_scope(self) -> None:
        duplicate_rules = {
            "pack_id": "standards_pack",
            "source": "company-standard",
            "version": "1",
            "scope": ["all-adam"],
            "rules": [
                {
                    "rule_id": "RULE_001",
                    "description": "First rule.",
                    "severity": "warning",
                    "source": "company-standard",
                    "version": "1",
                    "scope": ["all-adam"],
                    "evidence": "references/company/rules.md#rule-001",
                },
                {
                    "rule_id": "RULE_001",
                    "description": "Duplicate rule.",
                    "severity": "warning",
                    "source": "company-standard",
                    "version": "1",
                    "scope": ["all-adam"],
                    "evidence": "references/company/rules.md#rule-001b",
                },
            ],
        }
        with self.assertRaisesRegex(StaticRuleError, "duplicate rule_id"):
            validate_static_rule_pack_payload(duplicate_rules)

        non_string_scope = {
            "pack_id": "standards_pack",
            "source": "company-standard",
            "version": "1",
            "scope": ["all-adam"],
            "rules": [
                {
                    "rule_id": "RULE_002",
                    "description": "Scope must be strings.",
                    "severity": "warning",
                    "source": "company-standard",
                    "version": "1",
                    "scope": [{"dataset": "ANY"}],
                    "evidence": "references/company/rules.md#rule-002",
                }
            ],
        }
        with self.assertRaisesRegex(StaticRuleError, "scope values must be strings"):
            validate_static_rule_pack_payload(non_string_scope)

    def test_static_rule_pack_loader_reads_json_file(self) -> None:
        workspace = _workspace_dir("static_rule_pack_loader")
        pack_path = workspace / "rules.json"
        pack_path.write_text(
            json.dumps(
                {
                    "pack_id": "local_policy",
                    "source": "local-policy",
                    "version": "1",
                    "scope": ["generated-r"],
                    "rules": [
                        {
                            "rule_id": "LOCAL_POLICY_001",
                            "description": "Local policy example with explicit evidence.",
                            "severity": "info",
                            "source": "local-policy",
                            "version": "1",
                            "scope": ["generated-r"],
                            "evidence": "references/local/policy.md#local-policy-001",
                            "enabled": False,
                        }
                    ],
                }
            ),
            encoding="utf-8",
        )

        rule_pack = load_static_rule_pack(pack_path)

        self.assertEqual(rule_pack.rules[0].severity, "info")
        self.assertFalse(rule_pack.rules[0].enabled)

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

    def test_static_rule_artifact_validation_rejects_anonymous_findings(self) -> None:
        workspace = _workspace_dir("static_rules_anonymous_finding")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n", encoding="utf-8")
        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            expected_output_path="outputs/missing.csv",
        )
        payload = report.as_dict()
        payload["status"] = "warning"
        payload["blocking_errors"] = []
        payload["findings"] = [{"rule_id": "R_PATCHED_DEMO_RULE", "severity": "warning"}]
        report_path = workspace / "static_report.json"
        report_path.write_text(json.dumps(payload), encoding="utf-8")

        with self.assertRaisesRegex(StaticRuleError, "rule-governance fields"):
            validate_static_rule_report_artifact(
                report_path,
                dataset="ANY",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
            )

    def test_static_rule_artifact_validation_rejects_invalid_finding_enums(self) -> None:
        workspace = _workspace_dir("static_rules_invalid_enum")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n", encoding="utf-8")
        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            expected_output_path="outputs/any.csv",
        )
        payload = report.as_dict()
        payload["status"] = "warning"
        payload["findings"] = [
            {
                "rule_id": "R_FAKE",
                "severity": "warning",
                "category": "demo_patch",
                "source_type": "system_contract",
                "confidence": "high",
                "evidence": "demo observation",
                "source_id": "",
            }
        ]
        report_path = workspace / "static_report.json"
        report_path.write_text(json.dumps(payload), encoding="utf-8")

        with self.assertRaisesRegex(StaticRuleError, "invalid category"):
            validate_static_rule_report_artifact(
                report_path,
                dataset="ANY",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
            )

    def test_static_rule_artifact_validation_rejects_non_system_source_without_id(self) -> None:
        workspace = _workspace_dir("static_rules_source_id")
        code_path = workspace / "build_any.R"
        code_path.write_text("write.csv(data.frame(ID = '01'), 'outputs/any.csv', row.names = FALSE)\n", encoding="utf-8")
        report = run_generated_r_static_checks(
            study_id="STUDY",
            run_id="run_static",
            dataset="ANY",
            code_path=code_path,
            expected_output_path="outputs/any.csv",
        )
        payload = report.as_dict()
        payload["status"] = "warning"
        payload["findings"] = [
            {
                "rule_id": "R_SPEC_VISIBILITY",
                "severity": "warning",
                "category": "spec_contract",
                "source_type": "approved_spec",
                "confidence": "medium",
                "evidence": "SUBJECT_ID",
                "source_id": "",
            }
        ]
        report_path = workspace / "static_report.json"
        report_path.write_text(json.dumps(payload), encoding="utf-8")

        with self.assertRaisesRegex(StaticRuleError, "has no source_id"):
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
