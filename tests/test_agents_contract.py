"""Tests for bounded agent decision contracts."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

from pydantic import ValidationError

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.agents import (
        AgentDecision,
        AgentNodeInput,
        AgentNodeOutput,
        build_agent_audit_summary,
        build_agent_node_input,
        build_agent_node_output,
        record_agent_decision,
    )
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.agents import (
        AgentDecision,
        AgentNodeInput,
        AgentNodeOutput,
        build_agent_audit_summary,
        build_agent_node_input,
        build_agent_node_output,
        record_agent_decision,
    )


class AgentContractTests(unittest.TestCase):
    def test_record_agent_decision_returns_json_safe_contract(self) -> None:
        record = record_agent_decision(
            agent="code_agent",
            node="generate_r_code_agent",
            decision="r_code_generated",
            dataset="adae",
            status="needs_review",
            reason="Generated code is waiting for human review.",
            outputs={"code_path": "runs/run/code/build_adae.R"},
            risk_flags=["static_check_limited_scope"],
            artifact_ids=["generated_code_adae"],
        )

        self.assertEqual(record["agent"], "code_agent")
        self.assertEqual(record["dataset"], "ADAE")
        self.assertEqual(record["outputs"]["code_path"], "runs/run/code/build_adae.R")
        self.assertEqual(record["risk_flags"], ["static_check_limited_scope"])
        self.assertTrue(record["created_at"].endswith("Z"))
        AgentDecision.model_validate(record)

    def test_agent_decision_normalizes_dataset_and_requires_utc_timestamp(self) -> None:
        record = AgentDecision.model_validate(
            {
                "agent": "code_agent",
                "node": "generate_r_code_agent",
                "decision": "r_code_generated",
                "dataset": "adae",
                "status": "needs_review",
                "created_at": "2026-05-30T00:00:00Z",
            }
        )

        self.assertEqual(record.dataset, "ADAE")
        with self.assertRaises(ValidationError):
            AgentDecision.model_validate(
                {
                    "agent": "code_agent",
                    "node": "generate_r_code_agent",
                    "decision": "r_code_generated",
                    "status": "needs_review",
                    "created_at": "2026-05-30T00:00:00+08:00",
                }
            )

    def test_agent_node_input_packages_explicit_context(self) -> None:
        package = build_agent_node_input(
            agent="spec_agent",
            node="draft_spec_agent",
            study_id="PSY201",
            run_id="run_agent_io",
            dataset="adae",
            task="Draft a review-required spec from approved evidence when no input spec exists.",
            inputs={"spec_source": "missing_input_spec"},
            artifact_ids=["evidence_bundle_adae"],
            risk_flags=["missing_input_spec"],
            evidence_bundle_id="bundle_adae",
            reference_query_ids=["query_cdisc_001"],
        )

        self.assertEqual(package["agent"], "spec_agent")
        self.assertEqual(package["dataset"], "ADAE")
        self.assertEqual(package["inputs"]["spec_source"], "missing_input_spec")
        self.assertEqual(package["artifact_ids"], ["evidence_bundle_adae"])
        self.assertEqual(package["reference_query_ids"], ["query_cdisc_001"])
        AgentNodeInput.model_validate(package)

    def test_agent_node_output_creates_matching_audit_decision(self) -> None:
        package = build_agent_node_output(
            agent="code_agent",
            node="generate_r_code_agent",
            study_id="PSY201",
            run_id="run_agent_io",
            dataset="adae",
            status="needs_review",
            decision="r_code_generated",
            reason="Generated code is waiting for human review.",
            outputs={"code_path": "runs/run_agent_io/code/build_adae.R"},
            artifact_ids=["generated_code_adae"],
            risk_flags=["static_check_limited_scope"],
        )

        self.assertEqual(package["agent"], "code_agent")
        self.assertEqual(package["dataset"], "ADAE")
        self.assertEqual(package["decision"], "r_code_generated")
        self.assertEqual(len(package["agent_decisions"]), 1)
        decision = package["agent_decisions"][0]
        self.assertEqual(decision["agent"], "code_agent")
        self.assertEqual(decision["node"], "generate_r_code_agent")
        self.assertEqual(decision["dataset"], "ADAE")
        self.assertEqual(decision["artifact_ids"], ["generated_code_adae"])
        AgentNodeOutput.model_validate(package)

    def test_agent_node_output_rejects_cross_agent_decisions(self) -> None:
        wrong_decision = record_agent_decision(
            agent="execution_agent",
            node="generate_r_code_agent",
            decision="r_execution_completed",
            dataset="ADAE",
            status="completed",
        )

        with self.assertRaises(ValidationError):
            build_agent_node_output(
                agent="code_agent",
                node="generate_r_code_agent",
                study_id="PSY201",
                run_id="run_agent_io",
                dataset="ADAE",
                status="needs_review",
                decision="r_code_generated",
                agent_decisions=[wrong_decision],
            )

    def test_agent_node_output_rejects_cross_node_decisions(self) -> None:
        wrong_decision = record_agent_decision(
            agent="code_agent",
            node="execute_approved_code",
            decision="r_code_generated",
            dataset="ADAE",
            status="needs_review",
        )

        with self.assertRaises(ValidationError):
            build_agent_node_output(
                agent="code_agent",
                node="generate_r_code_agent",
                study_id="PSY201",
                run_id="run_agent_io",
                dataset="ADAE",
                status="needs_review",
                decision="r_code_generated",
                agent_decisions=[wrong_decision],
            )

    def test_agent_node_output_rejects_cross_dataset_decisions(self) -> None:
        wrong_decision = record_agent_decision(
            agent="code_agent",
            node="generate_r_code_agent",
            decision="r_code_generated",
            dataset="ADSL",
            status="needs_review",
        )

        with self.assertRaises(ValidationError):
            build_agent_node_output(
                agent="code_agent",
                node="generate_r_code_agent",
                study_id="PSY201",
                run_id="run_agent_io",
                dataset="ADAE",
                status="needs_review",
                decision="r_code_generated",
                agent_decisions=[wrong_decision],
            )

    def test_study_level_agent_node_output_rejects_dataset_decisions(self) -> None:
        dataset_decision = record_agent_decision(
            agent="dependency_agent",
            node="plan_dependencies",
            decision="dependency_plan_prepared",
            dataset="ADAE",
            status="needs_review",
        )

        with self.assertRaises(ValidationError):
            build_agent_node_output(
                agent="dependency_agent",
                node="plan_dependencies",
                study_id="PSY201",
                run_id="run_agent_io",
                status="needs_review",
                decision="dependency_plan_prepared",
                agent_decisions=[dataset_decision],
            )

    def test_agent_decision_rejects_unknown_agent_role(self) -> None:
        with self.assertRaises(ValidationError):
            AgentDecision.model_validate(
                {
                    "agent": "free_form_agent",
                    "node": "anything",
                    "decision": "mutate_files",
                    "status": "running",
                    "created_at": "2026-05-30T00:00:00Z",
                }
            )

    def test_agent_audit_summary_groups_decisions_by_dataset(self) -> None:
        decisions = [
            record_agent_decision(
                agent="code_agent",
                node="code_generation",
                decision="r_code_generated",
                dataset="ADAE",
                status="needs_review",
                reason="Generated code requires review.",
                risk_flags=["static_check_limited_scope"],
            ),
            record_agent_decision(
                agent="execution_agent",
                node="execute_approved_code",
                decision="r_execution_completed",
                dataset="ADAE",
                status="completed",
                reason="R execution completed.",
            ),
            record_agent_decision(
                agent="validation_agent",
                node="compare_reference_output",
                decision="reference_compare_recorded",
                dataset="ADAE",
                status="differences",
                reason="Reference comparison was recorded as limited validation evidence.",
                risk_flags=["reference_compare_limited_scope"],
            ),
            record_agent_decision(
                agent="diagnosis_repair_agent",
                node="terminal_failure_review",
                decision="terminal_failure_triage_recorded",
                dataset="ADAE",
                status="needs_review",
                reason="Terminal-failure triage selected a controlled follow-up action.",
                risk_flags=["terminal_failure_triage_limited_scope"],
            ),
        ]

        summary = build_agent_audit_summary(
            study_id="PSY201",
            run_id="run_agent_summary",
            status="completed",
            target_datasets=["ADAE"],
            datasets={
                "ADAE": {
                    "status": "completed",
                    "risk_flags": [
                        "static_check_limited_scope",
                        "reference_compare_limited_scope",
                        "terminal_failure_triage_limited_scope",
                    ],
                }
            },
            agent_decisions=decisions,
            agent_node_inputs=[
                build_agent_node_input(
                    agent="code_agent",
                    node="code_generation",
                    study_id="PSY201",
                    run_id="run_agent_summary",
                    dataset="ADAE",
                    task="Generate review-required R code from the approved ADaM spec.",
                    artifact_ids=["input_spec_adae"],
                )
            ],
            agent_node_outputs=[
                build_agent_node_output(
                    agent="code_agent",
                    node="code_generation",
                    study_id="PSY201",
                    run_id="run_agent_summary",
                    dataset="ADAE",
                    status="needs_review",
                    decision="r_code_generated",
                    reason="Generated code requires review.",
                    artifact_ids=["generated_code_adae"],
                    risk_flags=["static_check_limited_scope"],
                )
            ],
            risk_flags=[
                "static_check_limited_scope",
                "reference_compare_limited_scope",
                "terminal_failure_triage_limited_scope",
            ],
        )

        self.assertEqual(summary["summary_type"], "agent_audit_summary")
        self.assertEqual(summary["summary_writer"]["agent"], "audit_agent")
        self.assertEqual(summary["decision_count"], 4)
        self.assertEqual(summary["agent_node_input_count"], 1)
        self.assertEqual(summary["agent_node_output_count"], 1)
        self.assertEqual(summary["agent_node_counts"]["code_agent"], 1)
        self.assertEqual(summary["agent_counts"]["code_agent"], 1)
        self.assertEqual(summary["agent_counts"]["validation_agent"], 1)
        self.assertEqual(summary["agent_counts"]["diagnosis_repair_agent"], 1)
        self.assertEqual(summary["datasets"]["ADAE"]["decision_count"], 4)
        self.assertEqual(summary["datasets"]["ADAE"]["agent_node_input_count"], 1)
        self.assertEqual(summary["datasets"]["ADAE"]["agent_node_output_count"], 1)
        self.assertEqual(summary["datasets"]["ADAE"]["agent_node_counts"]["code_agent"], 1)
        self.assertEqual(summary["datasets"]["ADAE"]["latest_node_outputs"][0]["decision"], "r_code_generated")
        self.assertEqual(summary["datasets"]["ADAE"]["status"], "completed")
        self.assertIn("static_check_limited_scope", summary["datasets"]["ADAE"]["risk_flags"])
        self.assertIn("reference_compare_limited_scope", summary["datasets"]["ADAE"]["risk_flags"])
        self.assertIn("terminal_failure_triage_limited_scope", summary["datasets"]["ADAE"]["risk_flags"])
        self.assertIn("graph_state.json", summary["limitations"][0])

    def test_agent_audit_summary_counts_invalid_dataset_node_io(self) -> None:
        summary = build_agent_audit_summary(
            study_id="PSY201",
            run_id="run_agent_summary_invalid_io",
            status="needs_review",
            target_datasets=["ADAE"],
            datasets={
                "ADAE": {
                    "status": "needs_review",
                    "agent_node_inputs": [
                        {
                            "agent": "code_agent",
                            "node": "code_generation",
                            "study_id": "PSY201",
                            "run_id": "run_agent_summary_invalid_io",
                            "dataset": "ADAE",
                            "task": "Generate review-required R code.",
                        },
                        {"agent": "free_form_agent", "node": "mutate_anything"},
                    ],
                    "agent_node_outputs": [
                        {
                            "agent": "code_agent",
                            "node": "code_generation",
                            "study_id": "PSY201",
                            "run_id": "run_agent_summary_invalid_io",
                            "dataset": "ADAE",
                            "status": "needs_review",
                            "decision": "r_code_generated",
                        },
                        {"agent": "code_agent", "node": "code_generation", "status": "needs_review"},
                    ],
                }
            },
        )

        dataset_summary = summary["datasets"]["ADAE"]
        self.assertEqual(dataset_summary["agent_node_input_count"], 1)
        self.assertEqual(dataset_summary["agent_node_output_count"], 1)
        self.assertEqual(dataset_summary["invalid_agent_node_input_count"], 1)
        self.assertEqual(dataset_summary["invalid_agent_node_output_count"], 1)


if __name__ == "__main__":
    unittest.main()
