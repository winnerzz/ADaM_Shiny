"""Tests for bounded agent decision contracts."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

from pydantic import ValidationError

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.agents import AgentDecision, build_agent_audit_summary, record_agent_decision
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.agents import AgentDecision, build_agent_audit_summary, record_agent_decision


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
            risk_flags=["static_check_placeholder"],
            artifact_ids=["generated_code_adae"],
        )

        self.assertEqual(record["agent"], "code_agent")
        self.assertEqual(record["dataset"], "ADAE")
        self.assertEqual(record["outputs"]["code_path"], "runs/run/code/build_adae.R")
        self.assertEqual(record["risk_flags"], ["static_check_placeholder"])
        self.assertTrue(record["created_at"].endswith("Z"))
        AgentDecision.model_validate(record)

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
                risk_flags=["static_check_placeholder"],
            ),
            record_agent_decision(
                agent="execution_agent",
                node="execute_approved_code",
                decision="r_execution_completed",
                dataset="ADAE",
                status="completed",
                reason="R execution completed.",
            ),
        ]

        summary = build_agent_audit_summary(
            study_id="PSY201",
            run_id="run_agent_summary",
            status="completed",
            target_datasets=["ADAE"],
            datasets={"ADAE": {"status": "completed", "risk_flags": ["static_check_placeholder"]}},
            agent_decisions=decisions,
            risk_flags=["static_check_placeholder"],
        )

        self.assertEqual(summary["summary_type"], "agent_audit_summary")
        self.assertEqual(summary["summary_writer"]["agent"], "audit_agent")
        self.assertEqual(summary["decision_count"], 2)
        self.assertEqual(summary["agent_counts"]["code_agent"], 1)
        self.assertEqual(summary["datasets"]["ADAE"]["decision_count"], 2)
        self.assertEqual(summary["datasets"]["ADAE"]["status"], "completed")
        self.assertIn("static_check_placeholder", summary["datasets"]["ADAE"]["risk_flags"])
        self.assertIn("graph_state.json", summary["limitations"][0])


if __name__ == "__main__":
    unittest.main()
