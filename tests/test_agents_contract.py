"""Tests for bounded agent decision contracts."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

from pydantic import ValidationError

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.agents import AgentDecision, record_agent_decision
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.agents import AgentDecision, record_agent_decision


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


if __name__ == "__main__":
    unittest.main()
