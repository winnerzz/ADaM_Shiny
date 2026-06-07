"""State ownership invariants for the LangGraph product flow."""

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
    from adam_agent.graph.gateway import GraphGateway
    from adam_agent.graph.workflow_state import project_graph_state_to_workflow
    from adam_agent.schemas.graph_state import DatasetRunState, InterruptState, StudyRunState
    from adam_agent.schemas.states import DatasetResultSummary
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.graph.gateway import GraphGateway
    from adam_agent.graph.workflow_state import project_graph_state_to_workflow
    from adam_agent.schemas.graph_state import DatasetRunState, InterruptState, StudyRunState
    from adam_agent.schemas.states import DatasetResultSummary


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class StateOwnershipTests(unittest.TestCase):
    def test_progress_uses_graph_state_not_stale_workflow_state(self) -> None:
        study_dir = _workspace_dir("state_ownership_progress") / "PSY201"
        run_id = "run_state_ownership_progress"
        run_dir = study_dir / "runs" / run_id
        run_dir.mkdir(parents=True)
        graph_state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            requested_datasets=["ADAE"],
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            current_interrupt=InterruptState(
                name="dependency_review",
                reason="Canonical dependency review is still open.",
            ),
            dependency_review_status="review_required",
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="pending",
                    result_summary=DatasetResultSummary(dataset="ADAE", status="pending"),
                )
            },
        )
        (run_dir / "graph_state.json").write_text(graph_state.model_dump_json(indent=2), encoding="utf-8")
        (run_dir / "workflow_state.json").write_text(
            json.dumps(
                {
                    "study_id": "PSY201",
                    "run_id": run_id,
                    "status": "completed",
                    "current_interrupt": None,
                    "datasets": {
                        "ADAE": {
                            "dataset": "ADAE",
                            "status": "completed",
                            "current_interrupt": None,
                        }
                    },
                },
                indent=2,
            ),
            encoding="utf-8",
        )

        progress = GraphGateway().progress_summary(study_dir=study_dir, run_id=run_id)

        self.assertEqual(progress["status"], "needs_review")
        self.assertEqual(progress["next_action"], "review_dependency_plan")
        self.assertEqual(progress["current_interrupt"]["name"], "dependency_review")
        self.assertEqual(progress["datasets"][0]["status"], "pending")

    def test_workflow_projection_is_derived_from_graph_state(self) -> None:
        study_dir = _workspace_dir("state_ownership_projection") / "PSY201"
        graph_state = StudyRunState(
            study_id="PSY201",
            run_id="run_state_ownership_projection",
            status="needs_review",
            target_datasets=["ADCM"],
            datasets={
                "ADCM": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_state_ownership_projection",
                    dataset="ADCM",
                    status="needs_review",
                    current_interrupt=InterruptState(
                        name="code_review",
                        dataset="ADCM",
                        reason="Generated code requires approval.",
                    ),
                    code_state={"status": "generated"},
                    result_summary=DatasetResultSummary(dataset="ADCM", status="needs_review"),
                )
            },
        )

        projection = project_graph_state_to_workflow(study_dir, graph_state, node="test_projection")

        self.assertEqual(projection["projection_source"], "langgraph")
        self.assertEqual(projection["workflow_control"], "graph_gateway_compatibility_shim")
        self.assertEqual(projection["status"], graph_state.status)
        self.assertEqual(projection["datasets"]["ADCM"]["status"], "needs_review")
        self.assertEqual(projection["datasets"]["ADCM"]["current_interrupt"], "code_review")
        self.assertTrue(projection["graph_state_path"].endswith("graph_state.json"))
        self.assertTrue(projection["workflow_state_path"].endswith("workflow_state.json"))

    def test_completed_dataset_progress_wins_over_old_terminal_failure_interrupt(self) -> None:
        study_dir = _workspace_dir("state_ownership_completed") / "PSY201"
        run_id = "run_state_ownership_completed"
        run_dir = study_dir / "runs" / run_id
        run_dir.mkdir(parents=True)
        graph_state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="completed",
            target_datasets=["ADAE"],
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="completed",
                    current_interrupt=InterruptState(
                        name="terminal_failure",
                        dataset="ADAE",
                        reason="Old failure projection should not reopen completed work.",
                    ),
                    execution_state={"status": "completed", "partial_output_usable": True},
                    validation_summary={"status": "pass"},
                    result_summary=DatasetResultSummary(dataset="ADAE", status="completed"),
                )
            },
        )
        (run_dir / "graph_state.json").write_text(graph_state.model_dump_json(indent=2), encoding="utf-8")

        progress = GraphGateway().progress_summary(study_dir=study_dir, run_id=run_id)

        self.assertEqual(progress["datasets"][0]["next_action"], "complete")
        self.assertEqual(progress["datasets"][0]["action_label"], "Real runtime output is available for review and compare.")


if __name__ == "__main__":
    unittest.main()
