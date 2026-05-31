"""Tests for the LangGraph-2 gateway and workflow projection."""

from __future__ import annotations

import json
import inspect
import sqlite3
import sys
import unittest
import uuid
from datetime import UTC, datetime
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.api.models import RunPlanRequest
    from adam_agent.api.service import prepare_run_plan
    from adam_agent.graph.checkpointing import build_checkpointer, default_sqlite_checkpointer_path, describe_checkpointer
    from adam_agent.graph.gateway import GraphGateway, _generation_quality_from_dataset_result
    from adam_agent.graph.output_quality import dataset_output_quality
    from adam_agent.graph.workflow_state import input_fingerprint, workflow_projection_consistency
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.graph_state import DatasetRunState, HumanCommand, InterruptState, StudyRunState
    from adam_agent.schemas.routing import FailureRecord
    from adam_agent.tools.artifacts import sha256_file
    from adam_agent.tools.static_rules import run_generated_r_static_checks, write_static_rule_report
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.api.models import RunPlanRequest
    from adam_agent.api.service import prepare_run_plan
    from adam_agent.graph.checkpointing import build_checkpointer, default_sqlite_checkpointer_path, describe_checkpointer
    from adam_agent.graph.gateway import GraphGateway, _generation_quality_from_dataset_result
    from adam_agent.graph.output_quality import dataset_output_quality
    from adam_agent.graph.workflow_state import input_fingerprint, workflow_projection_consistency
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.graph_state import DatasetRunState, HumanCommand, InterruptState, StudyRunState
    from adam_agent.schemas.routing import FailureRecord
    from adam_agent.tools.artifacts import sha256_file
    from adam_agent.tools.static_rules import run_generated_r_static_checks, write_static_rule_report


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


def _write_static_check_for_code(study_dir: Path, run_id: str, dataset: str, code_path: Path) -> tuple[Path, str]:
    target = dataset.strip().upper()
    static_path = study_dir / "runs" / run_id / "static_checks" / f"{target.lower()}_static_check.json"
    report = run_generated_r_static_checks(
        study_id=study_dir.name,
        run_id=run_id,
        dataset=target,
        code_path=code_path,
        expected_output_path=f"outputs/{target.lower()}.csv",
    )
    write_static_rule_report(report, path=static_path)
    return static_path, f"sha256:{sha256_file(static_path)}"


class GraphGatewayTests(unittest.TestCase):
    def test_output_quality_classification_matrix(self) -> None:
        cases = [
            (
                "real_runtime_output",
                dataset_output_quality(
                    status="completed",
                    execution_state={"status": "completed", "partial_output_usable": True},
                    validation_summary={"status": "pass"},
                ),
                True,
            ),
            (
                "structural_stub",
                dataset_output_quality(
                    status="completed_stub",
                    execution_state={"status": "completed", "partial_output_usable": True},
                    validation_summary={"status": "structural_stub_pass"},
                ),
                False,
            ),
            (
                "not_real_derivation",
                dataset_output_quality(
                    status="completed",
                    execution_state={
                        "status": "completed",
                        "partial_output_usable": True,
                        "generation_quality": {"not_real_derivation": True},
                    },
                    validation_summary={"status": "pass"},
                ),
                False,
            ),
            (
                "terminal_failure",
                dataset_output_quality(
                    status="completed",
                    execution_state={"status": "completed"},
                    validation_summary={"status": "fail", "terminal_failure": True, "partial_output_usable": False},
                ),
                False,
            ),
        ]
        for expected_status, quality, expected_eligible in cases:
            with self.subTest(expected_status=expected_status):
                self.assertEqual(quality["quality_status"], expected_status)
                self.assertEqual(quality["runtime_dependency_eligible"], expected_eligible)

    def test_study_output_quality_rollup_distinguishes_review_only_completion(self) -> None:
        from adam_agent.graph.output_quality import study_output_quality_rollup

        rollup = study_output_quality_rollup(
            [
                {
                    "dataset": "ADSL",
                    "output_quality": dataset_output_quality(
                        status="completed",
                        execution_state={
                            "status": "completed",
                            "generation_quality": {"not_real_derivation": True},
                        },
                    ),
                },
                {
                    "dataset": "ADAE",
                    "output_quality": dataset_output_quality(
                        status="completed_stub",
                        validation_summary={"status": "structural_stub_pass"},
                    ),
                },
            ],
            target_datasets=["ADSL", "ADAE"],
        )

        self.assertEqual(rollup["completion_quality"], "review_only_complete")
        self.assertEqual(rollup["review_only_outputs"], 2)
        self.assertEqual(rollup["real_runtime_outputs"], 0)
        self.assertEqual(rollup["runtime_dependency_eligible_outputs"], 0)

    def test_generation_quality_marks_only_mock_signals_as_not_real(self) -> None:
        cases = [
            (
                "real_openai_compatible",
                {
                    "llm_provider": "openai-compatible",
                    "llm_model": "gpt-5.5",
                    "provider_alias": "custom-http",
                    "transport": "openai-compatible",
                },
                {"provider": "openai-compatible", "model": "gpt-5.5"},
                False,
            ),
            (
                "mock_alias",
                {
                    "llm_provider": "openai-compatible",
                    "llm_model": "gpt-5.5",
                    "provider_alias": "mock",
                    "transport": "openai-compatible",
                },
                {"provider": "openai-compatible", "model": "gpt-5.5"},
                True,
            ),
            (
                "mock_transport",
                {
                    "llm_provider": "openai-compatible",
                    "llm_model": "gpt-5.5",
                    "provider_alias": "custom-http",
                    "transport": "mock",
                },
                {"provider": "openai-compatible", "model": "gpt-5.5"},
                True,
            ),
        ]
        for name, result, provider_config, expected_not_real in cases:
            with self.subTest(name=name):
                quality = _generation_quality_from_dataset_result(result, llm_provider=provider_config)
                self.assertEqual(quality["not_real_derivation"], expected_not_real)
                self.assertEqual(quality["llm_provider"], "openai-compatible")
                self.assertEqual(quality["llm_model"], "gpt-5.5")

    def test_gateway_blocks_legacy_llm_run_to_completion_with_projection(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_legacy_block") / "PSY201"
        study_dir.mkdir(parents=True)

        projection = GraphGateway().block_legacy_run_to_completion(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_legacy_block",
            requested_datasets=["adae"],
            execution_mode="llm_downstream_provider",
        )

        workflow_path = study_dir / "runs" / "run_legacy_block" / "workflow_state.json"
        workflow_state = json.loads(workflow_path.read_text(encoding="utf-8"))
        self.assertEqual(projection["status"], "blocked")
        self.assertEqual(workflow_state["current_interrupt"], "split_flow_required")
        self.assertEqual(workflow_state["requested_datasets"], ["ADAE"])
        self.assertEqual(workflow_state["workflow_control"], "legacy_run_to_completion_compatibility_shim")
        self.assertEqual(workflow_state["legacy_endpoint"], "POST /runs")
        self.assertTrue(workflow_state["product_flow_required"])
        self.assertIsNone(workflow_state["graph_state_path"])

    def test_gateway_runs_legacy_stub_to_completion_with_projection(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_legacy_stub") / "PSY201"
        study_dir.mkdir(parents=True)

        result = GraphGateway().run_legacy_to_completion(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_legacy_stub",
            target_datasets=["ADAE"],
            execution_mode="stub",
        )

        workflow_path = study_dir / "runs" / "run_legacy_stub" / "workflow_state.json"
        workflow_state = json.loads(workflow_path.read_text(encoding="utf-8"))
        self.assertEqual(result.graph_result["status"], "completed")
        self.assertEqual(result.workflow_projection["workflow_control"], "legacy_run_to_completion_compatibility_shim")
        self.assertEqual(workflow_state["status"], "completed")
        self.assertEqual(workflow_state["legacy_endpoint"], "POST /runs")
        self.assertFalse(workflow_state["product_flow_required"])
        self.assertIsNone(workflow_state["graph_state_path"])
        self.assertTrue(workflow_state["workflow_state_path"].endswith("runs/run_legacy_stub/workflow_state.json"))

    def test_gateway_dependency_plan_writes_consistent_workflow_projection(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_plan") / "PSY201"
        study_dir.mkdir(parents=True)

        result = GraphGateway().start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_gateway_plan",
            target_datasets=["ADAE"],
        )

        workflow_path = study_dir / "runs" / "run_lg2_gateway_plan" / "workflow_state.json"
        workflow_state = json.loads(workflow_path.read_text(encoding="utf-8"))
        consistency = workflow_projection_consistency(workflow_state, result.graph_state)

        self.assertEqual(result.graph_state.study_id, "PSY201")
        self.assertEqual(result.graph_state.run_id, "run_lg2_gateway_plan")
        self.assertEqual(result.graph_state.dependency_review_status, "review_required")
        self.assertEqual(result.graph_state.current_interrupt.name, "dependency_review")
        self.assertEqual(result.graph_state.target_datasets, ["ADAE"])
        self.assertIn("ADAE", result.graph_state.datasets)
        self.assertEqual(result.graph_state.agent_decisions[0]["agent"], "dependency_agent")
        self.assertEqual(result.graph_state.agent_decisions[0]["decision"], "dependency_plan_prepared")
        self.assertEqual(result.graph_state.agent_decisions[0]["outputs"]["dependency_review_status"], "review_required")
        self.assertEqual(result.graph_state.agent_node_inputs[0]["agent"], "dependency_agent")
        self.assertEqual(result.graph_state.agent_node_inputs[0]["node"], "dependency_plan")
        self.assertIsNone(result.graph_state.agent_node_inputs[0]["dataset"])
        self.assertEqual(
            result.graph_state.agent_node_inputs[0]["task"],
            "Prepare the study dependency plan and decide whether dependency review is needed.",
        )
        self.assertEqual(result.graph_state.agent_node_outputs[0]["agent"], "dependency_agent")
        self.assertEqual(result.graph_state.agent_node_outputs[0]["decision"], "dependency_plan_prepared")
        self.assertIsNone(result.graph_state.agent_node_outputs[0]["dataset"])
        self.assertEqual(
            result.graph_state.agent_node_outputs[0]["agent_decisions"][0],
            result.graph_state.agent_decisions[0],
        )
        self.assertEqual(result.graph_state.agent_audit_summary["summary_writer"]["agent"], "audit_agent")
        self.assertEqual(result.graph_state.agent_audit_summary["agent_counts"]["dependency_agent"], 1)
        self.assertTrue((study_dir / "runs" / "run_lg2_gateway_plan" / "audit" / "agent_summary.json").exists())
        persisted_state = json.loads(
            (study_dir / "runs" / "run_lg2_gateway_plan" / "graph_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(persisted_state["agent_node_inputs"][0]["agent"], "dependency_agent")
        self.assertEqual(persisted_state["agent_node_outputs"][0]["decision"], "dependency_plan_prepared")
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["workflow_control"], "graph_gateway_compatibility_shim")
        self.assertEqual(
            workflow_state["graph_state_path"],
            str((study_dir / "runs" / "run_lg2_gateway_plan" / "graph_state.json").as_posix()),
        )
        self.assertEqual(
            workflow_state["workflow_state_path"],
            str((study_dir / "runs" / "run_lg2_gateway_plan" / "workflow_state.json").as_posix()),
        )
        self.assertEqual(result.workflow_projection["workflow_control"], workflow_state["workflow_control"])
        self.assertEqual(result.workflow_projection["graph_state_path"], workflow_state["graph_state_path"])
        self.assertEqual(result.workflow_projection["workflow_state_path"], workflow_state["workflow_state_path"])
        runtime_persistence = result.graph_state.runtime_persistence
        self.assertEqual(runtime_persistence["source_of_truth"], "graph_state_json")
        self.assertEqual(runtime_persistence["langgraph_checkpointer_type"], "InMemorySaver")
        self.assertEqual(runtime_persistence["langgraph_checkpointer_backend"], "memory")
        self.assertFalse(runtime_persistence["langgraph_checkpointer_persistent"])
        self.assertFalse(runtime_persistence["native_interrupt_resume"])
        self.assertEqual(runtime_persistence["restart_recovery_source"], "graph_state_json")
        self.assertTrue(runtime_persistence["checkpoint_ledger_path"].endswith("runs/run_lg2_gateway_plan/graph_checkpoints.sqlite"))
        self.assertIn("not a LangGraph SQLite checkpointer", " ".join(runtime_persistence["notes"]))
        self.assertEqual(workflow_state["runtime_persistence"], runtime_persistence)
        self.assertEqual(workflow_state["current_interrupt"], "dependency_review")
        self.assertEqual(workflow_state["agent_decisions"][0]["agent"], "dependency_agent")
        self.assertEqual(workflow_state["agent_audit_summary"]["summary_type"], "agent_audit_summary")
        self.assertTrue(
            any(artifact["artifact_id"] == "agent_summary_psy201_run_lg2_gateway_plan" for artifact in workflow_state["artifacts"])
        )
        self.assertTrue(consistency["consistent"], consistency["mismatches"])

    def test_gateway_checkpoint_can_be_read_from_same_graph_instance(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_restart") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()

        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_gateway_restart",
            target_datasets=["ADAE", "ADCM"],
        )
        checkpoint_state = gateway.get_state(study_id="PSY201", run_id="run_lg2_gateway_restart")

        self.assertEqual(checkpoint_state["study_id"], "PSY201")
        self.assertEqual(checkpoint_state["run_id"], "run_lg2_gateway_restart")
        self.assertEqual(checkpoint_state["target_datasets"], ["ADAE", "ADCM"])
        self.assertEqual(checkpoint_state["current_interrupt"], "dependency_review")

    def test_gateway_progress_reports_runtime_persistence_boundary(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_runtime_persistence") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()

        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_runtime_persistence",
            target_datasets=["ADAE"],
        )
        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_runtime_persistence")
        graph_state = json.loads(
            (study_dir / "runs" / "run_lg2_runtime_persistence" / "graph_state.json").read_text(encoding="utf-8")
        )
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_runtime_persistence" / "workflow_state.json").read_text(encoding="utf-8")
        )

        runtime_persistence = progress["runtime_persistence"]
        self.assertEqual(runtime_persistence, graph_state["runtime_persistence"])
        self.assertEqual(runtime_persistence, workflow_state["runtime_persistence"])
        self.assertEqual(runtime_persistence["source_of_truth"], "graph_state_json")
        self.assertEqual(runtime_persistence["langgraph_checkpointer_type"], "InMemorySaver")
        self.assertEqual(runtime_persistence["langgraph_checkpointer_backend"], "memory")
        self.assertFalse(runtime_persistence["langgraph_checkpointer_persistent"])
        self.assertFalse(runtime_persistence["native_interrupt_resume"])
        self.assertEqual(runtime_persistence["native_interrupt_resume_scope"], "none")

    def test_checkpointing_boundary_defaults_to_nonpersistent_memory(self) -> None:
        study_dir = _workspace_dir("lg2_checkpointing_boundary") / "PSY201"
        bundle = build_checkpointer()
        payload = describe_checkpointer(checkpointer=bundle.checkpointer, study_dir=study_dir, run_id="run_boundary", bundle=bundle)

        self.assertEqual(payload["source_of_truth"], "graph_state_json")
        self.assertEqual(payload["langgraph_checkpointer_backend"], "memory")
        self.assertEqual(payload["langgraph_checkpointer_type"], "InMemorySaver")
        self.assertFalse(payload["langgraph_checkpointer_persistent"])
        self.assertFalse(payload["native_interrupt_resume"])
        self.assertEqual(payload["native_interrupt_resume_scope"], "none")
        self.assertEqual(payload["restart_recovery_source"], "graph_state_json")
        self.assertIn("in-memory only", " ".join(payload["notes"]))

    def test_checkpointing_boundary_rejects_unavailable_sqlite_backend(self) -> None:
        study_dir = _workspace_dir("lg2_checkpointing_sqlite_unavailable") / "PSY201"
        sqlite_path = default_sqlite_checkpointer_path(study_dir, "run_sqlite")
        with patch("adam_agent.graph.checkpointing.importlib.util.find_spec", return_value=None):
            with self.assertRaisesRegex(ValueError, "SQLite LangGraph checkpointer is not installed"):
                build_checkpointer("sqlite", sqlite_path=sqlite_path)  # type: ignore[arg-type]

    def test_checkpointing_boundary_requires_sqlite_path(self) -> None:
        with self.assertRaisesRegex(ValueError, "sqlite_path is required"):
            build_checkpointer("sqlite")  # type: ignore[arg-type]

    def test_checkpointing_boundary_rejects_unavailable_postgres_backend(self) -> None:
        with self.assertRaisesRegex(ValueError, "Postgres LangGraph checkpointer is not installed|not wired database lifecycle"):
            build_checkpointer("postgres")  # type: ignore[arg-type]

    def test_custom_checkpointer_is_reported_without_persistence_claim(self) -> None:
        class CustomCheckpointer:
            pass

        study_dir = _workspace_dir("lg2_checkpointing_custom") / "PSY201"
        payload = describe_checkpointer(checkpointer=CustomCheckpointer(), study_dir=study_dir, run_id="run_custom")

        self.assertEqual(payload["langgraph_checkpointer_backend"], "custom")
        self.assertEqual(payload["langgraph_checkpointer_type"], "CustomCheckpointer")
        self.assertFalse(payload["langgraph_checkpointer_persistent"])
        self.assertEqual(payload["restart_recovery_source"], "graph_state_json")

    def test_default_sqlite_checkpointer_path_is_separate_from_product_ledger(self) -> None:
        study_dir = _workspace_dir("lg2_checkpointing_path") / "PSY201"
        path = default_sqlite_checkpointer_path(study_dir, "run_path")

        self.assertEqual(path.name, "langgraph_checkpoints.sqlite")
        self.assertTrue(str(path.as_posix()).endswith("runs/run_path/langgraph_checkpoints.sqlite"))
        self.assertNotEqual(path.name, "graph_checkpoints.sqlite")

    def test_sqlite_checkpointer_metadata_marks_native_resume_when_package_available(self) -> None:
        study_dir = _workspace_dir("lg2_checkpointing_sqlite_available") / "PSY201"
        sqlite_path = default_sqlite_checkpointer_path(study_dir, "run_sqlite")
        try:
            bundle = build_checkpointer(
                "sqlite",
                sqlite_path=sqlite_path,
            )
        except ValueError as exc:
            if "not installed" in str(exc) or "cannot be imported" in str(exc):
                self.skipTest(str(exc))
            raise

        try:
            payload = describe_checkpointer(
                checkpointer=bundle.checkpointer,
                study_dir=_workspace_dir("lg2_checkpointing_sqlite_payload") / "PSY201",
                run_id="run_sqlite",
                bundle=bundle,
            )
            self.assertEqual(payload["langgraph_checkpointer_backend"], "sqlite")
            self.assertEqual(payload["langgraph_checkpointer_type"], "SqliteSaver")
            self.assertTrue(payload["langgraph_checkpointer_persistent"])
            self.assertTrue(payload["native_interrupt_resume"])
            self.assertEqual(payload["native_interrupt_resume_scope"], "native_pilot_interrupts_only")
            self.assertEqual(payload["restart_recovery_source"], "langgraph_sqlite_checkpointer")
            self.assertTrue(payload["langgraph_checkpoint_path"].endswith("langgraph_checkpoints.sqlite"))
            self.assertEqual(payload["langgraph_checkpoint_path"], str(sqlite_path.as_posix()))
            self.assertIn("local single-process recovery", " ".join(payload["notes"]))
        finally:
            bundle.close()

    def test_sqlite_checkpointer_can_read_interrupt_after_new_gateway_when_package_available(self) -> None:
        study_dir = _workspace_dir("lg2_checkpointing_sqlite_restart") / "PSY201"
        study_dir.mkdir(parents=True)
        sqlite_path = default_sqlite_checkpointer_path(study_dir, "run_sqlite_restart")

        try:
            gateway = GraphGateway(checkpointer_backend="sqlite", sqlite_checkpointer_path=sqlite_path)
        except ValueError as exc:
            if "not installed" in str(exc) or "cannot be imported" in str(exc):
                self.skipTest(str(exc))
            raise

        try:
            gateway.start_native_dependency_review(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_sqlite_restart",
                target_datasets=["ADAE"],
            )
        finally:
            gateway.close()

        reloaded_gateway = GraphGateway(checkpointer_backend="sqlite", sqlite_checkpointer_path=sqlite_path)
        try:
            snapshot = reloaded_gateway.get_state(study_id="PSY201", run_id="run_sqlite_restart")
        finally:
            reloaded_gateway.close()

        self.assertEqual(snapshot["study_id"], "PSY201")
        self.assertEqual(snapshot["run_id"], "run_sqlite_restart")
        self.assertEqual(snapshot["current_interrupt"], "dependency_review")

    def test_gateway_persists_canonical_state_for_process_restart_resume(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_persisted_state") / "PSY201"
        study_dir.mkdir(parents=True)

        GraphGateway().start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_gateway_persisted",
            target_datasets=["ADAE"],
        )
        reloaded = GraphGateway().load_graph_state(study_dir=study_dir, run_id="run_lg2_gateway_persisted")
        db_path = study_dir / "runs" / "run_lg2_gateway_persisted" / "graph_checkpoints.sqlite"

        self.assertEqual(reloaded.current_interrupt.name, "dependency_review")
        self.assertEqual(reloaded.dependency_review_status, "review_required")
        self.assertTrue(db_path.exists())
        conn = sqlite3.connect(db_path)
        try:
            count = conn.execute("select count(*) from graph_checkpoints").fetchone()[0]
        finally:
            conn.close()
        self.assertGreaterEqual(count, 1)

    def test_prepare_run_plan_uses_graph_projection(self) -> None:
        study_dir = _workspace_dir("lg2_api_prepare") / "PSY201"
        study_dir.mkdir(parents=True)

        response = prepare_run_plan(
            RunPlanRequest(
                study_dir=str(study_dir),
                study_id="PSY201",
                run_id="run_lg2_api_prepare",
                target_datasets=["ADAE"],
            )
        )

        workflow_path = study_dir / "runs" / "run_lg2_api_prepare" / "workflow_state.json"
        graph_path = study_dir / "runs" / "run_lg2_api_prepare" / "graph_state.json"
        workflow_state = json.loads(workflow_path.read_text(encoding="utf-8"))

        self.assertEqual(response.dependency_review_status, "review_required")
        self.assertEqual(response.target_datasets, ["ADAE"])
        self.assertEqual(response.graph_state_path, str(graph_path.as_posix()))
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["current_interrupt"], "dependency_review")

    def test_graph_state_endpoint_returns_canonical_state(self) -> None:
        from fastapi.testclient import TestClient

        from adam_agent.api.app import create_app

        study_dir = _workspace_dir("lg2_api_graph_state") / "PSY201"
        study_dir.mkdir(parents=True)
        client = TestClient(create_app())
        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "study_id": "PSY201",
                "run_id": "run_lg2_api_graph_state",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)
        prepared_payload = prepared.json()
        self.assertEqual(prepared_payload["graph_state_path"], str((study_dir / "runs" / "run_lg2_api_graph_state" / "graph_state.json").as_posix()))
        self.assertEqual(prepared_payload["workflow_state_path"], str((study_dir / "runs" / "run_lg2_api_graph_state" / "workflow_state.json").as_posix()))

        response = client.get(
            "/runs/run_lg2_api_graph_state/graph-state",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["study_id"], "PSY201")
        self.assertEqual(payload["run_id"], "run_lg2_api_graph_state")
        self.assertEqual(payload["current_interrupt"]["name"], "dependency_review")
        self.assertIn("ADAE", payload["datasets"])

    def test_dependency_review_endpoint_resumes_graph_state(self) -> None:
        from fastapi.testclient import TestClient

        from adam_agent.api.app import create_app

        study_dir = _workspace_dir("lg2_api_dependency_review") / "PSY201"
        study_dir.mkdir(parents=True)
        client = TestClient(create_app())
        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "study_id": "PSY201",
                "run_id": "run_lg2_dependency_review",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)

        reviewed = client.post(
            "/runs/run_lg2_dependency_review/dependency-review",
            json={
                "study_dir": str(study_dir),
                "reviewer": "tester",
                "decision": "approve",
                "notes": "Proceed with MVP dependency plan.",
            },
        )

        self.assertEqual(reviewed.status_code, 200, reviewed.text)
        payload = reviewed.json()
        self.assertTrue(payload["approved"])
        self.assertIsNone(payload["current_interrupt"])
        graph_state = client.get(
            "/runs/run_lg2_dependency_review/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_dependency_review" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertIsNone(graph_state["current_interrupt"])
        self.assertEqual(graph_state["human_commands"][0]["interrupt"], "dependency_review")
        self.assertEqual(graph_state["human_commands"][0]["action"], "approve")
        self.assertIsNone(workflow_state["current_interrupt"])
        self.assertEqual(workflow_state["projection_source"], "langgraph")

    def test_gateway_review_dependency_owns_interrupt_resume(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_dependency_review_entrypoint") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_gateway_dependency_review",
            target_datasets=["ADAE"],
        )

        result = gateway.review_dependency(
            study_dir=study_dir,
            run_id="run_lg2_gateway_dependency_review",
            decision="approve",
            reviewer="tester",
            notes="Dependency plan accepted for this run.",
            approved_dependency_datasets=["adsl"],
        )

        self.assertTrue(result.approved)
        self.assertIsNone(result.current_interrupt)
        self.assertIsNone(result.graph_state.current_interrupt)
        self.assertEqual(result.graph_state.human_commands[0].interrupt, "dependency_review")
        self.assertEqual(result.graph_state.human_commands[0].payload["approved_dependency_datasets"], ["ADSL"])
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_gateway_dependency_review" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertIsNone(workflow_state["current_interrupt"])
        self.assertEqual(workflow_state["projection_source"], "langgraph")

    def test_gateway_native_dependency_review_interrupt_roundtrip_persists_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_dependency_review") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()

        interrupted = gateway.start_native_dependency_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_dependency_review",
            target_datasets=["ADAE"],
        )

        self.assertEqual(interrupted.graph_state.current_interrupt.name, "dependency_review")
        native_boundary = interrupted.graph_state.runtime_persistence["native_dependency_review_interrupt"]
        self.assertEqual(native_boundary["boundary"], "dependency_review_pilot_only")
        self.assertEqual(native_boundary["open_interrupt_count"], 1)
        self.assertEqual(native_boundary["next_nodes"], ["wait_for_dependency_review"])

        reviewed = gateway.resume_native_dependency_review(
            study_dir=study_dir,
            run_id="run_lg2_native_dependency_review",
            decision="approve",
            reviewer="tester",
            notes="Native interrupt pilot approved.",
        )

        self.assertTrue(reviewed.approved)
        self.assertIsNone(reviewed.current_interrupt)
        self.assertIsNone(reviewed.graph_state.current_interrupt)
        self.assertEqual(reviewed.graph_state.dependency_review_status, "approved")
        self.assertEqual(reviewed.graph_state.human_commands[0].interrupt, "dependency_review")
        self.assertEqual(
            reviewed.graph_state.runtime_persistence["native_dependency_review_interrupt"]["open_interrupt_count"],
            0,
        )
        graph_state = json.loads(
            (study_dir / "runs" / "run_lg2_native_dependency_review" / "graph_state.json").read_text(encoding="utf-8")
        )
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_native_dependency_review" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(graph_state["dependency_review_status"], "approved")
        self.assertIsNone(workflow_state["current_interrupt"])
        self.assertEqual(workflow_state["dependency_review_status"], "approved")
        self.assertEqual(workflow_state["projection_source"], "langgraph")

    def test_gateway_native_dependency_review_reject_persists_closed_failed_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_dependency_review_reject") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_native_dependency_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_dependency_review_reject",
            target_datasets=["ADAE"],
        )

        reviewed = gateway.resume_native_dependency_review(
            study_dir=study_dir,
            run_id="run_lg2_native_dependency_review_reject",
            decision="reject",
            reviewer="tester",
            notes="Rejected native dependency-review pilot.",
        )

        self.assertFalse(reviewed.approved)
        self.assertIsNone(reviewed.current_interrupt)
        self.assertIsNone(reviewed.graph_state.current_interrupt)
        self.assertEqual(reviewed.graph_state.status, "failed")
        self.assertEqual(reviewed.graph_state.dependency_review_status, "rejected")
        self.assertEqual(
            reviewed.graph_state.runtime_persistence["native_dependency_review_interrupt"]["open_interrupt_count"],
            0,
        )

    def test_gateway_resume_preserves_other_dataset_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_resume_preserve_interrupt") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_resume_preserve_interrupt",
            status="needs_review",
            target_datasets=["ADAE", "ADCM"],
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_resume_preserve_interrupt",
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="draft_spec_review", dataset="ADAE"),
                ),
                "ADCM": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_resume_preserve_interrupt",
                    dataset="ADCM",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADCM"),
                ),
            },
        )

        result = GraphGateway().resume(
            study_dir=study_dir,
            graph_state=state,
            command=HumanCommand(
                interrupt="draft_spec_review",
                action="approve",
                dataset="ADAE",
                reviewer="tester",
            ),
        )

        self.assertIsNone(result.graph_state.datasets["ADAE"].current_interrupt)
        self.assertEqual(result.graph_state.current_interrupt.name, "code_review")
        self.assertEqual(result.graph_state.current_interrupt.dataset, "ADCM")
        self.assertEqual(result.graph_state.status, "needs_review")

    def test_gateway_resume_rejects_study_command_without_matching_open_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_resume_reject_study_mismatch") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_resume_reject_study_mismatch",
            status="needs_review",
            current_interrupt=InterruptState(name="dependency_review", status="resolved"),
        )

        with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
            GraphGateway().resume(
                study_dir=study_dir,
                graph_state=state,
                command=HumanCommand(
                    interrupt="dependency_review",
                    action="approve",
                    reviewer="tester",
                ),
            )

        self.assertFalse((study_dir / "runs" / "run_lg2_resume_reject_study_mismatch" / "graph_state.json").exists())

    def test_gateway_resume_rejects_dataset_command_for_other_open_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_resume_reject_dataset_mismatch") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_resume_reject_dataset_mismatch",
            status="needs_review",
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_resume_reject_dataset_mismatch",
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="draft_spec_review", dataset="ADAE"),
                )
            },
        )

        with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
            GraphGateway().resume(
                study_dir=study_dir,
                graph_state=state,
                command=HumanCommand(
                    interrupt="code_review",
                    action="approve",
                    dataset="ADAE",
                    reviewer="tester",
                ),
            )

        self.assertFalse((study_dir / "runs" / "run_lg2_resume_reject_dataset_mismatch" / "graph_state.json").exists())

    def test_gateway_resume_rejects_dataset_command_without_dataset_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_resume_reject_missing_dataset_interrupt") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_resume_reject_missing_dataset_interrupt",
            status="needs_review",
            current_interrupt=InterruptState(name="dependency_review"),
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_resume_reject_missing_dataset_interrupt",
                    dataset="ADAE",
                    status="pending",
                )
            },
        )

        with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
            GraphGateway().resume(
                study_dir=study_dir,
                graph_state=state,
                command=HumanCommand(
                    interrupt="draft_spec_review",
                    action="approve",
                    dataset="ADAE",
                    reviewer="tester",
                ),
            )

        self.assertFalse((study_dir / "runs" / "run_lg2_resume_reject_missing_dataset_interrupt" / "graph_state.json").exists())

    def test_gateway_records_dataset_code_review_in_canonical_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_code_review") / "PSY201"
        code_dir = study_dir / "runs" / "run_lg2_code_review" / "code"
        review_dir = study_dir / "runs" / "run_lg2_code_review" / "review"
        static_dir = study_dir / "runs" / "run_lg2_code_review" / "static_checks"
        code_dir.mkdir(parents=True)
        review_dir.mkdir()
        static_dir.mkdir()
        code_path = code_dir / "build_adae.R"
        review_path = review_dir / "adae_code_review.json"
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_code_review", "ADAE", code_path)
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        code_sha = f"sha256:{sha256_file(code_path)}"
        gateway = GraphGateway()
        generated = gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_code_review",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
        )
        generated_dataset = generated.graph_state.datasets["ADAE"]
        generated_agents = [item["agent"] for item in generated_dataset.agent_decisions]
        self.assertIn("code_agent", generated_agents)
        self.assertIn("static_review_agent", generated_agents)
        self.assertEqual(generated_dataset.agent_decisions[0]["outputs"]["record_source"], "graph_gateway_default")
        self.assertEqual([item["agent"] for item in generated_dataset.agent_node_inputs], ["code_agent", "static_review_agent"])
        self.assertEqual([item["agent"] for item in generated_dataset.agent_node_outputs], ["code_agent", "static_review_agent"])
        self.assertEqual(generated_dataset.agent_node_outputs[0]["agent_decisions"][0], generated_dataset.agent_decisions[0])
        self.assertEqual(generated_dataset.agent_node_outputs[1]["agent_decisions"][0], generated_dataset.agent_decisions[1])
        self.assertIn("static_check_limited_scope", generated_dataset.risk_flags)

        result = gateway.record_code_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_code_review",
            dataset="ADAE",
            command=HumanCommand(
                interrupt="code_review",
                action="approve",
                dataset="ADAE",
                reviewer="tester",
                notes="Approved graph-native code review.",
            ),
            review_path=review_path,
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_code_review" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(dataset_state.code_state["status"], "approved")
        self.assertEqual(dataset_state.code_state["code_sha256"], code_sha)
        self.assertEqual(dataset_state.human_commands[0].interrupt, "code_review")
        self.assertEqual(dataset_state.human_commands[0].action, "approve")
        self.assertIsNone(dataset_state.current_interrupt)
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["code_state"]["status"], "approved")

    def test_gateway_review_code_writes_artifact_and_records_canonical_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_review_code_entrypoint") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_review_code_entrypoint"
        code_dir = run_dir / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_review_code_entrypoint", "ADAE", code_path)
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_review_code_entrypoint",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        result = gateway.review_code(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_review_code_entrypoint",
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Gateway owns the review artifact.",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        review_path = Path(result.review_path)
        review_payload = json.loads(review_path.read_text(encoding="utf-8"))
        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(result.decision, "approve")
        self.assertTrue(result.approved)
        self.assertEqual(result.static_check_path, str(static_path.as_posix()))
        self.assertEqual(review_payload["decision"], "approve")
        self.assertEqual(review_payload["code_sha256"], f"sha256:{sha256_file(code_path)}")
        self.assertEqual(dataset_state.code_state["status"], "approved")
        self.assertEqual(dataset_state.code_state["review_path"], str(review_path.as_posix()))
        self.assertEqual(result.workflow_projection["datasets"]["ADAE"]["code_state"]["status"], "approved")

    def test_gateway_review_code_from_command_bridges_native_interrupt_to_artifact_flow(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_bridge") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_native_code_review_bridge"
        code_dir = run_dir / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_native_code_review_bridge",
            "ADAE",
            code_path,
        )
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_bridge",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        result = gateway.review_code_from_command(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_bridge",
            command=HumanCommand(
                interrupt="code_review",
                action="approve",
                dataset="ADAE",
                reviewer="native_tester",
                notes="Approved from native interrupt bridge.",
            ),
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        review_path = Path(result.review_path)
        review_payload = json.loads(review_path.read_text(encoding="utf-8"))
        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertTrue(result.approved)
        self.assertEqual(result.decision, "approve")
        self.assertEqual(review_payload["reviewer"], "native_tester")
        self.assertEqual(review_payload["code_sha256"], f"sha256:{sha256_file(code_path)}")
        self.assertEqual(review_payload["static_check_sha256"], static_sha)
        self.assertEqual(dataset_state.code_state["status"], "approved")
        self.assertEqual(dataset_state.code_state["review_path"], str(review_path.as_posix()))
        self.assertIsNone(dataset_state.current_interrupt)
        self.assertEqual(dataset_state.human_commands[-1].interrupt, "code_review")
        self.assertEqual(result.workflow_projection["datasets"]["ADAE"]["code_state"]["status"], "approved")

    def test_gateway_review_code_from_command_bridges_reject_without_execution_unlock(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_bridge_reject") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_native_code_review_bridge_reject"
        code_dir = run_dir / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_native_code_review_bridge_reject",
            "ADAE",
            code_path,
        )
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_bridge_reject",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        result = gateway.review_code_from_command(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_bridge_reject",
            command=HumanCommand(
                interrupt="code_review",
                action="reject",
                dataset="ADAE",
                reviewer="native_tester",
                notes="Reject from native interrupt bridge.",
            ),
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        review_path = Path(result.review_path)
        review_payload = json.loads(review_path.read_text(encoding="utf-8"))
        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertFalse(result.approved)
        self.assertEqual(result.decision, "reject")
        self.assertEqual(review_payload["decision"], "reject")
        self.assertFalse(review_payload["approved"])
        self.assertEqual(review_payload["static_check_sha256"], static_sha)
        self.assertEqual(dataset_state.code_state["status"], "rejected")
        self.assertEqual(dataset_state.code_state["review_path"], str(review_path.as_posix()))
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(result.workflow_projection["datasets"]["ADAE"]["code_state"]["status"], "rejected")

    def test_gateway_review_code_from_command_rejects_mismatched_interrupt_without_artifact(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_bridge_mismatch") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_native_code_review_bridge_mismatch"
        code_dir = run_dir / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_native_code_review_bridge_mismatch",
            "ADAE",
            code_path,
        )
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_bridge_mismatch",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        graph_state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_bridge_mismatch",
        )
        graph_state.datasets["ADAE"].current_interrupt = InterruptState(
            name="draft_spec_review",
            dataset="ADAE",
            reason="Different open interrupt for mismatch test.",
        )
        gateway._persist_graph_state(study_dir, graph_state, node="test_interrupt_mismatch")

        with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
            gateway.review_code_from_command(
                study_dir=study_dir,
                run_id="run_lg2_native_code_review_bridge_mismatch",
                command=HumanCommand(
                    interrupt="code_review",
                    action="approve",
                    dataset="ADAE",
                    reviewer="native_tester",
                ),
                input_fingerprint_payload=input_fingerprint(study_dir),
            )

        self.assertFalse((run_dir / "review" / "adae_code_review.json").exists())
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_code_review_bridge_mismatch")
        self.assertEqual(reloaded.datasets["ADAE"].code_state["status"], "generated")

    def test_gateway_review_code_from_command_rejects_dataset_command_behind_study_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_bridge_study_gate") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_native_code_review_bridge_study_gate"
        code_dir = run_dir / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_native_code_review_bridge_study_gate",
            "ADAE",
            code_path,
        )
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_bridge_study_gate",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        graph_state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_bridge_study_gate",
        )
        graph_state.current_interrupt = InterruptState(
            name="dependency_review",
            reason="Study-level dependency review must be resolved first.",
        )
        gateway._persist_graph_state(study_dir, graph_state, node="test_study_level_gate")

        with self.assertRaisesRegex(ValueError, "Study-level interrupt dependency_review must be resolved"):
            gateway.review_code_from_command(
                study_dir=study_dir,
                run_id="run_lg2_native_code_review_bridge_study_gate",
                command=HumanCommand(
                    interrupt="code_review",
                    action="approve",
                    dataset="ADAE",
                    reviewer="native_tester",
                ),
                input_fingerprint_payload=input_fingerprint(study_dir),
            )

        self.assertFalse((run_dir / "review" / "adae_code_review.json").exists())
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_code_review_bridge_study_gate")
        self.assertEqual(reloaded.current_interrupt.name, "dependency_review")
        self.assertEqual(reloaded.datasets["ADAE"].code_state["status"], "generated")

    def test_gateway_review_code_from_command_allows_dataset_interrupt_as_current_rollup(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_bridge_dataset_rollup") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_native_code_review_bridge_dataset_rollup"
        code_dir = run_dir / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_native_code_review_bridge_dataset_rollup",
            "ADAE",
            code_path,
        )
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_bridge_dataset_rollup",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        graph_state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_bridge_dataset_rollup",
        )
        graph_state.current_interrupt = InterruptState(
            name="code_review",
            dataset="ADAE",
            reason="Rolled-up dataset interrupt should not be treated as study-level.",
        )
        gateway._persist_graph_state(study_dir, graph_state, node="test_dataset_level_rollup")

        result = gateway.review_code_from_command(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_bridge_dataset_rollup",
            command=HumanCommand(
                interrupt="code_review",
                action="approve",
                dataset="ADAE",
                reviewer="native_tester",
            ),
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        self.assertTrue(result.approved)
        self.assertEqual(result.graph_state.datasets["ADAE"].code_state["status"], "approved")

    def test_gateway_review_code_cleans_artifact_when_recording_fails(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_review_code_cleanup") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_review_code_cleanup"
        code_dir = run_dir / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_review_code_cleanup", "ADAE", code_path)
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_review_code_cleanup",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        with patch.object(gateway, "record_code_review", side_effect=ValueError("forced graph failure")):
            with self.assertRaisesRegex(ValueError, "forced graph failure"):
                gateway.review_code(
                    study_dir=study_dir,
                    study_id="PSY201",
                    run_id="run_lg2_review_code_cleanup",
                    dataset="ADAE",
                    decision="approve",
                    reviewer="tester",
                    input_fingerprint_payload=input_fingerprint(study_dir),
                )

        self.assertFalse((run_dir / "review" / "adae_code_review.json").exists())
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_review_code_cleanup")
        self.assertEqual(reloaded.datasets["ADAE"].code_state["status"], "generated")

    def test_gateway_records_draft_spec_review_in_canonical_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_draft_spec_review") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_draft_spec_review"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = run_dir / "specs"
        llm_dir = run_dir / "llm"
        review_dir = run_dir / "reviews"
        approved_dir = run_dir / "approved_specs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        llm_dir.mkdir()
        review_dir.mkdir()
        approved_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        prompt_path = llm_dir / "adae_draft_prompt.txt"
        response_path = llm_dir / "adae_draft_response.json"
        prompt_path.write_text("draft ADAE spec", encoding="utf-8")
        response_path.write_text(json.dumps({"dataset": "ADAE"}), encoding="utf-8")

        gateway = GraphGateway()
        generated = gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_draft_spec_review",
            dataset="ADAE",
            draft_spec_path=draft_path,
            prompt_path=prompt_path,
            response_path=response_path,
            input_fingerprint_payload=fingerprint,
        )

        dataset_state = generated.graph_state.datasets["ADAE"]
        self.assertEqual(dataset_state.spec_state["status"], "draft_generated")
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(dataset_state.agent_decisions[0]["agent"], "spec_agent")
        self.assertEqual(dataset_state.agent_decisions[0]["decision"], "draft_spec_generated")
        self.assertEqual(dataset_state.agent_node_inputs[0]["agent"], "spec_agent")
        self.assertEqual(dataset_state.agent_node_inputs[0]["node"], "draft_spec_generation")
        self.assertEqual(dataset_state.agent_node_outputs[0]["decision"], "draft_spec_generated")
        self.assertEqual(dataset_state.agent_node_outputs[0]["agent_decisions"][0], dataset_state.agent_decisions[0])
        self.assertEqual(generated.graph_state.agent_decisions[-1]["agent"], "spec_agent")
        self.assertIn("spec_agent", [item["agent"] for item in generated.graph_state.agent_node_outputs])
        self.assertEqual(generated.workflow_projection["datasets"]["ADAE"]["spec_state"]["status"], "draft_generated")
        self.assertEqual(generated.workflow_projection["datasets"]["ADAE"]["agent_decisions"][0]["agent"], "spec_agent")
        approved_path = approved_dir / "adae_approved_spec.json"
        approved_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint, "status": "approved_draft"}),
            encoding="utf-8",
        )
        approved_sha = f"sha256:{sha256_file(approved_path)}"
        review_path = review_dir / "adae_draft_spec_review.json"
        review_path.write_text(
            json.dumps({"decision": "approve", "approved": True, "approved_spec_sha256": approved_sha}),
            encoding="utf-8",
        )

        reviewed = gateway.record_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_draft_spec_review",
            dataset="ADAE",
            command=HumanCommand(
                interrupt="draft_spec_review",
                action="approve",
                dataset="ADAE",
                reviewer="tester",
                notes="Approved graph-native draft spec.",
            ),
            review_path=review_path,
            draft_spec_path=draft_path,
            approved_spec_path=approved_path,
            approved_spec_sha256=approved_sha,
            input_fingerprint_payload=fingerprint,
        )

        dataset_state = reviewed.graph_state.datasets["ADAE"]
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_draft_spec_review" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(dataset_state.spec_state["status"], "approved")
        self.assertEqual(dataset_state.spec_state["approved_spec_sha256"], approved_sha)
        self.assertEqual(dataset_state.human_commands[0].interrupt, "draft_spec_review")
        self.assertIsNone(dataset_state.current_interrupt)
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["spec_state"]["status"], "approved")

    def test_gateway_review_draft_spec_writes_artifacts_and_records_canonical_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_review_draft_spec_entrypoint") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_review_draft_spec_entrypoint"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = run_dir / "specs"
        llm_dir = run_dir / "llm"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        llm_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        prompt_path = llm_dir / "adae_draft_prompt.txt"
        response_path = llm_dir / "adae_draft_response.json"
        prompt_path.write_text("draft ADAE spec", encoding="utf-8")
        response_path.write_text(json.dumps({"dataset": "ADAE"}), encoding="utf-8")
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_review_draft_spec_entrypoint",
            dataset="ADAE",
            draft_spec_path=draft_path,
            prompt_path=prompt_path,
            response_path=response_path,
            input_fingerprint_payload=fingerprint,
        )

        result = gateway.review_draft_spec(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_review_draft_spec_entrypoint",
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Gateway owns draft-spec review artifacts.",
            input_fingerprint_payload=fingerprint,
        )

        review_path = Path(result.review_path)
        approved_path = Path(str(result.approved_spec_path))
        review_payload = json.loads(review_path.read_text(encoding="utf-8"))
        approved_payload = json.loads(approved_path.read_text(encoding="utf-8"))
        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(result.decision, "approve")
        self.assertTrue(result.approved)
        self.assertEqual(review_payload["decision"], "approve")
        self.assertEqual(review_payload["approved_spec_sha256"], f"sha256:{sha256_file(approved_path)}")
        self.assertEqual(approved_payload["status"], "approved_draft")
        self.assertEqual(approved_payload["approved_by"], "tester")
        self.assertEqual(dataset_state.spec_state["status"], "approved")
        self.assertEqual(dataset_state.spec_state["review_path"], str(review_path.as_posix()))
        self.assertEqual(dataset_state.spec_state["approved_spec_path"], str(approved_path.as_posix()))
        self.assertEqual(result.workflow_projection["datasets"]["ADAE"]["spec_state"]["status"], "approved")

    def test_gateway_review_draft_spec_cleans_artifacts_when_recording_fails(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_review_draft_spec_cleanup") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_review_draft_spec_cleanup"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = run_dir / "specs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_review_draft_spec_cleanup",
            dataset="ADAE",
            draft_spec_path=draft_path,
            input_fingerprint_payload=fingerprint,
        )

        with patch.object(gateway, "record_draft_spec_review", side_effect=ValueError("forced graph failure")):
            with self.assertRaisesRegex(ValueError, "forced graph failure"):
                gateway.review_draft_spec(
                    study_dir=study_dir,
                    study_id="PSY201",
                    run_id="run_lg2_review_draft_spec_cleanup",
                    dataset="ADAE",
                    decision="approve",
                    reviewer="tester",
                    input_fingerprint_payload=fingerprint,
                )

        self.assertFalse((run_dir / "reviews" / "adae_draft_spec_review.json").exists())
        self.assertFalse((run_dir / "approved_specs" / "adae_approved_spec.json").exists())
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_review_draft_spec_cleanup")
        self.assertEqual(reloaded.datasets["ADAE"].spec_state["status"], "draft_generated")

    def test_gateway_finalize_inputs_records_existing_input_spec(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_finalize_input_spec") / "PSY201"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        spec_path = spec_dir / "adae.json"
        spec_path.write_text(json.dumps({"dataset": "ADAE", "variables": []}), encoding="utf-8")
        gateway = GraphGateway()

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "needs_review",
                "spec_source": "input_spec",
                "input_spec_path": str(spec_path.as_posix()),
                "product_context_warnings": ["Context warning."],
                "agent_decisions": [],
                "agent_node_inputs": [
                    {
                        "agent": "evidence_agent",
                        "node": "prepare_inputs",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_finalize_input_spec",
                        "dataset": "ADAE",
                        "task": "Validate supplied input spec and prepare product context.",
                        "created_at": "2026-05-31T00:00:01Z",
                    }
                ],
                "agent_node_outputs": [
                    {
                        "agent": "evidence_agent",
                        "node": "prepare_inputs",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_finalize_input_spec",
                        "dataset": "ADAE",
                        "status": "ready",
                        "decision": "input_spec_ready",
                        "reason": "A supplied input spec was selected as the derivation authority.",
                        "outputs": {"spec_source": "input_spec"},
                        "created_at": "2026-05-31T00:00:02Z",
                    }
                ],
                "risk_flags": [],
            }
            result = gateway.finalize_inputs(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_gateway_finalize_input_spec",
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
                rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
            )

        compile_graph.assert_called_once()
        invoked_state = compile_graph.return_value.invoke.call_args.args[0]
        self.assertEqual(invoked_state["execution_mode"], "graph_product_prepare")
        self.assertEqual(invoked_state["dataset"], "ADAE")
        self.assertEqual(result.spec_source, "input_spec")
        self.assertEqual(result.input_spec_path, str(spec_path.as_posix()))
        self.assertEqual(result.warnings, ["Context warning."])

        dataset_state = result.graph_state.datasets["ADAE"]
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_gateway_finalize_input_spec" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(dataset_state.spec_state["status"], "input_spec_ready")
        self.assertEqual(dataset_state.status, "pending")
        self.assertIsNone(dataset_state.current_interrupt)
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["spec_state"]["status"], "input_spec_ready")
        self.assertEqual(dataset_state.agent_node_inputs[0]["agent"], "evidence_agent")
        self.assertEqual(dataset_state.agent_node_outputs[0]["decision"], "input_spec_ready")
        self.assertIn("evidence_agent", [item["agent"] for item in result.graph_state.agent_node_inputs])
        self.assertIn("input_spec_ready", [item["decision"] for item in result.graph_state.agent_node_outputs])
        persisted_state = json.loads(
            (study_dir / "runs" / "run_lg2_gateway_finalize_input_spec" / "graph_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(persisted_state["datasets"]["ADAE"]["agent_node_outputs"][0]["decision"], "input_spec_ready")
        self.assertIn("input_spec_ready", [item["decision"] for item in persisted_state["agent_node_outputs"]])

    def test_gateway_finalize_inputs_records_review_required_draft_spec(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_finalize_draft_spec") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_gateway_finalize_draft_spec"
        spec_dir = run_dir / "specs"
        llm_dir = run_dir / "llm"
        spec_dir.mkdir(parents=True)
        llm_dir.mkdir()
        draft_path = spec_dir / "adae_draft_spec.json"
        prompt_path = llm_dir / "adae_draft_prompt.txt"
        response_path = llm_dir / "adae_draft_response.json"
        draft_path.write_text(json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM"}]}), encoding="utf-8")
        prompt_path.write_text("draft prompt", encoding="utf-8")
        response_path.write_text(json.dumps({"dataset": "ADAE"}), encoding="utf-8")
        gateway = GraphGateway()

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "needs_review",
                "spec_source": "missing_input_spec",
                "draft_spec_path": str(draft_path.as_posix()),
                "draft_spec_prompt_path": str(prompt_path.as_posix()),
                "draft_spec_response_path": str(response_path.as_posix()),
                "draft_spec_variables": [{"variable": "AETERM"}],
                "product_context_warnings": ["Draft warning."],
                "agent_decisions": [],
                "risk_flags": [],
            }
            result = gateway.finalize_inputs(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_gateway_finalize_draft_spec",
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        self.assertEqual(result.spec_source, "missing_input_spec")
        self.assertEqual(result.draft_spec_path, str(draft_path.as_posix()))
        self.assertEqual(result.draft_spec_variables, [{"variable": "AETERM"}])
        dataset_state = result.graph_state.datasets["ADAE"]
        workflow_state = json.loads((run_dir / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(dataset_state.spec_state["status"], "draft_generated")
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(result.graph_state.dependency_review_status, "accepted")
        self.assertEqual(
            result.graph_state.dependency_plan["dependency_review_status_before_product_step"],
            "review_required",
        )
        self.assertEqual(workflow_state["current_interrupt"], "draft_spec_review")

    def test_gateway_generate_draft_spec_forces_fresh_draft_and_rejects_input_spec(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_force_draft_spec") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_gateway_force_draft_spec"
        spec_dir = run_dir / "specs"
        llm_dir = run_dir / "llm"
        spec_dir.mkdir(parents=True)
        llm_dir.mkdir()
        draft_path = spec_dir / "adae_draft_spec.json"
        prompt_path = llm_dir / "adae_draft_prompt.txt"
        response_path = llm_dir / "adae_draft_response.json"
        draft_path.write_text(json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM"}]}), encoding="utf-8")
        prompt_path.write_text("draft prompt", encoding="utf-8")
        response_path.write_text(json.dumps({"dataset": "ADAE"}), encoding="utf-8")
        gateway = GraphGateway()

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "needs_review",
                "spec_source": "draft_spec",
                "draft_spec_path": str(draft_path.as_posix()),
                "draft_spec_prompt_path": str(prompt_path.as_posix()),
                "draft_spec_response_path": str(response_path.as_posix()),
                "draft_spec_variables": [{"variable": "AETERM"}],
                "product_context_warnings": [],
                "agent_decisions": [],
                "risk_flags": [],
            }
            result = gateway.generate_draft_spec(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_gateway_force_draft_spec",
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        invoked_state = compile_graph.return_value.invoke.call_args.args[0]
        self.assertTrue(invoked_state["force_new_draft_spec"])
        self.assertEqual(result.spec_source, "draft_spec")
        self.assertEqual(result.draft_spec_path, str(draft_path.as_posix()))

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "needs_review",
                "spec_source": "input_spec",
                "input_spec_path": str((study_dir / "input_spec" / "adae.json").as_posix()),
                "product_context_warnings": [],
                "agent_decisions": [],
                "risk_flags": [],
            }
            with self.assertRaisesRegex(ValueError, "draft spec generation is not needed"):
                gateway.generate_draft_spec(
                    study_dir=study_dir,
                    study_id="PSY201",
                    run_id="run_lg2_gateway_force_draft_spec_input",
                    dataset="ADAE",
                    llm_provider={"provider": "mock", "model": "mock-model"},
                    llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
                )

    def test_gateway_native_draft_spec_review_roundtrip_persists_formal_review_artifact(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_draft_spec_roundtrip") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()

        started = gateway.start_native_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_draft_spec_roundtrip",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = started.graph_state.datasets["ADAE"]
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(dataset_state.spec_state["status"], "draft_generated")
        self.assertIn("native_draft_spec_review_interrupt", started.graph_state.runtime_persistence)
        self.assertEqual(
            started.graph_state.runtime_persistence["native_draft_spec_review_interrupt"]["boundary"],
            "draft_spec_review_pilot_only",
        )

        reviewed = gateway.resume_native_draft_spec_review(
            study_dir=study_dir,
            run_id="run_lg2_native_draft_spec_roundtrip",
            dataset="ADAE",
            decision="approve",
            reviewer="native_tester",
            notes="Native draft-spec gateway roundtrip approved.",
        )

        review_path = Path(reviewed.review_path)
        review_payload = json.loads(review_path.read_text(encoding="utf-8"))
        reviewed_dataset = reviewed.graph_state.datasets["ADAE"]
        self.assertTrue(reviewed.approved)
        self.assertEqual(review_payload["decision"], "approve")
        self.assertEqual(review_payload["reviewer"], "native_tester")
        self.assertTrue(Path(str(reviewed.approved_spec_path)).exists())
        self.assertEqual(reviewed_dataset.spec_state["status"], "approved")
        self.assertIsNone(reviewed_dataset.current_interrupt)
        self.assertEqual(reviewed_dataset.human_commands[-1].interrupt, "draft_spec_review")
        self.assertEqual(
            reviewed.graph_state.runtime_persistence["native_draft_spec_review_resume"]["native_status"],
            "approved",
        )
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg2_native_draft_spec_roundtrip"
                / "reviews"
                / "adae_draft_spec_review.json"
            ).exists()
        )

    def test_gateway_native_draft_spec_review_reject_roundtrip_keeps_review_locked(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_draft_spec_reject_roundtrip") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_native_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_draft_spec_reject_roundtrip",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        reviewed = gateway.resume_native_draft_spec_review(
            study_dir=study_dir,
            run_id="run_lg2_native_draft_spec_reject_roundtrip",
            dataset="ADAE",
            decision="reject",
            reviewer="native_tester",
            notes="Draft spec needs revision.",
        )

        review_payload = json.loads(Path(reviewed.review_path).read_text(encoding="utf-8"))
        dataset_state = reviewed.graph_state.datasets["ADAE"]
        self.assertFalse(reviewed.approved)
        self.assertEqual(review_payload["decision"], "reject")
        self.assertEqual(dataset_state.spec_state["status"], "rejected")
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(
            reviewed.graph_state.runtime_persistence["native_draft_spec_review_resume"]["native_status"],
            "rejected",
        )

    def test_gateway_native_draft_spec_review_resume_requires_canonical_draft_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_draft_spec_resume_gate") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_native_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_draft_spec_resume_gate",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_draft_spec_resume_gate")
        graph_state.datasets["ADAE"].current_interrupt = InterruptState(
            name="code_review",
            dataset="ADAE",
            reason="Canonical state is no longer waiting for draft-spec review.",
        )
        gateway._persist_graph_state(study_dir, graph_state, node="test_native_draft_spec_resume_gate")

        with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
            gateway.resume_native_draft_spec_review(
                study_dir=study_dir,
                run_id="run_lg2_native_draft_spec_resume_gate",
                dataset="ADAE",
                decision="approve",
                reviewer="native_tester",
            )

        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_lg2_native_draft_spec_resume_gate"
                / "reviews"
                / "adae_draft_spec_review.json"
            ).exists()
        )
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_draft_spec_resume_gate")
        self.assertEqual(reloaded.datasets["ADAE"].spec_state["status"], "draft_generated")

    def test_gateway_native_draft_spec_review_resume_rejects_study_interrupt_before_native_resume(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_draft_spec_study_gate") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_native_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_draft_spec_study_gate",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_draft_spec_study_gate")
        graph_state.current_interrupt = InterruptState(
            name="dependency_review",
            reason="Study dependency review was reopened before dataset review resumed.",
        )
        graph_state.dependency_review_status = "review_required"
        gateway._persist_graph_state(study_dir, graph_state, node="test_native_draft_spec_study_gate")

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            with self.assertRaisesRegex(ValueError, "Study-level interrupt dependency_review must be resolved"):
                gateway.resume_native_draft_spec_review(
                    study_dir=study_dir,
                    run_id="run_lg2_native_draft_spec_study_gate",
                    dataset="ADAE",
                    decision="approve",
                    reviewer="native_tester",
                )

        compile_graph.assert_not_called()
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_lg2_native_draft_spec_study_gate"
                / "reviews"
                / "adae_draft_spec_review.json"
            ).exists()
        )
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_draft_spec_study_gate")
        self.assertEqual(reloaded.current_interrupt.name, "dependency_review")
        self.assertEqual(reloaded.datasets["ADAE"].spec_state["status"], "draft_generated")

    def test_gateway_mark_inputs_changed_updates_canonical_state_and_projection(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_mark_inputs_changed") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_mark_inputs_changed",
            target_datasets=["ADAE"],
        )
        input_ready = gateway.record_input_spec_ready(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_mark_inputs_changed",
            dataset="ADAE",
            input_spec_path=spec_dir / "adae.json",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        input_ready_state = input_ready.graph_state.datasets["ADAE"]
        self.assertEqual(input_ready_state.agent_node_inputs[0]["agent"], "evidence_agent")
        self.assertEqual(input_ready_state.agent_node_outputs[0]["decision"], "input_spec_ready")
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        result = gateway.mark_inputs_changed(study_dir=study_dir, run_id="run_lg2_mark_inputs_changed")

        graph_state = result.graph_state
        dataset_state = graph_state.datasets["ADAE"]
        workflow_state = json.loads((study_dir / "runs" / "run_lg2_mark_inputs_changed" / "workflow_state.json").read_text(encoding="utf-8"))
        consistency = workflow_projection_consistency(workflow_state, graph_state)
        self.assertTrue(graph_state.dependency_plan["plan_stale"])
        self.assertIn("input_sdtm/ae.csv", graph_state.dependency_plan["input_diff"]["changed_files"])
        self.assertEqual(graph_state.dependency_review_status, "stale")
        self.assertEqual(graph_state.current_interrupt.name, "dependency_review")
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(dataset_state.spec_state["status"], "stale")
        self.assertTrue(consistency["consistent"], consistency["mismatches"])

        with self.assertRaisesRegex(ValueError, "dependency plan is stale"):
            gateway.dependency_gate_for_product_step(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_mark_inputs_changed",
                dataset="ADAE",
            )

    def test_gateway_mark_inputs_changed_raises_for_legacy_run_without_graph_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_mark_inputs_changed_legacy") / "PSY201"
        study_dir.mkdir(parents=True)

        with self.assertRaisesRegex(ValueError, "Graph state does not exist"):
            GraphGateway().mark_inputs_changed(study_dir=study_dir, run_id="run_no_graph_state")

    def test_gateway_mark_all_inputs_changed_scans_canonical_graph_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_mark_all_inputs_changed") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        sdtm_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_graph_state_only",
            target_datasets=["ADAE"],
        )
        (study_dir / "runs" / "run_graph_state_only" / "workflow_state.json").unlink()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        result = gateway.mark_all_inputs_changed(study_dir=study_dir)

        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_state_only")
        workflow_path = study_dir / "runs" / "run_graph_state_only" / "workflow_state.json"
        self.assertEqual(result.touched_graph_runs, ["run_graph_state_only"])
        self.assertEqual(result.skipped_graph_runs, [])
        self.assertTrue(graph_state.dependency_plan["plan_stale"])
        self.assertEqual(graph_state.dependency_review_status, "stale")
        self.assertTrue(workflow_path.exists())

    def test_gateway_mark_all_inputs_changed_preserves_existing_stale_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_mark_all_preserves_stale") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        sdtm_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_preserve_stale",
            target_datasets=["ADAE"],
        )
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")
        first = gateway.mark_all_inputs_changed(study_dir=study_dir)

        second = gateway.mark_all_inputs_changed(study_dir=study_dir)

        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id="run_preserve_stale")
        self.assertEqual(first.touched_graph_runs, ["run_preserve_stale"])
        self.assertEqual(second.touched_graph_runs, [])
        self.assertTrue(graph_state.dependency_plan["plan_stale"])
        self.assertEqual(graph_state.dependency_review_status, "stale")

    def test_gateway_mark_all_inputs_changed_reports_corrupt_graph_state_as_skipped(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_mark_all_skips_corrupt") / "PSY201"
        run_dir = study_dir / "runs" / "run_corrupt"
        run_dir.mkdir(parents=True)
        (run_dir / "graph_state.json").write_text("{not-json", encoding="utf-8")

        result = GraphGateway().mark_all_inputs_changed(study_dir=study_dir)

        self.assertEqual(result.touched_graph_runs, [])
        self.assertEqual(result.skipped_graph_runs, ["run_corrupt"])

    def test_gateway_mark_study_inputs_changed_returns_legacy_and_graph_touches(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_mark_study_inputs_changed") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        sdtm_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_upload_gateway_boundary",
            target_datasets=["ADAE"],
        )
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        result = gateway.mark_study_inputs_changed(study_dir=study_dir)

        self.assertIn("digest", result.input_fingerprint)
        self.assertTrue(result.input_diff["changed"])
        self.assertEqual(result.touched_runs, ["run_upload_gateway_boundary"])
        self.assertEqual(result.touched_graph_runs, ["run_upload_gateway_boundary"])
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id="run_upload_gateway_boundary")
        workflow_state = json.loads((study_dir / "runs" / "run_upload_gateway_boundary" / "workflow_state.json").read_text(encoding="utf-8"))
        consistency = workflow_projection_consistency(workflow_state, graph_state)
        self.assertEqual(graph_state.dependency_review_status, "stale")
        self.assertTrue(consistency["consistent"], consistency["mismatches"])

    def test_gateway_dependency_gate_starts_plan_and_blocks_unresolved_dependency(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_dependency_gate") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        (legacy_dir / "ADAE.sas").write_text("data adae; merge ae adsl; by usubjid; run;", encoding="utf-8")

        gateway = GraphGateway()

        with self.assertRaisesRegex(ValueError, "cannot continue until dependency issues are resolved"):
            gateway.dependency_gate_for_product_step(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_gateway_dependency_gate",
                dataset="ADAE",
            )

        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_gateway_dependency_gate")
        self.assertEqual(graph_state.dependency_review_status, "blocked")
        self.assertIn("ADAE", graph_state.target_datasets)

    def test_gateway_dependency_gate_returns_plan_when_open(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_dependency_gate_open") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()

        gate = gateway.dependency_gate_for_product_step(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_gateway_dependency_gate_open",
            dataset="ADAE",
        )

        self.assertEqual(gate.dependency_review_status, "accepted")
        self.assertIn("ADAE", gate.runnable_datasets)
        self.assertEqual(gate.dependency_resolution, [])

    def test_gateway_generate_code_does_not_accept_external_dependency_artifacts(self) -> None:
        signature = inspect.signature(GraphGateway.generate_code)

        self.assertNotIn("dependency_artifacts", signature.parameters)
        self.assertNotIn("dependency_resolution", signature.parameters)

    def test_gateway_product_spec_methods_do_not_accept_external_dependency_resolution(self) -> None:
        for method in [GraphGateway.finalize_inputs, GraphGateway.generate_draft_spec]:
            signature = inspect.signature(method)
            self.assertNotIn("dependency_resolution", signature.parameters)

    def test_gateway_records_execution_agent_decision_in_canonical_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_execution_agent_decision") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()

        result = gateway.record_execution(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_execution_agent_decision",
            dataset="ADAE",
            execution_state={
                "status": "completed",
                "validation_status": "passed",
                "output_path": "runs/run_lg2_execution_agent_decision/outputs/adae.csv",
                "terminal_failure": False,
                "partial_output_usable": True,
            },
            validation_summary={"status": "passed"},
            artifacts=[],
            failures=[],
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_execution_agent_decision" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(dataset_state.status, "completed")
        self.assertEqual(dataset_state.agent_decisions[0]["agent"], "execution_agent")
        self.assertEqual(dataset_state.agent_decisions[0]["decision"], "r_execution_completed")
        self.assertEqual(dataset_state.agent_node_inputs[0]["agent"], "execution_agent")
        self.assertEqual(dataset_state.agent_node_outputs[0]["decision"], "r_execution_completed")
        self.assertEqual(dataset_state.agent_node_outputs[0]["agent_decisions"][0], dataset_state.agent_decisions[0])
        self.assertEqual(result.graph_state.agent_decisions[-1]["agent"], "execution_agent")
        self.assertIn("execution_agent", [item["agent"] for item in result.graph_state.agent_node_outputs])
        self.assertEqual(result.graph_state.agent_audit_summary["datasets"]["ADAE"]["status"], "completed")
        self.assertEqual(dataset_state.agent_audit_summary["agent_counts"]["execution_agent"], 1)
        self.assertEqual(workflow_state["datasets"]["ADAE"]["agent_decisions"][0]["agent"], "execution_agent")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["agent_audit_summary"]["decision_count"], 1)

    def test_gateway_generates_code_through_dataset_graph_and_records_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_generate_code") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_gateway_generate_code"
        code_dir = run_dir / "code"
        llm_dir = run_dir / "llm"
        spec_dir = study_dir / "input_spec"
        code_dir.mkdir(parents=True)
        llm_dir.mkdir()
        spec_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text(
            "dir.create('outputs', showWarnings = FALSE)\n"
            "write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_gateway_generate_code",
            "ADAE",
            code_path,
        )
        response_path = llm_dir / "adae_llm_response.json"
        parsed_path = llm_dir / "adae_parsed_response.json"
        spec_path = spec_dir / "ads_adae_full.json"
        response_path.write_text(json.dumps({"dataset": "ADAE"}), encoding="utf-8")
        parsed_path.write_text(json.dumps({"dataset": "ADAE", "r_code": code_path.read_text(encoding="utf-8")}), encoding="utf-8")
        spec_path.write_text(json.dumps({"dataset": "ADAE", "variables": [{"variable": "USUBJID"}]}), encoding="utf-8")
        gateway = GraphGateway()

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "needs_review",
                "code_path": str(code_path.as_posix()),
                "generated_code": code_path.read_text(encoding="utf-8"),
                "static_check_path": str(static_path.as_posix()),
                "input_spec_path": str(spec_path.as_posix()),
                "spec_source": "input_spec",
                "llm_response_path": str(response_path.as_posix()),
                "parsed_response_path": str(parsed_path.as_posix()),
                "code_assumptions": ["Assumption under review."],
                "code_risk_points": ["Review generated derivation."],
                "code_used_inputs": ["AE"],
                "code_expected_outputs": ["outputs/adae.csv"],
                "product_context_warnings": ["Context warning."],
                "agent_decisions": [],
                "agent_node_inputs": [
                    {
                        "agent": "code_agent",
                        "node": "code_generation",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_generate_code",
                        "dataset": "ADAE",
                        "task": "Generate R code from the approved ADaM spec.",
                        "inputs": {"spec_source": "input_spec"},
                        "created_at": "2026-05-31T00:01:01Z",
                    },
                    {
                        "agent": "static_review_agent",
                        "node": "code_generation",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_generate_code",
                        "dataset": "ADAE",
                        "task": "Run deterministic static checks on generated R code.",
                        "artifact_ids": ["static_check_adae"],
                        "created_at": "2026-05-31T00:01:02Z",
                    },
                ],
                "agent_node_outputs": [
                    {
                        "agent": "code_agent",
                        "node": "code_generation",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_generate_code",
                        "dataset": "ADAE",
                        "status": "needs_review",
                        "decision": "r_code_generated",
                        "outputs": {"code_path": str(code_path.as_posix())},
                        "created_at": "2026-05-31T00:01:03Z",
                    },
                    {
                        "agent": "static_review_agent",
                        "node": "code_generation",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_generate_code",
                        "dataset": "ADAE",
                        "status": "warning",
                        "decision": "static_check_recorded",
                        "outputs": {"static_check_path": str(static_path.as_posix())},
                        "risk_flags": ["static_check_limited_scope"],
                        "created_at": "2026-05-31T00:01:04Z",
                    },
                ],
                "risk_flags": [],
            }
            result = gateway.generate_code(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_gateway_generate_code",
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
                rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
            )

        compile_graph.assert_called_once()
        invoked_state = compile_graph.return_value.invoke.call_args.args[0]
        self.assertEqual(invoked_state["execution_mode"], "graph_product_generate_code")
        self.assertEqual(invoked_state["dataset"], "ADAE")
        self.assertEqual(invoked_state["study_dir"], str(study_dir))
        self.assertEqual(result.code_path, str(code_path.as_posix()))
        self.assertEqual(result.static_check_path, str(static_path.as_posix()))
        self.assertEqual(result.draft_spec_path, str(spec_path.as_posix()))
        self.assertEqual(result.warnings, ["Context warning."])

        dataset_state = result.graph_state.datasets["ADAE"]
        workflow_state = json.loads(
            (run_dir / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(dataset_state.code_state["static_check_sha256"], static_sha)
        self.assertTrue(dataset_state.code_state["generation_quality"]["not_real_derivation"])
        self.assertEqual(dataset_state.code_state["generation_quality"]["llm_provider"], "mock")
        self.assertEqual([item["agent"] for item in dataset_state.agent_node_inputs], ["code_agent", "static_review_agent"])
        self.assertEqual([item["agent"] for item in dataset_state.agent_node_outputs], ["code_agent", "static_review_agent"])
        self.assertEqual(
            [item["agent"] for item in result.graph_state.agent_node_outputs],
            ["dependency_agent", "code_agent", "static_review_agent"],
        )
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["current_interrupt"], "code_review")
        persisted_state = json.loads((run_dir / "graph_state.json").read_text(encoding="utf-8"))
        self.assertEqual(
            [item["decision"] for item in persisted_state["datasets"]["ADAE"]["agent_node_outputs"]],
            ["r_code_generated", "static_check_recorded"],
        )
        self.assertEqual(
            [item["decision"] for item in persisted_state["agent_node_outputs"]],
            ["dependency_plan_prepared", "r_code_generated", "static_check_recorded"],
        )

    def test_gateway_native_code_review_roundtrip_persists_formal_review_artifact(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_roundtrip") / "PSY201"
        spec_dir = study_dir / "input_spec"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir.mkdir(parents=True)
        sdtm_dir.mkdir()
        (sdtm_dir / "dm.csv").write_text("USUBJID,ARM\n01,Placebo\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()

        started = gateway.start_native_code_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_roundtrip",
            dataset="ADSL",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
        )

        dataset_state = started.graph_state.datasets["ADSL"]
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertIn("native_code_review_interrupt", started.graph_state.runtime_persistence)
        self.assertEqual(
            started.graph_state.runtime_persistence["native_code_review_interrupt"]["next_nodes"],
            ["wait_for_code_review"],
        )

        reviewed = gateway.resume_native_code_review(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_roundtrip",
            dataset="ADSL",
            decision="approve",
            reviewer="native_tester",
            notes="Native code review gateway roundtrip approved.",
        )

        review_path = Path(reviewed.review_path)
        review_payload = json.loads(review_path.read_text(encoding="utf-8"))
        reviewed_dataset = reviewed.graph_state.datasets["ADSL"]
        self.assertTrue(reviewed.approved)
        self.assertEqual(review_payload["decision"], "approve")
        self.assertEqual(review_payload["reviewer"], "native_tester")
        self.assertEqual(reviewed_dataset.code_state["status"], "approved")
        self.assertIsNone(reviewed_dataset.current_interrupt)
        self.assertEqual(reviewed_dataset.human_commands[-1].interrupt, "code_review")
        self.assertEqual(reviewed.graph_state.runtime_persistence["native_code_review_resume"]["native_status"], "approved")
        persisted_state = json.loads(
            (study_dir / "runs" / "run_lg2_native_code_review_roundtrip" / "graph_state.json").read_text(
                encoding="utf-8"
            )
        )
        self.assertEqual(persisted_state["datasets"]["ADSL"]["code_state"]["status"], "approved")
        self.assertTrue((study_dir / "runs" / "run_lg2_native_code_review_roundtrip" / "review" / "adsl_code_review.json").exists())

    def test_gateway_native_code_review_reject_roundtrip_keeps_execution_locked(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_reject_roundtrip") / "PSY201"
        spec_dir = study_dir / "input_spec"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir.mkdir(parents=True)
        sdtm_dir.mkdir()
        (sdtm_dir / "dm.csv").write_text("USUBJID,ARM\n01,Placebo\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_native_code_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_reject_roundtrip",
            dataset="ADSL",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
        )

        reviewed = gateway.resume_native_code_review(
            study_dir=study_dir,
            run_id="run_lg2_native_code_review_reject_roundtrip",
            dataset="ADSL",
            decision="reject",
            reviewer="native_tester",
            notes="Generated code needs revision.",
        )

        review_payload = json.loads(Path(reviewed.review_path).read_text(encoding="utf-8"))
        dataset_state = reviewed.graph_state.datasets["ADSL"]
        self.assertFalse(reviewed.approved)
        self.assertEqual(review_payload["decision"], "reject")
        self.assertEqual(dataset_state.code_state["status"], "rejected")
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(reviewed.graph_state.runtime_persistence["native_code_review_resume"]["native_status"], "rejected")

    def test_gateway_native_code_review_resume_requires_canonical_code_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_code_review_resume_gate") / "PSY201"
        spec_dir = study_dir / "input_spec"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir.mkdir(parents=True)
        sdtm_dir.mkdir()
        (sdtm_dir / "dm.csv").write_text("USUBJID,ARM\n01,Placebo\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_native_code_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_code_review_resume_gate",
            dataset="ADSL",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
        )
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_code_review_resume_gate")
        graph_state.datasets["ADSL"].current_interrupt = InterruptState(
            name="draft_spec_review",
            dataset="ADSL",
            reason="Canonical state is no longer waiting for code review.",
        )
        gateway._persist_graph_state(study_dir, graph_state, node="test_native_code_review_resume_gate")

        with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
            gateway.resume_native_code_review(
                study_dir=study_dir,
                run_id="run_lg2_native_code_review_resume_gate",
                dataset="ADSL",
                decision="approve",
                reviewer="native_tester",
            )

        self.assertFalse(
            (study_dir / "runs" / "run_lg2_native_code_review_resume_gate" / "review" / "adsl_code_review.json").exists()
        )
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_native_code_review_resume_gate")
        self.assertEqual(reloaded.datasets["ADSL"].code_state["status"], "generated")

    def test_gateway_generate_code_uses_gateway_owned_dependency_plan(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_owned_dependency_plan") / "PSY201"
        run_id = "run_lg2_gateway_owned_dependency_plan"
        run_dir = study_dir / "runs" / run_id
        output_dir = run_dir / "outputs"
        code_dir = run_dir / "code"
        spec_dir = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        output_dir.mkdir(parents=True)
        code_dir.mkdir()
        spec_dir.mkdir(parents=True)
        reference_dir.mkdir()
        adsl_path = output_dir / "adsl.csv"
        reference_adsl_path = reference_dir / "adsl.csv"
        adsl_path.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        reference_adsl_path.write_text("USUBJID,TRTSDT\n99,2099-01-01\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        code_path = code_dir / "build_adae.R"
        code_path.write_text(
            "dir.create('outputs', showWarnings = FALSE)\n"
            "write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )
        static_path, static_sha = _write_static_check_for_code(study_dir, run_id, "ADAE", code_path)
        gateway = GraphGateway()
        gateway.record_execution(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADSL",
            execution_state={
                "status": "completed",
                "terminal_failure": False,
                "partial_output_usable": True,
                "output_path": str(adsl_path.as_posix()),
            },
            validation_summary={"status": "passed"},
            artifacts=[
                ArtifactRef(
                    artifact_id="output_adam_psy201_run_lg2_gateway_owned_dependency_plan_adsl",
                    kind="output_adam",
                    path=str(adsl_path.as_posix()),
                    sha256=f"sha256:{sha256_file(adsl_path)}",
                    dataset="ADSL",
                    format="csv",
                    role="output",
                )
            ],
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            target_datasets=["ADAE"],
        )

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "needs_review",
                "code_path": str(code_path.as_posix()),
                "generated_code": code_path.read_text(encoding="utf-8"),
                "static_check_path": str(static_path.as_posix()),
                "input_spec_path": str((spec_dir / "adae.json").as_posix()),
                "spec_source": "input_spec",
                "agent_decisions": [],
                "risk_flags": [],
            }
            result = gateway.generate_code(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        invoked_state = compile_graph.return_value.invoke.call_args.args[0]
        resolution = invoked_state["dependency_resolution"]
        self.assertEqual(len(resolution), 1)
        self.assertEqual(resolution[0]["target_dataset"], "ADAE")
        self.assertEqual(resolution[0]["required_dataset"], "ADSL")
        self.assertEqual(resolution[0]["resolution_status"], "available")
        self.assertEqual(resolution[0]["artifact_source"], "run_output")
        self.assertEqual(resolution[0]["artifact_path"], str(adsl_path.as_posix()))
        self.assertNotEqual(resolution[0]["artifact_path"], str(reference_adsl_path.as_posix()))

        dependency_artifacts = result.graph_state.datasets["ADAE"].code_state["dependency_artifacts"]
        self.assertEqual(
            dependency_artifacts,
            [
                {
                    "required_dataset": "ADSL",
                    "artifact_path": str(adsl_path.as_posix()),
                    "artifact_sha256": f"sha256:{sha256_file(adsl_path)}",
                    "artifact_source": "run_output",
                }
            ],
        )
        self.assertEqual(result.graph_state.datasets["ADSL"].status, "completed")
        self.assertEqual(result.graph_state.dependency_review_status, "accepted")

    def test_gateway_executes_approved_code_through_dataset_graph_and_records_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_execute_approved_code") / "PSY201"
        run_id = "run_lg2_gateway_execute"
        run_dir = study_dir / "runs" / run_id
        code_dir = run_dir / "code"
        review_dir = run_dir / "review"
        code_dir.mkdir(parents=True)
        review_dir.mkdir()
        code_path = code_dir / "build_adae.R"
        review_path = review_dir / "adae_code_review.json"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, run_id, "ADAE", code_path)
        code_sha = f"sha256:{sha256_file(code_path)}"
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        gateway.record_code_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            command=HumanCommand(
                interrupt="code_review",
                action="approve",
                dataset="ADAE",
                reviewer="tester",
                notes="Approved graph-native execution test.",
            ),
            review_path=review_path,
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass"},
                "output_path": "runs/run_lg2_gateway_execute/outputs/adae.csv",
                "validation_report_path": "runs/run_lg2_gateway_execute/validation/adae_validation_report.json",
                "diagnostics_path": "",
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [
                    {
                        "agent": "execution_agent",
                        "node": "execute_approved_code",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_execute",
                        "dataset": "ADAE",
                        "task": "Execute approved generated R code in the configured R boundary and report validation status.",
                        "inputs": {
                            "code_path": str(code_path.as_posix()),
                            "static_check_path": str(static_path.as_posix()),
                            "rscript_path_provided": True,
                        },
                        "created_at": "2026-05-31T00:02:01Z",
                    }
                ],
                "agent_node_outputs": [
                    {
                        "agent": "execution_agent",
                        "node": "execute_approved_code",
                        "study_id": "PSY201",
                        "run_id": "run_lg2_gateway_execute",
                        "dataset": "ADAE",
                        "status": "completed",
                        "decision": "r_execution_completed",
                        "reason": "Executed approved generated R code in the configured R boundary.",
                        "outputs": {
                            "output_path": "runs/run_lg2_gateway_execute/outputs/adae.csv",
                            "validation_status": "pass",
                            "terminal_failure": False,
                        },
                        "created_at": "2026-05-31T00:02:02Z",
                    }
                ],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }

            result = gateway.execute_approved_code(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
                rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
            )

        compile_graph.assert_called_once()
        invoked_state = compile_graph.return_value.invoke.call_args.args[0]
        self.assertEqual(invoked_state["execution_mode"], "graph_product_execute")
        self.assertEqual(invoked_state["dataset"], "ADAE")
        self.assertEqual(invoked_state["study_dir"], str(study_dir))
        self.assertEqual(result.status, "completed")
        self.assertEqual(result.validation_status, "pass")
        self.assertFalse(result.terminal_failure)

        dataset_state = result.graph_state.datasets["ADAE"]
        workflow_state = json.loads(
            (run_dir / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(dataset_state.status, "completed")
        self.assertEqual(dataset_state.execution_state["status"], "completed")
        self.assertEqual(dataset_state.execution_state["output_path"], "runs/run_lg2_gateway_execute/outputs/adae.csv")
        self.assertEqual(dataset_state.execution_state["generation_quality"], {})
        self.assertFalse(dataset_state.execution_state["not_real_derivation"])
        self.assertIn("execution_agent", [item["agent"] for item in dataset_state.agent_decisions])
        self.assertEqual(dataset_state.agent_node_inputs[-1]["agent"], "execution_agent")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["decision"], "r_execution_completed")
        self.assertEqual(result.graph_state.agent_node_outputs[-1]["agent"], "execution_agent")
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["status"], "completed")
        persisted_state = json.loads((run_dir / "graph_state.json").read_text(encoding="utf-8"))
        self.assertEqual(
            persisted_state["datasets"]["ADAE"]["agent_node_outputs"][-1]["decision"],
            "r_execution_completed",
        )
        self.assertEqual(persisted_state["agent_node_outputs"][-1]["decision"], "r_execution_completed")

    def test_gateway_execution_preserves_generation_quality_signal(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_execute_generation_quality") / "PSY201"
        run_id = "run_lg2_execute_generation_quality"
        run_dir = study_dir / "runs" / run_id
        code_dir = run_dir / "code"
        review_dir = run_dir / "review"
        code_dir.mkdir(parents=True)
        review_dir.mkdir()
        code_path = code_dir / "build_adsl.R"
        review_path = review_dir / "adsl_code_review.json"
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adsl.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, run_id, "ADSL", code_path)
        code_sha = f"sha256:{sha256_file(code_path)}"
        generation_quality = {
            "llm_provider": "mock",
            "llm_model": "mock-model",
            "provider_alias": "mock",
            "transport": "mock",
            "not_real_derivation": True,
        }
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADSL",
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            generation_quality=generation_quality,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        gateway.record_code_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADSL",
            command=HumanCommand(
                interrupt="code_review",
                action="approve",
                dataset="ADSL",
                reviewer="tester",
                notes="Approved mock-quality propagation test.",
            ),
            review_path=review_path,
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass"},
                "output_path": "runs/run_lg2_execute_generation_quality/outputs/adsl.csv",
                "validation_report_path": "runs/run_lg2_execute_generation_quality/validation/adsl_validation_report.json",
                "diagnostics_path": "",
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            result = gateway.execute_approved_code(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADSL",
            )

        dataset_state = result.graph_state.datasets["ADSL"]
        self.assertEqual(dataset_state.execution_state["generation_quality"], generation_quality)
        self.assertTrue(dataset_state.execution_state["not_real_derivation"])

    def test_gateway_execute_requires_graph_approved_code_before_dataset_graph_invocation(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_execute_requires_code_review") / "PSY201"
        run_id = "run_lg2_execute_requires_code_review"
        code_dir = study_dir / "runs" / run_id / "code"
        code_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, run_id, "ADAE", code_path)
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            with self.assertRaisesRegex(ValueError, "approved code-review decision"):
                gateway.execute_approved_code(
                    study_dir=study_dir,
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
                )

        compile_graph.assert_not_called()

    def test_gateway_draft_spec_review_rejects_changed_draft_hash(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_draft_spec_hash") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_draft_spec_hash"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = run_dir / "specs"
        review_dir = run_dir / "reviews"
        approved_dir = run_dir / "approved_specs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        review_dir.mkdir()
        approved_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        approved_path = approved_dir / "adae_approved_spec.json"
        approved_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint, "status": "approved_draft"}),
            encoding="utf-8",
        )
        approved_sha = f"sha256:{sha256_file(approved_path)}"
        review_path = review_dir / "adae_draft_spec_review.json"
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_draft_spec_hash",
            dataset="ADAE",
            draft_spec_path=draft_path,
            input_fingerprint_payload=fingerprint,
        )
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TAMPERED"}], "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )

        with self.assertRaisesRegex(ValueError, "Draft spec changed after graph draft generation"):
            gateway.record_draft_spec_review(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_draft_spec_hash",
                dataset="ADAE",
                command=HumanCommand(
                    interrupt="draft_spec_review",
                    action="approve",
                    dataset="ADAE",
                    reviewer="tester",
                ),
                review_path=review_path,
                draft_spec_path=draft_path,
                approved_spec_path=approved_path,
                approved_spec_sha256=approved_sha,
                input_fingerprint_payload=fingerprint,
            )

    def test_gateway_draft_spec_approval_requires_approved_artifact_and_hash(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_draft_spec_no_artifact") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_draft_spec_no_artifact"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = run_dir / "specs"
        review_dir = run_dir / "reviews"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        review_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        review_path = review_dir / "adae_draft_spec_review.json"
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_draft_spec_no_artifact",
            dataset="ADAE",
            draft_spec_path=draft_path,
            input_fingerprint_payload=fingerprint,
        )

        with self.assertRaisesRegex(ValueError, "requires an approved spec artifact and hash"):
            gateway.record_draft_spec_review(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_draft_spec_no_artifact",
                dataset="ADAE",
                command=HumanCommand(
                    interrupt="draft_spec_review",
                    action="approve",
                    dataset="ADAE",
                    reviewer="tester",
                ),
                review_path=review_path,
                draft_spec_path=draft_path,
                input_fingerprint_payload=fingerprint,
            )

    def test_gateway_preserves_other_dataset_interrupt_after_draft_approval(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_preserve_interrupt") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_preserve_interrupt"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = run_dir / "specs"
        review_dir = run_dir / "reviews"
        approved_dir = run_dir / "approved_specs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        review_dir.mkdir()
        approved_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        approved_path = approved_dir / "adae_approved_spec.json"
        approved_path.write_text(
            json.dumps({"dataset": "ADAE", "variables": [], "input_fingerprint": fingerprint, "status": "approved_draft"}),
            encoding="utf-8",
        )
        approved_sha = f"sha256:{sha256_file(approved_path)}"
        review_path = review_dir / "adae_draft_spec_review.json"
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_preserve_interrupt",
            dataset="ADAE",
            draft_spec_path=draft_path,
            input_fingerprint_payload=fingerprint,
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_preserve_interrupt").model_copy(deep=True)
        state.datasets["ADCM"] = DatasetRunState(
            study_id="PSY201",
            run_id="run_lg2_preserve_interrupt",
            dataset="ADCM",
            status="needs_review",
            current_interrupt=InterruptState(name="code_review", dataset="ADCM", reason="Review ADCM code."),
            input_fingerprint=fingerprint,
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_other_interrupt")

        reviewed = gateway.record_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_preserve_interrupt",
            dataset="ADAE",
            command=HumanCommand(
                interrupt="draft_spec_review",
                action="approve",
                dataset="ADAE",
                reviewer="tester",
            ),
            review_path=review_path,
            draft_spec_path=draft_path,
            approved_spec_path=approved_path,
            approved_spec_sha256=approved_sha,
            input_fingerprint_payload=fingerprint,
        )

        self.assertEqual(reviewed.graph_state.current_interrupt.name, "code_review")
        self.assertEqual(reviewed.graph_state.current_interrupt.dataset, "ADCM")

    def test_gateway_code_review_rejects_changed_spec_hash(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_spec_hash") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_spec_hash"
        code_dir = run_dir / "code"
        review_dir = run_dir / "review"
        static_dir = run_dir / "static_checks"
        spec_dir = study_dir / "input_spec"
        code_dir.mkdir(parents=True)
        review_dir.mkdir()
        static_dir.mkdir()
        spec_dir.mkdir(parents=True)
        code_path = code_dir / "build_adae.R"
        review_path = review_dir / "adae_code_review.json"
        spec_path = spec_dir / "adae.json"
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_spec_hash", "ADAE", code_path)
        spec_path.write_text(json.dumps({"dataset": "ADAE", "variables": []}), encoding="utf-8")
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        code_sha = f"sha256:{sha256_file(code_path)}"
        spec_sha = f"sha256:{sha256_file(spec_path)}"
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_spec_hash",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            spec_source="input_spec",
            spec_path=spec_path,
            spec_sha256=spec_sha,
        )
        spec_path.write_text(json.dumps({"dataset": "ADAE", "variables": [{"variable": "TAMPERED"}]}), encoding="utf-8")

        with self.assertRaisesRegex(ValueError, "Approved spec changed after graph code generation"):
            gateway.record_code_review(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_spec_hash",
                dataset="ADAE",
                command=HumanCommand(
                    interrupt="code_review",
                    action="approve",
                    dataset="ADAE",
                    reviewer="tester",
                    notes="Should fail.",
                    payload={
                        "review_path": str(review_path.as_posix()),
                        "code_path": str(code_path.as_posix()),
                        "code_sha256": code_sha,
                        "static_check_path": str(static_path.as_posix()),
                        "static_check_sha256": static_sha,
                        "spec_source": "input_spec",
                        "spec_path": str(spec_path.as_posix()),
                        "spec_sha256": spec_sha,
                    },
                ),
                review_path=review_path,
                code_path=code_path,
                code_sha256=code_sha,
                static_check_path=static_path,
                static_check_sha256=static_sha,
            )

    def test_gateway_code_generation_requires_graph_approved_draft_spec(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_codegen_requires_graph_draft") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_codegen_requires_graph_draft"
        code_dir = run_dir / "code"
        approved_dir = run_dir / "approved_specs"
        code_dir.mkdir(parents=True)
        approved_dir.mkdir()
        approved_path = approved_dir / "adae_approved_spec.json"
        code_path = code_dir / "build_adae.R"
        approved_path.write_text(json.dumps({"dataset": "ADAE", "variables": []}), encoding="utf-8")
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_codegen_requires_graph_draft", "ADAE", code_path)

        with self.assertRaisesRegex(ValueError, "recorded in graph state"):
            GraphGateway().record_code_generation(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_codegen_requires_graph_draft",
                dataset="ADAE",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
                static_check_path=static_path,
                static_check_sha256=static_sha,
                spec_source="approved_draft_spec",
                spec_path=approved_path,
                spec_sha256=f"sha256:{sha256_file(approved_path)}",
            )

    def test_gateway_code_generation_requires_static_check_artifact(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_codegen_requires_static") / "PSY201"
        code_path = study_dir / "runs" / "run_lg2_codegen_requires_static" / "code" / "build_adae.R"
        code_path.parent.mkdir(parents=True)
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")

        with self.assertRaisesRegex(ValueError, "requires a static-check artifact"):
            GraphGateway().record_code_generation(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_codegen_requires_static",
                dataset="ADAE",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
            )

    def test_gateway_code_generation_rejects_blocked_static_check(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_codegen_rejects_blocked_static") / "PSY201"
        code_path = study_dir / "runs" / "run_lg2_codegen_rejects_blocked_static" / "code" / "build_adae.R"
        code_path.parent.mkdir(parents=True)
        code_path.write_text("system('whoami')\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_codegen_rejects_blocked_static", "ADAE", code_path)

        with self.assertRaisesRegex(ValueError, "blocking findings"):
            GraphGateway().record_code_generation(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_codegen_rejects_blocked_static",
                dataset="ADAE",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
                static_check_path=static_path,
                static_check_sha256=static_sha,
            )

    def test_gateway_code_generation_rejects_static_check_bound_to_other_code(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_codegen_static_bound_code") / "PSY201"
        code_dir = study_dir / "runs" / "run_lg2_static_bound_code" / "code"
        code_dir.mkdir(parents=True)
        original_code = code_dir / "build_original.R"
        current_code = code_dir / "build_adae.R"
        original_code.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        current_code.write_text(
            "system('whoami')\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n",
            encoding="utf-8",
        )
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_static_bound_code",
            "ADAE",
            original_code,
        )

        with self.assertRaisesRegex(ValueError, "not bound to the current generated R code"):
            GraphGateway().record_code_generation(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_static_bound_code",
                dataset="ADAE",
                code_path=current_code,
                code_sha256=f"sha256:{sha256_file(current_code)}",
                static_check_path=static_path,
                static_check_sha256=static_sha,
            )

    def test_gateway_code_generation_rejects_incomplete_static_check_artifact(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_codegen_incomplete_static") / "PSY201"
        run_dir = study_dir / "runs" / "run_lg2_incomplete_static"
        code_path = run_dir / "code" / "build_adae.R"
        static_path = run_dir / "static_checks" / "adae_static_check.json"
        code_path.parent.mkdir(parents=True)
        static_path.parent.mkdir(parents=True)
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path.write_text(json.dumps({"status": "pass"}), encoding="utf-8")

        with self.assertRaisesRegex(ValueError, "missing required fields"):
            GraphGateway().record_code_generation(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_incomplete_static",
                dataset="ADAE",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
                static_check_path=static_path,
                static_check_sha256=f"sha256:{sha256_file(static_path)}",
            )

    def test_gateway_records_compare_summary_in_canonical_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare") / "PSY201"
        compare_dir = study_dir / "runs" / "run_lg2_compare" / "compare"
        compare_dir.mkdir(parents=True)
        report_path = compare_dir / "adae_compare_report.json"
        report_path.write_text(
            json.dumps({"dataset": "ADAE", "status": "missing_reference"}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare",
            target_datasets=["ADAE"],
        )

        result = gateway.record_compare(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare",
            dataset="ADAE",
            compare_summary={
                "dataset": "ADAE",
                "status": "missing_reference",
                "generated_file": "adae.csv",
                "note": "No reference ADaM was found for this dataset.",
            },
            compare_report_path=report_path,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_compare" / "workflow_state.json").read_text(encoding="utf-8")
        )
        dataset_state = result.graph_state.datasets["ADAE"]
        validation_decisions = [
            item for item in dataset_state.agent_decisions if item["agent"] == "validation_agent"
        ]
        self.assertEqual(dataset_state.compare_summary["status"], "missing_reference")
        self.assertEqual(dataset_state.result_summary.compare_status, "missing_reference")
        self.assertEqual(validation_decisions[0]["decision"], "reference_compare_recorded")
        self.assertEqual(validation_decisions[0]["status"], "missing_reference")
        self.assertEqual(validation_decisions[0]["outputs"]["compare_status"], "missing_reference")
        self.assertEqual(dataset_state.agent_node_inputs[-1]["agent"], "validation_agent")
        self.assertEqual(dataset_state.agent_node_inputs[-1]["node"], "compare_reference_output")
        self.assertEqual(dataset_state.agent_node_inputs[-1]["task"], "Record generated-vs-reference ADaM comparison evidence.")
        self.assertEqual(dataset_state.agent_node_inputs[-1]["inputs"]["reference_role"], "comparison_evidence_only")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["agent"], "validation_agent")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["decision"], "reference_compare_recorded")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["outputs"]["compare_status"], "missing_reference")
        self.assertIn("reference_compare_limited_scope", dataset_state.risk_flags)
        self.assertIn("compare_report_adae", [artifact.artifact_id for artifact in dataset_state.artifacts])
        self.assertIn("validation_agent", [item["agent"] for item in result.graph_state.agent_decisions])
        self.assertIn("validation_agent", [item["agent"] for item in result.graph_state.agent_node_inputs])
        self.assertIn("validation_agent", [item["agent"] for item in result.graph_state.agent_node_outputs])
        self.assertEqual(result.graph_state.agent_audit_summary["datasets"]["ADAE"]["agent_counts"]["validation_agent"], 1)
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["compare_summary"]["status"], "missing_reference")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["agent_audit_summary"]["agent_counts"]["validation_agent"], 1)
        persisted_state = json.loads(
            (study_dir / "runs" / "run_lg2_compare" / "graph_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(
            persisted_state["datasets"]["ADAE"]["agent_node_outputs"][-1]["decision"],
            "reference_compare_recorded",
        )
        self.assertEqual(persisted_state["agent_node_outputs"][-1]["agent"], "validation_agent")

    def test_gateway_compare_keeps_same_second_rerun_audit_records(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_same_second") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_same_second",
            target_datasets=["ADAE"],
        )

        fixed_now = datetime(2026, 5, 31, 0, 3, 0, tzinfo=UTC)
        with patch("adam_agent.agents.contracts.utc_now", return_value=fixed_now):
            gateway.record_compare(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_compare_same_second",
                dataset="ADAE",
                compare_summary={"dataset": "ADAE", "status": "missing_reference", "generated_file": "adae.csv"},
                input_fingerprint_payload=input_fingerprint(study_dir),
            )
            result = gateway.record_compare(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_compare_same_second",
                dataset="ADAE",
                compare_summary={"dataset": "ADAE", "status": "match", "generated_file": "adae.csv"},
                input_fingerprint_payload=input_fingerprint(study_dir),
            )

        dataset_state = result.graph_state.datasets["ADAE"]
        validation_decisions = [
            item
            for item in dataset_state.agent_decisions
            if item["agent"] == "validation_agent" and item["node"] == "compare_reference_output"
        ]
        validation_outputs = [
            item
            for item in dataset_state.agent_node_outputs
            if item["agent"] == "validation_agent" and item["node"] == "compare_reference_output"
        ]
        self.assertEqual([item["outputs"]["compare_status"] for item in validation_decisions], ["missing_reference", "match"])
        self.assertEqual([item["outputs"]["compare_status"] for item in validation_outputs], ["missing_reference", "match"])
        self.assertEqual(result.graph_state.agent_node_outputs[-1]["outputs"]["compare_status"], "match")
        self.assertEqual(dataset_state.result_summary.compare_status, "match")

    def test_gateway_writes_compare_report_artifact_when_requested(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_report_writer") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_report_writer",
            target_datasets=["ADAE"],
        )

        result = gateway.record_compare(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_report_writer",
            dataset="ADAE",
            compare_summary={
                "dataset": "ADAE",
                "status": "missing_reference",
                "generated_file": "adae.csv",
                "note": "No reference ADaM was found for this dataset.",
            },
            write_compare_report=True,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        report_path = study_dir / "runs" / "run_lg2_compare_report_writer" / "compare" / "adae_compare_report.json"
        report_payload = json.loads(report_path.read_text(encoding="utf-8"))
        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(report_payload["dataset"], "ADAE")
        self.assertEqual(report_payload["status"], "missing_reference")
        self.assertEqual(report_payload["report_path"], str(report_path.as_posix()))
        self.assertEqual(dataset_state.compare_summary["report_path"], str(report_path.as_posix()))
        self.assertIn("compare_report_adae", [artifact.artifact_id for artifact in dataset_state.artifacts])

    def test_gateway_compare_reference_output_computes_and_records_compare(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_entrypoint") / "PSY201"
        output_dir = study_dir / "runs" / "run_lg2_compare_entrypoint" / "outputs"
        validation_dir = study_dir / "runs" / "run_lg2_compare_entrypoint" / "validation"
        reference_dir = study_dir / "reference_adam"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir()
        reference_dir.mkdir(parents=True)
        (output_dir / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (validation_dir / "adae_validation_report.json").write_text(
            json.dumps({"dataset": "ADAE", "status": "pass"}),
            encoding="utf-8",
        )
        (reference_dir / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_entrypoint",
            target_datasets=["ADAE"],
        )

        result = gateway.compare_reference_output(
            study_dir=study_dir,
            run_id="run_lg2_compare_entrypoint",
            dataset="ADAE",
        )

        report_path = study_dir / "runs" / "run_lg2_compare_entrypoint" / "compare" / "adae_compare_report.json"
        self.assertEqual(result.compare_summary["status"], "match")
        self.assertTrue(report_path.exists())
        self.assertEqual(result.graph_state.datasets["ADAE"].compare_summary["status"], "match")
        self.assertEqual(result.graph_state.datasets["ADAE"].result_summary.compare_status, "match")
        self.assertEqual(result.graph_state.datasets["ADAE"].agent_node_inputs[-1]["agent"], "validation_agent")
        self.assertEqual(result.graph_state.datasets["ADAE"].agent_node_outputs[-1]["decision"], "reference_compare_recorded")

    def test_gateway_compare_requires_existing_graph_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_requires_state") / "PSY201"
        study_dir.mkdir(parents=True)

        with self.assertRaisesRegex(ValueError, "Graph state must exist"):
            GraphGateway().record_compare(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_compare_requires_state",
                dataset="ADAE",
                compare_summary={"dataset": "ADAE", "status": "missing_generated"},
                input_fingerprint_payload=input_fingerprint(study_dir),
            )

    def test_gateway_compare_does_not_refresh_dataset_product_fingerprint(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_preserve_product_fingerprint") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        sdtm_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_preserve_product_fingerprint",
            target_datasets=["ADAE"],
        )
        before = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_compare_preserve_product_fingerprint",
        ).datasets["ADAE"].input_fingerprint
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")
        current = input_fingerprint(study_dir)

        result = gateway.record_compare(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_preserve_product_fingerprint",
            dataset="ADAE",
            compare_summary={"dataset": "ADAE", "status": "missing_generated"},
            input_fingerprint_payload=current,
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(dataset_state.input_fingerprint["digest"], before["digest"])
        self.assertEqual(dataset_state.compare_summary["input_fingerprint"]["digest"], current["digest"])

    def test_gateway_compare_preserves_open_study_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_preserve_interrupt") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_preserve_interrupt",
            target_datasets=["ADAE"],
        )

        result = gateway.record_compare(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_preserve_interrupt",
            dataset="ADAE",
            compare_summary={"dataset": "ADAE", "status": "missing_generated"},
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        self.assertEqual(result.graph_state.current_interrupt.name, "dependency_review")
        self.assertEqual(result.graph_state.status, "needs_review")

    def test_gateway_compare_preserves_study_interrupt_over_dataset_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_preserve_study_interrupt") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_preserve_study_interrupt",
            target_datasets=["ADAE", "ADCM"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_compare_preserve_study_interrupt",
        ).model_copy(deep=True)
        state.datasets["ADCM"].current_interrupt = InterruptState(
            name="code_review",
            dataset="ADCM",
            reason="Review ADCM code.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_dataset_interrupt")

        result = gateway.record_compare(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_compare_preserve_study_interrupt",
            dataset="ADAE",
            compare_summary={"dataset": "ADAE", "status": "missing_generated"},
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        self.assertEqual(result.graph_state.current_interrupt.name, "dependency_review")
        self.assertIsNone(result.graph_state.current_interrupt.dataset)

    def test_gateway_records_terminal_failure_review_in_canonical_state(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_review") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_review",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_review",
        ).model_copy(deep=True)
        failure = FailureRecord(
            failure_id="failure_adae_runtime",
            dataset="ADAE",
            node="execute_approved_code",
            failure_type="sandbox_error",
            message="R execution failed.",
            root_cause="r_runtime_error",
            recommended_route="repair_code",
        )
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].failures = [failure]
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure")

        result = gateway.record_terminal_failure_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_review",
            dataset="ADAE",
            command=HumanCommand(
                interrupt="terminal_failure",
                action="repair_code",
                dataset="ADAE",
                reviewer="tester",
                notes="Repair generated R code.",
            ),
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        workflow_state = json.loads(
            (study_dir / "runs" / "run_lg2_terminal_failure_review" / "workflow_state.json").read_text(encoding="utf-8")
        )
        triage_decisions = [
            item for item in dataset_state.agent_decisions if item["agent"] == "diagnosis_repair_agent"
        ]
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "terminal_failure")
        self.assertEqual(dataset_state.execution_state["next_action"], "repair_generated_code")
        self.assertEqual(dataset_state.execution_state["terminal_failure_review"]["action"], "repair_code")
        self.assertEqual(dataset_state.human_commands[-1].action, "repair_code")
        self.assertEqual(dataset_state.result_summary.metadata["terminal_failure_next_action"], "repair_generated_code")
        self.assertEqual(triage_decisions[0]["decision"], "terminal_failure_triage_recorded")
        self.assertEqual(triage_decisions[0]["inputs"]["human_action"], "repair_code")
        self.assertEqual(triage_decisions[0]["outputs"]["next_action"], "repair_generated_code")
        self.assertTrue(triage_decisions[0]["outputs"]["interrupt_open"])
        self.assertIn("repair_code", triage_decisions[0]["inputs"]["recommended_routes"])
        self.assertEqual(dataset_state.agent_node_inputs[-1]["agent"], "diagnosis_repair_agent")
        self.assertEqual(dataset_state.agent_node_inputs[-1]["node"], "terminal_failure_review")
        self.assertEqual(dataset_state.agent_node_inputs[-1]["task"], "Record human triage for a terminal dataset failure.")
        self.assertEqual(dataset_state.agent_node_inputs[-1]["inputs"]["human_action"], "repair_code")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["agent"], "diagnosis_repair_agent")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["decision"], "terminal_failure_triage_recorded")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["outputs"]["next_action"], "repair_generated_code")
        self.assertTrue(dataset_state.agent_node_outputs[-1]["outputs"]["interrupt_open"])
        self.assertIn("terminal_failure_triage_limited_scope", dataset_state.risk_flags)
        self.assertIn("diagnosis_repair_agent", [item["agent"] for item in result.graph_state.agent_decisions])
        self.assertIn("diagnosis_repair_agent", [item["agent"] for item in result.graph_state.agent_node_inputs])
        self.assertIn("diagnosis_repair_agent", [item["agent"] for item in result.graph_state.agent_node_outputs])
        self.assertEqual(
            result.graph_state.agent_audit_summary["datasets"]["ADAE"]["agent_counts"]["diagnosis_repair_agent"],
            1,
        )
        self.assertEqual(workflow_state["datasets"]["ADAE"]["current_interrupt"], "terminal_failure")
        self.assertEqual(
            workflow_state["datasets"]["ADAE"]["agent_audit_summary"]["agent_counts"]["diagnosis_repair_agent"],
            1,
        )
        progress = gateway.progress_summary(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_review",
        )
        adae_progress = {item["dataset"]: item for item in progress["datasets"]}["ADAE"]
        self.assertEqual(adae_progress["next_action"], "repair_generated_code")
        self.assertEqual(adae_progress["available_actions"], [])
        self.assertNotIn(
            ("dataset", "ADAE", "terminal_failure"),
            {(item["scope"], item["dataset"], item["name"]) for item in progress["review_queue"]},
        )
        persisted_state = json.loads(
            (study_dir / "runs" / "run_lg2_terminal_failure_review" / "graph_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(
            persisted_state["datasets"]["ADAE"]["agent_node_outputs"][-1]["decision"],
            "terminal_failure_triage_recorded",
        )
        self.assertEqual(persisted_state["agent_node_outputs"][-1]["agent"], "diagnosis_repair_agent")

    def test_gateway_review_terminal_failure_entrypoint_persists_triage(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_review_entrypoint") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_review_entrypoint",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_review_entrypoint",
        ).model_copy(deep=True)
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_entrypoint")

        result = gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_review_entrypoint",
            dataset="ADAE",
            decision="retry_execution",
            reviewer="tester",
            notes="Retry after reviewing diagnostics.",
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(result.decision, "retry_execution")
        self.assertIsNone(result.current_interrupt)
        self.assertEqual(result.next_action, "retry_approved_execution")
        self.assertEqual(dataset_state.status, "pending")
        self.assertIsNone(dataset_state.current_interrupt)
        self.assertEqual(dataset_state.human_commands[-1].interrupt, "terminal_failure")
        self.assertEqual(dataset_state.human_commands[-1].action, "retry_execution")
        triage_decisions = [
            item for item in dataset_state.agent_decisions if item["agent"] == "diagnosis_repair_agent"
        ]
        self.assertEqual(triage_decisions[0]["outputs"]["next_action"], "retry_approved_execution")
        self.assertFalse(triage_decisions[0]["outputs"]["interrupt_open"])
        self.assertEqual(dataset_state.agent_node_outputs[-1]["agent"], "diagnosis_repair_agent")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["outputs"]["next_action"], "retry_approved_execution")
        self.assertFalse(dataset_state.agent_node_outputs[-1]["outputs"]["interrupt_open"])

    def test_gateway_review_terminal_failure_from_command_bridges_to_triage_flow(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_command_bridge") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_command_bridge",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_command_bridge",
        ).model_copy(deep=True)
        state.current_interrupt = None
        state.dependency_review_status = "accepted"
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_command_bridge")

        result = gateway.review_terminal_failure_from_command(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_command_bridge",
            command=HumanCommand(
                interrupt="terminal_failure",
                action="repair_code",
                dataset="ADAE",
                reviewer="native_tester",
                notes="Repair generated code through native command bridge.",
            ),
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(result.decision, "repair_code")
        self.assertEqual(result.current_interrupt, "terminal_failure")
        self.assertEqual(result.next_action, "repair_generated_code")
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "terminal_failure")
        self.assertEqual(dataset_state.human_commands[-1].interrupt, "terminal_failure")
        self.assertEqual(dataset_state.human_commands[-1].reviewer, "native_tester")
        self.assertEqual(dataset_state.execution_state["terminal_failure_review"]["action"], "repair_code")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["decision"], "terminal_failure_triage_recorded")
        self.assertEqual(dataset_state.agent_node_outputs[-1]["outputs"]["next_action"], "repair_generated_code")

    def test_gateway_review_terminal_failure_from_command_rejects_mismatched_interrupt_without_triage(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_command_mismatch") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_command_mismatch",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_command_mismatch",
        ).model_copy(deep=True)
        state.current_interrupt = None
        state.dependency_review_status = "accepted"
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="code_review",
            dataset="ADAE",
            reason="Canonical state is not waiting for terminal-failure review.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_command_mismatch")

        with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
            gateway.review_terminal_failure_from_command(
                study_dir=study_dir,
                run_id="run_lg2_terminal_failure_command_mismatch",
                command=HumanCommand(
                    interrupt="terminal_failure",
                    action="retry_execution",
                    dataset="ADAE",
                    reviewer="native_tester",
                ),
            )

        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_terminal_failure_command_mismatch")
        self.assertNotIn("terminal_failure_review", reloaded.datasets["ADAE"].execution_state)
        self.assertEqual(reloaded.datasets["ADAE"].current_interrupt.name, "code_review")

    def test_gateway_review_terminal_failure_from_command_rejects_invalid_action_without_triage(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_command_invalid_action") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_command_invalid_action",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_command_invalid_action",
        ).model_copy(deep=True)
        state.current_interrupt = None
        state.dependency_review_status = "accepted"
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_command_invalid_action")

        with self.assertRaisesRegex(ValueError, "Terminal failure review command action is not supported"):
            gateway.review_terminal_failure_from_command(
                study_dir=study_dir,
                run_id="run_lg2_terminal_failure_command_invalid_action",
                command=HumanCommand.model_construct(
                    interrupt="terminal_failure",
                    action="approve",
                    dataset="ADAE",
                    reviewer="native_tester",
                ),
            )

        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_terminal_failure_command_invalid_action")
        self.assertNotIn("terminal_failure_review", reloaded.datasets["ADAE"].execution_state)
        self.assertEqual(reloaded.datasets["ADAE"].current_interrupt.name, "terminal_failure")

    def test_gateway_review_terminal_failure_from_command_rejects_dataset_command_behind_study_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_command_study_gate") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_command_study_gate",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_command_study_gate",
        ).model_copy(deep=True)
        state.current_interrupt = InterruptState(
            name="dependency_review",
            reason="Study dependency review must be resolved first.",
        )
        state.dependency_review_status = "review_required"
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_command_study_gate")

        with self.assertRaisesRegex(ValueError, "Study-level interrupt dependency_review must be resolved"):
            gateway.review_terminal_failure_from_command(
                study_dir=study_dir,
                run_id="run_lg2_terminal_failure_command_study_gate",
                command=HumanCommand(
                    interrupt="terminal_failure",
                    action="retry_execution",
                    dataset="ADAE",
                    reviewer="native_tester",
                ),
            )

        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_terminal_failure_command_study_gate")
        self.assertEqual(reloaded.current_interrupt.name, "dependency_review")
        self.assertNotIn("terminal_failure_review", reloaded.datasets["ADAE"].execution_state)

    def test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_progress_actions") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_progress_actions",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_progress_actions",
        ).model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_progress_actions")

        progress = gateway.progress_summary(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_progress_actions",
        )

        adae_progress = {item["dataset"]: item for item in progress["datasets"]}["ADAE"]
        self.assertEqual(adae_progress["next_action"], "review_terminal_failure")
        self.assertIn(
            ("dataset", "ADAE", "terminal_failure", "interrupt"),
            {(item["scope"], item["dataset"], item["name"], item["source"]) for item in progress["review_queue"]},
        )
        self.assertEqual(
            [item["action"] for item in adae_progress["available_actions"]],
            [
                "retry_execution",
                "repair_code",
                "revise_spec",
                "request_new_input",
                "skip_dataset",
                "continue_other_datasets",
            ],
        )

        gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_progress_actions",
            dataset="ADAE",
            decision="continue_other_datasets",
            reviewer="tester",
        )
        reviewed_progress = gateway.progress_summary(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_progress_actions",
        )

        reviewed_adae = {item["dataset"]: item for item in reviewed_progress["datasets"]}["ADAE"]
        self.assertEqual(reviewed_adae["next_action"], "continue_other_datasets")
        self.assertEqual(reviewed_adae["available_actions"], [])
        self.assertNotIn(
            ("dataset", "ADAE", "terminal_failure"),
            {(item["scope"], item["dataset"], item["name"]) for item in reviewed_progress["review_queue"]},
        )

    def test_gateway_review_terminal_failure_entrypoint_rejects_invalid_decision(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_review_invalid_decision") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_review_invalid_decision",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_review_invalid_decision",
        ).model_copy(deep=True)
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_invalid_decision")

        with self.assertRaisesRegex(ValueError, "Terminal failure decision must be"):
            gateway.review_terminal_failure(
                study_dir=study_dir,
                run_id="run_lg2_terminal_failure_review_invalid_decision",
                dataset="ADAE",
                decision="approve_anyway",
                reviewer="tester",
            )

    def test_gateway_review_terminal_failure_entrypoint_requires_open_terminal_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_review_not_waiting") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_review_not_waiting",
            target_datasets=["ADAE"],
        )

        with self.assertRaisesRegex(ValueError, "not waiting for terminal_failure review"):
            gateway.review_terminal_failure(
                study_dir=study_dir,
                run_id="run_lg2_terminal_failure_review_not_waiting",
                dataset="ADAE",
                decision="retry_execution",
                reviewer="tester",
            )

    def test_gateway_skip_terminal_failure_marks_study_failed(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_failure_skip") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_skip",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_failure_skip",
        ).model_copy(deep=True)
        state.datasets["ADAE"].status = "terminal_failure"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADAE",
            reason="R execution failed or produced an unusable output.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_skip")

        result = gateway.record_terminal_failure_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_failure_skip",
            dataset="ADAE",
            command=HumanCommand(
                interrupt="terminal_failure",
                action="skip_dataset",
                dataset="ADAE",
                reviewer="tester",
                notes="Do not use this failed output.",
            ),
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        self.assertEqual(result.graph_state.datasets["ADAE"].status, "failed")
        self.assertIsNone(result.graph_state.datasets["ADAE"].current_interrupt)
        self.assertEqual(result.graph_state.status, "failed")

    def test_gateway_rolls_up_multi_dataset_status_without_losing_other_progress(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_multi_dataset_rollup") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_multi_dataset_rollup",
            target_datasets=["ADAE", "ADCM"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_multi_dataset_rollup",
        ).model_copy(deep=True)
        state.datasets["ADAE"].status = "completed"
        state.datasets["ADAE"].execution_state = {
            "status": "completed",
            "output_path": str((study_dir / "runs" / "run_lg2_multi_dataset_rollup" / "outputs" / "adae.csv").as_posix()),
            "terminal_failure": False,
        }
        state.datasets["ADCM"].status = "needs_review"
        state.datasets["ADCM"].current_interrupt = InterruptState(
            name="code_review",
            dataset="ADCM",
            reason="Review ADCM generated code.",
        )
        state.current_interrupt = None
        state.status = "needs_review"
        gateway._persist_graph_state(study_dir, state, node="test_seed_multi_dataset_rollup")
        report_path = study_dir / "runs" / "run_lg2_multi_dataset_rollup" / "compare" / "adae_compare.json"
        report_path.parent.mkdir(parents=True)
        report_path.write_text(json.dumps({"dataset": "ADAE", "status": "missing_reference"}), encoding="utf-8")

        result = gateway.record_compare(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_multi_dataset_rollup",
            dataset="ADAE",
            compare_summary={"dataset": "ADAE", "status": "missing_reference"},
            compare_report_path=report_path,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        self.assertEqual(result.graph_state.datasets["ADAE"].status, "completed")
        self.assertEqual(result.graph_state.datasets["ADCM"].current_interrupt.name, "code_review")
        self.assertEqual(result.graph_state.current_interrupt.dataset, "ADCM")
        self.assertEqual(result.graph_state.status, "needs_review")

    def test_gateway_rollup_prioritizes_terminal_failure_over_code_review(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_terminal_rollup_priority") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_rollup_priority",
            target_datasets=["ADAE", "ADCM"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_terminal_rollup_priority",
        ).model_copy(deep=True)
        state.current_interrupt = None
        state.datasets["ADAE"].status = "needs_review"
        state.datasets["ADAE"].current_interrupt = InterruptState(
            name="code_review",
            dataset="ADAE",
            reason="Review ADAE code.",
        )
        state.datasets["ADCM"].status = "terminal_failure"
        state.datasets["ADCM"].current_interrupt = InterruptState(
            name="terminal_failure",
            dataset="ADCM",
            reason="ADCM execution failed.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_priority")
        report_path = study_dir / "runs" / "run_lg2_terminal_rollup_priority" / "compare" / "adae_compare.json"
        report_path.parent.mkdir(parents=True)
        report_path.write_text(json.dumps({"dataset": "ADAE", "status": "missing_generated"}), encoding="utf-8")

        result = gateway.record_compare(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_terminal_rollup_priority",
            dataset="ADAE",
            compare_summary={"dataset": "ADAE", "status": "missing_generated"},
            compare_report_path=report_path,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        self.assertEqual(result.graph_state.status, "terminal_failure")
        self.assertEqual(result.graph_state.current_interrupt.name, "terminal_failure")
        self.assertEqual(result.graph_state.current_interrupt.dataset, "ADCM")

    def test_prepare_keeps_new_dependency_review_ahead_of_old_dataset_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_dependency_review_priority") / "PSY201"
        input_spec = study_dir / "input_spec"
        input_spec.mkdir(parents=True)
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        code_path = study_dir / "runs" / "run_lg2_dependency_review_priority" / "code" / "build_adae.R"
        code_path.parent.mkdir(parents=True)
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_dependency_review_priority", "ADAE", code_path)
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_dependency_review_priority",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
        )
        result = gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_dependency_review_priority",
            target_datasets=["ADCM"],
        )

        self.assertEqual(result.graph_state.current_interrupt.name, "dependency_review")
        self.assertIsNone(result.graph_state.current_interrupt.dataset)
        self.assertIn("ADAE", result.graph_state.datasets)
        self.assertEqual(result.graph_state.datasets["ADAE"].current_interrupt.name, "code_review")

    def test_gateway_progress_summary_reports_graph_owned_next_actions(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress") / "PSY201"
        input_spec = study_dir / "input_spec"
        input_spec.mkdir(parents=True)
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress",
            target_datasets=["ADAE", "ADCM"],
        )
        code_path = study_dir / "runs" / "run_lg2_progress" / "code" / "build_adae.R"
        code_path.parent.mkdir(parents=True)
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names=FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_lg2_progress", "ADAE", code_path)
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
        )

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_progress")

        self.assertEqual(progress["status"], "needs_review")
        self.assertEqual(progress["current_interrupt"]["name"], "dependency_review")
        self.assertEqual(progress["next_action"], "review_dependency_plan")
        by_dataset = {item["dataset"]: item for item in progress["datasets"]}
        self.assertEqual(by_dataset["ADAE"]["next_action"], "review_code")
        self.assertEqual(by_dataset["ADAE"]["action_label"], "Review generated R code.")
        self.assertTrue(by_dataset["ADAE"]["blocked"])
        self.assertIn("Study-level dependency review", by_dataset["ADAE"]["blocked_reason"])
        self.assertEqual(by_dataset["ADCM"]["next_action"], "blocked")
        self.assertIn("Study-level dependency review", by_dataset["ADCM"]["blocked_reason"])
        review_items = {(item["scope"], item["dataset"], item["name"], item["source"]) for item in progress["review_queue"]}
        self.assertIn(("study", "", "dependency_review", "interrupt"), review_items)
        self.assertIn(("dataset", "ADAE", "code_review", "interrupt"), review_items)
        self.assertIn("Review dependency plan.", {item["action_label"] for item in progress["review_queue"]})
        self.assertTrue(Path(progress["graph_state_path"]).exists())
        self.assertTrue(Path(progress["workflow_state_path"]).exists())

    def test_gateway_progress_summary_reports_missing_workflow_projection_as_none(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress_no_workflow_projection") / "PSY201"
        input_spec = study_dir / "input_spec"
        input_spec.mkdir(parents=True)
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_no_workflow_projection",
            target_datasets=["ADAE"],
        )
        workflow_path = study_dir / "runs" / "run_lg2_progress_no_workflow_projection" / "workflow_state.json"
        self.assertTrue(workflow_path.exists())
        workflow_path.unlink()

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_progress_no_workflow_projection")

        self.assertTrue(Path(progress["graph_state_path"]).exists())
        self.assertIsNone(progress["workflow_state_path"])

    def test_gateway_progress_summary_marks_not_real_outputs_as_review_only(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress_not_real_output") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_not_real_output",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_progress_not_real_output",
        ).model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "completed"
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].code_state = {
            "status": "approved",
            "generation_quality": {
                "llm_provider": "mock",
                "llm_model": "mock-model",
                "not_real_derivation": True,
            },
        }
        state.datasets["ADSL"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "generation_quality": {
                "llm_provider": "mock",
                "llm_model": "mock-model",
                "not_real_derivation": True,
            },
            "not_real_derivation": True,
        }
        state.datasets["ADSL"].validation_summary = {"status": "pass"}
        gateway._persist_graph_state(study_dir, state, node="test_seed_not_real_progress")

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_progress_not_real_output")

        by_dataset = {item["dataset"]: item for item in progress["datasets"]}
        quality = by_dataset["ADSL"]["output_quality"]
        self.assertEqual(quality["quality_status"], "not_real_derivation")
        self.assertFalse(quality["runtime_dependency_eligible"])
        self.assertTrue(quality["not_real_derivation"])
        self.assertIn("not_real_derivation", " ".join(by_dataset["ADSL"]["warnings"]))
        self.assertEqual(progress["output_quality_rollup"]["completion_quality"], "review_only_complete")
        self.assertEqual(progress["output_quality_rollup"]["review_only_outputs"], 1)
        self.assertEqual(progress["output_quality_rollup"]["real_runtime_outputs"], 0)
        self.assertEqual(progress["next_action"], "review_outputs")
        self.assertIn("review-only/demo outputs", progress["action_label"])
        self.assertEqual(by_dataset["ADSL"]["next_action"], "complete")
        self.assertIn("Review-only/demo output", by_dataset["ADSL"]["action_label"])

    def test_gateway_progress_summary_marks_mixed_completion_quality(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress_mixed_output_quality") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_mixed_output_quality",
            target_datasets=["ADSL", "ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_progress_mixed_output_quality",
        ).model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "completed"
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
        }
        state.datasets["ADSL"].validation_summary = {"status": "pass"}
        state.datasets["ADAE"].status = "completed_stub"
        state.datasets["ADAE"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "stubbed_r_execution": True,
        }
        state.datasets["ADAE"].validation_summary = {"status": "structural_stub_pass"}
        gateway._persist_graph_state(study_dir, state, node="test_seed_mixed_quality_progress")

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_progress_mixed_output_quality")

        self.assertEqual(progress["output_quality_rollup"]["completion_quality"], "mixed_output_quality_complete")
        self.assertEqual(progress["output_quality_rollup"]["real_runtime_outputs"], 1)
        self.assertEqual(progress["output_quality_rollup"]["review_only_outputs"], 1)
        self.assertEqual(progress["next_action"], "review_outputs")
        self.assertIn("some outputs are review-only/demo", progress["action_label"])

    def test_gateway_progress_summary_prioritizes_stale_plan_replan(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress_stale") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_stale",
            target_datasets=["ADAE"],
        )
        gateway.record_input_spec_ready(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_stale",
            dataset="ADAE",
            input_spec_path=spec_dir / "adae.json",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")
        gateway.mark_inputs_changed(study_dir=study_dir, run_id="run_lg2_progress_stale")

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_progress_stale")

        self.assertEqual(progress["current_interrupt"]["name"], "dependency_review")
        self.assertEqual(progress["dependency_review_status"], "stale")
        self.assertEqual(progress["next_action"], "replan_dependencies")
        self.assertTrue(progress["plan_stale"])
        by_dataset = {item["dataset"]: item for item in progress["datasets"]}
        self.assertEqual(by_dataset["ADAE"]["next_action"], "review_draft_spec")
        self.assertTrue(by_dataset["ADAE"]["blocked"])
        self.assertIn("stale", by_dataset["ADAE"]["blocked_reason"])

    def test_gateway_progress_summary_blocks_review_required_dependency_sources(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress_review_required") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_review_required",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_progress_review_required",
        ).model_copy(deep=True)
        state.dependency_review_status = "review_required"
        state.dependency_decisions = [
            {
                "dataset": "ADAE",
                "source": "legacy_code",
                "decision": "requires_human_review",
                "review_required": True,
            }
        ]
        state.current_interrupt = None
        state.datasets["ADAE"].current_interrupt = None
        state.datasets["ADAE"].spec_state = {"status": "input_spec_ready"}
        state.datasets["ADAE"].status = "pending"
        gateway._persist_graph_state(study_dir, state, node="test_seed_review_required_progress")

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_progress_review_required")

        self.assertEqual(progress["next_action"], "review_dependency_plan")
        self.assertIn(
            ("study", "", "dependency_review", "progress"),
            {(item["scope"], item["dataset"], item["name"], item["source"]) for item in progress["review_queue"]},
        )
        by_dataset = {item["dataset"]: item for item in progress["datasets"]}
        self.assertTrue(by_dataset["ADAE"]["blocked"])
        self.assertEqual(by_dataset["ADAE"]["next_action"], "blocked")
        self.assertIn("legacy_code", by_dataset["ADAE"]["blocked_reason"])


if __name__ == "__main__":
    unittest.main()
