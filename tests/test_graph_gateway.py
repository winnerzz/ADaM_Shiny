"""Tests for the LangGraph-2 gateway and workflow projection."""

from __future__ import annotations

import json
import inspect
import os
import sqlite3
import sys
import unittest
import uuid
from datetime import UTC, datetime
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.api.models import RunPlanRequest
    from adam_agent.api.service import prepare_run_plan
    from adam_agent.graph.checkpointing import build_checkpointer, default_sqlite_checkpointer_path, describe_checkpointer
    from adam_agent.graph.gateway import (
        GraphGateway,
        GraphGatewayNativeDatasetFullRunResult,
        _generation_quality_from_dataset_result,
        _native_study_loop_dependency_outputs_available,
        _resolve_run_artifact_path,
    )
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
    from adam_agent.graph.gateway import (
        GraphGateway,
        GraphGatewayNativeDatasetFullRunResult,
        _generation_quality_from_dataset_result,
        _native_study_loop_dependency_outputs_available,
        _resolve_run_artifact_path,
    )
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


def _seed_adae_terminal_failure_after_review(
    *,
    test_name: str,
    run_id: str,
    triage_action: str,
) -> tuple[GraphGateway, Path]:
    study_dir = _workspace_dir(test_name) / "PSY201"
    sdtm_dir = study_dir / "input_sdtm"
    spec_dir = study_dir / "input_spec"
    code_dir = study_dir / "runs" / run_id / "code"
    sdtm_dir.mkdir(parents=True)
    spec_dir.mkdir()
    code_dir.mkdir(parents=True)
    (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (spec_dir / "adae.json").write_text(
        json.dumps({"dataset": "ADAE", "variables": [{"variable": "USUBJID", "source_domains": ["AE"]}]}),
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
    gateway.record_code_generation(
        study_dir=study_dir,
        study_id="PSY201",
        run_id=run_id,
        dataset="ADAE",
        code_path=code_path,
        code_sha256=f"sha256:{sha256_file(code_path)}",
        static_check_path=static_path,
        static_check_sha256=static_sha,
        spec_source="input_spec",
        spec_path=spec_dir / "adae.json",
        spec_sha256=f"sha256:{sha256_file(spec_dir / 'adae.json')}",
        input_fingerprint_payload=input_fingerprint(study_dir),
    )
    gateway.review_code(
        study_dir=study_dir,
        study_id="PSY201",
        run_id=run_id,
        dataset="ADAE",
        decision="approve",
        reviewer="tester",
        input_fingerprint_payload=input_fingerprint(study_dir),
    )
    gateway.record_execution(
        study_dir=study_dir,
        study_id="PSY201",
        run_id=run_id,
        dataset="ADAE",
        execution_state={
            "status": "terminal_failure",
            "validation_status": "failed",
            "terminal_failure": True,
            "partial_output_usable": False,
            "diagnostics_path": str((study_dir / "runs" / run_id / "diagnostics" / "adae_failure.json").as_posix()),
        },
        validation_summary={"status": "failed"},
        artifacts=[],
        failures=[
            FailureRecord(
                failure_id=f"failure_{run_id}_adae",
                dataset="ADAE",
                node="execute_approved_code",
                failure_type="sandbox_error",
                message="R execution failed.",
                root_cause="r_runtime_error",
                recommended_route=triage_action,
            )
        ],
        input_fingerprint_payload=input_fingerprint(study_dir),
    )
    gateway.review_terminal_failure(
        study_dir=study_dir,
        run_id=run_id,
        dataset="ADAE",
        decision=triage_action,
        reviewer="tester",
        input_fingerprint_payload=input_fingerprint(study_dir),
    )
    return gateway, study_dir


class GraphGatewayTests(unittest.TestCase):
    def setUp(self) -> None:
        self._old_backend = os.environ.get("ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND")
        os.environ["ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND"] = "memory"

    def tearDown(self) -> None:
        if self._old_backend is None:
            os.environ.pop("ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND", None)
        else:
            os.environ["ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND"] = self._old_backend

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

    def test_gateway_dependency_planning_artifacts_are_upserted(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_plan_artifact_upsert") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_gateway_plan_artifact_upsert",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_gateway_plan_artifact_upsert",
        ).model_copy(deep=True)

        gateway._persist_graph_state(study_dir, state, node="test_repeated_planning_artifact_persist")
        reloaded = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_gateway_plan_artifact_upsert",
        )
        artifact_ids = [artifact.artifact_id for artifact in reloaded.artifacts]

        self.assertEqual(
            artifact_ids.count("dependency_plan_psy201_run_lg2_gateway_plan_artifact_upsert"),
            1,
        )
        self.assertEqual(
            artifact_ids.count("dependency_review_psy201_run_lg2_gateway_plan_artifact_upsert"),
            1,
        )
        self.assertEqual(
            artifact_ids.count("agent_summary_psy201_run_lg2_gateway_plan_artifact_upsert"),
            1,
        )

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
        native_resume = progress["native_resume"]
        self.assertFalse(native_resume["available"])
        self.assertEqual(native_resume["scope"], "none")
        self.assertEqual(native_resume["boundary"], "graph_state_projection_only")
        self.assertEqual(
            native_resume["explicit_resume_endpoint"],
            "POST /runs/{run_id}/datasets/{dataset}/native-resume",
        )
        self.assertEqual(native_resume["default_review_path"], "split_flow_review_endpoints")
        self.assertEqual(native_resume["restart_recovery_source"], "graph_state_json")
        self.assertEqual(native_resume["interrupt_queue"], [])
        self.assertFalse(native_resume["has_queue_items"])
        self.assertEqual(native_resume["queue_item_count"], 0)
        self.assertFalse(native_resume["runtime_can_resume"])
        self.assertTrue(native_resume["checkpoint_paths_match"])
        self.assertIn("Durable native LangGraph interrupt resume is not enabled", native_resume["message"])
        self.assertNotIn("endpoint", progress["native_resume"])

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

    def test_sqlite_progress_marks_native_resume_when_package_available(self) -> None:
        study_dir = _workspace_dir("lg2_checkpointing_sqlite_progress") / "PSY201"
        study_dir.mkdir(parents=True)
        sqlite_path = default_sqlite_checkpointer_path(study_dir, "run_sqlite_progress")

        try:
            gateway = GraphGateway(checkpointer_backend="sqlite", sqlite_checkpointer_path=sqlite_path)
        except ValueError as exc:
            if "not installed" in str(exc) or "cannot be imported" in str(exc):
                self.skipTest(str(exc))
            raise

        try:
            gateway.start_dependency_plan(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_sqlite_progress",
                target_datasets=["ADAE"],
            )
            progress = gateway.progress_summary(study_dir=study_dir, run_id="run_sqlite_progress")
        finally:
            gateway.close()

        self.assertTrue(progress["native_resume"]["available"])
        self.assertEqual(progress["native_resume"]["scope"], "native_pilot_interrupts_only")
        self.assertEqual(progress["native_resume"]["boundary"], "durable_native_interrupt_resume")
        self.assertEqual(progress["native_resume"]["runtime_binding_status"], "bound")
        self.assertEqual(progress["native_resume"]["resume_unavailable_reason"], "")
        self.assertEqual(
            progress["native_resume"]["explicit_resume_endpoint"],
            "POST /runs/{run_id}/datasets/{dataset}/native-resume",
        )
        self.assertNotIn("endpoint", progress["native_resume"])
        self.assertEqual(progress["native_resume"]["restart_recovery_source"], "langgraph_sqlite_checkpointer")
        self.assertIn("available for pilot graph interrupts", progress["native_resume"]["message"])
        self.assertFalse(progress["native_resume"]["has_queue_items"])
        self.assertEqual(progress["native_resume"]["queue_item_count"], 0)

    def test_progress_reports_native_resume_queue_without_enabling_memory_resume(self) -> None:
        study_dir = _workspace_dir("lg2_native_resume_queue_memory") / "PSY201"
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

        gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_resume_queue_memory",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_resume_queue_memory")

        queue = progress["native_resume"]["interrupt_queue"]
        self.assertEqual(len(queue), 1)
        self.assertEqual(queue[0]["dataset"], "ADAE")
        self.assertEqual(queue[0]["interrupt"], "code_review")
        self.assertFalse(queue[0]["can_resume"])
        self.assertIsNone(queue[0]["resume_endpoint"])
        self.assertEqual(queue[0]["default_review_path"], "split_flow_review_endpoints")
        self.assertEqual([item["action"] for item in queue[0]["available_actions"]], ["approve", "reject"])
        self.assertFalse(progress["native_resume"]["available"])
        self.assertEqual(progress["native_resume"]["runtime_binding_status"], "run_not_durable")
        self.assertEqual(progress["native_resume"]["resume_unavailable_reason"], "run_not_durable")
        self.assertTrue(progress["native_resume"]["has_queue_items"])
        self.assertEqual(progress["native_resume"]["queue_item_count"], 1)
        self.assertEqual(progress["study_loop_result"], {})

    def test_progress_native_resume_queue_respects_study_level_gate(self) -> None:
        study_dir = _workspace_dir("lg2_native_resume_queue_study_gate") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_native_resume_queue_study_gate",
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            current_interrupt=InterruptState(name="dependency_review", reason="Review dependency plan first."),
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_native_resume_queue_study_gate",
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(study_dir, state, node="test_seed_native_resume_study_gate")

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_resume_queue_study_gate")

        self.assertEqual(progress["next_action"], "review_dependency_plan")
        self.assertEqual(progress["review_queue"][0]["name"], "dependency_review")
        self.assertEqual(progress["native_resume"]["interrupt_queue"], [])

    def test_progress_native_resume_queue_respects_blocked_dataset_gate(self) -> None:
        study_dir = _workspace_dir("lg2_native_resume_queue_blocked_dataset") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_native_resume_queue_blocked_dataset",
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            dependency_review_status="stale",
            dependency_plan={"plan_stale": True},
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_native_resume_queue_blocked_dataset",
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(study_dir, state, node="test_seed_native_resume_blocked_dataset")

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_resume_queue_blocked_dataset")

        self.assertEqual(progress["next_action"], "replan_dependencies")
        self.assertEqual(progress["datasets"][0]["next_action"], "review_code")
        self.assertTrue(progress["datasets"][0]["blocked"])
        self.assertEqual(progress["datasets"][0]["available_actions"], [])
        self.assertEqual(progress["native_resume"]["interrupt_queue"], [])

    def test_progress_durable_native_resume_queue_still_respects_study_gate(self) -> None:
        study_dir = _workspace_dir("lg2_native_resume_queue_durable_study_gate") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_native_resume_queue_durable_study_gate",
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            current_interrupt=InterruptState(name="dependency_review", reason="Review dependency plan first."),
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_native_resume_queue_durable_study_gate",
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(
            study_dir,
            state,
            node="test_seed_durable_native_resume_study_gate",
            runtime_persistence_extra={
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(
                    (
                        study_dir
                        / "runs"
                        / "run_lg2_native_resume_queue_durable_study_gate"
                        / "langgraph_checkpoints.sqlite"
                    ).as_posix()
                ),
            },
        )

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_resume_queue_durable_study_gate")

        self.assertFalse(progress["native_resume"]["available"])
        self.assertEqual(progress["native_resume"]["boundary"], "graph_state_projection_only")
        self.assertFalse(progress["native_resume"]["runtime_can_resume"])
        self.assertFalse(progress["native_resume"]["checkpoint_paths_match"])
        self.assertEqual(progress["native_resume"]["runtime_binding_status"], "service_not_durable")
        self.assertEqual(progress["native_resume"]["resume_unavailable_reason"], "service_not_durable")
        self.assertEqual(progress["native_resume"]["interrupt_queue"], [])
        self.assertFalse(progress["native_resume"]["has_queue_items"])
        self.assertEqual(progress["native_resume"]["queue_item_count"], 0)
        self.assertIn("current service is not opened with a durable checkpointer", progress["native_resume"]["message"])

    def test_progress_durable_native_resume_queue_still_respects_stale_plan(self) -> None:
        study_dir = _workspace_dir("lg2_native_resume_queue_durable_stale_plan") / "PSY201"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_native_resume_queue_durable_stale_plan",
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            dependency_review_status="stale",
            dependency_plan={"plan_stale": True},
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_native_resume_queue_durable_stale_plan",
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(
            study_dir,
            state,
            node="test_seed_durable_native_resume_stale_plan",
            runtime_persistence_extra={
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(
                    (
                        study_dir
                        / "runs"
                        / "run_lg2_native_resume_queue_durable_stale_plan"
                        / "langgraph_checkpoints.sqlite"
                    ).as_posix()
                ),
            },
        )

        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_resume_queue_durable_stale_plan")

        self.assertFalse(progress["native_resume"]["available"])
        self.assertEqual(progress["native_resume"]["boundary"], "graph_state_projection_only")
        self.assertFalse(progress["native_resume"]["runtime_can_resume"])
        self.assertFalse(progress["native_resume"]["checkpoint_paths_match"])
        self.assertEqual(progress["native_resume"]["runtime_binding_status"], "service_not_durable")
        self.assertEqual(progress["native_resume"]["resume_unavailable_reason"], "service_not_durable")
        self.assertEqual(progress["native_resume"]["interrupt_queue"], [])
        self.assertFalse(progress["native_resume"]["has_queue_items"])
        self.assertEqual(progress["native_resume"]["queue_item_count"], 0)
        self.assertEqual(progress["datasets"][0]["available_actions"], [])
        self.assertIn("current service is not opened with a durable checkpointer", progress["native_resume"]["message"])

    def test_progress_native_resume_requires_current_durable_gateway_binding(self) -> None:
        study_dir = _workspace_dir("lg2_native_resume_queue_stale_durable_state") / "PSY201"
        study_dir.mkdir(parents=True)
        run_id = "run_lg2_native_resume_queue_stale_durable_state"
        state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(
            study_dir,
            state,
            node="test_seed_stale_native_resume_runtime",
            runtime_persistence_extra={
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(
                    (study_dir / "runs" / run_id / "langgraph_checkpoints.sqlite").as_posix()
                ),
            },
        )

        progress = gateway.progress_summary(study_dir=study_dir, run_id=run_id)

        queue = progress["native_resume"]["interrupt_queue"]
        self.assertFalse(progress["native_resume"]["available"])
        self.assertEqual(progress["native_resume"]["boundary"], "graph_state_projection_only")
        self.assertFalse(progress["native_resume"]["runtime_can_resume"])
        self.assertFalse(progress["native_resume"]["checkpoint_paths_match"])
        self.assertEqual(progress["native_resume"]["runtime_binding_status"], "service_not_durable")
        self.assertEqual(progress["native_resume"]["resume_unavailable_reason"], "service_not_durable")
        self.assertTrue(progress["native_resume"]["has_queue_items"])
        self.assertEqual(progress["native_resume"]["queue_item_count"], 1)
        self.assertEqual(queue[0]["dataset"], "ADAE")
        self.assertFalse(queue[0]["can_resume"])
        self.assertIsNone(queue[0]["resume_endpoint"])
        self.assertEqual(queue[0]["default_review_path"], "split_flow_review_endpoints")
        self.assertIn("current service is not opened with a durable checkpointer", progress["native_resume"]["message"])

    def test_progress_native_resume_reports_checkpoint_path_mismatch_reason(self) -> None:
        study_dir = _workspace_dir("lg3_native_resume_checkpoint_mismatch_reason") / "PSY201"
        study_dir.mkdir(parents=True)
        run_id = "run_lg3_native_resume_checkpoint_mismatch_reason"
        recorded_path = study_dir / "runs" / run_id / "langgraph_checkpoints.sqlite"
        active_path = study_dir / "runs" / run_id / "other_langgraph_checkpoints.sqlite"
        state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(
            study_dir,
            state,
            node="test_seed_native_resume_checkpoint_mismatch_reason",
            runtime_persistence_extra={
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(recorded_path.as_posix()),
            },
        )

        with (
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch.object(gateway, "_native_interrupt_checkpoint_path", return_value=str(active_path.as_posix())),
        ):
            progress = gateway.progress_summary(study_dir=study_dir, run_id=run_id)

        queue = progress["native_resume"]["interrupt_queue"]
        self.assertFalse(progress["native_resume"]["available"])
        self.assertTrue(progress["native_resume"]["runtime_can_resume"])
        self.assertFalse(progress["native_resume"]["checkpoint_paths_match"])
        self.assertEqual(progress["native_resume"]["runtime_binding_status"], "checkpoint_path_mismatch")
        self.assertEqual(progress["native_resume"]["resume_unavailable_reason"], "checkpoint_path_mismatch")
        self.assertEqual(queue[0]["dataset"], "ADAE")
        self.assertFalse(queue[0]["can_resume"])
        self.assertIn("checkpoint path does not match", progress["native_resume"]["message"])

    def test_sqlite_progress_marks_native_resume_queue_as_resumable_when_package_available(self) -> None:
        study_dir = _workspace_dir("lg2_native_resume_queue_sqlite") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        sqlite_path = default_sqlite_checkpointer_path(study_dir, "run_lg2_native_resume_queue_sqlite")

        try:
            gateway = GraphGateway(checkpointer_backend="sqlite", sqlite_checkpointer_path=sqlite_path)
        except ValueError as exc:
            if "not installed" in str(exc) or "cannot be imported" in str(exc):
                self.skipTest(str(exc))
            raise

        try:
            gateway.start_native_dataset_product_loop(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_native_resume_queue_sqlite",
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )
            progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_resume_queue_sqlite")
        finally:
            gateway.close()

        queue = progress["native_resume"]["interrupt_queue"]
        self.assertEqual(len(queue), 1)
        self.assertEqual(queue[0]["dataset"], "ADAE")
        self.assertEqual(queue[0]["interrupt"], "code_review")
        self.assertTrue(queue[0]["can_resume"])
        self.assertEqual(queue[0]["resume_endpoint"], "POST /runs/{run_id}/datasets/{dataset}/native-resume")
        self.assertTrue(progress["native_resume"]["available"])
        self.assertEqual(progress["native_resume"]["runtime_binding_status"], "bound")
        self.assertEqual(progress["native_resume"]["resume_unavailable_reason"], "")
        self.assertTrue(progress["native_resume"]["has_queue_items"])
        self.assertEqual(progress["native_resume"]["queue_item_count"], 1)
        self.assertEqual(progress["study_loop_result"], {})

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
        self.assertEqual(
            reviewed.graph_state.runtime_persistence["native_draft_spec_review_resume"]["resume_source"],
            "langgraph_command_resume",
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
        self.assertEqual(
            reviewed.graph_state.runtime_persistence["native_draft_spec_review_resume"]["resume_source"],
            "langgraph_command_resume",
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
        self.assertEqual(dataset_state.code_state["assumptions"], ["Assumption under review."])
        self.assertEqual(dataset_state.code_state["risk_points"], ["Review generated derivation."])
        self.assertEqual(dataset_state.code_state["used_inputs"], ["AE"])
        self.assertEqual(dataset_state.code_state["expected_outputs"], ["outputs/adae.csv"])
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

    def test_gateway_native_dataset_product_loop_input_spec_executes_after_code_review(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_dataset_product_loop") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "dm.csv").write_text("USUBJID,AGE\n01,50\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        output_artifact = ArtifactRef(
            artifact_id="output_adam_psy201_run_lg2_native_loop_adsl",
            kind="output_adam",
            path="runs/run_lg2_native_loop/outputs/adsl.csv",
            sha256=f"sha256:{'8' * 64}",
            dataset="ADSL",
            format="csv",
            role="output",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg2_native_loop_adsl",
            kind="validation_report",
            path="runs/run_lg2_native_loop/validation/adsl_validation_report.json",
            sha256=f"sha256:{'9' * 64}",
            dataset="ADSL",
            format="json",
            role="output",
        )
        fake_execution = SimpleNamespace(
            terminal_failure=False,
            response_status="completed",
            validation_status="pass",
            output_path="runs/run_lg2_native_loop/outputs/adsl.csv",
            validation_report_path="runs/run_lg2_native_loop/validation/adsl_validation_report.json",
            diagnostics_path=None,
            errors=[],
            warnings=[],
            validation_report={"status": "pass", "errors": [], "warnings": []},
            failure_records=[],
            artifacts={"output_adam": output_artifact, "validation_report": validation_artifact},
        )
        gateway = GraphGateway()

        started = gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_loop",
            dataset="ADSL",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        self.assertEqual(started.graph_state.datasets["ADSL"].current_interrupt.name, "code_review")
        self.assertIn("native_dataset_product_loop_interrupt", started.graph_state.runtime_persistence)
        self.assertEqual(
            started.graph_state.runtime_persistence["native_dataset_product_loop_interrupt"]["boundary"],
            "dataset_product_loop_pilot_only",
        )

        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_execution):
            completed = gateway.resume_native_dataset_product_loop(
                study_dir=study_dir,
                run_id="run_lg2_native_loop",
                dataset="ADSL",
                decision="approve",
                reviewer="tester",
                notes="Approve generated R and execute the native loop pilot.",
            )

        dataset_state = completed.graph_state.datasets["ADSL"]
        self.assertTrue(completed.approved)
        self.assertIsNotNone(completed.execution)
        self.assertEqual(completed.execution.status, "completed")
        self.assertEqual(dataset_state.status, "completed")
        self.assertEqual(dataset_state.execution_state["status"], "completed")
        self.assertEqual(dataset_state.human_commands[-1].interrupt, "code_review")
        self.assertIn("native_dataset_product_loop_resume", completed.graph_state.runtime_persistence)
        self.assertTrue((study_dir / "runs" / "run_lg2_native_loop" / "review" / "adsl_code_review.json").exists())

    def test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_resume_memory_block") / "PSY201"
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
        gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_resume_memory_block",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch.object(gateway, "resume_native_code_review") as resume_code:
            with self.assertRaisesRegex(ValueError, "Native LangGraph interrupt resume is not enabled"):
                gateway.resume_native_dataset_interrupt(
                    study_dir=study_dir,
                    run_id="run_lg2_native_resume_memory_block",
                    dataset="ADAE",
                    decision="approve",
                    reviewer="tester",
                    notes="Default memory checkpointer must fail closed.",
                )

        resume_code.assert_not_called()
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_lg2_native_resume_memory_block"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_gateway_native_resume_rejects_recorded_checkpoint_path_mismatch(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_resume_path_mismatch") / "PSY201"
        study_dir.mkdir(parents=True)
        run_id = "run_lg2_native_resume_path_mismatch"
        graph_state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        recorded_checkpoint_path = study_dir / "runs" / run_id / "recorded" / "langgraph_checkpoints.sqlite"
        active_checkpoint_path = study_dir / "runs" / run_id / "active" / "langgraph_checkpoints.sqlite"
        gateway = GraphGateway()
        gateway._persist_graph_state(
            study_dir,
            graph_state,
            node="test_seed_native_resume_path_mismatch",
            runtime_persistence_extra={
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(recorded_checkpoint_path.as_posix()),
            },
        )

        with (
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch.object(gateway, "_native_interrupt_checkpoint_path", return_value=str(active_checkpoint_path.as_posix())),
            patch.object(gateway, "resume_native_code_review") as resume_code,
        ):
            with self.assertRaisesRegex(ValueError, "current checkpointer configuration"):
                gateway.resume_native_dataset_interrupt(
                    study_dir=study_dir,
                    run_id=run_id,
                    dataset="ADAE",
                    decision="approve",
                    reviewer="tester",
                    notes="A different checkpointer path must not resume this run.",
                )

        resume_code.assert_not_called()
        self.assertFalse((study_dir / "runs" / run_id / "review" / "adae_code_review.json").exists())

    def test_progress_native_resume_checkpoint_path_match_normalizes_equivalent_paths(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_resume_path_equivalent") / "PSY201"
        study_dir.mkdir(parents=True)
        run_id = "run_lg2_native_resume_path_equivalent"
        checkpoint_path = study_dir / "runs" / run_id / "langgraph_checkpoints.sqlite"
        graph_state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(name="code_review", dataset="ADAE", reason="Review ADAE code."),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(
            study_dir,
            graph_state,
            node="test_seed_native_resume_path_equivalent",
            runtime_persistence_extra={
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(checkpoint_path.relative_to(Path.cwd()).as_posix()),
            },
        )

        with (
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch.object(gateway, "_native_interrupt_checkpoint_path", return_value=str(checkpoint_path.as_posix())),
        ):
            progress = gateway.progress_summary(study_dir=study_dir, run_id=run_id)

        self.assertTrue(progress["native_resume"]["available"])
        self.assertTrue(progress["native_resume"]["checkpoint_paths_match"])
        queue = progress["native_resume"]["interrupt_queue"]
        self.assertEqual(queue[0]["dataset"], "ADAE")
        self.assertTrue(queue[0]["can_resume"])
        self.assertEqual(queue[0]["resume_endpoint"], "POST /runs/{run_id}/datasets/{dataset}/native-resume")

    def test_gateway_native_dataset_product_loop_reject_does_not_execute(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_dataset_product_loop_reject") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "dm.csv").write_text("USUBJID,AGE\n01,50\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_loop_reject",
            dataset="ADSL",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch("adam_agent.graph.gateway.GraphGateway.execute_approved_code") as execute_approved:
            rejected = gateway.resume_native_dataset_product_loop(
                study_dir=study_dir,
                run_id="run_lg2_native_loop_reject",
                dataset="ADSL",
                decision="reject",
                reviewer="tester",
                notes="Generated R is not acceptable.",
            )

        execute_approved.assert_not_called()
        dataset_state = rejected.graph_state.datasets["ADSL"]
        self.assertFalse(rejected.approved)
        self.assertIsNone(rejected.execution)
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.code_state["status"], "rejected")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertNotIn("native_dataset_product_loop_resume", rejected.graph_state.runtime_persistence)

    def test_gateway_native_dataset_product_loop_missing_spec_stops_at_draft_review(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_dataset_loop_missing_spec") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()

        started = gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_loop_missing_spec",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = started.graph_state.datasets["ADAE"]
        self.assertEqual(started.spec_source, "draft_spec")
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(dataset_state.spec_state["status"], "draft_generated")
        self.assertFalse(dataset_state.code_state)
        self.assertIn("native_dataset_product_loop_interrupt", started.graph_state.runtime_persistence)
        self.assertEqual(
            started.graph_state.runtime_persistence["native_dataset_product_loop_interrupt"]["boundary"],
            "dataset_product_loop_pilot_only",
        )
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg2_native_loop_missing_spec"
                / "specs"
                / "adae_draft_spec.json"
            ).exists()
        )

    def test_gateway_native_dataset_product_loop_draft_approval_continues_to_code_review(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_dataset_loop_draft_to_code") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_loop_draft_to_code",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        continued = gateway.resume_native_dataset_product_loop_draft_spec(
            study_dir=study_dir,
            run_id="run_lg2_native_loop_draft_to_code",
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Approve generated draft spec and continue the native loop pilot.",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = continued.graph_state.datasets["ADAE"]
        self.assertTrue(continued.draft_review.approved)
        self.assertIsNotNone(continued.code_generation)
        self.assertEqual(dataset_state.spec_state["status"], "approved")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(dataset_state.code_state["spec_source"], "approved_draft_spec")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.human_commands[-1].interrupt, "draft_spec_review")
        self.assertEqual(
            continued.graph_state.runtime_persistence["native_draft_spec_review_resume"]["resume_source"],
            "langgraph_command_resume",
        )
        self.assertIn("native_dataset_product_loop_draft_resume", continued.graph_state.runtime_persistence)
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg2_native_loop_draft_to_code"
                / "code"
                / "build_adae.R"
            ).exists()
        )
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_lg2_native_loop_draft_to_code"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_gateway_native_dataset_product_loop_draft_reject_does_not_generate_code(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_dataset_loop_draft_reject") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_loop_draft_reject",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch.object(gateway, "start_native_dataset_product_loop") as start_loop:
            rejected = gateway.resume_native_dataset_product_loop_draft_spec(
                study_dir=study_dir,
                run_id="run_lg2_native_loop_draft_reject",
                dataset="ADAE",
                decision="reject",
                reviewer="tester",
                notes="Draft spec is not acceptable.",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        start_loop.assert_not_called()
        dataset_state = rejected.graph_state.datasets["ADAE"]
        self.assertFalse(rejected.draft_review.approved)
        self.assertIsNone(rejected.code_generation)
        self.assertEqual(dataset_state.spec_state["status"], "rejected")
        self.assertFalse(dataset_state.code_state)
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(
            rejected.graph_state.runtime_persistence["native_draft_spec_review_resume"]["resume_source"],
            "langgraph_command_resume",
        )
        self.assertFalse((study_dir / "runs" / "run_lg2_native_loop_draft_reject" / "code").exists())

    def test_gateway_native_dataset_product_loop_respects_repair_code_terminal_followup(self) -> None:
        gateway, study_dir = _seed_adae_terminal_failure_after_review(
            test_name="lg2_gateway_native_loop_repair_followup",
            run_id="run_lg2_native_loop_repair_followup",
            triage_action="repair_code",
        )

        result = gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_loop_repair_followup",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(dataset_state.code_state["terminal_failure_followup"]["action"], "repair_code")
        self.assertEqual(dataset_state.execution_state["terminal_failure_followup_consumed_by"], "generate_code")

    def test_gateway_native_dataset_product_loop_routes_revise_spec_followup_to_draft_review(self) -> None:
        gateway, study_dir = _seed_adae_terminal_failure_after_review(
            test_name="lg2_gateway_native_loop_revise_followup",
            run_id="run_lg2_native_loop_revise_followup",
            triage_action="revise_spec",
        )

        result = gateway.start_native_dataset_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_loop_revise_followup",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(dataset_state.spec_state["status"], "draft_generated")
        self.assertEqual(dataset_state.spec_state["terminal_failure_followup"]["action"], "revise_spec")
        self.assertEqual(dataset_state.execution_state["terminal_failure_followup_consumed_by"], "draft_spec")
        self.assertEqual(dataset_state.code_state["status"], "stale")
        self.assertEqual(dataset_state.code_state["terminal_failure_followup"]["action"], "revise_spec")

    def test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_start") / "PSY201"
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

        result = gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_start",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.phase, "waiting_for_human_gate")
        self.assertEqual(result.current_interrupt, "code_review")
        self.assertFalse(result.approved)
        self.assertIsNone(result.execution)
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["contract"], "single_dataset_spec_code_review_execute")
        self.assertTrue(contract["execution_requires_explicit_resume"])
        self.assertEqual(contract["resume_mode"], "graph_state_full_run_compatibility")
        self.assertTrue(contract["compatibility_resume_available"])
        self.assertTrue(contract["compatibility_resume_currently_available"])
        self.assertEqual(contract["compatibility_resume_boundary"], "current_graph_state_review_gate")
        self.assertEqual(contract["compatibility_resume_supported_interrupts"], ["draft_spec_review", "code_review"])
        self.assertTrue(contract["graph_state_resume_available"])
        self.assertEqual(contract["resume_endpoint"], "POST /runs/{run_id}/datasets/{dataset}/native-full-run/resume")
        self.assertEqual(contract["native_resume_endpoint"], "POST /runs/{run_id}/datasets/{dataset}/native-resume")
        self.assertFalse(contract["durable_resume_available"])
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertEqual(contract["durable_full_run_resume_boundary"], "not_implemented")
        self.assertFalse(contract["durable_native_interrupt_resume_available"])
        self.assertEqual(contract["durable_native_resume_scope"], "none")
        self.assertEqual(contract["durable_native_interrupt_resume_boundary"], "graph_state_projection_only")
        self.assertFalse(contract["repair_or_revision_continued"])
        self.assertIn("native_dataset_product_loop_interrupt", result.graph_state.runtime_persistence)
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_lg3_native_full_run_start"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_gateway_submit_graph_command_approves_current_code_review_gate(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_graph_command_code_review") / "PSY201"
        run_id = "run_lg3_gateway_graph_command_code_review"
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
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        result = gateway.submit_graph_command(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            interrupt="code_review",
            action="approve",
            reviewer="qa_user",
            notes="Approved through graph command.",
        )

        self.assertEqual(result.scope, "dataset")
        self.assertEqual(result.dataset, "ADAE")
        self.assertEqual(result.interrupt, "code_review")
        self.assertEqual(result.action, "approve")
        self.assertTrue(result.approved)
        self.assertEqual(result.next_action, "execute_approved_code")
        self.assertEqual(result.graph_state.datasets["ADAE"].code_state["status"], "approved")
        self.assertTrue(result.review_artifact_path.endswith("runs/run_lg3_gateway_graph_command_code_review/review/adae_code_review.json"))

    def test_gateway_submit_graph_command_respects_study_level_interrupt_precedence(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_graph_command_study_gate") / "PSY201"
        run_id = "run_lg3_gateway_graph_command_study_gate"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            current_interrupt=InterruptState(name="dependency_review", reason="Review dependency plan first."),
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(
                        name="code_review",
                        dataset="ADAE",
                        reason="Review ADAE code.",
                    ),
                )
            },
        )
        gateway = GraphGateway()
        gateway._persist_graph_state(study_dir, state, node="test_seed_graph_command_study_gate")

        with self.assertRaisesRegex(ValueError, "Resolve study-level interrupt dependency_review"):
            gateway.submit_graph_command(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                interrupt="code_review",
                action="approve",
                reviewer="qa_user",
            )

        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        self.assertEqual(reloaded.current_interrupt.name, "dependency_review")
        self.assertEqual(reloaded.datasets["ADAE"].current_interrupt.name, "code_review")
        self.assertFalse((study_dir / "runs" / run_id / "review" / "adae_code_review.json").exists())

    def test_gateway_submit_graph_command_rejects_dependency_review_blocking_statuses(self) -> None:
        for status, plan_stale in [("stale", True), ("rejected", False)]:
            with self.subTest(status=status):
                study_dir = _workspace_dir(f"lg3_gateway_graph_command_{status}_gate") / "PSY201"
                run_id = f"run_lg3_gateway_graph_command_{status}_gate"
                study_dir.mkdir(parents=True)
                state = StudyRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    status="needs_review",
                    target_datasets=["ADAE"],
                    runnable_datasets=["ADAE"],
                    dependency_review_status=status,
                    dependency_plan={"plan_stale": plan_stale},
                    datasets={
                        "ADAE": DatasetRunState(
                            study_id="PSY201",
                            run_id=run_id,
                            dataset="ADAE",
                            status="needs_review",
                            current_interrupt=InterruptState(
                                name="code_review",
                                dataset="ADAE",
                                reason="Review ADAE code.",
                            ),
                        )
                    },
                )
                gateway = GraphGateway()
                gateway._persist_graph_state(study_dir, state, node="test_seed_graph_command_dependency_status_gate")

                with self.assertRaisesRegex(ValueError, "Resolve dependency_review"):
                    gateway.submit_graph_command(
                        study_dir=study_dir,
                        run_id=run_id,
                        dataset="ADAE",
                        interrupt="code_review",
                        action="approve",
                        reviewer="qa_user",
                    )

                reloaded = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
                self.assertEqual(reloaded.dependency_review_status, status)
                self.assertEqual(reloaded.datasets["ADAE"].current_interrupt.name, "code_review")
                self.assertFalse((study_dir / "runs" / run_id / "review" / "adae_code_review.json").exists())

    def test_gateway_submit_graph_command_rejects_reserved_execute_after_approval(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_graph_command_reserved_execute") / "PSY201"
        run_id = "run_lg3_gateway_graph_command_reserved_execute"
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
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with self.assertRaisesRegex(ValueError, "execute_after_approval is reserved"):
            gateway.submit_graph_command(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                interrupt="code_review",
                action="approve",
                reviewer="qa_user",
                execute_after_approval=True,
            )

    def test_gateway_submit_graph_command_rejects_wrong_interrupt(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_graph_command_wrong_interrupt") / "PSY201"
        run_id = "run_lg3_gateway_graph_command_wrong_interrupt"
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
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with self.assertRaisesRegex(ValueError, "does not match current interrupt code_review"):
            gateway.submit_graph_command(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                interrupt="draft_spec_review",
                action="approve",
                reviewer="qa_user",
            )

    def test_gateway_lg3_native_full_run_metadata_marks_bound_native_interrupt_resume(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_bound_native_interrupt") / "PSY201"
        run_id = "run_lg3_native_full_run_bound_native_interrupt"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        checkpoint_path = study_dir / "runs" / run_id / "langgraph_checkpoints.sqlite"
        runtime_payload = {
            "source_of_truth": "graph_state_json",
            "graph_state_path": str((study_dir / "runs" / run_id / "graph_state.json").as_posix()),
            "workflow_projection_path": str((study_dir / "runs" / run_id / "workflow_state.json").as_posix()),
            "checkpoint_ledger_path": str((study_dir / "runs" / run_id / "graph_checkpoints.sqlite").as_posix()),
            "checkpoint_ledger_role": "product_audit_ledger",
            "langgraph_checkpoint_path": str(checkpoint_path.as_posix()),
            "langgraph_checkpointer_type": "TestPersistentCheckpointer",
            "langgraph_checkpointer_backend": "sqlite",
            "langgraph_checkpointer_persistent": True,
            "native_interrupt_resume": True,
            "native_interrupt_resume_scope": "native_pilot_interrupts_only",
            "restart_recovery_source": "test_runtime_bound_checkpointer",
            "notes": ["Test double for runtime-bound native interrupt metadata."],
        }
        gateway = GraphGateway()

        with (
            patch("adam_agent.graph.gateway.describe_checkpointer", side_effect=lambda **_: dict(runtime_payload)),
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch.object(gateway, "_native_interrupt_checkpoint_path", return_value=str(checkpoint_path.as_posix())),
        ):
            result = gateway.start_native_dataset_full_run(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(contract["resume_mode"], "graph_state_full_run_compatibility")
        self.assertTrue(contract["compatibility_resume_available"])
        self.assertTrue(contract["compatibility_resume_currently_available"])
        self.assertTrue(contract["graph_state_resume_available"])
        self.assertFalse(contract["durable_resume_available"])
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertEqual(contract["durable_full_run_resume_boundary"], "not_implemented")
        self.assertTrue(contract["durable_native_interrupt_resume_available"])
        self.assertTrue(contract["durable_native_interrupt_checkpointer_bound"])
        self.assertEqual(contract["durable_native_resume_scope"], "native_pilot_interrupts_only")
        self.assertEqual(contract["durable_native_interrupt_resume_boundary"], "durable_native_interrupt_resume")

    def test_gateway_lg3_native_dataset_full_run_approval_executes(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_execute") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "dm.csv").write_text("USUBJID,AGE\n01,50\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        output_artifact = ArtifactRef(
            artifact_id="output_adam_psy201_run_lg3_native_full_run_adsl",
            kind="output_adam",
            path="runs/run_lg3_native_full_run_execute/outputs/adsl.csv",
            sha256=f"sha256:{'a' * 64}",
            dataset="ADSL",
            format="csv",
            role="output",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_native_full_run_adsl",
            kind="validation_report",
            path="runs/run_lg3_native_full_run_execute/validation/adsl_validation_report.json",
            sha256=f"sha256:{'b' * 64}",
            dataset="ADSL",
            format="json",
            role="output",
        )
        fake_execution = SimpleNamespace(
            terminal_failure=False,
            response_status="completed",
            validation_status="pass",
            output_path="runs/run_lg3_native_full_run_execute/outputs/adsl.csv",
            validation_report_path="runs/run_lg3_native_full_run_execute/validation/adsl_validation_report.json",
            diagnostics_path=None,
            errors=[],
            warnings=[],
            validation_report={"status": "pass", "errors": [], "warnings": []},
            failure_records=[],
            artifacts={"output_adam": output_artifact, "validation_report": validation_artifact},
        )
        gateway = GraphGateway()
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_execute",
            dataset="ADSL",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_execution):
            result = gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id="run_lg3_native_full_run_execute",
                dataset="ADSL",
                decision="approve",
                reviewer="tester",
                notes="Approve generated R and execute the LG3 full-run contract.",
            )

        dataset_state = result.graph_state.datasets["ADSL"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.phase, "executed")
        self.assertTrue(result.approved)
        self.assertIsNotNone(result.execution)
        self.assertEqual(result.execution.status, "completed")
        self.assertEqual(dataset_state.status, "completed")
        self.assertEqual(dataset_state.execution_state["status"], "completed")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "executed")
        self.assertTrue(contract["approved"])
        self.assertTrue(contract["executed_after_approval"])
        self.assertEqual(contract["last_interrupt"], "code_review")
        self.assertEqual(contract["resume_mode"], "graph_state_full_run_compatibility")
        self.assertTrue(contract["compatibility_resume_available"])
        self.assertFalse(contract["compatibility_resume_currently_available"])
        self.assertEqual(contract["compatibility_resume_boundary"], "historical_contract_only")
        self.assertFalse(contract["durable_resume_available"])
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertEqual(
            result.graph_state.runtime_persistence["native_code_review_resume"]["native_status"],
            "approved",
        )
        self.assertEqual(
            result.graph_state.runtime_persistence["native_code_review_resume"]["resume_source"],
            "langgraph_command_resume",
        )
        self.assertEqual(
            result.graph_state.runtime_persistence["native_dataset_product_loop_resume"],
            {
                "resumed": True,
                "dataset": "ADSL",
                "action": "approve",
                "executed_after_approval": True,
            },
        )
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg3_native_full_run_execute"
                / "review"
                / "adsl_code_review.json"
            ).exists()
        )

    def test_gateway_lg3_native_dataset_full_run_approval_can_pause_before_execution(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_approve_pause") / "PSY201"
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
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_approve_pause",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch.object(gateway, "execute_approved_code") as execute_approved:
            result = gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id="run_lg3_native_full_run_approve_pause",
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
                notes="Approve generated R but leave execution for a later explicit step.",
                execute_after_approval=False,
            )

        execute_approved.assert_not_called()
        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.phase, "reviewed")
        self.assertTrue(result.approved)
        self.assertIsNone(result.execution)
        self.assertIsNone(result.current_interrupt)
        self.assertEqual(dataset_state.status, "pending")
        self.assertEqual(dataset_state.code_state["status"], "approved")
        self.assertFalse(dataset_state.execution_state)
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "reviewed")
        self.assertTrue(contract["approved"])
        self.assertFalse(contract["executed_after_approval"])
        self.assertEqual(
            result.graph_state.runtime_persistence["native_code_review_resume"]["native_status"],
            "approved",
        )
        self.assertEqual(
            result.graph_state.runtime_persistence["native_code_review_resume"]["resume_source"],
            "langgraph_command_resume",
        )
        self.assertNotIn("native_dataset_product_loop_resume", result.graph_state.runtime_persistence)
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg3_native_full_run_approve_pause"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_gateway_lg3_paused_full_run_explicit_execution_updates_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_pause_then_execute") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        output_artifact = ArtifactRef(
            artifact_id="output_adam_psy201_run_lg3_pause_then_execute_adae",
            kind="output_adam",
            path="runs/run_lg3_pause_then_execute/outputs/adae.csv",
            sha256=f"sha256:{'c' * 64}",
            dataset="ADAE",
            format="csv",
            role="output",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_pause_then_execute_adae",
            kind="validation_report",
            path="runs/run_lg3_pause_then_execute/validation/adae_validation_report.json",
            sha256=f"sha256:{'d' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        fake_execution = SimpleNamespace(
            terminal_failure=False,
            response_status="completed",
            validation_status="pass",
            output_path="runs/run_lg3_pause_then_execute/outputs/adae.csv",
            validation_report_path="runs/run_lg3_pause_then_execute/validation/adae_validation_report.json",
            diagnostics_path=None,
            errors=[],
            warnings=[],
            validation_report={"status": "pass", "errors": [], "warnings": []},
            failure_records=[],
            artifacts={"output_adam": output_artifact, "validation_report": validation_artifact},
        )
        gateway = GraphGateway()
        run_id = "run_lg3_pause_then_execute"
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        approved = gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Approve code but leave R execution to the explicit UI action.",
            execute_after_approval=False,
        )
        self.assertEqual(approved.graph_state.runtime_persistence["native_dataset_full_run"]["phase"], "reviewed")

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass", "errors": [], "warnings": []},
                "output_path": fake_execution.output_path,
                "validation_report_path": fake_execution.validation_report_path,
                "diagnostics_path": "",
                "real_run_artifacts": fake_execution.artifacts,
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [],
                "agent_node_outputs": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            executed = gateway.execute_native_dataset_full_run(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        contract = executed.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(executed.status, "completed")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "executed")
        self.assertEqual(contract["last_interrupt"], "code_review")
        self.assertTrue(contract["approved"])
        self.assertTrue(contract["executed_after_approval"])
        self.assertFalse(contract["terminal_failure"])
        self.assertFalse(contract["durable_resume_available"])
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertEqual(contract["durable_full_run_resume_boundary"], "not_implemented")
        self.assertNotIn("next_action", contract)

    def test_gateway_native_full_run_execute_rejects_non_lg3_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_execute_rejects_non_lg3") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        code_dir = study_dir / "runs" / "run_non_lg3_execute" / "code"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        code_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        code_path = code_dir / "build_adae.R"
        code_path.write_text("write.csv(data.frame(AETERM='HEADACHE'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_non_lg3_execute", "ADAE", code_path)
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_non_lg3_execute",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            spec_source="input_spec",
            spec_path=spec_dir / "adae.json",
            spec_sha256=f"sha256:{sha256_file(spec_dir / 'adae.json')}",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        gateway.review_code(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_non_lg3_execute",
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        with self.assertRaisesRegex(ValueError, "No LG3 native full-run contract exists"):
            gateway.execute_native_dataset_full_run(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_non_lg3_execute",
                dataset="ADAE",
            )

    def test_gateway_native_full_run_execute_rejects_polluted_lg3_boundary_without_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_execute_rejects_polluted_contract") / "PSY201"
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
        run_id = "run_lg3_polluted_contract"
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        state.runtime_persistence["native_dataset_full_run"] = {
            "dataset": "ADAE",
            "boundary": "lg3_backend_contract",
            "phase": "waiting_for_human_gate",
        }
        gateway._persist_graph_state(
            study_dir,
            state,
            node="test_seed_polluted_lg3_boundary_without_contract",
            runtime_persistence_extra={"native_dataset_full_run": state.runtime_persistence["native_dataset_full_run"]},
        )

        with self.assertRaisesRegex(ValueError, "No LG3 native full-run contract exists"):
            gateway.execute_native_dataset_full_run(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

    def test_gateway_lg3_paused_explicit_execution_does_not_claim_durable_resume(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_pause_execute_no_durable_claim") / "PSY201"
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
        run_id = "run_lg3_pause_execute_no_durable"
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            execute_after_approval=False,
        )

        with (
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph,
        ):
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass", "errors": [], "warnings": []},
                "output_path": "runs/run_lg3_pause_execute_no_durable/outputs/adae.csv",
                "validation_report_path": "runs/run_lg3_pause_execute_no_durable/validation/adae_validation_report.json",
                "diagnostics_path": "",
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [],
                "agent_node_outputs": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            executed = gateway.execute_approved_code(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        contract = executed.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(contract["phase"], "executed")
        self.assertFalse(contract["durable_resume_available"])
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertFalse(contract["durable_native_interrupt_resume_available"])

    def test_gateway_lg3_paused_full_run_explicit_execution_failure_updates_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_pause_then_fail") / "PSY201"
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
        run_id = "run_lg3_pause_then_fail"
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Approve code but leave R execution to the explicit UI action.",
            execute_after_approval=False,
        )

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "failed",
                "response_status": "terminal_failure",
                "real_validation_status": "fail",
                "terminal_failure": True,
                "validation_report": {"status": "fail", "errors": ["R execution failed"], "warnings": []},
                "output_path": "",
                "validation_report_path": "runs/run_lg3_pause_then_fail/validation/adae_validation_report.json",
                "diagnostics_path": "runs/run_lg3_pause_then_fail/diagnostics/adae_failure_report.json",
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [],
                "agent_node_outputs": [],
                "risk_flags": [],
                "execution_errors": ["R execution failed"],
                "execution_warnings": [],
            }
            executed = gateway.execute_approved_code(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        contract = executed.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(executed.status, "terminal_failure")
        self.assertTrue(executed.terminal_failure)
        self.assertEqual(executed.graph_state.datasets["ADAE"].current_interrupt.name, "terminal_failure")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "terminal_failure")
        self.assertEqual(contract["current_interrupt"], "terminal_failure")
        self.assertEqual(contract["next_action"], "terminal_failure")
        self.assertEqual(contract["last_interrupt"], "code_review")
        self.assertTrue(contract["approved"])
        self.assertTrue(contract["executed_after_approval"])
        self.assertTrue(contract["terminal_failure"])

    def test_gateway_lg3_paused_repair_followup_execution_preserves_context(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_paused_repair_followup_execute") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        failure_validation = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_paused_repair_adae_failure",
            kind="validation_report",
            path="runs/run_lg3_paused_repair/validation/adae_failure_validation_report.json",
            sha256=f"sha256:{'e' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        gateway = GraphGateway()
        run_id = "run_lg3_paused_repair"
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code") as execute_r:
            execute_r.return_value = SimpleNamespace(
                terminal_failure=True,
                response_status="terminal_failure",
                validation_status="fail",
                output_path=None,
                validation_report_path="runs/run_lg3_paused_repair/validation/adae_failure_validation_report.json",
                diagnostics_path="runs/run_lg3_paused_repair/diagnostics/adae_failure_report.json",
                errors=["R execution failed"],
                warnings=[],
                validation_report={"status": "fail", "errors": ["R execution failed"], "warnings": []},
                failure_records=[
                    FailureRecord(
                        failure_id="failure_lg3_paused_repair_adae",
                        dataset="ADAE",
                        node="execute_approved_code",
                        failure_type="sandbox_error",
                        message="R execution failed.",
                        root_cause="r_runtime_error",
                        recommended_route="repair_code",
                    )
                ],
                artifacts={"validation_report": failure_validation},
            )
            gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
            )
        gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="repair_code",
            reviewer="tester",
        )
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        paused = gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            execute_after_approval=False,
        )
        paused_contract = paused.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertTrue(paused_contract["repair_or_revision_continued"])
        self.assertEqual(paused_contract["terminal_failure_followup"]["action"], "repair_code")

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass", "errors": [], "warnings": []},
                "output_path": "runs/run_lg3_paused_repair/outputs/adae.csv",
                "validation_report_path": "runs/run_lg3_paused_repair/validation/adae_validation_report.json",
                "diagnostics_path": "",
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [],
                "agent_node_outputs": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            executed = gateway.execute_approved_code(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        contract = executed.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(contract["phase"], "executed")
        self.assertTrue(contract["repair_or_revision_continued"])
        self.assertEqual(contract["terminal_failure_followup"]["action"], "repair_code")
        self.assertEqual(contract["terminal_failure_followup"]["last_interrupt"], "terminal_failure")
        self.assertEqual(contract["terminal_failure_followup"]["next_action"], "repair_generated_code")

    def test_gateway_lg3_native_resume_entrypoint_preserves_full_run_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_resume_full_run_contract") / "PSY201"
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
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_resume_full_run_contract",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        checkpoint_path = (
            study_dir
            / "runs"
            / "run_lg3_native_resume_full_run_contract"
            / "langgraph_checkpoints.sqlite"
        )
        graph_state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg3_native_resume_full_run_contract",
        )
        gateway._persist_graph_state(
            study_dir,
            graph_state,
            node="test_seed_lg3_native_resume_runtime",
            runtime_persistence_extra={
                "native_dataset_full_run": graph_state.runtime_persistence["native_dataset_full_run"],
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(checkpoint_path.as_posix()),
            },
        )

        with (
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch.object(gateway, "_native_interrupt_checkpoint_path", return_value=str(checkpoint_path.as_posix())),
            patch.object(gateway, "execute_approved_code") as execute_approved,
        ):
            result = gateway.resume_native_dataset_interrupt(
                study_dir=study_dir,
                run_id="run_lg3_native_resume_full_run_contract",
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
                notes="Resume the LG3 full-run contract through the native-resume entrypoint.",
                execute_after_approval=False,
            )

        execute_approved.assert_not_called()
        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.interrupt, "code_review")
        self.assertEqual(result.decision, "approve")
        self.assertIsNone(result.execution)
        self.assertEqual(dataset_state.status, "pending")
        self.assertEqual(dataset_state.code_state["status"], "approved")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "reviewed")
        self.assertTrue(contract["approved"])
        self.assertFalse(contract["executed_after_approval"])

    def test_gateway_lg3_native_resume_entrypoint_uses_native_code_review_resume_path(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_resume_full_run_native_code_path") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        run_id = "run_lg3_native_resume_full_run_native_code_path"
        gateway = GraphGateway()
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        checkpoint_path = study_dir / "runs" / run_id / "langgraph_checkpoints.sqlite"
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        full_run_contract = dict(graph_state.runtime_persistence["native_dataset_full_run"])
        gateway._persist_graph_state(
            study_dir,
            graph_state,
            node="test_seed_lg3_native_resume_runtime",
            runtime_persistence_extra={
                "native_dataset_full_run": full_run_contract,
                "native_study_product_loop": {
                    "boundary": "native_study_product_loop",
                    "full_run_datasets": {"ADAE": dict(full_run_contract)},
                },
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(checkpoint_path.as_posix()),
            },
        )

        with (
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch.object(gateway, "_native_interrupt_checkpoint_path", return_value=str(checkpoint_path.as_posix())),
            patch.object(gateway, "resume_native_code_review", wraps=gateway.resume_native_code_review) as native_code,
            patch.object(
                gateway,
                "resume_native_dataset_full_run",
                wraps=gateway.resume_native_dataset_full_run,
            ) as compatibility_resume,
        ):
            result = gateway.resume_native_dataset_interrupt(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
                notes="Durable native resume should use DatasetGraph code_review resume.",
                execute_after_approval=False,
            )

        native_code.assert_called_once()
        compatibility_resume.assert_not_called()
        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.resume_path, "durable_native_interrupt")
        self.assertEqual(result.full_run_resume_path, "native_code_review")
        self.assertEqual(result.interrupt, "code_review")
        self.assertEqual(dataset_state.code_state["status"], "approved")
        self.assertEqual(contract["phase"], "reviewed")
        self.assertEqual(contract["last_interrupt"], "code_review")
        self.assertEqual(contract["resume_path"], "durable_native_interrupt")
        self.assertEqual(contract["full_run_resume_path"], "native_code_review")
        self.assertEqual(contract["resume_mode"], "graph_state_full_run_compatibility")
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertFalse(contract["executed_after_approval"])
        nested_contract = result.graph_state.runtime_persistence["native_study_product_loop"]["full_run_datasets"][
            "ADAE"
        ]
        self.assertEqual(nested_contract["phase"], "reviewed")
        self.assertIsNone(nested_contract["current_interrupt"])
        self.assertFalse(nested_contract["compatibility_resume_currently_available"])
        self.assertEqual(nested_contract["compatibility_resume_boundary"], "historical_contract_only")

    def test_gateway_lg3_native_resume_entrypoint_uses_native_draft_spec_resume_path(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_resume_full_run_native_draft_path") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        run_id = "run_lg3_native_resume_full_run_native_draft_path"
        gateway = GraphGateway()
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        checkpoint_path = study_dir / "runs" / run_id / "langgraph_checkpoints.sqlite"
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        gateway._persist_graph_state(
            study_dir,
            graph_state,
            node="test_seed_lg3_native_resume_runtime",
            runtime_persistence_extra={
                "native_dataset_full_run": graph_state.runtime_persistence["native_dataset_full_run"],
                "native_interrupt_resume": True,
                "native_interrupt_resume_scope": "native_pilot_interrupts_only",
                "restart_recovery_source": "langgraph_sqlite_checkpointer",
                "langgraph_checkpoint_path": str(checkpoint_path.as_posix()),
            },
        )

        with (
            patch.object(gateway, "native_interrupt_resume_available", return_value=True),
            patch.object(gateway, "_native_interrupt_checkpoint_path", return_value=str(checkpoint_path.as_posix())),
            patch.object(
                gateway,
                "resume_native_dataset_product_loop_draft_spec",
                wraps=gateway.resume_native_dataset_product_loop_draft_spec,
            ) as native_draft,
            patch.object(
                gateway,
                "resume_native_dataset_full_run",
                wraps=gateway.resume_native_dataset_full_run,
            ) as compatibility_resume,
        ):
            result = gateway.resume_native_dataset_interrupt(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
                notes="Durable native resume should use DatasetGraph draft_spec resume.",
                execute_after_approval=False,
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        native_draft.assert_called_once()
        compatibility_resume.assert_not_called()
        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.resume_path, "durable_native_interrupt")
        self.assertEqual(result.full_run_resume_path, "native_draft_spec_review")
        self.assertEqual(result.interrupt, "draft_spec_review")
        self.assertEqual(dataset_state.spec_state["status"], "approved")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(contract["phase"], "waiting_for_human_gate")
        self.assertEqual(contract["current_interrupt"], "code_review")
        self.assertEqual(contract["last_interrupt"], "draft_spec_review")
        self.assertTrue(contract["code_generation_continued"])
        self.assertEqual(contract["resume_path"], "durable_native_interrupt")
        self.assertEqual(contract["full_run_resume_path"], "native_draft_spec_review")
        self.assertEqual(contract["resume_mode"], "graph_state_full_run_compatibility")
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertFalse(contract["executed_after_approval"])

    def test_gateway_lg3_native_dataset_full_run_reject_does_not_execute(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_reject") / "PSY201"
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
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_reject",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch.object(gateway, "execute_approved_code") as execute_approved:
            result = gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id="run_lg3_native_full_run_reject",
                dataset="ADAE",
                decision="reject",
                reviewer="tester",
                notes="Generated R is not acceptable.",
            )

        execute_approved.assert_not_called()
        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.phase, "waiting_for_human_gate")
        self.assertEqual(result.current_interrupt, "code_review")
        self.assertFalse(result.approved)
        self.assertIsNone(result.execution)
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.code_state["status"], "rejected")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "waiting_for_human_gate")
        self.assertFalse(contract["approved"])
        self.assertFalse(contract["executed_after_approval"])
        self.assertEqual(
            result.graph_state.runtime_persistence["native_code_review_resume"]["native_status"],
            "rejected",
        )
        self.assertEqual(
            result.graph_state.runtime_persistence["native_code_review_resume"]["resume_source"],
            "langgraph_command_resume",
        )
        self.assertNotIn("native_dataset_product_loop_resume", result.graph_state.runtime_persistence)

    def test_gateway_lg3_native_dataset_full_run_uses_approved_draft_spec(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_approved_draft") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "runs" / "run_lg3_native_full_run_approved_draft" / "specs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        draft_path = spec_dir / "adae_draft_spec.json"
        fingerprint = input_fingerprint(study_dir)
        draft_path.write_text(
            json.dumps(
                {
                    "dataset": "ADAE",
                    "input_fingerprint": fingerprint,
                    "variables": [{"variable": "AETERM", "source_domains": ["AE"]}],
                }
            ),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_approved_draft",
            dataset="ADAE",
            draft_spec_path=draft_path,
        )
        reviewed = gateway.review_draft_spec(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_approved_draft",
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Approve generated draft spec for LG3 full-run start.",
        )

        result = gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_approved_draft",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.phase, "waiting_for_human_gate")
        self.assertEqual(result.current_interrupt, "code_review")
        self.assertEqual(dataset_state.spec_state["status"], "approved")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(dataset_state.code_state["spec_source"], "approved_draft_spec")
        self.assertEqual(dataset_state.code_state["spec_path"], reviewed.approved_spec_path)
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg3_native_full_run_approved_draft"
                / "code"
                / "build_adae.R"
            ).exists()
        )

    def test_gateway_lg3_native_dataset_full_run_draft_approval_continues_to_code_review(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_draft_to_code") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()

        started = gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_draft_to_code",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model", "api_key": "must-not-persist"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        self.assertEqual(started.current_interrupt, "draft_spec_review")
        started_contract = started.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(started_contract["boundary"], "lg3_backend_contract")
        self.assertTrue(started_contract["llm_provider"]["api_key_present"])
        self.assertNotIn("api_key", started_contract["llm_provider"])

        result = gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id="run_lg3_native_full_run_draft_to_code",
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Approve draft spec and continue LG3 full-run to code review.",
            llm_provider={"provider": "mock", "model": "mock-model", "api_key": "must-not-persist"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        contract = result.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(result.phase, "waiting_for_human_gate")
        self.assertEqual(result.current_interrupt, "code_review")
        self.assertEqual(result.decision, "approve")
        self.assertTrue(result.approved)
        self.assertIsNone(result.execution)
        self.assertEqual(dataset_state.spec_state["status"], "approved")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(dataset_state.code_state["spec_source"], "approved_draft_spec")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "waiting_for_human_gate")
        self.assertEqual(contract["last_interrupt"], "draft_spec_review")
        self.assertTrue(contract["code_generation_continued"])
        self.assertFalse(contract["executed_after_approval"])
        self.assertEqual(
            result.graph_state.runtime_persistence["native_draft_spec_review_resume"]["resume_source"],
            "langgraph_command_resume",
        )
        self.assertIn("native_dataset_product_loop_draft_resume", result.graph_state.runtime_persistence)
        self.assertTrue(contract["llm_provider"]["api_key_present"])
        self.assertNotIn("api_key", contract["llm_provider"])
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg3_native_full_run_draft_to_code"
                / "approved_specs"
                / "adae_approved_spec.json"
            ).exists()
        )
        self.assertTrue(
            (
                study_dir
                / "runs"
                / "run_lg3_native_full_run_draft_to_code"
                / "code"
                / "build_adae.R"
            ).exists()
        )
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_lg3_native_full_run_draft_to_code"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_gateway_lg3_native_dataset_full_run_draft_approval_requires_llm_config(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_draft_missing_llm") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_draft_missing_llm",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with self.assertRaisesRegex(ValueError, "requires llm_provider and llm_exposure"):
            gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id="run_lg3_native_full_run_draft_missing_llm",
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
                notes="Do not continue without explicit LLM settings.",
            )

        reloaded = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg3_native_full_run_draft_missing_llm",
        )
        self.assertEqual(reloaded.datasets["ADAE"].current_interrupt.name, "draft_spec_review")
        self.assertFalse((study_dir / "runs" / "run_lg3_native_full_run_draft_missing_llm" / "code").exists())

    def test_gateway_lg3_native_full_run_terminal_failure_records_contract_boundary(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_terminal_failure") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_terminal_failure_adae",
            kind="validation_report",
            path="runs/run_lg3_native_full_run_terminal_failure/validation/adae_validation_report.json",
            sha256=f"sha256:{'c' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        failure = FailureRecord(
            failure_id="failure_lg3_adae_terminal",
            dataset="ADAE",
            node="execute_approved_code",
            failure_type="sandbox_error",
            message="R execution failed.",
            root_cause="r_runtime_error",
            recommended_route="repair_code",
        )
        fake_execution = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path="runs/run_lg3_native_full_run_terminal_failure/validation/adae_validation_report.json",
            diagnostics_path="runs/run_lg3_native_full_run_terminal_failure/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail", "errors": ["R execution failed"], "warnings": []},
            failure_records=[failure],
            artifacts={"validation_report": validation_artifact},
        )
        gateway = GraphGateway()
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_terminal_failure",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_execution):
            failed = gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id="run_lg3_native_full_run_terminal_failure",
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
                notes="Approve code and reach terminal failure through LG3 full-run.",
            )

        failed_contract = failed.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(failed.phase, "terminal_failure")
        self.assertTrue(failed.execution.terminal_failure)
        self.assertEqual(failed.current_interrupt, "terminal_failure")
        self.assertEqual(failed_contract["boundary"], "lg3_backend_contract")
        self.assertEqual(failed_contract["phase"], "terminal_failure")
        self.assertTrue(failed_contract["terminal_failure"])
        self.assertTrue(failed_contract["executed_after_approval"])

        triaged = gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id="run_lg3_native_full_run_terminal_failure",
            dataset="ADAE",
            decision="repair_code",
            reviewer="tester",
            notes="Route failed LG3 full-run to repair code through the formal terminal-failure review.",
        )

        dataset_state = triaged.graph_state.datasets["ADAE"]
        triage_contract = triaged.graph_state.runtime_persistence.get("native_dataset_full_run", {})
        self.assertEqual(triaged.decision, "repair_code")
        self.assertEqual(triaged.current_interrupt, "terminal_failure")
        self.assertEqual(triaged.next_action, "repair_generated_code")
        self.assertEqual(dataset_state.execution_state["terminal_failure_review"]["action"], "repair_code")
        self.assertEqual(triage_contract["boundary"], "lg3_backend_contract")
        self.assertEqual(triage_contract["phase"], "terminal_failure_triaged")
        self.assertEqual(triage_contract["last_interrupt"], "terminal_failure")
        self.assertEqual(triage_contract["decision"], "repair_code")
        self.assertEqual(triage_contract["next_action"], "repair_generated_code")
        self.assertTrue(triage_contract["terminal_failure"])
        self.assertFalse(triage_contract["compatibility_resume_currently_available"])
        self.assertEqual(triage_contract["compatibility_resume_boundary"], "terminal_failure_review_endpoint")

    def test_gateway_lg3_native_full_run_repair_followup_preserves_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_repair_followup") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_repair_followup_adae",
            kind="validation_report",
            path="runs/run_lg3_native_full_run_repair_followup/validation/adae_validation_report.json",
            sha256=f"sha256:{'d' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        fake_execution = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path="runs/run_lg3_native_full_run_repair_followup/validation/adae_validation_report.json",
            diagnostics_path="runs/run_lg3_native_full_run_repair_followup/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail", "errors": ["R execution failed"], "warnings": []},
            failure_records=[
                FailureRecord(
                    failure_id="failure_lg3_adae_repair_followup",
                    dataset="ADAE",
                    node="execute_approved_code",
                    failure_type="sandbox_error",
                    message="R execution failed.",
                    root_cause="r_runtime_error",
                    recommended_route="repair_code",
                )
            ],
            artifacts={"validation_report": validation_artifact},
        )
        gateway = GraphGateway()
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_repair_followup",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_execution):
            gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id="run_lg3_native_full_run_repair_followup",
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
            )
        gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id="run_lg3_native_full_run_repair_followup",
            dataset="ADAE",
            decision="repair_code",
            reviewer="tester",
        )

        repaired = gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_repair_followup",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = repaired.graph_state.datasets["ADAE"]
        contract = repaired.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(repaired.phase, "waiting_for_human_gate")
        self.assertEqual(repaired.current_interrupt, "code_review")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.code_state["terminal_failure_followup"]["action"], "repair_code")
        self.assertEqual(dataset_state.execution_state["terminal_failure_followup_consumed_by"], "generate_code")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "waiting_for_human_gate")
        self.assertEqual(contract["last_interrupt"], "terminal_failure")
        self.assertEqual(contract["decision"], "repair_code")
        self.assertEqual(contract["next_action"], "code_review")
        self.assertTrue(contract["terminal_failure"])
        self.assertTrue(contract["repair_or_revision_continued"])

    def test_gateway_lg3_native_full_run_revise_followup_preserves_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_revise_followup") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_revise_followup_adae",
            kind="validation_report",
            path="runs/run_lg3_native_full_run_revise_followup/validation/adae_validation_report.json",
            sha256=f"sha256:{'e' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        fake_execution = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path="runs/run_lg3_native_full_run_revise_followup/validation/adae_validation_report.json",
            diagnostics_path="runs/run_lg3_native_full_run_revise_followup/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail", "errors": ["R execution failed"], "warnings": []},
            failure_records=[
                FailureRecord(
                    failure_id="failure_lg3_adae_revise_followup",
                    dataset="ADAE",
                    node="execute_approved_code",
                    failure_type="sandbox_error",
                    message="R execution failed.",
                    root_cause="spec_error",
                    recommended_route="revise_spec",
                )
            ],
            artifacts={"validation_report": validation_artifact},
        )
        gateway = GraphGateway()
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_revise_followup",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_execution):
            gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id="run_lg3_native_full_run_revise_followup",
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
            )
        gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id="run_lg3_native_full_run_revise_followup",
            dataset="ADAE",
            decision="revise_spec",
            reviewer="tester",
        )

        revised = gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_native_full_run_revise_followup",
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        dataset_state = revised.graph_state.datasets["ADAE"]
        contract = revised.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(revised.phase, "waiting_for_human_gate")
        self.assertEqual(revised.current_interrupt, "draft_spec_review")
        self.assertEqual(dataset_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(dataset_state.spec_state["terminal_failure_followup"]["action"], "revise_spec")
        self.assertEqual(dataset_state.execution_state["terminal_failure_followup_consumed_by"], "draft_spec")
        self.assertEqual(dataset_state.code_state["status"], "stale")
        self.assertEqual(dataset_state.code_state["terminal_failure_followup"]["action"], "revise_spec")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "waiting_for_human_gate")
        self.assertEqual(contract["last_interrupt"], "terminal_failure")
        self.assertEqual(contract["decision"], "revise_spec")
        self.assertEqual(contract["next_action"], "draft_spec_review")
        self.assertTrue(contract["terminal_failure"])
        self.assertTrue(contract["repair_or_revision_continued"])

    def test_gateway_lg3_native_full_run_metadata_ignores_foreign_contract(self) -> None:
        def seed_and_restart(case_name: str, stale_payload: dict[str, object]) -> dict[str, object]:
            study_dir = _workspace_dir(case_name) / "PSY201"
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
            run_id = f"run_{case_name}"
            gateway.start_native_dataset_full_run(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )
            state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
            state.runtime_persistence["native_dataset_full_run"] = stale_payload
            gateway._persist_graph_state(
                study_dir,
                state,
                node="test_seed_foreign_lg3_metadata",
                runtime_persistence_extra={"native_dataset_full_run": stale_payload},
            )

            restarted = gateway.start_native_dataset_full_run(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
                llm_provider={
                    "provider": "mock",
                    "model": "mock-model",
                    "api_key": "must-not-persist",
                },
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )
            return restarted.graph_state.runtime_persistence["native_dataset_full_run"]

        stale_terminal_followup = {
            "dataset": "ADCM",
            "phase": "terminal_failure_triaged",
            "boundary": "lg3_backend_contract",
            "last_interrupt": "terminal_failure",
            "decision": "repair_code",
            "terminal_failure": True,
            "foreign_marker": "must-not-carry",
        }
        contract = seed_and_restart("lg3_foreign_dataset_contract", stale_terminal_followup)
        self.assertEqual(contract["dataset"], "ADAE")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertFalse(contract["repair_or_revision_continued"])
        self.assertNotIn("last_interrupt", contract)
        self.assertNotIn("decision", contract)
        self.assertNotIn("terminal_failure", contract)
        self.assertNotIn("foreign_marker", contract)
        self.assertTrue(contract["llm_provider"]["api_key_present"])
        self.assertNotIn("api_key", contract["llm_provider"])

        stale_non_lg3_boundary = dict(stale_terminal_followup)
        stale_non_lg3_boundary["dataset"] = "ADAE"
        stale_non_lg3_boundary["boundary"] = "dataset_product_loop_pilot_only"
        contract = seed_and_restart("lg3_foreign_boundary_contract", stale_non_lg3_boundary)
        self.assertEqual(contract["dataset"], "ADAE")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertFalse(contract["repair_or_revision_continued"])
        self.assertNotIn("last_interrupt", contract)
        self.assertNotIn("decision", contract)
        self.assertNotIn("terminal_failure", contract)
        self.assertNotIn("foreign_marker", contract)
        self.assertTrue(contract["llm_provider"]["api_key_present"])
        self.assertNotIn("api_key", contract["llm_provider"])

    def test_gateway_lg3_native_full_run_repair_execution_keeps_followup_context(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_repair_execute_context") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        failure_validation = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_repair_execute_context_adae_failure",
            kind="validation_report",
            path="runs/run_lg3_repair_execute_context/validation/adae_failure_validation_report.json",
            sha256=f"sha256:{'f' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        failed_execution = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path="runs/run_lg3_repair_execute_context/validation/adae_failure_validation_report.json",
            diagnostics_path="runs/run_lg3_repair_execute_context/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail", "errors": ["R execution failed"], "warnings": []},
            failure_records=[
                FailureRecord(
                    failure_id="failure_lg3_adae_repair_execute_context",
                    dataset="ADAE",
                    node="execute_approved_code",
                    failure_type="sandbox_error",
                    message="R execution failed.",
                    root_cause="r_runtime_error",
                    recommended_route="repair_code",
                )
            ],
            artifacts={"validation_report": failure_validation},
        )
        success_output = ArtifactRef(
            artifact_id="output_adam_psy201_run_lg3_repair_execute_context_adae",
            kind="output_adam",
            path="runs/run_lg3_repair_execute_context/outputs/adae.csv",
            sha256=f"sha256:{'1' * 64}",
            dataset="ADAE",
            format="csv",
            role="output",
        )
        success_validation = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_repair_execute_context_adae_success",
            kind="validation_report",
            path="runs/run_lg3_repair_execute_context/validation/adae_validation_report.json",
            sha256=f"sha256:{'2' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        successful_execution = SimpleNamespace(
            terminal_failure=False,
            response_status="completed",
            validation_status="pass",
            output_path="runs/run_lg3_repair_execute_context/outputs/adae.csv",
            validation_report_path="runs/run_lg3_repair_execute_context/validation/adae_validation_report.json",
            diagnostics_path=None,
            errors=[],
            warnings=[],
            validation_report={"status": "pass", "errors": [], "warnings": []},
            failure_records=[],
            artifacts={"output_adam": success_output, "validation_report": success_validation},
        )
        gateway = GraphGateway()
        run_id = "run_lg3_repair_execute_context"
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=failed_execution):
            gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
            )
        gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="repair_code",
            reviewer="tester",
        )
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=successful_execution):
            repaired = gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
            )

        contract = repaired.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(repaired.phase, "executed")
        self.assertFalse(contract["terminal_failure"])
        self.assertTrue(contract["repair_or_revision_continued"])
        self.assertNotIn("next_action", contract)
        self.assertEqual(contract["terminal_failure_followup"]["action"], "repair_code")
        self.assertEqual(contract["terminal_failure_followup"]["last_interrupt"], "terminal_failure")
        self.assertEqual(contract["terminal_failure_followup"]["next_action"], "repair_generated_code")

    def test_gateway_lg3_native_full_run_revise_draft_review_keeps_followup_context(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_native_full_run_revise_review_context") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_revise_review_context_adae",
            kind="validation_report",
            path="runs/run_lg3_revise_review_context/validation/adae_validation_report.json",
            sha256=f"sha256:{'3' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        fake_execution = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path="runs/run_lg3_revise_review_context/validation/adae_validation_report.json",
            diagnostics_path="runs/run_lg3_revise_review_context/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail", "errors": ["R execution failed"], "warnings": []},
            failure_records=[
                FailureRecord(
                    failure_id="failure_lg3_adae_revise_review_context",
                    dataset="ADAE",
                    node="execute_approved_code",
                    failure_type="sandbox_error",
                    message="R execution failed.",
                    root_cause="spec_error",
                    recommended_route="revise_spec",
                )
            ],
            artifacts={"validation_report": validation_artifact},
        )
        gateway = GraphGateway()
        run_id = "run_lg3_revise_review_context"
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_execution):
            gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id=run_id,
                dataset="ADAE",
                decision="approve",
                reviewer="tester",
            )
        gateway.review_terminal_failure(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="revise_spec",
            reviewer="tester",
        )
        gateway.start_native_dataset_full_run(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            dataset="ADAE",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        revised = gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        contract = revised.graph_state.runtime_persistence["native_dataset_full_run"]
        self.assertEqual(revised.phase, "waiting_for_human_gate")
        self.assertEqual(revised.current_interrupt, "code_review")
        self.assertTrue(contract["repair_or_revision_continued"])
        self.assertEqual(contract["terminal_failure_followup"]["action"], "revise_spec")
        self.assertEqual(contract["terminal_failure_followup"]["last_interrupt"], "terminal_failure")
        self.assertEqual(contract["terminal_failure_followup"]["next_action"], "revise_approved_spec")
        self.assertTrue(contract["code_generation_continued"])

    def test_gateway_native_study_product_loop_starts_multiple_runnable_datasets(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_multi") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (spec_dir / "adcm.json").write_text(
            json.dumps({"dataset": "ADCM", "variables": [{"variable": "CMTRT", "source_domains": ["CM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()

        result = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_multi",
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        self.assertEqual(result.started_datasets, ["ADAE", "ADCM"])
        self.assertEqual(set(result.dataset_results), {"ADAE", "ADCM"})
        self.assertEqual(result.graph_state.status, "needs_review")
        self.assertEqual(result.graph_state.current_interrupt.name, "code_review")
        self.assertEqual(result.graph_state.current_interrupt.dataset, "ADAE")
        for dataset in ("ADAE", "ADCM"):
            dataset_state = result.graph_state.datasets[dataset]
            self.assertEqual(dataset_state.status, "needs_review")
            self.assertEqual(dataset_state.current_interrupt.name, "code_review")
            self.assertEqual(dataset_state.code_state["status"], "generated")
        review_queue = {(item["dataset"], item["name"]) for item in result.review_queue}
        self.assertIn(("ADAE", "code_review"), review_queue)
        self.assertIn(("ADCM", "code_review"), review_queue)
        self.assertFalse(result.native_resume_available)
        self.assertEqual(result.native_resume_scope, "none")
        self.assertEqual(result.resume_boundary, "graph_state_projection_only")
        self.assertTrue(result.native_resume_has_queue_items)
        self.assertEqual(result.native_resume_queue_item_count, 2)
        self.assertEqual(
            [(item["dataset"], item["interrupt"], item["can_resume"]) for item in result.native_resume_interrupts],
            [("ADAE", "code_review", False), ("ADCM", "code_review", False)],
        )
        self.assertIn("native_study_product_loop", result.graph_state.runtime_persistence)
        self.assertIsInstance(result.dataset_results["ADAE"], GraphGatewayNativeDatasetFullRunResult)
        self.assertIsInstance(result.dataset_results["ADCM"], GraphGatewayNativeDatasetFullRunResult)
        self.assertEqual(
            result.graph_state.runtime_persistence["native_study_product_loop"]["boundary"],
            "study_lg3_full_run_dispatch",
        )
        self.assertEqual(
            result.graph_state.runtime_persistence["native_study_product_loop"]["dataset_entry_contract"],
            "single_dataset_spec_code_review_execute",
        )
        self.assertEqual(
            result.graph_state.runtime_persistence["native_study_product_loop"]["dispatch_status"],
            "datasets_dispatched",
        )
        self.assertEqual(
            set(result.graph_state.runtime_persistence["native_study_product_loop"]["full_run_datasets"]),
            {"ADAE", "ADCM"},
        )
        for dataset in ("ADAE", "ADCM"):
            full_run_contract = result.graph_state.runtime_persistence["native_study_product_loop"][
                "full_run_datasets"
            ][dataset]
            self.assertTrue(full_run_contract["compatibility_resume_currently_available"])
            self.assertEqual(full_run_contract["compatibility_resume_boundary"], "current_graph_state_review_gate")
            self.assertEqual(
                full_run_contract["compatibility_resume_supported_interrupts"],
                ["draft_spec_review", "code_review"],
            )
        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_study_loop_multi")
        self.assertEqual(progress["study_loop_result"]["source"], "graph_progress")
        self.assertEqual(progress["requested_datasets"], ["ADAE", "ADCM"])
        self.assertEqual(progress["study_loop_result"]["started_datasets"], ["ADAE", "ADCM"])
        self.assertEqual(progress["study_loop_result"]["boundary"], "study_lg3_full_run_dispatch")
        self.assertFalse(progress["study_loop_result"]["native_resume_available"])
        self.assertEqual(progress["study_loop_result"]["native_resume_scope"], "none")
        self.assertEqual(progress["study_loop_result"]["resume_boundary"], "graph_state_projection_only")
        self.assertEqual(
            progress["study_loop_result"]["resume_boundary"],
            progress["native_resume"]["boundary"],
        )
        self.assertEqual(
            progress["study_loop_result"]["native_resume_available"],
            progress["native_resume"]["available"],
        )
        self.assertEqual(
            progress["study_loop_result"]["native_resume_scope"],
            progress["native_resume"]["scope"],
        )
        self.assertEqual(
            progress["study_loop_result"]["native_resume_has_queue_items"],
            progress["native_resume"]["has_queue_items"],
        )
        self.assertEqual(
            progress["study_loop_result"]["native_resume_queue_item_count"],
            progress["native_resume"]["queue_item_count"],
        )
        self.assertEqual(result.native_resume_available, progress["study_loop_result"]["native_resume_available"])
        self.assertEqual(result.native_resume_scope, progress["study_loop_result"]["native_resume_scope"])
        self.assertEqual(result.resume_boundary, progress["study_loop_result"]["resume_boundary"])
        self.assertEqual(result.native_resume_has_queue_items, progress["study_loop_result"]["native_resume_has_queue_items"])
        self.assertEqual(result.native_resume_queue_item_count, progress["study_loop_result"]["native_resume_queue_item_count"])
        self.assertIn("stopped at human review gates", progress["study_loop_result"]["message"])
        self.assertEqual(
            {(item["dataset"], item["name"]) for item in progress["study_loop_result"]["review_queue"]},
            {(item["dataset"], item["name"]) for item in progress["review_queue"]},
        )

    def test_gateway_lg3_resume_updates_study_loop_nested_full_run_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_study_loop_nested_contract_after_resume") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        for dataset, variable, domain in (("adae", "AETERM", "AE"), ("adcm", "CMTRT", "CM")):
            (spec_dir / f"{dataset}.json").write_text(
                json.dumps(
                    {
                        "dataset": dataset.upper(),
                        "variables": [{"variable": variable, "source_domains": [domain]}],
                    }
                ),
                encoding="utf-8",
            )
        gateway = GraphGateway()
        gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg3_nested_contract_resume",
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        result = gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id="run_lg3_nested_contract_resume",
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Approve ADAE but leave ADCM in code review.",
            execute_after_approval=False,
        )

        full_run_datasets = result.graph_state.runtime_persistence["native_study_product_loop"]["full_run_datasets"]
        self.assertEqual(full_run_datasets["ADAE"]["phase"], "reviewed")
        self.assertIsNone(full_run_datasets["ADAE"]["current_interrupt"])
        self.assertFalse(full_run_datasets["ADAE"]["compatibility_resume_currently_available"])
        self.assertEqual(full_run_datasets["ADAE"]["compatibility_resume_boundary"], "historical_contract_only")
        self.assertNotIn("next_action", full_run_datasets["ADAE"])
        self.assertEqual(full_run_datasets["ADCM"]["phase"], "waiting_for_human_gate")
        self.assertEqual(full_run_datasets["ADCM"]["current_interrupt"], "code_review")
        self.assertTrue(full_run_datasets["ADCM"]["compatibility_resume_currently_available"])
        self.assertEqual(result.graph_state.datasets["ADCM"].current_interrupt.name, "code_review")

    def test_gateway_lg3_native_execute_updates_study_loop_nested_full_run_contract(self) -> None:
        study_dir = _workspace_dir("lg3_gateway_study_loop_nested_contract_after_execute") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        for dataset, variable, domain in (("adae", "AETERM", "AE"), ("adcm", "CMTRT", "CM")):
            (spec_dir / f"{dataset}.json").write_text(
                json.dumps(
                    {
                        "dataset": dataset.upper(),
                        "variables": [{"variable": variable, "source_domains": [domain]}],
                    }
                ),
                encoding="utf-8",
            )
        output_artifact = ArtifactRef(
            artifact_id="output_adam_psy201_run_lg3_nested_execute_adae",
            kind="output_adam",
            path="runs/run_lg3_nested_contract_execute/outputs/adae.csv",
            sha256=f"sha256:{'e' * 64}",
            dataset="ADAE",
            format="csv",
            role="output",
        )
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg3_nested_execute_adae",
            kind="validation_report",
            path="runs/run_lg3_nested_contract_execute/validation/adae_validation_report.json",
            sha256=f"sha256:{'f' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        gateway = GraphGateway()
        run_id = "run_lg3_nested_contract_execute"
        gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        gateway.resume_native_dataset_full_run(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="approve",
            reviewer="tester",
            notes="Approve ADAE but leave execution to the explicit native endpoint.",
            execute_after_approval=False,
        )

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass", "errors": [], "warnings": []},
                "output_path": output_artifact.path,
                "validation_report_path": validation_artifact.path,
                "diagnostics_path": "",
                "real_run_artifacts": {"output_adam": output_artifact, "validation_report": validation_artifact},
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [],
                "agent_node_outputs": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            result = gateway.execute_native_dataset_full_run(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        full_run_datasets = result.graph_state.runtime_persistence["native_study_product_loop"]["full_run_datasets"]
        self.assertEqual(full_run_datasets["ADAE"]["phase"], "executed")
        self.assertTrue(full_run_datasets["ADAE"]["approved"])
        self.assertTrue(full_run_datasets["ADAE"]["executed_after_approval"])
        self.assertFalse(full_run_datasets["ADAE"]["terminal_failure"])
        self.assertIsNone(full_run_datasets["ADAE"]["current_interrupt"])
        self.assertFalse(full_run_datasets["ADAE"]["compatibility_resume_currently_available"])
        self.assertEqual(full_run_datasets["ADAE"]["compatibility_resume_boundary"], "historical_contract_only")
        self.assertEqual(full_run_datasets["ADAE"]["last_interrupt"], "code_review")
        self.assertNotIn("next_action", full_run_datasets["ADAE"])
        self.assertEqual(full_run_datasets["ADCM"]["phase"], "waiting_for_human_gate")
        self.assertEqual(full_run_datasets["ADCM"]["current_interrupt"], "code_review")
        self.assertTrue(full_run_datasets["ADCM"]["compatibility_resume_currently_available"])
        self.assertEqual(result.graph_state.datasets["ADCM"].current_interrupt.name, "code_review")

    def test_gateway_native_study_product_loop_preserves_mixed_spec_gates(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_mixed_spec") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (legacy_dir / "adcm.sas").write_text("data adcm; set cm; run;\n", encoding="utf-8")
        gateway = GraphGateway()

        result = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_mixed_spec",
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        self.assertEqual(result.started_datasets, ["ADAE", "ADCM"])
        adae_state = result.graph_state.datasets["ADAE"]
        adcm_state = result.graph_state.datasets["ADCM"]
        self.assertEqual(adae_state.current_interrupt.name, "code_review")
        self.assertEqual(adae_state.code_state["status"], "generated")
        self.assertEqual(adcm_state.current_interrupt.name, "draft_spec_review")
        self.assertEqual(adcm_state.spec_state["status"], "draft_generated")
        self.assertFalse(adcm_state.code_state)
        self.assertIsInstance(result.dataset_results["ADAE"], GraphGatewayNativeDatasetFullRunResult)
        self.assertIsInstance(result.dataset_results["ADCM"], GraphGatewayNativeDatasetFullRunResult)
        self.assertEqual(result.dataset_results["ADAE"].current_interrupt, "code_review")
        self.assertEqual(result.dataset_results["ADCM"].current_interrupt, "draft_spec_review")
        self.assertEqual(result.graph_state.dependency_review_status, "accepted")
        self.assertIsNotNone(result.graph_state.dependency_plan["dependency_planning_warning_records"])
        self.assertEqual(
            result.graph_state.dependency_plan["dependency_planning_warning_records"][0]["code"],
            "input_spec_gap_no_default_dependency",
        )

        reviewed_code = gateway.resume_native_code_review(
            study_dir=study_dir,
            run_id="run_lg2_native_study_loop_mixed_spec",
            dataset="ADAE",
            decision="reject",
            reviewer="tester",
            notes="Code review can proceed after spec-gap warning handoff.",
        )
        self.assertFalse(reviewed_code.approved)

        reviewed_draft = gateway.resume_native_draft_spec_review(
            study_dir=study_dir,
            run_id="run_lg2_native_study_loop_mixed_spec",
            dataset="ADCM",
            decision="reject",
            reviewer="tester",
            notes="Draft review can proceed after spec-gap warning handoff.",
        )
        self.assertFalse(reviewed_draft.approved)

    def test_gateway_native_study_product_loop_restart_preserves_spec_gap_review_resume(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_spec_gap_restart") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (legacy_dir / "adcm.sas").write_text("data adcm; set cm; run;\n", encoding="utf-8")
        gateway = GraphGateway()
        first = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_spec_gap_restart",
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        self.assertEqual(first.graph_state.dependency_review_status, "accepted")

        with patch.object(gateway, "start_native_dataset_full_run") as start_dataset_loop:
            second = gateway.start_native_study_product_loop(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_native_study_loop_spec_gap_restart",
                target_datasets=["ADAE", "ADCM"],
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        start_dataset_loop.assert_not_called()
        self.assertEqual(second.started_datasets, [])
        self.assertEqual(second.graph_state.dependency_review_status, "accepted")
        self.assertNotEqual(second.graph_state.current_interrupt.name, "dependency_review")

        reviewed_code = gateway.resume_native_code_review(
            study_dir=study_dir,
            run_id="run_lg2_native_study_loop_spec_gap_restart",
            dataset="ADAE",
            decision="reject",
            reviewer="tester",
            notes="Code review remains resumable after spec-gap restart.",
        )
        self.assertFalse(reviewed_code.approved)

        reviewed_draft = gateway.resume_native_draft_spec_review(
            study_dir=study_dir,
            run_id="run_lg2_native_study_loop_spec_gap_restart",
            dataset="ADCM",
            decision="reject",
            reviewer="tester",
            notes="Draft review remains resumable after spec-gap restart.",
        )
        self.assertFalse(reviewed_draft.approved)

    def test_gateway_dependency_warning_without_structured_records_fails_closed(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_warning_without_records_fails_closed") / "PSY201"
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
            run_id="run_lg2_warning_without_records_fails_closed",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_warning_without_records_fails_closed",
        ).model_copy(deep=True)
        state.dependency_review_status = "warning"
        state.dependency_plan["dependency_planning_warnings"] = ["Legacy warning without a structured code."]
        state.dependency_plan["dependency_planning_warning_records"] = []
        state.current_interrupt = InterruptState(
            name="dependency_review",
            dataset=None,
            status="open",
            reason="Dependency planning completed with warnings.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_warning_without_records")

        with self.assertRaisesRegex(ValueError, "dependency plan has warnings"):
            gateway.dependency_gate_for_product_step(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_warning_without_records_fails_closed",
                dataset="ADAE",
            )

    def test_gateway_native_study_product_loop_does_not_start_dependency_blocked_targets(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_dependency_block") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "lb.csv").write_text("USUBJID,LBTEST\n01,ALT\n", encoding="utf-8")
        (spec_dir / "adtte.json").write_text(
            json.dumps(
                {
                    "dataset": "ADTTE",
                    "variables": [
                        {
                            "variable": "CNSR",
                            "source_domains": ["ADLB"],
                            "derivation": "Use ADLB threshold records.",
                        }
                    ],
                }
            ),
            encoding="utf-8",
        )
        gateway = GraphGateway()

        result = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_dependency_block",
            target_datasets=["ADTTE"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        self.assertEqual(result.started_datasets, [])
        self.assertEqual(result.dataset_results, {})
        self.assertEqual(result.skipped_datasets, [])
        self.assertEqual(result.graph_state.current_interrupt.name, "dependency_review")
        self.assertEqual(result.graph_state.current_interrupt.dataset, None)
        self.assertTrue(any(block["dataset"] == "ADTTE" for block in result.blocked_datasets))
        loop_metadata = result.graph_state.runtime_persistence["native_study_product_loop"]
        self.assertEqual(loop_metadata["dispatch_status"], "no_dataset_dispatched")
        self.assertEqual(loop_metadata["full_run_datasets"], {})
        self.assertFalse((study_dir / "runs" / "run_lg2_native_study_loop_dependency_block" / "code").exists())

    def test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_restart") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (spec_dir / "adcm.json").write_text(
            json.dumps({"dataset": "ADCM", "variables": [{"variable": "CMTRT", "source_domains": ["CM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        first = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_restart",
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        with patch.object(gateway, "start_native_dataset_full_run") as start_dataset_loop:
            second = gateway.start_native_study_product_loop(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_native_study_loop_restart",
                target_datasets=["ADAE", "ADCM"],
                llm_provider={"provider": "mock", "model": "mock-model"},
                llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
            )

        start_dataset_loop.assert_not_called()
        self.assertEqual(first.started_datasets, ["ADAE", "ADCM"])
        self.assertEqual(second.started_datasets, [])
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in second.skipped_datasets],
            [
                ("ADAE", "existing_graph_progress", "review_code"),
                ("ADCM", "existing_graph_progress", "review_code"),
            ],
        )
        self.assertEqual(second.graph_state.datasets["ADAE"].current_interrupt.name, "code_review")
        self.assertEqual(second.graph_state.datasets["ADCM"].current_interrupt.name, "code_review")
        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_study_loop_restart")
        self.assertEqual(progress["study_loop_result"]["started_datasets"], [])
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in progress["study_loop_result"]["skipped_datasets"]],
            [
                ("ADAE", "existing_graph_progress", "review_code"),
                ("ADCM", "existing_graph_progress", "review_code"),
            ],
        )
        self.assertIn("existing graph progress was preserved for ADAE, ADCM", progress["study_loop_result"]["message"])

    def test_gateway_native_study_product_loop_starts_new_target_while_preserving_existing_progress(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_partial_preserve") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (spec_dir / "adcm.json").write_text(
            json.dumps({"dataset": "ADCM", "variables": [{"variable": "CMTRT", "source_domains": ["CM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()

        first = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_partial_preserve",
            target_datasets=["ADAE"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        second = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_partial_preserve",
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        self.assertEqual(first.started_datasets, ["ADAE"])
        self.assertEqual(second.started_datasets, ["ADCM"])
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in second.skipped_datasets],
            [("ADAE", "existing_graph_progress", "review_code")],
        )
        self.assertEqual(second.graph_state.datasets["ADAE"].current_interrupt.name, "code_review")
        self.assertEqual(second.graph_state.datasets["ADCM"].current_interrupt.name, "code_review")
        progress = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_study_loop_partial_preserve")
        self.assertEqual(progress["study_loop_result"]["started_datasets"], ["ADCM"])
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in progress["study_loop_result"]["skipped_datasets"]],
            [("ADAE", "existing_graph_progress", "review_code")],
        )

    def test_gateway_native_study_product_loop_starts_downstream_after_graph_output_dependency(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_after_dependency_output") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        run_id = "run_lg2_native_study_loop_after_dependency_output"
        run_dir = study_dir / "runs" / run_id
        output_dir = run_dir / "outputs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        output_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "dm.csv").write_text("USUBJID,ARM\n01,Placebo\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        (spec_dir / "adae.json").write_text(
            json.dumps(
                {
                    "dataset": "ADAE",
                    "variables": [
                        {"variable": "USUBJID", "source_domains": ["AE"]},
                        {"variable": "TRTSDT", "source_domains": ["ADSL"]},
                    ],
                }
            ),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        first = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            target_datasets=["ADAE"],
            approved_dependency_datasets=["ADSL"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        self.assertEqual(first.started_datasets, ["ADSL"])
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in first.skipped_datasets],
            [("ADAE", "waiting_for_runtime_dependency_output", "complete_dependency_output")],
        )

        adsl_output = output_dir / "adsl.csv"
        adsl_output.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id).model_copy(deep=True)
        adsl_state = state.datasets["ADSL"]
        adsl_state.status = "completed"
        adsl_state.current_interrupt = None
        adsl_state.code_state["generation_quality"] = {"not_real_derivation": False}
        adsl_state.execution_state.update(
            {
                "status": "completed",
                "terminal_failure": False,
                "partial_output_usable": True,
                "output_path": str(adsl_output.as_posix()),
            }
        )
        adsl_state.validation_summary = {"status": "passed"}
        adsl_state.artifacts.append(
            ArtifactRef(
                artifact_id="output_adam_psy201_run_lg2_native_study_loop_after_dependency_output_adsl",
                kind="output_adam",
                path=str(adsl_output.as_posix()),
                sha256=f"sha256:{sha256_file(adsl_output)}",
                dataset="ADSL",
                format="csv",
                role="output",
            )
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_completed_adsl_dependency_output")

        second = gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            target_datasets=["ADAE"],
            approved_dependency_datasets=["ADSL"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )

        self.assertEqual(second.started_datasets, ["ADAE"])
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in second.skipped_datasets],
            [("ADSL", "existing_graph_progress", "complete")],
        )
        adae_state = second.graph_state.datasets["ADAE"]
        self.assertEqual(adae_state.current_interrupt.name, "code_review")
        self.assertEqual(adae_state.code_state["dependency_artifacts"][0]["required_dataset"], "ADSL")
        self.assertEqual(adae_state.code_state["dependency_artifacts"][0]["artifact_source"], "run_output")
        self.assertEqual(adae_state.code_state["dependency_artifacts"][0]["artifact_path"], str(adsl_output.as_posix()))
        self.assertNotIn("ADAE", {block["dataset"] for block in second.blocked_datasets})
        progress = gateway.progress_summary(study_dir=study_dir, run_id=run_id)
        self.assertEqual(progress["study_loop_result"]["started_datasets"], ["ADAE"])
        self.assertEqual(progress["study_loop_result"]["skipped_datasets"][0]["dataset"], "ADSL")

    def test_gateway_native_study_loop_dependency_output_gate_requires_run_output_artifact_hash(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_dependency_hash_gate") / "PSY201"
        output_dir = study_dir / "runs" / "run_lg2_native_study_loop_dependency_hash_gate" / "outputs"
        output_dir.mkdir(parents=True)
        adsl_output = output_dir / "adsl.csv"
        adsl_output.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        output_sha = f"sha256:{sha256_file(adsl_output)}"
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_dependency_hash_gate",
            status="pending",
            requested_datasets=["ADAE"],
            target_datasets=["ADSL", "ADAE"],
            runnable_datasets=["ADSL", "ADAE"],
            dependency_plan={
                "dataset_dependencies": {"ADSL": [], "ADAE": ["ADSL"]},
                "execution_batches": [["ADSL"], ["ADAE"]],
            },
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "available": True,
                    "artifact_path": str(adsl_output.as_posix()),
                    "artifact_sha256": None,
                    "artifact_source": "run_output",
                    "resolution_status": "available",
                    "allowed_actions": [],
                    "selected_action": "use_existing_dataset",
                    "reason": "Malformed available record without a hash should fail closed.",
                }
            ],
            datasets={
                "ADSL": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_native_study_loop_dependency_hash_gate",
                    dataset="ADSL",
                    status="completed",
                    execution_state={
                        "status": "completed",
                        "terminal_failure": False,
                        "partial_output_usable": True,
                        "output_path": str(adsl_output.as_posix()),
                    },
                    artifacts=[
                        ArtifactRef(
                            artifact_id="output_adam_psy201_run_lg2_native_study_loop_dependency_hash_gate_adsl",
                            kind="output_adam",
                            path=str(adsl_output.as_posix()),
                            sha256=output_sha,
                            dataset="ADSL",
                            format="csv",
                            role="output",
                        )
                    ],
                ),
                "ADAE": DatasetRunState(study_id="PSY201", run_id="run_lg2_native_study_loop_dependency_hash_gate", dataset="ADAE"),
            },
        )
        run_dir = study_dir / "runs" / "run_lg2_native_study_loop_dependency_hash_gate"

        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=run_dir))
        state.dependency_resolution[0]["artifact_sha256"] = output_sha
        self.assertTrue(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=run_dir))
        state.dependency_resolution[0]["artifact_source"] = "reference_adam"
        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=run_dir))
        state.dependency_resolution[0]["artifact_source"] = "run_output"
        state.datasets["ADSL"].status = "completed_stub"
        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=run_dir))
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].execution_state["terminal_failure"] = True
        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=run_dir))
        state.datasets["ADSL"].execution_state["terminal_failure"] = False
        state.datasets["ADSL"].artifacts = []
        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=run_dir))

    def test_gateway_native_study_loop_dependency_output_gate_treats_run_relative_and_absolute_paths_as_same(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_dependency_path_equivalence") / "PSY201"
        run_id = "run_lg2_native_study_loop_dependency_path_equivalence"
        output_dir = study_dir / "runs" / run_id / "outputs"
        output_dir.mkdir(parents=True)
        adsl_output = output_dir / "adsl.csv"
        adsl_output.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        output_sha = f"sha256:{sha256_file(adsl_output)}"
        state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="pending",
            requested_datasets=["ADAE"],
            target_datasets=["ADSL", "ADAE"],
            runnable_datasets=["ADSL", "ADAE"],
            dependency_plan={
                "dataset_dependencies": {"ADSL": [], "ADAE": ["ADSL"]},
                "execution_batches": [["ADSL"], ["ADAE"]],
            },
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "available": True,
                    "artifact_path": "outputs/adsl.csv",
                    "artifact_sha256": output_sha,
                    "artifact_source": "run_output",
                    "resolution_status": "available",
                    "allowed_actions": [],
                    "selected_action": "use_existing_dataset",
                    "reason": "Run-relative dependency path should match graph-owned absolute artifact path.",
                }
            ],
            datasets={
                "ADSL": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADSL",
                    status="completed",
                    execution_state={
                        "status": "completed",
                        "terminal_failure": False,
                        "partial_output_usable": True,
                        "output_path": str(adsl_output.as_posix()),
                    },
                    artifacts=[
                        ArtifactRef(
                            artifact_id="output_adam_psy201_run_lg2_native_study_loop_dependency_path_equivalence_adsl",
                            kind="output_adam",
                            path=str(adsl_output.as_posix()),
                            sha256=output_sha,
                            dataset="ADSL",
                            format="csv",
                            role="output",
                        )
                    ],
                ),
                "ADAE": DatasetRunState(study_id="PSY201", run_id=run_id, dataset="ADAE"),
            },
        )

        self.assertTrue(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=study_dir / "runs" / run_id))
        state.dependency_resolution[0]["artifact_path"] = "outputs/wrong_adsl.csv"
        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=study_dir / "runs" / run_id))

    def test_gateway_native_study_loop_dependency_output_gate_rejects_paths_outside_graph_run_dir(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_dependency_outside_path") / "PSY201"
        run_id = "run_lg2_native_study_loop_dependency_outside_path"
        good_output = study_dir / "runs" / run_id / "outputs" / "adsl.csv"
        decoy_output = study_dir.parent / "decoy" / "runs" / run_id / "outputs" / "adsl.csv"
        good_output.parent.mkdir(parents=True)
        decoy_output.parent.mkdir(parents=True)
        good_output.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        decoy_output.write_text("USUBJID,TRTSDT\n99,2099-01-01\n", encoding="utf-8")
        good_sha = f"sha256:{sha256_file(good_output)}"
        decoy_sha = f"sha256:{sha256_file(decoy_output)}"
        state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="pending",
            requested_datasets=["ADAE"],
            target_datasets=["ADSL", "ADAE"],
            runnable_datasets=["ADSL", "ADAE"],
            dependency_plan={
                "dataset_dependencies": {"ADSL": [], "ADAE": ["ADSL"]},
                "execution_batches": [["ADSL"], ["ADAE"]],
            },
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "available": True,
                    "artifact_path": str(decoy_output.as_posix()),
                    "artifact_sha256": decoy_sha,
                    "artifact_source": "run_output",
                    "resolution_status": "available",
                    "allowed_actions": [],
                    "selected_action": "use_existing_dataset",
                    "reason": "Dependency record points outside the graph-owned run directory.",
                }
            ],
            datasets={
                "ADSL": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADSL",
                    status="completed",
                    execution_state={
                        "status": "completed",
                        "terminal_failure": False,
                        "partial_output_usable": True,
                        "output_path": str(good_output.as_posix()),
                    },
                    artifacts=[
                        ArtifactRef(
                            artifact_id="output_adam_psy201_run_lg2_native_study_loop_dependency_outside_path_adsl",
                            kind="output_adam",
                            path=str(good_output.as_posix()),
                            sha256=good_sha,
                            dataset="ADSL",
                            format="csv",
                            role="output",
                        )
                    ],
                ),
                "ADAE": DatasetRunState(study_id="PSY201", run_id=run_id, dataset="ADAE"),
            },
        )

        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=study_dir / "runs" / run_id))
        state.dependency_resolution[0]["artifact_path"] = "../decoy/runs/run_lg2_native_study_loop_dependency_outside_path/outputs/adsl.csv"
        self.assertFalse(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=study_dir / "runs" / run_id))
        state.dependency_resolution[0]["artifact_path"] = str(good_output.as_posix())
        state.dependency_resolution[0]["artifact_sha256"] = good_sha
        self.assertTrue(_native_study_loop_dependency_outputs_available(state, "ADAE", run_dir=study_dir / "runs" / run_id))

    def test_resolve_run_artifact_path_normalizes_equivalent_run_scoped_paths(self) -> None:
        run_dir = _workspace_dir("lg2_resolve_run_artifact_path") / "PSY201" / "runs" / "run_path_matrix"
        output_path = run_dir / "outputs" / "adsl.csv"
        output_path.parent.mkdir(parents=True)
        output_path.write_text("USUBJID\n01\n", encoding="utf-8")

        equivalent_inputs = [
            "outputs/adsl.csv",
            "outputs\\adsl.csv",
            "runs/run_path_matrix/outputs/adsl.csv",
            "runs\\run_path_matrix\\outputs\\adsl.csv",
            str(output_path.as_posix()),
        ]

        for artifact_path in equivalent_inputs:
            with self.subTest(artifact_path=artifact_path):
                resolved = _resolve_run_artifact_path(run_dir, artifact_path)
                self.assertIsNotNone(resolved)
                self.assertEqual(
                    resolved.resolve(strict=False).as_posix(),
                    output_path.resolve(strict=False).as_posix(),
                )

    def test_resolve_run_artifact_path_rejects_empty_or_outside_run_paths(self) -> None:
        study_root = _workspace_dir("lg2_resolve_run_artifact_path_escape") / "PSY201"
        run_dir = study_root / "runs" / "run_path_escape"
        outside_path = study_root / "outside" / "adsl.csv"
        outside_path.parent.mkdir(parents=True)
        outside_path.write_text("USUBJID\n99\n", encoding="utf-8")

        rejected_inputs = [
            "",
            "   ",
            "../outside/adsl.csv",
            "outputs/../../outside/adsl.csv",
            "prefix/runs/run_path_escape/outputs/adsl.csv",
            "runs/run_other/outputs/adsl.csv",
            str(outside_path.as_posix()),
        ]

        for artifact_path in rejected_inputs:
            with self.subTest(artifact_path=artifact_path):
                self.assertIsNone(_resolve_run_artifact_path(run_dir, artifact_path))

    def test_gateway_progress_hides_study_loop_result_after_inputs_change(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_study_loop_stale_progress") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        ae_path = sdtm_dir / "ae.csv"
        ae_path.write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (spec_dir / "adcm.json").write_text(
            json.dumps({"dataset": "ADCM", "variables": [{"variable": "CMTRT", "source_domains": ["CM"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_native_study_product_loop(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_native_study_loop_stale",
            target_datasets=["ADAE", "ADCM"],
            llm_provider={"provider": "mock", "model": "mock-model"},
            llm_exposure={"mode": "metadata_only", "data_classification": "unknown"},
        )
        before = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_study_loop_stale")
        self.assertEqual(before["study_loop_result"]["source"], "graph_progress")
        self.assertEqual(before["study_loop_result"]["started_datasets"], ["ADAE", "ADCM"])
        ae_path.write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        gateway.mark_inputs_changed(study_dir=study_dir, run_id="run_lg2_native_study_loop_stale")
        after = gateway.progress_summary(study_dir=study_dir, run_id="run_lg2_native_study_loop_stale")

        self.assertTrue(after["plan_stale"])
        self.assertEqual(after["dependency_review_status"], "stale")
        self.assertEqual(after["study_loop_result"], {})

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

    def test_gateway_native_terminal_failure_review_roundtrip_persists_triage(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_terminal_failure_roundtrip") / "PSY201"
        run_id = "run_lg2_native_terminal_failure_roundtrip"
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
        validation_artifact = ArtifactRef(
            artifact_id="validation_report_psy201_run_lg2_native_terminal_failure_roundtrip_adae",
            kind="validation_report",
            path="runs/run_lg2_native_terminal_failure_roundtrip/validation/adae_validation_report.json",
            sha256=f"sha256:{'5' * 64}",
            dataset="ADAE",
            format="json",
            role="output",
        )
        failure = FailureRecord(
            failure_id="failure_adae_native_terminal",
            dataset="ADAE",
            node="execute_approved_code",
            failure_type="sandbox_error",
            message="R execution failed.",
            root_cause="r_runtime_error",
            recommended_route="repair_code",
        )
        fake_result = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path="runs/run_lg2_native_terminal_failure_roundtrip/validation/adae_validation_report.json",
            diagnostics_path="runs/run_lg2_native_terminal_failure_roundtrip/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail", "errors": ["R execution failed"], "warnings": []},
            failure_records=[failure],
            artifacts={"validation_report": validation_artifact},
        )
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
                notes="Approved failing native terminal pilot.",
            ),
            review_path=review_path,
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )

        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_result):
            started = gateway.start_native_terminal_failure_review(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        self.assertTrue(started.terminal_failure)
        self.assertEqual(started.graph_state.datasets["ADAE"].current_interrupt.name, "terminal_failure")
        self.assertIn("native_terminal_failure_review_interrupt", started.graph_state.runtime_persistence)
        self.assertEqual(
            started.graph_state.runtime_persistence["native_terminal_failure_review_interrupt"]["boundary"],
            "terminal_failure_review_pilot_only",
        )
        before_review_progress = gateway.progress_summary(study_dir=study_dir, run_id=run_id)
        self.assertEqual(
            [
                (item["dataset"], item["interrupt"], item["can_resume"])
                for item in before_review_progress["native_resume"]["interrupt_queue"]
            ],
            [("ADAE", "terminal_failure", False)],
        )

        reviewed = gateway.resume_native_terminal_failure_review(
            study_dir=study_dir,
            run_id=run_id,
            dataset="ADAE",
            decision="repair_code",
            reviewer="native_tester",
            notes="Repair after native terminal-failure review.",
        )

        dataset_state = reviewed.graph_state.datasets["ADAE"]
        self.assertEqual(reviewed.decision, "repair_code")
        self.assertEqual(reviewed.current_interrupt, "terminal_failure")
        self.assertEqual(reviewed.next_action, "repair_generated_code")
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.execution_state["terminal_failure_review"]["action"], "repair_code")
        self.assertEqual(dataset_state.human_commands[-1].reviewer, "native_tester")
        self.assertEqual(
            reviewed.graph_state.runtime_persistence["native_terminal_failure_review_resume"]["native_status"],
            "triaged",
        )
        after_review_progress = gateway.progress_summary(study_dir=study_dir, run_id=run_id)
        self.assertEqual(after_review_progress["native_resume"]["interrupt_queue"], [])
        persisted_state = json.loads((run_dir / "graph_state.json").read_text(encoding="utf-8"))
        self.assertEqual(
            persisted_state["datasets"]["ADAE"]["execution_state"]["terminal_failure_review"]["action"],
            "repair_code",
        )

    def test_gateway_native_terminal_failure_review_resume_requires_canonical_terminal_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_terminal_failure_resume_gate") / "PSY201"
        run_id = "run_lg2_native_terminal_failure_resume_gate"
        code_dir = study_dir / "runs" / run_id / "code"
        review_dir = study_dir / "runs" / run_id / "review"
        code_dir.mkdir(parents=True)
        review_dir.mkdir()
        code_path = code_dir / "build_adae.R"
        review_path = review_dir / "adae_code_review.json"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, run_id, "ADAE", code_path)
        code_sha = f"sha256:{sha256_file(code_path)}"
        fake_result = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path=None,
            diagnostics_path="runs/run_lg2_native_terminal_failure_resume_gate/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail"},
            failure_records=[],
            artifacts={},
        )
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
            ),
            review_path=review_path,
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_result):
            gateway.start_native_terminal_failure_review(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        graph_state.datasets["ADAE"].current_interrupt = InterruptState(
            name="code_review",
            dataset="ADAE",
            reason="Canonical state changed before native resume.",
        )
        gateway._persist_graph_state(study_dir, graph_state, node="test_native_terminal_failure_resume_gate")

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            with self.assertRaisesRegex(ValueError, "does not match any current open graph interrupt"):
                gateway.resume_native_terminal_failure_review(
                    study_dir=study_dir,
                    run_id=run_id,
                    dataset="ADAE",
                    decision="repair_code",
                    reviewer="native_tester",
                )

        compile_graph.assert_not_called()
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        self.assertNotIn("terminal_failure_review", reloaded.datasets["ADAE"].execution_state)
        self.assertEqual(reloaded.datasets["ADAE"].current_interrupt.name, "code_review")

    def test_gateway_native_terminal_failure_review_resume_rejects_study_interrupt_before_native_resume(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_terminal_failure_study_gate") / "PSY201"
        run_id = "run_lg2_native_terminal_failure_study_gate"
        code_dir = study_dir / "runs" / run_id / "code"
        review_dir = study_dir / "runs" / run_id / "review"
        code_dir.mkdir(parents=True)
        review_dir.mkdir()
        code_path = code_dir / "build_adae.R"
        review_path = review_dir / "adae_code_review.json"
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, run_id, "ADAE", code_path)
        code_sha = f"sha256:{sha256_file(code_path)}"
        fake_result = SimpleNamespace(
            terminal_failure=True,
            response_status="terminal_failure",
            validation_status="fail",
            output_path=None,
            validation_report_path=None,
            diagnostics_path="runs/run_lg2_native_terminal_failure_study_gate/diagnostics/adae_failure_report.json",
            errors=["R execution failed"],
            warnings=[],
            validation_report={"status": "fail"},
            failure_records=[],
            artifacts={},
        )
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
            ),
            review_path=review_path,
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code", return_value=fake_result):
            gateway.start_native_terminal_failure_review(
                study_dir=study_dir,
                study_id="PSY201",
                run_id=run_id,
                dataset="ADAE",
            )

        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        graph_state.current_interrupt = InterruptState(
            name="dependency_review",
            reason="Study dependency review must be resolved first.",
        )
        gateway._persist_graph_state(study_dir, graph_state, node="test_native_terminal_failure_study_gate")

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            with self.assertRaisesRegex(ValueError, "Study-level interrupt dependency_review must be resolved"):
                gateway.resume_native_terminal_failure_review(
                    study_dir=study_dir,
                    run_id=run_id,
                    dataset="ADAE",
                    decision="repair_code",
                    reviewer="native_tester",
                )

        compile_graph.assert_not_called()
        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        self.assertEqual(reloaded.current_interrupt.name, "dependency_review")
        self.assertNotIn("terminal_failure_review", reloaded.datasets["ADAE"].execution_state)

    def test_gateway_native_terminal_failure_review_start_fails_closed_without_interrupt(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_native_terminal_failure_no_interrupt") / "PSY201"
        run_id = "run_lg2_native_terminal_failure_no_interrupt"
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
            ),
            review_path=review_path,
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        fake_result = {
            "status": "completed",
            "response_status": "completed",
            "terminal_failure": False,
            "current_interrupt": None,
        }

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            graph = compile_graph.return_value
            graph.invoke.return_value = fake_result
            with self.assertRaisesRegex(ValueError, "did not stop at native terminal_failure"):
                gateway.start_native_terminal_failure_review(
                    study_dir=study_dir,
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                )
            graph.get_state.assert_not_called()

        reloaded = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
        dataset_state = reloaded.datasets["ADAE"]
        self.assertEqual(dataset_state.status, "pending")
        self.assertIsNone(dataset_state.current_interrupt)
        self.assertNotEqual(dataset_state.execution_state.get("status"), "terminal_failure")
        self.assertNotIn("terminal_failure_review", dataset_state.execution_state)
        self.assertNotIn("native_terminal_failure_review_interrupt", reloaded.runtime_persistence)

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
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_lg2_compare_entrypoint").model_copy(deep=True)
        state.datasets["ADAE"].status = "completed"
        state.datasets["ADAE"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": str((output_dir / "adae.csv").as_posix()),
        }
        state.datasets["ADAE"].artifacts.append(
            ArtifactRef(
                artifact_id="output_adam_psy201_run_lg2_compare_entrypoint_adae",
                kind="output_adam",
                path=str((output_dir / "adae.csv").as_posix()),
                sha256=f"sha256:{sha256_file(output_dir / 'adae.csv')}",
                dataset="ADAE",
                format="csv",
                role="output",
            )
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_compare_output_artifact")

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

    def test_gateway_compare_reference_output_ignores_unrecorded_stale_output_file(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_compare_ignores_stale_output") / "PSY201"
        output_dir = study_dir / "runs" / "run_lg2_compare_stale_output" / "outputs"
        validation_dir = study_dir / "runs" / "run_lg2_compare_stale_output" / "validation"
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
            run_id="run_lg2_compare_stale_output",
            target_datasets=["ADAE"],
        )

        result = gateway.compare_reference_output(
            study_dir=study_dir,
            run_id="run_lg2_compare_stale_output",
            dataset="ADAE",
        )

        dataset_state = result.graph_state.datasets["ADAE"]
        self.assertEqual(result.compare_summary["status"], "missing_generated")
        self.assertEqual(dataset_state.compare_summary["status"], "missing_generated")
        self.assertNotIn("generated_file", dataset_state.compare_summary)
        self.assertFalse(any(artifact.kind == "output_adam" for artifact in dataset_state.artifacts))
        self.assertFalse(dataset_state.execution_state.get("output_path"))

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
        self.assertNotIn("native_dataset_full_run", result.graph_state.runtime_persistence)

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
        self.assertEqual(by_dataset["ADAE"]["available_actions"], [])
        self.assertEqual(by_dataset["ADCM"]["next_action"], "blocked")
        self.assertIn("Study-level dependency review", by_dataset["ADCM"]["blocked_reason"])
        review_items = {(item["scope"], item["dataset"], item["name"], item["source"]) for item in progress["review_queue"]}
        self.assertIn(("study", "", "dependency_review", "interrupt"), review_items)
        self.assertIn(("dataset", "ADAE", "code_review", "interrupt"), review_items)
        self.assertIn("Review dependency plan.", {item["action_label"] for item in progress["review_queue"]})
        self.assertTrue(Path(progress["graph_state_path"]).exists())
        self.assertTrue(Path(progress["workflow_state_path"]).exists())

    def test_gateway_progress_summary_exposes_review_gate_actions_when_unblocked(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress_review_gate_actions") / "PSY201"
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
            run_id="run_lg2_progress_review_gate_actions",
            target_datasets=["ADAE"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_progress_review_gate_actions",
        ).model_copy(deep=True)
        state.current_interrupt = None
        state.dependency_review_status = "accepted"
        gateway._persist_graph_state(study_dir, state, node="test_accept_dependency_for_review_actions")
        code_path = study_dir / "runs" / "run_lg2_progress_review_gate_actions" / "code" / "build_adae.R"
        code_path.parent.mkdir(parents=True)
        code_path.write_text("write.csv(data.frame(ID='01'), 'outputs/adae.csv', row.names=FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_lg2_progress_review_gate_actions",
            "ADAE",
            code_path,
        )
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_review_gate_actions",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
        )

        progress = gateway.progress_summary(
            study_dir=study_dir,
            run_id="run_lg2_progress_review_gate_actions",
        )

        adae_progress = {item["dataset"]: item for item in progress["datasets"]}["ADAE"]
        self.assertEqual(adae_progress["next_action"], "review_code")
        self.assertEqual(
            adae_progress["available_actions"],
            [
                {"action": "approve", "label": "Approve Code"},
                {"action": "reject", "label": "Reject Code"},
            ],
        )
        queue_by_name = {(item["dataset"], item["name"]): item for item in progress["review_queue"]}
        self.assertEqual(
            queue_by_name[("ADAE", "code_review")]["available_actions"],
            [
                {"action": "approve", "label": "Approve Code"},
                {"action": "reject", "label": "Reject Code"},
            ],
        )

    def test_gateway_progress_summary_exposes_draft_spec_review_actions(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_progress_draft_spec_actions") / "PSY201"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_progress_draft_spec_actions",
            target_datasets=["ADCM"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_lg2_progress_draft_spec_actions",
        ).model_copy(deep=True)
        state.current_interrupt = None
        state.dependency_review_status = "accepted"
        state.datasets["ADCM"].status = "needs_review"
        state.datasets["ADCM"].spec_state = {"status": "draft_generated"}
        state.datasets["ADCM"].current_interrupt = InterruptState(
            name="draft_spec_review",
            dataset="ADCM",
            reason="Review the generated draft spec.",
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_draft_spec_review_actions")

        progress = gateway.progress_summary(
            study_dir=study_dir,
            run_id="run_lg2_progress_draft_spec_actions",
        )

        adcm_progress = {item["dataset"]: item for item in progress["datasets"]}["ADCM"]
        self.assertEqual(adcm_progress["next_action"], "review_draft_spec")
        self.assertEqual(
            adcm_progress["available_actions"],
            [
                {"action": "approve", "label": "Approve Draft Spec"},
                {"action": "reject", "label": "Reject Draft Spec"},
            ],
        )
        queue_by_name = {(item["dataset"], item["name"]): item for item in progress["review_queue"]}
        self.assertEqual(
            queue_by_name[("ADCM", "draft_spec_review")]["available_actions"],
            [
                {"action": "approve", "label": "Approve Draft Spec"},
                {"action": "reject", "label": "Reject Draft Spec"},
            ],
        )

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
