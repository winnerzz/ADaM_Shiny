"""Tests for the LangGraph-2 gateway and workflow projection."""

from __future__ import annotations

import json
import inspect
import sqlite3
import sys
import unittest
import uuid
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.api.models import RunPlanRequest
    from adam_agent.api.service import prepare_run_plan
    from adam_agent.graph.gateway import GraphGateway
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
    from adam_agent.graph.gateway import GraphGateway
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
        self.assertEqual(result.graph_state.agent_audit_summary["summary_writer"]["agent"], "audit_agent")
        self.assertEqual(result.graph_state.agent_audit_summary["agent_counts"]["dependency_agent"], 1)
        self.assertTrue((study_dir / "runs" / "run_lg2_gateway_plan" / "audit" / "agent_summary.json").exists())
        self.assertEqual(workflow_state["projection_source"], "langgraph")
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
        workflow_state = json.loads(workflow_path.read_text(encoding="utf-8"))

        self.assertEqual(response.dependency_review_status, "review_required")
        self.assertEqual(response.target_datasets, ["ADAE"])
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
        self.assertEqual(generated.graph_state.agent_decisions[-1]["agent"], "spec_agent")
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
        gateway.record_input_spec_ready(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_mark_inputs_changed",
            dataset="ADAE",
            input_spec_path=spec_dir / "adae.json",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
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
        self.assertEqual(result.graph_state.agent_decisions[-1]["agent"], "execution_agent")
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
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["current_interrupt"], "code_review")

    def test_gateway_generate_code_uses_gateway_owned_dependency_plan(self) -> None:
        study_dir = _workspace_dir("lg2_gateway_owned_dependency_plan") / "PSY201"
        run_id = "run_lg2_gateway_owned_dependency_plan"
        run_dir = study_dir / "runs" / run_id
        output_dir = run_dir / "outputs"
        code_dir = run_dir / "code"
        spec_dir = study_dir / "input_spec"
        output_dir.mkdir(parents=True)
        code_dir.mkdir()
        spec_dir.mkdir(parents=True)
        adsl_path = output_dir / "adsl.csv"
        adsl_path.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
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
        self.assertIn("execution_agent", [item["agent"] for item in dataset_state.agent_decisions])
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["status"], "completed")

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
        self.assertEqual(dataset_state.compare_summary["status"], "missing_reference")
        self.assertEqual(dataset_state.result_summary.compare_status, "missing_reference")
        self.assertIn("compare_report_adae", [artifact.artifact_id for artifact in dataset_state.artifacts])
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["compare_summary"]["status"], "missing_reference")

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
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "terminal_failure")
        self.assertEqual(dataset_state.execution_state["next_action"], "repair_generated_code")
        self.assertEqual(dataset_state.execution_state["terminal_failure_review"]["action"], "repair_code")
        self.assertEqual(dataset_state.human_commands[-1].action, "repair_code")
        self.assertEqual(dataset_state.result_summary.metadata["terminal_failure_next_action"], "repair_generated_code")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["current_interrupt"], "terminal_failure")

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


if __name__ == "__main__":
    unittest.main()
