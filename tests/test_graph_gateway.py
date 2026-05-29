"""Tests for the LangGraph-2 gateway and workflow projection."""

from __future__ import annotations

import json
import sqlite3
import sys
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.api.models import RunPlanRequest
    from adam_agent.api.service import prepare_run_plan
    from adam_agent.graph.gateway import GraphGateway
    from adam_agent.graph.workflow_state import input_fingerprint, workflow_projection_consistency
    from adam_agent.schemas.graph_state import DatasetRunState, HumanCommand, InterruptState, StudyRunState
    from adam_agent.tools.artifacts import sha256_file
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.api.models import RunPlanRequest
    from adam_agent.api.service import prepare_run_plan
    from adam_agent.graph.gateway import GraphGateway
    from adam_agent.graph.workflow_state import input_fingerprint, workflow_projection_consistency
    from adam_agent.schemas.graph_state import DatasetRunState, HumanCommand, InterruptState, StudyRunState
    from adam_agent.tools.artifacts import sha256_file


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


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
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertEqual(workflow_state["current_interrupt"], "dependency_review")
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
        static_path = static_dir / "adae_static_check.json"
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path.write_text(json.dumps({"status": "warning_only"}), encoding="utf-8")
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        code_sha = f"sha256:{sha256_file(code_path)}"
        static_sha = f"sha256:{sha256_file(static_path)}"
        gateway = GraphGateway()
        gateway.record_code_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_code_review",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=code_sha,
            static_check_path=static_path,
            static_check_sha256=static_sha,
        )

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
        self.assertEqual(generated.workflow_projection["datasets"]["ADAE"]["spec_state"]["status"], "draft_generated")
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
        static_path = static_dir / "adae_static_check.json"
        spec_path = spec_dir / "adae.json"
        code_path.write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        static_path.write_text(json.dumps({"status": "warning_only"}), encoding="utf-8")
        spec_path.write_text(json.dumps({"dataset": "ADAE", "variables": []}), encoding="utf-8")
        review_path.write_text(json.dumps({"decision": "approve", "approved": True}), encoding="utf-8")
        code_sha = f"sha256:{sha256_file(code_path)}"
        static_sha = f"sha256:{sha256_file(static_path)}"
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

        with self.assertRaisesRegex(ValueError, "recorded in graph state"):
            GraphGateway().record_code_generation(
                study_dir=study_dir,
                study_id="PSY201",
                run_id="run_lg2_codegen_requires_graph_draft",
                dataset="ADAE",
                code_path=code_path,
                code_sha256=f"sha256:{sha256_file(code_path)}",
                spec_source="approved_draft_spec",
                spec_path=approved_path,
                spec_sha256=f"sha256:{sha256_file(approved_path)}",
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


if __name__ == "__main__":
    unittest.main()
