"""Tests for the Phase 8.1 FastAPI backend boundary."""

from __future__ import annotations

import csv
import json
import sys
import unittest
import uuid
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app
    from adam_agent.graph.workflow_state import input_fingerprint
    from adam_agent.tools.artifacts import sha256_file
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app
    from adam_agent.graph.workflow_state import input_fingerprint
    from adam_agent.tools.artifacts import sha256_file


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class Phase8ApiTests(unittest.TestCase):
    def test_health_endpoint(self) -> None:
        client = TestClient(create_app())

        response = client.get("/health")

        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json(), {"status": "ok"})

    def test_index_serves_local_web_ui(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        self.assertIn("text/html", response.headers["content-type"])
        self.assertIn("ADaM Agent Studio", response.text)
        self.assertIn("Study Dashboard", response.text)
        self.assertIn("Dependency Map", response.text)
        self.assertIn("Dataset Execution Cards", response.text)
        self.assertIn("operationBanner", response.text)
        self.assertIn("globalStatusDetail", response.text)
        self.assertIn("generation plan", response.text)
        self.assertIn("nextActionText", response.text)
        self.assertIn("Try With Shiny Demo Data", response.text)
        self.assertIn("Use My Study Files", response.text)
        self.assertIn("Generate R Code", response.text)
        self.assertIn("Approve And Run Locally", response.text)
        self.assertIn("finalize-inputs", response.text)
        self.assertIn("finalizedInputsByDataset", response.text)
        self.assertIn("Audit Timeline", response.text)
        self.assertIn("Advanced settings and audit files", response.text)
        self.assertIn("Upload Define", response.text)
        self.assertIn("Upload Legacy Code", response.text)
        self.assertIn("Add another ADaM target", response.text)
        self.assertIn("addTargetButton", response.text)
        self.assertIn("Real LLM API", response.text)
        self.assertIn("testLlmButton", response.text)
        self.assertIn("timeout_seconds: 300", response.text)
        self.assertIn("generatedByDataset", response.text)
        self.assertIn("reviewByDataset", response.text)
        self.assertIn("executionByDataset", response.text)
        self.assertIn("data-card-target", response.text)
        self.assertIn("resetActiveDatasetView", response.text)
        self.assertNotIn("resetGeneratedState", response.text)
        self.assertNotIn("Create / Open Study", response.text)
        self.assertNotIn("Run Approved Code In Sandbox", response.text)

    def test_product_workspace_endpoint_creates_hidden_default_workspace(self) -> None:
        client = TestClient(create_app())

        response = client.post("/product-workspace")

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertTrue(payload["study_id"].startswith("study_"))
        self.assertTrue(payload["run_id"].startswith("run_"))
        self.assertTrue((Path(payload["study_dir"]) / "input_sdtm").is_dir())
        self.assertIn("input_summary", payload)

    def test_workspace_endpoint_creates_canonical_folders(self) -> None:
        study_dir = _workspace_dir("phase8_workspace_endpoint") / "MY_STUDY"
        client = TestClient(create_app())

        response = client.post(
            "/studies/workspace",
            json={"study_dir": str(study_dir), "study_id": "MY_STUDY"},
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["study_id"], "MY_STUDY")
        for folder in ["input_sdtm", "input_spec", "input_define", "reference_adam", "legacy_code", "runs"]:
            self.assertTrue((study_dir / folder).is_dir())

    def test_upload_endpoint_saves_files_by_role_and_rescans(self) -> None:
        study_dir = _workspace_dir("phase8_upload_endpoint") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})

        response = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "sdtm"},
            files=[("files", ("dm.csv", b"USUBJID\n01\n", "text/csv"))],
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["role"], "sdtm")
        self.assertTrue((study_dir / "input_sdtm" / "dm.csv").exists())
        self.assertEqual(payload["input_summary"]["sdtm"][0]["dataset"], "DM")
        self.assertIn("digest", payload["input_fingerprint"])
        self.assertTrue(payload["input_diff"]["changed"])
        self.assertIn("input_sdtm/dm.csv", payload["input_diff"]["added"])

        define_response = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "define"},
            files=[("files", ("define.xml", b"<ODM></ODM>\n", "application/xml"))],
        )
        self.assertEqual(define_response.status_code, 200, define_response.text)
        define_payload = define_response.json()
        self.assertTrue((study_dir / "input_define" / "define.xml").exists())
        self.assertEqual(define_payload["input_summary"]["define"][0]["file_name"], "define.xml")

        legacy_response = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "legacy"},
            files=[("files", ("build_adlb.sas", b"data adlb; run;\n", "text/plain"))],
        )
        self.assertEqual(legacy_response.status_code, 200, legacy_response.text)
        legacy_payload = legacy_response.json()
        self.assertTrue((study_dir / "legacy_code" / "build_adlb.sas").exists())
        self.assertEqual(legacy_payload["input_summary"]["legacy_code"][0]["file_name"], "build_adlb.sas")
        self.assertEqual(legacy_payload["input_summary"]["legacy_code"][0]["status"], "ok")
        self.assertEqual(legacy_payload["input_summary"]["legacy_code"][0]["preview_type"], "code")
        self.assertIn("data adlb", legacy_payload["input_summary"]["legacy_code"][0]["text_preview"].lower())

    def test_upload_marks_existing_workflow_state_stale(self) -> None:
        study_dir = _workspace_dir("phase8_upload_stales_workflow") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (study_dir / "input_spec" / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM"}]}),
            encoding="utf-8",
        )

        plan = client.post(
            "/runs/prepare",
            json={"study_dir": str(study_dir), "run_id": "run_stale", "target_datasets": ["ADAE"]},
        )
        self.assertEqual(plan.status_code, 200, plan.text)

        upload = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "legacy"},
            files=[("files", ("build_adae.sas", b"data adae; set ae; run;\n", "text/plain"))],
        )

        self.assertEqual(upload.status_code, 200, upload.text)
        self.assertIn("run_stale", upload.json()["touched_runs"])
        state = json.loads((study_dir / "runs" / "run_stale" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertTrue(state["plan_stale"])
        self.assertTrue(state["input_diff"]["changed"])
        self.assertIn("ADAE", state["stale_datasets"])

    def test_demo_study_endpoint_prepares_shiny_demo_shape(self) -> None:
        source = _demo_source("phase8_api_demo_source")
        target = _workspace_dir("phase8_api_demo_target") / "demo_adam"
        client = TestClient(create_app())

        response = client.post(
            "/demo-study",
            params={"demo_source_dir": str(source), "study_dir": str(target)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["study_id"], "demo_adam")
        self.assertEqual(payload["target_datasets"], ["ADAE"])
        self.assertIn(payload["execution_mode"], {"llm_downstream_provider", "llm_downstream_r_sandbox"})
        self.assertTrue((target / "input_sdtm" / "ae.csv").exists())
        self.assertTrue((target / "input_sdtm" / "dm.csv").exists())
        self.assertTrue((target / "input_sdtm" / "ex.csv").exists())
        self.assertTrue((target / "input_spec" / "ads_adae_full.csv").exists())
        self.assertTrue((target / "input_spec" / "ads_adsl_full.csv").exists())
        self.assertFalse((target / "input_spec" / "adae.csv").exists())
        self.assertFalse((target / "input_spec" / "adsl.csv").exists())
        self.assertTrue((target / "reference_adam" / "adsl.csv").exists())
        self.assertTrue((target / "reference_adam" / "adae.csv").exists())
        self.assertFalse((target / "legacy_code" / "adae.sas").exists())
        self.assertTrue(any("PSY201 is a separate project" in note for note in payload["notes"]))

    def test_study_inputs_endpoint_returns_human_oriented_file_summary(self) -> None:
        source = _demo_source("phase8_api_input_summary_source")
        target = _workspace_dir("phase8_api_input_summary_target") / "demo_adam"
        client = TestClient(create_app())
        demo_response = client.post(
            "/demo-study",
            params={"demo_source_dir": str(source), "study_dir": str(target)},
        )
        self.assertEqual(demo_response.status_code, 200, demo_response.text)

        response = client.get("/study-inputs", params={"study_dir": str(target)})

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["study_id"], "demo_adam")
        self.assertEqual({item["dataset"] for item in payload["sdtm"]}, {"AE", "DM", "EX"})
        self.assertEqual({item["dataset"] for item in payload["reference_adam"]}, {"ADAE", "ADSL"})
        self.assertIn("ads_adae_full.csv", {item["file_name"] for item in payload["specs"]})

    def test_study_inputs_marks_sas7bdat_as_runtime_supported_when_preview_unavailable(self) -> None:
        study_dir = _workspace_dir("phase8_sas7bdat_summary") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "dm.sas7bdat").write_bytes(b"not a real sas table")

        response = client.get("/study-inputs", params={"study_dir": str(study_dir)})

        self.assertEqual(response.status_code, 200, response.text)
        item = response.json()["sdtm"][0]
        self.assertEqual(item["file_name"], "dm.sas7bdat")
        self.assertEqual(item["format"], "sas7bdat")
        self.assertIn(item["status"], {"not_previewed", "error"})
        self.assertIn("haven", item["note"])

    def test_demo_study_rejects_run_to_completion_llm_endpoint(self) -> None:
        source = _demo_source("phase8_api_demo_run_source")
        target = _workspace_dir("phase8_api_demo_run_target") / "demo_adam"
        client = TestClient(create_app())
        demo_response = client.post(
            "/demo-study",
            params={"demo_source_dir": str(source), "study_dir": str(target)},
        )
        self.assertEqual(demo_response.status_code, 200, demo_response.text)
        demo = demo_response.json()

        run_response = client.post(
            "/runs",
            json={
                "study_dir": demo["study_dir"],
                "run_id": "run_demo_from_endpoint",
                "target_datasets": demo["target_datasets"],
                "config_path": demo["config_path"],
                "execution_mode": "llm_downstream_provider",
            },
        )

        self.assertEqual(run_response.status_code, 400)
        self.assertIn("would bypass review gates", run_response.json()["detail"])
        state = json.loads((target / "runs" / "run_demo_from_endpoint" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(state["current_interrupt"], "split_flow_required")

    def test_generate_review_execute_split_flow(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_split_flow")
        client = TestClient(create_app())

        plan = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_split_flow",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        self.assertEqual(plan.json()["runnable_datasets"], ["ADAE"])

        generated = client.post(
            "/runs/run_split_flow/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        generated_payload = generated.json()
        self.assertEqual(generated_payload["status"], "code_generated")
        self.assertTrue(generated_payload["static_check_path"].endswith("adae_static_check.json"))
        self.assertTrue(Path(generated_payload["static_check_path"]).exists())
        static_check = json.loads(Path(generated_payload["static_check_path"]).read_text(encoding="utf-8"))
        self.assertEqual(static_check["status"], "warning_only")
        workflow_state = json.loads((study_dir / "runs" / "run_split_flow" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(workflow_state["datasets"]["ADAE"]["status"], "needs_review")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["code_state"]["status"], "generated")
        self.assertEqual(workflow_state["current_interrupt"], "code_review")
        self.assertTrue((study_dir / "runs" / "run_split_flow" / "code" / "build_adae.R").exists())
        self.assertTrue(generated_payload["generated_code"])
        self.assertFalse((study_dir / "runs" / "run_split_flow" / "outputs" / "adae.csv").exists())

        blocked_execute = client.post(
            "/runs/run_split_flow/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir)},
        )
        self.assertEqual(blocked_execute.status_code, 400)
        self.assertIn("must be approved", blocked_execute.json()["detail"])

        review = client.post(
            "/runs/run_split_flow/datasets/ADAE/code-review",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Approved for sandbox execution.",
            },
        )
        self.assertEqual(review.status_code, 200, review.text)
        self.assertTrue(review.json()["approved"])
        self.assertTrue(review.json()["static_check_path"].endswith("adae_static_check.json"))
        graph_state = client.get(
            "/runs/run_split_flow/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        adae_graph_state = graph_state.json()["datasets"]["ADAE"]
        self.assertEqual(adae_graph_state["code_state"]["status"], "approved")
        self.assertEqual(adae_graph_state["human_commands"][-1]["interrupt"], "code_review")
        self.assertEqual(adae_graph_state["human_commands"][-1]["action"], "approve")

        executed = client.post(
            "/runs/run_split_flow/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertEqual(executed.json()["status"], "completed")
        output_path = study_dir / "runs" / "run_split_flow" / "outputs" / "adae.csv"
        self.assertTrue(output_path.exists())
        with output_path.open(newline="", encoding="utf-8") as handle:
            rows = list(csv.DictReader(handle))
        self.assertGreater(len(rows), 0)
        self.assertIn("AETERM", rows[0])
        self.assertEqual(rows[0]["AETERM"], "HEADACHE")

        table = client.get(
            "/runs/run_split_flow/datasets/ADAE/table",
            params={"study_dir": str(study_dir), "kind": "generated", "page": 1, "page_size": 1},
        )
        self.assertEqual(table.status_code, 200, table.text)
        self.assertEqual(table.json()["status"], "ok")
        self.assertEqual(len(table.json()["rows"]), 1)

        compare = client.get(
            "/runs/run_split_flow/datasets/ADAE/compare",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(compare.status_code, 200, compare.text)
        self.assertEqual(compare.json()["dataset"], "ADAE")
        self.assertIn(compare.json()["status"], {"match", "differences", "missing_reference"})

        download = client.get(
            "/runs/run_split_flow/datasets/ADAE/download",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        self.assertEqual(download.status_code, 200, download.text)
        self.assertIn("USUBJID", download.text)

    def test_adsl_uses_same_split_flow_as_other_adam_targets(self) -> None:
        study_dir = _study_with_adsl_inputs("phase8_adsl_split_flow")
        client = TestClient(create_app())

        plan = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_adsl_split_flow",
                "target_datasets": ["ADSL"],
            },
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        self.assertEqual(plan.json()["runnable_datasets"], ["ADSL"])

        finalized = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        self.assertEqual(finalized.json()["status"], "input_spec_ready")

        generated = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/generate-code",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        generated_payload = generated.json()
        self.assertEqual(generated_payload["status"], "code_generated")
        self.assertTrue(generated_payload["code_path"].endswith("code/build_adsl.R"))
        self.assertTrue(generated_payload["static_check_path"].endswith("adsl_static_check.json"))
        self.assertIn("mock ADSL generation", generated_payload["generated_code"])
        self.assertFalse((study_dir / "runs" / "run_adsl_split_flow" / "outputs" / "adsl.csv").exists())
        self.assertFalse((study_dir / "runs" / "run_adsl_split_flow" / "audit" / "adsl_manifest.json").exists())

        blocked_execute = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/execute-approved-code",
            json={"study_dir": str(study_dir)},
        )
        self.assertEqual(blocked_execute.status_code, 400)
        self.assertIn("must be approved", blocked_execute.json()["detail"])

        review = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        self.assertTrue(review.json()["approved"])

        executed = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertEqual(executed.json()["status"], "completed")
        output_path = study_dir / "runs" / "run_adsl_split_flow" / "outputs" / "adsl.csv"
        self.assertTrue(output_path.exists())
        with output_path.open(newline="", encoding="utf-8") as handle:
            rows = list(csv.DictReader(handle))
        self.assertEqual(rows[0]["USUBJID"], "01")
        self.assertTrue((study_dir / "runs" / "run_adsl_split_flow" / "llm" / "adsl_context.json").exists())
        self.assertTrue((study_dir / "runs" / "run_adsl_split_flow" / "llm" / "adsl_response.json").exists())
        self.assertFalse((study_dir / "runs" / "run_adsl_split_flow" / "audit" / "adsl_manifest.json").exists())

    def test_approved_draft_spec_is_invalidated_when_inputs_change(self) -> None:
        study_dir = _workspace_dir("phase8_stale_draft_spec") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")

        finalized = client.post(
            "/runs/run_stale_draft/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        self.assertEqual(finalized.json()["next_action"], "review_draft_spec")

        review = client.post(
            "/runs/run_stale_draft/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)

        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")
        generated = client.post(
            "/runs/run_stale_draft/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(generated.status_code, 400)
        self.assertIn("Approved draft spec is stale", generated.json()["detail"])

    def test_approved_draft_spec_is_invalidated_when_approved_file_changes(self) -> None:
        study_dir = _workspace_dir("phase8_tampered_approved_draft_spec") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")

        finalized = client.post(
            "/runs/run_tampered_draft/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        review = client.post(
            "/runs/run_tampered_draft/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        approved_path = Path(review.json()["approved_spec_path"])
        approved_payload = json.loads(approved_path.read_text(encoding="utf-8"))
        approved_payload["variables"].append({"variable": "TAMPERED", "source_domains": ["AE"]})
        approved_path.write_text(json.dumps(approved_payload), encoding="utf-8")

        generated = client.post(
            "/runs/run_tampered_draft/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(generated.status_code, 400)
        self.assertIn("Approved draft spec changed after approval", generated.json()["detail"])

    def test_finalize_inputs_blocks_review_required_dependency_evidence(self) -> None:
        study_dir = _workspace_dir("phase8_dependency_review_blocks_finalize") / "MY_STUDY"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        legacy_dir = study_dir / "legacy_code"
        output_dir = study_dir / "runs" / "run_dependency_warning" / "outputs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        legacy_dir.mkdir()
        output_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (output_dir / "adlb.csv").write_text("USUBJID,PARAMCD\n01,ALT\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        (legacy_dir / "ADAE.sas").write_text("data adae; merge ae adsl; by usubjid; run;", encoding="utf-8")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_dependency_gate/datasets/ADAE/finalize-inputs",
            json={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("cannot continue until dependency issues are resolved", response.json()["detail"])

    def test_finalize_inputs_blocks_dependency_warning(self) -> None:
        study_dir = _workspace_dir("phase8_dependency_warning_blocks_finalize") / "MY_STUDY"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        legacy_dir = study_dir / "legacy_code"
        output_dir = study_dir / "runs" / "run_dependency_warning" / "outputs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        legacy_dir.mkdir()
        output_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (output_dir / "adlb.csv").write_text("USUBJID,PARAMCD\n01,ALT\n", encoding="utf-8")
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
        (legacy_dir / "ADTTE.sas").write_text("data adtte; merge adlb adae; by usubjid; run;", encoding="utf-8")
        client = TestClient(create_app())

        prepared = client.post(
            "/runs/prepare",
            json={"study_dir": str(study_dir), "run_id": "run_dependency_warning", "target_datasets": ["ADTTE"]},
        )
        response = client.post(
            "/runs/run_dependency_warning/datasets/ADTTE/finalize-inputs",
            json={"study_dir": str(study_dir)},
        )

        self.assertEqual(prepared.status_code, 200, prepared.text)
        self.assertEqual(prepared.json()["dependency_review_status"], "warning")
        self.assertEqual(response.status_code, 400)
        self.assertIn("dependency plan has warnings", response.json()["detail"])

    def test_draft_spec_without_fingerprint_cannot_be_approved(self) -> None:
        study_dir = _workspace_dir("phase8_draft_without_fingerprint") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_no_fingerprint"
        spec_dir = run_dir / "specs"
        spec_dir.mkdir(parents=True)
        (spec_dir / "adae_draft_spec.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": []}),
            encoding="utf-8",
        )
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_no_fingerprint/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("no input fingerprint", response.json()["detail"])

    def test_approved_draft_spec_without_fingerprint_cannot_be_reused(self) -> None:
        study_dir = _workspace_dir("phase8_approved_draft_without_fingerprint") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_approved_no_fingerprint"
        approved_dir = run_dir / "approved_specs"
        review_dir = run_dir / "reviews"
        approved_dir.mkdir(parents=True)
        review_dir.mkdir(parents=True)
        (approved_dir / "adae_approved_spec.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": []}),
            encoding="utf-8",
        )
        (review_dir / "adae_draft_spec_review.json").write_text(
            json.dumps({"decision": "approve", "approved": True}),
            encoding="utf-8",
        )
        client = TestClient(create_app())

        generated = client.post(
            "/runs/run_approved_no_fingerprint/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(generated.status_code, 400)
        self.assertIn("missing its input fingerprint", generated.json()["detail"])

    def test_execute_approved_code_terminal_failure_is_explicit(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_failure")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        run_dir = study_dir / "runs" / "run_terminal"
        code_dir = run_dir / "code"
        (code_dir / "build_adae.R").write_text("stop('forced failure')\n", encoding="utf-8")
        review = client.post(
            "/runs/run_terminal/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("Generated code changed after graph code generation", review.json()["detail"])
        generated = client.post(
            "/runs/run_terminal/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        (code_dir / "build_adae.R").write_text("stop('forced failure')\n", encoding="utf-8")
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal",
            dataset="ADAE",
            code_path=code_dir / "build_adae.R",
            code_sha256=f"sha256:{sha256_file(code_dir / 'build_adae.R')}",
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)

        executed = client.post(
            "/runs/run_terminal/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 200, executed.text)
        payload = executed.json()
        self.assertEqual(payload["status"], "terminal_failure")
        self.assertTrue(payload["terminal_failure"])
        report = json.loads(Path(payload["validation_report_path"]).read_text(encoding="utf-8"))
        self.assertTrue(report["terminal_failure"])
        self.assertFalse(report["partial_output_usable"])

    def test_terminal_failure_output_is_not_previewed_or_downloadable(self) -> None:
        study_dir = _workspace_dir("phase8_terminal_failure_hidden_output") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_terminal_hidden"
        code_dir = run_dir / "code"
        review_dir = run_dir / "review"
        outputs_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        code_dir.mkdir(parents=True)
        review_dir.mkdir(parents=True)
        outputs_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (outputs_dir / "adae.csv").write_text("USUBJID,AETERM\n01,PARTIAL\n", encoding="utf-8")
        (validation_dir / "adae_validation_report.json").write_text(
            json.dumps({"dataset": "ADAE", "status": "fail", "terminal_failure": True, "partial_output_usable": False}),
            encoding="utf-8",
        )
        client = TestClient(create_app())

        table = client.get(
            "/runs/run_terminal_hidden/datasets/ADAE/table",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        download = client.get(
            "/runs/run_terminal_hidden/datasets/ADAE/download",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        review = client.get(
            "/runs/run_terminal_hidden/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(table.status_code, 200, table.text)
        self.assertEqual(table.json()["status"], "missing")
        self.assertEqual(download.status_code, 404)
        self.assertEqual(review.status_code, 200, review.text)
        self.assertIsNone(review.json()["dataset_reviews"][0]["output_preview"])
        self.assertIsNone(review.json()["dataset_reviews"][0]["output_path"])

    def test_code_approval_is_invalidated_when_code_or_inputs_change(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_stale_code_approval")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_stale_code/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review = client.post(
            "/runs/run_stale_code/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        code_path = study_dir / "runs" / "run_stale_code" / "code" / "build_adae.R"
        code_path.write_text(code_path.read_text(encoding="utf-8") + "\n# changed after approval\n", encoding="utf-8")

        changed_code = client.post(
            "/runs/run_stale_code/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(changed_code.status_code, 400)
        self.assertIn("Generated code changed after approval", changed_code.json()["detail"])

        review_again = client.post(
            "/runs/run_stale_code/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review_again.status_code, 400)
        self.assertIn("Generated code must be recorded in graph state", review_again.json()["detail"])
        regenerated = client.post(
            "/runs/run_stale_code/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(regenerated.status_code, 200, regenerated.text)
        review_again = client.post(
            "/runs/run_stale_code/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review_again.status_code, 200, review_again.text)
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        changed_inputs = client.post(
            "/runs/run_stale_code/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(changed_inputs.status_code, 400)
        self.assertIn("Code approval is stale", changed_inputs.json()["detail"])

    def test_code_review_does_not_write_approval_when_generated_code_changed(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_no_stale_code_review_file")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_no_stale_code_review_file/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_no_stale_code_review_file" / "code" / "build_adae.R"
        review_path = study_dir / "runs" / "run_no_stale_code_review_file" / "review" / "adae_code_review.json"
        code_path.write_text(code_path.read_text(encoding="utf-8") + "\n# changed before review\n", encoding="utf-8")

        review = client.post(
            "/runs/run_no_stale_code_review_file/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("Generated code changed after graph code generation", review.json()["detail"])
        self.assertFalse(review_path.exists())

    def test_code_review_does_not_write_approval_when_inputs_changed_after_code_generation(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_no_stale_input_review_file")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_no_stale_input_review_file/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review_path = study_dir / "runs" / "run_no_stale_input_review_file" / "review" / "adae_code_review.json"
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        review = client.post(
            "/runs/run_no_stale_input_review_file/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("Study inputs changed after graph code generation", review.json()["detail"])
        self.assertFalse(review_path.exists())

    def test_product_steps_do_not_reprepare_and_overwrite_existing_graph_dataset_state(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_no_reprepare_overwrite")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_no_reprepare_overwrite/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        before = client.get(
            "/runs/run_no_reprepare_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(before.status_code, 200, before.text)
        before_code_state = before.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(before_code_state["status"], "generated")

        finalized = client.post(
            "/runs/run_no_reprepare_overwrite/datasets/ADAE/finalize-inputs",
            json={"study_dir": str(study_dir)},
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        after = client.get(
            "/runs/run_no_reprepare_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(after.status_code, 200, after.text)
        after_code_state = after.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(after_code_state["status"], "generated")
        self.assertEqual(after_code_state["code_sha256"], before_code_state["code_sha256"])

    def test_public_prepare_does_not_overwrite_existing_graph_dataset_state(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_prepare_no_overwrite")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_prepare_no_overwrite/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        before = client.get(
            "/runs/run_prepare_no_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(before.status_code, 200, before.text)
        before_code_state = before.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(before_code_state["status"], "generated")

        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_prepare_no_overwrite",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)
        after = client.get(
            "/runs/run_prepare_no_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(after.status_code, 200, after.text)
        after_code_state = after.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(after_code_state["status"], "generated")
        self.assertEqual(after_code_state["code_sha256"], before_code_state["code_sha256"])

    def test_public_prepare_marks_existing_graph_dataset_state_stale_when_inputs_change(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_prepare_marks_stale")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_prepare_marks_stale/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_prepare_marks_stale",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)
        graph_state = client.get(
            "/runs/run_prepare_marks_stale/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        dataset_state = graph_state.json()["datasets"]["ADAE"]
        self.assertEqual(dataset_state["status"], "needs_review")
        self.assertEqual(dataset_state["current_interrupt"]["name"], "code_review")
        self.assertEqual(dataset_state["code_state"]["status"], "stale")
        self.assertIn("input_sdtm/ae.csv", dataset_state["code_state"]["input_diff"]["changed_files"])

    def test_code_review_requires_graph_generated_code_state(self) -> None:
        study_dir = _workspace_dir("phase8_code_review_requires_graph") / "MY_STUDY"
        code_dir = study_dir / "runs" / "run_no_graph_generation" / "code"
        code_dir.mkdir(parents=True)
        (code_dir / "build_adae.R").write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        client = TestClient(create_app())

        review = client.post(
            "/runs/run_no_graph_generation/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(review.status_code, 400)
        self.assertIn("Generated code must be recorded in graph state", review.json()["detail"])

    def test_execute_requires_graph_code_review_not_only_review_json(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_execute_requires_graph_review")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_execute_requires_graph_review/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        run_dir = study_dir / "runs" / "run_execute_requires_graph_review"
        code_path = run_dir / "code" / "build_adae.R"
        static_check_path = run_dir / "static_checks" / "adae_static_check.json"
        review_dir = run_dir / "review"
        review_dir.mkdir(exist_ok=True)
        graph_state = client.get(
            "/runs/run_execute_requires_graph_review/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        code_state = graph_state.json()["datasets"]["ADAE"]["code_state"]
        (review_dir / "adae_code_review.json").write_text(
            json.dumps(
                {
                    "study_id": study_dir.name,
                    "run_id": "run_execute_requires_graph_review",
                    "dataset": "ADAE",
                    "decision": "approve",
                    "approved": True,
                    "reviewer": "tester",
                    "input_fingerprint": input_fingerprint(study_dir),
                    "code_path": str(code_path.as_posix()),
                    "code_sha256": f"sha256:{sha256_file(code_path)}",
                    "static_check_path": str(static_check_path.as_posix()),
                    "static_check_sha256": f"sha256:{sha256_file(static_check_path)}",
                    "spec_source": code_state.get("spec_source"),
                    "spec_path": code_state.get("spec_path"),
                    "spec_sha256": code_state.get("spec_sha256"),
                }
            ),
            encoding="utf-8",
        )

        executed = client.post(
            "/runs/run_execute_requires_graph_review/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 400, executed.text)
        self.assertIn("Graph state does not contain an approved code-review decision", executed.json()["detail"])

    def test_execute_rejects_changed_graph_spec_artifact(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_execute_changed_graph_spec")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_changed_graph_spec/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review = client.post(
            "/runs/run_changed_graph_spec/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        spec_path = study_dir / "input_spec" / "adae.json"
        spec_payload = json.loads(spec_path.read_text(encoding="utf-8"))
        spec_payload["variables"].append({"variable": "TAMPERED", "source_domains": ["AE"]})
        spec_path.write_text(json.dumps(spec_payload), encoding="utf-8")

        executed = client.post(
            "/runs/run_changed_graph_spec/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 400, executed.text)
        self.assertIn("Code approval is stale", executed.json()["detail"])

    def test_execute_rejects_changed_runtime_dependency_artifact(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_runtime_dependency_hash")
        dependency_output_dir = study_dir / "runs" / "run_dependency_hash" / "outputs"
        dependency_output_dir.mkdir(parents=True)
        dependency_path = dependency_output_dir / "adsl.csv"
        dependency_path.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (study_dir / "input_spec" / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        client = TestClient(create_app())

        generated = client.post(
            "/runs/run_dependency_hash/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        graph_state = client.get(
            "/runs/run_dependency_hash/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        dependency_artifacts = graph_state.json()["datasets"]["ADAE"]["code_state"]["dependency_artifacts"]
        self.assertEqual(dependency_artifacts[0]["required_dataset"], "ADSL")
        self.assertTrue(dependency_artifacts[0]["artifact_path"].endswith("outputs/adsl.csv"))
        review = client.post(
            "/runs/run_dependency_hash/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        dependency_path.write_text("USUBJID,TRTSDT\n01,2024-02-02\n", encoding="utf-8")

        executed = client.post(
            "/runs/run_dependency_hash/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 400, executed.text)
        self.assertIn("Dependency artifact changed after graph approval", executed.json()["detail"])

    def test_code_review_cleans_approval_json_when_graph_recording_fails(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_review_cleanup_on_graph_failure")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_review_cleanup/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review_path = study_dir / "runs" / "run_review_cleanup" / "review" / "adae_code_review.json"

        with patch("adam_agent.api.service.GraphGateway.record_code_review", side_effect=ValueError("forced graph failure")):
            review = client.post(
                "/runs/run_review_cleanup/datasets/ADAE/code-review",
                json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
            )

        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("forced graph failure", review.json()["detail"])
        self.assertFalse(review_path.exists())
        graph_state = client.get(
            "/runs/run_review_cleanup/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        self.assertEqual(graph_state.json()["datasets"]["ADAE"]["code_state"]["status"], "generated")

    def test_review_summary_recovers_multiple_outputs_from_same_run(self) -> None:
        study_dir = _workspace_dir("phase8_multi_output_review") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_multi_output"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (output_dir / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        for dataset in ["ADSL", "ADAE"]:
            (validation_dir / f"{dataset.lower()}_validation_report.json").write_text(
                json.dumps(
                    {
                        "dataset": dataset,
                        "status": "pass",
                        "terminal_failure": False,
                        "partial_output_usable": True,
                    }
                ),
                encoding="utf-8",
            )
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_multi_output/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        reviews = response.json()["dataset_reviews"]
        self.assertEqual({item["dataset"] for item in reviews}, {"ADSL", "ADAE"})
        for item in reviews:
            self.assertEqual(item["status"], "completed")
            self.assertTrue(item["output_preview"])
            self.assertTrue(item["output_path"].endswith(f"{item['dataset'].lower()}.csv"))

    def test_llm_connection_test_rejects_mock_provider(self) -> None:
        client = TestClient(create_app())

        response = client.post(
            "/llm/test-connection",
            json={
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "llm_exposure": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("real provider", response.json()["detail"])

    def test_llm_connection_test_uses_browser_scoped_provider_settings(self) -> None:
        client = TestClient(create_app())
        requests = []

        class FakeLLMClient:
            def generate(self, request):
                requests.append(request)
                return SimpleNamespace(
                    response_text="ADAM_AGENT_CONNECTION_OK",
                    call_record=SimpleNamespace(
                        provider_alias="openai-compatible",
                        transport="fake-transport",
                        provider_base_url="https://relay.example/v1",
                        external_relay=True,
                        risk_flags=["external_relay"],
                    ),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeLLMClient()) as builder:
            response = client.post(
                "/llm/test-connection",
                json={
                    "llm_provider": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "base_url": "https://relay.example/v1",
                        "api_key": "test-key",
                        "allow_custom_base_url": True,
                        "custom_base_url_approved_by": "tester",
                    },
                    "llm_exposure": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 3,
                        "include_reference_rows": True,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "ok")
        self.assertEqual(payload["model"], "gpt-5.5")
        self.assertTrue(payload["external_relay"])
        self.assertEqual(payload["risk_flags"], ["external_relay"])
        self.assertEqual(len(requests), 1)
        self.assertEqual(requests[0].model, "gpt-5.5")
        self.assertEqual(requests[0].exposure.mode, "demo_rich_context")
        builder.assert_called_once()
        provider_config = builder.call_args.args[0]
        self.assertEqual(provider_config.api_key, "test-key")

    def test_generate_code_uses_browser_scoped_real_provider_settings(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_real_provider_override")
        client = TestClient(create_app())
        requests = []

        class FakeLLMClient:
            def generate(self, request):
                requests.append(request)
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)",
                            "assumptions": ["Test response."],
                            "risk_points": [],
                            "used_inputs": ["AE"],
                            "expected_outputs": ["outputs/adae.csv"],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeLLMClient()) as builder:
            response = client.post(
                "/runs/run_real_provider_override/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "base_url": "https://relay.example/v1",
                        "api_key": "test-key",
                        "max_tokens": 1234,
                        "allow_custom_base_url": True,
                        "custom_base_url_approved_by": "tester",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 3,
                        "include_reference_rows": True,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        self.assertEqual(response.json()["status"], "code_generated")
        builder.assert_called_once()
        self.assertEqual(builder.call_args.args[0].provider, "openai-compatible")
        self.assertEqual(builder.call_args.args[0].model, "gpt-5.5")
        self.assertEqual(builder.call_args.args[0].api_key, "test-key")
        self.assertEqual(len(requests), 1)
        self.assertEqual(requests[0].model, "gpt-5.5")
        self.assertEqual(requests[0].max_tokens, 1234)
        self.assertEqual(requests[0].exposure.mode, "demo_rich_context")
        self.assertIn("## Target Spec", requests[0].prompt)
        self.assertNotIn('"target_spec"', requests[0].prompt)
        self.assertTrue(requests[0].prompt_artifact_id.startswith("llm_prompt_compact_"))
        self.assertTrue(
            (study_dir / "runs" / "run_real_provider_override" / "llm" / "adae_compact_prompt.txt").exists()
        )

    def test_missing_input_spec_requires_draft_spec_approval_before_code_generation(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_missing_spec_draft")
        client = TestClient(create_app())
        requests = []

        class FakeLLMClient:
            def generate(self, request):
                requests.append(request)
                if request.node == "draft_spec_from_evidence":
                    return SimpleNamespace(
                        response_text=json.dumps(
                            {
                                "dataset": "ADAE",
                                "variables": [
                                    {
                                        "variable": "AETERM",
                                        "label": "Reported Term for the Adverse Event",
                                        "type": "character",
                                        "source_domains": ["AE"],
                                        "source_variables": ["AETERM"],
                                        "derivation": "Copy from AE.AETERM based on legacy ADAE.sas evidence.",
                                        "confidence": 0.7,
                                        "review_required": True,
                                        "review_reasons": ["Generated from legacy SAS because approved spec is missing."],
                                        "risk_level": "high",
                                        "assumptions": ["Legacy SAS is treated as evidence, not directly executed."],
                                    }
                                ],
                            }
                        ),
                        call_record=SimpleNamespace(),
                    )
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(AETERM='HEADACHE'), 'outputs/adae.csv', row.names = FALSE)",
                            "assumptions": ["Used generated draft spec."],
                            "risk_points": ["Draft spec requires human review."],
                            "used_inputs": ["AE"],
                            "expected_outputs": ["outputs/adae.csv"],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeLLMClient()):
            blocked = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )
            draft_response = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/draft-spec",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )
            review_response = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/draft-spec-review",
                json={
                    "study_dir": str(study_dir),
                    "reviewer": "tester",
                    "decision": "approve",
                    "notes": "Approved draft spec for this test run.",
                },
            )
            response = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )

        self.assertEqual(blocked.status_code, 400, blocked.text)
        self.assertIn("Generate and approve a draft spec", blocked.json()["detail"])
        self.assertEqual(draft_response.status_code, 200, draft_response.text)
        draft_payload = draft_response.json()
        self.assertEqual(draft_payload["status"], "draft_spec_generated")
        self.assertTrue(draft_payload["spec_path"].endswith("specs/adae_draft_spec.json"))
        self.assertEqual(review_response.status_code, 200, review_response.text)
        self.assertTrue(review_response.json()["approved"])
        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "code_generated")
        self.assertTrue(payload["draft_spec_path"].endswith("approved_specs/adae_approved_spec.json"))
        self.assertTrue(any("user-approved draft spec" in warning for warning in payload["warnings"]))
        self.assertEqual([request.node for request in requests], ["draft_spec_from_evidence", "generate_downstream_code_for_review"])
        draft_spec = json.loads((study_dir / "runs" / "run_missing_spec_draft" / "specs" / "adae_draft_spec.json").read_text(encoding="utf-8"))
        self.assertEqual(draft_spec["dataset"], "ADAE")
        self.assertEqual(draft_spec["status"], "draft")
        self.assertEqual(draft_spec["variables"][0]["variable"], "AETERM")
        compact_prompt = (study_dir / "runs" / "run_missing_spec_draft" / "llm" / "adae_compact_prompt.txt").read_text(encoding="utf-8")
        self.assertIn("AETERM|character|AE.AETERM|Copy from AE.AETERM", compact_prompt)
        self.assertIn("user-approved draft spec", compact_prompt)

    def test_finalize_inputs_uses_existing_input_spec_without_draft_generation(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_finalize_existing_spec")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_finalize_existing_spec/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "input_spec_ready")
        self.assertTrue(payload["input_spec_available"])
        self.assertFalse(payload["draft_spec_required"])
        self.assertEqual(payload["next_action"], "generate_code")
        self.assertTrue(payload["input_spec_path"].endswith("input_spec/adae.json"))
        self.assertIsNone(payload["draft_spec"])
        self.assertFalse((study_dir / "runs" / "run_finalize_existing_spec" / "specs" / "adae_draft_spec.json").exists())

    def test_finalize_inputs_generates_review_required_draft_spec_when_spec_missing(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_finalize_missing_spec")
        client = TestClient(create_app())
        requests = []

        class FakeDraftLLMClient:
            def generate(self, request):
                requests.append(request)
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "variables": [
                                {
                                    "variable": "AETERM",
                                    "label": "Reported Term",
                                    "type": "character",
                                    "source_domains": ["AE"],
                                    "source_variables": ["AETERM"],
                                    "derivation": "Copy from AE.AETERM based on uploaded evidence.",
                                    "confidence": 0.7,
                                    "review_required": True,
                                    "review_reasons": ["Generated because approved input_spec is missing."],
                                    "risk_level": "high",
                                }
                            ],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeDraftLLMClient()):
            response = client.post(
                "/runs/run_finalize_missing_spec/datasets/ADAE/finalize-inputs",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "draft_spec_review_required")
        self.assertFalse(payload["input_spec_available"])
        self.assertTrue(payload["draft_spec_required"])
        self.assertTrue(payload["draft_spec_generated"])
        self.assertEqual(payload["next_action"], "review_draft_spec")
        self.assertIsNotNone(payload["draft_spec"])
        self.assertEqual(payload["draft_spec"]["variables"][0]["variable"], "AETERM")
        self.assertEqual([request.node for request in requests], ["draft_spec_from_evidence"])
        self.assertTrue((study_dir / "runs" / "run_finalize_missing_spec" / "specs" / "adae_draft_spec.json").exists())

    def test_finalize_inputs_accepts_mock_model_alias_from_ui(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_finalize_mock_alias")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_finalize_mock_alias/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "llm_provider_override": {"provider": "mock", "model": "mock"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "draft_spec_review_required")
        self.assertEqual(payload["draft_spec"]["dataset"], "ADAE")
        self.assertTrue(payload["draft_spec"]["variables"])

    def test_finalize_inputs_passes_rscript_path_to_context_builder(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_finalize_rscript_path")
        client = TestClient(create_app())
        rscript_paths = []

        class FakeDraftLLMClient:
            def generate(self, _request):
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "variables": [
                                {
                                    "variable": "AETERM",
                                    "label": "Reported Term",
                                    "type": "character",
                                    "source_domains": ["AE"],
                                    "source_variables": ["AETERM"],
                                    "derivation": "Copy from AE.AETERM.",
                                    "confidence": 0.7,
                                    "review_required": True,
                                    "risk_level": "high",
                                }
                            ],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        from adam_agent.api import service as service_module

        original_builder = service_module.build_target_llm_context

        def tracking_builder(*args, **kwargs):
            rscript_paths.append(kwargs.get("rscript_path"))
            return original_builder(*args, **kwargs)

        with (
            patch("adam_agent.api.service.build_llm_client", return_value=FakeDraftLLMClient()),
            patch("adam_agent.api.service.build_target_llm_context", side_effect=tracking_builder),
        ):
            response = client.post(
                "/runs/run_finalize_rscript_path/datasets/ADAE/finalize-inputs",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe",
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        self.assertTrue(rscript_paths)
        self.assertTrue(all(path == "C:/Dev/R-4.5.2/bin/Rscript.exe" for path in rscript_paths))

    def test_generate_code_returns_readable_error_for_bad_llm_json(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_bad_llm_json")
        client = TestClient(create_app())

        class BadJsonLLMClient:
            def generate(self, _request):
                return SimpleNamespace(response_text="not json", call_record=SimpleNamespace())

        with patch("adam_agent.api.service.build_llm_client", return_value=BadJsonLLMClient()):
            response = client.post(
                "/runs/run_bad_llm_json/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                    },
                },
            )

        self.assertEqual(response.status_code, 400)
        self.assertIn("not valid JSON", response.json()["detail"])

    def test_create_run_rejects_llm_run_to_completion(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_api_create_run")
        client = TestClient(create_app())

        response = client.post(
            "/runs",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_phase8_api",
                "target_datasets": ["ADAE"],
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "execution_mode": "llm_downstream_provider",
            },
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("Use /runs/prepare", response.json()["detail"])
        state = json.loads((study_dir / "runs" / "run_phase8_api" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(state["status"], "blocked")
        self.assertEqual(state["current_interrupt"], "split_flow_required")

    def test_create_run_rejects_missing_study_dir(self) -> None:
        client = TestClient(create_app())

        response = client.post(
            "/runs",
            json={
                "study_dir": str(ROOT / ".tmp_tests" / "does_not_exist"),
                "run_id": "run_missing",
                "target_datasets": ["ADAE"],
            },
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("study_dir does not exist", response.json()["detail"])

    def test_artifact_endpoint_blocks_path_escape(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_api_path_escape")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_x/artifacts/read",
            params={"study_dir": str(study_dir)},
            json={"relative_path": "../outside.json"},
        )

        self.assertEqual(response.status_code, 404)


def _study_with_adae_inputs(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    input_spec = study_dir / "input_spec"
    reference_adam = study_dir / "reference_adam"
    input_sdtm.mkdir(parents=True)
    input_spec.mkdir()
    reference_adam.mkdir()
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (input_spec / "adae.json").write_text(
        json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
        encoding="utf-8",
    )
    (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
    return study_dir


def _study_with_adsl_inputs(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    input_spec = study_dir / "input_spec"
    reference_adam = study_dir / "reference_adam"
    input_sdtm.mkdir(parents=True)
    input_spec.mkdir()
    reference_adam.mkdir()
    (input_sdtm / "dm.csv").write_text(
        "STUDYID,USUBJID,SUBJID,ARM,ACTARM\nS1,01,1001,Placebo,Placebo\n",
        encoding="utf-8",
    )
    (input_sdtm / "ex.csv").write_text("USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n", encoding="utf-8")
    (input_spec / "adsl.json").write_text(
        json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
        encoding="utf-8",
    )
    (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
    return study_dir


def _study_without_spec_with_auxiliary_evidence(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    reference_adam = study_dir / "reference_adam"
    legacy_code = study_dir / "legacy_code"
    input_sdtm.mkdir(parents=True)
    reference_adam.mkdir()
    legacy_code.mkdir()
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM,AESTDTC\n01,HEADACHE,2024-01-02\n", encoding="utf-8")
    (reference_adam / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (legacy_code / "ADAE.sas").write_text(
        "proc sql; create table adae as select usubjid, aeterm from sdtmdata.ae; quit;\n",
        encoding="utf-8",
    )
    return study_dir


def _demo_source(name: str) -> Path:
    source = _workspace_dir(name) / "demo-data"
    source.mkdir(parents=True)
    (source / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (source / "dm.csv").write_text("USUBJID,SUBJID\n01,1001\n", encoding="utf-8")
    (source / "ex.csv").write_text("USUBJID,EXSTDTC\n01,2024-01-01\n", encoding="utf-8")
    (source / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
    (source / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (source / "ads_adae_full.csv").write_text(
        "Dataset,Variable,Label,Type,Source,Derivation\n"
        "ADAE,USUBJID,Unique Subject Identifier,Copied,SDTM.AE.USUBJID,Copied from source\n"
        "ADAE,TRTSDT,Treatment Start Date,Copied,ADSL.TRTSDT,Copied from ADSL\n",
        encoding="utf-8",
    )
    (source / "ads_adsl_full.csv").write_text(
        "Dataset,Variable,Label,Type,Source,Derivation\n"
        "ADSL,USUBJID,Unique Subject Identifier,Copied,SDTM.DM.USUBJID,Copied from source\n",
        encoding="utf-8",
    )
    return source


if __name__ == "__main__":
    unittest.main()
