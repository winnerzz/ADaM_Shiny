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
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app


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
        self.assertIn("Try With Shiny Demo Data", response.text)
        self.assertIn("Use My Study Files", response.text)
        self.assertIn("Generate R Code", response.text)
        self.assertIn("Approve And Run Locally", response.text)
        self.assertIn("Audit Timeline", response.text)
        self.assertIn("Advanced settings and audit files", response.text)
        self.assertIn("Upload Define", response.text)
        self.assertIn("Upload Legacy Code", response.text)
        self.assertIn("Add another ADaM target", response.text)
        self.assertIn("addTargetButton", response.text)
        self.assertIn("Real LLM API", response.text)
        self.assertIn("testLlmButton", response.text)
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

    def test_demo_study_can_run_through_api(self) -> None:
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

        self.assertEqual(run_response.status_code, 200, run_response.text)
        payload = run_response.json()
        self.assertEqual(payload["status"], "completed")
        self.assertEqual(payload["dataset_results"][0]["dataset"], "ADAE")
        self.assertEqual(payload["dataset_results"][0]["status"], "completed_stub")

        review_response = client.get(
            "/runs/run_demo_from_endpoint/review-summary",
            params={"study_dir": demo["study_dir"]},
        )
        self.assertEqual(review_response.status_code, 200, review_response.text)
        review = review_response.json()
        self.assertEqual(review["study_id"], "demo_adam")
        self.assertEqual(review["dataset_reviews"][0]["dataset"], "ADAE")
        self.assertTrue(review["dataset_reviews"][0]["generated_code"])
        self.assertEqual(review["dataset_reviews"][0]["output_preview"]["file_name"], "adae.csv")
        self.assertEqual(review["dataset_reviews"][0]["reference_preview"]["file_name"], "adae.csv")

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

    def test_create_run_and_read_artifacts(self) -> None:
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

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "completed")
        self.assertEqual(payload["execution_mode"], "llm_downstream_provider")
        self.assertEqual(payload["dataset_results"][0]["status"], "completed_stub")
        self.assertTrue(payload["audit_manifest"].endswith("audit/manifest.json"))

        plan = client.get(
            "/runs/run_phase8_api/dependency-plan",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        self.assertEqual(plan.json()["requested_datasets"], ["ADAE"])

        validation = client.get(
            "/runs/run_phase8_api/datasets/ADAE/validation",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(validation.status_code, 200, validation.text)
        self.assertEqual(validation.json()["status"], "structural_stub_pass")

        manifest = client.get(
            "/runs/run_phase8_api/audit-manifest",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(manifest.status_code, 200, manifest.text)
        self.assertEqual(manifest.json()["manifest_scope"], "study")

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
