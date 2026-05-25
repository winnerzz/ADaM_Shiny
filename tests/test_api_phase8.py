"""Tests for the Phase 8.1 FastAPI backend boundary."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

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


if __name__ == "__main__":
    unittest.main()
