"""API tests for external Codex package import."""

from __future__ import annotations

import sys
import unittest
import uuid
from pathlib import Path

from fastapi.testclient import TestClient

from tests.temp_workspace import test_session_root

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()

try:
    from adam_agent.api.app import create_app
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.api.app import create_app


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


def _write_package(root: Path, dataset: str = "ADAE") -> None:
    for relative_path in ["R/00_config.R", "R/01_helpers.R", "R/run_all.R", "report.md"]:
        path = root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("# placeholder\n", encoding="utf-8")
    (root / "R" / f"20_{dataset.lower()}.R").write_text(
        "dir.create('outputs', showWarnings = FALSE, recursive = TRUE)\n"
        f"write.csv(data.frame(USUBJID = '01'), 'outputs/{dataset.lower()}.csv', row.names = FALSE)\n",
        encoding="utf-8",
    )


class ExternalCodePackageApiTests(unittest.TestCase):
    def test_import_external_code_package_endpoint_records_review_gate(self) -> None:
        study_dir = _workspace_dir("external_code_api") / "PSY201"
        (study_dir / "input_sdtm").mkdir(parents=True)
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        package_dir = _workspace_dir("external_code_api_package")
        _write_package(package_dir)

        client = TestClient(create_app())
        response = client.post(
            "/runs/run_external_api/datasets/ADAE/import-external-code-package",
            json={
                "study_dir": str(study_dir),
                "study_id": "PSY201",
                "staging_root": str(package_dir),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "external_code_imported")
        self.assertEqual(payload["dataset"], "ADAE")
        self.assertTrue(payload["code_path"].endswith("runs/run_external_api/code/build_adae.R"))
        self.assertTrue(payload["static_check_path"].endswith("runs/run_external_api/static_checks/adae_external_static_check.json"))
        progress = client.get(
            "/runs/run_external_api/progress",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(progress.status_code, 200, progress.text)
        progress_payload = progress.json()
        dataset_progress = progress_payload["datasets"][0]
        self.assertEqual(dataset_progress["dataset"], "ADAE")
        self.assertEqual(dataset_progress["next_action"], "review_code")
        self.assertEqual(dataset_progress["code_status"], "generated")

        review = client.post(
            "/runs/run_external_api/datasets/ADAE/code-review",
            json={
                "study_dir": str(study_dir),
                "reviewer": "unit_test",
                "decision": "approve",
                "notes": "External code package reviewed in API smoke test.",
            },
        )
        self.assertEqual(review.status_code, 200, review.text)
        review_payload = review.json()
        self.assertTrue(review_payload["approved"])
        self.assertEqual(review_payload["decision"], "approve")

        progress_after_review = client.get(
            "/runs/run_external_api/progress",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(progress_after_review.status_code, 200, progress_after_review.text)
        reviewed_dataset = progress_after_review.json()["datasets"][0]
        self.assertEqual(reviewed_dataset["next_action"], "execute_approved_code")
        self.assertEqual(reviewed_dataset["code_status"], "approved")


if __name__ == "__main__":
    unittest.main()
