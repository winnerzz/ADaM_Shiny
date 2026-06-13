"""Tests for importing external Codex-authored R packages into Studio flow."""

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
    from adam_agent.agents.external_code_package import ExternalCodePackageImportRequest, import_external_code_package
    from adam_agent.api.models import ImportExternalCodePackageRequest
    from adam_agent.api.service import import_external_code_package_for_dataset
    from adam_agent.graph.gateway import GraphGateway
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.agents.external_code_package import ExternalCodePackageImportRequest, import_external_code_package
    from adam_agent.api.models import ImportExternalCodePackageRequest
    from adam_agent.api.service import import_external_code_package_for_dataset
    from adam_agent.graph.gateway import GraphGateway


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


def _write_external_r_package(staging_root: Path, dataset: str = "ADAE") -> Path:
    for relative_path in ["R/00_config.R", "R/01_helpers.R", "R/run_all.R", "report.md"]:
        path = staging_root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("# placeholder\n", encoding="utf-8")
    code_path = staging_root / "R" / f"20_{dataset.lower()}.R"
    code_path.write_text(
        "dir.create('outputs', showWarnings = FALSE, recursive = TRUE)\n"
        f"write.csv(data.frame(USUBJID = '01'), 'outputs/{dataset.lower()}.csv', row.names = FALSE)\n",
        encoding="utf-8",
    )
    (staging_root / "assumptions.md").write_text("- External package assumption\n", encoding="utf-8")
    return code_path


class ExternalCodePackageImportTests(unittest.TestCase):
    def test_import_copies_external_code_into_official_review_path(self) -> None:
        study_dir = _workspace_dir("external_import") / "PSY201"
        study_dir.mkdir()
        staging_root = _workspace_dir("external_package")
        _write_external_r_package(staging_root, "ADAE")

        result = import_external_code_package(
            ExternalCodePackageImportRequest(
                study_dir=str(study_dir),
                study_id="PSY201",
                run_id="run_external",
                dataset="ADAE",
                staging_root=str(staging_root),
                assumptions_path="assumptions.md",
            )
        )

        official_code = study_dir / "runs" / "run_external" / "code" / "build_adae.R"
        import_manifest = Path(result.manifest_path)
        package_manifest = Path(result.package_manifest_path)
        static_check = Path(result.static_check_path)

        self.assertEqual(Path(result.code_path), official_code)
        self.assertTrue(official_code.exists())
        self.assertTrue(import_manifest.exists())
        self.assertTrue(package_manifest.exists())
        self.assertTrue(static_check.exists())
        self.assertEqual(result.assumptions, ["External package assumption"])
        manifest_payload = json.loads(import_manifest.read_text(encoding="utf-8"))
        self.assertFalse(manifest_payload["official_output_created"])
        self.assertTrue(manifest_payload["review_required"])

    def test_service_import_records_existing_code_review_gate(self) -> None:
        study_dir = _workspace_dir("external_import_service") / "PSY201"
        (study_dir / "input_sdtm").mkdir(parents=True)
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        staging_root = _workspace_dir("external_package_service")
        _write_external_r_package(staging_root, "ADAE")

        response = import_external_code_package_for_dataset(
            "run_external_service",
            "ADAE",
            ImportExternalCodePackageRequest(
                study_dir=str(study_dir),
                study_id="PSY201",
                staging_root=str(staging_root),
                assumptions_path="assumptions.md",
            ),
        )
        graph_state = GraphGateway().load_graph_state(study_dir=study_dir, run_id="run_external_service")
        dataset_state = graph_state.datasets["ADAE"]

        self.assertEqual(response.status, "external_code_imported")
        self.assertEqual(dataset_state.status, "needs_review")
        self.assertEqual(dataset_state.current_interrupt.name, "code_review")
        self.assertEqual(dataset_state.code_state["status"], "generated")
        self.assertEqual(
            Path(dataset_state.code_state["code_path"]),
            study_dir / "runs" / "run_external_service" / "code" / "build_adae.R",
        )
        self.assertEqual(dataset_state.code_state["spec_source"], "external_codex_package")
        self.assertIn("external_codex_code_review_required", dataset_state.risk_flags)
        self.assertTrue(Path(response.static_check_path or "").exists())


if __name__ == "__main__":
    unittest.main()
