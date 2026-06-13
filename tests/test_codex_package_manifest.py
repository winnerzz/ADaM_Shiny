"""Tests for Codex-authored package manifests."""

from __future__ import annotations

import sys
import unittest
import uuid
from pathlib import Path

from pydantic import ValidationError

from tests.temp_workspace import test_session_root

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()

try:
    from adam_agent.schemas.codex_package import (
        CodexPackageArtifact,
        CodexPackageManifest,
        build_codex_package_manifest,
    )
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.schemas.codex_package import (
        CodexPackageArtifact,
        CodexPackageManifest,
        build_codex_package_manifest,
    )


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


def _touch(root: Path, relative_path: str) -> None:
    path = root / relative_path
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("placeholder\n", encoding="utf-8")


class CodexPackageManifestTests(unittest.TestCase):
    def test_spec_package_manifest_accepts_psy_like_handoff(self) -> None:
        root = _workspace_dir("codex_spec_manifest")
        for relative_path in [
            "specs/sdtm_spec_draft.csv",
            "specs/adam_spec_draft.csv",
            "specs/adam_build_order.csv",
            "specs/assumptions.md",
            "specs/sdtmig_extract/sdtmig_evidence.csv",
        ]:
            _touch(root, relative_path)

        manifest = build_codex_package_manifest(
            package_id="pkg_psy201_spec",
            package_type="spec_authoring",
            study_id="PSY201",
            run_id="run_codex_spec",
            target_datasets=["addm", "adae"],
            source_workspace=str(root.parent),
            staging_root=str(root),
            codex_thread_id="thread_001",
            assumptions_path="specs/assumptions.md",
            artifacts=[
                {
                    "path": "specs/sdtmig_extract/sdtmig_evidence.csv",
                    "role": "spec",
                    "required": False,
                    "description": "Optional SDTMIG extract used as evidence.",
                }
            ],
        )
        readiness = manifest.readiness()

        self.assertEqual(manifest.target_datasets, ["ADDM", "ADAE"])
        self.assertTrue(manifest.review_required)
        self.assertTrue(manifest.not_production)
        self.assertTrue(readiness.ready)
        self.assertEqual(readiness.blockers, [])
        self.assertEqual(manifest.expected_missing_paths(), [])

    def test_r_code_package_manifest_requires_core_r_files(self) -> None:
        root = _workspace_dir("codex_r_manifest")
        for relative_path in [
            "R/00_config.R",
            "R/01_helpers.R",
            "R/run_all.R",
            "report.md",
        ]:
            _touch(root, relative_path)

        manifest = build_codex_package_manifest(
            package_id="pkg_psy201_r",
            package_type="r_code_authoring",
            study_id="PSY201",
            run_id="run_codex_r",
            target_datasets=["ADDM"],
            source_workspace=str(root.parent),
            staging_root=str(root),
        )

        self.assertTrue(manifest.readiness().ready)
        self.assertEqual(
            {artifact.path for artifact in manifest.artifacts if artifact.required},
            {"R/00_config.R", "R/01_helpers.R", "R/run_all.R", "report.md"},
        )

    def test_missing_required_file_blocks_readiness(self) -> None:
        root = _workspace_dir("codex_missing_manifest")
        _touch(root, "specs/adam_spec_draft.csv")

        manifest = build_codex_package_manifest(
            package_id="pkg_missing",
            package_type="spec_authoring",
            study_id="PSY201",
            run_id="run_missing",
            target_datasets=["ADAE"],
            source_workspace=str(root.parent),
            staging_root=str(root),
        )
        readiness = manifest.readiness()

        self.assertFalse(readiness.ready)
        self.assertIn("staging_root_missing_required_files", readiness.blockers)
        self.assertIn("specs/sdtm_spec_draft.csv", readiness.missing_disk_paths)
        self.assertIn("specs/adam_build_order.csv", readiness.missing_disk_paths)
        self.assertIn("specs/assumptions.md", readiness.missing_disk_paths)

    def test_manifest_rejects_escape_paths_and_non_review_package(self) -> None:
        with self.assertRaises(ValidationError):
            CodexPackageArtifact(path="../outside.csv", role="spec")

        with self.assertRaises(ValidationError):
            CodexPackageManifest(
                package_id="pkg_bad",
                package_type="spec_authoring",
                study_id="PSY201",
                run_id="run_bad",
                target_datasets=["ADAE"],
                source_workspace=str(ROOT),
                staging_root=str(ROOT),
                review_required=False,
            )


if __name__ == "__main__":
    unittest.main()
