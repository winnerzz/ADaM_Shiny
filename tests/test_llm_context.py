"""Tests for Phase 7.4 LLM context package building."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

from tests.temp_workspace import test_session_root

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()
LOCAL_RSCRIPT = Path(r"C:\Dev\R-4.5.2\bin\Rscript.exe")

try:
    from adam_agent.llm.context import build_target_llm_context, write_llm_context_package
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.llm import LLMExposureConfig
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.llm.context import build_target_llm_context, write_llm_context_package
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.llm import LLMExposureConfig


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class LLMContextTests(unittest.TestCase):
    def test_metadata_only_context_profiles_sources_without_sample_rows(self) -> None:
        study_dir = _study_with_adae_spec_and_adsl_dependency("llm_context_metadata_only")
        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_metadata_only",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "resolution_status": "available",
                    "artifact_path": str((study_dir / "reference_adam" / "adsl.csv").as_posix()),
                    "artifact_source": "reference_adam",
                }
            ],
            source_datasets=["AE"],
        )

        payload = package.as_dict()
        self.assertEqual(payload["target_dataset"], "ADAE")
        self.assertIn("AE", payload["source_dataset_profiles"])
        self.assertNotIn("DM", payload["source_dataset_profiles"])
        self.assertEqual(payload["source_dataset_profiles"]["AE"]["sample_rows"], [])
        self.assertEqual(payload["source_dataset_profiles"]["AE"]["read_path"], "../../input_sdtm/ae.csv")
        self.assertEqual(payload["resolved_dependencies"], {})
        self.assertTrue(any("Reference ADaM ADSL is available only for comparison" in warning for warning in payload["warnings"]))
        self.assertEqual(payload["target_spec"]["json"]["dataset"], "ADAE")
        self.assertEqual(payload["runtime_contract"]["language"], "R")
        self.assertEqual(payload["runtime_contract"]["runtime_output_path"], "outputs/adae.csv")
        self.assertIn("colClasses", payload["runtime_contract"]["csv_read_policy"])
        self.assertTrue(payload["runtime_contract"]["write_only_to_run_dir"])

    def test_demo_rich_context_includes_configured_sample_rows(self) -> None:
        study_dir = _study_with_adae_spec_and_adsl_dependency("llm_context_demo_rows")
        exposure = LLMExposureConfig(
            mode="demo_rich_context",
            data_classification="processed_demo",
            external_api_allowed=True,
            sample_rows_per_dataset=1,
        )

        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_demo_rows",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "resolution_status": "available",
                    "artifact_path": str((study_dir / "reference_adam" / "adsl.csv").as_posix()),
                    "artifact_source": "reference_adam",
                }
            ],
            exposure=exposure,
            source_datasets=["AE"],
        )

        payload = package.as_dict()
        self.assertEqual(payload["exposure"]["mode"], "demo_rich_context")
        self.assertEqual(payload["source_dataset_profiles"]["AE"]["sample_rows"], [{"USUBJID": "01", "AETERM": "HEADACHE"}])
        self.assertEqual(payload["resolved_dependencies"], {})
        self.assertTrue(any("Reference ADaM ADSL is available only for comparison" in warning for warning in payload["warnings"]))

    def test_context_warns_on_missing_dependency_artifact_and_missing_spec(self) -> None:
        study_dir = _workspace_dir("llm_context_missing_dependency") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        (input_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")

        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_missing_dependency",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "resolution_status": "available",
                    "artifact_path": str((study_dir / "reference_adam" / "missing_adsl.csv").as_posix()),
                    "artifact_source": "reference_adam",
                }
            ],
        )

        self.assertEqual(package.resolved_dependencies, {})
        self.assertIsNone(package.target_spec)
        self.assertTrue(any("Reference ADaM ADSL is available only for comparison" in warning for warning in package.warnings))
        self.assertTrue(any("No input_spec artifact" in warning for warning in package.warnings))

    def test_context_uses_only_dependency_records_for_current_target(self) -> None:
        study_dir = _study_with_adae_spec_and_adsl_dependency("llm_context_target_filter")
        (study_dir / "reference_adam" / "adlb.csv").write_text("USUBJID,PARAMCD\n01,ALT\n", encoding="utf-8")

        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_target_filter",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "resolution_status": "available",
                    "artifact_path": str((study_dir / "reference_adam" / "adsl.csv").as_posix()),
                    "artifact_source": "reference_adam",
                },
                {
                    "target_dataset": "ADTTE",
                    "required_dataset": "ADLB",
                    "resolution_status": "available",
                    "artifact_path": str((study_dir / "reference_adam" / "adlb.csv").as_posix()),
                    "artifact_source": "reference_adam",
                },
            ],
            source_datasets=["AE"],
        )

        self.assertEqual(package.resolved_dependencies, {})
        self.assertNotIn("ADLB", package.resolved_dependencies)

    def test_context_exposes_run_output_dependency_as_runtime_input(self) -> None:
        study_dir = _study_with_adae_spec_and_adsl_dependency("llm_context_run_output_dependency")
        run_output = study_dir / "runs" / "run_context_output_dependency" / "outputs"
        run_output.mkdir(parents=True)
        (run_output / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")

        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_output_dependency",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[
                {
                    "target_dataset": "ADAE",
                    "required_dataset": "ADSL",
                    "resolution_status": "available",
                    "artifact_path": str((run_output / "adsl.csv").as_posix()),
                    "artifact_source": "run_output",
                }
            ],
            source_datasets=["AE"],
        )

        self.assertIn("ADSL", package.resolved_dependencies)
        self.assertEqual(package.resolved_dependencies["ADSL"]["read_path"], "outputs/adsl.csv")

    def test_context_selects_ads_full_spec_filename(self) -> None:
        study_dir = _workspace_dir("llm_context_ads_full_spec") / "demo_adam"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_adam = study_dir / "reference_adam"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_adam.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "ads_adae_full.csv").write_text(
            "Dataset,Variable,Label,Type,Source,Derivation\n"
            "ADAE,AETERM,Reported Term,Copied,SDTM.AE.AETERM,Copied from source\n",
            encoding="utf-8",
        )

        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_ads_full_spec",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[],
        )

        self.assertIsNotNone(package.target_spec)
        self.assertTrue(package.target_spec["path"].endswith("ads_adae_full.csv"))

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_context_profiles_sas7bdat_with_explicit_rscript_path(self) -> None:
        study_dir = _workspace_dir("llm_context_sas7bdat_rscript") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_sdtm.mkdir(parents=True)
        create_script = study_dir / "create_sas7bdat.R"
        create_script.write_text(
            f"""
if (!requireNamespace("haven", quietly = TRUE)) {{
  stop("haven is required for this test")
}}
ae <- data.frame(USUBJID = c("01", "02"), AETERM = c("HEADACHE", "NAUSEA"))
haven::write_sas(ae, "{(input_sdtm / 'ae.sas7bdat').as_posix()}")
""",
            encoding="utf-8",
        )
        import subprocess

        subprocess.run([str(LOCAL_RSCRIPT), str(create_script)], check=True, capture_output=True, text=True)
        exposure = LLMExposureConfig(
            mode="demo_rich_context",
            data_classification="processed_demo",
            external_api_allowed=True,
            sample_rows_per_dataset=1,
        )

        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_sas7bdat_rscript",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[],
            exposure=exposure,
            source_datasets=["AE"],
            rscript_path=str(LOCAL_RSCRIPT),
        )

        profile = package.source_dataset_profiles["AE"]
        self.assertEqual(profile["status"], "ok")
        self.assertEqual(profile["format"], "sas7bdat")
        self.assertEqual(profile["columns"], ["USUBJID", "AETERM"])
        self.assertEqual(profile["sample_rows"], [{"USUBJID": "01", "AETERM": "HEADACHE"}])
        self.assertFalse(any("Rscript with the R package 'haven'" in warning for warning in package.warnings))

    def test_write_llm_context_package_creates_audit_artifact(self) -> None:
        study_dir = _study_with_adae_spec_and_adsl_dependency("llm_context_write")
        package = build_target_llm_context(
            study_id="PSY201",
            run_id="run_context_write",
            target_dataset="ADAE",
            study_dir=study_dir,
            dependency_resolution=[],
        )

        artifact = write_llm_context_package(package, study_dir)

        self.assertIsInstance(artifact, ArtifactRef)
        self.assertEqual(artifact.kind, "llm_prompt")
        self.assertEqual(artifact.role, "audit")
        self.assertTrue(Path(artifact.path).exists())
        payload = json.loads(Path(artifact.path).read_text(encoding="utf-8"))
        self.assertEqual(payload["target_dataset"], "ADAE")


def _study_with_adae_spec_and_adsl_dependency(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    input_spec = study_dir / "input_spec"
    reference_adam = study_dir / "reference_adam"
    input_sdtm.mkdir(parents=True)
    input_spec.mkdir()
    reference_adam.mkdir()
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")
    (input_sdtm / "dm.csv").write_text("USUBJID,AGE\n01,34\n02,41\n", encoding="utf-8")
    (input_spec / "adae.json").write_text(
        json.dumps(
            {
                "dataset": "ADAE",
                "variables": [
                    {
                        "variable": "AETERM",
                        "source_domains": ["AE"],
                        "derivation": "Copy AE.AETERM.",
                    }
                ],
            }
        ),
        encoding="utf-8",
    )
    (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n02,2024-01-02\n", encoding="utf-8")
    return study_dir


if __name__ == "__main__":
    unittest.main()
