"""Tests for the Phase 5 ADSL minimal real-loop building blocks."""

from __future__ import annotations

import sys
import csv
import json
import subprocess
import unittest
import uuid
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"
LOCAL_RSCRIPT = Path(r"C:\Dev\R-4.5.2\bin\Rscript.exe")

try:
    from adam_agent.adsl.diagnostics import diagnose_adsl_failure
    from adam_agent.adsl.r_template import render_build_adsl_r
    from adam_agent.adsl.runner import run_adsl_minimal
    from adam_agent.adsl.spec_builder import build_starter_adsl_spec, create_demo_approved_spec
    from adam_agent.adsl.validator import validate_adsl_csv, write_validation_report
    from adam_agent.tools.r_runner import LocalRRunner, RRunRequest
    from adam_agent.tools.sdtm_reader import DatasetProfile, SDTMReader
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.adsl.diagnostics import diagnose_adsl_failure
    from adam_agent.adsl.r_template import render_build_adsl_r
    from adam_agent.adsl.runner import run_adsl_minimal
    from adam_agent.adsl.spec_builder import build_starter_adsl_spec, create_demo_approved_spec
    from adam_agent.adsl.validator import validate_adsl_csv, write_validation_report
    from adam_agent.tools.r_runner import LocalRRunner, RRunRequest
    from adam_agent.tools.sdtm_reader import DatasetProfile, SDTMReader


def workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class Phase5AdslLoopTests(unittest.TestCase):
    def test_diagnose_adsl_failure_classifies_missing_source_variable_as_spec_error(self) -> None:
        r_result = LocalRRunner(rscript_path=None).run(
            RRunRequest(
                code="",
                dataset="ADSL",
                run_id="run_diag",
                working_dir=str(workspace_dir("diag_missing_source")),
                script_path="missing.R",
            )
        )
        r_result.stderr = "Error in eval(predvars, data, env): object 'EXSTDTC' not found"

        record = diagnose_adsl_failure(stage="run_or_validate", r_result=r_result)

        self.assertEqual(record.failure_type, "spec_error")
        self.assertEqual(record.failure_id, "failure_adsl_source_variable_missing")
        self.assertEqual(record.root_cause, "source_variable_missing")
        self.assertEqual(record.recommended_route, "revise_spec")

    def test_diagnose_adsl_failure_routes_key_validation_to_human_review(self) -> None:
        record = diagnose_adsl_failure(
            stage="run_or_validate",
            validation_report={
                "dataset": "ADSL",
                "status": "fail",
                "errors": ["USUBJID is not unique"],
            },
        )

        self.assertEqual(record.failure_type, "validation_error")
        self.assertEqual(record.root_cause, "key_integrity_error")
        self.assertEqual(record.recommended_route, "human_review")

    def test_starter_spec_marks_treatment_and_safety_candidates_review_required(self) -> None:
        dm_profile = DatasetProfile(
            dataset="DM",
            path="dm.csv",
            format="csv",
            status="ok",
            columns=["STUDYID", "USUBJID", "AGE", "SEX", "ARM"],
        )
        ex_profile = DatasetProfile(
            dataset="EX",
            path="ex.csv",
            format="csv",
            status="ok",
            columns=["USUBJID", "EXSTDTC", "EXENDTC"],
        )

        result = build_starter_adsl_spec(dm_profile, ex_profile)
        variables = {variable.variable: variable for variable in result.draft_spec.variables}

        self.assertEqual(result.draft_spec.status, "draft")
        self.assertIn("USUBJID", variables)
        self.assertFalse(variables["USUBJID"].review_required)
        for variable in ["TRTSDT", "TRTEDT", "SAFFL"]:
            self.assertTrue(variables[variable].review_required)
            self.assertEqual(variables[variable].risk_level, "high")
            self.assertIn("production", " ".join(variables[variable].assumptions).lower())

    def test_demo_approval_records_that_starter_rules_are_not_production_approved(self) -> None:
        result = build_starter_adsl_spec(
            DatasetProfile(dataset="DM", path="dm.csv", format="csv", status="ok", columns=["USUBJID"]),
            DatasetProfile(dataset="EX", path="ex.csv", format="csv", status="ok", columns=["USUBJID"]),
        )

        approved_spec, approval = create_demo_approved_spec(result.draft_spec)

        self.assertEqual(approved_spec.status, "approved")
        self.assertEqual(approval.approval_mode, "demo_only_no_review")
        self.assertIn("not production-approved", approval.notes)
        self.assertTrue(all(variable.approval_status == "approved" for variable in approved_spec.variables))

    def test_rendered_r_code_contains_io_paths_and_caveat(self) -> None:
        code = render_build_adsl_r(
            dm_path="studies/PSY201/input_sdtm/dm.csv",
            ex_path="studies/PSY201/input_sdtm/ex.csv",
            output_path="studies/PSY201/runs/run_001/outputs/adsl.csv",
        )

        self.assertIn("read.csv", code)
        self.assertIn("haven::read_sas", code)
        self.assertIn("not a production ADaM program", code)
        self.assertIn("Production SAFFL must come from study evidence", code)
        self.assertIn("adsl.csv", code)

    def test_local_r_runner_reports_missing_rscript_without_fake_success(self) -> None:
        tmp = workspace_dir("local_r_runner_missing")
        runner = LocalRRunner(rscript_path=None)
        runner.rscript_path = None

        result = runner.run(
            RRunRequest(
                code="print('hello')",
                dataset="ADSL",
                run_id="run_001",
                working_dir=str(tmp),
            )
        )

        self.assertFalse(result.success)
        self.assertEqual(result.exit_code, 127)
        self.assertIn("Rscript is not available", result.stderr)

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_local_r_runner_executes_rendered_adsl_script_when_rscript_available(self) -> None:
        tmp = workspace_dir("local_r_runner_real")
        input_dir = tmp / "input_sdtm"
        output_dir = tmp / "runs" / "run_001" / "outputs"
        input_dir.mkdir(parents=True)
        dm = input_dir / "dm.csv"
        ex = input_dir / "ex.csv"
        adsl = output_dir / "adsl.csv"
        dm.write_text(
            "STUDYID,USUBJID,AGE,SEX,ARM\nS1,01,34,F,Test Drug\nS1,02,41,M,Placebo\n",
            encoding="utf-8",
        )
        ex.write_text(
            "USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n",
            encoding="utf-8",
        )
        code = render_build_adsl_r(dm_path=dm, ex_path=ex, output_path=adsl)

        result = LocalRRunner(rscript_path=str(LOCAL_RSCRIPT)).run(
            RRunRequest(
                code=code,
                dataset="ADSL",
                run_id="run_001",
                working_dir=str(tmp),
                script_path=str(tmp / "runs" / "run_001" / "code" / "build_adsl.R"),
            )
        )

        self.assertTrue(result.success, result.stderr)
        self.assertTrue(adsl.exists())
        report = validate_adsl_csv(adsl, required_columns=["USUBJID", "TRTSDT", "TRTEDT", "SAFFL"])
        self.assertEqual(report["status"], "pass")
        with adsl.open("r", encoding="utf-8", newline="") as handle:
            rows = list(csv.DictReader(handle))
        self.assertEqual([row["USUBJID"] for row in rows], ["01", "02"])

    def test_adsl_validator_passes_valid_output_and_writes_report(self) -> None:
        tmp = workspace_dir("adsl_validator_pass")
        adsl = tmp / "adsl.csv"
        adsl.write_text(
            "USUBJID,TRTSDT,TRTEDT,SAFFL\n01,2024-01-01,2024-01-05,Y\n02,,,N\n",
            encoding="utf-8",
        )

        report = validate_adsl_csv(adsl, required_columns=["USUBJID", "SAFFL"])
        report_path = write_validation_report(report, tmp / "adsl_validation_report.json")

        self.assertEqual(report["status"], "pass")
        self.assertTrue(report_path.exists())
        self.assertIn("study-specific", " ".join(report["warnings"]))

    def test_adsl_validator_fails_duplicate_usubjid_and_bad_saffl(self) -> None:
        tmp = workspace_dir("adsl_validator_fail")
        adsl = tmp / "adsl.csv"
        adsl.write_text(
            "USUBJID,TRTSDT,TRTEDT,SAFFL\n01,2024-01-06,2024-01-05,MAYBE\n01,,,Y\n",
            encoding="utf-8",
        )

        report = validate_adsl_csv(adsl)

        self.assertEqual(report["status"], "fail")
        self.assertTrue(any("USUBJID is not unique" in error for error in report["errors"]))
        self.assertTrue(any("SAFFL contains unexpected values" in error for error in report["errors"]))

    def test_csv_profiles_can_feed_starter_spec(self) -> None:
        tmp = workspace_dir("profiles_feed_spec")
        dm = tmp / "dm.csv"
        ex = tmp / "ex.csv"
        dm.write_text("USUBJID,AGE,SEX\n01,34,F\n", encoding="utf-8")
        ex.write_text("USUBJID,EXSTDTC\n01,2024-01-01\n", encoding="utf-8")

        reader = SDTMReader()
        result = build_starter_adsl_spec(reader.profile(dm), reader.profile(ex))
        variable_names = [variable.variable for variable in result.draft_spec.variables]

        self.assertIn("USUBJID", variable_names)
        self.assertIn("TRTSDT", variable_names)
        self.assertIn("SAFFL", variable_names)

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_run_adsl_minimal_writes_run_artifacts_and_manifest(self) -> None:
        study_dir = workspace_dir("adsl_minimal_service") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        (study_dir / "reference_adam").mkdir()
        (study_dir / "input_spec").mkdir()
        (study_dir / "input_define").mkdir()
        (study_dir / "legacy_code").mkdir()
        (study_dir / "runs").mkdir()
        (input_dir / "dm.csv").write_text(
            "STUDYID,USUBJID,AGE,SEX,RACE,ARM\nS1,01,34,F,WHITE,Test Drug\nS1,02,41,M,BLACK,Placebo\n",
            encoding="utf-8",
        )
        (input_dir / "ex.csv").write_text(
            "USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n",
            encoding="utf-8",
        )

        result = run_adsl_minimal(
            study_dir,
            run_id="run_001",
            rscript_path=str(LOCAL_RSCRIPT),
        )
        run_dir = study_dir / "runs" / "run_001"

        self.assertEqual(result.status, "completed")
        self.assertTrue((run_dir / "specs" / "adsl_draft_spec.json").exists())
        self.assertTrue((run_dir / "specs" / "adsl_approved_spec.json").exists())
        self.assertTrue((run_dir / "code" / "build_adsl.R").exists())
        self.assertTrue((run_dir / "outputs" / "adsl.csv").exists())
        self.assertTrue((run_dir / "validation" / "adsl_validation_report.json").exists())
        self.assertTrue((run_dir / "compare" / "adsl_compare_report.json").exists())
        self.assertTrue((run_dir / "audit" / "manifest.json").exists())
        self.assertEqual(result.validation_report["status"], "pass")
        self.assertIn("output_adsl", result.artifacts)
        manifest_text = (run_dir / "audit" / "manifest.json").read_text(encoding="utf-8")
        self.assertIn("adsl_output_csv", manifest_text)
        self.assertIn("phase5_adsl_minimal", manifest_text)

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_run_adsl_minimal_reads_sas7bdat_inputs_through_r_haven(self) -> None:
        study_dir = workspace_dir("adsl_minimal_sas7bdat") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        for folder in ["reference_adam", "input_spec", "input_define", "legacy_code", "runs"]:
            (study_dir / folder).mkdir()
        create_sas = f"""
        if (!requireNamespace("haven", quietly = TRUE)) stop("haven required")
        dm <- data.frame(
          STUDYID = c("S1", "S1"),
          USUBJID = c("01", "02"),
          AGE = c(34, 41),
          SEX = c("F", "M"),
          stringsAsFactors = FALSE
        )
        ex <- data.frame(
          USUBJID = c("01"),
          EXSTDTC = c("2024-01-01"),
          EXENDTC = c("2024-01-05"),
          stringsAsFactors = FALSE
        )
        haven::write_sas(dm, "{(input_dir / 'dm.sas7bdat').as_posix()}")
        haven::write_sas(ex, "{(input_dir / 'ex.sas7bdat').as_posix()}")
        """
        create_script = study_dir / "create_sas7bdat.R"
        create_script.write_text(create_sas, encoding="utf-8")
        completed = subprocess.run(
            [str(LOCAL_RSCRIPT), str(create_script)],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)

        result = run_adsl_minimal(
            study_dir,
            run_id="run_sas7bdat",
            rscript_path=str(LOCAL_RSCRIPT),
        )

        adsl = study_dir / "runs" / "run_sas7bdat" / "outputs" / "adsl.csv"
        self.assertEqual(result.status, "completed")
        self.assertTrue(adsl.exists())
        self.assertEqual(result.validation_report["status"], "pass")
        self.assertIn("output_adsl", result.artifacts)
        self.assertEqual(result.artifacts["output_adsl"].artifact_id, "adsl_output_csv")

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_sas7bdat_profile_does_not_invent_missing_date_columns_in_spec(self) -> None:
        study_dir = workspace_dir("adsl_minimal_sas7bdat_missing_dates") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        for folder in ["reference_adam", "input_spec", "input_define", "legacy_code", "runs"]:
            (study_dir / folder).mkdir()
        create_sas = f"""
        if (!requireNamespace("haven", quietly = TRUE)) stop("haven required")
        dm <- data.frame(
          STUDYID = c("S1", "S1"),
          USUBJID = c("01", "02"),
          stringsAsFactors = FALSE
        )
        ex <- data.frame(
          USUBJID = c("01"),
          stringsAsFactors = FALSE
        )
        haven::write_sas(dm, "{(input_dir / 'dm.sas7bdat').as_posix()}")
        haven::write_sas(ex, "{(input_dir / 'ex.sas7bdat').as_posix()}")
        """
        create_script = study_dir / "create_sas7bdat_missing_dates.R"
        create_script.write_text(create_sas, encoding="utf-8")
        completed = subprocess.run(
            [str(LOCAL_RSCRIPT), str(create_script)],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)

        result = run_adsl_minimal(
            study_dir,
            run_id="run_sas7bdat_missing_dates",
            rscript_path=str(LOCAL_RSCRIPT),
        )

        self.assertEqual(result.status, "completed")
        draft_spec_path = study_dir / "runs" / "run_sas7bdat_missing_dates" / "specs" / "adsl_draft_spec.json"
        draft_spec = json.loads(draft_spec_path.read_text(encoding="utf-8"))
        variable_names = {variable["variable"] for variable in draft_spec["variables"]}
        self.assertIn("SAFFL", variable_names)
        self.assertNotIn("TRTSDT", variable_names)
        self.assertNotIn("TRTEDT", variable_names)

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_cli_run_adsl_minimal_outputs_summary_json(self) -> None:
        study_dir = workspace_dir("adsl_minimal_cli") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        for folder in ["reference_adam", "input_spec", "input_define", "legacy_code", "runs"]:
            (study_dir / folder).mkdir()
        (input_dir / "dm.csv").write_text(
            "STUDYID,USUBJID,AGE,SEX\nS1,01,34,F\n",
            encoding="utf-8",
        )
        (input_dir / "ex.csv").write_text(
            "USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n",
            encoding="utf-8",
        )

        completed = subprocess.run(
            [
                sys.executable,
                "-m",
                "adam_agent.cli",
                "run-adsl-minimal",
                "--study-dir",
                str(study_dir),
                "--run-id",
                "run_cli",
                "--rscript-path",
                str(LOCAL_RSCRIPT),
            ],
            cwd=str(ROOT),
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertIn('"status": "completed"', completed.stdout)
        self.assertTrue((study_dir / "runs" / "run_cli" / "outputs" / "adsl.csv").exists())

    def test_cli_run_adsl_minimal_returns_structured_failure_for_missing_inputs(self) -> None:
        study_dir = workspace_dir("adsl_minimal_cli_missing_inputs") / "PSY201"
        study_dir.mkdir(parents=True)

        completed = subprocess.run(
            [
                sys.executable,
                "-m",
                "adam_agent.cli",
                "run-adsl-minimal",
                "--study-dir",
                str(study_dir),
                "--run-id",
                "run_cli_missing",
            ],
            cwd=str(ROOT),
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(completed.returncode, 1)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["status"], "failed")
        self.assertEqual(payload["failure_type"], "input_error")
        self.assertEqual(payload["root_cause"], "missing_required_input")
        self.assertEqual(payload["recommended_route"], "fail")
        self.assertEqual(payload["failure_id"], "failure_adsl_missing_required_input")
        self.assertTrue((study_dir / "runs" / "run_cli_missing" / "diagnostics" / "adsl_failure_report.json").exists())

    def test_run_adsl_minimal_writes_failure_report_for_missing_inputs(self) -> None:
        study_dir = workspace_dir("adsl_minimal_missing_inputs") / "PSY201"
        study_dir.mkdir(parents=True)

        result = run_adsl_minimal(study_dir, run_id="run_missing_inputs")

        self.assertEqual(result.status, "failed")
        self.assertIsNotNone(result.failure_record)
        self.assertEqual(result.failure_record.failure_id, "failure_adsl_missing_required_input")
        self.assertEqual(result.failure_record.failure_type, "input_error")
        self.assertEqual(result.failure_record.root_cause, "missing_required_input")
        self.assertEqual(result.failure_record.recommended_route, "fail")
        self.assertIn("failure_report", result.artifacts)
        failure_report = study_dir / "runs" / "run_missing_inputs" / "diagnostics" / "adsl_failure_report.json"
        manifest = study_dir / "runs" / "run_missing_inputs" / "audit" / "manifest.json"
        self.assertTrue(failure_report.exists())
        manifest_text = manifest.read_text(encoding="utf-8")
        self.assertIn("adsl_failure_report", manifest_text)
        self.assertIn("missing_required_input", manifest_text)

    def test_run_adsl_minimal_writes_failure_report_for_spec_build_failure(self) -> None:
        study_dir = workspace_dir("adsl_minimal_spec_failure") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        (input_dir / "dm.csv").write_text("STUDYID,AGE\nS1,34\n", encoding="utf-8")
        (input_dir / "ex.csv").write_text("USUBJID,EXSTDTC\n01,2024-01-01\n", encoding="utf-8")

        result = run_adsl_minimal(study_dir, run_id="run_spec_failure")

        self.assertEqual(result.status, "failed")
        self.assertIsNotNone(result.failure_record)
        self.assertEqual(result.failure_record.failure_type, "spec_error")
        self.assertEqual(result.failure_record.root_cause, "source_variable_missing")
        self.assertEqual(result.failure_record.recommended_route, "revise_spec")
        failure_report = study_dir / "runs" / "run_spec_failure" / "diagnostics" / "adsl_failure_report.json"
        manifest = study_dir / "runs" / "run_spec_failure" / "audit" / "manifest.json"
        self.assertTrue(failure_report.exists())
        manifest_text = manifest.read_text(encoding="utf-8")
        self.assertIn("failure_adsl_source_variable_missing", manifest_text)
        self.assertIn("revise_spec", manifest_text)

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_run_adsl_minimal_writes_failure_report_for_profile_failure(self) -> None:
        study_dir = workspace_dir("adsl_minimal_profile_failure") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        (input_dir / "dm.sas7bdat").write_text("not a sas dataset", encoding="utf-8")
        (input_dir / "ex.sas7bdat").write_text("not a sas dataset", encoding="utf-8")

        result = run_adsl_minimal(study_dir, run_id="run_profile_failure", rscript_path=str(LOCAL_RSCRIPT))

        self.assertEqual(result.status, "failed")
        self.assertIsNotNone(result.failure_record)
        self.assertEqual(result.failure_record.failure_type, "input_error")
        self.assertEqual(result.failure_record.root_cause, "unsupported_or_unreadable_input")
        failure_report = study_dir / "runs" / "run_profile_failure" / "diagnostics" / "adsl_failure_report.json"
        manifest = study_dir / "runs" / "run_profile_failure" / "audit" / "manifest.json"
        self.assertTrue(failure_report.exists())
        self.assertIn("unsupported_or_unreadable_input", manifest.read_text(encoding="utf-8"))

    def test_run_adsl_minimal_writes_failure_report_for_r_failure(self) -> None:
        study_dir = workspace_dir("adsl_minimal_r_failure") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        (input_dir / "dm.csv").write_text("USUBJID\n01\n", encoding="utf-8")
        (input_dir / "ex.csv").write_text("USUBJID\n01\n", encoding="utf-8")

        result = run_adsl_minimal(study_dir, run_id="run_r_failure", rscript_path="C:/missing/Rscript.exe")

        self.assertEqual(result.status, "failed")
        self.assertIsNotNone(result.failure_record)
        self.assertEqual(result.failure_record.failure_type, "sandbox_error")
        self.assertEqual(result.failure_record.root_cause, "r_runtime_error")
        failure_report = study_dir / "runs" / "run_r_failure" / "diagnostics" / "adsl_failure_report.json"
        manifest = study_dir / "runs" / "run_r_failure" / "audit" / "manifest.json"
        self.assertTrue(failure_report.exists())
        self.assertIn("r_runtime_error", manifest.read_text(encoding="utf-8"))

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_run_adsl_minimal_writes_failure_report_for_validation_failure(self) -> None:
        study_dir = workspace_dir("adsl_minimal_validation_failure") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        (input_dir / "dm.csv").write_text("USUBJID\n01\n", encoding="utf-8")
        (input_dir / "ex.csv").write_text("USUBJID\n01\n", encoding="utf-8")

        forced_report = {
            "dataset": "ADSL",
            "status": "fail",
            "checks": [],
            "warnings": [],
            "errors": ["USUBJID is not unique"],
        }
        with patch("adam_agent.adsl.runner.validate_adsl_csv", return_value=forced_report):
            result = run_adsl_minimal(study_dir, run_id="run_validation_failure", rscript_path=str(LOCAL_RSCRIPT))

        self.assertEqual(result.status, "failed")
        self.assertIsNotNone(result.failure_record)
        self.assertEqual(result.failure_record.failure_type, "validation_error")
        self.assertEqual(result.failure_record.root_cause, "key_integrity_error")
        self.assertEqual(result.failure_record.recommended_route, "human_review")
        failure_report = study_dir / "runs" / "run_validation_failure" / "diagnostics" / "adsl_failure_report.json"
        manifest = study_dir / "runs" / "run_validation_failure" / "audit" / "manifest.json"
        self.assertTrue(failure_report.exists())
        self.assertIn("key_integrity_error", manifest.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
