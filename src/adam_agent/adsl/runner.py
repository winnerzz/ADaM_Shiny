"""Orchestrate the Phase 5 ADSL minimal real loop."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from adam_agent.adsl.diagnostics import diagnose_adsl_failure, write_failure_report
from adam_agent.adsl.r_template import render_build_adsl_r
from adam_agent.adsl.spec_builder import build_starter_adsl_spec, create_demo_approved_spec
from adam_agent.adsl.validator import validate_adsl_csv, write_validation_report
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.tools.artifacts import ArtifactStore
from adam_agent.tools.r_runner import LocalRRunner, RRunRequest, RRunResult
from adam_agent.tools.sdtm_reader import DatasetProfile, SDTMReader
from adam_agent.tools.study_inputs import StudyInputIndex, StudyInputScanner


@dataclass
class AdslRunResult:
    """Summary of one Phase 5 ADSL minimal run."""

    study_id: str
    run_id: str
    status: str
    run_dir: str
    r_result: RRunResult
    validation_report: dict[str, Any]
    artifacts: dict[str, ArtifactRef]
    manifest: ArtifactRef
    warnings: list[str]
    failure_record: FailureRecord | None = None


def run_adsl_minimal(
    study_dir: str | Path,
    *,
    run_id: str,
    rscript_path: str | None = None,
    study_id: str | None = None,
    manifest_name: str = "manifest.json",
) -> AdslRunResult:
    """Run the Phase 5 ADSL starter loop for a local study folder."""

    study_path = Path(study_dir)
    resolved_study_id = study_id or study_path.name
    scanner = StudyInputScanner(study_path, study_id=resolved_study_id)
    index = scanner.scan()
    warnings = list(index.warnings)

    store = ArtifactStore(study_path, study_id=resolved_study_id, run_id=run_id, manifest_name=manifest_name)
    run_dir = study_path / "runs" / run_id
    specs_dir = run_dir / "specs"
    code_dir = run_dir / "code"
    outputs_dir = run_dir / "outputs"
    validation_dir = run_dir / "validation"
    compare_dir = run_dir / "compare"
    profile_dir = run_dir / "profile"
    diagnostics_dir = run_dir / "diagnostics"
    for folder in [specs_dir, code_dir, outputs_dir, validation_dir, compare_dir, profile_dir, diagnostics_dir]:
        folder.mkdir(parents=True, exist_ok=True)
    failure_report_path = diagnostics_dir / "adsl_failure_report.json"

    try:
        dm_artifact = _require_input(index, "DM")
        ex_artifact = _require_input(index, "EX")
    except ValueError as exc:
        return _failed_run_result(
            store=store,
            failure_report_path=failure_report_path,
            study_id=resolved_study_id,
            run_id=run_id,
            run_dir=run_dir,
            stage="scan_inputs",
            message=str(exc),
            warnings=warnings,
        )

    store.add_ref(dm_artifact)
    store.add_ref(ex_artifact)

    reader = SDTMReader()
    dm_profile = reader.profile(dm_artifact.path, dataset="DM")
    ex_profile = reader.profile(ex_artifact.path, dataset="EX")
    dm_profile = _profile_or_r_runtime_profile(dm_profile, rscript_path=rscript_path, profile_dir=profile_dir)
    ex_profile = _profile_or_r_runtime_profile(ex_profile, rscript_path=rscript_path, profile_dir=profile_dir)
    if dm_profile.status != "ok" or ex_profile.status != "ok":
        message = (
            "DM/EX profiling failed or is unsupported: "
            f"DM={dm_profile.status} {dm_profile.message}; "
            f"EX={ex_profile.status} {ex_profile.message}"
        )
        return _failed_run_result(
            store=store,
            failure_report_path=failure_report_path,
            study_id=resolved_study_id,
            run_id=run_id,
            run_dir=run_dir,
            stage="profile_inputs",
            message=message,
            warnings=warnings,
        )

    try:
        spec_result = build_starter_adsl_spec(dm_profile, ex_profile)
    except ValueError as exc:
        return _failed_run_result(
            store=store,
            failure_report_path=failure_report_path,
            study_id=resolved_study_id,
            run_id=run_id,
            run_dir=run_dir,
            stage="draft_spec",
            message=str(exc),
            warnings=warnings,
        )
    approved_spec, approval = create_demo_approved_spec(spec_result.draft_spec)

    draft_spec_path = specs_dir / "adsl_draft_spec.json"
    approved_spec_path = specs_dir / "adsl_approved_spec.json"
    approval_path = specs_dir / "adsl_approval_record.json"
    code_path = code_dir / "build_adsl.R"
    output_path = outputs_dir / "adsl.csv"
    validation_path = validation_dir / "adsl_validation_report.json"
    compare_path = compare_dir / "adsl_compare_report.json"

    _write_json(draft_spec_path, spec_result.draft_spec.model_dump(mode="json"))
    _write_json(approved_spec_path, approved_spec.model_dump(mode="json"))
    _write_json(approval_path, approval.model_dump(mode="json"))

    code = render_build_adsl_r(
        dm_path=dm_artifact.path,
        ex_path=ex_artifact.path,
        output_path=output_path,
    )
    code_path.write_text(code, encoding="utf-8")

    r_result = LocalRRunner(rscript_path=rscript_path).run(
        RRunRequest(
            code="",
            dataset="ADSL",
            run_id=run_id,
            working_dir=str(study_path),
            script_path=str(code_path),
        )
    )

    if r_result.success:
        validation_report = validate_adsl_csv(
            output_path,
            required_columns=["USUBJID", "SAFFL"],
        )
    else:
        validation_report = {
            "dataset": "ADSL",
            "status": "not_run",
            "checks": [],
            "warnings": [],
            "errors": [r_result.stderr],
        }
    write_validation_report(validation_report, validation_path)
    _write_json(compare_path, {"dataset": "ADSL", "status": "skipped", "reason": "No reference ADSL comparison is implemented in Phase 5 starter service."})

    artifacts: dict[str, ArtifactRef] = {
        "draft_spec": store.register_existing(
            draft_spec_path,
            artifact_id="adsl_draft_spec",
            kind="draft_spec",
            role="intermediate",
            dataset="ADSL",
        ),
        "approved_spec": store.register_existing(
            approved_spec_path,
            artifact_id="adsl_approved_spec",
            kind="approved_spec",
            role="output",
            dataset="ADSL",
        ),
        "approval_record": store.register_existing(
            approval_path,
            artifact_id="adsl_approval_record",
            kind="tool_log",
            role="audit",
            dataset="ADSL",
        ),
        "generated_code": store.register_existing(
            code_path,
            artifact_id="adsl_generated_code",
            kind="generated_code",
            role="output",
            dataset="ADSL",
            format="R",
        ),
        "validation_report": store.register_existing(
            validation_path,
            artifact_id="adsl_validation_report",
            kind="validation_report",
            role="output",
            dataset="ADSL",
        ),
        "compare_report": store.register_existing(
            compare_path,
            artifact_id="adsl_compare_report",
            kind="compare_report",
            role="output",
            dataset="ADSL",
        ),
    }
    failure_record: FailureRecord | None = None
    if not r_result.success or validation_report["status"] != "pass":
        failure_record = diagnose_adsl_failure(
            stage="run_or_validate",
            r_result=r_result,
            validation_report=validation_report,
            artifact_ids=["adsl_generated_code", "adsl_validation_report"],
        )
        write_failure_report(failure_record, failure_report_path)
        artifacts["failure_report"] = store.register_existing(
            failure_report_path,
            artifact_id="adsl_failure_report",
            kind="tool_log",
            role="audit",
            dataset="ADSL",
        )
    if output_path.exists():
        artifacts["output_adsl"] = store.register_existing(
            output_path,
            artifact_id="adsl_output_csv",
            kind="output_adam",
            role="output",
            dataset="ADSL",
        )

    manifest = store.write_manifest(
        extra={
            "dataset": "ADSL",
            "phase": "phase5_adsl_minimal",
            "r_exit_code": r_result.exit_code,
            "validation_status": validation_report["status"],
            "failure_id": failure_record.failure_id if failure_record else None,
            "failure_type": failure_record.failure_type if failure_record else None,
            "root_cause": failure_record.root_cause if failure_record else None,
            "recommended_route": failure_record.recommended_route if failure_record else None,
            "warnings": warnings,
        }
    )
    status = "completed" if r_result.success and validation_report["status"] == "pass" else "failed"
    return AdslRunResult(
        study_id=resolved_study_id,
        run_id=run_id,
        status=status,
        run_dir=str(run_dir.as_posix()),
        r_result=r_result,
        validation_report=validation_report,
        artifacts=artifacts,
        manifest=manifest,
        warnings=warnings,
        failure_record=failure_record,
    )


def _require_input(index: StudyInputIndex, domain: str) -> ArtifactRef:
    try:
        return index.input_sdtm[domain]
    except KeyError as exc:
        raise ValueError(f"ADSL minimal run requires input_sdtm/{domain.lower()}.csv or .sas7bdat") from exc


def _failed_run_result(
    *,
    store: ArtifactStore,
    failure_report_path: Path,
    study_id: str,
    run_id: str,
    run_dir: Path,
    stage: str,
    message: str,
    warnings: list[str],
) -> AdslRunResult:
    failure_record = diagnose_adsl_failure(stage=stage, message=message)
    write_failure_report(failure_record, failure_report_path)
    failure_ref = store.register_existing(
        failure_report_path,
        artifact_id="adsl_failure_report",
        kind="tool_log",
        role="audit",
        dataset="ADSL",
    )
    validation_report = {
        "dataset": "ADSL",
        "status": "not_run",
        "checks": [],
        "warnings": [],
        "errors": [message],
    }
    manifest = store.write_manifest(
        extra={
            "dataset": "ADSL",
            "phase": "phase6_failure_diagnosis",
            "validation_status": "not_run",
            "failure_id": failure_record.failure_id,
            "failure_type": failure_record.failure_type,
            "root_cause": failure_record.root_cause,
            "recommended_route": failure_record.recommended_route,
            "warnings": warnings,
        }
    )
    return AdslRunResult(
        study_id=study_id,
        run_id=run_id,
        status="failed",
        run_dir=str(run_dir.as_posix()),
        r_result=RRunResult(dataset="ADSL", exit_code=2, stderr=message),
        validation_report=validation_report,
        artifacts={"failure_report": failure_ref},
        manifest=manifest,
        warnings=warnings,
        failure_record=failure_record,
    )


def _profile_or_r_runtime_profile(profile: DatasetProfile, *, rscript_path: str | None, profile_dir: Path) -> DatasetProfile:
    if profile.status == "ok":
        return profile
    if profile.format == "sas7bdat" and profile.status == "not_implemented_yet":
        return _profile_sas7bdat_with_r(profile, rscript_path=rscript_path, profile_dir=profile_dir)
    return profile


def _profile_sas7bdat_with_r(profile: DatasetProfile, *, rscript_path: str | None, profile_dir: Path) -> DatasetProfile:
    runner = LocalRRunner(rscript_path=rscript_path)
    if not runner.rscript_path:
        return DatasetProfile(
            dataset=profile.dataset,
            path=profile.path,
            format=profile.format,
            status="error",
            message="sas7bdat profiling requires Rscript with the R package 'haven'.",
        )

    r_code = f'''
if (!requireNamespace("haven", quietly = TRUE)) {{
  stop("Reading sas7bdat requires the R package 'haven'.")
}}
data <- haven::read_sas({_r_string(profile.path)})
cat(paste(names(data), collapse = "\t"), "\n", nrow(data), "\n", sep = "")
'''
    result = runner.run(
        RRunRequest(
            code=r_code,
            dataset=profile.dataset,
            run_id="profile",
            working_dir=str(profile_dir),
            script_path=str(profile_dir / f"profile_{profile.dataset.lower()}.R"),
            timeout_seconds=60,
        )
    )

    if not result.success:
        return DatasetProfile(
            dataset=profile.dataset,
            path=profile.path,
            format=profile.format,
            status="error",
            message=result.stderr.strip() or result.stdout.strip() or "R sas7bdat profiling failed",
        )

    lines = result.stdout.splitlines()
    if len(lines) < 2:
        return DatasetProfile(
            dataset=profile.dataset,
            path=profile.path,
            format=profile.format,
            status="error",
            message="R sas7bdat profiling returned incomplete output.",
        )
    columns = lines[0].split("\t") if lines[0] else []
    try:
        row_count = int(lines[1])
    except ValueError:
        row_count = None

    return DatasetProfile(
        dataset=profile.dataset,
        path=profile.path,
        format=profile.format,
        status="ok",
        columns=columns,
        row_count=row_count,
        sample_rows=[],
        message="Profiled sas7bdat columns through R haven.",
    )


def _r_string(value: str) -> str:
    return '"' + value.replace("\\", "/").replace('"', '\\"') + '"'


def _write_json(path: Path, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")
