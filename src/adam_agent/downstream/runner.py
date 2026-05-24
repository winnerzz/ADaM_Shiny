"""Generic downstream ADaM runner for LLM-generated R code."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from adam_agent.llm.clients import LLMClient, LLMRequest, MockLLMClient
from adam_agent.llm.context import build_target_llm_context, write_llm_context_package
from adam_agent.llm.generated_code import LLMGeneratedCodeError, parse_generated_code_response, write_generated_code_artifacts
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.llm import LLMCallRecord, LLMExposureConfig
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.r_runner import RRunRequest, RRunResult


@dataclass
class DownstreamRunResult:
    """Summary of one downstream ADaM generation attempt."""

    study_id: str
    run_id: str
    dataset: str
    status: str
    validation_status: str
    run_dir: str
    artifacts: dict[str, ArtifactRef]
    llm_call_record: LLMCallRecord | None
    r_result: RRunResult | None
    validation_report: dict[str, Any]
    warnings: list[str] = field(default_factory=list)
    error: str = ""


def run_downstream_adam(
    *,
    study_dir: str | Path,
    study_id: str,
    run_id: str,
    target_dataset: str,
    dependency_resolution: list[dict[str, Any]],
    llm_client: LLMClient | None = None,
    exposure: LLMExposureConfig | None = None,
    source_datasets: list[str] | None = None,
    provider: str = "mock",
    model: str = "mock-model",
    r_runner: Any | None = None,
) -> DownstreamRunResult:
    """Generate and execute one downstream ADaM target through stable tool boundaries."""

    root = Path(study_dir)
    target = target_dataset.strip().upper()
    run_dir = root / "runs" / run_id
    artifacts: dict[str, ArtifactRef] = {}
    exposure_config = exposure or LLMExposureConfig()
    runner = r_runner or StructuralStubRRunner()

    context = build_target_llm_context(
        study_id=study_id,
        run_id=run_id,
        target_dataset=target,
        study_dir=root,
        dependency_resolution=dependency_resolution,
        exposure=exposure_config,
        source_datasets=source_datasets,
    )
    context_artifact = write_llm_context_package(context, root)
    artifacts["llm_context"] = context_artifact

    prompt = _prompt_from_context(context.as_dict())
    llm = llm_client or MockLLMClient(fixed_response_text=_default_mock_generated_code_response(target))
    llm_response = llm.generate(
        LLMRequest(
            prompt=prompt,
            provider=provider,
            model=model,
            exposure=exposure_config,
            node="generate_downstream_code",
            call_id=f"llm_{run_id}_{target.lower()}",
            datasets_included=_datasets_included(context.as_dict()),
            variables_included=_variables_included(context.as_dict()),
            sample_row_counts=_sample_row_counts(context.as_dict()),
            prompt_artifact_id=context_artifact.artifact_id,
            response_artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{target.lower()}",
            redaction_policy="phase7_context_package_policy",
        )
    )

    try:
        generated_package = parse_generated_code_response(llm_response.response_text, expected_dataset=target)
    except LLMGeneratedCodeError as exc:
        validation_report = _validation_report(target, "llm_output_parse_error", errors=[str(exc)])
        validation_artifact = _write_validation_artifact(root, study_id, run_id, target, validation_report)
        artifacts["validation_report"] = validation_artifact
        return DownstreamRunResult(
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status="failed",
            validation_status="llm_output_parse_error",
            run_dir=str(run_dir.as_posix()),
            artifacts=artifacts,
            llm_call_record=llm_response.call_record,
            r_result=None,
            validation_report=validation_report,
            warnings=context.warnings,
            error=str(exc),
        )

    generated_artifacts = write_generated_code_artifacts(
        study_id=study_id,
        run_id=run_id,
        study_dir=root,
        package=generated_package,
        response_text=llm_response.response_text,
    )
    artifacts["llm_response"] = generated_artifacts.response_artifact
    artifacts["generated_code"] = generated_artifacts.code_artifact
    artifacts["llm_parsed_response"] = generated_artifacts.package_artifact

    r_result = runner.run(
        RRunRequest(
            code="",
            dataset=target,
            run_id=run_id,
            working_dir=str(run_dir),
            script_path=generated_artifacts.code_artifact.path,
        )
    )

    output_path = Path(context.runtime_contract["output_path"])
    validation_report = _validate_downstream_output(
        target=target,
        output_path=output_path,
        r_result=r_result,
        expected_outputs=generated_package.expected_outputs,
        context_warnings=context.warnings,
        stubbed_r_execution=isinstance(runner, StructuralStubRRunner),
        provider=provider,
        model=model,
    )
    validation_artifact = _write_validation_artifact(root, study_id, run_id, target, validation_report)
    artifacts["validation_report"] = validation_artifact
    if output_path.exists() and output_path.is_file():
        artifacts["output_adam"] = ArtifactRef(
            artifact_id=f"output_adam_{study_id.lower()}_{run_id}_{target.lower()}",
            kind="output_adam",
            path=str(output_path.as_posix()),
            sha256=f"sha256:{sha256_file(output_path)}",
            dataset=target,
            format=output_path.suffix.lower().lstrip("."),
            role="output",
        )

    status = "completed_stub" if validation_report["status"] == "structural_stub_pass" else "completed" if validation_report["status"] == "pass" else "failed"
    return DownstreamRunResult(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status=status,
        validation_status=validation_report["status"],
        run_dir=str(run_dir.as_posix()),
        artifacts=artifacts,
        llm_call_record=llm_response.call_record,
        r_result=r_result,
        validation_report=validation_report,
        warnings=context.warnings,
    )


class StructuralStubRRunner:
    """Stub runner for graph integration that writes the canonical output file."""

    def run(self, request: RRunRequest) -> RRunResult:
        output_path = Path(request.working_dir) / "outputs" / f"{request.dataset.lower()}.csv"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text("USUBJID\n", encoding="utf-8")
        return RRunResult(dataset=request.dataset, exit_code=0, stdout=f"Structural stub wrote {output_path.name}", stderr="")


def _prompt_from_context(context: dict[str, Any]) -> str:
    return json.dumps(context, indent=2, sort_keys=True)


def _datasets_included(context: dict[str, Any]) -> list[str]:
    datasets = list(context.get("source_dataset_profiles", {}).keys())
    datasets.extend(context.get("resolved_dependencies", {}).keys())
    return datasets


def _variables_included(context: dict[str, Any]) -> list[str]:
    variables: list[str] = []
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        for profile in context.get(section, {}).values():
            for column in profile.get("columns", []):
                if column not in variables:
                    variables.append(column)
    return variables


def _sample_row_counts(context: dict[str, Any]) -> dict[str, int]:
    counts: dict[str, int] = {}
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        for dataset, profile in context.get(section, {}).items():
            sample_rows = profile.get("sample_rows", [])
            if sample_rows:
                counts[dataset] = len(sample_rows)
    return counts


def _default_mock_generated_code_response(target: str) -> str:
    dataset = target.upper()
    filename = dataset.lower()
    r_code = f"""dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
output <- data.frame(USUBJID = character(), stringsAsFactors = FALSE)
write.csv(output, file = "outputs/{filename}.csv", row.names = FALSE)
"""
    return json.dumps(
        {
            "dataset": dataset,
            "r_code": r_code,
            "assumptions": ["Mock downstream runner writes an empty structural output."],
            "risk_points": ["This is not a real ADaM derivation."],
            "used_inputs": [],
            "expected_outputs": [f"{filename}.csv"],
        }
    )


def _validate_downstream_output(
    *,
    target: str,
    output_path: Path,
    r_result: RRunResult,
    expected_outputs: list[str],
    context_warnings: list[str],
    stubbed_r_execution: bool,
    provider: str,
    model: str,
) -> dict[str, Any]:
    errors: list[str] = []
    warnings: list[str] = list(context_warnings)
    checks = [
        {"name": "r_exit_code_zero", "pass": r_result.success},
        {"name": "output_file_exists", "pass": output_path.exists() and output_path.is_file()},
    ]
    if not r_result.success:
        errors.append(r_result.stderr or "R execution failed.")
    if not output_path.exists():
        errors.append(f"Expected output file was not written: {output_path.as_posix()}")
    expected_name = output_path.name
    if expected_outputs and expected_name not in expected_outputs:
        warnings.append(f"LLM expected_outputs does not include canonical output file {expected_name}.")
    dependency_profile_warnings = [
        warning
        for warning in warnings
        if warning.startswith("Profile not fully available") or warning.startswith("Artifact missing")
    ]
    if dependency_profile_warnings:
        errors.extend(dependency_profile_warnings)

    status = "fail" if errors else "structural_stub_pass" if stubbed_r_execution else "pass"
    return {
        "dataset": target,
        "status": status,
        "checks": checks,
        "warnings": warnings,
        "errors": errors,
        "expected_outputs": expected_outputs,
        "output_path": str(output_path.as_posix()),
        "r_exit_code": r_result.exit_code,
        "stubbed_r_execution": stubbed_r_execution,
        "llm_provider": provider,
        "llm_model": model,
        "not_real_derivation": stubbed_r_execution or provider == "mock",
    }


def _validation_report(target: str, status: str, *, errors: list[str]) -> dict[str, Any]:
    return {
        "dataset": target,
        "status": status,
        "checks": [],
        "warnings": [],
        "errors": errors,
    }


def _write_validation_artifact(
    study_dir: Path,
    study_id: str,
    run_id: str,
    target: str,
    report: dict[str, Any],
) -> ArtifactRef:
    target_lower = target.lower()
    validation_path = study_dir / "runs" / run_id / "validation" / f"{target_lower}_validation_report.json"
    validation_path.parent.mkdir(parents=True, exist_ok=True)
    validation_path.write_text(json.dumps(report, indent=2, sort_keys=True), encoding="utf-8")
    return ArtifactRef(
        artifact_id=f"validation_{study_id.lower()}_{run_id}_{target_lower}",
        kind="validation_report",
        path=str(validation_path.as_posix()),
        sha256=f"sha256:{sha256_file(validation_path)}",
        dataset=target,
        format="json",
        role="output",
    )
