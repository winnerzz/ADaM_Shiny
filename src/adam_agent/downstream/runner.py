"""Generic downstream ADaM runner for LLM-generated R code."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from adam_agent.downstream.diagnostics import diagnose_downstream_failure, write_downstream_failure_report
from adam_agent.llm.clients import LLMClient, LLMRequest, MockLLMClient
from adam_agent.llm.context import build_target_llm_context, write_llm_context_package
from adam_agent.llm.generated_code import LLMGeneratedCodeError, parse_generated_code_response, write_generated_code_artifacts
from adam_agent.llm.mock_code import default_mock_generated_code_response
from adam_agent.llm.prompt_compaction import (
    MAX_SAMPLE_ROWS_IN_PROMPT,
    compact_prompt_from_context,
    repair_prompt_from_failure,
    write_compact_prompt_artifact,
)
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.llm import LLMCallRecord, LLMExposureConfig
from adam_agent.schemas.routing import FailureRecord
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.r_runner import RRunRequest, RRunResult
from adam_agent.tools.static_rules import (
    StaticRuleReport,
    run_generated_r_static_checks,
    write_static_rule_report,
)


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
    failure_records: list[FailureRecord] = field(default_factory=list)


@dataclass
class _AttemptResult:
    """Internal result for one generate/repair attempt."""

    status: str
    validation_status: str
    call_record: LLMCallRecord | None
    r_result: RRunResult | None
    validation_report: dict[str, Any]
    error: str = ""
    failure_record: FailureRecord | None = None
    generated_code: str = ""
    raw_response_text: str = ""


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
    max_repair_attempts: int = 1,
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

    provider_key = provider.strip().lower()
    if llm_client is None and provider_key != "mock":
        validation_report = _validation_report(
            target,
            "llm_client_required",
            errors=[f"Provider {provider} requires an explicit LLM client; refusing to fall back to mock."],
        )
        validation_report.update(
            {
                "llm_provider": provider,
                "llm_model": model,
                "stubbed_r_execution": False,
                "not_real_derivation": True,
            }
        )
        validation_artifact = _write_validation_artifact(root, study_id, run_id, target, validation_report)
        artifacts["validation_report"] = validation_artifact
        return DownstreamRunResult(
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status="failed",
            validation_status="llm_client_required",
            run_dir=str(run_dir.as_posix()),
            artifacts=artifacts,
            llm_call_record=None,
            r_result=None,
            validation_report=validation_report,
            warnings=context.warnings,
            error=validation_report["errors"][0],
        )

    context_dict = context.as_dict()
    required_identifiers = _required_identifiers_from_context(context_dict)
    required_identifier_source_id = _target_spec_source_id_from_context(context_dict)
    prompt = compact_prompt_from_context(context_dict)
    prompt_artifact = write_compact_prompt_artifact(
        study_id=study_id,
        run_id=run_id,
        target_dataset=target,
        study_dir=root,
        prompt=prompt,
        source_context_artifact_id=context_artifact.artifact_id,
    )
    artifacts["llm_prompt"] = prompt_artifact
    llm = llm_client or MockLLMClient(fixed_response_text=_default_mock_generated_code_response(target))
    failure_records: list[FailureRecord] = []

    llm_response = llm.generate(
        _llm_request(
            prompt=prompt,
            target=target,
            study_id=study_id,
            run_id=run_id,
            provider=provider,
            model=model,
            exposure=exposure_config,
            context_dict=context_dict,
            context_artifact_id=prompt_artifact.artifact_id,
            node="generate_downstream_code",
            call_id=f"llm_{run_id}_{target.lower()}",
            response_artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{target.lower()}",
        )
    )
    attempt = _run_generated_response_attempt(
        study_id=study_id,
        run_id=run_id,
        study_dir=root,
        run_dir=run_dir,
        target=target,
        context_warnings=context.warnings,
        runtime_output_path=Path(context.runtime_contract["output_path"]),
        llm_response_text=llm_response.response_text,
        call_record=llm_response.call_record,
        runner=runner,
        provider=provider,
        model=model,
        artifacts=artifacts,
        repair_attempt=0,
        required_identifiers=required_identifiers,
        required_identifier_source_id=required_identifier_source_id,
    )

    if attempt.failure_record is not None:
        failure_records.append(attempt.failure_record)

    if attempt.status in {"completed", "completed_stub"}:
        return DownstreamRunResult(
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status=attempt.status,
            validation_status=attempt.validation_status,
            run_dir=str(run_dir.as_posix()),
            artifacts=artifacts,
            llm_call_record=attempt.call_record,
            r_result=attempt.r_result,
            validation_report=attempt.validation_report,
            warnings=context.warnings,
            failure_records=failure_records,
        )

    if attempt.failure_record and _should_repair(attempt.failure_record, max_repair_attempts=max_repair_attempts):
        repair_prompt = _repair_prompt_from_failure(
            compact_context_prompt=prompt,
            raw_response_text=attempt.raw_response_text,
            generated_code=attempt.generated_code,
            validation_report=attempt.validation_report,
            r_result=attempt.r_result,
            failure_record=attempt.failure_record,
        )
        repair_response = llm.generate(
            _llm_request(
                prompt=repair_prompt,
                target=target,
                study_id=study_id,
                run_id=run_id,
                provider=provider,
                model=model,
                exposure=exposure_config,
                context_dict=context_dict,
                context_artifact_id=prompt_artifact.artifact_id,
                node="repair_downstream_code",
                call_id=f"llm_{run_id}_{target.lower()}_repair1",
                response_artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{target.lower()}_repair1",
                system_prompt=_code_repair_system_prompt(target),
            )
        )
        repair_attempt = _run_generated_response_attempt(
            study_id=study_id,
            run_id=run_id,
            study_dir=root,
            run_dir=run_dir,
            target=target,
            context_warnings=context.warnings,
            runtime_output_path=Path(context.runtime_contract["output_path"]),
            llm_response_text=repair_response.response_text,
            call_record=repair_response.call_record,
            runner=runner,
            provider=provider,
            model=model,
            artifacts=artifacts,
            repair_attempt=1,
            required_identifiers=required_identifiers,
            required_identifier_source_id=required_identifier_source_id,
            attempt_label="repair1",
        )
        if repair_attempt.failure_record is not None:
            failure_records.append(repair_attempt.failure_record)

        if repair_attempt.status in {"completed", "completed_stub"}:
            failure_report = write_downstream_failure_report(
                study_dir=root,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                failure_records=failure_records,
                validation_report=repair_attempt.validation_report,
                status="repaired",
            )
            artifacts["failure_report"] = failure_report
            return DownstreamRunResult(
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                status=repair_attempt.status,
                validation_status=repair_attempt.validation_status,
                run_dir=str(run_dir.as_posix()),
                artifacts=artifacts,
                llm_call_record=repair_attempt.call_record,
                r_result=repair_attempt.r_result,
                validation_report=repair_attempt.validation_report,
                warnings=context.warnings,
                failure_records=failure_records,
            )
        attempt = repair_attempt

    failure_report = write_downstream_failure_report(
        study_dir=root,
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        failure_records=failure_records,
        validation_report=attempt.validation_report,
        status="failed",
    )
    artifacts["failure_report"] = failure_report

    return DownstreamRunResult(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="failed",
        validation_status=attempt.validation_status,
        run_dir=str(run_dir.as_posix()),
        artifacts=artifacts,
        llm_call_record=attempt.call_record,
        r_result=attempt.r_result,
        validation_report=attempt.validation_report,
        warnings=context.warnings,
        error=attempt.error,
        failure_records=failure_records,
    )


class StructuralStubRRunner:
    """Stub runner for graph integration that writes the canonical output file."""

    backend_name = "structural_stub"
    hardened = False
    network_disabled = False

    def run(self, request: RRunRequest) -> RRunResult:
        output_path = Path(request.working_dir) / "outputs" / f"{request.dataset.lower()}.csv"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text("USUBJID\n", encoding="utf-8")
        return RRunResult(dataset=request.dataset, exit_code=0, stdout=f"Structural stub wrote {output_path.name}", stderr="")


def _run_generated_response_attempt(
    *,
    study_id: str,
    run_id: str,
    study_dir: Path,
    run_dir: Path,
    target: str,
    context_warnings: list[str],
    runtime_output_path: Path,
    llm_response_text: str,
    call_record: LLMCallRecord | None,
    runner: Any,
    provider: str,
    model: str,
    artifacts: dict[str, ArtifactRef],
    repair_attempt: int,
    required_identifiers: list[str],
    required_identifier_source_id: str | None,
    attempt_label: str | None = None,
) -> _AttemptResult:
    """Parse, write, execute, validate, and diagnose one LLM response."""

    artifact_suffix = f"_{attempt_label}" if attempt_label else ""
    raw_response_artifact = _write_raw_llm_response_artifact(
        study_dir,
        study_id,
        run_id,
        target,
        llm_response_text,
        attempt_label=attempt_label,
    )
    artifacts[f"llm_response{artifact_suffix}"] = raw_response_artifact

    try:
        generated_package = parse_generated_code_response(llm_response_text, expected_dataset=target)
    except LLMGeneratedCodeError as exc:
        validation_report = _validation_report(target, "llm_output_parse_error", errors=[str(exc)])
        _add_attempt_metadata(
            validation_report,
            provider=provider,
            model=model,
            call_record=call_record,
            stubbed_r_execution=isinstance(runner, StructuralStubRRunner),
            repair_attempt=repair_attempt,
            attempt_label=attempt_label,
        )
        failure_record = diagnose_downstream_failure(
            dataset=target,
            stage="llm_parse",
            message=str(exc),
            validation_report=validation_report,
            artifact_ids=[raw_response_artifact.artifact_id],
            repair_attempt=repair_attempt,
        )
        validation_report["failure"] = failure_record.model_dump(mode="json")
        validation_artifact = _write_validation_artifact(
            study_dir,
            study_id,
            run_id,
            target,
            validation_report,
            attempt_label=attempt_label,
        )
        artifacts[f"validation_report{artifact_suffix}"] = validation_artifact
        artifacts["validation_report"] = validation_artifact
        return _AttemptResult(
            status="failed",
            validation_status="llm_output_parse_error",
            call_record=call_record,
            r_result=None,
            validation_report=validation_report,
            error=str(exc),
            failure_record=failure_record,
            raw_response_text=llm_response_text,
        )

    generated_artifacts = write_generated_code_artifacts(
        study_id=study_id,
        run_id=run_id,
        study_dir=study_dir,
        package=generated_package,
        response_text=llm_response_text,
        attempt_label=attempt_label,
    )
    artifacts[f"llm_response{artifact_suffix}"] = generated_artifacts.response_artifact
    artifacts[f"generated_code{artifact_suffix}"] = generated_artifacts.code_artifact
    artifacts[f"llm_parsed_response{artifact_suffix}"] = generated_artifacts.package_artifact
    if attempt_label is None:
        artifacts["llm_response"] = generated_artifacts.response_artifact
        artifacts["generated_code"] = generated_artifacts.code_artifact
        artifacts["llm_parsed_response"] = generated_artifacts.package_artifact

    static_report, static_artifact = _write_static_check_artifact(
        study_dir=study_dir,
        study_id=study_id,
        run_id=run_id,
        target=target,
        code_path=Path(generated_artifacts.code_artifact.path),
        required_identifiers=required_identifiers,
        required_identifier_source_id=required_identifier_source_id,
        rscript_path=getattr(runner, "rscript_path", None),
        attempt_label=attempt_label,
    )
    artifacts[f"static_check{artifact_suffix}"] = static_artifact
    if attempt_label is None:
        artifacts["static_check"] = static_artifact
    if static_report.blocking_errors:
        static_errors = [finding.message for finding in static_report.blocking_errors]
        validation_report = _validation_report(target, "static_rule_error", errors=static_errors)
        validation_report["static_check"] = static_report.as_dict()
        _add_attempt_metadata(
            validation_report,
            provider=provider,
            model=model,
            call_record=call_record,
            stubbed_r_execution=isinstance(runner, StructuralStubRRunner),
            repair_attempt=repair_attempt,
            attempt_label=attempt_label,
        )
        failure_record = diagnose_downstream_failure(
            dataset=target,
            stage="static_check",
            message="; ".join(static_errors),
            validation_report=validation_report,
            artifact_ids=[
                generated_artifacts.response_artifact.artifact_id,
                generated_artifacts.code_artifact.artifact_id,
                generated_artifacts.package_artifact.artifact_id,
                static_artifact.artifact_id,
            ],
            repair_attempt=repair_attempt,
        )
        validation_report["failure"] = failure_record.model_dump(mode="json")
        validation_artifact = _write_validation_artifact(
            study_dir,
            study_id,
            run_id,
            target,
            validation_report,
            attempt_label=attempt_label,
        )
        artifacts[f"validation_report{artifact_suffix}"] = validation_artifact
        artifacts["validation_report"] = validation_artifact
        return _AttemptResult(
            status="failed",
            validation_status="static_rule_error",
            call_record=call_record,
            r_result=None,
            validation_report=validation_report,
            error="; ".join(static_errors),
            failure_record=failure_record,
            generated_code=generated_package.r_code,
            raw_response_text=llm_response_text,
        )

    preflight_errors = _real_r_preflight_errors(
        run_dir=run_dir,
        target=target,
        script_path=Path(generated_artifacts.code_artifact.path),
        output_path=runtime_output_path,
        stubbed_r_execution=isinstance(runner, StructuralStubRRunner),
    )
    if preflight_errors:
        validation_report = _validation_report(target, "r_sandbox_preflight_error", errors=preflight_errors)
        validation_report["sandbox"] = _sandbox_boundary_payload(runner, run_dir)
        _add_attempt_metadata(
            validation_report,
            provider=provider,
            model=model,
            call_record=call_record,
            stubbed_r_execution=False,
            repair_attempt=repair_attempt,
            attempt_label=attempt_label,
        )
        failure_record = diagnose_downstream_failure(
            dataset=target,
            stage="preflight",
            message="; ".join(preflight_errors),
            validation_report=validation_report,
            artifact_ids=[
                generated_artifacts.response_artifact.artifact_id,
                generated_artifacts.code_artifact.artifact_id,
                generated_artifacts.package_artifact.artifact_id,
            ],
            repair_attempt=repair_attempt,
        )
        validation_report["failure"] = failure_record.model_dump(mode="json")
        validation_artifact = _write_validation_artifact(
            study_dir,
            study_id,
            run_id,
            target,
            validation_report,
            attempt_label=attempt_label,
        )
        artifacts[f"validation_report{artifact_suffix}"] = validation_artifact
        artifacts["validation_report"] = validation_artifact
        return _AttemptResult(
            status="failed",
            validation_status="r_sandbox_preflight_error",
            call_record=call_record,
            r_result=None,
            validation_report=validation_report,
            error="; ".join(preflight_errors),
            failure_record=failure_record,
            generated_code=generated_package.r_code,
            raw_response_text=llm_response_text,
        )

    r_result = runner.run(
        RRunRequest(
            code="",
            dataset=target,
            run_id=run_id,
            working_dir=str(run_dir),
            script_path=generated_artifacts.code_artifact.path,
        )
    )
    sandbox_boundary = _sandbox_boundary_payload(runner, run_dir)
    validation_report = _validate_downstream_output(
        target=target,
        output_path=runtime_output_path,
        r_result=r_result,
        expected_outputs=generated_package.expected_outputs,
        context_warnings=context_warnings,
        stubbed_r_execution=isinstance(runner, StructuralStubRRunner),
        provider=provider,
        model=model,
        call_record=call_record,
        sandbox_boundary=sandbox_boundary,
    )
    _add_attempt_metadata(
        validation_report,
        provider=provider,
        model=model,
        call_record=call_record,
        stubbed_r_execution=isinstance(runner, StructuralStubRRunner),
        repair_attempt=repair_attempt,
        attempt_label=attempt_label,
    )
    failure_record = None
    if validation_report["status"] == "fail":
        failure_record = diagnose_downstream_failure(
            dataset=target,
            stage="r_sandbox" if not r_result.success else "validation",
            r_result=r_result,
            validation_report=validation_report,
            artifact_ids=[
                generated_artifacts.response_artifact.artifact_id,
                generated_artifacts.code_artifact.artifact_id,
                generated_artifacts.package_artifact.artifact_id,
            ],
            repair_attempt=repair_attempt,
        )
        validation_report["failure"] = failure_record.model_dump(mode="json")

    validation_artifact = _write_validation_artifact(
        study_dir,
        study_id,
        run_id,
        target,
        validation_report,
        attempt_label=attempt_label,
    )
    artifacts[f"validation_report{artifact_suffix}"] = validation_artifact
    artifacts["validation_report"] = validation_artifact
    if validation_report["status"] in {"pass", "structural_stub_pass"} and runtime_output_path.exists() and runtime_output_path.is_file():
        output_artifact = _output_artifact(study_id, run_id, target, runtime_output_path)
        artifacts[f"output_adam{artifact_suffix}"] = output_artifact
        artifacts["output_adam"] = output_artifact

    status = (
        "completed_stub"
        if validation_report["status"] == "structural_stub_pass"
        else "completed"
        if validation_report["status"] == "pass"
        else "failed"
    )
    return _AttemptResult(
        status=status,
        validation_status=validation_report["status"],
        call_record=call_record,
        r_result=r_result,
        validation_report=validation_report,
        error="; ".join(str(error) for error in validation_report.get("errors", [])),
        failure_record=failure_record,
        generated_code=generated_package.r_code,
        raw_response_text=llm_response_text,
    )


def _real_r_preflight_errors(
    *,
    run_dir: Path,
    target: str,
    script_path: Path,
    output_path: Path,
    stubbed_r_execution: bool,
) -> list[str]:
    """Check path boundaries before local Rscript executes generated code."""

    if stubbed_r_execution:
        return []

    errors: list[str] = []
    resolved_run_dir = run_dir.resolve()
    resolved_script = script_path.resolve()
    resolved_output = output_path.resolve()
    expected_code_dir = (resolved_run_dir / "code").resolve()
    expected_output = (resolved_run_dir / "outputs" / f"{target.lower()}.csv").resolve()

    if not _is_relative_to(resolved_script, expected_code_dir):
        errors.append(f"Generated R script must live under run code directory: {expected_code_dir.as_posix()}")
    if resolved_output != expected_output:
        errors.append(f"Runtime output path must be canonical: {expected_output.as_posix()}")
    return errors


def _is_relative_to(path: Path, parent: Path) -> bool:
    try:
        path.relative_to(parent)
    except ValueError:
        return False
    return True


def _prompt_from_context(context: dict[str, Any]) -> str:
    return compact_prompt_from_context(context)


def _code_generation_system_prompt(target: str) -> str:
    dataset = target.upper()
    return (
        "Return only one valid minified JSON object. Do not use markdown. "
        "Do not explain or reason step by step. Use short, readable base R code. "
        "Top-level keys must be: dataset, r_code, assumptions, risk_points, used_inputs, expected_outputs. "
        f"dataset must be {dataset}. r_code runs from the run working directory and must write outputs/{dataset.lower()}.csv. "
        "Read input data only from the read_path values listed in the prompt; do not search folders or assume an inputs directory. "
        "For CSV input, use colClasses='character' and check.names=FALSE so subject IDs and sequence values are not changed by R type guessing. "
        "The list fields must be arrays of strings."
    )


def _code_repair_system_prompt(target: str) -> str:
    dataset = target.upper()
    return (
        "You are repairing generated R code for an ADaM prototype. "
        "Return only valid JSON. Do not wrap the JSON in markdown. "
        "The JSON object must contain exactly these top-level fields: "
        "dataset, r_code, assumptions, risk_points, used_inputs, expected_outputs. "
        f"dataset must be {dataset}. r_code must write outputs/{dataset.lower()}.csv "
        "relative to the working directory. assumptions, risk_points, used_inputs, "
        "and expected_outputs must be arrays of strings. Fix only code/runtime/output "
        "contract problems; do not invent missing source variables or new clinical rules."
    )


def _llm_request(
    *,
    prompt: str,
    target: str,
    study_id: str,
    run_id: str,
    provider: str,
    model: str,
    exposure: LLMExposureConfig,
    context_dict: dict[str, Any],
    context_artifact_id: str,
    node: str,
    call_id: str,
    response_artifact_id: str,
    system_prompt: str | None = None,
) -> LLMRequest:
    return LLMRequest(
        prompt=prompt,
        system_prompt=system_prompt or _code_generation_system_prompt(target),
        provider=provider,
        model=model,
        exposure=exposure,
        node=node,
        call_id=call_id,
        datasets_included=_datasets_included(context_dict),
        variables_included=_variables_included(context_dict),
        sample_row_counts=_sample_row_counts(context_dict),
        subject_level_data_included=bool(_sample_row_counts(context_dict)),
        prompt_artifact_id=context_artifact_id,
        response_artifact_id=response_artifact_id,
        redaction_policy="phase7_context_package_policy",
    )


def _repair_prompt_from_failure(
    *,
    compact_context_prompt: str,
    raw_response_text: str,
    generated_code: str,
    validation_report: dict[str, Any],
    r_result: RRunResult | None,
    failure_record: FailureRecord,
) -> str:
    return repair_prompt_from_failure(
        compact_context_prompt=compact_context_prompt,
        raw_response_text=raw_response_text,
        generated_code=generated_code,
        validation_report=validation_report,
        r_result=r_result,
        failure_record=failure_record,
    )


def _should_repair(record: FailureRecord, *, max_repair_attempts: int) -> bool:
    if max_repair_attempts < 1:
        return False
    if record.repair_attempt >= max_repair_attempts:
        return False
    return record.recommended_route == "repair_code" and record.failure_type in {"llm_error", "code_error"}


def _write_raw_llm_response_artifact(
    study_dir: Path,
    study_id: str,
    run_id: str,
    target: str,
    response_text: str,
    attempt_label: str | None = None,
) -> ArtifactRef:
    target_lower = target.lower()
    suffix = f"_{attempt_label}" if attempt_label else ""
    response_path = study_dir / "runs" / run_id / "llm" / f"{target_lower}_response{suffix}.json"
    response_path.parent.mkdir(parents=True, exist_ok=True)
    response_path.write_text(response_text, encoding="utf-8")
    return ArtifactRef(
        artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{target_lower}{suffix}",
        kind="llm_response",
        path=str(response_path.as_posix()),
        sha256=f"sha256:{sha256_file(response_path)}",
        dataset=target,
        format="json",
        role="audit",
        metadata={"parsed": False, "attempt_label": attempt_label or "initial"},
    )


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
                counts[dataset] = min(len(sample_rows), MAX_SAMPLE_ROWS_IN_PROMPT)
    return counts


def _default_mock_generated_code_response(target: str) -> str:
    return default_mock_generated_code_response(target)


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
    call_record: LLMCallRecord | None,
    sandbox_boundary: dict[str, Any] | None = None,
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
    expected_output_names = {Path(value).name for value in expected_outputs}
    if expected_outputs and expected_name not in expected_output_names:
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
        "r_stdout": r_result.stdout,
        "r_stderr": r_result.stderr,
        "stubbed_r_execution": stubbed_r_execution,
        "llm_provider": provider,
        "llm_model": model,
        "provider_alias": call_record.provider_alias if call_record else None,
        "transport": call_record.transport if call_record else None,
        "provider_base_url": call_record.provider_base_url if call_record else None,
        "external_relay": call_record.external_relay if call_record else False,
        "risk_flags": call_record.risk_flags if call_record else [],
        "not_real_derivation": stubbed_r_execution or provider == "mock",
        "sandbox": sandbox_boundary
        or {
            "backend_name": "structural_stub" if stubbed_r_execution else "unknown",
            "hardened": False,
            "run_dir": str(output_path.parent.parent.as_posix()),
            "network_disabled": False,
            "notes": ["Sandbox boundary metadata was not provided by the runner."],
        },
    }


def _sandbox_boundary_payload(runner: Any, run_dir: Path) -> dict[str, Any]:
    if hasattr(runner, "boundary"):
        boundary = runner.boundary()
        if hasattr(boundary, "as_dict"):
            return boundary.as_dict()
    return {
        "backend_name": getattr(runner, "backend_name", runner.__class__.__name__),
        "hardened": bool(getattr(runner, "hardened", False)),
        "run_dir": str(run_dir.as_posix()),
        "network_disabled": bool(getattr(runner, "network_disabled", False)),
        "notes": ["Sandbox runner does not expose detailed boundary metadata."],
    }


def _add_attempt_metadata(
    report: dict[str, Any],
    *,
    provider: str,
    model: str,
    call_record: LLMCallRecord | None,
    stubbed_r_execution: bool,
    repair_attempt: int,
    attempt_label: str | None,
) -> None:
    report.update(
        {
            "llm_provider": provider,
            "llm_model": model,
            "provider_alias": call_record.provider_alias if call_record else None,
            "transport": call_record.transport if call_record else None,
            "provider_base_url": call_record.provider_base_url if call_record else None,
            "external_relay": call_record.external_relay if call_record else False,
            "risk_flags": call_record.risk_flags if call_record else [],
            "stubbed_r_execution": stubbed_r_execution,
            "not_real_derivation": stubbed_r_execution or provider == "mock",
            "repair_attempt": repair_attempt,
            "attempt_label": attempt_label or "initial",
        }
    )


def _output_artifact(study_id: str, run_id: str, target: str, output_path: Path) -> ArtifactRef:
    return ArtifactRef(
        artifact_id=f"output_adam_{study_id.lower()}_{run_id}_{target.lower()}",
        kind="output_adam",
        path=str(output_path.as_posix()),
        sha256=f"sha256:{sha256_file(output_path)}",
        dataset=target,
        format=output_path.suffix.lower().lstrip("."),
        role="output",
    )


def _write_static_check_artifact(
    *,
    study_dir: Path,
    study_id: str,
    run_id: str,
    target: str,
    code_path: Path,
    required_identifiers: list[str],
    required_identifier_source_id: str | None,
    rscript_path: str | Path | None,
    attempt_label: str | None,
) -> tuple[StaticRuleReport, ArtifactRef]:
    target_lower = target.lower()
    suffix = f"_{attempt_label}" if attempt_label else ""
    static_path = study_dir / "runs" / run_id / "static_checks" / f"{target_lower}_static_check{suffix}.json"
    report = run_generated_r_static_checks(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        code_path=code_path,
        expected_output_path=f"outputs/{target_lower}.csv",
        required_identifiers=required_identifiers,
        required_identifier_source_id=required_identifier_source_id,
        rscript_path=rscript_path,
    )
    write_static_rule_report(report, path=static_path)
    artifact = ArtifactRef(
        artifact_id=f"static_check_{study_id.lower()}_{run_id}_{target_lower}{suffix}",
        kind="tool_log",
        path=str(static_path.as_posix()),
        sha256=f"sha256:{sha256_file(static_path)}",
        dataset=target,
        format="json",
        role="audit",
        metadata={
            "attempt_label": attempt_label or "initial",
            "static_rule_status": report.status,
            "blocking_error_count": len(report.blocking_errors),
            "limited_scope": True,
        },
    )
    return report, artifact


def _required_identifiers_from_context(context: dict[str, Any]) -> list[str]:
    target_spec = context.get("target_spec")
    if not isinstance(target_spec, dict):
        return []
    variables = target_spec.get("variables")
    if not isinstance(variables, list):
        return []
    identifiers: list[str] = []
    for item in variables:
        if not isinstance(item, dict):
            continue
        name = item.get("variable") or item.get("name") or item.get("Variable")
        text = str(name or "").strip().upper()
        if text and text not in identifiers:
            identifiers.append(text)
    return identifiers[:50]


def _target_spec_source_id_from_context(context: dict[str, Any]) -> str | None:
    target_spec = context.get("target_spec")
    if not isinstance(target_spec, dict):
        return None
    artifact_id = str(target_spec.get("artifact_id") or "").strip()
    if artifact_id:
        return artifact_id
    path = str(target_spec.get("path") or "").strip()
    if path:
        return Path(path).as_posix()
    return None


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
    attempt_label: str | None = None,
) -> ArtifactRef:
    target_lower = target.lower()
    suffix = f"_{attempt_label}" if attempt_label else ""
    validation_path = study_dir / "runs" / run_id / "validation" / f"{target_lower}_validation_report{suffix}.json"
    validation_path.parent.mkdir(parents=True, exist_ok=True)
    validation_path.write_text(json.dumps(report, indent=2, sort_keys=True), encoding="utf-8")
    return ArtifactRef(
        artifact_id=f"validation_{study_id.lower()}_{run_id}_{target_lower}{suffix}",
        kind="validation_report",
        path=str(validation_path.as_posix()),
        sha256=f"sha256:{sha256_file(validation_path)}",
        dataset=target,
        format="json",
        role="output",
        metadata={"attempt_label": attempt_label or "initial"},
    )

