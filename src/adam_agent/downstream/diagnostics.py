"""Failure diagnosis helpers for downstream LLM-generated ADaM runs."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.r_runner import RRunResult


def diagnose_downstream_failure(
    *,
    dataset: str,
    stage: str,
    message: str = "",
    r_result: RRunResult | None = None,
    validation_report: dict[str, Any] | None = None,
    artifact_ids: list[str] | None = None,
    repair_attempt: int = 0,
) -> FailureRecord:
    """Classify a downstream failure into a routeable audit record."""

    target = dataset.strip().upper()
    artifact_ids = artifact_ids or []
    combined_message = _combined_message(message=message, r_result=r_result, validation_report=validation_report)
    lower_message = combined_message.lower()

    if stage == "llm_parse":
        return _record(
            dataset=target,
            failure_type="llm_error",
            message=combined_message or "LLM output did not satisfy the generated-code contract.",
            root_cause="code_contract_error",
            recommended_route="repair_code",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    if stage == "preflight":
        return _record(
            dataset=target,
            failure_type="sandbox_error",
            message=combined_message or "Generated R code failed sandbox preflight checks.",
            root_cause="sandbox_policy_violation",
            recommended_route="human_review",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    if stage == "static_check":
        return _record(
            dataset=target,
            failure_type="code_error",
            message=combined_message or "Generated R code failed deterministic static checks.",
            root_cause="static_rule_violation",
            recommended_route="repair_code",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    if stage == "r_sandbox" and r_result is not None and not r_result.success:
        if _looks_like_r_environment_error(lower_message, r_result.exit_code):
            return _record(
                dataset=target,
                failure_type="sandbox_error",
                message=combined_message,
                root_cause="r_environment_error",
                recommended_route="human_review",
                artifact_ids=artifact_ids,
                repair_attempt=repair_attempt,
            )
        if _looks_like_missing_source_variable(lower_message):
            return _record(
                dataset=target,
                failure_type="spec_error",
                message=combined_message,
                root_cause="source_variable_missing",
                recommended_route="revise_spec",
                artifact_ids=artifact_ids,
                repair_attempt=repair_attempt,
            )
        return _record(
            dataset=target,
            failure_type="code_error",
            message=combined_message or "Generated R code failed at runtime.",
            root_cause="r_runtime_error",
            recommended_route="repair_code",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    if validation_report and validation_report.get("status") == "fail":
        if _looks_like_dependency_profile_error(lower_message):
            return _record(
                dataset=target,
                failure_type="input_error",
                message=combined_message,
                root_cause="input_or_dependency_error",
                recommended_route="human_review",
                artifact_ids=artifact_ids,
                repair_attempt=repair_attempt,
            )
        if _looks_like_missing_source_variable(lower_message):
            return _record(
                dataset=target,
                failure_type="spec_error",
                message=combined_message,
                root_cause="source_variable_missing",
                recommended_route="revise_spec",
                artifact_ids=artifact_ids,
                repair_attempt=repair_attempt,
            )
        if "expected output file was not written" in lower_message:
            return _record(
                dataset=target,
                failure_type="code_error",
                message=combined_message,
                root_cause="output_contract_violation",
                recommended_route="repair_code",
                artifact_ids=artifact_ids,
                repair_attempt=repair_attempt,
            )
        return _record(
            dataset=target,
            failure_type="validation_error",
            message=combined_message or "Downstream output validation failed.",
            root_cause="validation_failure",
            recommended_route="human_review",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    return _record(
        dataset=target,
        failure_type="unknown",
        message=combined_message or "Downstream failure could not be classified.",
        root_cause="unknown",
        recommended_route="human_review",
        artifact_ids=artifact_ids,
        repair_attempt=repair_attempt,
    )


def write_downstream_failure_report(
    *,
    study_dir: str | Path,
    study_id: str,
    run_id: str,
    dataset: str,
    failure_records: list[FailureRecord],
    validation_report: dict[str, Any] | None = None,
    status: str = "failed",
) -> ArtifactRef:
    """Write the cumulative downstream failure report for one dataset."""

    root = Path(study_dir)
    target = dataset.strip().upper()
    target_lower = target.lower()
    report_path = root / "runs" / run_id / "diagnostics" / f"{target_lower}_failure_report.json"
    report_path.parent.mkdir(parents=True, exist_ok=True)
    latest = failure_records[-1] if failure_records else None
    payload = {
        "study_id": study_id,
        "run_id": run_id,
        "dataset": target,
        "status": status,
        "failure_count": len(failure_records),
        "latest_failure_id": latest.failure_id if latest else None,
        "latest_root_cause": latest.root_cause if latest else None,
        "latest_recommended_route": latest.recommended_route if latest else None,
        "repair_attempts_used": max((record.repair_attempt for record in failure_records), default=0),
        "failures": [record.model_dump(mode="json") for record in failure_records],
        "validation_report": validation_report or {},
    }
    report_path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")
    return ArtifactRef(
        artifact_id=f"failure_report_{study_id.lower()}_{run_id}_{target_lower}",
        kind="tool_log",
        path=str(report_path.as_posix()),
        sha256=f"sha256:{sha256_file(report_path)}",
        dataset=target,
        format="json",
        role="audit",
        metadata={
            "failure_count": len(failure_records),
            "latest_root_cause": latest.root_cause if latest else None,
            "latest_recommended_route": latest.recommended_route if latest else None,
        },
    )


def _record(
    *,
    dataset: str,
    failure_type: str,
    message: str,
    root_cause: str,
    recommended_route: str,
    artifact_ids: list[str],
    repair_attempt: int,
) -> FailureRecord:
    return FailureRecord(
        failure_id=f"failure_{dataset.lower()}_{repair_attempt}_{_safe_id(root_cause)}",
        dataset=dataset,
        node="diagnose_downstream_failure",
        failure_type=failure_type,
        message=_short(message),
        artifact_ids=artifact_ids,
        root_cause=root_cause,
        recommended_route=recommended_route,
        repair_attempt=repair_attempt,
    )


def _combined_message(
    *,
    message: str,
    r_result: RRunResult | None,
    validation_report: dict[str, Any] | None,
) -> str:
    parts: list[str] = []
    if message:
        parts.append(message)
    if r_result is not None:
        if r_result.stderr:
            parts.append(r_result.stderr)
        elif r_result.stdout and not r_result.success:
            parts.append(r_result.stdout)
    if validation_report:
        errors = validation_report.get("errors") or []
        if errors:
            parts.append("; ".join(str(error) for error in errors))
    return _short(" | ".join(part.strip() for part in parts if part and part.strip()))


def _looks_like_missing_source_variable(lower_message: str) -> bool:
    patterns = [
        "undefined columns selected",
        "unknown or uninitialised column",
        "can't subset columns that don't exist",
        "column `",
        "object '",
        "object \"",
        "not found",
        "missing source variable",
        "missing required columns",
    ]
    return any(pattern in lower_message for pattern in patterns)


def _looks_like_dependency_profile_error(lower_message: str) -> bool:
    patterns = [
        "profile not fully available",
        "artifact missing",
        "dependency",
        "found_but_unusable",
    ]
    return any(pattern in lower_message for pattern in patterns)


def _looks_like_r_environment_error(lower_message: str, exit_code: int) -> bool:
    patterns = [
        "rscript is not available",
        "failed to start rscript",
        "no such file or directory",
        "the system cannot find the file specified",
    ]
    return exit_code in {126, 127} or any(pattern in lower_message for pattern in patterns)


def _short(message: str, *, limit: int = 1200) -> str:
    cleaned = " ".join(str(message).split())
    if len(cleaned) <= limit:
        return cleaned
    return cleaned[: limit - 3] + "..."


def _safe_id(value: str) -> str:
    safe = "".join(character.lower() if character.isalnum() else "_" for character in value)
    safe = "_".join(part for part in safe.split("_") if part)
    return safe or "unknown"
