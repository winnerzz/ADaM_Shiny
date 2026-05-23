"""Failure diagnosis helpers for the ADSL minimal loop."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from adam_agent.schemas.routing import FailureRecord
from adam_agent.tools.r_runner import RRunResult


def diagnose_adsl_failure(
    *,
    stage: str,
    message: str = "",
    r_result: RRunResult | None = None,
    validation_report: dict[str, Any] | None = None,
    artifact_ids: list[str] | None = None,
    repair_attempt: int = 0,
) -> FailureRecord:
    """Classify a Phase 6 MVP ADSL failure into a routable record."""

    artifact_ids = artifact_ids or []
    combined_message = _combined_message(message=message, r_result=r_result, validation_report=validation_report)
    lower_message = combined_message.lower()

    if stage in {"scan_inputs", "profile_inputs"}:
        return _record(
            failure_type="input_error",
            message=combined_message or "ADSL input preparation failed",
            root_cause=_input_root_cause(lower_message),
            recommended_route="fail",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    if stage == "draft_spec":
        root_cause = "source_variable_missing" if _looks_like_missing_source_variable(lower_message) else "spec_draft_error"
        return _record(
            failure_type="spec_error",
            message=combined_message or "ADSL starter spec drafting failed",
            root_cause=root_cause,
            recommended_route="revise_spec",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    if r_result is not None and not r_result.success:
        if _looks_like_missing_source_variable(lower_message):
            return _record(
                failure_type="spec_error",
                message=combined_message,
                root_cause="source_variable_missing",
                recommended_route="revise_spec",
                artifact_ids=artifact_ids,
                repair_attempt=repair_attempt,
            )
        return _record(
            failure_type="sandbox_error",
            message=combined_message or "R execution failed",
            root_cause="r_runtime_error",
            recommended_route="fail",
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    if validation_report and validation_report.get("status") == "fail":
        errors = " ".join(str(error) for error in validation_report.get("errors", []))
        lower_errors = errors.lower()
        if "missing required columns" in lower_errors:
            root_cause = "output_contract_violation"
            route = "revise_spec"
        elif "usubjid" in lower_errors:
            root_cause = "key_integrity_error"
            route = "human_review"
        else:
            root_cause = "validation_failure"
            route = "human_review"
        return _record(
            failure_type="validation_error",
            message=combined_message or errors or "ADSL validation failed",
            root_cause=root_cause,
            recommended_route=route,
            artifact_ids=artifact_ids,
            repair_attempt=repair_attempt,
        )

    return _record(
        failure_type="unknown",
        message=combined_message or "ADSL failure could not be classified",
        root_cause="unknown",
        recommended_route="human_review",
        artifact_ids=artifact_ids,
        repair_attempt=repair_attempt,
    )


def write_failure_report(record: FailureRecord, path: str | Path) -> Path:
    """Write a failure report JSON artifact."""

    output_path = Path(path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(record.model_dump(mode="json"), indent=2, sort_keys=True), encoding="utf-8")
    return output_path


def _record(
    *,
    failure_type: str,
    message: str,
    root_cause: str,
    recommended_route: str,
    artifact_ids: list[str],
    repair_attempt: int,
) -> FailureRecord:
    return FailureRecord(
        failure_id=f"failure_adsl_{_safe_id(root_cause)}",
        dataset="ADSL",
        node="diagnose_adsl_failure",
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


def _input_root_cause(lower_message: str) -> str:
    if "requires input_sdtm" in lower_message or "missing" in lower_message:
        return "missing_required_input"
    if "haven" in lower_message or "unsupported" in lower_message or "profiling" in lower_message:
        return "unsupported_or_unreadable_input"
    return "input_preparation_error"


def _looks_like_missing_source_variable(lower_message: str) -> bool:
    patterns = [
        "object '",
        "object \"",
        "not found",
        "undefined columns selected",
        "unknown or uninitialised column",
        "missing required columns",
        "requires dm.",
    ]
    return any(pattern in lower_message for pattern in patterns)


def _short(message: str, *, limit: int = 1200) -> str:
    cleaned = " ".join(str(message).split())
    if len(cleaned) <= limit:
        return cleaned
    return cleaned[: limit - 3] + "..."


def _safe_id(value: str) -> str:
    safe = "".join(character.lower() if character.isalnum() else "_" for character in value)
    safe = "_".join(part for part in safe.split("_") if part)
    return safe or "unknown"
