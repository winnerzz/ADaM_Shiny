"""Output-quality read-model helpers.

These helpers translate graph state into user-facing quality signals. They do
not decide clinical correctness and they do not mutate run state.
"""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any


def dataset_output_quality(
    *,
    status: str = "",
    code_state: Mapping[str, Any] | None = None,
    execution_state: Mapping[str, Any] | None = None,
    validation_summary: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Return a small quality signal for progress and review read models."""

    code = dict(code_state or {})
    execution = dict(execution_state or {})
    validation = dict(validation_summary or {})
    code_generation_quality = _mapping(code.get("generation_quality"))
    execution_generation_quality = _mapping(execution.get("generation_quality"))

    status_value = str(status or "").strip()
    execution_status = str(execution.get("status") or "").strip()
    validation_status = str(validation.get("status") or "").strip()
    terminal_failure = (
        status_value == "terminal_failure"
        or execution_status == "terminal_failure"
        or execution.get("terminal_failure") is True
        or validation.get("terminal_failure") is True
    )
    structural_stub = (
        status_value == "completed_stub"
        or execution_status == "completed_stub"
        or validation_status in {"structural_stub_pass", "passed_stub"}
        or execution.get("stubbed_r_execution") is True
    )
    not_real_derivation = (
        execution.get("not_real_derivation") is True
        or execution_generation_quality.get("not_real_derivation") is True
        or code_generation_quality.get("not_real_derivation") is True
    )
    partial_output_usable = execution.get("partial_output_usable")
    if partial_output_usable is None:
        partial_output_usable = validation.get("partial_output_usable")

    if terminal_failure:
        quality_status = "terminal_failure"
    elif structural_stub:
        quality_status = "structural_stub"
    elif not_real_derivation:
        quality_status = "not_real_derivation"
    elif status_value == "completed" or execution_status == "completed":
        quality_status = "real_runtime_output"
    else:
        quality_status = "not_completed"

    runtime_dependency_eligible = quality_status == "real_runtime_output" and partial_output_usable is not False
    warnings: list[str] = []
    if terminal_failure:
        warnings.append("Execution ended in terminal failure; any output is not treated as usable runtime evidence.")
    if structural_stub:
        warnings.append("This is a structural stub/demo output. It is visible for review, but cannot satisfy downstream runtime dependencies.")
    if not_real_derivation:
        warnings.append("Code generation was marked not_real_derivation, such as mock/offline generation. Review it, but do not treat it as downstream runtime evidence.")
    if partial_output_usable is False:
        warnings.append("Execution marked partial output as unusable.")

    provider = (
        execution_generation_quality.get("llm_provider")
        or code_generation_quality.get("llm_provider")
        or None
    )
    model = execution_generation_quality.get("llm_model") or code_generation_quality.get("llm_model") or None

    return {
        "quality_status": quality_status,
        "runtime_dependency_eligible": runtime_dependency_eligible,
        "structural_stub": structural_stub,
        "not_real_derivation": not_real_derivation,
        "terminal_failure": terminal_failure,
        "provider": provider,
        "model": model,
        "warnings": warnings,
    }


def _mapping(value: Any) -> dict[str, Any]:
    return dict(value) if isinstance(value, Mapping) else {}
