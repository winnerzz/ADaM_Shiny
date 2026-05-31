"""Output-quality read-model helpers.

These helpers translate graph state into user-facing quality signals. They do
not decide clinical correctness and they do not mutate run state.
"""

from __future__ import annotations

from collections.abc import Iterable, Mapping
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


def study_output_quality_rollup(
    dataset_progress: Iterable[Mapping[str, Any]],
    *,
    target_datasets: Iterable[str] | None = None,
) -> dict[str, Any]:
    """Summarize target output quality without changing graph state."""

    by_dataset: dict[str, Mapping[str, Any]] = {}
    ordered_datasets: list[str] = []
    for item in dataset_progress:
        dataset = str(item.get("dataset") or "").strip().upper()
        if not dataset:
            continue
        by_dataset[dataset] = item
        if dataset not in ordered_datasets:
            ordered_datasets.append(dataset)

    target_names = _normalized_names(target_datasets)
    if not target_names:
        target_names = ordered_datasets

    status_counts: dict[str, int] = {}
    real_runtime_outputs = 0
    review_only_outputs = 0
    terminal_failure_outputs = 0
    not_completed_outputs = 0
    runtime_dependency_eligible_outputs = 0

    for dataset in target_names:
        item = by_dataset.get(dataset, {})
        quality = _mapping(item.get("output_quality"))
        quality_status = str(quality.get("quality_status") or "not_completed").strip() or "not_completed"
        status_counts[quality_status] = status_counts.get(quality_status, 0) + 1
        if quality_status == "real_runtime_output":
            real_runtime_outputs += 1
        elif quality_status in {"structural_stub", "not_real_derivation"}:
            review_only_outputs += 1
        elif quality_status == "terminal_failure":
            terminal_failure_outputs += 1
        else:
            not_completed_outputs += 1
        if quality.get("runtime_dependency_eligible") is True:
            runtime_dependency_eligible_outputs += 1

    total_targets = len(target_names)
    if total_targets == 0:
        completion_quality = "no_targets"
    elif terminal_failure_outputs:
        completion_quality = "terminal_failure_present"
    elif not_completed_outputs:
        completion_quality = "in_progress"
    elif real_runtime_outputs == total_targets:
        completion_quality = "real_runtime_complete"
    elif review_only_outputs == total_targets:
        completion_quality = "review_only_complete"
    elif real_runtime_outputs and review_only_outputs:
        completion_quality = "mixed_output_quality_complete"
    else:
        completion_quality = "unknown"

    warnings: list[str] = []
    if review_only_outputs:
        warnings.append(
            "One or more planned outputs are review-only/demo outputs and cannot satisfy downstream runtime dependencies."
        )
    if terminal_failure_outputs:
        warnings.append("One or more planned outputs ended in terminal failure.")
    if not_completed_outputs:
        warnings.append("One or more planned outputs are not complete yet.")

    return {
        "completion_quality": completion_quality,
        "total_targets": total_targets,
        "real_runtime_outputs": real_runtime_outputs,
        "review_only_outputs": review_only_outputs,
        "terminal_failure_outputs": terminal_failure_outputs,
        "not_completed_outputs": not_completed_outputs,
        "runtime_dependency_eligible_outputs": runtime_dependency_eligible_outputs,
        "quality_status_counts": status_counts,
        "warnings": warnings,
    }


def _mapping(value: Any) -> dict[str, Any]:
    return dict(value) if isinstance(value, Mapping) else {}


def _normalized_names(values: Iterable[str] | None) -> list[str]:
    names: list[str] = []
    for value in values or []:
        name = str(value or "").strip().upper()
        if name and name not in names:
            names.append(name)
    return names
