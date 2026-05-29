"""Render compact, auditable prompts from full LLM context packages."""

from __future__ import annotations

import csv
import io
import json
from pathlib import Path
from typing import Any

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.tools.artifacts import sha256_file


DERIVATION_LIMIT = 500
SOURCE_LIMIT = 250
LABEL_LIMIT = 80
SAMPLE_VALUE_LIMIT = 120
MAX_SAMPLE_ROWS_IN_PROMPT = 3


def compact_prompt_from_context(context: dict[str, Any]) -> str:
    """Build a token-conscious prompt while preserving variable-level spec logic."""

    target = str(context.get("target_dataset") or "UNKNOWN").upper()
    sections = [
        "Use this compact ADaM generation context. Return only the strict JSON object requested by the system instructions.",
        "",
        "## Task",
        f"Study: {context.get('study_id') or 'unknown'}",
        f"Run: {context.get('run_id') or 'unknown'}",
        f"Target ADaM dataset: {target}",
        "",
        "## Runtime Contract",
        _runtime_contract_text(context.get("runtime_contract", {})),
        "",
        "## Target Spec",
        _compact_spec_text(context.get("target_spec"), target=target),
        "",
        "## Source Data Profiles",
        _profiles_text(context.get("source_dataset_profiles", {}), title="SDTM source"),
        "",
        "## Resolved ADaM Dependencies",
        _profiles_text(context.get("resolved_dependencies", {}), title="ADaM dependency"),
        "",
        "## Exposure Policy",
        _json_line(context.get("exposure", {})),
    ]
    warnings = [str(item) for item in context.get("warnings", []) if str(item).strip()]
    if warnings:
        sections.extend(["", "## Context Warnings", "\n".join(f"- {warning}" for warning in warnings)])
    return "\n".join(sections).strip() + "\n"


def repair_prompt_from_failure(
    *,
    compact_context_prompt: str,
    raw_response_text: str,
    generated_code: str,
    validation_report: dict[str, Any],
    r_result: Any | None,
    failure_record: Any,
) -> str:
    """Build a compact repair prompt that avoids embedding the full context JSON."""

    failure_payload = failure_record.model_dump(mode="json") if hasattr(failure_record, "model_dump") else failure_record
    r_payload = {
        "exit_code": getattr(r_result, "exit_code", None),
        "stdout": getattr(r_result, "stdout", "") if r_result else "",
        "stderr": getattr(r_result, "stderr", "") if r_result else "",
    }
    return (
        "Repair the previous generated R response. Return the same strict JSON contract. "
        "Do not change the clinical intent. If the evidence shows a missing source variable "
        "or spec conflict, report that as a risk point instead of inventing a derivation.\n\n"
        "## Compact Original Context\n"
        f"{compact_context_prompt.strip()}\n\n"
        "## Failure\n"
        f"{json.dumps(failure_payload, indent=2, sort_keys=True)}\n\n"
        "## Validation Report\n"
        f"{json.dumps(validation_report, indent=2, sort_keys=True)}\n\n"
        "## R Result\n"
        f"{json.dumps(r_payload, indent=2, sort_keys=True)}\n\n"
        "## Previous Raw LLM Response\n"
        f"{_truncate(raw_response_text, 12000)}\n\n"
        "## Previous R Code\n"
        f"{_truncate(generated_code, 12000)}\n"
    )


def write_compact_prompt_artifact(
    *,
    study_id: str,
    run_id: str,
    target_dataset: str,
    study_dir: str | Path,
    prompt: str,
    source_context_artifact_id: str | None = None,
) -> ArtifactRef:
    """Persist the exact compact prompt sent to the provider."""

    target = target_dataset.strip().upper()
    target_lower = target.lower()
    prompt_path = Path(study_dir) / "runs" / run_id / "llm" / f"{target_lower}_compact_prompt.txt"
    prompt_path.parent.mkdir(parents=True, exist_ok=True)
    prompt_path.write_text(prompt, encoding="utf-8")
    return ArtifactRef(
        artifact_id=f"llm_prompt_compact_{study_id.lower()}_{run_id}_{target_lower}",
        kind="llm_prompt",
        path=str(prompt_path.as_posix()),
        sha256=f"sha256:{sha256_file(prompt_path)}",
        dataset=target,
        format="txt",
        role="audit",
        metadata={
            "compact_prompt": True,
            "source_context_artifact_id": source_context_artifact_id,
        },
    )


def _runtime_contract_text(contract: Any) -> str:
    if not isinstance(contract, dict):
        return "- Runtime contract unavailable."
    keys = [
        "language",
        "working_directory",
        "relative_output_path",
        "runtime_output_path",
        "output_path",
        "code_path",
        "write_only_to_run_dir",
        "no_network",
        "input_path_policy",
        "csv_read_policy",
    ]
    lines = []
    for key in keys:
        if key in contract:
            lines.append(f"- {key}: {contract[key]}")
    return "\n".join(lines) if lines else "- Runtime contract unavailable."


def _compact_spec_text(target_spec: Any, *, target: str) -> str:
    if not isinstance(target_spec, dict):
        return f"No approved input spec was found for {target}. Generate only if other evidence is sufficient and flag this as a risk point."

    rows = _spec_rows(target_spec, target=target)
    if not rows:
        text = str(target_spec.get("text") or "").strip()
        if text:
            return "Spec text could not be parsed into rows. Use this truncated evidence:\n" + _truncate(text, 6000)
        return f"Spec artifact was present for {target}, but no readable variable rows were found. Flag this as a risk point."

    lines = ["variable|type|source|derivation"]
    copied: list[str] = []
    for row in rows:
        variable = _clean_cell(row.get("variable") or row.get("Variable"))
        kind = _clean_cell(row.get("type") or row.get("Type"))
        source = _strip_sdtm_prefix(_truncate(_source_text(row), SOURCE_LIMIT))
        derivation = _truncate(
            _clean_cell(row.get("derivation") or row.get("Derivation") or row.get("derivation_rule")),
            DERIVATION_LIMIT,
        )
        label = _truncate(_clean_cell(row.get("label") or row.get("Label")), LABEL_LIMIT)
        if not derivation and label:
            derivation = f"Label: {label}"
        if kind.lower() == "copied" and source:
            copied.append(f"{variable}<-{source}")
            continue
        lines.append("|".join([variable, kind, source, derivation]))
    if copied:
        lines.insert(1, "COPIED: " + ", ".join(copied))
    return "\n".join(lines)


def _spec_rows(target_spec: dict[str, Any], *, target: str) -> list[dict[str, Any]]:
    parsed = target_spec.get("json")
    if isinstance(parsed, dict):
        rows = _rows_from_json_spec(parsed, target=target)
        if rows:
            return rows
    if isinstance(parsed, list):
        rows = []
        for item in parsed:
            if isinstance(item, dict):
                rows.extend(_rows_from_json_spec(item, target=target))
        if rows:
            return rows

    text = str(target_spec.get("text") or "")
    if not text.strip():
        return []
    fmt = str(target_spec.get("format") or "").lower()
    if fmt == "csv" or _looks_like_csv_spec(text):
        return _rows_from_csv_spec(text, target=target)
    return []


def _rows_from_json_spec(payload: dict[str, Any], *, target: str) -> list[dict[str, Any]]:
    if "variables" in payload and isinstance(payload["variables"], list):
        dataset = str(payload.get("dataset") or payload.get("Dataset") or target).upper()
        if dataset != target:
            return []
        return [row for row in payload["variables"] if isinstance(row, dict)]
    rows = []
    for key, value in payload.items():
        if str(key).upper() != target:
            continue
        if isinstance(value, dict):
            rows.extend(_rows_from_json_spec(value, target=target))
        elif isinstance(value, list):
            rows.extend(row for row in value if isinstance(row, dict))
    return rows


def _rows_from_csv_spec(text: str, *, target: str) -> list[dict[str, Any]]:
    try:
        reader = csv.DictReader(io.StringIO(text))
        rows = []
        for row in reader:
            dataset = str(row.get("Dataset") or row.get("dataset") or target).strip().upper()
            if dataset == target:
                rows.append(dict(row))
        return rows
    except csv.Error:
        return []


def _profiles_text(profiles: Any, *, title: str) -> str:
    if not isinstance(profiles, dict) or not profiles:
        return f"No {title} profiles included."
    parts = []
    for dataset, profile in sorted(profiles.items()):
        if not isinstance(profile, dict):
            continue
        columns = [str(column) for column in profile.get("columns", [])]
        status = profile.get("status") or "unknown"
        fmt = profile.get("format") or "unknown"
        row_count = profile.get("row_count")
        message = str(profile.get("message") or "").strip()
        read_path = str(profile.get("read_path") or profile.get("run_relative_path") or "").strip()
        parts.append(
            "\n".join(
                [
                    f"### {str(dataset).upper()} [{fmt}; status={status}; rows={row_count if row_count is not None else 'unknown'}; cols={len(columns)}]",
                    f"Read path from R working directory: {read_path}" if read_path else "",
                    "Columns: " + (", ".join(columns) if columns else "(columns unavailable)"),
                    _sample_rows_text(profile.get("sample_rows", [])),
                    f"Note: {message}" if message else "",
                ]
            ).strip()
        )
    return "\n\n".join(part for part in parts if part) or f"No {title} profiles included."


def _sample_rows_text(sample_rows: Any) -> str:
    if not isinstance(sample_rows, list) or not sample_rows:
        return "Sample rows: not included."
    lines = ["Sample rows:"]
    for index, row in enumerate(sample_rows[:MAX_SAMPLE_ROWS_IN_PROMPT], start=1):
        if not isinstance(row, dict):
            continue
        cells = [f"{key}={_truncate(_clean_cell(value), SAMPLE_VALUE_LIMIT)}" for key, value in row.items()]
        lines.append(f"- row {index}: " + "; ".join(cells))
    return "\n".join(lines)


def _json_line(value: Any) -> str:
    try:
        return json.dumps(value, sort_keys=True)
    except TypeError:
        return str(value)


def _source_text(row: dict[str, Any]) -> str:
    direct = _clean_cell(row.get("source") or row.get("Source"))
    if direct:
        return direct
    domains = _as_string_list(row.get("source_domains") or row.get("source_domain"))
    columns = _as_string_list(row.get("source_columns") or row.get("source_column") or row.get("source_variables") or row.get("source_variable"))
    if domains and columns:
        if len(domains) == 1:
            return ", ".join(f"{domains[0]}.{column}" for column in columns)
        return "; ".join(
            f"{domain}.{column}" for domain in domains for column in columns
        )
    if domains:
        return ", ".join(domains)
    if columns:
        return ", ".join(columns)
    return ""


def _as_string_list(value: Any) -> list[str]:
    if value is None:
        return []
    if isinstance(value, list):
        return [_clean_cell(item) for item in value if _clean_cell(item)]
    cleaned = _clean_cell(value)
    return [cleaned] if cleaned else []


def _looks_like_csv_spec(text: str) -> bool:
    first_line = text.splitlines()[0] if text.splitlines() else ""
    lowered = first_line.lower()
    return "," in first_line and "variable" in lowered and ("derivation" in lowered or "source" in lowered)


def _strip_sdtm_prefix(value: str) -> str:
    return value.removeprefix("SDTM.").removeprefix("sdtm.")


def _clean_cell(value: Any) -> str:
    return " ".join(str(value or "").replace("\r", " ").replace("\n", " ").split())


def _truncate(value: str, limit: int) -> str:
    if len(value) <= limit:
        return value
    return value[: max(0, limit - 15)].rstrip() + " [truncated]"
