"""Deterministic generated-vs-reference ADaM comparison helpers."""

from __future__ import annotations

import csv
import io
import json
from pathlib import Path
from typing import Any


def compare_dataset_files(dataset: str, generated_path: Path | None, reference_path: Path | None) -> dict[str, Any]:
    """Compare generated and reference ADaM CSV files without mutating state."""

    target = dataset.strip().upper()
    if generated_path is None or not generated_path.exists():
        return {
            "dataset": target,
            "status": "missing_generated",
            "note": "Generated ADaM output is not available yet.",
        }
    if reference_path is None or not reference_path.exists():
        return {
            "dataset": target,
            "status": "missing_reference",
            "generated_file": generated_path.name,
            "note": "No reference ADaM was found for this dataset.",
        }
    if generated_path.suffix.lower() != ".csv" or reference_path.suffix.lower() != ".csv":
        return {
            "dataset": target,
            "status": "not_supported",
            "generated_file": generated_path.name,
            "reference_file": reference_path.name,
            "note": "Compare is currently implemented for CSV generated/reference ADaM tables.",
        }
    generated = _read_csv_table(generated_path)
    reference = _read_csv_table(reference_path)
    if generated["status"] != "ok":
        return {
            "dataset": target,
            "status": "error",
            "generated_file": generated_path.name,
            "reference_file": reference_path.name,
            "note": generated["note"],
        }
    if reference["status"] != "ok":
        return {
            "dataset": target,
            "status": "error",
            "generated_file": generated_path.name,
            "reference_file": reference_path.name,
            "note": reference["note"],
        }

    generated_columns = generated["columns"]
    reference_columns = reference["columns"]
    common_columns = [column for column in generated_columns if column in reference_columns]
    generated_only_columns = [column for column in generated_columns if column not in reference_columns]
    reference_only_columns = [column for column in reference_columns if column not in generated_columns]
    key_columns = choose_compare_keys(target, common_columns)
    generated_rows = generated["rows"]
    reference_rows = reference["rows"]
    row_count_generated = len(generated_rows)
    row_count_reference = len(reference_rows)
    generated_only_keys: list[str] = []
    reference_only_keys: list[str] = []
    mismatch_samples: list[dict[str, str]] = []
    compared_cells = 0
    matched_rows = 0
    mismatch_count = 0

    if key_columns:
        generated_by_key = _rows_by_key(generated_rows, key_columns)
        reference_by_key = _rows_by_key(reference_rows, key_columns)
        generated_key_set = set(generated_by_key)
        reference_key_set = set(reference_by_key)
        generated_only_keys = sorted(generated_key_set - reference_key_set)[:20]
        reference_only_keys = sorted(reference_key_set - generated_key_set)[:20]
        for key in sorted(generated_key_set & reference_key_set):
            matched_rows += 1
            generated_row = generated_by_key[key]
            reference_row = reference_by_key[key]
            for column in common_columns:
                if column in key_columns:
                    continue
                compared_cells += 1
                generated_value = _string_cell(generated_row.get(column))
                reference_value = _string_cell(reference_row.get(column))
                if generated_value != reference_value:
                    mismatch_count += 1
                    if len(mismatch_samples) < 25:
                        mismatch_samples.append(
                            {
                                "key": key,
                                "column": column,
                                "generated": generated_value,
                                "reference": reference_value,
                            }
                        )
    else:
        for index, (generated_row, reference_row) in enumerate(zip(generated_rows, reference_rows), start=1):
            matched_rows += 1
            for column in common_columns:
                compared_cells += 1
                generated_value = _string_cell(generated_row.get(column))
                reference_value = _string_cell(reference_row.get(column))
                if generated_value != reference_value:
                    mismatch_count += 1
                    if len(mismatch_samples) < 25:
                        mismatch_samples.append(
                            {
                                "key": f"row {index}",
                                "column": column,
                                "generated": generated_value,
                                "reference": reference_value,
                            }
                        )

    status = "match"
    if (
        row_count_generated != row_count_reference
        or generated_only_columns
        or reference_only_columns
        or generated_only_keys
        or reference_only_keys
        or mismatch_count
    ):
        status = "differences"
    return {
        "dataset": target,
        "status": status,
        "generated_file": generated_path.name,
        "reference_file": reference_path.name,
        "row_count_generated": row_count_generated,
        "row_count_reference": row_count_reference,
        "row_count_delta": row_count_generated - row_count_reference,
        "generated_only_columns": generated_only_columns,
        "reference_only_columns": reference_only_columns,
        "common_columns": common_columns,
        "key_columns": key_columns,
        "matched_rows": matched_rows,
        "generated_only_keys": generated_only_keys,
        "reference_only_keys": reference_only_keys,
        "compared_cells": compared_cells,
        "mismatch_count": mismatch_count,
        "mismatch_samples": mismatch_samples,
        "note": "Initial CSV compare. This checks structure and sampled cell differences, not full clinical rule conformance.",
    }


def usable_generated_output_path(run_dir: Path, dataset: str) -> Path | None:
    """Return a generated output only when validation says it is usable."""

    target = dataset.strip().upper()
    output_path = run_dir / "outputs" / f"{target.lower()}.csv"
    if not output_path.exists() or not output_path.is_file():
        return None
    report = _read_json_if_exists(run_dir / "validation" / f"{target.lower()}_validation_report.json")
    if report.get("status") not in {"pass", "structural_stub_pass"}:
        return None
    if report.get("terminal_failure") is True or report.get("partial_output_usable") is False:
        return None
    return output_path


def reference_adam_path(study_dir: Path, dataset: str) -> Path | None:
    """Find a local reference ADaM table for comparison evidence."""

    folder = study_dir / "reference_adam"
    for suffix in [".csv", ".sas7bdat"]:
        for name in [dataset.lower(), dataset.upper()]:
            path = folder / f"{name}{suffix}"
            if path.exists():
                return path
    return None


def choose_compare_keys(dataset: str, common_columns: list[str]) -> list[str]:
    """Select stable row keys for a structural compare report."""

    upper_to_original = {column.upper(): column for column in common_columns}
    candidates = {
        "ADAE": ["USUBJID", "AESEQ"],
        "ADCM": ["USUBJID", "CMSEQ"],
        "ADLB": ["USUBJID", "PARAMCD", "AVISITN", "ADT", "ATPTN"],
        "ADEX": ["USUBJID", "EXSEQ"],
        "ADEG": ["USUBJID", "PARAMCD", "AVISITN", "ADT", "ATPTN"],
        "ADSL": ["USUBJID"],
    }
    chosen = [upper_to_original[key] for key in candidates.get(dataset.upper(), ["USUBJID"]) if key in upper_to_original]
    if chosen:
        return chosen
    if "USUBJID" in upper_to_original:
        return [upper_to_original["USUBJID"]]
    return []


def _read_csv_table(path: Path) -> dict[str, Any]:
    try:
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            text = handle.read()
    except (OSError, UnicodeDecodeError) as exc:
        return {"status": "error", "note": str(exc), "columns": [], "rows": []}
    try:
        reader = csv.DictReader(io.StringIO(text))
        columns = list(reader.fieldnames or [])
        rows = [{column: _string_cell(row.get(column)) for column in columns} for row in reader]
    except csv.Error as exc:
        return {"status": "error", "note": str(exc), "columns": [], "rows": []}
    return {"status": "ok", "note": "", "columns": columns, "rows": rows}


def _rows_by_key(rows: list[dict[str, str]], key_columns: list[str]) -> dict[str, dict[str, str]]:
    keyed: dict[str, dict[str, str]] = {}
    for index, row in enumerate(rows, start=1):
        key = "|".join(_string_cell(row.get(column)) for column in key_columns)
        if not key.strip("|"):
            key = f"row {index}"
        if key in keyed:
            key = f"{key}#{index}"
        keyed[key] = row
    return keyed


def _read_json_if_exists(path: Path) -> dict[str, Any]:
    if not path.exists() or not path.is_file():
        return {}
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, UnicodeDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def _string_cell(value: Any) -> str:
    if value is None:
        return ""
    return str(value)
