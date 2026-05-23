"""Minimal ADSL validation for the Phase 5 starter loop."""

from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import Any


DEFAULT_REQUIRED_COLUMNS = ["USUBJID"]


def validate_adsl_csv(path: str | Path, *, required_columns: list[str] | None = None) -> dict[str, Any]:
    """Validate a generated ADSL CSV with minimal structural checks."""

    adsl_path = Path(path)
    required = required_columns or DEFAULT_REQUIRED_COLUMNS
    checks: list[dict[str, Any]] = []
    warnings: list[str] = []
    errors: list[str] = []

    if not adsl_path.exists():
        return _report(
            checks=[{"name": "output_exists", "status": "fail", "details": {"path": str(adsl_path.as_posix())}}],
            warnings=warnings,
            errors=[f"ADSL output does not exist: {adsl_path}"],
        )

    checks.append({"name": "output_exists", "status": "pass", "details": {"path": str(adsl_path.as_posix())}})
    with adsl_path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        columns = list(reader.fieldnames or [])
        rows = list(reader)

    missing_columns = [column for column in required if column not in columns]
    checks.append(
        {
            "name": "required_columns_present",
            "status": "pass" if not missing_columns else "fail",
            "details": {"required": required, "missing": missing_columns, "columns": columns},
        }
    )
    if missing_columns:
        errors.append(f"Missing required columns: {missing_columns}")

    if "USUBJID" in columns:
        usubjids = [_clean(row.get("USUBJID", "")) for row in rows]
        missing_keys = sum(1 for value in usubjids if not value)
        duplicate_keys = sorted({value for value in usubjids if value and usubjids.count(value) > 1})
        checks.append(
            {
                "name": "usubjid_non_missing",
                "status": "pass" if missing_keys == 0 else "fail",
                "details": {"missing_count": missing_keys},
            }
        )
        checks.append(
            {
                "name": "usubjid_unique",
                "status": "pass" if not duplicate_keys else "fail",
                "details": {"duplicate_usubjid": duplicate_keys},
            }
        )
        if missing_keys:
            errors.append("USUBJID contains missing values")
        if duplicate_keys:
            errors.append("USUBJID is not unique")

    if {"TRTSDT", "TRTEDT"}.issubset(columns):
        order_issues = [
            row.get("USUBJID", "")
            for row in rows
            if _clean(row.get("TRTSDT", ""))
            and _clean(row.get("TRTEDT", ""))
            and _clean(row.get("TRTSDT", "")) > _clean(row.get("TRTEDT", ""))
        ]
        checks.append(
            {
                "name": "trt_dates_order",
                "status": "pass" if not order_issues else "fail",
                "details": {"affected_usubjid": order_issues},
            }
        )
        if order_issues:
            errors.append("TRTSDT is after TRTEDT for at least one subject")

    if "SAFFL" in columns:
        invalid_values = sorted({_clean(row.get("SAFFL", "")) for row in rows} - {"Y", "N", ""})
        checks.append(
            {
                "name": "saffl_expected_values",
                "status": "pass" if not invalid_values else "fail",
                "details": {"invalid_values": invalid_values},
            }
        )
        warnings.append("SAFFL validation checks values only; production safety-population logic is study-specific.")
        if invalid_values:
            errors.append("SAFFL contains unexpected values")

    return _report(checks=checks, warnings=warnings, errors=errors)


def write_validation_report(report: dict[str, Any], path: str | Path) -> Path:
    """Write a validation report as JSON."""

    output_path = Path(path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(report, indent=2, sort_keys=True), encoding="utf-8")
    return output_path


def _report(*, checks: list[dict[str, Any]], warnings: list[str], errors: list[str]) -> dict[str, Any]:
    return {
        "dataset": "ADSL",
        "status": "pass" if not errors else "fail",
        "checks": checks,
        "warnings": warnings,
        "errors": errors,
    }


def _clean(value: str | None) -> str:
    if value is None:
        return ""
    return str(value).strip()
