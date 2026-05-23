"""Lightweight SDTM profiling tools."""

from __future__ import annotations

import csv
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Literal


ProfileStatus = Literal["ok", "not_implemented_yet", "error"]


@dataclass
class DatasetProfile:
    """Small metadata profile for an input dataset."""

    dataset: str
    path: str
    format: str
    status: ProfileStatus
    columns: list[str] = field(default_factory=list)
    row_count: int | None = None
    sample_rows: list[dict[str, str]] = field(default_factory=list)
    message: str = ""


class SDTMReader:
    """Read lightweight metadata from MVP source data files."""

    def profile(self, path: str | Path, *, dataset: str | None = None, sample_rows: int = 0) -> DatasetProfile:
        source_path = Path(path)
        suffix = source_path.suffix.lower()
        dataset_name = dataset or source_path.stem.upper()

        if suffix == ".csv":
            return self._profile_csv(source_path, dataset_name, sample_rows)
        if suffix == ".sas7bdat":
            return DatasetProfile(
                dataset=dataset_name,
                path=str(source_path.as_posix()),
                format="sas7bdat",
                status="not_implemented_yet",
                message="sas7bdat profiling is not implemented in Phase 4",
            )
        return DatasetProfile(
            dataset=dataset_name,
            path=str(source_path.as_posix()),
            format=suffix.lstrip("."),
            status="error",
            message=f"Unsupported format for SDTM profiling: {suffix}",
        )

    def _profile_csv(self, path: Path, dataset: str, sample_rows: int) -> DatasetProfile:
        if sample_rows < 0:
            raise ValueError("sample_rows cannot be negative")

        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.DictReader(handle)
            columns = list(reader.fieldnames or [])
            samples: list[dict[str, str]] = []
            row_count = 0
            for row in reader:
                row_count += 1
                if len(samples) < sample_rows:
                    samples.append({column: _string_value(row.get(column)) for column in columns})

        return DatasetProfile(
            dataset=dataset,
            path=str(path.as_posix()),
            format="csv",
            status="ok",
            columns=columns,
            row_count=row_count,
            sample_rows=samples,
        )


def _string_value(value: Any) -> str:
    if value is None:
        return ""
    return str(value)
