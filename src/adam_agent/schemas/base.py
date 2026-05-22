"""Shared schema primitives."""

from __future__ import annotations

from datetime import UTC, datetime
from typing import Annotated

from pydantic import AfterValidator, BaseModel, ConfigDict, Field


def strip_non_empty(value: str) -> str:
    """Strip strings and reject empty values."""

    stripped = value.strip()
    if not stripped:
        raise ValueError("value must not be empty or whitespace")
    return stripped


NonEmptyStr = Annotated[str, Field(min_length=1), AfterValidator(strip_non_empty)]
Sha256 = Annotated[str, Field(pattern=r"^sha256:[A-Fa-f0-9]{64}$")]


class StrictBaseModel(BaseModel):
    """Base class for checkpoint/API/audit-facing schemas."""

    model_config = ConfigDict(extra="forbid", validate_assignment=True)


def utc_now() -> datetime:
    """Return a timezone-aware timestamp for default factory use."""

    return datetime.now(UTC)
