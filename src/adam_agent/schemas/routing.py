"""Routing and failure schemas."""

from __future__ import annotations

from datetime import datetime
from typing import Literal

from pydantic import Field

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now


RouteName = Literal[
    "continue",
    "silent_pass",
    "human_review",
    "repair_code",
    "revise_spec",
    "revise_lineage",
    "request_reference",
    "fail",
]

FailureType = Literal[
    "input_error",
    "spec_error",
    "lineage_error",
    "code_error",
    "sandbox_error",
    "validation_error",
    "compare_error",
    "llm_error",
    "unknown",
]


class RouteDecision(StrictBaseModel):
    """Lightweight record of why the graph chose a route."""

    decision_id: NonEmptyStr
    node: NonEmptyStr
    dataset: str | None = None
    decision: RouteName
    reason: NonEmptyStr
    confidence: float | None = Field(default=None, ge=0.0, le=1.0)
    created_at: datetime = Field(default_factory=utc_now)


class FailureRecord(StrictBaseModel):
    """Structured failure and diagnosis record."""

    failure_id: NonEmptyStr
    dataset: str | None = None
    node: NonEmptyStr
    failure_type: FailureType
    message: NonEmptyStr
    artifact_ids: list[str] = Field(default_factory=list)
    root_cause: str | None = None
    recommended_route: RouteName | None = None
    repair_attempt: int = Field(default=0, ge=0)
    created_at: datetime = Field(default_factory=utc_now)
