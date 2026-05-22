"""Spec schemas for draft and approved ADaM contracts."""

from __future__ import annotations

from datetime import datetime
from typing import Literal

from pydantic import Field, model_validator

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now


SpecVariableType = Literal["character", "numeric", "date", "datetime", "time", "boolean", "unknown"]
SpecApprovalStatus = Literal["draft", "approved", "rejected"]
RiskLevel = Literal["low", "medium", "high"]


class SpecVariable(StrictBaseModel):
    """Variable-level draft or approved ADaM spec record."""

    variable: NonEmptyStr
    label: str = ""
    type: SpecVariableType = "unknown"
    source_domains: list[str] = Field(default_factory=list)
    source_variables: list[str] = Field(default_factory=list)
    derivation: NonEmptyStr
    evidence_ids: list[NonEmptyStr] = Field(default_factory=list)
    approval_ids: list[NonEmptyStr] = Field(default_factory=list)
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    review_required: bool = True
    review_reasons: list[str] = Field(default_factory=list)
    risk_level: RiskLevel = "medium"
    approval_status: SpecApprovalStatus = "draft"
    assumptions: list[str] = Field(default_factory=list)

    @model_validator(mode="after")
    def validate_review_and_approval(self) -> "SpecVariable":
        if not self.evidence_ids:
            raise ValueError("SpecVariable requires at least one evidence_id")
        if self.approval_status == "approved" and not self.approval_ids:
            raise ValueError("approved SpecVariable requires at least one approval_id")
        if self.risk_level == "high" and not self.review_required and not self.approval_ids:
            raise ValueError("high-risk variables require review or approval evidence")
        return self


class SpecDocument(StrictBaseModel):
    """Dataset-level machine-readable ADaM spec document."""

    dataset: NonEmptyStr
    status: SpecApprovalStatus = "draft"
    variables: list[SpecVariable] = Field(default_factory=list)
    source_spec_artifact_id: str | None = None
    created_at: datetime = Field(default_factory=utc_now)
    updated_at: datetime = Field(default_factory=utc_now)

    @model_validator(mode="after")
    def validate_document_status(self) -> "SpecDocument":
        if not self.variables:
            raise ValueError("SpecDocument requires at least one variable")
        if self.status == "approved":
            not_approved = [var.variable for var in self.variables if var.approval_status != "approved"]
            if not_approved:
                raise ValueError(f"approved SpecDocument contains unapproved variables: {not_approved}")
        return self
