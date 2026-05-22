"""Approval schemas for draft-to-approved spec transitions."""

from __future__ import annotations

from datetime import datetime
from typing import Literal

from pydantic import Field, model_validator

from adam_agent.schemas.base import NonEmptyStr, Sha256, StrictBaseModel, utc_now


ApprovalMode = Literal["human", "silent_pass", "demo_only_no_review"]


class ApprovalRecord(StrictBaseModel):
    """Approval metadata that prevents draft specs from silently becoming approved."""

    approval_id: NonEmptyStr
    dataset: NonEmptyStr
    reviewed_variables: list[NonEmptyStr] = Field(default_factory=list)
    approval_mode: ApprovalMode
    approved_by: NonEmptyStr
    approved_at: datetime = Field(default_factory=utc_now)
    source_draft_spec_hash: Sha256
    unresolved_assumptions: list[str] = Field(default_factory=list)
    rule_id: str | None = None
    notes: str = ""

    @model_validator(mode="after")
    def validate_approval_record(self) -> "ApprovalRecord":
        if not self.reviewed_variables:
            raise ValueError("ApprovalRecord requires reviewed_variables")
        if self.approval_mode == "silent_pass" and not self.rule_id:
            raise ValueError("silent_pass approval requires rule_id")
        if self.approval_mode == "demo_only_no_review" and not self.notes:
            raise ValueError("demo_only_no_review approval requires notes")
        return self
