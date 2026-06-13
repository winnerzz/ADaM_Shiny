"""Schemas for the review-gated Code Agent package builder."""

from __future__ import annotations

from typing import Any, Literal

from pydantic import Field, field_validator

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel


CodeAgentAttemptStatus = Literal[
    "drafted",
    "static_failed",
    "runtime_failed",
    "trial_passed",
    "environment_unavailable",
    "blocked",
]

CodeAgentResultStatus = Literal["ready_for_review", "blocked"]


class CodeAgentFailureClassification(StrictBaseModel):
    """How the Code Agent classified a static/runtime failure."""

    category: NonEmptyStr
    repairable: bool
    reason: str = ""
    next_action: str = ""


class CodeAgentTask(StrictBaseModel):
    """Input contract for building one reviewable code package."""

    study_dir: NonEmptyStr
    study_id: NonEmptyStr
    run_id: NonEmptyStr
    dataset: NonEmptyStr
    context: dict[str, Any]
    context_artifact_id: str | None = None
    spec_source: str
    approved_spec_path: str | None = None
    llm_provider: dict[str, Any] = Field(default_factory=dict)
    llm_exposure: dict[str, Any] = Field(default_factory=dict)
    rscript_path: str | None = None
    max_attempts: int = Field(default=2, ge=1, le=5)
    required_identifiers: list[str] = Field(default_factory=list)
    required_identifier_source_id: str | None = None

    @field_validator("dataset")
    @classmethod
    def normalize_dataset(cls, value: str) -> str:
        return value.strip().upper()


class CodeAgentAttempt(StrictBaseModel):
    """One draft/check/run attempt inside the Code Agent package."""

    attempt: int = Field(ge=1)
    status: CodeAgentAttemptStatus
    code_path: str | None = None
    response_path: str | None = None
    parsed_response_path: str | None = None
    static_check_path: str | None = None
    runtime_report_path: str | None = None
    trial_output_path: str | None = None
    static_status: str | None = None
    runtime_status: str | None = None
    exit_code: int | None = None
    errors: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)
    repair_reason: str = ""
    failure_classification: CodeAgentFailureClassification | None = None


class CodeAgentResult(StrictBaseModel):
    """Review package emitted by the Code Agent before human code review."""

    status: CodeAgentResultStatus
    ready_for_human_review: bool
    final_code_path: str
    final_code: str
    static_check_path: str
    response_path: str
    parsed_response_path: str
    prompt_path: str
    review_package_path: str
    review_markdown_path: str | None = None
    attempts: list[CodeAgentAttempt] = Field(default_factory=list)
    assumptions: list[str] = Field(default_factory=list)
    risk_points: list[str] = Field(default_factory=list)
    used_inputs: list[str] = Field(default_factory=list)
    expected_outputs: list[str] = Field(default_factory=list)
    trial_run_status: str = "not_run"
    trial_runtime_report_path: str | None = None
    trial_output_path: str | None = None
    failure_classification: CodeAgentFailureClassification | None = None
    llm_provider: str | None = None
    llm_model: str | None = None
    provider_alias: str | None = None
    transport: str | None = None
    provider_base_url: str | None = None
    not_real_derivation: bool = False
    artifact_refs: list[ArtifactRef] = Field(default_factory=list)

