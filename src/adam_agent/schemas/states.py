"""Study and dataset state schemas."""

from __future__ import annotations

from datetime import datetime
from typing import Any, Literal

from pydantic import Field

from adam_agent.schemas.approval import ApprovalRecord
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now
from adam_agent.schemas.evidence import EvidenceRecord
from adam_agent.schemas.llm import LLMCallRecord, LLMExposureConfig
from adam_agent.schemas.routing import FailureRecord, RouteDecision
from adam_agent.schemas.specs import SpecDocument


RunStatus = Literal["pending", "running", "needs_review", "completed", "completed_stub", "failed", "cancelled"]
DatasetRole = Literal["subject_level", "event_level", "findings_level", "basic_data_structure", "other"]


class DatasetResultSummary(StrictBaseModel):
    """Study-level summary of one dataset subgraph result."""

    dataset: NonEmptyStr
    status: RunStatus
    output_artifact_ids: list[str] = Field(default_factory=list)
    audit_artifact_id: str | None = None
    validation_status: str | None = None
    compare_status: str | None = None
    failure_ids: list[str] = Field(default_factory=list)
    metadata: dict[str, Any] = Field(default_factory=dict)
    updated_at: datetime = Field(default_factory=utc_now)


class StudyState(StrictBaseModel):
    """Study-level orchestration state."""

    study_id: NonEmptyStr
    run_id: NonEmptyStr
    status: RunStatus = "pending"
    current_phase: NonEmptyStr = "study_orchestration"
    target_datasets: list[NonEmptyStr] = Field(default_factory=list)
    dependency_graph: dict[str, list[str]] = Field(default_factory=dict)
    dataset_status: dict[str, RunStatus] = Field(default_factory=dict)
    dataset_results: dict[str, DatasetResultSummary] = Field(default_factory=dict)
    study_artifacts: list[ArtifactRef] = Field(default_factory=list)
    llm_exposure: LLMExposureConfig = Field(default_factory=LLMExposureConfig)
    global_decisions: list[RouteDecision] = Field(default_factory=list)
    errors: list[FailureRecord] = Field(default_factory=list)
    created_at: datetime = Field(default_factory=utc_now)
    updated_at: datetime = Field(default_factory=utc_now)


class DatasetState(StrictBaseModel):
    """State for one target ADaM dataset."""

    study_id: NonEmptyStr
    run_id: NonEmptyStr
    dataset: NonEmptyStr
    dataset_role: DatasetRole = "other"
    status: RunStatus = "pending"
    input_domains: list[NonEmptyStr] = Field(default_factory=list)
    input_artifacts: list[ArtifactRef] = Field(default_factory=list)
    reference_artifacts: list[ArtifactRef] = Field(default_factory=list)
    lineage: dict[str, Any] = Field(default_factory=dict)
    draft_spec: SpecDocument | None = None
    approved_spec: SpecDocument | None = None
    evidence_records: list[EvidenceRecord] = Field(default_factory=list)
    approval_records: list[ApprovalRecord] = Field(default_factory=list)
    generated_code: str = ""
    code_artifact: ArtifactRef | None = None
    output_artifacts: list[ArtifactRef] = Field(default_factory=list)
    validation_summary: dict[str, Any] = Field(default_factory=dict)
    compare_summary: dict[str, Any] = Field(default_factory=dict)
    llm_exposure: LLMExposureConfig = Field(default_factory=LLMExposureConfig)
    llm_calls: list[LLMCallRecord] = Field(default_factory=list)
    route_decisions: list[RouteDecision] = Field(default_factory=list)
    failures: list[FailureRecord] = Field(default_factory=list)
    repair_attempts: int = Field(default=0, ge=0)
    max_repair_attempts: int = Field(default=3, ge=0)
    created_at: datetime = Field(default_factory=utc_now)
    updated_at: datetime = Field(default_factory=utc_now)
