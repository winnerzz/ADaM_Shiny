"""Canonical graph-native state contracts for LangGraph-2."""

from __future__ import annotations

from datetime import datetime
from typing import Any, Literal

from pydantic import Field, model_validator

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary, RunStatus


InterruptName = Literal[
    "dependency_review",
    "draft_spec_review",
    "code_review",
    "terminal_failure",
    "dependency_user_action_required",
]
HumanCommandAction = Literal[
    "approve",
    "reject",
    "retry",
    "revise",
    "approve_dependency_generation",
    "retry_execution",
    "repair_code",
    "revise_spec",
    "request_new_input",
    "skip_dataset",
    "continue_other_datasets",
]


class InterruptState(StrictBaseModel):
    """A durable human-in-the-loop checkpoint."""

    name: InterruptName
    dataset: str | None = None
    status: Literal["open", "resolved"] = "open"
    reason: str = ""
    payload: dict[str, Any] = Field(default_factory=dict)
    created_at: datetime = Field(default_factory=utc_now)
    resolved_at: datetime | None = None


class HumanCommand(StrictBaseModel):
    """Command used to resume a graph-native interrupt."""

    interrupt: InterruptName
    action: HumanCommandAction
    dataset: str | None = None
    reviewer: NonEmptyStr = "local_user"
    notes: str = ""
    payload: dict[str, Any] = Field(default_factory=dict)
    created_at: datetime = Field(default_factory=utc_now)


class DatasetRunState(StrictBaseModel):
    """Canonical state owned by one ADaM dataset graph."""

    study_id: NonEmptyStr
    run_id: NonEmptyStr
    dataset: NonEmptyStr
    status: RunStatus = "pending"
    current_interrupt: InterruptState | None = None
    input_fingerprint: dict[str, Any] = Field(default_factory=dict)
    dependency_resolution: list[dict[str, Any]] = Field(default_factory=list)
    spec_state: dict[str, Any] = Field(default_factory=dict)
    code_state: dict[str, Any] = Field(default_factory=dict)
    execution_state: dict[str, Any] = Field(default_factory=dict)
    validation_summary: dict[str, Any] = Field(default_factory=dict)
    compare_summary: dict[str, Any] = Field(default_factory=dict)
    artifacts: list[ArtifactRef] = Field(default_factory=list)
    failures: list[FailureRecord] = Field(default_factory=list)
    human_commands: list[HumanCommand] = Field(default_factory=list)
    agent_decisions: list[dict[str, Any]] = Field(default_factory=list)
    agent_node_inputs: list[dict[str, Any]] = Field(default_factory=list)
    agent_node_outputs: list[dict[str, Any]] = Field(default_factory=list)
    risk_flags: list[str] = Field(default_factory=list)
    agent_audit_summary: dict[str, Any] = Field(default_factory=dict)
    evidence_bundle_id: str | None = None
    reference_queries: list[dict[str, Any]] = Field(default_factory=list)
    result_summary: DatasetResultSummary | None = None
    updated_at: datetime = Field(default_factory=utc_now)


class StudyRunState(StrictBaseModel):
    """Canonical state for one study-level LangGraph run."""

    version: int = 2
    study_id: NonEmptyStr
    run_id: NonEmptyStr
    status: RunStatus = "pending"
    requested_datasets: list[NonEmptyStr] = Field(default_factory=list)
    target_datasets: list[NonEmptyStr] = Field(default_factory=list)
    runnable_datasets: list[NonEmptyStr] = Field(default_factory=list)
    blocked_datasets: list[dict[str, Any]] = Field(default_factory=list)
    current_interrupt: InterruptState | None = None
    input_fingerprint: dict[str, Any] = Field(default_factory=dict)
    dependency_plan: dict[str, Any] = Field(default_factory=dict)
    dependency_decisions: list[dict[str, Any]] = Field(default_factory=list)
    dependency_resolution: list[dict[str, Any]] = Field(default_factory=list)
    dependency_review_status: str | None = None
    datasets: dict[str, DatasetRunState] = Field(default_factory=dict)
    artifacts: list[ArtifactRef] = Field(default_factory=list)
    failures: list[FailureRecord] = Field(default_factory=list)
    human_commands: list[HumanCommand] = Field(default_factory=list)
    agent_decisions: list[dict[str, Any]] = Field(default_factory=list)
    agent_node_inputs: list[dict[str, Any]] = Field(default_factory=list)
    agent_node_outputs: list[dict[str, Any]] = Field(default_factory=list)
    risk_flags: list[str] = Field(default_factory=list)
    agent_audit_summary: dict[str, Any] = Field(default_factory=dict)
    evidence_bundle_id: str | None = None
    reference_queries: list[dict[str, Any]] = Field(default_factory=list)
    runtime_persistence: dict[str, Any] = Field(default_factory=dict)
    created_at: datetime = Field(default_factory=utc_now)
    updated_at: datetime = Field(default_factory=utc_now)

    @model_validator(mode="after")
    def validate_dataset_ownership(self) -> "StudyRunState":
        for key, dataset_state in self.datasets.items():
            normalized_key = key.strip().upper()
            if normalized_key != dataset_state.dataset.strip().upper():
                raise ValueError("datasets keys must match DatasetRunState.dataset")
            if dataset_state.study_id != self.study_id or dataset_state.run_id != self.run_id:
                raise ValueError("DatasetRunState must belong to the enclosing StudyRunState")
        return self
