"""Shared contracts for auditable LangGraph agent nodes.

In this project an agent is a graph node with a bounded tool contract. These
records make each node's decision visible in the canonical graph state.
"""

from __future__ import annotations

import re
from typing import Any, Literal

from pydantic import Field, field_validator, model_validator

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now


AgentRole = Literal[
    "evidence_agent",
    "dependency_agent",
    "spec_agent",
    "code_agent",
    "static_review_agent",
    "execution_agent",
    "validation_agent",
    "diagnosis_repair_agent",
    "audit_agent",
]


class AgentDecision(StrictBaseModel):
    """One auditable decision made by a bounded graph agent."""

    agent: AgentRole
    node: NonEmptyStr
    decision: NonEmptyStr
    dataset: str | None = None
    status: NonEmptyStr
    reason: str = ""
    inputs: dict[str, Any] = Field(default_factory=dict)
    outputs: dict[str, Any] = Field(default_factory=dict)
    risk_flags: list[str] = Field(default_factory=list)
    artifact_ids: list[str] = Field(default_factory=list)
    created_at: str

    @field_validator("dataset")
    @classmethod
    def normalize_decision_dataset(cls, value: str | None) -> str | None:
        return _normalize_dataset(value)

    @field_validator("created_at")
    @classmethod
    def validate_decision_created_at(cls, value: str) -> str:
        return _utc_z_timestamp(value)


class AgentNodeInput(StrictBaseModel):
    """Typed input package for one bounded graph-agent node.

    This is a contract for future graph nodes. It keeps agent work scoped to
    explicit study/run context, declared artifacts, and a caller-provided task.
    """

    agent: AgentRole
    node: NonEmptyStr
    study_id: NonEmptyStr
    run_id: NonEmptyStr
    dataset: str | None = None
    task: NonEmptyStr
    inputs: dict[str, Any] = Field(default_factory=dict)
    artifact_ids: list[str] = Field(default_factory=list)
    risk_flags: list[str] = Field(default_factory=list)
    evidence_bundle_id: str | None = None
    reference_query_ids: list[str] = Field(default_factory=list)
    created_at: str = Field(default_factory=lambda: utc_now().isoformat(timespec="seconds").replace("+00:00", "Z"))

    @field_validator("dataset")
    @classmethod
    def normalize_input_dataset(cls, value: str | None) -> str | None:
        return _normalize_dataset(value)

    @field_validator("created_at")
    @classmethod
    def validate_input_created_at(cls, value: str) -> str:
        return _utc_z_timestamp(value)


class AgentNodeOutput(StrictBaseModel):
    """Typed output package for one bounded graph-agent node.

    A node output may contain multiple audit decisions, but every decision must
    belong to the same agent/node/dataset boundary.
    """

    agent: AgentRole
    node: NonEmptyStr
    study_id: NonEmptyStr
    run_id: NonEmptyStr
    dataset: str | None = None
    status: NonEmptyStr
    decision: NonEmptyStr
    reason: str = ""
    outputs: dict[str, Any] = Field(default_factory=dict)
    risk_flags: list[str] = Field(default_factory=list)
    artifact_ids: list[str] = Field(default_factory=list)
    agent_decisions: list[AgentDecision] = Field(default_factory=list)
    created_at: str = Field(default_factory=lambda: utc_now().isoformat(timespec="seconds").replace("+00:00", "Z"))

    @field_validator("dataset")
    @classmethod
    def normalize_output_dataset(cls, value: str | None) -> str | None:
        return _normalize_dataset(value)

    @field_validator("created_at")
    @classmethod
    def validate_output_created_at(cls, value: str) -> str:
        return _utc_z_timestamp(value)

    @model_validator(mode="after")
    def validate_decision_scope(self) -> "AgentNodeOutput":
        for decision in self.agent_decisions:
            if decision.agent != self.agent:
                raise ValueError("agent_decisions must belong to the output agent")
            if decision.node != self.node:
                raise ValueError("agent_decisions must belong to the output node")
            if self.dataset is None and decision.dataset is not None:
                raise ValueError("study-level agent outputs cannot include dataset-scoped decisions")
            if self.dataset and decision.dataset != self.dataset:
                raise ValueError("agent_decisions dataset must match the output dataset")
        return self


def record_agent_decision(
    *,
    agent: AgentRole,
    node: str,
    decision: str,
    status: str,
    dataset: str | None = None,
    reason: str = "",
    inputs: dict[str, Any] | None = None,
    outputs: dict[str, Any] | None = None,
    risk_flags: list[str] | None = None,
    artifact_ids: list[str] | None = None,
) -> dict[str, Any]:
    """Return a JSON-safe agent decision record."""

    return AgentDecision(
        agent=agent,
        node=node,
        decision=decision,
        dataset=dataset.strip().upper() if dataset else None,
        status=status,
        reason=reason,
        inputs=inputs or {},
        outputs=outputs or {},
        risk_flags=risk_flags or [],
        artifact_ids=artifact_ids or [],
        created_at=utc_now().isoformat(timespec="seconds").replace("+00:00", "Z"),
    ).model_dump(mode="json")


def build_agent_node_input(
    *,
    agent: AgentRole,
    node: str,
    study_id: str,
    run_id: str,
    task: str,
    dataset: str | None = None,
    inputs: dict[str, Any] | None = None,
    artifact_ids: list[str] | None = None,
    risk_flags: list[str] | None = None,
    evidence_bundle_id: str | None = None,
    reference_query_ids: list[str] | None = None,
) -> dict[str, Any]:
    """Return a JSON-safe input package for a bounded graph-agent node."""

    return AgentNodeInput(
        agent=agent,
        node=node,
        study_id=study_id,
        run_id=run_id,
        dataset=dataset,
        task=task,
        inputs=inputs or {},
        artifact_ids=artifact_ids or [],
        risk_flags=risk_flags or [],
        evidence_bundle_id=evidence_bundle_id,
        reference_query_ids=reference_query_ids or [],
    ).model_dump(mode="json")


def build_agent_node_output(
    *,
    agent: AgentRole,
    node: str,
    study_id: str,
    run_id: str,
    status: str,
    decision: str,
    dataset: str | None = None,
    reason: str = "",
    outputs: dict[str, Any] | None = None,
    risk_flags: list[str] | None = None,
    artifact_ids: list[str] | None = None,
    agent_decisions: list[dict[str, Any]] | None = None,
) -> dict[str, Any]:
    """Return a JSON-safe output package plus a matching audit decision."""

    normalized_decisions = list(agent_decisions or [])
    if not normalized_decisions:
        normalized_decisions = [
            record_agent_decision(
                agent=agent,
                node=node,
                decision=decision,
                dataset=dataset,
                status=status,
                reason=reason,
                outputs=outputs or {},
                risk_flags=risk_flags or [],
                artifact_ids=artifact_ids or [],
            )
        ]
    return AgentNodeOutput(
        agent=agent,
        node=node,
        study_id=study_id,
        run_id=run_id,
        dataset=dataset,
        status=status,
        decision=decision,
        reason=reason,
        outputs=outputs or {},
        risk_flags=risk_flags or [],
        artifact_ids=artifact_ids or [],
        agent_decisions=[AgentDecision.model_validate(item) for item in normalized_decisions],
    ).model_dump(mode="json")


def _utc_z_timestamp(value: str) -> str:
    normalized = value.strip()
    if not re.fullmatch(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", normalized):
        raise ValueError("created_at must be an ISO UTC timestamp ending in Z")
    return normalized


def _normalize_dataset(value: str | None) -> str | None:
    if value is None:
        return None
    normalized = value.strip().upper()
    return normalized or None
