"""Shared contracts for auditable LangGraph agent nodes.

In this project an agent is a graph node with a bounded tool contract. These
records make each node's decision visible in the canonical graph state.
"""

from __future__ import annotations

from typing import Any, Literal

from pydantic import Field

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now


AgentRole = Literal[
    "evidence_agent",
    "dependency_agent",
    "spec_agent",
    "code_agent",
    "static_review_agent",
    "execution_agent",
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
