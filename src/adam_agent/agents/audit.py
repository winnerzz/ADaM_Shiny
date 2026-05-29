"""Derived audit summaries for bounded graph agents."""

from __future__ import annotations

from collections import Counter
import json
from pathlib import Path
from typing import Any

from adam_agent.agents.contracts import AgentDecision
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.base import utc_now
from adam_agent.tools.artifacts import sha256_file


def build_agent_audit_summary(
    *,
    study_id: str,
    run_id: str,
    status: str,
    target_datasets: list[str] | None = None,
    datasets: dict[str, Any] | None = None,
    agent_decisions: list[dict[str, Any]] | None = None,
    risk_flags: list[str] | None = None,
    current_interrupt: Any = None,
    summary_artifact_id: str | None = None,
    summary_path: str | Path | None = None,
    generated_at: str | None = None,
) -> dict[str, Any]:
    """Build a human-readable audit summary from canonical graph state.

    This summary is a derived read model. It must not become a second source of
    workflow truth.
    """

    normalized_decisions, invalid_decision_count = _normalize_agent_decisions(agent_decisions or [])
    normalized_datasets = _normalize_datasets(datasets or {})
    normalized_targets = _normalize_dataset_names(
        target_datasets or list(normalized_datasets) or _datasets_from_decisions(normalized_decisions)
    )
    summary_flags = sorted({str(flag) for flag in risk_flags or [] if str(flag).strip()})
    dataset_summaries = {
        dataset: _dataset_summary(
            dataset=dataset,
            dataset_state=normalized_datasets.get(dataset, {}),
            decisions=[item for item in normalized_decisions if item.get("dataset") == dataset],
        )
        for dataset in normalized_targets
    }
    study_decisions = [item for item in normalized_decisions if not item.get("dataset")]
    generated = generated_at or utc_now().isoformat(timespec="seconds").replace("+00:00", "Z")

    return {
        "version": 1,
        "summary_type": "agent_audit_summary",
        "summary_source": "graph_state",
        "study_id": study_id,
        "run_id": run_id,
        "status": status,
        "generated_at": generated,
        "summary_artifact_id": summary_artifact_id,
        "summary_path": str(Path(summary_path).as_posix()) if summary_path else None,
        "current_interrupt": _interrupt_name(current_interrupt),
        "summary_writer": {
            "agent": "audit_agent",
            "node": "write_agent_audit_summary",
            "decision": "agent_audit_summary_written",
            "status": status,
            "reason": "Summarized graph-owned agent decisions for human audit review.",
        },
        "target_datasets": normalized_targets,
        "decision_count": len(normalized_decisions),
        "invalid_decision_count": invalid_decision_count,
        "agent_counts": _counts_by(normalized_decisions, "agent"),
        "status_counts": _counts_by(normalized_decisions, "status"),
        "risk_flags": summary_flags,
        "study_decisions": [_decision_view(item) for item in study_decisions],
        "datasets": dataset_summaries,
        "limitations": [
            "This is a derived read model. Canonical truth remains graph_state.json.",
            "Agent decisions are append-only audit history, not a current-only rollback view.",
            "Static review entries are limited-scope checks unless a stronger policy label is present.",
        ],
    }


def build_agent_audit_summary_from_state(
    state: Any,
    *,
    summary_artifact_id: str | None = None,
    summary_path: str | Path | None = None,
    generated_at: str | None = None,
) -> dict[str, Any]:
    """Build an agent audit summary from a StudyRunState-like object."""

    payload = _to_dict(state)
    return build_agent_audit_summary(
        study_id=str(payload.get("study_id", "")),
        run_id=str(payload.get("run_id", "")),
        status=str(payload.get("status", "")),
        target_datasets=list(payload.get("target_datasets") or []),
        datasets=payload.get("datasets") or {},
        agent_decisions=list(payload.get("agent_decisions") or []),
        risk_flags=list(payload.get("risk_flags") or []),
        current_interrupt=payload.get("current_interrupt"),
        summary_artifact_id=summary_artifact_id,
        summary_path=summary_path,
        generated_at=generated_at,
    )


def write_agent_audit_summary(
    summary: dict[str, Any],
    *,
    path: str | Path,
    artifact_id: str,
) -> ArtifactRef:
    """Write an agent audit summary and return its audit artifact reference."""

    summary_path = Path(path)
    summary_path.parent.mkdir(parents=True, exist_ok=True)
    summary_path.write_text(json.dumps(summary, indent=2, sort_keys=True), encoding="utf-8")
    return ArtifactRef(
        artifact_id=artifact_id,
        kind="tool_log",
        path=str(summary_path.as_posix()),
        sha256=f"sha256:{sha256_file(summary_path)}",
        format="json",
        role="audit",
        metadata={
            "agent": "audit_agent",
            "summary_type": "agent_audit_summary",
            "summary_source": "graph_state",
        },
    )


def _normalize_agent_decisions(records: list[dict[str, Any]]) -> tuple[list[dict[str, Any]], int]:
    decisions = []
    invalid_count = 0
    for record in records:
        try:
            decisions.append(AgentDecision.model_validate(record).model_dump(mode="json"))
        except ValueError:
            invalid_count += 1
    return decisions, invalid_count


def _normalize_datasets(datasets: dict[str, Any]) -> dict[str, dict[str, Any]]:
    normalized = {}
    for key, value in datasets.items():
        dataset = str(key).strip().upper()
        if dataset:
            normalized[dataset] = _to_dict(value)
    return normalized


def _normalize_dataset_names(values: list[Any]) -> list[str]:
    names = []
    for value in values:
        name = str(value).strip().upper()
        if name and name not in names:
            names.append(name)
    return names


def _datasets_from_decisions(decisions: list[dict[str, Any]]) -> list[str]:
    return _normalize_dataset_names([item.get("dataset") for item in decisions if item.get("dataset")])


def _dataset_summary(
    *,
    dataset: str,
    dataset_state: dict[str, Any],
    decisions: list[dict[str, Any]],
) -> dict[str, Any]:
    return {
        "dataset": dataset,
        "status": dataset_state.get("status"),
        "current_interrupt": _interrupt_name(dataset_state.get("current_interrupt")),
        "human_review_required": _interrupt_name(dataset_state.get("current_interrupt")) is not None,
        "decision_count": len(decisions),
        "agent_counts": _counts_by(decisions, "agent"),
        "latest_decisions": [_decision_view(item) for item in _latest_by_agent(decisions)],
        "risk_flags": sorted({str(flag) for flag in dataset_state.get("risk_flags", []) if str(flag).strip()}),
        "artifact_ids": [
            str(artifact.get("artifact_id"))
            for artifact in dataset_state.get("artifacts", [])
            if isinstance(artifact, dict) and artifact.get("artifact_id")
        ],
    }


def _latest_by_agent(decisions: list[dict[str, Any]]) -> list[dict[str, Any]]:
    latest: dict[str, dict[str, Any]] = {}
    for decision in decisions:
        agent = str(decision.get("agent") or "")
        if not agent:
            continue
        previous = latest.get(agent)
        if previous is None or str(decision.get("created_at") or "") >= str(previous.get("created_at") or ""):
            latest[agent] = decision
    return [latest[agent] for agent in sorted(latest)]


def _decision_view(decision: dict[str, Any]) -> dict[str, Any]:
    return {
        "agent": decision.get("agent"),
        "node": decision.get("node"),
        "decision": decision.get("decision"),
        "dataset": decision.get("dataset"),
        "status": decision.get("status"),
        "reason": decision.get("reason", ""),
        "risk_flags": list(decision.get("risk_flags") or []),
        "artifact_ids": list(decision.get("artifact_ids") or []),
        "created_at": decision.get("created_at"),
    }


def _counts_by(decisions: list[dict[str, Any]], field: str) -> dict[str, int]:
    counter = Counter(str(item.get(field)) for item in decisions if item.get(field))
    return dict(sorted(counter.items()))


def _interrupt_name(interrupt: Any) -> str | None:
    if interrupt is None:
        return None
    payload = _to_dict(interrupt)
    if payload.get("status") not in {None, "open"}:
        return None
    name = payload.get("name")
    return str(name) if name else None


def _to_dict(value: Any) -> dict[str, Any]:
    if value is None:
        return {}
    if isinstance(value, dict):
        return value
    if hasattr(value, "model_dump"):
        return value.model_dump(mode="json")
    return {}
