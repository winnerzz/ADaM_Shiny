"""Routing helpers for Phase 3 graph skeletons."""

from __future__ import annotations

from langgraph.types import Send

from adam_agent.graph.state import DatasetGraphState, DatasetRoute, StudyGraphState


def route_after_risk(state: DatasetGraphState) -> DatasetRoute:
    """Route a dataset after stub risk assessment."""

    if state.get("human_review_required", False):
        return "human_review"
    return "continue"


def route_after_sandbox(state: DatasetGraphState) -> DatasetRoute:
    """Route a dataset after stub sandbox execution."""

    failure_type = state.get("failure_type")
    if failure_type == "code_error":
        if state.get("repair_attempts", 0) >= state.get("max_repair_attempts", 3):
            return "fail"
        return "repair_code"
    if failure_type == "spec_error":
        if state.get("repair_attempts", 0) >= state.get("max_repair_attempts", 3):
            return "fail"
        return "revise_spec"
    if failure_type:
        return "fail"
    return "success"


def route_after_foundation(state: StudyGraphState) -> str | list[Send]:
    """Route study execution after foundation datasets have completed."""

    if state.get("route") == "foundation_failed":
        return "mark_downstream_blocked"
    sends = [
        Send("run_downstream_dataset", task)
        for task in state.get("downstream_tasks", [])
    ]
    return sends or "reduce_dataset_results"
