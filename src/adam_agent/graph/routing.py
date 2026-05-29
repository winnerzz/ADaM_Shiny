"""Routing helpers for Phase 3 graph skeletons."""

from __future__ import annotations

from adam_agent.graph.state import DatasetGraphState, DatasetRoute


def route_after_risk(state: DatasetGraphState) -> DatasetRoute:
    """Route a dataset after stub risk assessment."""

    if state.get("human_review_required", False):
        return "human_review"
    return "continue"


def route_after_sandbox(state: DatasetGraphState) -> DatasetRoute:
    """Route a dataset after stub sandbox execution."""

    if state.get("execution_mode") in {
        "llm_downstream_stubbed",
        "llm_downstream_provider",
        "llm_downstream_r_sandbox",
    }:
        return "success" if state.get("real_run_completed") else "fail"
    if state.get("execution_mode") == "real_adsl_minimal":
        return "fail"

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
