"""Shared terminal-failure human action contract."""

from __future__ import annotations


TERMINAL_FAILURE_REVIEW_ACTIONS: tuple[dict[str, str], ...] = (
    {"action": "retry_execution", "label": "Retry Execution"},
    {"action": "repair_code", "label": "Repair Code"},
    {"action": "revise_spec", "label": "Revise Spec"},
    {"action": "request_new_input", "label": "Request New Input"},
    {"action": "skip_dataset", "label": "Skip Dataset"},
    {"action": "continue_other_datasets", "label": "Continue Other Datasets"},
)

TERMINAL_FAILURE_REVIEW_ACTION_NAMES_IN_ORDER: tuple[str, ...] = tuple(
    item["action"] for item in TERMINAL_FAILURE_REVIEW_ACTIONS
)
TERMINAL_FAILURE_REVIEW_ACTION_NAMES: frozenset[str] = frozenset(TERMINAL_FAILURE_REVIEW_ACTION_NAMES_IN_ORDER)


def terminal_failure_review_action_names_payload() -> list[str]:
    """Return the interrupt action list without exposing shared mutable state."""

    return list(TERMINAL_FAILURE_REVIEW_ACTION_NAMES_IN_ORDER)
