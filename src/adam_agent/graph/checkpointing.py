"""LangGraph checkpointer boundary for local product runs."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal

from langgraph.checkpoint.memory import InMemorySaver


CheckpointerBackend = Literal["memory"]


@dataclass(frozen=True)
class CheckpointerBundle:
    """Compiled graph checkpointer plus explicit capability metadata."""

    checkpointer: Any
    backend: CheckpointerBackend
    persistent: bool
    restart_recovery_source: str
    notes: tuple[str, ...]


def build_checkpointer(backend: CheckpointerBackend = "memory") -> CheckpointerBundle:
    """Create the configured checkpointer without overstating persistence."""

    if backend != "memory":
        raise ValueError("Only the in-memory LangGraph checkpointer is available in this build.")
    return CheckpointerBundle(
        checkpointer=InMemorySaver(),
        backend="memory",
        persistent=False,
        restart_recovery_source="graph_state_json",
        notes=(
            "Default LangGraph checkpointer is in-memory only.",
            "graph_checkpoints.sqlite is a local product audit ledger, not a LangGraph SQLite checkpointer.",
            "Full native LangGraph interrupt/checkpointer resume remains future work.",
        ),
    )


def describe_checkpointer(
    *,
    checkpointer: Any,
    study_dir: str | Path,
    run_id: str,
    bundle: CheckpointerBundle | None = None,
) -> dict[str, Any]:
    """Return product-facing persistence metadata for the active checkpointer."""

    run_dir = Path(study_dir) / "runs" / run_id
    checkpointer_type = type(checkpointer).__name__
    persistent = bool(bundle.persistent) if bundle is not None else False
    restart_recovery_source = bundle.restart_recovery_source if bundle is not None else "graph_state_json"
    notes = list(bundle.notes) if bundle is not None else [
        "graph_checkpoints.sqlite is a local product audit ledger, not a LangGraph SQLite checkpointer.",
        "Full native LangGraph interrupt/checkpointer resume remains future work.",
    ]
    return {
        "source_of_truth": "graph_state_json",
        "graph_state_path": str((run_dir / "graph_state.json").as_posix()),
        "workflow_projection_path": str((run_dir / "workflow_state.json").as_posix()),
        "checkpoint_ledger_path": str((run_dir / "graph_checkpoints.sqlite").as_posix()),
        "checkpoint_ledger_role": "product_audit_ledger",
        "langgraph_checkpointer_type": checkpointer_type,
        "langgraph_checkpointer_backend": bundle.backend if bundle is not None else "custom",
        "langgraph_checkpointer_persistent": persistent,
        "native_interrupt_resume": False,
        "restart_recovery_source": restart_recovery_source,
        "notes": notes,
    }
