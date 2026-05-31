"""LangGraph checkpointer boundary for local product runs."""

from __future__ import annotations

from dataclasses import dataclass
import importlib
import importlib.util
from pathlib import Path
from typing import Any, Literal

from langgraph.checkpoint.memory import InMemorySaver


CheckpointerBackend = Literal["memory", "sqlite", "postgres"]


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

    if backend == "sqlite":
        return _build_sqlite_checkpointer()
    if backend == "postgres":
        return _build_postgres_checkpointer()
    if backend != "memory":
        raise ValueError(f"Unknown LangGraph checkpointer backend: {backend}")
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


def _build_sqlite_checkpointer() -> CheckpointerBundle:
    """Build the optional SQLite checkpointer only when the package is installed."""

    if importlib.util.find_spec("langgraph.checkpoint.sqlite") is None:
        raise ValueError(
            "SQLite LangGraph checkpointer is not installed. Install the optional "
            "`langgraph-checkpoint-sqlite` package before enabling backend='sqlite'."
        )
    importlib.import_module("langgraph.checkpoint.sqlite")
    raise ValueError(
        "SQLite LangGraph checkpointer package is present but the local product has not "
        "wired database lifecycle management yet."
    )


def _build_postgres_checkpointer() -> CheckpointerBundle:
    """Build the optional Postgres checkpointer only when the package is installed."""

    if importlib.util.find_spec("langgraph.checkpoint.postgres") is None:
        raise ValueError(
            "Postgres LangGraph checkpointer is not installed. Install the optional "
            "`langgraph-checkpoint-postgres` package before enabling backend='postgres'."
        )
    importlib.import_module("langgraph.checkpoint.postgres")
    raise ValueError(
        "Postgres LangGraph checkpointer package is present but the local product has not "
        "wired database lifecycle management yet."
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
