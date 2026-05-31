"""LangGraph checkpointer boundary for local product runs."""

from __future__ import annotations

from dataclasses import dataclass
import importlib
import importlib.util
from pathlib import Path
import sqlite3
from typing import Any, Callable, Literal

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
    checkpoint_path: str | None = None
    close_callback: Callable[[], None] | None = None

    def close(self) -> None:
        """Close any process-local resource held by the checkpointer bundle."""

        if self.close_callback is not None:
            self.close_callback()


def default_sqlite_checkpointer_path(study_dir: str | Path, run_id: str) -> Path:
    """Return the per-run LangGraph SQLite checkpoint path."""

    return Path(study_dir).expanduser() / "runs" / run_id / "langgraph_checkpoints.sqlite"


def build_checkpointer(
    backend: CheckpointerBackend = "memory",
    *,
    sqlite_path: str | Path | None = None,
) -> CheckpointerBundle:
    """Create the configured checkpointer without overstating persistence."""

    if backend == "sqlite":
        return _build_sqlite_checkpointer(sqlite_path=sqlite_path)
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


def _build_sqlite_checkpointer(*, sqlite_path: str | Path | None) -> CheckpointerBundle:
    """Build the optional SQLite checkpointer only when package and path exist."""

    if sqlite_path is None or str(sqlite_path).strip() == "":
        raise ValueError("sqlite_path is required when enabling backend='sqlite'.")

    if importlib.util.find_spec("langgraph.checkpoint.sqlite") is None:
        raise ValueError(
            "SQLite LangGraph checkpointer is not installed. Install the optional "
            "`langgraph-checkpoint-sqlite` package before enabling backend='sqlite'."
        )
    try:
        module = importlib.import_module("langgraph.checkpoint.sqlite")
        sqlite_saver_cls = getattr(module, "SqliteSaver")
    except (ImportError, AttributeError) as exc:
        raise ValueError(
            "SQLite LangGraph checkpointer package is present but cannot be imported "
            "with the installed LangGraph checkpoint version."
        ) from exc

    db_path = Path(sqlite_path).expanduser()
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(str(db_path), check_same_thread=False)
    try:
        checkpointer = sqlite_saver_cls(conn)
        setup = getattr(checkpointer, "setup", None)
        if callable(setup):
            setup()
    except Exception:
        conn.close()
        raise

    return CheckpointerBundle(
        checkpointer=checkpointer,
        backend="sqlite",
        persistent=True,
        restart_recovery_source="langgraph_sqlite_checkpointer",
        checkpoint_path=str(db_path.as_posix()),
        close_callback=conn.close,
        notes=(
            "LangGraph checkpointer is backed by a local SQLite database.",
            "This SQLite checkpointer is for local single-process recovery, not production multi-worker deployment.",
            "graph_checkpoints.sqlite remains a product audit ledger and is separate from the LangGraph runtime checkpoint store.",
        ),
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
        "langgraph_checkpoint_path": bundle.checkpoint_path if bundle is not None else None,
        "langgraph_checkpointer_type": checkpointer_type,
        "langgraph_checkpointer_backend": bundle.backend if bundle is not None else "custom",
        "langgraph_checkpointer_persistent": persistent,
        "native_interrupt_resume": persistent,
        "native_interrupt_resume_scope": "native_pilot_interrupts_only" if persistent else "none",
        "restart_recovery_source": restart_recovery_source,
        "notes": notes,
    }
