"""Durable graph-facing workflow state for the local UI/API flow."""

from __future__ import annotations

import json
import sqlite3
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from adam_agent.tools.artifacts import sha256_file


CANONICAL_INPUT_ROLES = {
    "input_sdtm": "input_sdtm",
    "input_spec": "input_spec",
    "input_define": "input_define",
    "legacy_code": "legacy_code",
    "reference_adam": "reference_adam",
}
_NO_CHANGE = object()


def utc_timestamp() -> str:
    """Return a compact ISO timestamp for audit JSON."""

    return datetime.now(UTC).isoformat(timespec="seconds").replace("+00:00", "Z")


def input_fingerprint(study_dir: str | Path) -> dict[str, Any]:
    """Fingerprint canonical study evidence files."""

    root = Path(study_dir)
    files: list[dict[str, Any]] = []
    for role, folder_name in CANONICAL_INPUT_ROLES.items():
        folder = root / folder_name
        if not folder.exists():
            continue
        for path in sorted(item for item in folder.iterdir() if item.is_file()):
            stat = path.stat()
            files.append(
                {
                    "role": role,
                    "path": path.relative_to(root).as_posix(),
                    "size": stat.st_size,
                    "mtime_ns": stat.st_mtime_ns,
                    "sha256": f"sha256:{sha256_file(path)}",
                }
            )
    return {
        "version": 1,
        "created_at": utc_timestamp(),
        "files": files,
        "digest": _fingerprint_digest(files),
    }


def compare_fingerprints(old: dict[str, Any] | None, new: dict[str, Any]) -> dict[str, Any]:
    """Return a small diff between two input fingerprints."""

    old_files = _files_by_path(old or {})
    new_files = _files_by_path(new)
    added = sorted(path for path in new_files if path not in old_files)
    removed = sorted(path for path in old_files if path not in new_files)
    changed = sorted(
        path
        for path in new_files
        if path in old_files and new_files[path].get("sha256") != old_files[path].get("sha256")
    )
    return {
        "changed": bool(added or removed or changed),
        "added": added,
        "removed": removed,
        "changed_files": changed,
        "old_digest": (old or {}).get("digest"),
        "new_digest": new.get("digest"),
    }


def load_workflow_state(study_dir: str | Path, run_id: str) -> dict[str, Any]:
    """Load the persisted workflow state if present."""

    path = _workflow_state_path(study_dir, run_id)
    if not path.exists():
        return {}
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, UnicodeDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def update_workflow_state(
    study_dir: str | Path,
    run_id: str,
    *,
    study_id: str | None = None,
    node: str,
    dataset: str | None = None,
    status: str | None = None,
    current_interrupt: Any = _NO_CHANGE,
    input_fingerprint_payload: dict[str, Any] | None = None,
    input_diff: dict[str, Any] | None = None,
    stale_datasets: list[str] | None = None,
    dataset_update: dict[str, Any] | None = None,
    extra: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """Persist one graph-facing workflow checkpoint."""

    root = Path(study_dir)
    run_dir = root / "runs" / run_id
    run_dir.mkdir(parents=True, exist_ok=True)
    state = load_workflow_state(root, run_id)
    now = utc_timestamp()
    state.setdefault("version", 1)
    state["study_id"] = study_id or state.get("study_id") or root.name
    state["run_id"] = run_id
    state["last_node"] = node
    state["updated_at"] = now
    if status is not None:
        state["status"] = status
    if current_interrupt is not _NO_CHANGE:
        state["current_interrupt"] = current_interrupt
    if input_fingerprint_payload is not None:
        state["input_fingerprint"] = input_fingerprint_payload
    if input_diff is not None:
        state["input_diff"] = input_diff
    if stale_datasets is not None:
        state["stale_datasets"] = sorted({item.strip().upper() for item in stale_datasets if item.strip()})
    if extra:
        state.update(extra)
    if dataset:
        target = dataset.strip().upper()
        datasets = state.setdefault("datasets", {})
        dataset_state = datasets.setdefault(target, {"dataset": target})
        dataset_state["updated_at"] = now
        dataset_state["last_node"] = node
        if status is not None:
            dataset_state["status"] = status
        if current_interrupt is not _NO_CHANGE:
            dataset_state["current_interrupt"] = current_interrupt
        if dataset_update:
            dataset_state.update(dataset_update)

    _write_json(_workflow_state_path(root, run_id), state)
    _write_sqlite_checkpoint(root, run_id, node=node, state=state)
    return state


def invalidate_active_workflows(study_dir: str | Path) -> dict[str, Any]:
    """Refresh fingerprints for existing runs after uploaded evidence changes."""

    root = Path(study_dir)
    new_fingerprint = input_fingerprint(root)
    runs_dir = root / "runs"
    touched: list[str] = []
    latest_diff: dict[str, Any] = {
        "changed": bool(new_fingerprint.get("files")),
        "added": [item["path"] for item in new_fingerprint.get("files", [])],
        "removed": [],
        "changed_files": [],
        "old_digest": None,
        "new_digest": new_fingerprint.get("digest"),
    }
    if runs_dir.exists():
        for run_dir in sorted(item for item in runs_dir.iterdir() if item.is_dir()):
            state = load_workflow_state(root, run_dir.name)
            diff = compare_fingerprints(state.get("input_fingerprint"), new_fingerprint)
            latest_diff = diff
            if diff["changed"]:
                stale = _known_dataset_names(state)
                update_workflow_state(
                    root,
                    run_dir.name,
                    study_id=state.get("study_id") or root.name,
                    node="input_upload_rescan",
                    status="inputs_changed",
                    input_fingerprint_payload=new_fingerprint,
                    input_diff=diff,
                    stale_datasets=stale,
                    extra={
                        "stale_reason": "Study input files changed after prior planning or review.",
                        "plan_stale": True,
                    },
                )
                touched.append(run_dir.name)
    return {"input_fingerprint": new_fingerprint, "input_diff": latest_diff, "touched_runs": touched}


def mark_workflow_inputs_current(study_dir: str | Path, run_id: str, *, study_id: str, node: str) -> dict[str, Any]:
    """Ensure a run has the latest input fingerprint in its workflow state."""

    fingerprint = input_fingerprint(study_dir)
    old = load_workflow_state(study_dir, run_id).get("input_fingerprint")
    diff = compare_fingerprints(old, fingerprint)
    return update_workflow_state(
        study_dir,
        run_id,
        study_id=study_id,
        node=node,
        input_fingerprint_payload=fingerprint,
        input_diff=diff,
        status="running",
        extra={"plan_stale": False} if not diff["changed"] else {},
    )


def _workflow_state_path(study_dir: str | Path, run_id: str) -> Path:
    return Path(study_dir) / "runs" / run_id / "workflow_state.json"


def _fingerprint_digest(files: list[dict[str, Any]]) -> str:
    payload = json.dumps(
        [
            {
                "role": item["role"],
                "path": item["path"],
                "size": item["size"],
                "sha256": item["sha256"],
            }
            for item in files
        ],
        sort_keys=True,
        separators=(",", ":"),
    )
    import hashlib

    return "sha256:" + hashlib.sha256(payload.encode("utf-8")).hexdigest()


def _files_by_path(fingerprint: dict[str, Any]) -> dict[str, dict[str, Any]]:
    return {
        str(item.get("path")): item
        for item in fingerprint.get("files", [])
        if isinstance(item, dict) and item.get("path")
    }


def _known_dataset_names(state: dict[str, Any]) -> list[str]:
    names = set()
    names.update(str(item).upper() for item in state.get("target_datasets", []) if str(item).strip())
    names.update(str(item).upper() for item in state.get("requested_datasets", []) if str(item).strip())
    names.update(str(item).upper() for item in (state.get("datasets") or {}).keys())
    return sorted(names)


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")


def _write_sqlite_checkpoint(study_dir: Path, run_id: str, *, node: str, state: dict[str, Any]) -> None:
    db_path = study_dir / "runs" / run_id / "workflow_checkpoints.sqlite"
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(db_path)
    try:
        conn.execute(
            """
            create table if not exists checkpoints (
                id integer primary key autoincrement,
                created_at text not null,
                node text not null,
                state_json text not null
            )
            """
        )
        conn.execute(
            "insert into checkpoints (created_at, node, state_json) values (?, ?, ?)",
            (utc_timestamp(), node, json.dumps(state, sort_keys=True)),
        )
        conn.commit()
    finally:
        conn.close()
