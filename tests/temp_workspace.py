"""Shared temporary workspace helpers for unittest modules.

Tests write under a per-process session directory so normal regression runs do
not leave thousands of UUID workspaces in the repository.
"""

from __future__ import annotations

import atexit
import os
import shutil
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
BASE_TMP_ROOT = ROOT / ".tmp_tests"
KEEP_ARTIFACTS_ENV = "ADAM_AGENT_KEEP_TEST_ARTIFACTS"
SESSION_ROOT_ENV = "ADAM_AGENT_TEST_SESSION_ROOT"

_SESSION_ROOT: Path | None = None


def test_session_root() -> Path:
    """Return a per-process temporary root and register cleanup once."""

    global _SESSION_ROOT
    if _SESSION_ROOT is not None:
        _SESSION_ROOT.mkdir(parents=True, exist_ok=True)
        return _SESSION_ROOT

    existing = os.environ.get(SESSION_ROOT_ENV)
    if existing:
        _SESSION_ROOT = Path(existing)
    else:
        _SESSION_ROOT = BASE_TMP_ROOT / f"s{os.getpid():x}{uuid.uuid4().hex[:6]}"
        os.environ[SESSION_ROOT_ENV] = str(_SESSION_ROOT)
        atexit.register(_cleanup_session_root, _SESSION_ROOT)

    _SESSION_ROOT.mkdir(parents=True, exist_ok=True)
    return _SESSION_ROOT


def _cleanup_session_root(path: Path) -> None:
    if os.environ.get(KEEP_ARTIFACTS_ENV):
        return
    try:
        shutil.rmtree(path)
    except FileNotFoundError:
        return
    except OSError:
        # Windows may briefly hold handles to sqlite/log files. The ignored
        # .tmp_tests directory can still be removed by a later cleanup command.
        return
