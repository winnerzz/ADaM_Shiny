"""Tests for repository-local temporary workspace cleanup."""

from __future__ import annotations

import os
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from tests import temp_workspace


class TempWorkspaceTests(unittest.TestCase):
    def test_cleanup_session_root_removes_default_test_artifacts(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            session = Path(tmp) / "session"
            nested = session / "workspace"
            nested.mkdir(parents=True)
            (nested / "artifact.txt").write_text("temp", encoding="utf-8")

            with patch.dict(os.environ, {temp_workspace.KEEP_ARTIFACTS_ENV: ""}, clear=False):
                temp_workspace._cleanup_session_root(session)

            self.assertFalse(session.exists())

    def test_cleanup_session_root_preserves_debug_artifacts_when_requested(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            session = Path(tmp) / "session"
            session.mkdir()
            (session / "artifact.txt").write_text("temp", encoding="utf-8")

            try:
                with patch.dict(os.environ, {temp_workspace.KEEP_ARTIFACTS_ENV: "1"}, clear=False):
                    temp_workspace._cleanup_session_root(session)

                self.assertTrue(session.exists())
            finally:
                shutil.rmtree(session, ignore_errors=True)
