"""Tests for the generated-code sandbox boundary."""

from __future__ import annotations

import sys
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.tools.r_runner import RRunRequest
    from adam_agent.tools.sandbox import LocalRscriptSandboxRunner
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.tools.r_runner import RRunRequest
    from adam_agent.tools.sandbox import LocalRscriptSandboxRunner


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class SandboxBoundaryTests(unittest.TestCase):
    def test_local_rscript_boundary_is_explicitly_not_hardened(self) -> None:
        run_dir = _workspace_dir("sandbox_boundary") / "runs" / "run_001"
        runner = LocalRscriptSandboxRunner(run_dir=run_dir, rscript_path=None)

        boundary = runner.boundary().as_dict()

        self.assertEqual(boundary["backend_name"], "local_rscript")
        self.assertFalse(boundary["hardened"])
        self.assertFalse(boundary["network_disabled"])
        self.assertTrue(any("not a hardened production sandbox" in note for note in boundary["notes"]))

    def test_local_rscript_boundary_rejects_script_path_outside_run_dir(self) -> None:
        workspace = _workspace_dir("sandbox_script_escape")
        run_dir = workspace / "runs" / "run_001"
        outside_script = workspace / "outside.R"
        run_dir.mkdir(parents=True)
        outside_script.write_text("print('outside')\n", encoding="utf-8")
        runner = LocalRscriptSandboxRunner(run_dir=run_dir, rscript_path=None)

        result = runner.run(
            RRunRequest(
                code="",
                dataset="ANY",
                run_id="run_001",
                working_dir=str(run_dir),
                script_path=str(outside_script),
            )
        )

        self.assertEqual(result.exit_code, 125)
        self.assertIn("outside sandbox run_dir", result.stderr)
        self.assertNotIn("Rscript is not available", result.stderr)

    def test_local_rscript_boundary_rejects_wrong_working_dir(self) -> None:
        workspace = _workspace_dir("sandbox_wrong_working_dir")
        run_dir = workspace / "runs" / "run_001"
        other_dir = workspace / "runs" / "run_002"
        run_dir.mkdir(parents=True)
        other_dir.mkdir(parents=True)
        runner = LocalRscriptSandboxRunner(run_dir=run_dir, rscript_path=None)

        result = runner.run(
            RRunRequest(
                code="",
                dataset="ANY",
                run_id="run_001",
                working_dir=str(other_dir),
                script_path=str(run_dir / "build_any.R"),
            )
        )

        self.assertEqual(result.exit_code, 125)
        self.assertIn("working_dir must equal sandbox run_dir", result.stderr)


if __name__ == "__main__":
    unittest.main()
