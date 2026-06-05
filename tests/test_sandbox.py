"""Tests for the generated-code sandbox boundary."""

from __future__ import annotations

import sys
import unittest
import uuid
from pathlib import Path

from tests.temp_workspace import test_session_root
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()

try:
    from adam_agent.tools.r_runner import LocalRRunner, RRunRequest
    from adam_agent.tools.sandbox import LocalRscriptSandboxRunner
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.tools.r_runner import LocalRRunner, RRunRequest
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
        self.assertEqual(boundary["environment_control"], "allowlist")
        self.assertIn("PATH", boundary["environment_variables"])
        self.assertEqual(boundary["rscript_arguments"], ["--vanilla"])
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

    def test_local_rscript_boundary_rejects_forbidden_r_calls_before_execution(self) -> None:
        run_dir = _workspace_dir("sandbox_forbidden_call") / "runs" / "run_001"
        run_dir.mkdir(parents=True)
        script_path = run_dir / "build_any.R"
        script_path.write_text("system('whoami')\n", encoding="utf-8")
        runner = LocalRscriptSandboxRunner(run_dir=run_dir, rscript_path=None)

        result = runner.run(
            RRunRequest(
                code="",
                dataset="ANY",
                run_id="run_001",
                working_dir=str(run_dir),
                script_path=str(script_path),
            )
        )

        self.assertEqual(result.exit_code, 125)
        self.assertIn("forbidden call", result.stderr)
        self.assertIn("system()", result.stderr)
        self.assertNotIn("Rscript is not available", result.stderr)

    def test_local_rscript_boundary_ignores_forbidden_call_words_in_strings_and_comments(self) -> None:
        run_dir = _workspace_dir("sandbox_forbidden_call_literal") / "runs" / "run_001"
        run_dir.mkdir(parents=True)
        script_path = run_dir / "build_any.R"
        script_path.write_text(
            "# system('not-a-call')\n"
            "message(\"system('not-a-call')\")\n",
            encoding="utf-8",
        )
        runner = LocalRscriptSandboxRunner(run_dir=run_dir, rscript_path=None)

        result = runner.run(
            RRunRequest(
                code="",
                dataset="ANY",
                run_id="run_001",
                working_dir=str(run_dir),
                script_path=str(script_path),
            )
        )

        self.assertNotEqual(result.exit_code, 125)
        self.assertNotIn("forbidden call", result.stderr)

    def test_local_rscript_boundary_passes_only_allowlisted_environment(self) -> None:
        run_dir = _workspace_dir("sandbox_environment") / "runs" / "run_001"
        run_dir.mkdir(parents=True)
        script_path = run_dir / "build_any.R"
        script_path.write_text("print('ok')\n", encoding="utf-8")
        runner = LocalRscriptSandboxRunner(
            run_dir=run_dir,
            rscript_path=None,
            inherited_environment={
                "Path": "C:/Dev/R/bin",
                "SystemRoot": "C:/Windows",
                "OPENAI_API_KEY": "secret",
                "ADAM_AGENT_LIVE_LLM_API_KEY": "secret",
                "R_LIBS_USER": "C:/Users/test/R",
            },
            allowed_environment_variables=("PATH", "SystemRoot", "R_LIBS_USER"),
        )
        captured: dict[str, object] = {}

        class CapturingRunner:
            rscript_path = "Rscript"

            def run(self, request: RRunRequest):  # type: ignore[no-untyped-def]
                captured["environment"] = request.environment
                from adam_agent.tools.r_runner import RRunResult

                return RRunResult(dataset=request.dataset, exit_code=0, stdout="", stderr="")

        runner._runner = CapturingRunner()  # type: ignore[assignment]

        result = runner.run(
            RRunRequest(
                code="",
                dataset="ANY",
                run_id="run_001",
                working_dir=str(run_dir),
                script_path=str(script_path),
            )
        )

        self.assertEqual(result.exit_code, 0)
        self.assertEqual(
            captured["environment"],
            {"PATH": "C:/Dev/R/bin", "SystemRoot": "C:/Windows", "R_LIBS_USER": "C:/Users/test/R"},
        )

    def test_local_rscript_boundary_adds_vanilla_execution_argument(self) -> None:
        run_dir = _workspace_dir("sandbox_vanilla_argument") / "runs" / "run_001"
        run_dir.mkdir(parents=True)
        script_path = run_dir / "build_any.R"
        script_path.write_text("print('ok')\n", encoding="utf-8")
        runner = LocalRscriptSandboxRunner(
            run_dir=run_dir,
            rscript_path=None,
            inherited_environment={"PATH": "C:/Dev/R/bin"},
        )
        captured: dict[str, object] = {}

        class CapturingRunner:
            rscript_path = "Rscript"

            def run(self, request: RRunRequest):  # type: ignore[no-untyped-def]
                captured["arguments"] = request.arguments
                from adam_agent.tools.r_runner import RRunResult

                return RRunResult(dataset=request.dataset, exit_code=0, stdout="", stderr="")

        runner._runner = CapturingRunner()  # type: ignore[assignment]

        result = runner.run(
            RRunRequest(
                code="",
                dataset="ANY",
                run_id="run_001",
                working_dir=str(run_dir),
                script_path=str(script_path),
            )
        )

        self.assertEqual(result.exit_code, 0)
        self.assertEqual(captured["arguments"], ("--vanilla",))

    def test_local_rscript_boundary_rejects_caller_supplied_rscript_arguments(self) -> None:
        run_dir = _workspace_dir("sandbox_rejects_rscript_arguments") / "runs" / "run_001"
        run_dir.mkdir(parents=True)
        script_path = run_dir / "build_any.R"
        script_path.write_text("print('ok')\n", encoding="utf-8")
        runner = LocalRscriptSandboxRunner(run_dir=run_dir, rscript_path=None)

        result = runner.run(
            RRunRequest(
                code="",
                dataset="ANY",
                run_id="run_001",
                working_dir=str(run_dir),
                script_path=str(script_path),
                arguments=("-e", "system('whoami')"),
            )
        )

        self.assertEqual(result.exit_code, 125)
        self.assertIn("caller-supplied Rscript arguments are not allowed", result.stderr)
        self.assertNotIn("Rscript is not available", result.stderr)

    def test_local_rscript_default_environment_does_not_pass_llm_or_r_profile_secrets(self) -> None:
        runner = LocalRscriptSandboxRunner(
            run_dir=_workspace_dir("sandbox_default_environment"),
            rscript_path=None,
            inherited_environment={
                "PATH": "C:/Dev/R/bin",
                "OPENAI_API_KEY": "secret",
                "ADAM_AGENT_LIVE_LLM_API_KEY": "secret",
                "R_PROFILE_USER": "C:/Users/test/.Rprofile",
                "R_ENVIRON_USER": "C:/Users/test/.Renviron",
            },
        )

        environment = runner.environment()

        self.assertEqual(environment, {"PATH": "C:/Dev/R/bin"})
        self.assertNotIn("OPENAI_API_KEY", environment)
        self.assertNotIn("ADAM_AGENT_LIVE_LLM_API_KEY", environment)
        self.assertNotIn("R_PROFILE_USER", environment)
        self.assertNotIn("R_ENVIRON_USER", environment)

    def test_local_rscript_environment_accepts_empty_inherited_environment(self) -> None:
        runner = LocalRscriptSandboxRunner(
            run_dir=_workspace_dir("sandbox_empty_environment"),
            rscript_path=None,
            inherited_environment={},
        )

        self.assertEqual(runner.environment(), {})

    def test_local_rscript_environment_accepts_empty_allowlist(self) -> None:
        runner = LocalRscriptSandboxRunner(
            run_dir=_workspace_dir("sandbox_empty_allowlist"),
            rscript_path=None,
            inherited_environment={"PATH": "C:/Dev/R/bin"},
            allowed_environment_variables=(),
        )

        self.assertEqual(runner.environment(), {})

    def test_local_r_runner_passes_arguments_and_environment_to_subprocess(self) -> None:
        run_dir = _workspace_dir("local_r_runner_invocation")
        script_path = run_dir / "build_any.R"
        script_path.write_text("print('ok')\n", encoding="utf-8")

        with patch("adam_agent.tools.r_runner.subprocess.run") as subprocess_run:
            subprocess_run.return_value.returncode = 0
            subprocess_run.return_value.stdout = "ok"
            subprocess_run.return_value.stderr = ""

            result = LocalRRunner(rscript_path="Rscript").run(
                RRunRequest(
                    code="",
                    dataset="ANY",
                    run_id="run_001",
                    working_dir=str(run_dir),
                    script_path=str(script_path),
                    environment={"PATH": "C:/Dev/R/bin"},
                    arguments=("--vanilla", "--default-packages=NULL"),
                )
            )

        self.assertTrue(result.success)
        command = subprocess_run.call_args.args[0]
        kwargs = subprocess_run.call_args.kwargs
        self.assertEqual(command, ["Rscript", "--vanilla", "--default-packages=NULL", str(script_path)])
        self.assertEqual(kwargs["env"], {"PATH": "C:/Dev/R/bin"})


if __name__ == "__main__":
    unittest.main()
