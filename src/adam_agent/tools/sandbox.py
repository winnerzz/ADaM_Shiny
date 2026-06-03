"""Sandbox runner boundary for generated-code execution.

The local implementation is intentionally a developer runner around Rscript.
It is not a hardened production sandbox: it cannot prevent arbitrary reads,
writes, network calls, or process behavior inside generated R code. The point of
this module is to give graph execution a stable interface that can later be
backed by Docker, Windows isolation, or another hardened backend.
"""

from __future__ import annotations

from dataclasses import dataclass
import os
from pathlib import Path
from typing import Protocol, runtime_checkable

from adam_agent.tools.r_runner import LocalRRunner, RRunRequest, RRunResult
from adam_agent.tools.r_safety import find_forbidden_r_calls


@runtime_checkable
class SandboxRunner(Protocol):
    """Protocol for any generated-code execution backend."""

    backend_name: str
    hardened: bool

    def run(self, request: RRunRequest) -> RRunResult:
        """Run generated code through the configured execution boundary."""


@dataclass(frozen=True)
class SandboxBoundary:
    """Human/audit-facing description of the selected sandbox boundary."""

    backend_name: str
    hardened: bool
    run_dir: str
    network_disabled: bool
    environment_control: str
    environment_variables: list[str]
    rscript_arguments: list[str]
    notes: list[str]

    def as_dict(self) -> dict[str, object]:
        return {
            "backend_name": self.backend_name,
            "hardened": self.hardened,
            "run_dir": self.run_dir,
            "network_disabled": self.network_disabled,
            "environment_control": self.environment_control,
            "environment_variables": list(self.environment_variables),
            "rscript_arguments": list(self.rscript_arguments),
            "notes": list(self.notes),
        }


class LocalRscriptSandboxRunner:
    """Developer-mode sandbox boundary backed by local Rscript."""

    backend_name = "local_rscript"
    hardened = False
    network_disabled = False

    def __init__(
        self,
        *,
        run_dir: str | Path,
        rscript_path: str | None = None,
        allowed_output_paths: list[str | Path] | tuple[str | Path, ...] | None = None,
        inherited_environment: dict[str, str] | None = None,
        allowed_environment_variables: list[str] | tuple[str, ...] | None = None,
    ) -> None:
        self.run_dir = Path(run_dir).resolve()
        self.allowed_output_paths = tuple(Path(path).resolve() for path in (allowed_output_paths or []))
        self.inherited_environment = dict(os.environ if inherited_environment is None else inherited_environment)
        allowed_environment_names = (
            _default_allowed_environment_variables()
            if allowed_environment_variables is None
            else allowed_environment_variables
        )
        self.allowed_environment_variables = tuple(
            dict.fromkeys(allowed_environment_names)
        )
        self._runner = LocalRRunner(rscript_path=rscript_path)

    @property
    def rscript_path(self) -> str | None:
        """Expose resolved Rscript path for compatibility with older checks."""

        return self._runner.rscript_path

    def boundary(self) -> SandboxBoundary:
        return SandboxBoundary(
            backend_name=self.backend_name,
            hardened=self.hardened,
            run_dir=str(self.run_dir.as_posix()),
            network_disabled=self.network_disabled,
            environment_control="allowlist",
            environment_variables=list(self.allowed_environment_variables),
            rscript_arguments=list(_sandbox_rscript_arguments()),
            notes=[
                "Local Rscript is a developer runner, not a hardened production sandbox.",
                "Only a small allowlist of environment variables is passed to Rscript.",
                "Rscript runs with --vanilla to avoid user startup files and saved workspaces.",
                "Use a containerized or OS-isolated backend before claiming system-level isolation.",
            ],
        )

    def run(self, request: RRunRequest) -> RRunResult:
        errors = self._preflight_errors(request)
        if errors:
            return RRunResult(
                dataset=request.dataset,
                exit_code=125,
                stdout="",
                stderr="Sandbox preflight failed: " + "; ".join(errors),
            )
        return self._runner.run(_request_with_sandbox_environment(request, self.environment()))

    def environment(self) -> dict[str, str]:
        """Return the controlled environment passed to local Rscript."""

        environment: dict[str, str] = {}
        for name in self.allowed_environment_variables:
            value = _environment_value(self.inherited_environment, name)
            if value is not None:
                environment[name] = value
        return environment

    def _preflight_errors(self, request: RRunRequest) -> list[str]:
        errors: list[str] = []
        working_dir = Path(request.working_dir).resolve()
        if working_dir != self.run_dir:
            errors.append(f"working_dir must equal sandbox run_dir: {self.run_dir.as_posix()}")
        script_path = _resolve_script_path(request, self.run_dir)
        if not _is_relative_to(script_path, self.run_dir):
            errors.append(f"script_path is outside sandbox run_dir: {script_path.as_posix()}")
        if request.timeout_seconds <= 0:
            errors.append("timeout_seconds must be positive")
        if request.arguments:
            arguments = ", ".join(request.arguments)
            errors.append(f"caller-supplied Rscript arguments are not allowed by local sandbox: {arguments}")
        for output_path in self.allowed_output_paths:
            if not _is_relative_to(output_path, self.run_dir):
                errors.append(f"allowed output path is outside sandbox run_dir: {output_path.as_posix()}")
        if not errors:
            forbidden_calls = find_forbidden_r_calls(_r_code_for_preflight(request, script_path))
            if forbidden_calls:
                calls = ", ".join(f"{call}()" for call in forbidden_calls)
                errors.append(f"generated R code uses forbidden call before local execution: {calls}")
        return errors


def _resolve_script_path(request: RRunRequest, run_dir: Path) -> Path:
    if request.script_path:
        path = Path(request.script_path)
        if not path.is_absolute():
            path = run_dir / path
        return path.resolve()
    return (run_dir / f"build_{request.dataset.lower()}.R").resolve()


def _is_relative_to(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
        return True
    except ValueError:
        return False


def _r_code_for_preflight(request: RRunRequest, script_path: Path) -> str:
    if request.code:
        return request.code
    if script_path.exists() and script_path.is_file():
        return script_path.read_text(encoding="utf-8", errors="replace")
    return ""


def _request_with_sandbox_environment(request: RRunRequest, environment: dict[str, str]) -> RRunRequest:
    return RRunRequest(
        code=request.code,
        dataset=request.dataset,
        run_id=request.run_id,
        working_dir=request.working_dir,
        scenario=request.scenario,
        script_path=request.script_path,
        timeout_seconds=request.timeout_seconds,
        environment=environment,
        arguments=_sandbox_rscript_arguments(),
    )


def _default_allowed_environment_variables() -> tuple[str, ...]:
    names = [
        "PATH",
        "SystemRoot",
        "WINDIR",
        "SystemDrive",
        "TMP",
        "TEMP",
        "HOME",
        "USERPROFILE",
        "HOMEDRIVE",
        "HOMEPATH",
        "APPDATA",
        "LOCALAPPDATA",
        "ProgramData",
        "ProgramFiles",
        "ProgramFiles(x86)",
        "CommonProgramFiles",
        "CommonProgramFiles(x86)",
        "PROCESSOR_ARCHITECTURE",
        "R_HOME",
        "R_ARCH",
        "R_LIBS",
        "R_LIBS_USER",
        "R_USER",
        "LANG",
        "LC_ALL",
        "TZ",
    ]
    return tuple(names)


def _sandbox_rscript_arguments() -> tuple[str, ...]:
    return ("--vanilla",)


def _environment_value(environment: dict[str, str], name: str) -> str | None:
    if name in environment:
        return environment[name]
    lower_name = name.lower()
    for key, value in environment.items():
        if key.lower() == lower_name:
            return value
    return None
