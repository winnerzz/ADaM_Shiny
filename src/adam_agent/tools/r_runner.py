"""R runner interface and stub implementation."""

from __future__ import annotations

import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path

from adam_agent.schemas.artifacts import ArtifactRef


@dataclass
class RRunRequest:
    """Input contract for R execution."""

    code: str
    dataset: str
    run_id: str
    working_dir: str
    scenario: str = "success"
    script_path: str | None = None
    timeout_seconds: int = 120


@dataclass
class RRunResult:
    """Structured R execution result."""

    dataset: str
    exit_code: int
    stdout: str = ""
    stderr: str = ""
    artifacts: list[ArtifactRef] = field(default_factory=list)

    @property
    def success(self) -> bool:
        return self.exit_code == 0


class StubRRunner:
    """Stub R runner that does not invoke a real R process."""

    def run(self, request: RRunRequest) -> RRunResult:
        if request.scenario == "failure":
            return RRunResult(
                dataset=request.dataset,
                exit_code=1,
                stdout="",
                stderr="Phase 4 stub R failure",
            )
        return RRunResult(
            dataset=request.dataset,
            exit_code=0,
            stdout=f"Phase 4 stub R success for {request.dataset}",
            stderr="",
        )


class LocalRRunner:
    """Local Rscript runner behind the R execution boundary."""

    def __init__(self, rscript_path: str | None = None) -> None:
        self.rscript_path = rscript_path or shutil.which("Rscript")

    def run(self, request: RRunRequest) -> RRunResult:
        if not self.rscript_path:
            return RRunResult(
                dataset=request.dataset,
                exit_code=127,
                stdout="",
                stderr="Rscript is not available on PATH. Install R or pass rscript_path.",
            )

        working_dir = Path(request.working_dir).resolve()
        working_dir.mkdir(parents=True, exist_ok=True)
        script_path = Path(request.script_path) if request.script_path else working_dir / f"build_{request.dataset.lower()}.R"
        if not script_path.is_absolute():
            script_path = script_path.resolve()
        if request.code:
            script_path.parent.mkdir(parents=True, exist_ok=True)
            script_path.write_text(request.code, encoding="utf-8")
        if not script_path.exists():
            return RRunResult(
                dataset=request.dataset,
                exit_code=2,
                stdout="",
                stderr=f"R script does not exist: {script_path}",
            )

        try:
            completed = subprocess.run(
                [self.rscript_path, str(script_path)],
                cwd=str(working_dir),
                capture_output=True,
                text=True,
                timeout=request.timeout_seconds,
                check=False,
            )
        except subprocess.TimeoutExpired as exc:
            return RRunResult(
                dataset=request.dataset,
                exit_code=124,
                stdout=exc.stdout or "",
                stderr=f"Rscript timed out after {request.timeout_seconds} seconds",
            )
        except OSError as exc:
            return RRunResult(
                dataset=request.dataset,
                exit_code=126,
                stdout="",
                stderr=f"Failed to start Rscript: {exc}",
            )

        return RRunResult(
            dataset=request.dataset,
            exit_code=completed.returncode,
            stdout=completed.stdout,
            stderr=completed.stderr,
        )
