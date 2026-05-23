"""R runner interface and stub implementation."""

from __future__ import annotations

from dataclasses import dataclass, field

from adam_agent.schemas.artifacts import ArtifactRef


@dataclass
class RRunRequest:
    """Input contract for R execution."""

    code: str
    dataset: str
    run_id: str
    working_dir: str
    scenario: str = "success"


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
