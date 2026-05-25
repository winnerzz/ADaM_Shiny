"""Service helpers behind the FastAPI routes."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from adam_agent.api.models import RunStudyRequest, RunStudyResponse
from adam_agent.graph.study_graph import compile_study_graph
from adam_agent.tools.config import ConfigLoader


class ApiServiceError(RuntimeError):
    """Raised when an API request cannot be fulfilled safely."""


def run_study_from_request(request: RunStudyRequest) -> RunStudyResponse:
    """Run the study graph from an API request and return a stable summary."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")

    study_id = request.study_id or study_dir.name
    config = ConfigLoader().load(request.config_path, study_id=study_id, run_id=request.run_id)
    execution_mode = request.execution_mode
    if execution_mode is None and config.llm_provider.provider != "mock":
        execution_mode = "llm_downstream_provider"
    if execution_mode is None:
        execution_mode = "stub"

    graph = compile_study_graph()
    result = graph.invoke(
        {
            "study_id": config.study_id,
            "run_id": config.run_id,
            "target_datasets": request.target_datasets,
            "execution_mode": execution_mode,
            "study_dir": str(study_dir),
            "rscript_path": request.rscript_path or "",
            "approved_dependency_datasets": request.approved_dependency_datasets,
            "llm_exposure": config.llm_exposure.model_dump(mode="json"),
            "llm_provider": {
                key: value
                for key, value in config.llm_provider.__dict__.items()
                if value is not None
            },
            "dataset_results": [],
            "blocked_datasets": [],
            "audit_artifacts": [],
        }
    )
    return _response_from_graph_result(result, execution_mode=execution_mode, study_dir=study_dir)


def read_run_json_artifact(study_dir: str | Path, run_id: str, relative_path: str) -> dict[str, Any]:
    """Read a JSON artifact below one run directory."""

    root = Path(study_dir).expanduser()
    run_dir = (root / "runs" / run_id).resolve()
    target_path = (run_dir / relative_path).resolve()
    if not _is_relative_to(target_path, run_dir):
        raise ApiServiceError("Artifact path must stay under the requested run directory.")
    if not target_path.exists() or not target_path.is_file():
        raise ApiServiceError(f"Artifact does not exist: {target_path}")
    if target_path.suffix.lower() != ".json":
        raise ApiServiceError("Only JSON artifacts are supported by this endpoint.")
    return json.loads(target_path.read_text(encoding="utf-8"))


def _response_from_graph_result(
    result: dict[str, Any],
    *,
    execution_mode: str,
    study_dir: Path,
) -> RunStudyResponse:
    audit_manifest = result.get("audit_manifest")
    return RunStudyResponse(
        study_id=result["study_id"],
        run_id=result["run_id"],
        status=result["status"],
        execution_mode=execution_mode,
        requested_datasets=result.get("requested_datasets", []),
        target_datasets=result.get("target_datasets", []),
        runnable_datasets=result.get("runnable_datasets", []),
        blocked_datasets=result.get("blocked_datasets", []),
        dependency_review_status=result.get("dependency_review_status"),
        run_dir=str((study_dir / "runs" / result["run_id"]).as_posix()),
        audit_manifest=audit_manifest.path if audit_manifest else None,
        dataset_results=[
            summary.model_dump(mode="json")
            for summary in result.get("dataset_results", [])
        ],
    )


def _is_relative_to(path: Path, parent: Path) -> bool:
    try:
        path.relative_to(parent)
    except ValueError:
        return False
    return True
