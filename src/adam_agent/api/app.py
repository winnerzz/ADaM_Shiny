"""FastAPI application for the local ADaM Agent Studio backend."""

from __future__ import annotations

from typing import Any

from fastapi import FastAPI, HTTPException, Query
from fastapi.responses import HTMLResponse

from adam_agent.api.models import ArtifactReadRequest, RunStudyRequest, RunStudyResponse
from adam_agent.api.service import ApiServiceError, read_run_json_artifact, run_study_from_request
from adam_agent.api.web import INDEX_HTML


def create_app() -> FastAPI:
    """Create the local FastAPI application."""

    app = FastAPI(
        title="ADaM Agent Studio API",
        version="0.1.0",
        description="Local-first API wrapper around the LangGraph ADaM prototype.",
    )

    @app.get("/health")
    def health() -> dict[str, str]:
        return {"status": "ok"}

    @app.get("/", response_class=HTMLResponse)
    def index() -> str:
        return INDEX_HTML

    @app.post("/runs", response_model=RunStudyResponse)
    def create_run(request: RunStudyRequest) -> RunStudyResponse:
        try:
            return run_study_from_request(request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.get("/runs/{run_id}/dependency-plan")
    def dependency_plan(
        run_id: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> dict[str, Any]:
        return _read_artifact(study_dir, run_id, "planning/dependency_plan.json")

    @app.get("/runs/{run_id}/audit-manifest")
    def audit_manifest(
        run_id: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> dict[str, Any]:
        return _read_artifact(study_dir, run_id, "audit/manifest.json")

    @app.get("/runs/{run_id}/datasets/{dataset}/validation")
    def dataset_validation(
        run_id: str,
        dataset: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> dict[str, Any]:
        return _read_artifact(study_dir, run_id, f"validation/{dataset.lower()}_validation_report.json")

    @app.get("/runs/{run_id}/datasets/{dataset}/diagnostics")
    def dataset_diagnostics(
        run_id: str,
        dataset: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> dict[str, Any]:
        return _read_artifact(study_dir, run_id, f"diagnostics/{dataset.lower()}_failure_report.json")

    @app.post("/runs/{run_id}/artifacts/read")
    def read_json_artifact(
        run_id: str,
        request: ArtifactReadRequest,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> dict[str, Any]:
        return _read_artifact(study_dir, run_id, request.relative_path)

    return app


def _read_artifact(study_dir: str, run_id: str, relative_path: str) -> dict[str, Any]:
    try:
        return read_run_json_artifact(study_dir, run_id, relative_path)
    except ApiServiceError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc


app = create_app()
