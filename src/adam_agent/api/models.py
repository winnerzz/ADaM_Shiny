"""API request and response contracts."""

from __future__ import annotations

from typing import Any

from pydantic import Field

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel


class RunStudyRequest(StrictBaseModel):
    """Create and run a study orchestration request."""

    study_dir: NonEmptyStr
    run_id: NonEmptyStr
    target_datasets: list[NonEmptyStr] = Field(min_length=1)
    study_id: str | None = None
    config_path: str | None = None
    execution_mode: str | None = None
    approved_dependency_datasets: list[str] = Field(default_factory=list)
    rscript_path: str | None = None


class ArtifactReadRequest(StrictBaseModel):
    """Request a JSON artifact by path relative to the run directory."""

    relative_path: NonEmptyStr


class RunStudyResponse(StrictBaseModel):
    """Study run summary returned by the API."""

    study_id: str
    run_id: str
    status: str
    execution_mode: str
    requested_datasets: list[str] = Field(default_factory=list)
    target_datasets: list[str] = Field(default_factory=list)
    runnable_datasets: list[str] = Field(default_factory=list)
    blocked_datasets: list[dict[str, Any]] = Field(default_factory=list)
    dependency_review_status: str | None = None
    run_dir: str
    audit_manifest: str | None = None
    dataset_results: list[dict[str, Any]] = Field(default_factory=list)
