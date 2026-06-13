"""Schemas for packages authored by external Codex skills."""

from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Literal

from pydantic import Field, field_validator, model_validator

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now


CodexPackageType = Literal["spec_authoring", "r_code_authoring"]
CodexPackageArtifactRole = Literal["spec", "code", "qc", "report", "log", "manifest", "assumption", "other"]


class CodexPackageArtifact(StrictBaseModel):
    """One file inside a Codex-authored handoff package."""

    path: NonEmptyStr
    role: CodexPackageArtifactRole
    required: bool = False
    description: str = ""
    sha256: str | None = None

    @field_validator("path")
    @classmethod
    def normalize_artifact_path(cls, value: str) -> str:
        normalized = value.strip().replace("\\", "/")
        if Path(normalized).is_absolute():
            raise ValueError("Codex package artifact paths must be relative to staging_root")
        if normalized.startswith("../") or "/../" in normalized:
            raise ValueError("Codex package artifact paths cannot escape staging_root")
        return normalized


class CodexPackageManifest(StrictBaseModel):
    """Review/audit manifest for a Codex-authored spec or R-code package."""

    package_id: NonEmptyStr
    package_type: CodexPackageType
    study_id: NonEmptyStr
    run_id: NonEmptyStr
    target_datasets: list[NonEmptyStr] = Field(min_length=1)
    created_at: datetime = Field(default_factory=utc_now)
    codex_thread_id: str | None = None
    source_workspace: NonEmptyStr
    staging_root: NonEmptyStr
    artifacts: list[CodexPackageArtifact] = Field(default_factory=list)
    assumptions_path: str | None = None
    review_required: bool = True
    not_production: bool = True
    warnings: list[str] = Field(default_factory=list)

    @field_validator("target_datasets")
    @classmethod
    def normalize_target_datasets(cls, value: list[str]) -> list[str]:
        normalized: list[str] = []
        for item in value:
            dataset = item.strip().upper()
            if dataset and dataset not in normalized:
                normalized.append(dataset)
        if not normalized:
            raise ValueError("target_datasets must contain at least one dataset")
        return normalized

    @field_validator("assumptions_path")
    @classmethod
    def normalize_optional_relative_path(cls, value: str | None) -> str | None:
        if value is None:
            return None
        normalized = value.strip().replace("\\", "/")
        if not normalized:
            return None
        if Path(normalized).is_absolute() or normalized.startswith("../") or "/../" in normalized:
            raise ValueError("assumptions_path must be relative to staging_root")
        return normalized

    @model_validator(mode="after")
    def validate_review_boundary(self) -> "CodexPackageManifest":
        if not self.review_required:
            raise ValueError("Codex-authored packages must remain review_required")
        if not self.not_production:
            raise ValueError("Codex-authored packages must be marked not_production")
        return self

    def expected_missing_paths(self) -> list[str]:
        """Return expected contract files not listed in the manifest."""

        listed = {artifact.path for artifact in self.artifacts}
        expected = required_paths_for_package_type(self.package_type)
        return [path for path in expected if path not in listed]

    def missing_files_on_disk(self) -> list[str]:
        """Return listed or expected files that do not exist under staging_root."""

        root = Path(self.staging_root).expanduser()
        candidates = {artifact.path for artifact in self.artifacts}
        candidates.update(required_paths_for_package_type(self.package_type))
        if self.assumptions_path:
            candidates.add(self.assumptions_path)
        missing: list[str] = []
        for relative_path in sorted(candidates):
            if not (root / relative_path).exists():
                missing.append(relative_path)
        return missing

    def readiness(self) -> "CodexPackageReadiness":
        """Summarize whether the package is structurally ready for Studio review."""

        missing_manifest_paths = self.expected_missing_paths()
        missing_disk_paths = self.missing_files_on_disk()
        blockers = []
        if missing_manifest_paths:
            blockers.append("manifest_missing_required_artifacts")
        if missing_disk_paths:
            blockers.append("staging_root_missing_required_files")
        return CodexPackageReadiness(
            ready=not blockers,
            blockers=blockers,
            missing_manifest_paths=missing_manifest_paths,
            missing_disk_paths=missing_disk_paths,
        )


class CodexPackageReadiness(StrictBaseModel):
    """Structural readiness summary for Studio ingestion."""

    ready: bool
    blockers: list[str] = Field(default_factory=list)
    missing_manifest_paths: list[str] = Field(default_factory=list)
    missing_disk_paths: list[str] = Field(default_factory=list)


def required_paths_for_package_type(package_type: CodexPackageType) -> list[str]:
    """Required files for a package produced by the clinical Codex skills."""

    if package_type == "spec_authoring":
        return [
            "specs/sdtm_spec_draft.csv",
            "specs/adam_spec_draft.csv",
            "specs/adam_build_order.csv",
            "specs/assumptions.md",
        ]
    return [
        "R/00_config.R",
        "R/01_helpers.R",
        "R/run_all.R",
        "report.md",
    ]


def build_codex_package_manifest(
    *,
    package_id: str,
    package_type: CodexPackageType,
    study_id: str,
    run_id: str,
    target_datasets: list[str],
    source_workspace: str,
    staging_root: str,
    codex_thread_id: str | None = None,
    artifacts: list[dict[str, object]] | None = None,
    assumptions_path: str | None = None,
    warnings: list[str] | None = None,
) -> CodexPackageManifest:
    """Build a manifest with required files inferred from package_type."""

    provided = [CodexPackageArtifact.model_validate(item) for item in artifacts or []]
    listed = {artifact.path for artifact in provided}
    for path in required_paths_for_package_type(package_type):
        if path not in listed:
            provided.append(
                CodexPackageArtifact(
                    path=path,
                    role=_role_for_required_path(path),
                    required=True,
                    description="Required by Codex package contract.",
                )
            )
    return CodexPackageManifest(
        package_id=package_id,
        package_type=package_type,
        study_id=study_id,
        run_id=run_id,
        target_datasets=target_datasets,
        codex_thread_id=codex_thread_id,
        source_workspace=source_workspace,
        staging_root=staging_root,
        artifacts=provided,
        assumptions_path=assumptions_path,
        warnings=warnings or [],
    )


def _role_for_required_path(path: str) -> CodexPackageArtifactRole:
    if path.startswith("specs/"):
        if path.endswith("assumptions.md"):
            return "assumption"
        return "spec"
    if path.startswith("R/"):
        return "code"
    if path.startswith("qc/"):
        return "qc"
    if path.startswith("logs/"):
        return "log"
    if path.endswith("report.md"):
        return "report"
    return "other"
