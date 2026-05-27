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


class StudyWorkspaceRequest(StrictBaseModel):
    """Create or open a local study workspace."""

    study_dir: NonEmptyStr
    study_id: str | None = None


class FileUploadResponse(StrictBaseModel):
    """Result of uploading files into one canonical study input role."""

    study_id: str
    study_dir: str
    role: str
    folder: str
    saved_files: list[str] = Field(default_factory=list)
    input_summary: "StudyInputSummary"


class RunPlanRequest(StrictBaseModel):
    """Prepare dependency planning without generating code or running R."""

    study_dir: NonEmptyStr
    run_id: NonEmptyStr
    target_datasets: list[NonEmptyStr] = Field(min_length=1)
    study_id: str | None = None
    approved_dependency_datasets: list[str] = Field(default_factory=list)


class RunPlanResponse(StrictBaseModel):
    """Human-facing dependency plan summary for a run."""

    study_id: str
    run_id: str
    requested_datasets: list[str] = Field(default_factory=list)
    target_datasets: list[str] = Field(default_factory=list)
    runnable_datasets: list[str] = Field(default_factory=list)
    blocked_datasets: list[dict[str, str]] = Field(default_factory=list)
    dependency_review_status: str
    dependency_decisions: list[dict[str, Any]] = Field(default_factory=list)
    dependency_resolution: list[dict[str, Any]] = Field(default_factory=list)
    dependency_warnings: list[str] = Field(default_factory=list)


class GenerateCodeRequest(StrictBaseModel):
    """Generate and persist R code for one dataset without executing it."""

    study_dir: NonEmptyStr
    study_id: str | None = None
    config_path: str | None = None
    approved_dependency_datasets: list[str] = Field(default_factory=list)


class GenerateCodeResponse(StrictBaseModel):
    """Generated-code review bundle returned before sandbox execution."""

    study_id: str
    run_id: str
    dataset: str
    status: str
    code_path: str
    generated_code: str
    assumptions: list[str] = Field(default_factory=list)
    risk_points: list[str] = Field(default_factory=list)
    used_inputs: list[str] = Field(default_factory=list)
    expected_outputs: list[str] = Field(default_factory=list)
    context_path: str | None = None
    response_path: str | None = None
    parsed_response_path: str | None = None
    dependency_review_status: str | None = None
    warnings: list[str] = Field(default_factory=list)


class CodeReviewRequest(StrictBaseModel):
    """Persist a local human code-review decision before sandbox execution."""

    study_dir: NonEmptyStr
    reviewer: str = "local_user"
    decision: str
    notes: str = ""


class CodeReviewResponse(StrictBaseModel):
    """Persisted code-review decision."""

    study_id: str
    run_id: str
    dataset: str
    decision: str
    review_path: str
    approved: bool


class ExecuteCodeRequest(StrictBaseModel):
    """Execute previously approved generated R code in the local R boundary."""

    study_dir: NonEmptyStr
    study_id: str | None = None
    rscript_path: str | None = None
    require_approval: bool = True


class ExecuteCodeResponse(StrictBaseModel):
    """Sandbox execution result for previously generated and approved code."""

    study_id: str
    run_id: str
    dataset: str
    status: str
    validation_status: str
    output_path: str | None = None
    validation_report_path: str | None = None
    diagnostics_path: str | None = None
    errors: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)


class ArtifactReadRequest(StrictBaseModel):
    """Request a JSON artifact by path relative to the run directory."""

    relative_path: NonEmptyStr


class DemoStudyResponse(StrictBaseModel):
    """Prepared local demo study information for the browser UI."""

    study_id: str
    study_dir: str
    demo_source_dir: str
    run_id: str
    target_datasets: list[str] = Field(default_factory=list)
    config_path: str
    execution_mode: str
    rscript_path: str | None = None
    created_files: list[str] = Field(default_factory=list)
    notes: list[str] = Field(default_factory=list)


class ProductWorkspaceResponse(StrictBaseModel):
    """Default local product workspace created without exposing paths first."""

    study_id: str
    study_dir: str
    run_id: str
    target_datasets: list[str] = Field(default_factory=list)
    config_path: str
    rscript_path: str | None = None
    input_summary: StudyInputSummary
    notes: list[str] = Field(default_factory=list)


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


class FilePreview(StrictBaseModel):
    """Small UI-facing preview of a local study input or output file."""

    role: str
    dataset: str | None = None
    file_name: str
    path: str
    format: str
    status: str
    row_count: int | None = None
    columns: list[str] = Field(default_factory=list)
    sample_rows: list[dict[str, str]] = Field(default_factory=list)
    preview_type: str = "table"
    text_preview: str = ""
    line_count: int | None = None
    detected_targets: list[str] = Field(default_factory=list)
    detected_dependencies: list[str] = Field(default_factory=list)
    note: str = ""


class StudyInputSummary(StrictBaseModel):
    """Human-oriented summary of the study workspace inputs."""

    study_id: str
    study_dir: str
    sdtm: list[FilePreview] = Field(default_factory=list)
    specs: list[FilePreview] = Field(default_factory=list)
    reference_adam: list[FilePreview] = Field(default_factory=list)
    define: list[FilePreview] = Field(default_factory=list)
    legacy_code: list[FilePreview] = Field(default_factory=list)
    invalid_files: list[dict[str, str]] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)


class DatasetReview(StrictBaseModel):
    """Human-oriented review bundle for one generated dataset."""

    dataset: str
    status: str
    validation_status: str | None = None
    compare_status: str | None = None
    output_path: str | None = None
    output_preview: FilePreview | None = None
    reference_preview: FilePreview | None = None
    compare_summary: "DatasetCompareResponse | None" = None
    downloads: list["DownloadItem"] = Field(default_factory=list)
    generated_code_path: str | None = None
    generated_code: str = ""
    assumptions: list[str] = Field(default_factory=list)
    risk_points: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)
    errors: list[str] = Field(default_factory=list)
    validation_report: dict[str, Any] = Field(default_factory=dict)
    diagnostics: dict[str, Any] = Field(default_factory=dict)


class RunReviewSummary(StrictBaseModel):
    """UI-friendly summary of a completed or failed run."""

    study_id: str
    run_id: str
    run_dir: str
    status: str
    plain_summary: str
    input_summary: StudyInputSummary
    dataset_reviews: list[DatasetReview] = Field(default_factory=list)
    advanced_artifacts: dict[str, str] = Field(default_factory=dict)


class TablePageResponse(StrictBaseModel):
    """One browser-sized page from a generated or reference table."""

    dataset: str
    kind: str
    file_name: str
    format: str
    status: str
    row_count: int | None = None
    columns: list[str] = Field(default_factory=list)
    page: int = 1
    page_size: int = 25
    total_pages: int = 0
    rows: list[dict[str, str]] = Field(default_factory=list)
    note: str = ""


class DatasetCompareResponse(StrictBaseModel):
    """Human-facing comparison between generated ADaM and reference ADaM."""

    dataset: str
    status: str
    generated_file: str | None = None
    reference_file: str | None = None
    row_count_generated: int | None = None
    row_count_reference: int | None = None
    row_count_delta: int | None = None
    generated_only_columns: list[str] = Field(default_factory=list)
    reference_only_columns: list[str] = Field(default_factory=list)
    common_columns: list[str] = Field(default_factory=list)
    key_columns: list[str] = Field(default_factory=list)
    matched_rows: int = 0
    generated_only_keys: list[str] = Field(default_factory=list)
    reference_only_keys: list[str] = Field(default_factory=list)
    compared_cells: int = 0
    mismatch_count: int = 0
    mismatch_samples: list[dict[str, str]] = Field(default_factory=list)
    report_path: str | None = None
    note: str = ""


class DownloadItem(StrictBaseModel):
    """One downloadable artifact advertised to the UI."""

    kind: str
    label: str
    available: bool
    file_name: str | None = None
    note: str = ""
