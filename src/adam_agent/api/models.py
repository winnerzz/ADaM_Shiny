"""API request and response contracts."""

from __future__ import annotations

from typing import Any

from pydantic import Field

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel


class RunStudyRequest(StrictBaseModel):
    """Legacy run-to-completion request kept for compatibility and smoke tests."""

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
    input_fingerprint: dict[str, Any] = Field(default_factory=dict)
    input_diff: dict[str, Any] = Field(default_factory=dict)
    touched_runs: list[str] = Field(default_factory=list)
    touched_graph_runs: list[str] = Field(default_factory=list)
    skipped_graph_runs: list[str] = Field(default_factory=list)


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
    graph_state_path: str | None = None
    workflow_state_path: str | None = None


class DatasetProgressItem(StrictBaseModel):
    """Graph-owned next-step summary for one dataset."""

    dataset: str
    status: str
    next_action: str
    action_label: str
    blocked: bool = False
    blocked_reason: str = ""
    current_interrupt: dict[str, Any] | None = None
    spec_status: str = ""
    code_status: str = ""
    execution_status: str = ""
    validation_status: str = ""
    compare_status: str = ""
    output_quality: dict[str, Any] = Field(default_factory=dict)
    warnings: list[str] = Field(default_factory=list)
    available_actions: list[dict[str, str]] = Field(default_factory=list)


class RunProgressResponse(StrictBaseModel):
    """Graph-owned progress summary for UI guidance."""

    study_id: str
    run_id: str
    status: str
    next_action: str
    action_label: str
    output_quality_rollup: dict[str, Any] = Field(default_factory=dict)
    current_interrupt: dict[str, Any] | None = None
    dependency_review_status: str | None = None
    plan_stale: bool = False
    target_datasets: list[str] = Field(default_factory=list)
    runnable_datasets: list[str] = Field(default_factory=list)
    blocked_datasets: list[dict[str, Any]] = Field(default_factory=list)
    review_queue: list[dict[str, Any]] = Field(default_factory=list)
    datasets: list[DatasetProgressItem] = Field(default_factory=list)
    runtime_persistence: dict[str, Any] = Field(default_factory=dict)
    graph_state_path: str
    workflow_state_path: str | None = None


class DependencyReviewRequest(StrictBaseModel):
    """Persist a human decision on the graph-native dependency plan."""

    study_dir: NonEmptyStr
    reviewer: str = "local_user"
    decision: str
    notes: str = ""
    approved_dependency_datasets: list[str] = Field(default_factory=list)


class DependencyReviewResponse(StrictBaseModel):
    """Persisted dependency-review decision."""

    study_id: str
    run_id: str
    decision: str
    approved: bool
    current_interrupt: str | None = None
    graph_state_path: str
    workflow_state_path: str


class GenerateCodeRequest(StrictBaseModel):
    """Generate and persist R code for one dataset without executing it."""

    study_dir: NonEmptyStr
    study_id: str | None = None
    config_path: str | None = None
    rscript_path: str | None = None
    require_spec_approval: bool = True
    llm_provider_override: "LLMProviderOverride | None" = None
    llm_exposure_override: "LLMExposureOverride | None" = None


class DraftSpecRequest(StrictBaseModel):
    """Generate a review-required draft spec when no approved input spec exists."""

    study_dir: NonEmptyStr
    study_id: str | None = None
    config_path: str | None = None
    rscript_path: str | None = None
    llm_provider_override: "LLMProviderOverride | None" = None
    llm_exposure_override: "LLMExposureOverride | None" = None


class DraftSpecResponse(StrictBaseModel):
    """Reviewable draft spec returned before code generation."""

    study_id: str
    run_id: str
    dataset: str
    status: str
    spec_path: str
    prompt_path: str
    response_path: str
    variables: list[dict[str, Any]] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)
    workflow_control: str = "graph_gateway_compatibility_shim"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None


class FinalizeInputsRequest(StrictBaseModel):
    """Confirm upload is complete and decide whether draft-spec generation is required."""

    study_dir: NonEmptyStr
    study_id: str | None = None
    config_path: str | None = None
    rscript_path: str | None = None
    llm_provider_override: "LLMProviderOverride | None" = None
    llm_exposure_override: "LLMExposureOverride | None" = None


class FinalizeInputsResponse(StrictBaseModel):
    """Result of the upload-complete checkpoint for one target dataset."""

    study_id: str
    run_id: str
    dataset: str
    status: str
    input_spec_available: bool
    draft_spec_required: bool
    draft_spec_generated: bool = False
    approved_draft_spec_available: bool = False
    next_action: str
    message: str
    input_spec_path: str | None = None
    approved_spec_path: str | None = None
    draft_spec: DraftSpecResponse | None = None
    warnings: list[str] = Field(default_factory=list)
    workflow_control: str = "graph_gateway_compatibility_shim"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None


class DraftSpecReviewRequest(StrictBaseModel):
    """Persist a user's decision on a generated draft spec."""

    study_dir: NonEmptyStr
    reviewer: str = "local_user"
    decision: str
    notes: str = ""


class DraftSpecReviewResponse(StrictBaseModel):
    """Persisted draft-spec review decision."""

    study_id: str
    run_id: str
    dataset: str
    decision: str
    review_path: str
    approved: bool
    approved_spec_path: str | None = None
    workflow_control: str = "graph_gateway_compatibility_shim"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None


class LLMProviderOverride(StrictBaseModel):
    """Session-scoped LLM provider settings supplied by the browser UI."""

    provider: str
    model: str
    base_url: str | None = None
    api_key: str | None = None
    api_key_env: str | None = None
    timeout_seconds: float | None = None
    max_tokens: int | None = None
    allow_custom_base_url: bool = False
    custom_base_url_approved_by: str | None = None
    anthropic_version: str | None = None


class LLMExposureOverride(StrictBaseModel):
    """Session-scoped LLM data exposure policy supplied by the browser UI."""

    mode: str = "demo_rich_context"
    data_classification: str = "processed_demo"
    external_api_allowed: bool = False
    approved_by: str | None = None
    approval_note: str = ""
    sample_rows_per_dataset: int | None = None
    include_reference_rows: bool | None = None


class LLMConnectionTestRequest(StrictBaseModel):
    """Validate a browser-supplied LLM provider config without generating ADaM code."""

    llm_provider: LLMProviderOverride
    llm_exposure: LLMExposureOverride


class LLMConnectionTestResponse(StrictBaseModel):
    """Result of a one-shot provider connectivity check."""

    status: str
    provider: str
    model: str
    provider_alias: str | None = None
    transport: str | None = None
    provider_base_url: str | None = None
    external_relay: bool = False
    risk_flags: list[str] = Field(default_factory=list)
    response_preview: str = ""
    note: str = ""


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
    draft_spec_path: str | None = None
    response_path: str | None = None
    parsed_response_path: str | None = None
    static_check_path: str | None = None
    dependency_review_status: str | None = None
    warnings: list[str] = Field(default_factory=list)
    workflow_control: str = "graph_gateway_compatibility_shim"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None


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
    static_check_path: str | None = None
    workflow_control: str = "graph_gateway_compatibility_shim"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None


class ExecuteCodeRequest(StrictBaseModel):
    """Execute previously approved generated R code in the local R boundary."""

    study_dir: NonEmptyStr
    study_id: str | None = None
    rscript_path: str | None = None


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
    terminal_failure: bool = False
    errors: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)
    workflow_control: str = "graph_gateway_compatibility_shim"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None


class TerminalFailureReviewRequest(StrictBaseModel):
    """Persist a human triage decision after terminal execution failure."""

    study_dir: NonEmptyStr
    reviewer: str = "local_user"
    decision: str
    notes: str = ""


class TerminalFailureReviewResponse(StrictBaseModel):
    """Persisted terminal-failure triage decision."""

    study_id: str
    run_id: str
    dataset: str
    decision: str
    current_interrupt: str | None = None
    next_action: str
    graph_state_path: str
    workflow_state_path: str


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
    workflow_control: str = "legacy_run_to_completion_compatibility_shim"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None
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
    output_quality: dict[str, Any] = Field(default_factory=dict)
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
    read_model_source: str = "unknown"
    graph_state_path: str | None = None
    workflow_state_path: str | None = None
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
