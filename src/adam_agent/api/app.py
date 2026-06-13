"""FastAPI application for the local ADaM Agent Studio backend."""

from __future__ import annotations

from typing import Any

from fastapi import FastAPI, File, HTTPException, Query, UploadFile
from fastapi.responses import FileResponse, HTMLResponse

from adam_agent.api.models import (
    ArtifactReadRequest,
    CodeReviewRequest,
    CodeReviewResponse,
    DatasetCompareResponse,
    DependencyReviewRequest,
    DependencyReviewResponse,
    DemoStudyResponse,
    DraftSpecRequest,
    DraftSpecResponse,
    DraftSpecReviewRequest,
    DraftSpecReviewResponse,
    ExecuteCodeRequest,
    ExecuteCodeResponse,
    FileDeleteResponse,
    FileUploadResponse,
    FinalizeInputsRequest,
    FinalizeInputsResponse,
    GenerateCodeRequest,
    GenerateCodeResponse,
    GraphCommandRequest,
    GraphCommandResponse,
    LLMConnectionTestRequest,
    LLMConnectionTestResponse,
    NativeDatasetFullRunStartRequest,
    NativeDatasetFullRunStartResponse,
    NativeDatasetFullRunResumeRequest,
    NativeDatasetFullRunResumeResponse,
    NativeDatasetFullRunExecuteRequest,
    NativeDatasetResumeRequest,
    NativeDatasetResumeResponse,
    NativeStudyStartRequest,
    NativeStudyStartResponse,
    ProductSessionRequest,
    ProductSessionResponse,
    ProductWorkspaceResponse,
    RuntimeReadinessResponse,
    RunReviewSummary,
    RunProgressResponse,
    RunPlanRequest,
    RunPlanResponse,
    RunStudyRequest,
    RunStudyResponse,
    StudyWorkspaceRequest,
    StudyInputSummary,
    TablePageResponse,
    TerminalFailureReviewRequest,
    TerminalFailureReviewResponse,
)
from adam_agent.api.service import (
    ApiServiceError,
    build_run_review_summary,
    compare_dataset_with_reference,
    cleanup_expired_product_sessions,
    close_product_session,
    create_default_product_workspace,
    dataset_download_path,
    delete_study_input_file,
    ensure_study_workspace,
    execute_approved_dataset_code,
    finalize_dataset_inputs,
    generate_dataset_draft_spec,
    generate_dataset_code,
    persist_code_review,
    persist_dependency_review,
    persist_draft_spec_review,
    persist_terminal_failure_review,
    prepare_run_plan,
    prepare_demo_study,
    read_dataset_table_page,
    read_run_graph_state,
    read_run_json_artifact,
    read_run_progress,
    build_runtime_readiness,
    resume_native_dataset_full_run,
    execute_native_dataset_full_run,
    resume_native_dataset_interrupt,
    run_study_from_request,
    save_uploaded_file_bytes,
    start_native_dataset_full_run,
    start_native_study_product_loop,
    submit_graph_command,
    summarize_study_inputs,
    test_llm_connection,
    touch_product_session,
)
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

    @app.get("/runtime/readiness", response_model=RuntimeReadinessResponse)
    def runtime_readiness() -> RuntimeReadinessResponse:
        try:
            return build_runtime_readiness()
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.get("/", response_class=HTMLResponse)
    def index() -> str:
        return INDEX_HTML

    @app.post("/demo-study", response_model=DemoStudyResponse)
    def create_demo_study(
        demo_source_dir: str | None = Query(None, description="Optional source demo-data folder override."),
        study_dir: str | None = Query(None, description="Optional output study folder override."),
    ) -> DemoStudyResponse:
        try:
            return prepare_demo_study(demo_source_dir=demo_source_dir, study_dir=study_dir)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/product-workspace", response_model=ProductWorkspaceResponse)
    def product_workspace() -> ProductWorkspaceResponse:
        try:
            return create_default_product_workspace()
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/product-session/touch", response_model=ProductSessionResponse)
    def product_session_touch(request: ProductSessionRequest) -> ProductSessionResponse:
        try:
            return touch_product_session(session_id=request.session_id, study_dir=request.study_dir)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/product-session/close", response_model=ProductSessionResponse)
    def product_session_close(request: ProductSessionRequest) -> ProductSessionResponse:
        try:
            return close_product_session(session_id=request.session_id, study_dir=request.study_dir)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/product-session/cleanup", response_model=ProductSessionResponse)
    def product_session_cleanup() -> ProductSessionResponse:
        try:
            return cleanup_expired_product_sessions()
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/studies/workspace", response_model=StudyInputSummary)
    def create_or_open_workspace(request: StudyWorkspaceRequest) -> StudyInputSummary:
        try:
            return ensure_study_workspace(request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/studies/files", response_model=FileUploadResponse)
    async def upload_study_files(
        study_dir: str = Query(..., description="Path to the local study folder."),
        role: str = Query(..., description="Input role: sdtm, spec, define, reference, or legacy."),
        study_id: str | None = Query(None, description="Optional study id override."),
        files: list[UploadFile] = File(...),
    ) -> FileUploadResponse:
        try:
            file_bytes = [(file.filename or "uploaded_file", await file.read()) for file in files]
            normalized_role, folder, saved, summary, upload_state = save_uploaded_file_bytes(
                study_dir=study_dir,
                role=role,
                files=file_bytes,
                study_id=study_id,
            )
            return FileUploadResponse(
                study_id=summary.study_id,
                study_dir=summary.study_dir,
                role=normalized_role,
                folder=folder,
                saved_files=saved,
                input_summary=summary,
                input_fingerprint=upload_state.get("input_fingerprint", {}),
                input_diff=upload_state.get("input_diff", {}),
                touched_runs=upload_state.get("touched_runs", []),
                touched_graph_runs=upload_state.get("touched_graph_runs", []),
                skipped_graph_runs=upload_state.get("skipped_graph_runs", []),
            )
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.delete("/studies/files", response_model=FileDeleteResponse)
    def delete_study_file(
        study_dir: str = Query(..., description="Path to the local study folder."),
        role: str = Query(..., description="Input role: sdtm, spec, define, reference, or legacy."),
        file_name: str = Query(..., description="Plain file name shown in the study input list."),
        study_id: str | None = Query(None, description="Optional study id override."),
    ) -> FileDeleteResponse:
        try:
            normalized_role, folder, deleted, summary, upload_state = delete_study_input_file(
                study_dir=study_dir,
                role=role,
                file_name=file_name,
                study_id=study_id,
            )
            return FileDeleteResponse(
                study_id=summary.study_id,
                study_dir=summary.study_dir,
                role=normalized_role,
                folder=folder,
                deleted_file=deleted,
                input_summary=summary,
                input_fingerprint=upload_state.get("input_fingerprint", {}),
                input_diff=upload_state.get("input_diff", {}),
                touched_runs=upload_state.get("touched_runs", []),
                touched_graph_runs=upload_state.get("touched_graph_runs", []),
                skipped_graph_runs=upload_state.get("skipped_graph_runs", []),
            )
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs", response_model=RunStudyResponse)
    def create_run(request: RunStudyRequest) -> RunStudyResponse:
        try:
            return run_study_from_request(request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/prepare", response_model=RunPlanResponse)
    def prepare_run(request: RunPlanRequest) -> RunPlanResponse:
        try:
            return prepare_run_plan(request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/native-study-loop", response_model=NativeStudyStartResponse)
    def native_study_loop(request: NativeStudyStartRequest) -> NativeStudyStartResponse:
        try:
            return start_native_study_product_loop(request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/native-full-run", response_model=NativeDatasetFullRunStartResponse)
    def native_dataset_full_run(
        run_id: str,
        dataset: str,
        request: NativeDatasetFullRunStartRequest,
    ) -> NativeDatasetFullRunStartResponse:
        try:
            return start_native_dataset_full_run(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/native-resume", response_model=NativeDatasetResumeResponse)
    def native_dataset_resume(
        run_id: str,
        dataset: str,
        request: NativeDatasetResumeRequest,
    ) -> NativeDatasetResumeResponse:
        try:
            return resume_native_dataset_interrupt(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post(
        "/runs/{run_id}/datasets/{dataset}/native-full-run/resume",
        response_model=NativeDatasetFullRunResumeResponse,
    )
    def native_dataset_full_run_resume(
        run_id: str,
        dataset: str,
        request: NativeDatasetFullRunResumeRequest,
    ) -> NativeDatasetFullRunResumeResponse:
        try:
            return resume_native_dataset_full_run(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post(
        "/runs/{run_id}/datasets/{dataset}/native-full-run/execute",
        response_model=ExecuteCodeResponse,
    )
    def native_dataset_full_run_execute(
        run_id: str,
        dataset: str,
        request: NativeDatasetFullRunExecuteRequest,
    ) -> ExecuteCodeResponse:
        try:
            return execute_native_dataset_full_run(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    # Compatibility endpoint for older clients. The browser product flow records
    # dependency decisions through /graph-command.
    @app.post("/runs/{run_id}/dependency-review", response_model=DependencyReviewResponse)
    def dependency_review(run_id: str, request: DependencyReviewRequest) -> DependencyReviewResponse:
        try:
            return persist_dependency_review(run_id, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/graph-command", response_model=GraphCommandResponse)
    def graph_command(run_id: str, request: GraphCommandRequest) -> GraphCommandResponse:
        try:
            return submit_graph_command(run_id, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/generate-code", response_model=GenerateCodeResponse)
    def generate_code(run_id: str, dataset: str, request: GenerateCodeRequest) -> GenerateCodeResponse:
        try:
            return generate_dataset_code(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/finalize-inputs", response_model=FinalizeInputsResponse)
    def finalize_inputs(run_id: str, dataset: str, request: FinalizeInputsRequest) -> FinalizeInputsResponse:
        try:
            return finalize_dataset_inputs(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/draft-spec", response_model=DraftSpecResponse)
    def draft_spec(run_id: str, dataset: str, request: DraftSpecRequest) -> DraftSpecResponse:
        try:
            return generate_dataset_draft_spec(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/draft-spec-review", response_model=DraftSpecReviewResponse)
    def draft_spec_review(run_id: str, dataset: str, request: DraftSpecReviewRequest) -> DraftSpecReviewResponse:
        try:
            return persist_draft_spec_review(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/llm/test-connection", response_model=LLMConnectionTestResponse)
    def llm_test_connection(request: LLMConnectionTestRequest) -> LLMConnectionTestResponse:
        try:
            return test_llm_connection(request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/code-review", response_model=CodeReviewResponse)
    def code_review(run_id: str, dataset: str, request: CodeReviewRequest) -> CodeReviewResponse:
        try:
            return persist_code_review(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/execute-approved-code", response_model=ExecuteCodeResponse)
    def execute_approved_code(run_id: str, dataset: str, request: ExecuteCodeRequest) -> ExecuteCodeResponse:
        try:
            return execute_approved_dataset_code(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.post("/runs/{run_id}/datasets/{dataset}/terminal-failure-review", response_model=TerminalFailureReviewResponse)
    def terminal_failure_review(
        run_id: str,
        dataset: str,
        request: TerminalFailureReviewRequest,
    ) -> TerminalFailureReviewResponse:
        try:
            return persist_terminal_failure_review(run_id, dataset, request)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.get("/study-inputs", response_model=StudyInputSummary)
    def study_inputs(
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> StudyInputSummary:
        try:
            return summarize_study_inputs(study_dir)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.get("/runs/{run_id}/review-summary", response_model=RunReviewSummary)
    def run_review_summary(
        run_id: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
        detail_level: str = Query("full", description="summary or full."),
    ) -> RunReviewSummary:
        try:
            return build_run_review_summary(study_dir, run_id, detail_level=detail_level)
        except ApiServiceError as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc

    @app.get("/runs/{run_id}/dependency-plan")
    def dependency_plan(
        run_id: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> dict[str, Any]:
        return _read_artifact(study_dir, run_id, "planning/dependency_plan.json")

    @app.get("/runs/{run_id}/graph-state")
    def graph_state(
        run_id: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> dict[str, Any]:
        try:
            return read_run_graph_state(study_dir, run_id)
        except ApiServiceError as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc

    @app.get("/runs/{run_id}/progress", response_model=RunProgressResponse)
    def run_progress(
        run_id: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> RunProgressResponse:
        try:
            return read_run_progress(study_dir, run_id)
        except ApiServiceError as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc

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

    @app.get("/runs/{run_id}/datasets/{dataset}/table", response_model=TablePageResponse)
    def dataset_table(
        run_id: str,
        dataset: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
        kind: str = Query("generated", description="generated or reference."),
        page: int = Query(1, ge=1),
        page_size: int = Query(25, ge=1, le=200),
    ) -> TablePageResponse:
        try:
            return read_dataset_table_page(study_dir, run_id, dataset, kind=kind, page=page, page_size=page_size)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.get("/runs/{run_id}/datasets/{dataset}/compare", response_model=DatasetCompareResponse)
    def dataset_compare(
        run_id: str,
        dataset: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
    ) -> DatasetCompareResponse:
        try:
            return compare_dataset_with_reference(study_dir, run_id, dataset)
        except ApiServiceError as exc:
            raise HTTPException(status_code=400, detail=str(exc)) from exc

    @app.get("/runs/{run_id}/datasets/{dataset}/download")
    def dataset_download(
        run_id: str,
        dataset: str,
        study_dir: str = Query(..., description="Path to the local study folder."),
        kind: str = Query(..., description="generated, reference, code, validation_report, or compare_report."),
    ) -> FileResponse:
        try:
            path = dataset_download_path(study_dir, run_id, dataset, kind)
            return FileResponse(path, filename=path.name)
        except ApiServiceError as exc:
            raise HTTPException(status_code=404, detail=str(exc)) from exc

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
