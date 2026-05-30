"""Service helpers behind the FastAPI routes."""

from __future__ import annotations

import csv
import io
import json
import re
import shutil
from datetime import datetime
from pathlib import Path
from typing import Any

from pydantic import ValidationError

from adam_agent.api.models import (
    CodeReviewResponse,
    DatasetCompareResponse,
    DatasetReview,
    DependencyReviewResponse,
    DemoStudyResponse,
    DownloadItem,
    DraftSpecResponse,
    DraftSpecReviewResponse,
    ExecuteCodeResponse,
    FilePreview,
    FinalizeInputsResponse,
    GenerateCodeResponse,
    LLMConnectionTestResponse,
    ProductWorkspaceResponse,
    RunReviewSummary,
    RunPlanRequest,
    RunPlanResponse,
    RunStudyRequest,
    RunStudyResponse,
    StudyWorkspaceRequest,
    StudyInputSummary,
    TablePageResponse,
    TerminalFailureReviewResponse,
)
from adam_agent.downstream.runner import StructuralStubRRunner
from adam_agent.graph.dependency_resolution import (
    available_dependency_targets,
    blocked_dependency_targets,
    missing_dependency_blocks,
    resolve_dependency_availability,
)
from adam_agent.graph.dependencies import plan_dataset_dependencies
from adam_agent.graph.gateway import GraphGateway
from adam_agent.graph.study_graph import compile_study_graph
from adam_agent.graph.workflow_state import (
    compare_fingerprints,
    input_fingerprint,
    invalidate_active_workflows,
    load_workflow_state,
    mark_workflow_inputs_current,
    update_workflow_state,
)
from adam_agent.llm.clients import (
    LLMClientConfigError,
    LLMProviderConfig,
    LLMProviderResponseError,
    LLMRequest,
    build_llm_client,
)
from adam_agent.schemas.graph_state import HumanCommand
from adam_agent.llm.context import build_target_llm_context, write_llm_context_package
from adam_agent.llm.generated_code import (
    LLMGeneratedCodeError,
    parse_generated_code_response,
    write_generated_code_artifacts,
)
from adam_agent.llm.mock_code import default_mock_generated_code_response
from adam_agent.llm.prompt_compaction import (
    MAX_SAMPLE_ROWS_IN_PROMPT,
    compact_prompt_from_context,
    write_compact_prompt_artifact,
)
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.llm import LLMExposureConfig
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.config import ConfigLoader
from adam_agent.tools.r_runner import LocalRRunner, RRunRequest
from adam_agent.tools.sdtm_reader import SDTMReader
from adam_agent.tools.study_inputs import StudyInputScanner


class ApiServiceError(RuntimeError):
    """Raised when an API request cannot be fulfilled safely."""


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_DEMO_SOURCE_DIR = ROOT.parent / "ADaM_Shiny-ADaM_Shiny_experimental" / "demo-data"
DEFAULT_DEMO_STUDY_ROOT = ROOT / ".tmp_tests" / "ui_demo_study"
DEFAULT_PRODUCT_STUDY_ROOT = ROOT / ".tmp_tests" / "local_product_studies"
DEFAULT_DEMO_CONFIG_PATH = ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"
DEFAULT_LOCAL_RSCRIPT = Path(r"C:\Dev\R-4.5.2\bin\Rscript.exe")
STUDY_INPUT_FOLDERS = ["input_sdtm", "input_spec", "input_define", "reference_adam", "legacy_code", "runs"]
UPLOAD_ROLE_TO_FOLDER = {
    "sdtm": "input_sdtm",
    "spec": "input_spec",
    "define": "input_define",
    "reference": "reference_adam",
    "legacy": "legacy_code",
}
MAX_DOWNLOAD_BYTES = 200 * 1024 * 1024
DEFAULT_TABLE_PAGE_SIZE = 25
MAX_TABLE_PAGE_SIZE = 200
GRAPH_GATEWAY_COMPATIBILITY_SHIM = "graph_gateway_compatibility_shim"


def _graph_compatibility_metadata(study_dir: str | Path, run_id: str) -> dict[str, str]:
    """Return explicit metadata for old endpoints that now write graph state."""

    root = Path(study_dir)
    run_dir = root / "runs" / run_id
    return {
        "workflow_control": GRAPH_GATEWAY_COMPATIBILITY_SHIM,
        "graph_state_path": str((run_dir / "graph_state.json").as_posix()),
        "workflow_state_path": str((run_dir / "workflow_state.json").as_posix()),
    }


def ensure_study_workspace(request: StudyWorkspaceRequest) -> StudyInputSummary:
    """Create or open a local study workspace and return its input summary."""

    root = Path(request.study_dir).expanduser()
    root.mkdir(parents=True, exist_ok=True)
    for folder in STUDY_INPUT_FOLDERS:
        (root / folder).mkdir(parents=True, exist_ok=True)
    return summarize_study_inputs(root, study_id=request.study_id or root.name)


def create_default_product_workspace() -> ProductWorkspaceResponse:
    """Create a default local workspace without making the user choose a path."""

    stamp = datetime.now().strftime("%Y%m%d%H%M%S%f")
    study_id = f"study_{stamp[:14]}"
    root = DEFAULT_PRODUCT_STUDY_ROOT / study_id
    summary = ensure_study_workspace(StudyWorkspaceRequest(study_dir=str(root), study_id=study_id))
    return ProductWorkspaceResponse(
        study_id=study_id,
        study_dir=str(root.resolve().as_posix()),
        run_id=f"run_{stamp[:14]}",
        target_datasets=[],
        config_path=str(DEFAULT_DEMO_CONFIG_PATH.as_posix()),
        rscript_path=str(DEFAULT_LOCAL_RSCRIPT.as_posix()) if DEFAULT_LOCAL_RSCRIPT.exists() else None,
        input_summary=summary,
        notes=[
            "A local workspace was created automatically for this browser session.",
            "Technical paths are hidden in Advanced settings unless needed for debugging.",
        ],
    )


def save_uploaded_file_bytes(
    *,
    study_dir: str | Path,
    role: str,
    files: list[tuple[str, bytes]],
    study_id: str | None = None,
) -> tuple[str, str, list[str], StudyInputSummary, dict[str, Any]]:
    """Save uploaded browser files into the canonical study input folder."""

    normalized_role = role.strip().lower()
    folder_name = UPLOAD_ROLE_TO_FOLDER.get(normalized_role)
    if folder_name is None:
        allowed = ", ".join(sorted(UPLOAD_ROLE_TO_FOLDER))
        raise ApiServiceError(f"Unsupported upload role: {role}. Allowed roles: {allowed}")
    root = Path(study_dir).expanduser()
    if not root.exists() or not root.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {root}")
    folder = root / folder_name
    folder.mkdir(parents=True, exist_ok=True)
    saved: list[str] = []
    for filename, content in files:
        safe_name = _safe_upload_name(filename)
        target = folder / safe_name
        target.write_bytes(content)
        saved.append(str(target.as_posix()))
    summary = summarize_study_inputs(root, study_id=study_id or root.name)
    upload_state = invalidate_active_workflows(root)
    return normalized_role, folder_name, saved, summary, upload_state


def prepare_demo_study(
    *,
    demo_source_dir: str | Path | None = None,
    study_dir: str | Path | None = None,
) -> DemoStudyResponse:
    """Prepare a local demo study from the root files in the Shiny demo-data folder."""

    source = Path(demo_source_dir).expanduser() if demo_source_dir else DEFAULT_DEMO_SOURCE_DIR
    if not source.exists() or not source.is_dir():
        raise ApiServiceError(f"Demo source folder does not exist: {source}")

    stamp = datetime.now().strftime("%Y%m%d%H%M%S%f")
    target = Path(study_dir).expanduser() if study_dir else DEFAULT_DEMO_STUDY_ROOT / f"demo_adam_{stamp}"
    if study_dir is None:
        _clear_demo_input_folders(target)
    target.mkdir(parents=True, exist_ok=True)
    for folder in ["input_sdtm", "input_spec", "reference_adam", "legacy_code", "runs"]:
        (target / folder).mkdir(parents=True, exist_ok=True)

    created_files: list[str] = []
    notes: list[str] = []
    _copy_required(source / "ae.csv", target / "input_sdtm" / "ae.csv", created_files)
    _copy_required(source / "dm.csv", target / "input_sdtm" / "dm.csv", created_files)
    _copy_required(source / "ex.csv", target / "input_sdtm" / "ex.csv", created_files)
    _copy_required(source / "adsl.csv", target / "reference_adam" / "adsl.csv", created_files)
    _copy_required(source / "adae.csv", target / "reference_adam" / "adae.csv", created_files)
    _copy_required(source / "ads_adae_full.csv", target / "input_spec" / "ads_adae_full.csv", created_files)
    _copy_first_required(
        [source / "ads_adsl_full.csv", source / "ads_dasl_full.csv"],
        target / "input_spec" / "ads_adsl_full.csv",
        created_files,
        logical_name="ADSL spec CSV",
    )

    notes.extend(
        [
            "Demo data is copied only from root-level files in the original Shiny demo-data folder.",
            "input_sdtm contains AE/DM/EX CSV source data.",
            "input_spec contains ads_adae_full.csv and ads_adsl_full.csv as user-provided spec evidence.",
            "reference_adam contains adsl.csv and adae.csv as final comparison evidence.",
            "PSY201 is a separate project and is intentionally not copied into this demo.",
        ]
    )

    return DemoStudyResponse(
        study_id=target.name,
        study_dir=str(target.resolve().as_posix()),
        demo_source_dir=str(source.resolve().as_posix()),
        run_id=f"run_ui_{stamp[:14]}",
        target_datasets=["ADAE"],
        config_path=str(DEFAULT_DEMO_CONFIG_PATH.as_posix()),
        execution_mode="llm_downstream_r_sandbox" if DEFAULT_LOCAL_RSCRIPT.exists() else "llm_downstream_provider",
        rscript_path=str(DEFAULT_LOCAL_RSCRIPT.as_posix()) if DEFAULT_LOCAL_RSCRIPT.exists() else None,
        created_files=created_files,
        notes=notes,
    )


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
    if execution_mode in {"llm_downstream_provider", "llm_downstream_r_sandbox"}:
        update_workflow_state(
            study_dir,
            config.run_id,
            study_id=study_id,
            node="run_study_request_blocked",
            status="blocked",
            current_interrupt="split_flow_required",
            input_fingerprint_payload=input_fingerprint(study_dir),
            extra={
                "requested_datasets": [str(item).strip().upper() for item in request.target_datasets],
                "execution_mode": execution_mode,
                "blocked_reason": "LLM ADaM generation must use the draft/spec/code-review/execute API flow.",
            },
        )
        raise ApiServiceError(
            "LLM ADaM generation cannot run through POST /runs because it would bypass review gates. "
            "Use /runs/prepare, finalize-inputs, draft-spec-review, generate-code, code-review, "
            "and execute-approved-code."
        )

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
    response = _response_from_graph_result(result, execution_mode=execution_mode, study_dir=study_dir)
    update_workflow_state(
        study_dir,
        config.run_id,
        study_id=study_id,
        node="run_study_request",
        status=response.status,
        input_fingerprint_payload=input_fingerprint(study_dir),
        extra={
            "requested_datasets": response.requested_datasets,
            "target_datasets": response.target_datasets,
            "runnable_datasets": response.runnable_datasets,
            "blocked_datasets": response.blocked_datasets,
            "dependency_review_status": response.dependency_review_status,
            "execution_mode": response.execution_mode,
            "dataset_results": response.dataset_results,
            "audit_manifest": response.audit_manifest,
        },
    )
    return response


def prepare_run_plan(request: RunPlanRequest) -> RunPlanResponse:
    """Plan dataset dependencies without generating code or running R."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    study_id = request.study_id or study_dir.name
    gateway_result = GraphGateway().start_dependency_plan(
        study_dir=study_dir,
        study_id=study_id,
        run_id=request.run_id,
        target_datasets=list(request.target_datasets),
        approved_dependency_datasets=request.approved_dependency_datasets,
    )
    graph_state = gateway_result.graph_state
    plan_payload = graph_state.dependency_plan
    blocked = list(graph_state.blocked_datasets)
    unsupported = plan_payload.get("unsupported_datasets", [])
    return RunPlanResponse(
        study_id=study_id,
        run_id=request.run_id,
        requested_datasets=list(graph_state.requested_datasets),
        target_datasets=list(graph_state.target_datasets),
        runnable_datasets=list(graph_state.runnable_datasets),
        blocked_datasets=blocked + [
            {"dataset": dataset, "reason": "unsupported_dataset", "blocked_by": "study_planner"}
            for dataset in unsupported
        ],
        dependency_review_status=graph_state.dependency_review_status or "accepted",
        dependency_decisions=list(graph_state.dependency_decisions),
        dependency_resolution=list(graph_state.dependency_resolution),
        dependency_warnings=list(plan_payload.get("dependency_planning_warnings", [])),
        workflow_state_path=str((study_dir / "runs" / request.run_id / "workflow_state.json").as_posix()),
    )


def persist_dependency_review(run_id: str, request: Any) -> DependencyReviewResponse:
    """Persist a human decision for the graph-native dependency review gate."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    decision = request.decision.strip().lower()
    if decision not in {"approve", "reject"}:
        raise ApiServiceError("Dependency review decision must be approve or reject.")
    gateway = GraphGateway()
    try:
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
    except FileNotFoundError as exc:
        raise ApiServiceError(str(exc)) from exc
    if graph_state.current_interrupt is None or graph_state.current_interrupt.name != "dependency_review":
        raise ApiServiceError("Current graph state is not waiting for dependency_review.")
    result = gateway.resume(
        study_dir=study_dir,
        graph_state=graph_state,
        command=HumanCommand(
            interrupt="dependency_review",
            action="approve" if decision == "approve" else "reject",
            reviewer=request.reviewer,
            notes=request.notes,
            payload={
                "approved_dependency_datasets": [
                    str(item).strip().upper()
                    for item in getattr(request, "approved_dependency_datasets", [])
                    if str(item).strip()
                ]
            },
        ),
    )
    current_interrupt = None
    if result.graph_state.current_interrupt is not None and result.graph_state.current_interrupt.status == "open":
        current_interrupt = result.graph_state.current_interrupt.name
    return DependencyReviewResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        decision=decision,
        approved=decision == "approve",
        current_interrupt=current_interrupt,
        graph_state_path=str((study_dir / "runs" / run_id / "graph_state.json").as_posix()),
        workflow_state_path=str((study_dir / "runs" / run_id / "workflow_state.json").as_posix()),
    )


def test_llm_connection(request: Any) -> LLMConnectionTestResponse:
    """Run a minimal provider call using browser-supplied settings without persisting secrets."""

    provider_config = _provider_config_from_override(request.llm_provider, fallback=LLMProviderConfig())
    if provider_config.provider.strip().lower() == "mock":
        raise ApiServiceError("Use a real provider for connection testing, not mock mode.")
    exposure = _exposure_config_from_override(request.llm_exposure, fallback=LLMExposureConfig())
    try:
        client = build_llm_client(provider_config)
        response = client.generate(
            LLMRequest(
                prompt="Reply with exactly: ADAM_AGENT_CONNECTION_OK",
                system_prompt="You are testing provider connectivity. Keep the response short.",
                provider=provider_config.provider,
                model=provider_config.model,
                exposure=exposure,
                node="ui_llm_connection_test",
                call_id="ui_llm_connection_test",
                max_tokens=32,
                prompt_artifact_id="ui_connection_test_prompt",
                response_artifact_id="ui_connection_test_response",
                redaction_policy="ui_connection_test_no_study_data",
            )
        )
    except (LLMClientConfigError, LLMProviderResponseError) as exc:
        raise ApiServiceError(str(exc)) from exc
    record = response.call_record
    return LLMConnectionTestResponse(
        status="ok",
        provider=provider_config.provider,
        model=provider_config.model,
        provider_alias=record.provider_alias,
        transport=record.transport,
        provider_base_url=record.provider_base_url,
        external_relay=record.external_relay,
        risk_flags=record.risk_flags,
        response_preview=response.response_text[:200],
        note="Connection test succeeded. No study data was sent.",
    )


def generate_dataset_code(run_id: str, dataset: str, request: Any) -> GenerateCodeResponse:
    """Generate and persist R code for one dataset without executing it."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    try:
        GraphGateway().validate_product_step_start(study_dir=study_dir, run_id=run_id, dataset=target, step="generate_code")
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    mark_workflow_inputs_current(study_dir, run_id, study_id=study_id, node="generate_code_start")
    config = ConfigLoader().load(request.config_path, study_id=study_id, run_id=run_id)
    gateway = GraphGateway()
    try:
        plan = gateway.dependency_gate_for_product_step(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    provider_config = _provider_config_from_override(request.llm_provider_override, fallback=config.llm_provider)
    exposure = _exposure_config_from_override(request.llm_exposure_override, fallback=config.llm_exposure)
    try:
        result = gateway.generate_code(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            dependency_resolution=plan.dependency_resolution,
            llm_provider=provider_config.__dict__,
            llm_exposure=exposure.model_dump(mode="json"),
            llm_client_builder=build_llm_client,
            target_context_builder=build_target_llm_context,
            rscript_path=getattr(request, "rscript_path", None) or "",
            dependency_artifacts=_dependency_artifacts_for_dataset(plan.dependency_resolution, target),
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    warnings = result.warnings + plan.dependency_warnings + [
        "Static R checks are limited guardrails before human review; they do not prove full CDISC/ADaM IG/P21 compliance."
    ]
    return GenerateCodeResponse(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="code_generated",
        code_path=result.code_path,
        generated_code=result.generated_code,
        assumptions=result.assumptions,
        risk_points=result.risk_points,
        used_inputs=result.used_inputs,
        expected_outputs=result.expected_outputs,
        context_path=result.context_path,
        draft_spec_path=result.draft_spec_path,
        response_path=result.response_path,
        parsed_response_path=result.parsed_response_path,
        static_check_path=result.static_check_path,
        dependency_review_status=plan.dependency_review_status,
        warnings=warnings,
        **_graph_compatibility_metadata(study_dir, run_id),
    )


def finalize_dataset_inputs(run_id: str, dataset: str, request: Any) -> FinalizeInputsResponse:
    """Confirm uploads are complete and generate a reviewable draft spec only if needed."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    try:
        GraphGateway().validate_product_step_start(study_dir=study_dir, run_id=run_id, dataset=target, step="finalize_inputs")
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    mark_workflow_inputs_current(study_dir, run_id, study_id=study_id, node="finalize_inputs_start")
    config = ConfigLoader().load(request.config_path, study_id=study_id, run_id=run_id)
    gateway = GraphGateway()
    try:
        plan = gateway.dependency_gate_for_product_step(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    provider_config = _provider_config_from_override(request.llm_provider_override, fallback=config.llm_provider)
    exposure = _exposure_config_from_override(request.llm_exposure_override, fallback=config.llm_exposure)
    try:
        result = gateway.finalize_inputs(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            dependency_resolution=plan.dependency_resolution,
            llm_provider=provider_config.__dict__,
            llm_exposure=exposure.model_dump(mode="json"),
            llm_client_builder=build_llm_client,
            target_context_builder=build_target_llm_context,
            rscript_path=getattr(request, "rscript_path", None) or "",
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    warnings = result.warnings + plan.dependency_warnings
    if result.spec_source == "input_spec":
        return FinalizeInputsResponse(
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status="input_spec_ready",
            input_spec_available=True,
            draft_spec_required=False,
            next_action="generate_code",
            message=f"Approved input_spec found for {target}. Draft spec generation is not needed.",
            input_spec_path=result.input_spec_path,
            warnings=warnings,
            **_graph_compatibility_metadata(study_dir, run_id),
        )
    if result.spec_source == "approved_draft_spec":
        return FinalizeInputsResponse(
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status="approved_draft_spec_ready",
            input_spec_available=False,
            draft_spec_required=True,
            approved_draft_spec_available=True,
            next_action="generate_code",
            message=f"A previously approved draft spec is available for {target}.",
            approved_spec_path=result.approved_spec_path,
            warnings=warnings,
            **_graph_compatibility_metadata(study_dir, run_id),
        )
    draft_response = DraftSpecResponse(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="draft_spec_generated",
        spec_path=result.draft_spec_path or "",
        prompt_path=result.draft_spec_prompt_path or "",
        response_path=result.draft_spec_response_path or "",
        variables=list(result.draft_spec_variables or []),
        warnings=warnings,
        **_graph_compatibility_metadata(study_dir, run_id),
    )
    return FinalizeInputsResponse(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="draft_spec_review_required",
        input_spec_available=False,
        draft_spec_required=True,
        draft_spec_generated=True,
        next_action="review_draft_spec",
        message=(
            f"No approved input_spec was found for {target}. "
            "A draft spec was generated from uploaded evidence and must be reviewed before R code generation."
        ),
        draft_spec=draft_response,
        warnings=draft_response.warnings,
        **_graph_compatibility_metadata(study_dir, run_id),
    )


def generate_dataset_draft_spec(run_id: str, dataset: str, request: Any) -> DraftSpecResponse:
    """Generate a draft spec and return it for human review before code generation."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    try:
        GraphGateway().validate_product_step_start(study_dir=study_dir, run_id=run_id, dataset=target, step="draft_spec")
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    mark_workflow_inputs_current(study_dir, run_id, study_id=study_id, node="draft_spec_start")
    config = ConfigLoader().load(request.config_path, study_id=study_id, run_id=run_id)
    gateway = GraphGateway()
    try:
        plan = gateway.dependency_gate_for_product_step(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    provider_config = _provider_config_from_override(request.llm_provider_override, fallback=config.llm_provider)
    exposure = _exposure_config_from_override(request.llm_exposure_override, fallback=config.llm_exposure)
    try:
        result = gateway.generate_draft_spec(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            dependency_resolution=plan.dependency_resolution,
            llm_provider=provider_config.__dict__,
            llm_exposure=exposure.model_dump(mode="json"),
            llm_client_builder=build_llm_client,
            target_context_builder=build_target_llm_context,
            rscript_path=getattr(request, "rscript_path", None) or "",
        )
    except ValueError as exc:
        raise ApiServiceError(f"Draft spec generation failed: {exc}") from exc
    warnings = result.warnings + plan.dependency_warnings
    return DraftSpecResponse(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="draft_spec_generated",
        spec_path=result.draft_spec_path or "",
        prompt_path=result.draft_spec_prompt_path or "",
        response_path=result.draft_spec_response_path or "",
        variables=list(result.draft_spec_variables or []),
        warnings=warnings,
        **_graph_compatibility_metadata(study_dir, run_id),
    )


def persist_draft_spec_review(run_id: str, dataset: str, request: Any) -> DraftSpecReviewResponse:
    """Persist a human decision for a generated draft spec."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    decision = request.decision.strip().lower()
    if decision not in {"approve", "reject"}:
        raise ApiServiceError("Draft spec review decision must be approve or reject.")
    gateway = GraphGateway()
    try:
        result = gateway.review_draft_spec(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id=run_id,
            dataset=target,
            decision=decision,
            reviewer=request.reviewer,
            notes=request.notes,
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    return DraftSpecReviewResponse(
        study_id=study_dir.name,
        run_id=run_id,
        dataset=target,
        decision=result.decision,
        review_path=result.review_path,
        approved=result.approved,
        approved_spec_path=result.approved_spec_path,
        **_graph_compatibility_metadata(study_dir, run_id),
    )


def persist_code_review(run_id: str, dataset: str, request: Any) -> CodeReviewResponse:
    """Persist a local code-review decision for generated R code."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    decision = request.decision.strip().lower()
    if decision not in {"approve", "reject"}:
        raise ApiServiceError("Code review decision must be approve or reject.")
    study_id = study_dir.name
    gateway = GraphGateway()
    try:
        result = gateway.review_code(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            decision=decision,
            reviewer=request.reviewer,
            notes=request.notes,
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    return CodeReviewResponse(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        decision=result.decision,
        review_path=result.review_path,
        approved=result.approved,
        static_check_path=result.static_check_path,
        **_graph_compatibility_metadata(study_dir, run_id),
    )


def execute_approved_dataset_code(run_id: str, dataset: str, request: Any) -> ExecuteCodeResponse:
    """Execute an already-generated and approved R script."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    gateway = GraphGateway()
    try:
        gateway.dependency_gate_for_product_step(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            start_if_missing=False,
        )
    except FileNotFoundError as exc:
        raise ApiServiceError("Graph state does not exist for this run. Generate code through the graph flow before execution.") from exc
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    try:
        result = gateway.execute_approved_code(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            rscript_path=getattr(request, "rscript_path", None) or "",
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    return ExecuteCodeResponse(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status=result.status,
        validation_status=result.validation_status,
        output_path=result.output_path,
        validation_report_path=result.validation_report_path,
        diagnostics_path=result.diagnostics_path,
        terminal_failure=result.terminal_failure,
        errors=result.errors,
        warnings=result.warnings,
        **_graph_compatibility_metadata(study_dir, run_id),
    )


def persist_terminal_failure_review(run_id: str, dataset: str, request: Any) -> TerminalFailureReviewResponse:
    """Persist human triage for a terminal execution failure."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    decision = request.decision.strip().lower()
    allowed = {
        "retry_execution",
        "repair_code",
        "revise_spec",
        "request_new_input",
        "skip_dataset",
        "continue_other_datasets",
    }
    if decision not in allowed:
        raise ApiServiceError(
            "Terminal failure decision must be retry_execution, repair_code, revise_spec, "
            "request_new_input, skip_dataset, or continue_other_datasets."
        )
    gateway = GraphGateway()
    try:
        graph_state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
    except FileNotFoundError as exc:
        raise ApiServiceError(str(exc)) from exc
    dataset_state = graph_state.datasets.get(target)
    if dataset_state is None:
        raise ApiServiceError(f"Dataset is not part of this graph run: {target}")
    interrupt = dataset_state.current_interrupt
    if interrupt is None or interrupt.name != "terminal_failure" or interrupt.status != "open":
        raise ApiServiceError("Current dataset graph state is not waiting for terminal_failure review.")
    try:
        result = gateway.record_terminal_failure_review(
            study_dir=study_dir,
            study_id=graph_state.study_id,
            run_id=run_id,
            dataset=target,
            command=HumanCommand(
                interrupt="terminal_failure",
                action=decision,
                dataset=target,
                reviewer=request.reviewer,
                notes=request.notes,
                payload={"decision": decision},
            ),
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    reviewed_dataset = result.graph_state.datasets[target]
    current_interrupt = None
    if reviewed_dataset.current_interrupt is not None and reviewed_dataset.current_interrupt.status == "open":
        current_interrupt = reviewed_dataset.current_interrupt.name
    return TerminalFailureReviewResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        dataset=target,
        decision=decision,
        current_interrupt=current_interrupt,
        next_action=str(reviewed_dataset.execution_state.get("next_action") or ""),
        graph_state_path=str((study_dir / "runs" / run_id / "graph_state.json").as_posix()),
        workflow_state_path=str((study_dir / "runs" / run_id / "workflow_state.json").as_posix()),
    )


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


def read_run_graph_state(study_dir: str | Path, run_id: str) -> dict[str, Any]:
    """Read the canonical graph state for one local run."""

    try:
        state = GraphGateway().load_graph_state(study_dir=study_dir, run_id=run_id)
    except FileNotFoundError as exc:
        raise ApiServiceError(str(exc)) from exc
    return state.model_dump(mode="json")


def read_dataset_table_page(
    study_dir: str | Path,
    run_id: str,
    dataset: str,
    *,
    kind: str,
    page: int = 1,
    page_size: int = DEFAULT_TABLE_PAGE_SIZE,
) -> TablePageResponse:
    """Return a paginated browser page for generated or reference ADaM CSV output."""

    root = _validated_study_root(study_dir)
    target = dataset.strip().upper()
    resolved_kind = kind.strip().lower()
    path = _dataset_file_for_kind(root, run_id, target, resolved_kind)
    if path is None:
        return TablePageResponse(
            dataset=target,
            kind=resolved_kind,
            file_name="",
            format="",
            status="missing",
            note=f"No {resolved_kind} file was found for {target}.",
        )
    if path.suffix.lower() != ".csv":
        return TablePageResponse(
            dataset=target,
            kind=resolved_kind,
            file_name=path.name,
            format=path.suffix.lower().lstrip("."),
            status="not_previewed",
            note="Full table browsing is currently available for CSV outputs. sas7bdat files can be downloaded and profiled.",
        )
    return _read_csv_page(path, dataset=target, kind=resolved_kind, page=page, page_size=page_size)


def compare_dataset_with_reference(study_dir: str | Path, run_id: str, dataset: str) -> DatasetCompareResponse:
    """Compare generated ADaM output with a reference ADaM table when both are available."""

    root = _validated_study_root(study_dir)
    target = dataset.strip().upper()
    run_dir = _validated_run_dir(root, run_id)
    output_path = _usable_generated_output_path(run_dir, target)
    reference_path = _reference_path(root, target)
    compare = _compare_dataset_files(target, output_path, reference_path)
    return _record_compare_in_graph_state(root, run_id, target, compare)


def _record_compare_in_graph_state(root: Path, run_id: str, dataset: str, compare: DatasetCompareResponse) -> DatasetCompareResponse:
    gateway = GraphGateway()
    try:
        graph_state = gateway.load_graph_state(study_dir=root, run_id=run_id)
        study_id = graph_state.study_id
    except FileNotFoundError:
        return compare
    result = gateway.record_compare(
        study_dir=root,
        study_id=study_id,
        run_id=run_id,
        dataset=dataset,
        compare_summary=compare.model_dump(mode="json"),
        write_compare_report=True,
        input_fingerprint_payload=input_fingerprint(root),
    )
    dataset_state = result.graph_state.datasets.get(dataset.strip().upper())
    if dataset_state is None:
        return compare
    payload = dict(dataset_state.compare_summary)
    payload = {key: value for key, value in payload.items() if key in DatasetCompareResponse.model_fields}
    return DatasetCompareResponse(**payload)


def dataset_download_path(study_dir: str | Path, run_id: str, dataset: str, kind: str) -> Path:
    """Resolve one downloadable dataset artifact while keeping access inside the study workspace."""

    root = _validated_study_root(study_dir)
    target = dataset.strip().upper()
    resolved_kind = kind.strip().lower()
    if resolved_kind == "compare_report":
        compare_dataset_with_reference(root, run_id, target)
    path = _dataset_download_file(root, run_id, target, resolved_kind)
    if path is None or not path.exists() or not path.is_file():
        raise ApiServiceError(f"Download is not available for {target} {resolved_kind}.")
    if not _is_relative_to(path.resolve(), root.resolve()):
        raise ApiServiceError("Download path must stay under the study workspace.")
    if path.stat().st_size > MAX_DOWNLOAD_BYTES:
        raise ApiServiceError(f"Download is too large for the local UI limit: {path.name}")
    return path


def summarize_study_inputs(study_dir: str | Path, *, study_id: str | None = None) -> StudyInputSummary:
    """Return a human-oriented summary of canonical study input folders."""

    root = Path(study_dir).expanduser()
    if not root.exists() or not root.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {root}")
    resolved_study_id = study_id or root.name
    index = StudyInputScanner(root, study_id=resolved_study_id).scan()
    reader = SDTMReader()

    return StudyInputSummary(
        study_id=resolved_study_id,
        study_dir=str(root.resolve().as_posix()),
        sdtm=[
            _preview_table_file(
                Path(artifact.path),
                role="SDTM input",
                dataset=dataset,
                reader=reader,
                sample_rows=3,
            )
            for dataset, artifact in sorted(index.input_sdtm.items())
        ],
        specs=[
            _preview_table_file(
                Path(artifact.path),
                role="Spec",
                dataset=_dataset_from_spec_name(key),
                reader=reader,
                sample_rows=3,
            )
            for key, artifact in sorted(index.input_spec.items())
        ],
        reference_adam=[
            _preview_table_file(
                Path(artifact.path),
                role="Reference ADaM",
                dataset=dataset,
                reader=reader,
                sample_rows=3,
            )
            for dataset, artifact in sorted(index.reference_adam.items())
        ],
        define=[
            _preview_table_file(
                Path(artifact.path),
                role="Define",
                dataset=None,
                reader=reader,
                sample_rows=3,
            )
            for _, artifact in sorted(index.input_define.items())
        ],
        legacy_code=[
            _preview_table_file(
                Path(artifact.path),
                role="Legacy Code",
                dataset=artifact.dataset,
                reader=reader,
                sample_rows=3,
            )
            for _, artifact in sorted(index.legacy_code.items())
        ],
        invalid_files=[
            {"path": item.path, "reason": item.reason}
            for item in index.invalid_files
        ],
        warnings=index.warnings,
    )


def build_run_review_summary(study_dir: str | Path, run_id: str) -> RunReviewSummary:
    """Build a UI-friendly run review bundle from run artifacts."""

    root = Path(study_dir).expanduser()
    if not root.exists() or not root.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {root}")
    run_dir = (root / "runs" / run_id).resolve()
    if not _is_relative_to(run_dir, root.resolve()) or not run_dir.exists():
        raise ApiServiceError(f"Run directory does not exist: {run_dir}")

    manifest = _read_json_if_exists(run_dir / "audit" / "manifest.json")
    study_id = str(manifest.get("study_id") or root.name)
    input_summary = summarize_study_inputs(root, study_id=study_id)
    requested = _string_list(manifest.get("requested_datasets"))
    result_datasets = [
        str(result.get("dataset", "")).strip().upper()
        for result in manifest.get("dataset_results", [])
        if str(result.get("dataset", "")).strip()
    ]
    datasets = _unique_non_empty(result_datasets + requested + _datasets_from_outputs(run_dir))
    dataset_reviews = [_dataset_review(root, run_id, dataset, manifest) for dataset in datasets]
    status = str(manifest.get("status") or _status_from_reviews(dataset_reviews))

    return RunReviewSummary(
        study_id=study_id,
        run_id=run_id,
        run_dir=str(run_dir.as_posix()),
        status=status,
        plain_summary=_plain_run_summary(status, dataset_reviews),
        input_summary=input_summary,
        dataset_reviews=dataset_reviews,
        advanced_artifacts=_advanced_artifacts(run_dir),
    )


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


def _validated_study_root(study_dir: str | Path) -> Path:
    root = Path(study_dir).expanduser()
    if not root.exists() or not root.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {root}")
    return root


def _validated_run_dir(root: Path, run_id: str) -> Path:
    run_dir = (root / "runs" / run_id).resolve()
    if not _is_relative_to(run_dir, root.resolve()) or not run_dir.exists():
        raise ApiServiceError(f"Run directory does not exist: {run_dir}")
    return run_dir


def _resolution_scope_datasets(requested: list[str], approved_dependencies: list[str]) -> list[str]:
    scope: list[str] = []
    for dataset in requested + approved_dependencies:
        normalized = str(dataset).strip().upper()
        if normalized and normalized not in scope:
            scope.append(normalized)
    return scope


def _runnable_datasets(
    target_datasets: list[str],
    requested_set: set[str],
    dependency_resolution: list[dict[str, Any]],
    *,
    dependencies: dict[str, list[str]],
    satisfied_dependency_datasets: list[str],
) -> list[str]:
    satisfied = set(satisfied_dependency_datasets)
    blocked = {
        str(record.get("target_dataset", "")).strip().upper()
        for record in dependency_resolution
        if record.get("resolution_status") in {"user_action_required", "found_but_unusable"}
    }
    runnable: list[str] = []
    for dataset in target_datasets:
        normalized = str(dataset).strip().upper()
        if normalized not in requested_set or normalized in blocked:
            continue
        missing = [
            dependency
            for dependency in dependencies.get(normalized, [])
            if dependency not in satisfied and dependency not in requested_set
        ]
        if not missing and normalized not in runnable:
            runnable.append(normalized)
    return runnable


def _provider_config_from_override(override: Any, *, fallback: LLMProviderConfig) -> LLMProviderConfig:
    if override is None:
        return fallback
    payload = override.model_dump(exclude_none=True) if hasattr(override, "model_dump") else dict(override)
    merged = {
        key: value
        for key, value in fallback.__dict__.items()
        if value is not None
    }
    merged.update(payload)
    return LLMProviderConfig(**merged)


def _exposure_config_from_override(override: Any, *, fallback: LLMExposureConfig) -> LLMExposureConfig:
    if override is None:
        return fallback
    payload = override.model_dump(exclude_none=True) if hasattr(override, "model_dump") else dict(override)
    merged = fallback.model_dump(mode="json")
    merged.update(payload)
    try:
        return LLMExposureConfig.model_validate(merged)
    except ValidationError as exc:
        raise ApiServiceError(str(exc)) from exc


def _plan_review_status(
    *,
    blocked: bool,
    warnings: list[str],
    decisions: list[dict[str, Any]],
) -> str:
    if blocked:
        return "blocked"
    if warnings:
        return "warning"
    if any(bool(decision.get("review_required")) for decision in decisions):
        return "review_required"
    return "accepted"


def _prompt_from_context(context: dict[str, Any]) -> str:
    return compact_prompt_from_context(context)


def _code_generation_system_prompt(target: str) -> str:
    dataset = target.upper()
    return (
        "Return only one valid minified JSON object. Do not use markdown. "
        "Do not explain or reason step by step. Use short, readable base R code. "
        "Top-level keys must be: dataset, r_code, assumptions, risk_points, used_inputs, expected_outputs. "
        f"dataset must be {dataset}. r_code runs from the run working directory and must write outputs/{dataset.lower()}.csv. "
        "Read input data only from the read_path values listed in the prompt; do not search folders or assume an inputs directory. "
        "For CSV input, use colClasses='character' and check.names=FALSE so subject IDs and sequence values are not changed by R type guessing. "
        "The list fields must be arrays of strings."
    )


def _llm_request_for_code_generation(
    *,
    prompt: str,
    target: str,
    study_id: str,
    run_id: str,
    provider_config: LLMProviderConfig,
    exposure: LLMExposureConfig,
    context_dict: dict[str, Any],
    prompt_artifact: ArtifactRef,
) -> LLMRequest:
    return LLMRequest(
        prompt=prompt,
        system_prompt=_code_generation_system_prompt(target),
        provider=provider_config.provider,
        model=provider_config.model,
        exposure=exposure,
        node="generate_downstream_code_for_review",
        call_id=f"llm_{run_id}_{target.lower()}",
        max_tokens=provider_config.max_tokens,
        datasets_included=_datasets_included(context_dict),
        variables_included=_variables_included(context_dict),
        sample_row_counts=_sample_row_counts(context_dict),
        subject_level_data_included=bool(_sample_row_counts(context_dict)),
        prompt_artifact_id=prompt_artifact.artifact_id,
        response_artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{target.lower()}",
        redaction_policy="phase8_code_generation_review_policy",
    )


def _datasets_included(context: dict[str, Any]) -> list[str]:
    datasets = list(context.get("source_dataset_profiles", {}).keys())
    datasets.extend(context.get("resolved_dependencies", {}).keys())
    return datasets


def _variables_included(context: dict[str, Any]) -> list[str]:
    variables: list[str] = []
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        for profile in context.get(section, {}).values():
            for column in profile.get("columns", []):
                if column not in variables:
                    variables.append(column)
    return variables


def _sample_row_counts(context: dict[str, Any]) -> dict[str, int]:
    counts: dict[str, int] = {}
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        for dataset, profile in context.get(section, {}).items():
            sample_rows = profile.get("sample_rows", [])
            if sample_rows:
                counts[dataset] = min(len(sample_rows), MAX_SAMPLE_ROWS_IN_PROMPT)
    return counts


def _default_mock_generated_code_response(target: str) -> str:
    return default_mock_generated_code_response(target)


def _dependency_artifacts_for_dataset(dependency_resolution: list[dict[str, Any]], target: str) -> list[dict[str, Any]]:
    target_dataset = target.strip().upper()
    artifacts: list[dict[str, Any]] = []
    for record in dependency_resolution:
        if str(record.get("target_dataset", "")).strip().upper() != target_dataset:
            continue
        if record.get("resolution_status") != "available":
            continue
        if record.get("artifact_source") == "reference_adam":
            continue
        artifact_path = record.get("artifact_path")
        artifact_sha = record.get("artifact_sha256")
        if not artifact_path or not artifact_sha:
            continue
        artifacts.append(
            {
                "required_dataset": str(record.get("required_dataset", "")).strip().upper(),
                "artifact_path": str(artifact_path),
                "artifact_sha256": str(artifact_sha),
                "artifact_source": record.get("artifact_source"),
            }
        )
    return artifacts


def _approved_dependencies_from_graph_state(study_dir: Path, run_id: str) -> list[str]:
    """Return dependency generation approvals that were persisted by human review."""

    try:
        graph_state = GraphGateway().load_graph_state(study_dir=study_dir, run_id=run_id)
    except FileNotFoundError:
        return []
    approved: list[str] = []
    for command in graph_state.human_commands:
        if command.interrupt != "dependency_review" or command.action != "approve":
            continue
        for item in command.payload.get("approved_dependency_datasets", []):
            dataset = str(item).strip().upper()
            if dataset and dataset not in approved:
                approved.append(dataset)
    return approved


def _approved_draft_spec_payload(study_dir: Path, run_id: str, target: str) -> dict[str, Any] | None:
    approved_path = study_dir / "runs" / run_id / "approved_specs" / f"{target.lower()}_approved_spec.json"
    review_path = study_dir / "runs" / run_id / "reviews" / f"{target.lower()}_draft_spec_review.json"
    if not approved_path.exists() or not approved_path.is_file():
        return None
    review = _read_json_if_exists(review_path)
    if review.get("decision") != "approve" or review.get("approved") is not True:
        return None
    text = _read_text_if_exists(approved_path, limit_chars=500000)
    parsed = _read_json_if_exists(approved_path)
    approved_fingerprint = parsed.get("input_fingerprint") or review.get("input_fingerprint")
    current_fingerprint = input_fingerprint(study_dir)
    if not approved_fingerprint or not approved_fingerprint.get("digest"):
        raise ApiServiceError(
            "Approved draft spec is missing its input fingerprint. "
            "Regenerate and approve the draft spec before generating R code."
        )
    if approved_fingerprint.get("digest") != current_fingerprint.get("digest"):
        diff = compare_fingerprints(approved_fingerprint, current_fingerprint)
        raise ApiServiceError(
            "Approved draft spec is stale because study inputs changed after approval. "
            "Regenerate and approve the draft spec before generating R code. "
            f"Input diff: added={diff.get('added', [])}; removed={diff.get('removed', [])}; changed={diff.get('changed_files', [])}."
        )
    approved_spec_sha = review.get("approved_spec_sha256")
    if not approved_spec_sha:
        raise ApiServiceError(
            "Approved draft spec review is missing the approved spec hash. "
            "Review and approve the draft spec again before generating R code."
        )
    current_spec_sha = f"sha256:{sha256_file(approved_path)}"
    if approved_spec_sha != current_spec_sha:
        raise ApiServiceError("Approved draft spec changed after approval. Review and approve the draft spec again.")
    return {
        "artifact_id": f"approved_draft_spec_{target.lower()}",
        "path": str(approved_path.as_posix()),
        "format": "json",
        "sha256": f"sha256:{sha256_file(approved_path)}",
        "text": text,
        "json": parsed,
        "draft": True,
        "approved": True,
        "source": "user_approved_draft_spec",
        "review_path": str(review_path.as_posix()),
    }


def _safe_upload_name(filename: str) -> str:
    name = Path(filename).name.strip()
    if not name or name in {".", ".."}:
        raise ApiServiceError("Uploaded file name is empty or unsafe.")
    if any(char in name for char in ["/", "\\", ":"]):
        raise ApiServiceError(f"Uploaded file name is unsafe: {filename}")
    return name


def _read_csv_page(
    path: Path,
    *,
    dataset: str,
    kind: str,
    page: int,
    page_size: int,
) -> TablePageResponse:
    resolved_page = max(1, page)
    resolved_size = max(1, min(page_size, MAX_TABLE_PAGE_SIZE))
    start = (resolved_page - 1) * resolved_size
    stop = start + resolved_size
    try:
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.DictReader(handle)
            columns = list(reader.fieldnames or [])
            rows: list[dict[str, str]] = []
            row_count = 0
            for row in reader:
                if start <= row_count < stop:
                    rows.append({column: _string_cell(row.get(column)) for column in columns})
                row_count += 1
    except (OSError, csv.Error, UnicodeDecodeError) as exc:
        return TablePageResponse(
            dataset=dataset,
            kind=kind,
            file_name=path.name,
            format="csv",
            status="error",
            page=resolved_page,
            page_size=resolved_size,
            note=str(exc),
        )
    total_pages = (row_count + resolved_size - 1) // resolved_size if row_count else 0
    return TablePageResponse(
        dataset=dataset,
        kind=kind,
        file_name=path.name,
        format="csv",
        status="ok",
        row_count=row_count,
        columns=columns,
        page=resolved_page,
        page_size=resolved_size,
        total_pages=total_pages,
        rows=rows,
    )


def _dataset_file_for_kind(root: Path, run_id: str, dataset: str, kind: str) -> Path | None:
    if kind == "generated":
        return _usable_generated_output_path(root / "runs" / run_id, dataset)
    if kind == "reference":
        return _reference_path(root, dataset)
    raise ApiServiceError("Table kind must be generated or reference.")


def _usable_generated_output_path(run_dir: Path, dataset: str) -> Path | None:
    target = dataset.strip().upper()
    output_path = run_dir / "outputs" / f"{target.lower()}.csv"
    if not output_path.exists() or not output_path.is_file():
        return None
    report = _read_json_if_exists(run_dir / "validation" / f"{target.lower()}_validation_report.json")
    if report.get("status") not in {"pass", "structural_stub_pass"}:
        return None
    if report.get("terminal_failure") is True or report.get("partial_output_usable") is False:
        return None
    return output_path


def _dataset_download_file(root: Path, run_id: str, dataset: str, kind: str) -> Path | None:
    run_dir = root / "runs" / run_id
    if kind == "generated":
        return _dataset_file_for_kind(root, run_id, dataset, "generated")
    if kind == "reference":
        return _reference_path(root, dataset)
    if kind == "code":
        path = run_dir / "code" / f"build_{dataset.lower()}.R"
        return path if path.exists() else None
    if kind == "validation_report":
        path = run_dir / "validation" / f"{dataset.lower()}_validation_report.json"
        return path if path.exists() else None
    if kind == "compare_report":
        path = run_dir / "compare" / f"{dataset.lower()}_compare_report.json"
        return path if path.exists() else None
    raise ApiServiceError("Download kind must be generated, reference, code, validation_report, or compare_report.")


def _preview_table_file(
    path: Path,
    *,
    role: str,
    dataset: str | None,
    reader: SDTMReader,
    sample_rows: int,
) -> FilePreview:
    suffix = path.suffix.lower().lstrip(".")
    if not path.exists():
        return FilePreview(
            role=role,
            dataset=dataset,
            file_name=path.name,
            path=str(path.as_posix()),
            format=suffix,
            status="missing",
            note="File does not exist.",
        )
    if path.suffix.lower() == ".csv":
        try:
            profile = reader.profile(path, dataset=dataset or path.stem.upper(), sample_rows=sample_rows)
            return FilePreview(
                role=role,
                dataset=profile.dataset,
                file_name=path.name,
                path=str(path.as_posix()),
                format=profile.format,
                status=profile.status,
                row_count=profile.row_count,
                columns=profile.columns,
                sample_rows=profile.sample_rows,
                note=profile.message,
            )
        except (OSError, csv.Error, UnicodeDecodeError) as exc:
            return FilePreview(
                role=role,
                dataset=dataset,
                file_name=path.name,
                path=str(path.as_posix()),
                format=suffix,
                status="error",
                note=str(exc),
            )
    if path.suffix.lower() == ".sas7bdat":
        r_preview = _preview_sas7bdat_with_r(path, role=role, dataset=dataset or path.stem.upper(), sample_rows=sample_rows)
        if r_preview is not None:
            return r_preview
        return FilePreview(
            role=role,
            dataset=dataset or path.stem.upper(),
            file_name=path.name,
            path=str(path.as_posix()),
            format="sas7bdat",
            status="not_previewed",
            note="sas7bdat is supported by the R runtime path. Browser preview requires Rscript and the R package haven.",
        )
    if path.suffix.lower() in {".sas", ".r", ".txt", ".xml", ".json", ".yaml", ".yml", ".md"}:
        return _preview_text_file(path, role=role, dataset=dataset, suffix=suffix)
    return FilePreview(
        role=role,
        dataset=dataset,
        file_name=path.name,
        path=str(path.as_posix()),
        format=suffix,
        status="not_previewed",
        preview_type="file",
        note="This file type is recognized, but no browser preview is implemented yet.",
    )


def _preview_sas7bdat_with_r(path: Path, *, role: str, dataset: str, sample_rows: int) -> FilePreview | None:
    rscript_path = str(DEFAULT_LOCAL_RSCRIPT) if DEFAULT_LOCAL_RSCRIPT.exists() else None
    runner = LocalRRunner(rscript_path=rscript_path)
    if not runner.rscript_path:
        return None
    work_dir = path.parent / ".adam_preview"
    r_code = f'''
if (!requireNamespace("haven", quietly = TRUE)) {{
  stop("Reading sas7bdat requires the R package 'haven'.")
}}
data <- haven::read_sas({_r_string(str(path))})
sample <- utils::head(data, {max(0, sample_rows)})
sample[] <- lapply(sample, as.character)
cat("__COLUMNS__\\n")
cat(paste(names(data), collapse = "\\t"), "\\n", sep = "")
cat("__ROW_COUNT__\\n")
cat(nrow(data), "\\n", sep = "")
cat("__SAMPLE_CSV__\\n")
utils::write.csv(sample, stdout(), row.names = FALSE, na = "")
'''
    result = runner.run(
        RRunRequest(
            code=r_code,
            dataset=dataset,
            run_id="api_preview",
            working_dir=str(work_dir),
            script_path=str(work_dir / f"preview_{dataset.lower()}.R"),
            timeout_seconds=60,
        )
    )
    if not result.success:
        return FilePreview(
            role=role,
            dataset=dataset,
            file_name=path.name,
            path=str(path.as_posix()),
            format="sas7bdat",
            status="error",
            note="R haven preview failed: " + (result.stderr.strip() or result.stdout.strip() or "unknown error"),
        )
    try:
        columns_text = _between_markers(result.stdout, "__COLUMNS__", "__ROW_COUNT__").strip()
        row_count_text = _between_markers(result.stdout, "__ROW_COUNT__", "__SAMPLE_CSV__").strip()
        sample_csv = result.stdout.split("__SAMPLE_CSV__", 1)[1].strip()
    except IndexError:
        return FilePreview(
            role=role,
            dataset=dataset,
            file_name=path.name,
            path=str(path.as_posix()),
            format="sas7bdat",
            status="error",
            note="R haven preview returned unreadable output.",
        )
    columns = columns_text.split("\t") if columns_text else []
    try:
        row_count = int(row_count_text)
    except ValueError:
        row_count = None
    rows: list[dict[str, str]] = []
    if sample_csv:
        try:
            reader = csv.DictReader(io.StringIO(sample_csv))
            rows = [{column: _string_cell(row.get(column)) for column in columns} for row in reader]
        except csv.Error:
            rows = []
    return FilePreview(
        role=role,
        dataset=dataset,
        file_name=path.name,
        path=str(path.as_posix()),
        format="sas7bdat",
        status="ok",
        row_count=row_count,
        columns=columns,
        sample_rows=rows,
        note="Previewed through local R haven.",
    )


def _preview_text_file(path: Path, *, role: str, dataset: str | None, suffix: str) -> FilePreview:
    try:
        text = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        try:
            text = path.read_text(encoding="latin-1")
        except OSError as exc:
            return _text_preview_error(path, role=role, dataset=dataset, suffix=suffix, message=str(exc))
    except OSError as exc:
        return _text_preview_error(path, role=role, dataset=dataset, suffix=suffix, message=str(exc))

    lines = text.splitlines()
    preview = "\n".join(lines[:80])
    detected_targets = _detect_adam_tokens(f"{path.stem}\n{text}")
    dependency_text = "\n".join(line for line in lines if _looks_like_dependency_line(line))
    detected_dependencies = [token for token in _detect_adam_tokens(dependency_text) if token != (dataset or "").upper()]
    detected_dataset = dataset or _dataset_from_name_or_text(path.stem, text)
    return FilePreview(
        role=role,
        dataset=detected_dataset,
        file_name=path.name,
        path=str(path.as_posix()),
        format=suffix,
        status="ok",
        preview_type="code" if suffix in {"sas", "r"} else "text",
        text_preview=preview,
        line_count=len(lines),
        detected_targets=detected_targets,
        detected_dependencies=detected_dependencies,
        note="Text/code preview is limited to the first 80 lines.",
    )


def _looks_like_dependency_line(line: str) -> bool:
    lower = f" {line.lower()} "
    return any(keyword in lower for keyword in [" merge ", " set ", " join ", " from "])


def _detect_adam_tokens(text: str) -> list[str]:
    tokens: list[str] = []
    for match in re.finditer(r"\bAD[A-Z0-9_]{1,}\b", text.upper()):
        token = match.group(0).strip("_")
        if len(token) > 2 and token not in tokens:
            tokens.append(token)
    for match in re.finditer(r"\bADS[_\-\s]+(AD[A-Z0-9]{1,})(?:[_\-\s]+FULL)?\b", text.upper()):
        token = match.group(1)
        if token not in tokens:
            tokens.append(token)
    return tokens


def _dataset_from_name_or_text(name: str, text: str) -> str | None:
    tokens = _detect_adam_tokens(name)
    if tokens:
        return tokens[0]
    sas_data = re.search(r"\bdata\s+([A-Za-z_][A-Za-z0-9_]*\.)?(AD[A-Za-z0-9_]{1,})\b", text, flags=re.IGNORECASE)
    if sas_data:
        return sas_data.group(2).upper()
    tokens = _detect_adam_tokens(text)
    return tokens[0] if tokens else None


def _r_string(value: str) -> str:
    return '"' + value.replace("\\", "/").replace('"', '\\"') + '"'


def _between_markers(text: str, start: str, end: str) -> str:
    return text.split(start, 1)[1].split(end, 1)[0]


def _text_preview_error(path: Path, *, role: str, dataset: str | None, suffix: str, message: str) -> FilePreview:
    return FilePreview(
        role=role,
        dataset=dataset,
        file_name=path.name,
        path=str(path.as_posix()),
        format=suffix,
        status="error",
        preview_type="text",
        note=message,
    )


def _compare_dataset_files(dataset: str, generated_path: Path | None, reference_path: Path | None) -> DatasetCompareResponse:
    if generated_path is None or not generated_path.exists():
        return DatasetCompareResponse(dataset=dataset, status="missing_generated", note="Generated ADaM output is not available yet.")
    if reference_path is None or not reference_path.exists():
        return DatasetCompareResponse(
            dataset=dataset,
            status="missing_reference",
            generated_file=generated_path.name,
            note="No reference ADaM was found for this dataset.",
        )
    if generated_path.suffix.lower() != ".csv" or reference_path.suffix.lower() != ".csv":
        return DatasetCompareResponse(
            dataset=dataset,
            status="not_supported",
            generated_file=generated_path.name,
            reference_file=reference_path.name,
            note="Compare is currently implemented for CSV generated/reference ADaM tables.",
        )
    generated = _read_csv_table(generated_path)
    reference = _read_csv_table(reference_path)
    if generated["status"] != "ok":
        return DatasetCompareResponse(dataset=dataset, status="error", generated_file=generated_path.name, reference_file=reference_path.name, note=generated["note"])
    if reference["status"] != "ok":
        return DatasetCompareResponse(dataset=dataset, status="error", generated_file=generated_path.name, reference_file=reference_path.name, note=reference["note"])

    generated_columns = generated["columns"]
    reference_columns = reference["columns"]
    common_columns = [column for column in generated_columns if column in reference_columns]
    generated_only_columns = [column for column in generated_columns if column not in reference_columns]
    reference_only_columns = [column for column in reference_columns if column not in generated_columns]
    key_columns = _choose_compare_keys(dataset, common_columns)
    generated_rows = generated["rows"]
    reference_rows = reference["rows"]
    row_count_generated = len(generated_rows)
    row_count_reference = len(reference_rows)
    generated_only_keys: list[str] = []
    reference_only_keys: list[str] = []
    mismatch_samples: list[dict[str, str]] = []
    compared_cells = 0
    matched_rows = 0
    mismatch_count = 0

    if key_columns:
        generated_by_key = _rows_by_key(generated_rows, key_columns)
        reference_by_key = _rows_by_key(reference_rows, key_columns)
        generated_key_set = set(generated_by_key)
        reference_key_set = set(reference_by_key)
        generated_only_keys = sorted(generated_key_set - reference_key_set)[:20]
        reference_only_keys = sorted(reference_key_set - generated_key_set)[:20]
        for key in sorted(generated_key_set & reference_key_set):
            matched_rows += 1
            generated_row = generated_by_key[key]
            reference_row = reference_by_key[key]
            for column in common_columns:
                if column in key_columns:
                    continue
                compared_cells += 1
                generated_value = _string_cell(generated_row.get(column))
                reference_value = _string_cell(reference_row.get(column))
                if generated_value != reference_value:
                    mismatch_count += 1
                    if len(mismatch_samples) < 25:
                        mismatch_samples.append(
                            {
                                "key": key,
                                "column": column,
                                "generated": generated_value,
                                "reference": reference_value,
                            }
                        )
    else:
        for index, (generated_row, reference_row) in enumerate(zip(generated_rows, reference_rows), start=1):
            matched_rows += 1
            for column in common_columns:
                compared_cells += 1
                generated_value = _string_cell(generated_row.get(column))
                reference_value = _string_cell(reference_row.get(column))
                if generated_value != reference_value:
                    mismatch_count += 1
                    if len(mismatch_samples) < 25:
                        mismatch_samples.append(
                            {
                                "key": f"row {index}",
                                "column": column,
                                "generated": generated_value,
                                "reference": reference_value,
                            }
                        )

    status = "match"
    if (
        row_count_generated != row_count_reference
        or generated_only_columns
        or reference_only_columns
        or generated_only_keys
        or reference_only_keys
        or mismatch_count
    ):
        status = "differences"
    return DatasetCompareResponse(
        dataset=dataset,
        status=status,
        generated_file=generated_path.name,
        reference_file=reference_path.name,
        row_count_generated=row_count_generated,
        row_count_reference=row_count_reference,
        row_count_delta=row_count_generated - row_count_reference,
        generated_only_columns=generated_only_columns,
        reference_only_columns=reference_only_columns,
        common_columns=common_columns,
        key_columns=key_columns,
        matched_rows=matched_rows,
        generated_only_keys=generated_only_keys,
        reference_only_keys=reference_only_keys,
        compared_cells=compared_cells,
        mismatch_count=mismatch_count,
        mismatch_samples=mismatch_samples,
        note="Initial CSV compare. This checks structure and sampled cell differences, not full clinical rule conformance.",
    )


def _read_csv_table(path: Path) -> dict[str, Any]:
    try:
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            text = handle.read()
    except (OSError, UnicodeDecodeError) as exc:
        return {"status": "error", "note": str(exc), "columns": [], "rows": []}
    try:
        reader = csv.DictReader(io.StringIO(text))
        columns = list(reader.fieldnames or [])
        rows = [{column: _string_cell(row.get(column)) for column in columns} for row in reader]
    except csv.Error as exc:
        return {"status": "error", "note": str(exc), "columns": [], "rows": []}
    return {"status": "ok", "note": "", "columns": columns, "rows": rows}


def _choose_compare_keys(dataset: str, common_columns: list[str]) -> list[str]:
    upper_to_original = {column.upper(): column for column in common_columns}
    candidates = {
        "ADAE": ["USUBJID", "AESEQ"],
        "ADCM": ["USUBJID", "CMSEQ"],
        "ADLB": ["USUBJID", "PARAMCD", "AVISITN", "ADT", "ATPTN"],
        "ADEX": ["USUBJID", "EXSEQ"],
        "ADEG": ["USUBJID", "PARAMCD", "AVISITN", "ADT", "ATPTN"],
        "ADSL": ["USUBJID"],
    }
    chosen = [upper_to_original[key] for key in candidates.get(dataset.upper(), ["USUBJID"]) if key in upper_to_original]
    if chosen:
        return chosen
    if "USUBJID" in upper_to_original:
        return [upper_to_original["USUBJID"]]
    return []


def _rows_by_key(rows: list[dict[str, str]], key_columns: list[str]) -> dict[str, dict[str, str]]:
    keyed: dict[str, dict[str, str]] = {}
    for index, row in enumerate(rows, start=1):
        key = "|".join(_string_cell(row.get(column)) for column in key_columns)
        if not key.strip("|"):
            key = f"row {index}"
        if key in keyed:
            key = f"{key}#{index}"
        keyed[key] = row
    return keyed


def _string_cell(value: Any) -> str:
    if value is None:
        return ""
    return str(value)


def _dataset_review(root: Path, run_id: str, dataset: str, manifest: dict[str, Any]) -> DatasetReview:
    dataset_lower = dataset.lower()
    run_dir = root / "runs" / run_id
    result = _dataset_result_from_manifest(manifest, dataset)
    validation_report = _read_json_if_exists(run_dir / "validation" / f"{dataset_lower}_validation_report.json")
    diagnostics = _read_json_if_exists(run_dir / "diagnostics" / f"{dataset_lower}_failure_report.json")
    parsed_response = _read_json_if_exists(run_dir / "llm" / f"{dataset_lower}_parsed_response.json")
    code_path = run_dir / "code" / f"build_{dataset_lower}.R"
    output_path = _usable_generated_output_path(run_dir, dataset)
    reference_path = _reference_path(root, dataset)
    reader = SDTMReader()
    compare_summary = _compare_dataset_files(dataset, output_path, reference_path)
    if compare_summary.status not in {"missing_generated", "missing_reference"}:
        compare_summary = _record_compare_in_graph_state(root, run_id, dataset, compare_summary)
    else:
        compare_summary = _record_compare_in_graph_state(root, run_id, dataset, compare_summary)
    compare_status = compare_summary.status if compare_summary.status != "missing_generated" else result.get("compare_status")

    return DatasetReview(
        dataset=dataset,
        status=str(result.get("status") or _dataset_status_from_report(validation_report, output_path)),
        validation_status=result.get("validation_status") or validation_report.get("status"),
        compare_status=compare_status,
        output_path=str(output_path.as_posix()) if output_path else None,
        output_preview=_preview_table_file(output_path, role="Generated ADaM", dataset=dataset, reader=reader, sample_rows=5)
        if output_path
        else None,
        reference_preview=_preview_table_file(reference_path, role="Reference ADaM", dataset=dataset, reader=reader, sample_rows=5)
        if reference_path is not None
        else None,
        compare_summary=compare_summary,
        downloads=_dataset_downloads(root, run_id, dataset),
        generated_code_path=str(code_path.as_posix()) if code_path.exists() else None,
        generated_code=_read_text_if_exists(code_path, limit_chars=40000),
        assumptions=_string_list(parsed_response.get("assumptions")),
        risk_points=_string_list(parsed_response.get("risk_points")),
        warnings=_string_list(validation_report.get("warnings")),
        errors=_string_list(validation_report.get("errors")),
        validation_report=validation_report,
        diagnostics=diagnostics,
    )


def _dataset_result_from_manifest(manifest: dict[str, Any], dataset: str) -> dict[str, Any]:
    target = dataset.upper()
    for result in manifest.get("dataset_results", []):
        if str(result.get("dataset", "")).strip().upper() == target:
            return result
    return {}


def _dataset_status_from_report(report: dict[str, Any], output_path: Path | None) -> str:
    status = report.get("status")
    if status == "pass":
        return "completed"
    if status == "structural_stub_pass":
        return "completed_stub"
    if status:
        return "failed"
    return "completed" if output_path is not None else "unknown"


def _dataset_downloads(root: Path, run_id: str, dataset: str) -> list[DownloadItem]:
    specs = [
        ("generated", "Generated ADaM"),
        ("reference", "Reference ADaM"),
        ("code", "Generated R Code"),
        ("validation_report", "Validation Report"),
        ("compare_report", "Compare Report"),
    ]
    items: list[DownloadItem] = []
    for kind, label in specs:
        path = _dataset_download_file(root, run_id, dataset, kind)
        available = path is not None and path.exists() and path.is_file()
        items.append(
            DownloadItem(
                kind=kind,
                label=label,
                available=available,
                file_name=path.name if available and path is not None else None,
                note="" if available else "Not available yet.",
            )
        )
    return items


def _reference_path(root: Path, dataset: str) -> Path | None:
    folder = root / "reference_adam"
    for suffix in [".csv", ".sas7bdat"]:
        for name in [dataset.lower(), dataset.upper()]:
            path = folder / f"{name}{suffix}"
            if path.exists():
                return path
    return None


def _read_json_if_exists(path: Path) -> dict[str, Any]:
    if not path.exists() or not path.is_file():
        return {}
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, UnicodeDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")


def _read_text_if_exists(path: Path, *, limit_chars: int) -> str:
    if not path.exists() or not path.is_file():
        return ""
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return ""
    if len(text) <= limit_chars:
        return text
    return text[:limit_chars] + "\n\n# [truncated for UI preview]"


def _string_list(value: Any) -> list[str]:
    if not isinstance(value, list):
        return []
    return [str(item) for item in value if str(item).strip()]


def _unique_non_empty(values: list[str]) -> list[str]:
    result: list[str] = []
    for value in values:
        normalized = str(value).strip().upper()
        if normalized and normalized not in result:
            result.append(normalized)
    return result


def _datasets_from_outputs(run_dir: Path) -> list[str]:
    output_dir = run_dir / "outputs"
    if not output_dir.exists():
        return []
    return [path.stem.upper() for path in sorted(output_dir.glob("*.csv"))]


def _advanced_artifacts(run_dir: Path) -> dict[str, str]:
    candidates = {
        "dependency_plan": run_dir / "planning" / "dependency_plan.json",
        "dependency_review": run_dir / "planning" / "dependency_review.md",
        "audit_manifest": run_dir / "audit" / "manifest.json",
    }
    for path in sorted((run_dir / "llm").glob("*_context.json")) if (run_dir / "llm").exists() else []:
        candidates[f"llm_context_{path.stem.removesuffix('_context')}"] = path
    for path in sorted((run_dir / "llm").glob("*_compact_prompt.txt")) if (run_dir / "llm").exists() else []:
        candidates[f"llm_prompt_{path.stem.removesuffix('_compact_prompt')}"] = path
    for path in sorted((run_dir / "validation").glob("*_validation_report.json")) if (run_dir / "validation").exists() else []:
        candidates[f"validation_{path.stem.removesuffix('_validation_report')}"] = path
    for path in sorted((run_dir / "diagnostics").glob("*_failure_report.json")) if (run_dir / "diagnostics").exists() else []:
        candidates[f"diagnostics_{path.stem.removesuffix('_failure_report')}"] = path
    return {key: str(path.as_posix()) for key, path in candidates.items() if path.exists()}


def _status_from_reviews(reviews: list[DatasetReview]) -> str:
    if any(review.status == "failed" for review in reviews):
        return "failed"
    if reviews:
        return "completed"
    return "unknown"


def _plain_run_summary(status: str, reviews: list[DatasetReview]) -> str:
    if not reviews:
        return f"Run status is {status}. No dataset output was found yet."
    completed = sum(1 for review in reviews if review.status in {"completed", "completed_stub"})
    failed = sum(1 for review in reviews if review.status == "failed")
    stubbed = sum(1 for review in reviews if review.status == "completed_stub")
    parts = [f"Run status is {status}.", f"{completed} dataset(s) completed"]
    if failed:
        parts.append(f"{failed} failed")
    if stubbed:
        parts.append(f"{stubbed} used structural stub mode")
    return " ".join(parts) + "."


def _dataset_from_spec_name(key: str) -> str | None:
    normalized = key.lower()
    for prefix in ["ads_", "spec_", "adam_"]:
        if normalized.startswith(prefix):
            normalized = normalized[len(prefix):]
    for suffix in ["_full", "_spec", "_approved_spec", "_input_spec"]:
        if normalized.endswith(suffix):
            normalized = normalized[: -len(suffix)]
    dataset = normalized.upper()
    return dataset if dataset else None


def _copy_required(source: Path, destination: Path, created_files: list[str]) -> None:
    if not source.exists() or not source.is_file():
        raise ApiServiceError(f"Required demo file does not exist: {source}")
    _copy_file(source, destination, created_files)


def _copy_first_required(
    sources: list[Path],
    destination: Path,
    created_files: list[str],
    *,
    logical_name: str,
) -> None:
    for source in sources:
        if source.exists() and source.is_file():
            _copy_file(source, destination, created_files)
            return
    candidates = ", ".join(str(source) for source in sources)
    raise ApiServiceError(f"Required demo file does not exist for {logical_name}: {candidates}")


def _copy_file(source: Path, destination: Path, created_files: list[str]) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(source, destination)
    created_files.append(str(destination.as_posix()))


def _clear_demo_input_folders(target: Path) -> None:
    managed_files = [
        target / "input_sdtm" / "ae.csv",
        target / "input_sdtm" / "dm.csv",
        target / "input_sdtm" / "ex.csv",
        target / "input_spec" / "adae.csv",
        target / "input_spec" / "adsl.csv",
        target / "input_spec" / "ads_adae_full.csv",
        target / "input_spec" / "ads_adsl_full.csv",
        target / "reference_adam" / "adsl.csv",
        target / "reference_adam" / "adae.csv",
        target / "legacy_code" / "adae.sas",
        target / "legacy_code" / "addm.sas",
    ]
    for path in managed_files:
        try:
            if path.exists() and path.is_file():
                path.unlink()
        except OSError:
            continue

