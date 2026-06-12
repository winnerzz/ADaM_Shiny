"""Service helpers behind the FastAPI routes."""

from __future__ import annotations

import csv
import io
import json
import os
import re
import shutil
import subprocess
import tempfile
from collections.abc import Iterator
from contextlib import contextmanager
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
    GraphCommandResponse,
    LLMConnectionTestResponse,
    NativeDatasetFullRunStartResponse,
    NativeDatasetFullRunResumeResponse,
    NativeDatasetResumeResponse,
    NativeStudyDatasetStartResult,
    NativeStudyStartResponse,
    ProductWorkspaceResponse,
    RuntimeReadinessCheck,
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
from adam_agent.graph.checkpointing import default_sqlite_checkpointer_path
from adam_agent.graph.execution_modes import (
    LEGACY_RUN_BLOCKED_LLM_MODES,
    LEGACY_RUN_ENDPOINT_MODES,
    LLM_DOWNSTREAM_PROVIDER_MODE,
    LLM_DOWNSTREAM_R_SANDBOX_MODE,
    format_execution_modes,
)
from adam_agent.graph.gateway import GraphGateway
from adam_agent.graph.output_quality import dataset_output_quality
from adam_agent.graph.workflow_state import (
    compare_fingerprints,
    input_fingerprint,
)
from adam_agent.schemas.graph_state import DatasetRunState, StudyRunState
from adam_agent.llm.clients import (
    LLMClientConfigError,
    LLMProviderConfig,
    LLMProviderResponseError,
    LLMRequest,
    build_llm_client,
)
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
from adam_agent.tools.compare import TableReader, compare_dataset_files, reference_adam_path, usable_generated_output_path
from adam_agent.tools.config import ConfigLoader
from adam_agent.tools.r_runner import LocalRRunner, RRunRequest
from adam_agent.tools.sdtm_reader import SDTMReader
from adam_agent.tools.study_inputs import StudyInputScanner


class ApiServiceError(RuntimeError):
    """Raised when an API request cannot be fulfilled safely."""


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_DEMO_SOURCE_DIR = ROOT.parent / "ADaM_Shiny-ADaM_Shiny_experimental" / "demo-data"
DEFAULT_DEMO_STUDY_ROOT = ROOT / ".tmp_tests" / "ui_demo_study"
DEFAULT_PRODUCT_STUDY_ROOT = Path(
    os.environ.get("LOCALAPPDATA")
    or os.environ.get("APPDATA")
    or (Path.home() / ".adam_agent_studio")
) / "ADaMAgentStudio" / "studies"
DEFAULT_DEMO_CONFIG_PATH = ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"
DEFAULT_LOCAL_RSCRIPT_ENV = "ADAM_AGENT_RSCRIPT_PATH"
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
_GRAPH_STATE_UNSET = object()
SERVICE_CHECKPOINTER_BACKEND_ENV = "ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND"
DEFAULT_SERVICE_CHECKPOINTER_BACKEND = "sqlite"
DEMO_STUDY_ROOT_ENV = "ADAM_AGENT_DEMO_STUDY_ROOT"
PRODUCT_STUDY_ROOT_ENV = "ADAM_AGENT_PRODUCT_STUDY_ROOT"


def _default_rscript_path() -> str | None:
    """Resolve Rscript without baking one developer machine path into the product."""

    configured = os.environ.get(DEFAULT_LOCAL_RSCRIPT_ENV, "").strip()
    if configured:
        path = Path(configured).expanduser()
        return str(path) if path.exists() else configured
    return shutil.which("Rscript")


def _request_rscript_path(request: Any) -> str:
    """Resolve the per-request Rscript path, falling back to the service default."""

    requested = str(getattr(request, "rscript_path", "") or "").strip()
    if requested:
        return requested
    return _default_rscript_path() or ""


def _default_demo_study_root() -> Path:
    return Path(os.environ.get(DEMO_STUDY_ROOT_ENV, DEFAULT_DEMO_STUDY_ROOT)).expanduser()


def _default_product_study_root() -> Path:
    return Path(os.environ.get(PRODUCT_STUDY_ROOT_ENV, DEFAULT_PRODUCT_STUDY_ROOT)).expanduser()


# Keep service-layer GraphGateway construction centralized here. Endpoint
# helpers should call this factory instead of instantiating GraphGateway.
def _new_graph_gateway(
    *,
    study_dir: str | Path | None = None,
    run_id: str | None = None,
) -> GraphGateway:
    """Construct the service-owned workflow gateway in one place.

    FastAPI/service code should not choose checkpointer details per endpoint.
    Future persistence backends belong behind this boundary.
    """

    backend = os.environ.get(SERVICE_CHECKPOINTER_BACKEND_ENV, DEFAULT_SERVICE_CHECKPOINTER_BACKEND).strip().lower()
    if backend in {"", "default", "local", "durable"}:
        backend = DEFAULT_SERVICE_CHECKPOINTER_BACKEND
    if backend == "memory":
        return GraphGateway()
    if backend == "sqlite":
        if study_dir is None or not run_id:
            return GraphGateway()
        sqlite_path = default_sqlite_checkpointer_path(study_dir, run_id)
        try:
            return GraphGateway(checkpointer_backend="sqlite", sqlite_checkpointer_path=sqlite_path)
        except ValueError as exc:
            raise ApiServiceError(str(exc)) from exc
    if backend == "postgres":
        try:
            return GraphGateway(checkpointer_backend="postgres")
        except ValueError as exc:
            raise ApiServiceError(str(exc)) from exc
    raise ApiServiceError(
        f"Unsupported service GraphGateway checkpointer backend: {backend}. "
        "Use memory, sqlite, or postgres."
    )


@contextmanager
def _open_graph_gateway(
    *,
    study_dir: str | Path | None = None,
    run_id: str | None = None,
) -> Iterator[GraphGateway]:
    """Open a service-scoped graph gateway and always release its resources."""

    gateway = _new_graph_gateway(study_dir=study_dir, run_id=run_id)
    try:
        yield gateway
    finally:
        gateway.close()


def _gateway_compatibility_metadata(result: Any) -> dict[str, str]:
    """Copy compatibility metadata from the gateway-owned workflow projection."""

    projection = getattr(result, "workflow_projection", None)
    if not isinstance(projection, dict):
        raise ApiServiceError("GraphGateway did not return a workflow projection.")
    metadata = {
        "workflow_control": projection.get("workflow_control"),
        "graph_state_path": projection.get("graph_state_path"),
        "workflow_state_path": projection.get("workflow_state_path"),
    }
    missing = [key for key, value in metadata.items() if not value]
    if missing:
        raise ApiServiceError(
            "GraphGateway workflow projection is missing compatibility metadata: "
            + ", ".join(missing)
        )
    return {key: str(value) for key, value in metadata.items()}


def _gateway_projection_paths(result: Any) -> dict[str, str]:
    """Copy canonical/projection paths from the gateway-owned workflow projection."""

    metadata = _gateway_compatibility_metadata(result)
    return {
        "graph_state_path": metadata["graph_state_path"],
        "workflow_state_path": metadata["workflow_state_path"],
    }


def _gateway_legacy_metadata(result: Any) -> dict[str, str | None]:
    """Copy legacy `/runs` metadata from the gateway-owned workflow projection."""

    projection = getattr(result, "workflow_projection", None)
    if not isinstance(projection, dict):
        raise ApiServiceError("GraphGateway did not return a legacy workflow projection.")
    workflow_control = projection.get("workflow_control")
    workflow_state_path = projection.get("workflow_state_path")
    if not workflow_control or not workflow_state_path:
        raise ApiServiceError("GraphGateway legacy workflow projection is missing compatibility metadata.")
    graph_state_path = projection.get("graph_state_path")
    return {
        "workflow_control": str(workflow_control),
        "graph_state_path": str(graph_state_path) if graph_state_path else None,
        "workflow_state_path": str(workflow_state_path),
    }


def ensure_study_workspace(request: StudyWorkspaceRequest) -> StudyInputSummary:
    """Create or open a local study workspace and return its input summary."""

    root = Path(request.study_dir).expanduser()
    root.mkdir(parents=True, exist_ok=True)
    for folder in STUDY_INPUT_FOLDERS:
        (root / folder).mkdir(parents=True, exist_ok=True)
    return summarize_study_inputs(root, study_id=request.study_id or root.name)


def build_runtime_readiness() -> RuntimeReadinessResponse:
    """Report environment capabilities without reading study data or calling an external LLM."""

    checks: list[RuntimeReadinessCheck] = []
    next_actions: list[str] = []
    capabilities: dict[str, bool] = {}

    workspace_root = _default_product_study_root()
    workspace_ok, workspace_details = _check_workspace_root(workspace_root)
    capabilities["managed_workspace"] = workspace_ok
    checks.append(
        RuntimeReadinessCheck(
            name="managed_workspace",
            status="ready" if workspace_ok else "needs_setup",
            user_message=(
                "Study workspace storage is ready."
                if workspace_ok
                else "Study workspace storage is not writable."
            ),
            details=workspace_details,
        )
    )
    if not workspace_ok:
        next_actions.append("Check that the application data folder or configured study volume is writable.")

    rscript_path = _default_rscript_path()
    r_ok, r_details = _check_rscript(rscript_path)
    capabilities["r_execution"] = r_ok
    checks.append(
        RuntimeReadinessCheck(
            name="r_execution",
            status="ready" if r_ok else "needs_setup",
            user_message=(
                "R execution is available."
                if r_ok
                else "R execution is not available; generated code can be reviewed but not run here."
            ),
            details=r_details,
        )
    )
    if not r_ok:
        next_actions.append("Install R in the runtime image or set ADAM_AGENT_RSCRIPT_PATH for this service.")

    haven_ok, haven_details = _check_r_package(rscript_path, "haven") if r_ok else (False, {"skipped": "Rscript is unavailable."})
    capabilities["sas7bdat_preview"] = bool(r_ok and haven_ok)
    checks.append(
        RuntimeReadinessCheck(
            name="sas7bdat_preview",
            status="ready" if r_ok and haven_ok else "needs_setup",
            user_message=(
                "SAS7BDAT preview and comparison support is available."
                if r_ok and haven_ok
                else "SAS7BDAT files can be uploaded, but preview/compare needs R with the haven package."
            ),
            details=haven_details,
        )
    )
    if r_ok and not haven_ok:
        next_actions.append("Install the R package haven in the runtime image or local R library.")

    config = ConfigLoader().load(None, study_id="READINESS", run_id="runtime_readiness")
    llm_is_mock = config.llm_provider.provider.strip().lower() == "mock"
    capabilities["offline_mock_llm"] = llm_is_mock
    capabilities["external_llm_configured"] = not llm_is_mock
    checks.append(
        RuntimeReadinessCheck(
            name="llm_default",
            status="ready" if llm_is_mock else "needs_review",
            user_message=(
                "Default LLM mode is offline mock. No external model call is made until Real LLM is selected."
                if llm_is_mock
                else "A real LLM provider is configured. Test the connection before generation."
            ),
            details={
                "provider": config.llm_provider.provider,
                "model": config.llm_provider.model,
                "external_connection_tested": False,
            },
        )
    )

    hard_blocked = not workspace_ok
    degraded = not hard_blocked and (not r_ok or not haven_ok)
    status = "blocked" if hard_blocked else "degraded" if degraded else "ready"
    user_status = "Needs setup" if hard_blocked else "Limited" if degraded else "Ready"
    if status == "ready":
        message = "Environment is ready for upload, review, R execution, and SAS7BDAT preview."
    elif status == "degraded":
        message = "Environment can run the app, but one or more runtime capabilities are limited."
    else:
        message = "Environment needs setup before study files can be managed safely."

    return RuntimeReadinessResponse(
        status=status,
        user_status=user_status,
        user_message=message,
        capabilities=capabilities,
        checks=checks,
        next_actions=next_actions,
        diagnostics={
            "workspace_root": str(workspace_root),
            "rscript_resolution": "env_or_path",
            "rscript_path": rscript_path,
            "config_mode": "server_default",
            "llm_connection_test": "manual_only",
        },
    )


def _check_workspace_root(root: Path) -> tuple[bool, dict[str, Any]]:
    try:
        root.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(prefix=".readiness_", suffix=".tmp", dir=root, delete=True) as handle:
            handle.write(b"ok")
            handle.flush()
        return True, {"path": str(root), "writable": True}
    except OSError as exc:
        return False, {"path": str(root), "writable": False, "error": str(exc)}


def _check_rscript(rscript_path: str | None) -> tuple[bool, dict[str, Any]]:
    if not rscript_path:
        return False, {"resolved_path": None, "error": "Rscript was not found on PATH or ADAM_AGENT_RSCRIPT_PATH."}
    try:
        completed = subprocess.run(
            [rscript_path, "--version"],
            capture_output=True,
            text=True,
            timeout=15,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        return False, {"resolved_path": rscript_path, "error": str(exc)}
    output = (completed.stdout or completed.stderr or "").strip()
    return completed.returncode == 0, {
        "resolved_path": rscript_path,
        "exit_code": completed.returncode,
        "version": output,
    }


def _check_r_package(rscript_path: str | None, package: str) -> tuple[bool, dict[str, Any]]:
    if not rscript_path:
        return False, {"package": package, "error": "Rscript is unavailable."}
    expression = f"if (!requireNamespace('{package}', quietly = TRUE)) quit(status = 12)"
    try:
        completed = subprocess.run(
            [rscript_path, "-e", expression],
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired) as exc:
        return False, {"package": package, "error": str(exc)}
    return completed.returncode == 0, {
        "package": package,
        "installed": completed.returncode == 0,
        "exit_code": completed.returncode,
        "stderr": (completed.stderr or "").strip(),
    }


def create_default_product_workspace() -> ProductWorkspaceResponse:
    """Create a default local workspace without making the user choose a path."""

    stamp = datetime.now().strftime("%Y%m%d%H%M%S%f")
    study_id = f"study_{stamp[:14]}"
    root = _default_product_study_root() / study_id
    summary = ensure_study_workspace(StudyWorkspaceRequest(study_dir=str(root), study_id=study_id))
    return ProductWorkspaceResponse(
        study_id=study_id,
        study_dir=str(root.resolve().as_posix()),
        run_id=f"run_{stamp[:14]}",
        target_datasets=[],
        config_path=str(DEFAULT_DEMO_CONFIG_PATH.as_posix()),
        rscript_path=None,
        workspace_mode="managed",
        config_mode="server_default",
        rscript_mode="path_lookup",
        input_summary=summary,
        notes=[
            "A local workspace was created automatically for this browser session.",
            "The default config and Rscript lookup are managed by the backend. Advanced path overrides are optional.",
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
    with _open_graph_gateway() as gateway:
        graph_invalidation = gateway.mark_study_inputs_changed(study_dir=root)
    upload_state = {
        "input_fingerprint": graph_invalidation.input_fingerprint,
        "input_diff": graph_invalidation.input_diff,
        "touched_runs": graph_invalidation.touched_runs,
        "touched_graph_runs": graph_invalidation.touched_graph_runs,
        "skipped_graph_runs": graph_invalidation.skipped_graph_runs,
    }
    return normalized_role, folder_name, saved, summary, upload_state


def delete_study_input_file(
    *,
    study_dir: str | Path,
    role: str,
    file_name: str,
    study_id: str | None = None,
) -> tuple[str, str, str, StudyInputSummary, dict[str, Any]]:
    """Remove one uploaded study input file and invalidate stale graph state."""

    normalized_role = role.strip().lower()
    folder_name = UPLOAD_ROLE_TO_FOLDER.get(normalized_role)
    if folder_name is None:
        allowed = ", ".join(sorted(UPLOAD_ROLE_TO_FOLDER))
        raise ApiServiceError(f"Unsupported input role: {role}. Allowed roles: {allowed}")
    safe_name = _safe_upload_name(file_name)
    if safe_name != file_name:
        raise ApiServiceError("Input file deletion only accepts a plain file name from the study input list.")
    root = Path(study_dir).expanduser()
    if not root.exists() or not root.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {root}")
    folder = (root / folder_name).resolve()
    resolved_root = root.resolve()
    if not _is_relative_to(folder, resolved_root):
        raise ApiServiceError("Input folder resolved outside the study workspace.")
    target = (folder / safe_name).resolve()
    if not _is_relative_to(target, folder):
        raise ApiServiceError("Input file resolved outside its canonical study input folder.")
    if not target.exists() or not target.is_file():
        raise ApiServiceError(f"Input file does not exist: {safe_name}")
    try:
        target.unlink()
    except OSError as exc:
        raise ApiServiceError(f"Could not delete input file {safe_name}: {exc}") from exc

    summary = summarize_study_inputs(root, study_id=study_id or root.name)
    with _open_graph_gateway() as gateway:
        graph_invalidation = gateway.mark_study_inputs_changed(study_dir=root)
    upload_state = {
        "input_fingerprint": graph_invalidation.input_fingerprint,
        "input_diff": graph_invalidation.input_diff,
        "touched_runs": graph_invalidation.touched_runs,
        "touched_graph_runs": graph_invalidation.touched_graph_runs,
        "skipped_graph_runs": graph_invalidation.skipped_graph_runs,
    }
    return normalized_role, folder_name, str(target.as_posix()), summary, upload_state


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
    target = Path(study_dir).expanduser() if study_dir else _default_demo_study_root() / f"demo_adam_{stamp}"
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
        target_datasets=["ADSL", "ADAE"],
        config_path=str(DEFAULT_DEMO_CONFIG_PATH.as_posix()),
        execution_mode=LLM_DOWNSTREAM_R_SANDBOX_MODE if _default_rscript_path() else LLM_DOWNSTREAM_PROVIDER_MODE,
        rscript_path=_default_rscript_path(),
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
        execution_mode = LLM_DOWNSTREAM_PROVIDER_MODE
    if execution_mode is None:
        raise ApiServiceError(
            "POST /runs requires an explicit execution_mode. "
            "Use execution_mode='stub' only for the legacy compatibility/test path, "
            "or start the graph product path with /runs/prepare or /runs/native-study-loop."
        )
    if execution_mode not in LEGACY_RUN_ENDPOINT_MODES:
        raise ApiServiceError(
            f"Unsupported execution_mode for POST /runs: {execution_mode}. "
            f"Allowed legacy endpoint modes: {format_execution_modes(LEGACY_RUN_ENDPOINT_MODES)}. "
            "Use /runs/prepare or /runs/native-study-loop for product LLM generation."
        )
    with _open_graph_gateway(study_dir=study_dir, run_id=config.run_id) as gateway:
        if execution_mode in LEGACY_RUN_BLOCKED_LLM_MODES:
            gateway.block_legacy_run_to_completion(
                study_dir=study_dir,
                run_id=config.run_id,
                study_id=study_id,
                requested_datasets=list(request.target_datasets),
                execution_mode=execution_mode,
            )
            raise ApiServiceError(
                "LLM ADaM generation cannot run through POST /runs because it would bypass review gates. "
                "Use /runs/prepare or /runs/native-study-loop, record human review through /graph-command, "
                "and execute LG3/native-full-run datasets through native-full-run/execute. "
                "Dataset split-flow endpoints remain compatibility/manual transition endpoints."
            )

        legacy_result = gateway.run_legacy_to_completion(
            study_dir=study_dir,
            study_id=config.study_id,
            run_id=config.run_id,
            target_datasets=list(request.target_datasets),
            execution_mode=execution_mode,
            approved_dependency_datasets=request.approved_dependency_datasets,
            rscript_path=_request_rscript_path(request),
            llm_exposure=config.llm_exposure.model_dump(mode="json"),
            llm_provider={
                key: value
                for key, value in config.llm_provider.__dict__.items()
                if value is not None
            },
        )
    return _response_from_legacy_graph_result(legacy_result, execution_mode=execution_mode, study_dir=study_dir)


def prepare_run_plan(request: RunPlanRequest) -> RunPlanResponse:
    """Plan dataset dependencies without generating code or running R."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    study_id = request.study_id or study_dir.name
    with _open_graph_gateway(study_dir=study_dir, run_id=request.run_id) as gateway:
        gateway_result = gateway.start_dependency_plan(
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
        dependency_warning_records=list(plan_payload.get("dependency_planning_warning_records", [])),
        **_gateway_projection_paths(gateway_result),
    )


def start_native_study_product_loop(request: Any) -> NativeStudyStartResponse:
    """Start runnable study datasets and stop at graph-owned review gates."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    targets = [
        str(dataset).strip().upper()
        for dataset in request.target_datasets
        if str(dataset).strip()
    ]
    if not targets:
        raise ApiServiceError("target_datasets must not be empty.")
    study_id = request.study_id or study_dir.name
    config = ConfigLoader().load(getattr(request, "config_path", None), study_id=study_id, run_id=request.run_id)
    provider_config = _provider_config_from_override(
        getattr(request, "llm_provider_override", None),
        fallback=config.llm_provider,
    )
    exposure = _exposure_config_from_override(
        getattr(request, "llm_exposure_override", None),
        fallback=config.llm_exposure,
    )
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=request.run_id) as gateway:
            result = gateway.start_native_study_product_loop(
                study_dir=study_dir,
                study_id=study_id,
                run_id=request.run_id,
                target_datasets=targets,
                approved_dependency_datasets=getattr(request, "approved_dependency_datasets", []),
                llm_provider=provider_config.__dict__,
                llm_exposure=exposure.model_dump(mode="json"),
                llm_client_builder=build_llm_client,
                target_context_builder=build_target_llm_context,
                rscript_path=_request_rscript_path(request),
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    dataset_results = [
        _native_study_dataset_start_result(dataset, dataset_result)
        for dataset, dataset_result in sorted(result.dataset_results.items())
    ]
    message = _native_study_start_message(
        started=result.started_datasets,
        skipped=result.skipped_datasets,
        blocked=result.blocked_datasets,
        review_queue=result.review_queue,
    )
    return NativeStudyStartResponse(
        study_id=result.graph_state.study_id,
        run_id=request.run_id,
        status=result.graph_state.status,
        started_datasets=list(result.started_datasets),
        skipped_datasets=list(result.skipped_datasets),
        blocked_datasets=list(result.blocked_datasets),
        review_queue=list(result.review_queue),
        native_resume_available=result.native_resume_available,
        native_resume_scope=result.native_resume_scope,
        resume_boundary=result.resume_boundary,
        native_resume_interrupts=list(result.native_resume_interrupts),
        native_resume_has_queue_items=result.native_resume_has_queue_items,
        native_resume_queue_item_count=result.native_resume_queue_item_count,
        dataset_results=dataset_results,
        message=message,
        **_gateway_compatibility_metadata(result),
    )


def start_native_dataset_full_run(run_id: str, dataset: str, request: Any) -> NativeDatasetFullRunStartResponse:
    """Start one dataset through the LG3 native full-run backend contract."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    if not target:
        raise ApiServiceError("dataset must not be empty.")
    study_id = request.study_id or study_dir.name
    config = ConfigLoader().load(getattr(request, "config_path", None), study_id=study_id, run_id=run_id)
    provider_config = _provider_config_from_override(
        getattr(request, "llm_provider_override", None),
        fallback=config.llm_provider,
    )
    exposure = _exposure_config_from_override(
        getattr(request, "llm_exposure_override", None),
        fallback=config.llm_exposure,
    )
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.start_native_dataset_full_run(
                study_dir=study_dir,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                llm_provider=provider_config.__dict__,
                llm_exposure=exposure.model_dump(mode="json"),
                llm_client_builder=build_llm_client,
                target_context_builder=build_target_llm_context,
                rscript_path=_request_rscript_path(request),
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    dataset_state = result.graph_state.datasets.get(target)
    current_interrupt = (
        dataset_state.current_interrupt.model_dump(mode="json")
        if dataset_state is not None and dataset_state.current_interrupt is not None
        else None
    )
    code_state = dataset_state.code_state if dataset_state is not None else {}
    spec_state = dataset_state.spec_state if dataset_state is not None else {}
    static_artifact = code_state.get("static_check_artifact") if isinstance(code_state, dict) else None
    next_action = str(current_interrupt.get("name") if isinstance(current_interrupt, dict) else "")
    code_path = str(code_state.get("code_path") or "") if isinstance(code_state, dict) else ""
    draft_spec_path = str(spec_state.get("draft_spec_path") or "") if isinstance(spec_state, dict) else ""
    static_check_path = str(code_state.get("static_check_path") or "") if isinstance(code_state, dict) else ""
    if not static_check_path and isinstance(static_artifact, dict):
        static_check_path = str(static_artifact.get("path") or "")
    return NativeDatasetFullRunStartResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        dataset=target,
        phase=result.phase,
        status=dataset_state.status if dataset_state is not None else result.graph_state.status,
        current_interrupt=current_interrupt,
        next_action=next_action,
        code_path=code_path or None,
        draft_spec_path=draft_spec_path or None,
        static_check_path=static_check_path or None,
        **_gateway_compatibility_metadata(result),
    )


def resume_native_dataset_interrupt(run_id: str, dataset: str, request: Any) -> NativeDatasetResumeResponse:
    """Resume a durable graph-native dataset interrupt when the runtime supports it."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            if not gateway.native_interrupt_resume_available():
                raise ApiServiceError(
                    "Native LangGraph interrupt resume is not enabled for this run. "
                    "Use the split-flow review endpoints, or enable a durable LangGraph checkpointer."
                )
            config = ConfigLoader().load(getattr(request, "config_path", None), study_id=study_dir.name, run_id=run_id)
            provider_config = _provider_config_from_override(
                getattr(request, "llm_provider_override", None),
                fallback=config.llm_provider,
            )
            exposure = _exposure_config_from_override(
                getattr(request, "llm_exposure_override", None),
                fallback=config.llm_exposure,
            )
            result = gateway.resume_native_dataset_interrupt(
                study_dir=study_dir,
                run_id=run_id,
                dataset=target,
                decision=request.decision,
                reviewer=request.reviewer,
                notes=request.notes,
                execute_after_approval=bool(getattr(request, "execute_after_approval", False)),
                llm_provider=provider_config.__dict__,
                llm_exposure=exposure.model_dump(mode="json"),
                llm_client_builder=build_llm_client,
                target_context_builder=build_target_llm_context,
                rscript_path=_request_rscript_path(request),
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    dataset_state = result.graph_state.datasets.get(target)
    next_action = ""
    if dataset_state is not None and dataset_state.current_interrupt is not None:
        next_action = dataset_state.current_interrupt.name
    executed = bool(getattr(result, "execution", None))
    return NativeDatasetResumeResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        dataset=target,
        interrupt=result.interrupt,
        decision=result.decision,
        status=dataset_state.status if dataset_state is not None else result.graph_state.status,
        current_interrupt=dataset_state.current_interrupt.model_dump(mode="json")
        if dataset_state is not None and dataset_state.current_interrupt is not None
        else None,
        executed=executed,
        next_action=next_action,
        **_gateway_projection_paths(result),
    )


def resume_native_dataset_full_run(run_id: str, dataset: str, request: Any) -> NativeDatasetFullRunResumeResponse:
    """Resume an LG3 full-run review gate without enabling durable native resume."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    decision = request.decision.strip().lower()
    if decision not in {"approve", "reject"}:
        raise ApiServiceError("LG3 full-run review decision must be approve or reject.")
    provider_config: Any | None = None
    exposure: Any | None = None
    if decision == "approve":
        config = ConfigLoader().load(getattr(request, "config_path", None), study_id=study_dir.name, run_id=run_id)
        provider_config = _provider_config_from_override(
            getattr(request, "llm_provider_override", None),
            fallback=config.llm_provider,
        )
        exposure = _exposure_config_from_override(
            getattr(request, "llm_exposure_override", None),
            fallback=config.llm_exposure,
        )
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.resume_native_dataset_full_run(
                study_dir=study_dir,
                run_id=run_id,
                dataset=target,
                decision=decision,
                reviewer=request.reviewer,
                notes=request.notes,
                execute_after_approval=bool(getattr(request, "execute_after_approval", False)),
                llm_provider=provider_config.__dict__ if provider_config is not None else None,
                llm_exposure=exposure.model_dump(mode="json") if exposure is not None else None,
                llm_client_builder=build_llm_client if provider_config is not None else None,
                target_context_builder=build_target_llm_context if provider_config is not None else None,
                rscript_path=_request_rscript_path(request),
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    dataset_state = result.graph_state.datasets.get(target)
    current_interrupt = (
        dataset_state.current_interrupt.model_dump(mode="json")
        if dataset_state is not None and dataset_state.current_interrupt is not None
        else None
    )
    next_action = str(current_interrupt.get("name") if isinstance(current_interrupt, dict) else "")
    execution = result.execution
    return NativeDatasetFullRunResumeResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        dataset=target,
        phase=result.phase,
        last_interrupt=str(
            (result.graph_state.runtime_persistence.get("native_dataset_full_run") or {}).get("last_interrupt") or ""
        )
        or None,
        current_interrupt=current_interrupt,
        decision=result.decision,
        approved=result.approved,
        executed=execution is not None,
        terminal_failure=bool(execution.terminal_failure) if execution is not None else False,
        next_action=next_action,
        **_gateway_projection_paths(result),
    )


def persist_dependency_review(run_id: str, request: Any) -> DependencyReviewResponse:
    """Compatibility path for old dependency-review clients.

    New browser/product flows submit human decisions through
    ``submit_graph_command()`` so all review gates share one command contract.
    """

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    decision = request.decision.strip().lower()
    if decision not in {"approve", "reject"}:
        raise ApiServiceError("Dependency review decision must be approve or reject.")
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.review_dependency(
                study_dir=study_dir,
                run_id=run_id,
                decision=decision,
                reviewer=request.reviewer,
                notes=request.notes,
                approved_dependency_datasets=[
                    str(item).strip().upper()
                    for item in getattr(request, "approved_dependency_datasets", [])
                    if str(item).strip()
                ],
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    return DependencyReviewResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        decision=result.decision,
        approved=result.approved,
        current_interrupt=result.current_interrupt,
        **_gateway_projection_paths(result),
    )


def submit_graph_command(run_id: str, request: Any) -> GraphCommandResponse:
    """Submit one human action to the graph without selecting a split-flow endpoint."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    payload = getattr(request, "payload", {}) or {}
    approved_dependencies = [
        str(item).strip().upper()
        for item in payload.get("approved_dependency_datasets", [])
        if str(item).strip()
    ] if isinstance(payload, dict) else []
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.submit_graph_command(
                study_dir=study_dir,
                run_id=run_id,
                dataset=getattr(request, "dataset", None),
                interrupt=getattr(request, "interrupt", None),
                action=request.action,
                reviewer=request.reviewer,
                notes=request.notes,
                approved_dependency_datasets=approved_dependencies,
                execute_after_approval=False,
                llm_provider=None,
                llm_exposure=None,
                llm_client_builder=None,
                target_context_builder=None,
                rscript_path="",
            )
    except (FileNotFoundError, ValueError) as exc:
        raise ApiServiceError(str(exc)) from exc
    return GraphCommandResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        scope=result.scope,
        dataset=result.dataset,
        interrupt=result.interrupt,
        action=result.action,
        status=result.status,
        next_action=result.next_action,
        current_interrupt=result.current_interrupt,
        available_actions=result.available_actions,
        review_artifact_path=result.review_artifact_path,
        approved=result.approved,
        executed=result.executed,
        terminal_failure=result.terminal_failure,
        **_gateway_compatibility_metadata(result),
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
    config = ConfigLoader().load(request.config_path, study_id=study_id, run_id=run_id)
    provider_config = _provider_config_from_override(request.llm_provider_override, fallback=config.llm_provider)
    exposure = _exposure_config_from_override(request.llm_exposure_override, fallback=config.llm_exposure)
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.generate_code(
                study_dir=study_dir,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                llm_provider=provider_config.__dict__,
                llm_exposure=exposure.model_dump(mode="json"),
                llm_client_builder=build_llm_client,
                target_context_builder=build_target_llm_context,
                rscript_path=_request_rscript_path(request),
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    warnings = result.warnings + list(result.dependency_warnings or []) + [
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
        dependency_review_status=result.dependency_review_status,
        warnings=warnings,
        **_gateway_compatibility_metadata(result),
    )


def finalize_dataset_inputs(run_id: str, dataset: str, request: Any) -> FinalizeInputsResponse:
    """Confirm uploads are complete and generate a reviewable draft spec only if needed."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    config = ConfigLoader().load(request.config_path, study_id=study_id, run_id=run_id)
    provider_config = _provider_config_from_override(request.llm_provider_override, fallback=config.llm_provider)
    exposure = _exposure_config_from_override(request.llm_exposure_override, fallback=config.llm_exposure)
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.finalize_inputs(
                study_dir=study_dir,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                llm_provider=provider_config.__dict__,
                llm_exposure=exposure.model_dump(mode="json"),
                llm_client_builder=build_llm_client,
                target_context_builder=build_target_llm_context,
                rscript_path=_request_rscript_path(request),
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    warnings = result.warnings + list(result.dependency_warnings or [])
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
            **_gateway_compatibility_metadata(result),
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
            **_gateway_compatibility_metadata(result),
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
        **_gateway_compatibility_metadata(result),
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
        **_gateway_compatibility_metadata(result),
    )


def generate_dataset_draft_spec(run_id: str, dataset: str, request: Any) -> DraftSpecResponse:
    """Generate a draft spec and return it for human review before code generation."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    config = ConfigLoader().load(request.config_path, study_id=study_id, run_id=run_id)
    provider_config = _provider_config_from_override(request.llm_provider_override, fallback=config.llm_provider)
    exposure = _exposure_config_from_override(request.llm_exposure_override, fallback=config.llm_exposure)
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.generate_draft_spec(
                study_dir=study_dir,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                llm_provider=provider_config.__dict__,
                llm_exposure=exposure.model_dump(mode="json"),
                llm_client_builder=build_llm_client,
                target_context_builder=build_target_llm_context,
                rscript_path=_request_rscript_path(request),
            )
    except ValueError as exc:
        raise ApiServiceError(f"Draft spec generation failed: {exc}") from exc
    warnings = result.warnings + list(result.dependency_warnings or [])
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
        **_gateway_compatibility_metadata(result),
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
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
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
        **_gateway_compatibility_metadata(result),
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
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
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
        **_gateway_compatibility_metadata(result),
    )


def execute_approved_dataset_code(run_id: str, dataset: str, request: Any) -> ExecuteCodeResponse:
    """Execute an already-generated and approved R script."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.execute_approved_code(
                study_dir=study_dir,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                rscript_path=_request_rscript_path(request),
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
        **_gateway_compatibility_metadata(result),
    )


def execute_native_dataset_full_run(run_id: str, dataset: str, request: Any) -> ExecuteCodeResponse:
    """Execute approved R code only for an existing LG3 full-run contract."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    study_id = request.study_id or study_dir.name
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.execute_native_dataset_full_run(
                study_dir=study_dir,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                rscript_path=_request_rscript_path(request),
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
        **_gateway_compatibility_metadata(result),
    )


def persist_terminal_failure_review(run_id: str, dataset: str, request: Any) -> TerminalFailureReviewResponse:
    """Persist human triage for a terminal execution failure."""

    study_dir = Path(request.study_dir).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {study_dir}")
    target = dataset.strip().upper()
    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            result = gateway.review_terminal_failure(
                study_dir=study_dir,
                run_id=run_id,
                dataset=target,
                decision=request.decision,
                reviewer=request.reviewer,
                notes=request.notes,
            )
    except ValueError as exc:
        raise ApiServiceError(str(exc)) from exc
    return TerminalFailureReviewResponse(
        study_id=result.graph_state.study_id,
        run_id=run_id,
        dataset=target,
        decision=result.decision,
        current_interrupt=result.current_interrupt,
        next_action=result.next_action,
        **_gateway_projection_paths(result),
    )


def read_run_json_artifact(study_dir: str | Path, run_id: str, relative_path: str) -> dict[str, Any]:
    """Read a JSON artifact below one run directory."""

    root = Path(study_dir).expanduser()
    run_dir = (root / "runs" / run_id).resolve()
    target_path = _json_artifact_path_for_read(root, run_id, relative_path)
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
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id)
    except FileNotFoundError as exc:
        raise ApiServiceError(str(exc)) from exc
    return state.model_dump(mode="json")


def read_run_progress(study_dir: str | Path, run_id: str) -> RunProgressResponse:
    """Read graph-owned progress guidance for one local run."""

    try:
        with _open_graph_gateway(study_dir=study_dir, run_id=run_id) as gateway:
            payload = gateway.progress_summary(study_dir=study_dir, run_id=run_id)
    except FileNotFoundError as exc:
        raise ApiServiceError(str(exc)) from exc
    return RunProgressResponse(**payload)


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
    if path.suffix.lower() == ".sas7bdat":
        return _read_sas7bdat_page(path, dataset=target, kind=resolved_kind, page=page, page_size=page_size)
    if path.suffix.lower() != ".csv":
        return TablePageResponse(
            dataset=target,
            kind=resolved_kind,
            file_name=path.name,
            format=path.suffix.lower().lstrip("."),
            status="not_previewed",
            note="Full table browsing is currently available for CSV and sas7bdat ADaM tables.",
        )
    return _read_csv_page(path, dataset=target, kind=resolved_kind, page=page, page_size=page_size)


def compare_dataset_with_reference(study_dir: str | Path, run_id: str, dataset: str) -> DatasetCompareResponse:
    """Compare generated ADaM output with a reference ADaM table when both are available."""

    root = _validated_study_root(study_dir)
    target = dataset.strip().upper()
    run_dir = _validated_run_dir(root, run_id)
    try:
        with _open_graph_gateway(study_dir=root, run_id=run_id) as gateway:
            result = gateway.compare_reference_output(
                study_dir=root,
                run_id=run_id,
                dataset=target,
                table_reader=_sas7bdat_table_reader(),
            )
    except FileNotFoundError:
        output_path = usable_generated_output_path(run_dir, target)
        reference_path = reference_adam_path(root, target)
        return DatasetCompareResponse(
            **compare_dataset_files(target, output_path, reference_path, table_reader=_sas7bdat_table_reader())
        )
    except (ValueError, ValidationError) as exc:
        graph_state_path = run_dir / "graph_state.json"
        if graph_state_path.exists():
            raise ApiServiceError(
                f"Canonical graph state for run {run_id} exists but cannot be read. "
                "Compare will not fall back to artifact-only mode."
            ) from exc
        raise ApiServiceError(str(exc)) from exc
    payload = {
        key: value
        for key, value in result.compare_summary.items()
        if key in DatasetCompareResponse.model_fields
    }
    return DatasetCompareResponse(**payload)


def dataset_download_path(study_dir: str | Path, run_id: str, dataset: str, kind: str) -> Path:
    """Resolve one downloadable dataset artifact while keeping access inside the study workspace."""

    root = _validated_study_root(study_dir)
    target = dataset.strip().upper()
    resolved_kind = kind.strip().lower()
    if resolved_kind == "compare_report":
        graph_state = _load_read_model_graph_state(
            root,
            run_id,
            fail_on_existing=True,
            fallback_message="Dataset download will not fall back to artifact-only mode.",
        )
        if graph_state is None or _graph_dataset_state(graph_state, target) is not None:
            compare_dataset_with_reference(root, run_id, target)
        path = _dataset_download_file(root, run_id, target, resolved_kind)
    else:
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


def build_run_review_summary(study_dir: str | Path, run_id: str, *, detail_level: str = "full") -> RunReviewSummary:
    """Build a UI-friendly run review bundle from run artifacts."""

    root = Path(study_dir).expanduser()
    if not root.exists() or not root.is_dir():
        raise ApiServiceError(f"study_dir does not exist or is not a directory: {root}")
    run_dir = (root / "runs" / run_id).resolve()
    if not _is_relative_to(run_dir, root.resolve()) or not run_dir.exists():
        raise ApiServiceError(f"Run directory does not exist: {run_dir}")

    manifest = _read_json_if_exists(run_dir / "audit" / "manifest.json")
    study_id = str(manifest.get("study_id") or root.name)
    resolved_detail_level = _review_summary_detail_level(detail_level)
    input_summary = _minimal_study_input_summary(root, study_id=study_id, detail_level=resolved_detail_level)
    requested = _string_list(manifest.get("requested_datasets"))
    result_datasets = [
        str(result.get("dataset", "")).strip().upper()
        for result in manifest.get("dataset_results", [])
        if str(result.get("dataset", "")).strip()
    ]
    graph_state = _load_read_model_graph_state(root, run_id, fail_on_existing=True)
    workflow_state = _read_json_if_exists(run_dir / "workflow_state.json") if graph_state is None else {}
    datasets = (
        _review_datasets_from_graph_state(graph_state)
        if graph_state is not None
        else _unique_non_empty(result_datasets + requested + _datasets_from_outputs(run_dir))
    )
    dataset_reviews = [
        _dataset_review(
            root,
            run_id,
            dataset,
            manifest,
            workflow_state,
            graph_state,
            detail_level=resolved_detail_level,
        )
        for dataset in datasets
    ]
    status = str(
        (graph_state.status if graph_state is not None else "")
        or manifest.get("status")
        or workflow_state.get("status")
        or _status_from_reviews(dataset_reviews)
    )
    read_model_source = (
        "graph_state"
        if graph_state is not None
        else "workflow_state_fallback"
        if workflow_state
        else "artifact_fallback"
    )
    graph_state_path = run_dir / "graph_state.json"
    workflow_state_path = run_dir / "workflow_state.json"

    return RunReviewSummary(
        study_id=graph_state.study_id if graph_state is not None else study_id,
        run_id=run_id,
        run_dir=str(run_dir.as_posix()),
        status=status,
        detail_level=resolved_detail_level,
        read_model_source=read_model_source,
        graph_state_path=str(graph_state_path.as_posix()) if graph_state is not None else None,
        workflow_state_path=str(workflow_state_path.as_posix()) if graph_state is None and workflow_state_path.exists() else None,
        plain_summary=_plain_run_summary(status, dataset_reviews),
        input_summary=input_summary,
        dataset_reviews=dataset_reviews,
        advanced_artifacts=_advanced_artifacts(run_dir, graph_state=graph_state),
    )


def _review_summary_detail_level(value: str) -> str:
    normalized = str(value or "").strip().lower()
    if normalized in {"summary", "light", "lightweight", "fast"}:
        return "summary"
    if normalized in {"full", "detail", "detailed"}:
        return "full"
    raise ApiServiceError("review-summary detail_level must be summary or full.")


def _minimal_study_input_summary(root: Path, *, study_id: str, detail_level: str) -> StudyInputSummary:
    note = (
        "Input file previews are owned by the input evidence panel and are not recomputed by run review."
        if detail_level == "full"
        else "Input file previews are omitted in summary mode. Open the input evidence panel to rescan previews."
    )
    return StudyInputSummary(
        study_id=study_id,
        study_dir=str(root.resolve().as_posix()),
        warnings=[note],
    )


def _response_from_legacy_graph_result(
    legacy_result: Any,
    *,
    execution_mode: str,
    study_dir: Path,
) -> RunStudyResponse:
    result = getattr(legacy_result, "graph_result", None)
    if not isinstance(result, dict):
        raise ApiServiceError("GraphGateway did not return a legacy graph result.")
    audit_manifest = result.get("audit_manifest")
    run_dir = study_dir / "runs" / result["run_id"]
    return RunStudyResponse(
        study_id=result["study_id"],
        run_id=result["run_id"],
        status=result["status"],
        execution_mode=execution_mode,
        **_gateway_legacy_metadata(legacy_result),
        requested_datasets=result.get("requested_datasets", []),
        target_datasets=result.get("target_datasets", []),
        runnable_datasets=result.get("runnable_datasets", []),
        blocked_datasets=result.get("blocked_datasets", []),
        dependency_review_status=result.get("dependency_review_status"),
        run_dir=str(run_dir.as_posix()),
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


def _native_study_dataset_start_result(dataset: str, result: Any) -> NativeStudyDatasetStartResult:
    target = dataset.strip().upper()
    if hasattr(result, "phase") and hasattr(result, "current_interrupt"):
        dataset_state = getattr(result.graph_state, "datasets", {}).get(target)
        spec_state = dataset_state.spec_state if dataset_state is not None else {}
        code_state = dataset_state.code_state if dataset_state is not None else {}
        code_path = _none_if_blank(str(code_state.get("code_path") or "") if isinstance(code_state, dict) else None)
        draft_spec_path = _none_if_blank(
            str(spec_state.get("draft_spec_path") or "") if isinstance(spec_state, dict) else None
        )
        current_interrupt = (
            dataset_state.current_interrupt.name
            if dataset_state is not None and dataset_state.current_interrupt is not None
            else getattr(result, "current_interrupt", None)
        )
        warnings = list(getattr(result, "warnings", []) or []) + list(getattr(result, "dependency_warnings", []) or [])
        if current_interrupt == "code_review":
            static_artifact = code_state.get("static_check_artifact") if isinstance(code_state, dict) else None
            static_check_path = _none_if_blank(
                str(code_state.get("static_check_path") or "") if isinstance(code_state, dict) else None
            )
            if not static_check_path and isinstance(static_artifact, dict):
                static_check_path = _none_if_blank(str(static_artifact.get("path") or ""))
            return NativeStudyDatasetStartResult(
                dataset=target,
                status="code_review_required",
                next_action="review_code",
                result_type="code_review",
                code_path=code_path,
                draft_spec_path=draft_spec_path,
                static_check_path=static_check_path,
                warnings=warnings,
            )
        if current_interrupt == "draft_spec_review":
            return NativeStudyDatasetStartResult(
                dataset=target,
                status="draft_spec_review_required",
                next_action="review_draft_spec",
                result_type="draft_spec_review",
                draft_spec_path=draft_spec_path,
                warnings=warnings,
            )
        return NativeStudyDatasetStartResult(
            dataset=target,
            status=dataset_state.status if dataset_state is not None else str(getattr(result, "phase", "") or "started"),
            next_action=str(current_interrupt or ""),
            result_type="native_full_run",
            warnings=warnings,
        )
    if hasattr(result, "code_path"):
        warnings = list(getattr(result, "warnings", []) or []) + list(getattr(result, "dependency_warnings", []) or [])
        return NativeStudyDatasetStartResult(
            dataset=target,
            status="code_review_required",
            next_action="review_code",
            result_type="code_review",
            code_path=getattr(result, "code_path", None),
            draft_spec_path=getattr(result, "draft_spec_path", None),
            static_check_path=getattr(result, "static_check_path", None),
            warnings=warnings,
        )
    warnings = list(getattr(result, "warnings", []) or []) + list(getattr(result, "dependency_warnings", []) or [])
    if getattr(result, "spec_source", "") == "input_spec":
        status = "input_spec_ready"
        next_action = "generate_code"
        result_type = "input_spec_ready"
    elif getattr(result, "spec_source", "") == "approved_draft_spec":
        status = "approved_draft_spec_ready"
        next_action = "generate_code"
        result_type = "approved_draft_spec_ready"
    else:
        status = "draft_spec_review_required"
        next_action = "review_draft_spec"
        result_type = "draft_spec_review"
    return NativeStudyDatasetStartResult(
        dataset=target,
        status=status,
        next_action=next_action,
        result_type=result_type,
        draft_spec_path=getattr(result, "draft_spec_path", None),
        warnings=warnings,
    )


def _none_if_blank(value: str | None) -> str | None:
    if value is None:
        return None
    stripped = value.strip()
    return stripped or None


def _native_study_start_message(
    *,
    started: list[str],
    skipped: list[dict[str, Any]],
    blocked: list[dict[str, Any]],
    review_queue: list[dict[str, Any]],
) -> str:
    if started:
        review_count = len(review_queue)
        return (
            f"Started {', '.join(started)} and stopped at human review gates. "
            f"{review_count} review item(s) are now queued."
        )
    if blocked:
        return "No dataset was started because dependency review or user action is still required."
    if skipped:
        datasets = [str(item.get("dataset") or "").strip().upper() for item in skipped]
        datasets = [dataset for dataset in datasets if dataset]
        return f"No new dataset was started; existing graph progress was preserved for {', '.join(datasets)}."
    return "No new dataset was started; existing graph progress was preserved."


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


def _read_sas7bdat_page(
    path: Path,
    *,
    dataset: str,
    kind: str,
    page: int,
    page_size: int,
) -> TablePageResponse:
    resolved_page = max(1, page)
    resolved_size = max(1, min(page_size, MAX_TABLE_PAGE_SIZE))
    table = _read_sas7bdat_table(path)
    if table["status"] != "ok":
        return TablePageResponse(
            dataset=dataset,
            kind=kind,
            file_name=path.name,
            format="sas7bdat",
            status=table["status"],
            page=resolved_page,
            page_size=resolved_size,
            note=table["note"],
        )
    rows = table["rows"]
    row_count = len(rows)
    start = (resolved_page - 1) * resolved_size
    stop = start + resolved_size
    total_pages = (row_count + resolved_size - 1) // resolved_size if row_count else 0
    return TablePageResponse(
        dataset=dataset,
        kind=kind,
        file_name=path.name,
        format="sas7bdat",
        status="ok",
        row_count=row_count,
        columns=table["columns"],
        page=resolved_page,
        page_size=resolved_size,
        total_pages=total_pages,
        rows=rows[start:stop],
        note="Read through local R haven.",
    )


def _sas7bdat_table_reader() -> TableReader:
    return _read_sas7bdat_table


def _read_sas7bdat_table(path: Path) -> dict[str, Any]:
    rscript_path = _default_rscript_path()
    runner = LocalRRunner(rscript_path=rscript_path)
    if not runner.rscript_path:
        return {
            "status": "not_supported",
            "note": "sas7bdat compare requires Rscript with the R package 'haven'.",
            "columns": [],
            "rows": [],
        }
    dataset = path.stem.upper()
    r_code = f'''
if (!requireNamespace("haven", quietly = TRUE)) {{
  stop("Reading sas7bdat requires the R package 'haven'.")
}}
data <- haven::read_sas({_r_string(str(path))})
data <- as.data.frame(data, stringsAsFactors = FALSE)
data[] <- lapply(data, as.character)
cat("__TABLE_CSV__\\n")
utils::write.csv(data, stdout(), row.names = FALSE, na = "")
'''
    with tempfile.TemporaryDirectory(prefix="adam_agent_compare_") as work_dir_text:
        work_dir = Path(work_dir_text)
        result = runner.run(
            RRunRequest(
                code=r_code,
                dataset=dataset,
                run_id="api_compare",
                working_dir=str(work_dir),
                script_path=str(work_dir / f"read_{dataset.lower()}.R"),
                timeout_seconds=180,
            )
        )
    if not result.success:
        return {
            "status": "error",
            "note": "R haven table read failed: " + (result.stderr.strip() or result.stdout.strip() or "unknown error"),
            "columns": [],
            "rows": [],
        }
    try:
        table_csv = result.stdout.split("__TABLE_CSV__", 1)[1].strip()
    except IndexError:
        return {
            "status": "error",
            "note": "R haven table read returned unreadable output.",
            "columns": [],
            "rows": [],
        }
    try:
        reader = csv.DictReader(io.StringIO(table_csv))
        columns = list(reader.fieldnames or [])
        rows = [{column: _string_cell(row.get(column)) for column in columns} for row in reader]
    except csv.Error as exc:
        return {"status": "error", "note": str(exc), "columns": [], "rows": []}
    return {"status": "ok", "note": "Read through local R haven.", "columns": columns, "rows": rows}


def _dataset_file_for_kind(root: Path, run_id: str, dataset: str, kind: str) -> Path | None:
    if kind == "generated":
        return _generated_output_path_for_read(root, run_id, dataset)
    if kind == "reference":
        return reference_adam_path(root, dataset)
    raise ApiServiceError("Table kind must be generated or reference.")


def _dataset_download_file(
    root: Path,
    run_id: str,
    dataset: str,
    kind: str,
    *,
    graph_state: StudyRunState | None | object = _GRAPH_STATE_UNSET,
) -> Path | None:
    run_dir = root / "runs" / run_id
    if kind == "generated":
        return _generated_output_path_for_read(root, run_id, dataset, graph_state=graph_state)
    if kind == "reference":
        return reference_adam_path(root, dataset)
    state = graph_state
    if state is _GRAPH_STATE_UNSET:
        state = _load_read_model_graph_state(
            root,
            run_id,
            fail_on_existing=True,
            fallback_message="Dataset artifact download will not fall back to artifact-only mode.",
        )
    if state is not None:
        dataset_state = _graph_dataset_state(state, dataset)
        if dataset_state is None:
            return None
        return _graph_dataset_artifact_path(run_dir, dataset_state, kind)
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
    rscript_path = _default_rscript_path()
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
    for match in re.finditer(r"\bAD[A-Z0-9_]{2,}\b", text.upper()):
        token = match.group(0).strip("_")
        if _is_adam_dataset_token(token) and token not in tokens:
            tokens.append(token)
    for match in re.finditer(r"\bADS[_\-\s]+(AD[A-Z0-9]{2,})(?:[_\-\s]+FULL)?\b", text.upper()):
        token = match.group(1)
        if _is_adam_dataset_token(token) and token not in tokens:
            tokens.append(token)
    return tokens


def _is_adam_dataset_token(token: str) -> bool:
    raw = str(token or "").upper()
    if "_" in raw:
        return False
    value = re.sub(r"[^A-Z0-9]", "", raw)
    if not re.fullmatch(r"AD[A-Z0-9]{2,6}", value):
        return False
    return value not in {"ADAM", "ADAMS", "ADDATA", "ADAMDATA", "ADSLIB", "ADVERSE"}


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


def _string_cell(value: Any) -> str:
    if value is None:
        return ""
    return str(value)


def _dataset_review(
    root: Path,
    run_id: str,
    dataset: str,
    manifest: dict[str, Any],
    workflow_state: dict[str, Any] | None = None,
    graph_state: StudyRunState | None = None,
    *,
    detail_level: str = "full",
) -> DatasetReview:
    dataset_lower = dataset.lower()
    run_dir = root / "runs" / run_id
    resolved_detail_level = _review_summary_detail_level(detail_level)
    result = _dataset_result_from_manifest(manifest, dataset)
    graph_dataset = _graph_dataset_state(graph_state, dataset)
    projected_dataset = _projected_dataset_state(workflow_state or {}, dataset) if graph_dataset is None else {}
    validation_report_path = _dataset_download_file(root, run_id, dataset, "validation_report", graph_state=graph_state)
    diagnostics_path = _graph_dataset_artifact_path(run_dir, graph_dataset, "diagnostics") if graph_dataset else None
    parsed_response_path = _graph_dataset_artifact_path(run_dir, graph_dataset, "parsed_response") if graph_dataset else None
    code_path = _dataset_download_file(root, run_id, dataset, "code", graph_state=graph_state)
    if graph_state is None:
        validation_report_path = run_dir / "validation" / f"{dataset_lower}_validation_report.json"
        diagnostics_path = run_dir / "diagnostics" / f"{dataset_lower}_failure_report.json"
        parsed_response_path = run_dir / "llm" / f"{dataset_lower}_parsed_response.json"
        code_path = run_dir / "code" / f"build_{dataset_lower}.R"
    validation_report = _read_json_if_exists(validation_report_path) if validation_report_path else {}
    diagnostics = _read_json_if_exists(diagnostics_path) if diagnostics_path else {}
    parsed_response = _read_json_if_exists(parsed_response_path) if parsed_response_path else {}
    output_path = _generated_output_path_for_read(root, run_id, dataset, graph_state=graph_state)
    reference_path = reference_adam_path(root, dataset)
    reader = SDTMReader() if resolved_detail_level == "full" else None
    compare_summary = (
        DatasetCompareResponse(
            **compare_dataset_files(dataset, output_path, reference_path, table_reader=_sas7bdat_table_reader())
        )
        if resolved_detail_level == "full"
        else _lightweight_compare_summary(dataset, output_path, reference_path, graph_dataset, result)
    )
    graph_compare_status = str(graph_dataset.compare_summary.get("status") or "") if graph_dataset else ""
    compare_status = (
        compare_summary.status
        if compare_summary.status != "missing_generated"
        else graph_compare_status or result.get("compare_status")
    )
    status = str(
        _graph_dataset_review_status(graph_dataset)
        or result.get("status")
        or _dataset_status_from_report(validation_report, output_path)
        or projected_dataset.get("status")
        or "unknown"
    )
    code_state = graph_dataset.code_state if graph_dataset else _state_map(projected_dataset.get("code_state"))
    execution_state = graph_dataset.execution_state if graph_dataset else _state_map(projected_dataset.get("execution_state"))
    graph_validation = graph_dataset.validation_summary if graph_dataset else {}
    review_validation = graph_validation or validation_report or _state_map(projected_dataset.get("validation_summary"))
    output_quality = dataset_output_quality(
        status=status,
        code_state=code_state,
        execution_state=execution_state,
        validation_summary=review_validation,
    )
    warnings = _unique_strings(
        _string_list(review_validation.get("warnings"))
        + _string_list(output_quality.get("warnings"))
        + _graph_dataset_warnings(graph_dataset)
    )

    return DatasetReview(
        dataset=dataset,
        status=status,
        validation_status=(
            str(review_validation.get("status") or execution_state.get("validation_status") or "")
            if graph_dataset
            else ""
        )
        or result.get("validation_status")
        or validation_report.get("status"),
        compare_status=compare_status,
        output_path=str(output_path.as_posix()) if output_path else None,
        output_quality=output_quality,
        output_preview=(
            _preview_table_file(output_path, role="Generated ADaM", dataset=dataset, reader=reader, sample_rows=5)
            if output_path and reader is not None
            else None
        ),
        reference_preview=(
            _preview_table_file(reference_path, role="Reference ADaM", dataset=dataset, reader=reader, sample_rows=5)
            if reference_path is not None and reader is not None
            else None
        ),
        compare_summary=compare_summary,
        downloads=_dataset_downloads(root, run_id, dataset, graph_state=graph_state),
        generated_code_path=str(code_path.as_posix()) if code_path is not None and code_path.exists() else None,
        generated_code=_read_text_if_exists(code_path, limit_chars=40000) if code_path is not None else "",
        assumptions=_string_list(code_state.get("assumptions")) or _string_list(parsed_response.get("assumptions")),
        risk_points=_string_list(code_state.get("risk_points")) or _string_list(parsed_response.get("risk_points")),
        warnings=warnings,
        errors=_string_list(review_validation.get("errors")),
        validation_report=review_validation,
        diagnostics=diagnostics,
    )


def _lightweight_compare_summary(
    dataset: str,
    output_path: Path | None,
    reference_path: Path | None,
    graph_dataset: DatasetRunState | None,
    result: dict[str, Any],
) -> DatasetCompareResponse:
    target = dataset.strip().upper()
    graph_summary = graph_dataset.compare_summary if graph_dataset is not None else {}
    if graph_summary:
        payload = {
            key: value
            for key, value in graph_summary.items()
            if key in DatasetCompareResponse.model_fields
        }
        payload.setdefault("dataset", target)
        payload.setdefault("status", str(graph_summary.get("status") or "recorded"))
        return DatasetCompareResponse(**payload)
    recorded_status = str(result.get("compare_status") or "").strip()
    if recorded_status:
        return DatasetCompareResponse(dataset=target, status=recorded_status)
    if output_path is None:
        return DatasetCompareResponse(dataset=target, status="missing_generated")
    if reference_path is None:
        return DatasetCompareResponse(dataset=target, status="missing_reference")
    return DatasetCompareResponse(
        dataset=target,
        status="not_run",
        generated_file=output_path.name,
        reference_file=reference_path.name,
        note="Compare is available but has not been run in this lightweight status refresh.",
    )


def _dataset_result_from_manifest(manifest: dict[str, Any], dataset: str) -> dict[str, Any]:
    target = dataset.upper()
    for result in manifest.get("dataset_results", []):
        if str(result.get("dataset", "")).strip().upper() == target:
            return result
    return {}


def _load_read_model_graph_state(
    root: Path,
    run_id: str,
    *,
    fail_on_existing: bool = False,
    fallback_message: str = "Review summary will not fall back to workflow_state.json.",
) -> StudyRunState | None:
    try:
        with _open_graph_gateway(study_dir=root, run_id=run_id) as gateway:
            return gateway.load_graph_state(study_dir=root, run_id=run_id)
    except FileNotFoundError:
        return None
    except (ValueError, ValidationError) as exc:
        graph_state_path = root / "runs" / run_id / "graph_state.json"
        if fail_on_existing and graph_state_path.exists():
            raise ApiServiceError(
                f"Canonical graph state for run {run_id} exists but cannot be read. "
                f"{fallback_message}"
            ) from exc
        return None


def _review_datasets_from_graph_state(graph_state: StudyRunState | None) -> list[str]:
    if graph_state is None:
        return []
    return _unique_non_empty(
        list(graph_state.target_datasets)
        + list(graph_state.requested_datasets)
        + list(graph_state.datasets)
    )


def _graph_dataset_state(graph_state: StudyRunState | None, dataset: str) -> DatasetRunState | None:
    if graph_state is None:
        return None
    return graph_state.datasets.get(dataset.strip().upper())


def _graph_dataset_artifact_path(
    run_dir: Path,
    dataset_state: DatasetRunState | None,
    kind: str,
) -> Path | None:
    if dataset_state is None:
        return None
    if kind == "code":
        artifact_path = _first_graph_artifact_path(run_dir, dataset_state, {"generated_code"})
        if artifact_path is not None:
            return artifact_path
        code_path = str(dataset_state.code_state.get("code_path") or "").strip()
        return _existing_run_artifact_path(run_dir, code_path)
    if kind == "validation_report":
        artifact_path = _first_graph_artifact_path(run_dir, dataset_state, {"validation_report"})
        if artifact_path is not None:
            return artifact_path
        validation_path = str(dataset_state.execution_state.get("validation_report_path") or "").strip()
        return _existing_run_artifact_path(run_dir, validation_path)
    if kind == "compare_report":
        artifact_path = _first_graph_artifact_path(run_dir, dataset_state, {"compare_report"})
        if artifact_path is not None:
            return artifact_path
        compare_path = str(dataset_state.compare_summary.get("report_path") or "").strip()
        if not compare_path and dataset_state.result_summary is not None:
            compare_path = str(dataset_state.result_summary.metadata.get("compare_report_path") or "").strip()
        return _existing_run_artifact_path(run_dir, compare_path)
    if kind == "diagnostics":
        artifact_path = _first_graph_artifact_path(run_dir, dataset_state, {"tool_log"}, name_contains="_failure_report")
        if artifact_path is not None:
            return artifact_path
        diagnostics_path = str(dataset_state.execution_state.get("diagnostics_path") or "").strip()
        return _existing_run_artifact_path(run_dir, diagnostics_path)
    if kind == "parsed_response":
        return _first_graph_artifact_path(run_dir, dataset_state, {"tool_log"}, name_contains="_parsed_response")
    return None


def _first_graph_artifact_path(
    run_dir: Path,
    dataset_state: DatasetRunState,
    kinds: set[str],
    *,
    name_contains: str = "",
) -> Path | None:
    for artifact in dataset_state.artifacts:
        if artifact.kind not in kinds:
            continue
        if name_contains and name_contains not in str(artifact.path):
            continue
        path = _existing_run_artifact_path(run_dir, str(artifact.path))
        if path is not None:
            return path
    return None


def _existing_run_artifact_path(run_dir: Path, artifact_path: str) -> Path | None:
    candidate = _resolve_review_artifact_path(run_dir, artifact_path)
    if candidate is not None and candidate.exists() and candidate.is_file():
        return candidate
    return None


def _json_artifact_path_for_read(root: Path, run_id: str, relative_path: str) -> Path:
    run_dir = (root / "runs" / run_id).resolve()
    normalized_text = str(relative_path).replace("\\", "/").strip()
    run_prefix = f"runs/{run_id}/"
    if normalized_text.startswith(run_prefix):
        normalized_text = normalized_text[len(run_prefix) :]
    else:
        marker = f"/runs/{run_id}/"
        marker_index = normalized_text.find(marker)
        if marker_index >= 0:
            normalized_text = normalized_text[marker_index + len(marker) :]
    normalized = Path(normalized_text)
    target_path = (run_dir / normalized).resolve()
    try:
        canonical_relative = target_path.relative_to(run_dir)
    except ValueError:
        canonical_relative = normalized
    graph_state = _load_read_model_graph_state(
        root,
        run_id,
        fail_on_existing=True,
        fallback_message="JSON artifact read will not fall back to artifact-only mode.",
    )
    if graph_state is None:
        return target_path
    study_artifact_path = _graph_study_artifact_path(run_dir, graph_state, canonical_relative)
    if study_artifact_path is not None:
        return study_artifact_path.resolve()
    dataset_artifact_path = _graph_dataset_artifact_path_by_relative(run_dir, graph_state, canonical_relative)
    if dataset_artifact_path is not None:
        return dataset_artifact_path.resolve()
    guarded = _guarded_dataset_artifact_request(canonical_relative)
    if guarded is None:
        raise ApiServiceError(f"Artifact is not recorded in canonical graph state: {relative_path}")
    dataset, kind = guarded
    dataset_state = _graph_dataset_state(graph_state, dataset)
    guarded_path = _graph_dataset_artifact_path(run_dir, dataset_state, kind)
    if guarded_path is None:
        raise ApiServiceError(f"Artifact is not recorded in canonical graph state: {relative_path}")
    return guarded_path.resolve()


def _graph_dataset_artifact_path_by_relative(
    run_dir: Path,
    graph_state: StudyRunState,
    relative_path: Path,
) -> Path | None:
    """Read any dataset artifact only when its exact path is in graph_state."""

    requested = Path(str(relative_path).replace("\\", "/"))
    for dataset_state in graph_state.datasets.values():
        for artifact in dataset_state.artifacts:
            candidate = _existing_run_artifact_path(run_dir, str(artifact.path))
            if candidate is None:
                continue
            try:
                candidate_relative = candidate.resolve().relative_to(run_dir.resolve())
            except ValueError:
                continue
            if Path(str(candidate_relative).replace("\\", "/")) == requested:
                return candidate
    return None


def _guarded_dataset_artifact_request(relative_path: Path) -> tuple[str, str] | None:
    parts = relative_path.parts
    if len(parts) != 2:
        return None
    folder, filename = parts[0].lower(), parts[1]
    lower_name = filename.lower()
    if folder == "validation" and lower_name.endswith("_validation_report.json"):
        return lower_name.removesuffix("_validation_report.json").upper(), "validation_report"
    if folder == "diagnostics" and lower_name.endswith("_failure_report.json"):
        return lower_name.removesuffix("_failure_report.json").upper(), "diagnostics"
    if folder == "compare" and lower_name.endswith("_compare_report.json"):
        return lower_name.removesuffix("_compare_report.json").upper(), "compare_report"
    if folder == "llm" and lower_name.endswith("_parsed_response.json"):
        return lower_name.removesuffix("_parsed_response.json").upper(), "parsed_response"
    return None


def _graph_study_artifact_path(
    run_dir: Path,
    graph_state: StudyRunState,
    relative_path: Path,
) -> Path | None:
    normalized_relative = Path(str(relative_path).replace("\\", "/"))
    for artifact in graph_state.artifacts:
        candidate = _existing_run_artifact_path(run_dir, str(artifact.path))
        if candidate is None:
            continue
        try:
            candidate_relative = candidate.resolve().relative_to(run_dir.resolve())
        except ValueError:
            continue
        if candidate_relative == normalized_relative:
            return candidate
    return None


def _graph_dataset_review_status(dataset_state: DatasetRunState | None) -> str:
    if dataset_state is None:
        return ""
    return str(dataset_state.status or "").strip()


def _review_output_path(run_dir: Path, dataset: str, dataset_state: DatasetRunState | None) -> Path | None:
    if dataset_state is not None and _graph_dataset_output_unusable(dataset_state):
        return None
    output_from_state = ""
    if dataset_state is not None:
        output_from_state = str(dataset_state.execution_state.get("output_path") or "").strip()
        if not output_from_state:
            for artifact in dataset_state.artifacts:
                if artifact.kind == "output_adam":
                    output_from_state = str(artifact.path or "").strip()
                    break
    if output_from_state:
        candidate = _resolve_review_artifact_path(run_dir, output_from_state)
        if candidate is not None and candidate.exists() and candidate.is_file():
            return candidate
    if dataset_state is not None:
        return None
    return usable_generated_output_path(run_dir, dataset)


def _generated_output_path_for_read(
    root: Path,
    run_id: str,
    dataset: str,
    *,
    graph_state: StudyRunState | None | object = _GRAPH_STATE_UNSET,
) -> Path | None:
    run_dir = root / "runs" / run_id
    state = graph_state
    if state is _GRAPH_STATE_UNSET:
        state = _load_read_model_graph_state(
            root,
            run_id,
            fail_on_existing=True,
            fallback_message="Generated output read will not fall back to validation artifacts.",
        )
    if state is None:
        return usable_generated_output_path(run_dir, dataset)
    dataset_state = _graph_dataset_state(state, dataset)
    if dataset_state is None:
        return None
    return _review_output_path(run_dir, dataset, dataset_state)


def _graph_dataset_output_unusable(dataset_state: DatasetRunState) -> bool:
    status = str(dataset_state.status or "").strip()
    interrupt = dataset_state.current_interrupt
    execution_state = dataset_state.execution_state
    validation_summary = dataset_state.validation_summary
    if status in {"terminal_failure", "failed"}:
        return True
    if interrupt is not None and interrupt.name == "terminal_failure" and interrupt.status == "open":
        return True
    if execution_state.get("terminal_failure") is True or validation_summary.get("terminal_failure") is True:
        return True
    if execution_state.get("partial_output_usable") is False or validation_summary.get("partial_output_usable") is False:
        return True
    return False


def _resolve_review_artifact_path(run_dir: Path, artifact_path: str) -> Path | None:
    artifact_path = artifact_path.strip()
    if not artifact_path:
        return None
    candidate = Path(artifact_path)
    if candidate.is_absolute():
        return candidate if _path_is_under(candidate, run_dir) else None
    normalized = Path(str(artifact_path).replace("\\", "/"))
    parts = normalized.parts
    run_marker_index = None
    for index in range(len(parts) - 1):
        if parts[index].lower() == "runs" and parts[index + 1] == run_dir.name:
            run_marker_index = index + 2
            break
    if run_marker_index is not None:
        candidate = run_dir.joinpath(*parts[run_marker_index:])
    else:
        candidate = run_dir / normalized
    return candidate if _path_is_under(candidate, run_dir) else None


def _path_is_under(candidate: Path, parent: Path) -> bool:
    try:
        candidate.resolve(strict=False).relative_to(parent.resolve(strict=False))
    except (OSError, ValueError):
        return False
    return True


def _graph_dataset_warnings(dataset_state: DatasetRunState | None) -> list[str]:
    if dataset_state is None:
        return []
    warnings: list[str] = []
    for state_map in [dataset_state.spec_state, dataset_state.code_state, dataset_state.execution_state]:
        raw_warnings = state_map.get("warnings")
        if isinstance(raw_warnings, list):
            warnings.extend(str(item) for item in raw_warnings)
        stale_reason = str(state_map.get("stale_reason") or "").strip()
        if stale_reason:
            warnings.append(stale_reason)
    for failure in dataset_state.failures:
        message = str(getattr(failure, "message", "") or "").strip()
        if message:
            warnings.append(message)
    return warnings


def _projected_dataset_state(workflow_state: dict[str, Any], dataset: str) -> dict[str, Any]:
    datasets = workflow_state.get("datasets")
    if not isinstance(datasets, dict):
        return {}
    projected = datasets.get(dataset.strip().upper())
    return projected if isinstance(projected, dict) else {}


def _state_map(value: Any) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


def _dataset_status_from_report(report: dict[str, Any], output_path: Path | None) -> str:
    status = report.get("status")
    if status == "pass":
        return "completed"
    if status == "structural_stub_pass":
        return "completed_stub"
    if status:
        return "failed"
    return "completed" if output_path is not None else "unknown"


def _dataset_downloads(
    root: Path,
    run_id: str,
    dataset: str,
    *,
    graph_state: StudyRunState | None | object = _GRAPH_STATE_UNSET,
) -> list[DownloadItem]:
    specs = [
        ("generated", "Generated ADaM"),
        ("reference", "Reference ADaM"),
        ("code", "Generated R Code"),
        ("validation_report", "Validation Report"),
        ("compare_report", "Compare Report"),
    ]
    items: list[DownloadItem] = []
    for kind, label in specs:
        path = _dataset_download_file(root, run_id, dataset, kind, graph_state=graph_state)
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


def _unique_strings(values: list[str]) -> list[str]:
    result: list[str] = []
    for value in values:
        normalized = str(value).strip()
        if normalized and normalized not in result:
            result.append(normalized)
    return result


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


def _advanced_artifacts(run_dir: Path, *, graph_state: StudyRunState | None = None) -> dict[str, str]:
    if graph_state is not None:
        candidates: dict[str, Path] = {}
        for artifact in graph_state.artifacts:
            key = f"{artifact.kind}_{artifact.artifact_id}"
            path = _existing_run_artifact_path(run_dir, str(artifact.path))
            if path is not None:
                candidates[key] = path
        for dataset, dataset_state in sorted(graph_state.datasets.items()):
            for artifact in dataset_state.artifacts:
                key = f"{dataset.lower()}_{artifact.kind}_{artifact.artifact_id}"
                path = _existing_run_artifact_path(run_dir, str(artifact.path))
                if path is not None:
                    candidates[key] = path
        return {key: str(path.as_posix()) for key, path in candidates.items() if path.exists()}
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
    failed = sum(1 for review in reviews if review.status == "failed")
    real_runtime = sum(
        1
        for review in reviews
        if review.output_quality.get("quality_status") == "real_runtime_output"
    )
    review_only = sum(
        1
        for review in reviews
        if review.output_quality.get("quality_status") in {"structural_stub", "not_real_derivation"}
    )
    terminal_failure = sum(
        1
        for review in reviews
        if review.output_quality.get("quality_status") == "terminal_failure"
    )
    parts = [f"Run status is {status}.", f"{real_runtime} dataset(s) have real runtime output"]
    if review_only:
        parts.append(f"{review_only} review-only/demo output(s)")
    if failed:
        parts.append(f"{failed} failed")
    if terminal_failure:
        parts.append(f"{terminal_failure} terminal failure output(s) hidden")
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

