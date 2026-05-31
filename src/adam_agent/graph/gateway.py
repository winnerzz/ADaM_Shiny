"""Graph-native gateway for LangGraph-2 workflow entry points."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import UTC, datetime
import json
from pathlib import Path
import sqlite3
from typing import Any

from langgraph.types import Command

from adam_agent.agents import (
    AgentDecision,
    AgentNodeInput,
    AgentNodeOutput,
    build_agent_node_input,
    build_agent_node_output,
    build_agent_audit_summary_from_state,
    record_agent_decision,
    write_agent_audit_summary,
)
from adam_agent.graph.checkpointing import CheckpointerBundle, build_checkpointer, describe_checkpointer
from adam_agent.graph.dataset_graph import compile_dataset_graph
from adam_agent.graph.execution import GraphExecutionError, assert_graph_code_review_current
from adam_agent.graph.execution_modes import (
    GRAPH_PRODUCT_EXECUTE_MODE,
    GRAPH_PRODUCT_GENERATE_CODE_MODE,
    GRAPH_PRODUCT_PREPARE_MODE,
)
from adam_agent.graph.output_quality import dataset_output_quality, study_output_quality_rollup
from adam_agent.graph.study_graph import compile_study_graph
from adam_agent.graph.workflow_state import (
    compare_fingerprints,
    input_fingerprint,
    invalidate_active_workflows,
    project_graph_state_to_workflow,
    update_workflow_state,
)
from adam_agent.schemas.graph_state import DatasetRunState, HumanCommand, InterruptState, StudyRunState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary
from adam_agent.schemas.base import utc_now
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.compare import compare_dataset_files, reference_adam_path, usable_generated_output_path
from adam_agent.tools.static_rules import StaticRuleError, validate_static_rule_report_artifact


LEGACY_RUN_TO_COMPLETION_COMPATIBILITY_SHIM = "legacy_run_to_completion_compatibility_shim"
TERMINAL_FAILURE_REVIEW_ACTIONS: tuple[dict[str, str], ...] = (
    {"action": "retry_execution", "label": "Retry Execution"},
    {"action": "repair_code", "label": "Repair Code"},
    {"action": "revise_spec", "label": "Revise Spec"},
    {"action": "request_new_input", "label": "Request New Input"},
    {"action": "skip_dataset", "label": "Skip Dataset"},
    {"action": "continue_other_datasets", "label": "Continue Other Datasets"},
)


@dataclass(frozen=True)
class GraphGatewayResult:
    """Graph state plus its UI projection."""

    graph_state: StudyRunState
    workflow_projection: dict[str, Any]


@dataclass(frozen=True)
class GraphGatewayExecutionResult(GraphGatewayResult):
    """Graph-owned execution result plus API-facing execution fields."""

    status: str
    validation_status: str
    output_path: str | None
    validation_report_path: str | None
    diagnostics_path: str | None
    terminal_failure: bool
    errors: list[str]
    warnings: list[str]


@dataclass(frozen=True)
class GraphGatewayCodeGenerationResult(GraphGatewayResult):
    """Graph-owned code-generation result plus API-facing fields."""

    code_path: str
    generated_code: str
    static_check_path: str | None
    draft_spec_path: str | None
    response_path: str | None
    parsed_response_path: str | None
    context_path: str | None
    assumptions: list[str]
    risk_points: list[str]
    used_inputs: list[str]
    expected_outputs: list[str]
    warnings: list[str]
    dependency_review_status: str | None = None
    dependency_warnings: list[str] | None = None


@dataclass(frozen=True)
class GraphGatewayCodeReviewResult(GraphGatewayResult):
    """Graph-owned code-review result plus API-facing fields."""

    decision: str
    review_path: str
    approved: bool
    static_check_path: str | None


@dataclass(frozen=True)
class GraphGatewayDraftSpecReviewResult(GraphGatewayResult):
    """Graph-owned draft-spec review result plus API-facing fields."""

    decision: str
    review_path: str
    approved: bool
    approved_spec_path: str | None


@dataclass(frozen=True)
class GraphGatewayTerminalFailureReviewResult(GraphGatewayResult):
    """Graph-owned terminal-failure review result plus API-facing fields."""

    decision: str
    current_interrupt: str | None
    next_action: str


@dataclass(frozen=True)
class GraphGatewayFinalizeInputsResult(GraphGatewayResult):
    """Graph-owned finalize-inputs result plus response-neutral fields."""

    spec_source: str
    warnings: list[str]
    dependency_review_status: str | None = None
    dependency_warnings: list[str] | None = None
    input_spec_path: str | None = None
    approved_spec_path: str | None = None
    draft_spec_path: str | None = None
    draft_spec_prompt_path: str | None = None
    draft_spec_response_path: str | None = None
    draft_spec_variables: list[dict[str, Any]] | None = None


@dataclass(frozen=True)
class GraphGatewayDependencyGateResult:
    """Graph-owned dependency gate result for one product step."""

    study_id: str
    run_id: str
    requested_datasets: list[str]
    target_datasets: list[str]
    runnable_datasets: list[str]
    blocked_datasets: list[dict[str, str]]
    dependency_review_status: str
    dependency_plan: dict[str, Any]
    dependency_decisions: list[dict[str, Any]]
    dependency_resolution: list[dict[str, Any]]
    dependency_warnings: list[str]
    workflow_state_path: str | None = None


@dataclass(frozen=True)
class GraphGatewayDependencyReviewResult(GraphGatewayResult):
    """Graph-owned dependency-review result plus API-facing fields."""

    decision: str
    approved: bool
    current_interrupt: str | None


@dataclass(frozen=True)
class GraphGatewayInputInvalidationResult:
    """Canonical and compatibility projections touched after study input changes."""

    input_fingerprint: dict[str, Any]
    input_diff: dict[str, Any]
    touched_runs: list[str]
    touched_graph_runs: list[str]
    skipped_graph_runs: list[str]


@dataclass(frozen=True)
class GraphGatewayLegacyRunResult:
    """Graph invocation result plus the legacy workflow projection."""

    graph_result: dict[str, Any]
    workflow_projection: dict[str, Any]


@dataclass(frozen=True)
class GraphGatewayCompareResult(GraphGatewayResult):
    """Graph-owned reference compare result plus API-facing compare fields."""

    compare_summary: dict[str, Any]


class GraphGateway:
    """Single entry point for starting, resuming, and reading graph runs.

    The first migration step supports graph-native dependency planning. Later
    phases should move dataset-level draft/spec/code/execute gates behind this
    same gateway instead of adding more FastAPI-local state transitions.
    """

    def __init__(
        self,
        *,
        checkpointer: Any | None = None,
        checkpointer_bundle: CheckpointerBundle | None = None,
        checkpointer_backend: str = "memory",
        sqlite_checkpointer_path: str | Path | None = None,
    ) -> None:
        bundle = checkpointer_bundle if checkpointer is None else None
        if checkpointer is None and bundle is None:
            bundle = build_checkpointer(checkpointer_backend, sqlite_path=sqlite_checkpointer_path)  # type: ignore[arg-type]
        self._checkpointer_bundle = bundle
        self._checkpointer = checkpointer if checkpointer is not None else bundle.checkpointer
        self._graph = compile_study_graph(checkpointer=self._checkpointer)

    def close(self) -> None:
        """Release process-local resources held by the configured checkpointer."""

        if self._checkpointer_bundle is not None:
            self._checkpointer_bundle.close()

    def __enter__(self) -> GraphGateway:
        return self

    def __exit__(self, exc_type: object, exc: object, traceback: object) -> None:
        self.close()

    def start_dependency_plan(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        target_datasets: list[str],
        approved_dependency_datasets: list[str] | None = None,
    ) -> GraphGatewayResult:
        """Run StudyGraph until the dependency-review checkpoint."""

        root = Path(study_dir).expanduser()
        if not root.exists() or not root.is_dir():
            raise ValueError(f"study_dir does not exist or is not a directory: {root}")
        normalized_targets = _normalize_dataset_list(target_datasets)
        if not normalized_targets:
            raise ValueError("target_datasets must not be empty")
        payload = {
            "study_id": study_id,
            "run_id": run_id,
            "target_datasets": normalized_targets,
            "approved_dependency_datasets": _normalize_dataset_list(approved_dependency_datasets or []),
            "study_dir": str(root),
            "graph_gateway_mode": "plan_only",
            "dataset_results": [],
            "blocked_datasets": [],
            "audit_artifacts": [],
        }
        result = self._graph.invoke(payload, config=self._config(study_id, run_id))
        graph_state = self._canonical_state_from_plan(result, study_dir=root)
        graph_state = self._merge_existing_dataset_progress(root, graph_state)
        _sync_study_agent_decisions(graph_state)
        self._persist_graph_state(root, graph_state, node="dependency_plan")
        projection = project_graph_state_to_workflow(root, graph_state, node="graph_gateway_plan")
        return GraphGatewayResult(graph_state=graph_state, workflow_projection=projection)

    def start_native_dependency_review(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        target_datasets: list[str],
        approved_dependency_datasets: list[str] | None = None,
    ) -> GraphGatewayResult:
        """Start a narrow native LangGraph interrupt pilot for dependency review."""

        root = Path(study_dir).expanduser()
        if not root.exists() or not root.is_dir():
            raise ValueError(f"study_dir does not exist or is not a directory: {root}")
        normalized_targets = _normalize_dataset_list(target_datasets)
        if not normalized_targets:
            raise ValueError("target_datasets must not be empty")
        result = self._graph.invoke(
            {
                "study_id": study_id,
                "run_id": run_id,
                "target_datasets": normalized_targets,
                "approved_dependency_datasets": _normalize_dataset_list(approved_dependency_datasets or []),
                "study_dir": str(root),
                "graph_gateway_mode": "native_dependency_review",
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            },
            config=self._config(study_id, run_id),
        )
        plan_values = self.get_state(study_id=study_id, run_id=run_id)
        graph_state = self._canonical_state_from_plan(plan_values or result, study_dir=root)
        native_interrupt = _native_interrupt_payload(
            self._graph.get_state(self._config(study_id, run_id)),
            boundary="dependency_review_pilot_only",
        )
        _sync_study_agent_decisions(graph_state)
        self._persist_graph_state(
            root,
            graph_state,
            node="native_dependency_review_interrupt",
            runtime_persistence_extra={"native_dependency_review_interrupt": native_interrupt},
        )
        projection = project_graph_state_to_workflow(
            root,
            graph_state,
            node="graph_gateway_native_dependency_review_interrupt",
        )
        return GraphGatewayResult(graph_state=graph_state, workflow_projection=projection)

    def resume_native_dependency_review(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        decision: str,
        reviewer: str,
        notes: str = "",
        approved_dependency_datasets: list[str] | None = None,
    ) -> GraphGatewayDependencyReviewResult:
        """Resume the native dependency-review interrupt pilot and persist canonical state."""

        root = Path(study_dir).expanduser()
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        if graph_state.current_interrupt is None or graph_state.current_interrupt.name != "dependency_review":
            raise ValueError("Current graph state is not waiting for dependency_review.")
        normalized_decision = decision.strip().lower()
        if normalized_decision not in {"approve", "reject"}:
            raise ValueError("Dependency review decision must be approve or reject.")
        command = HumanCommand(
            interrupt="dependency_review",
            action="approve" if normalized_decision == "approve" else "reject",
            reviewer=reviewer,
            notes=notes,
            payload={"approved_dependency_datasets": _normalize_dataset_list(approved_dependency_datasets or [])},
        )
        _assert_resume_command_matches_open_interrupt(graph_state, command)
        resumed = self._graph.invoke(
            Command(
                resume={
                    "action": command.action,
                    "reviewer": command.reviewer,
                    "notes": command.notes,
                    "payload": command.payload,
                }
            ),
            config=self._config(graph_state.study_id, run_id),
        )
        next_state = self._canonical_state_from_plan(resumed, study_dir=root)
        next_state.human_commands.append(command)
        next_state.dependency_review_status = "approved" if command.action == "approve" else "rejected"
        next_state.current_interrupt = None
        next_state.status = "pending" if command.action == "approve" else "failed"
        native_interrupt = _native_interrupt_payload(
            self._graph.get_state(self._config(graph_state.study_id, run_id)),
            boundary="dependency_review_pilot_only",
        )
        _sync_study_agent_decisions(next_state)
        self._persist_graph_state(
            root,
            next_state,
            node="native_dependency_review_resume",
            runtime_persistence_extra={"native_dependency_review_interrupt": native_interrupt},
        )
        projection = project_graph_state_to_workflow(
            root,
            next_state,
            node="graph_gateway_native_dependency_review_resume",
        )
        current_interrupt = None
        if next_state.current_interrupt is not None and next_state.current_interrupt.status == "open":
            current_interrupt = next_state.current_interrupt.name
        return GraphGatewayDependencyReviewResult(
            graph_state=next_state,
            workflow_projection=projection,
            decision=normalized_decision,
            approved=normalized_decision == "approve",
            current_interrupt=current_interrupt,
        )

    def block_legacy_run_to_completion(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        requested_datasets: list[str],
        execution_mode: str,
    ) -> dict[str, Any]:
        """Write the compatibility projection for rejected legacy LLM `/runs` calls."""

        root = Path(study_dir).expanduser()
        run_dir = root / "runs" / run_id
        return update_workflow_state(
            root,
            run_id,
            study_id=study_id,
            node="run_study_request_blocked",
            status="blocked",
            current_interrupt="split_flow_required",
            input_fingerprint_payload=input_fingerprint(root),
            extra={
                "requested_datasets": _normalize_dataset_list(requested_datasets),
                "execution_mode": execution_mode,
                "blocked_reason": "LLM ADaM generation must use the draft/spec/code-review/execute API flow.",
                "workflow_control": LEGACY_RUN_TO_COMPLETION_COMPATIBILITY_SHIM,
                "legacy_endpoint": "POST /runs",
                "product_flow_required": True,
                "graph_state_path": None,
                "workflow_state_path": str((run_dir / "workflow_state.json").as_posix()),
            },
        )

    def run_legacy_to_completion(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        target_datasets: list[str],
        execution_mode: str,
        approved_dependency_datasets: list[str] | None = None,
        rscript_path: str = "",
        llm_exposure: dict[str, Any] | None = None,
        llm_provider: dict[str, Any] | None = None,
    ) -> GraphGatewayLegacyRunResult:
        """Run the old `/runs` compatibility path and write its projection."""

        root = Path(study_dir).expanduser()
        result = self._graph.invoke(
            {
                "study_id": study_id,
                "run_id": run_id,
                "target_datasets": list(target_datasets),
                "execution_mode": execution_mode,
                "study_dir": str(root),
                "rscript_path": rscript_path,
                "approved_dependency_datasets": _normalize_dataset_list(approved_dependency_datasets or []),
                "llm_exposure": dict(llm_exposure or {}),
                "llm_provider": dict(llm_provider or {}),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            },
            config=self._config(study_id, run_id),
        )
        run_dir = root / "runs" / run_id
        projection = update_workflow_state(
            root,
            run_id,
            study_id=study_id,
            node="run_study_request",
            status=result.get("status"),
            input_fingerprint_payload=input_fingerprint(root),
            extra={
                "requested_datasets": result.get("requested_datasets", []),
                "target_datasets": result.get("target_datasets", []),
                "runnable_datasets": result.get("runnable_datasets", []),
                "blocked_datasets": result.get("blocked_datasets", []),
                "dependency_review_status": result.get("dependency_review_status"),
                "execution_mode": execution_mode,
                "dataset_results": [
                    summary.model_dump(mode="json") if hasattr(summary, "model_dump") else summary
                    for summary in result.get("dataset_results", [])
                ],
                "audit_manifest": _artifact_path(result.get("audit_manifest")),
                "workflow_control": LEGACY_RUN_TO_COMPLETION_COMPATIBILITY_SHIM,
                "legacy_endpoint": "POST /runs",
                "product_flow_required": False,
                "graph_state_path": None,
                "workflow_state_path": str((run_dir / "workflow_state.json").as_posix()),
            },
        )
        return GraphGatewayLegacyRunResult(graph_result=dict(result), workflow_projection=projection)

    def resume(self, *, study_dir: str | Path, graph_state: StudyRunState, command: HumanCommand) -> GraphGatewayResult:
        """Record a human command against a graph-native interrupt.

        This is intentionally conservative in LG2.1: it persists the command
        into canonical state and projection, but does not yet advance dataset
        product nodes. Those node transitions are introduced in LG2.2.
        """

        _assert_resume_command_matches_open_interrupt(graph_state, command)
        next_state = graph_state.model_copy(deep=True)
        next_state.human_commands.append(command)
        next_state.updated_at = utc_now()
        if (
            next_state.current_interrupt is not None
            and next_state.current_interrupt.dataset is None
            and next_state.current_interrupt.name == command.interrupt
            and command.action == "approve"
        ):
            next_state.current_interrupt = None
        if command.dataset:
            dataset_key = command.dataset.strip().upper()
            dataset_state = next_state.datasets.get(dataset_key)
            if dataset_state is not None:
                dataset_state.human_commands.append(command)
                dataset_state.current_interrupt = None if command.action == "approve" else dataset_state.current_interrupt
                dataset_state.updated_at = utc_now()
        _roll_up_study_state(next_state)
        if command.action != "approve" and next_state.status == "running":
            next_state.status = "needs_review"
        self._persist_graph_state(study_dir, next_state, node=f"resume_{command.interrupt}")
        projection = project_graph_state_to_workflow(study_dir, next_state, node=f"graph_gateway_resume_{command.interrupt}")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def review_dependency(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        decision: str,
        reviewer: str,
        notes: str = "",
        approved_dependency_datasets: list[str] | None = None,
    ) -> GraphGatewayDependencyReviewResult:
        """Persist a human decision for the study-level dependency-review gate."""

        root = Path(study_dir).expanduser()
        normalized_decision = decision.strip().lower()
        if normalized_decision not in {"approve", "reject"}:
            raise ValueError("Dependency review decision must be approve or reject.")
        try:
            graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        except FileNotFoundError as exc:
            raise ValueError(str(exc)) from exc
        if graph_state.current_interrupt is None or graph_state.current_interrupt.name != "dependency_review":
            raise ValueError("Current graph state is not waiting for dependency_review.")
        result = self.resume(
            study_dir=root,
            graph_state=graph_state,
            command=HumanCommand(
                interrupt="dependency_review",
                action="approve" if normalized_decision == "approve" else "reject",
                reviewer=reviewer,
                notes=notes,
                payload={
                    "approved_dependency_datasets": _normalize_dataset_list(approved_dependency_datasets or [])
                },
            ),
        )
        current_interrupt = None
        if result.graph_state.current_interrupt is not None and result.graph_state.current_interrupt.status == "open":
            current_interrupt = result.graph_state.current_interrupt.name
        return GraphGatewayDependencyReviewResult(
            graph_state=result.graph_state,
            workflow_projection=result.workflow_projection,
            decision=normalized_decision,
            approved=normalized_decision == "approve",
            current_interrupt=current_interrupt,
        )

    def review_code(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        decision: str,
        reviewer: str,
        notes: str = "",
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayCodeReviewResult:
        """Write the code-review artifact and persist the graph-owned decision."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        normalized_decision = decision.strip().lower()
        if normalized_decision not in {"approve", "reject"}:
            raise ValueError("Code review decision must be approve or reject.")
        run_dir = root / "runs" / run_id
        code_path = run_dir / "code" / f"build_{target.lower()}.R"
        if not code_path.exists() or not code_path.is_file():
            raise ValueError(f"Generated R code does not exist for review: {code_path}")
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        review_dir = run_dir / "review"
        review_dir.mkdir(parents=True, exist_ok=True)
        review_path = review_dir / f"{target.lower()}_code_review.json"
        static_check_path = run_dir / "static_checks" / f"{target.lower()}_static_check.json"
        static_path_for_state = static_check_path if static_check_path.exists() else None
        code_sha = f"sha256:{sha256_file(code_path)}"
        static_check_sha = f"sha256:{sha256_file(static_check_path)}" if static_check_path.exists() else None
        code_state = self._generated_code_state(root, run_id=run_id, dataset=target)
        spec_path = code_state.get("spec_path")
        spec_sha = code_state.get("spec_sha256")
        if spec_path and not spec_sha:
            raise ValueError("Generated-code graph state is missing the approved spec hash. Regenerate code before review.")
        if spec_path and spec_sha:
            current_spec_path = Path(str(spec_path))
            if not current_spec_path.exists() or not current_spec_path.is_file():
                raise ValueError(f"Approved spec used for code generation no longer exists: {current_spec_path}")
            if f"sha256:{sha256_file(current_spec_path)}" != spec_sha:
                raise ValueError("Approved spec changed after code generation. Regenerate code before review.")
        self.validate_code_review(
            study_dir=root,
            run_id=run_id,
            dataset=target,
            code_sha256=code_sha,
            static_check_sha256=static_check_sha,
            spec_sha256=spec_sha,
            input_fingerprint_payload=fingerprint,
        )
        command = HumanCommand(
            interrupt="code_review",
            action="approve" if normalized_decision == "approve" else "reject",
            dataset=target,
            reviewer=reviewer,
            notes=notes,
            payload={
                "review_path": str(review_path.as_posix()),
                "code_path": str(code_path.as_posix()),
                "code_sha256": code_sha,
                "static_check_path": str(static_check_path.as_posix()) if static_check_path.exists() else None,
                "static_check_sha256": static_check_sha,
                "spec_source": code_state.get("spec_source"),
                "spec_path": spec_path,
                "spec_sha256": spec_sha,
            },
        )
        payload = {
            "study_id": study_id,
            "run_id": run_id,
            "dataset": target,
            "decision": normalized_decision,
            "reviewer": reviewer,
            "notes": notes,
            "approved": normalized_decision == "approve",
            "reviewed_at": datetime.now(UTC).isoformat(timespec="seconds").replace("+00:00", "Z"),
            "input_fingerprint": fingerprint,
            "code_path": str(code_path.as_posix()),
            "code_sha256": code_sha,
            "static_check_path": str(static_check_path.as_posix()) if static_check_path.exists() else None,
            "static_check_sha256": static_check_sha,
            "spec_source": code_state.get("spec_source"),
            "spec_path": spec_path,
            "spec_sha256": spec_sha,
        }
        _write_json(review_path, payload)
        try:
            result = self.record_code_review(
                study_dir=root,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                command=command,
                review_path=review_path,
                code_path=code_path,
                code_sha256=code_sha,
                static_check_path=static_path_for_state,
                static_check_sha256=static_check_sha,
                input_fingerprint_payload=fingerprint,
            )
        except Exception:
            if not self._graph_code_review_matches_review_path(root, run_id=run_id, dataset=target, review_path=review_path):
                review_path.unlink(missing_ok=True)
            raise
        return GraphGatewayCodeReviewResult(
            graph_state=result.graph_state,
            workflow_projection=result.workflow_projection,
            decision=normalized_decision,
            review_path=str(review_path.as_posix()),
            approved=normalized_decision == "approve",
            static_check_path=str(static_check_path.as_posix()) if static_check_path.exists() else None,
        )

    def review_code_from_command(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        command: HumanCommand,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayCodeReviewResult:
        """Bridge a graph-native code-review command to the gateway artifact flow."""

        root = Path(study_dir).expanduser()
        if command.dataset is None:
            raise ValueError("Code review command must include a dataset.")
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        open_study_interrupt = _open_study_interrupt(graph_state)
        if open_study_interrupt is not None:
            raise ValueError(
                f"Study-level interrupt {open_study_interrupt.name} must be resolved before dataset code_review."
            )
        _assert_resume_command_matches_open_interrupt(graph_state, command)
        if command.interrupt != "code_review":
            raise ValueError("Code review command must target code_review.")
        if command.action not in {"approve", "reject"}:
            raise ValueError("Code review command action must be approve or reject.")
        return self.review_code(
            study_dir=root,
            study_id=graph_state.study_id,
            run_id=run_id,
            dataset=command.dataset,
            decision=command.action,
            reviewer=command.reviewer,
            notes=command.notes,
            input_fingerprint_payload=input_fingerprint_payload,
        )

    def start_native_code_review(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        llm_provider: dict[str, Any],
        llm_exposure: dict[str, Any],
        llm_client_builder: Any | None = None,
        target_context_builder: Any | None = None,
        rscript_path: str | None = None,
    ) -> GraphGatewayCodeGenerationResult:
        """Start an internal DatasetGraph native code-review interrupt pilot."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        plan = self.dependency_gate_for_product_step(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
        )
        dependency_resolution = list(plan.dependency_resolution)
        dependency_artifacts = _dependency_artifacts_for_dataset(dependency_resolution, target)
        self.validate_product_step_start(study_dir=root, run_id=run_id, dataset=target, step="generate_code")
        dataset_graph = compile_dataset_graph(checkpointer=self._checkpointer)
        result = dataset_graph.invoke(
            {
                "study_id": study_id,
                "run_id": run_id,
                "dataset": target,
                "execution_mode": GRAPH_PRODUCT_GENERATE_CODE_MODE,
                "study_dir": str(root),
                "rscript_path": rscript_path or "",
                "dependency_resolution": dependency_resolution,
                "llm_provider": llm_provider,
                "llm_exposure": llm_exposure,
                "llm_client_builder": llm_client_builder,
                "target_context_builder": target_context_builder,
                "native_code_review": True,
                "audit_artifacts": [],
            },
            config=self._dataset_config(study_id, run_id, target),
        )
        if "__interrupt__" not in result:
            raise ValueError(f"DatasetGraph did not stop at native code_review for {target}.")
        snapshot = dataset_graph.get_state(self._dataset_config(study_id, run_id, target))
        gateway_result = self._record_code_generation_from_dataset_result(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            result=snapshot.values,
            dependency_artifacts=dependency_artifacts,
            llm_provider=llm_provider,
            gate=plan,
            runtime_persistence_extra={
                "native_code_review_interrupt": _native_interrupt_payload(
                    snapshot,
                    boundary="code_review_pilot_only",
                )
            },
        )
        return gateway_result

    def resume_native_code_review(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        dataset: str,
        decision: str,
        reviewer: str,
        notes: str = "",
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayCodeReviewResult:
        """Resume the native code-review pilot through the formal review artifact flow."""

        root = Path(study_dir).expanduser()
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        target = dataset.strip().upper()
        normalized_decision = decision.strip().lower()
        if normalized_decision not in {"approve", "reject"}:
            raise ValueError("Code review decision must be approve or reject.")
        _assert_resume_command_matches_open_interrupt(
            graph_state,
            HumanCommand(
                interrupt="code_review",
                action="approve" if normalized_decision == "approve" else "reject",
                dataset=target,
                reviewer=reviewer,
                notes=notes,
            ),
        )
        dataset_graph = compile_dataset_graph(checkpointer=self._checkpointer)
        resumed = dataset_graph.invoke(
            Command(
                resume={
                    "action": normalized_decision,
                    "reviewer": reviewer,
                    "notes": notes,
                }
            ),
            config=self._dataset_config(graph_state.study_id, run_id, target),
        )
        expected_native_status = "approved" if normalized_decision == "approve" else "rejected"
        if resumed.get("native_code_review_status") != expected_native_status:
            raise ValueError(f"DatasetGraph did not resume native code_review for {target}.")
        commands = resumed.get("human_commands") or []
        if not commands:
            raise ValueError(f"DatasetGraph native code_review resume did not produce a human command for {target}.")
        command_payload = dict(commands[-1])
        result = self.review_code_from_command(
            study_dir=root,
            run_id=run_id,
            command=HumanCommand(
                interrupt=command_payload.get("interrupt", "code_review"),
                action=command_payload.get("action", normalized_decision),
                dataset=command_payload.get("dataset", target),
                reviewer=command_payload.get("reviewer", reviewer),
                notes=command_payload.get("notes", notes),
                payload=command_payload.get("payload") if isinstance(command_payload.get("payload"), dict) else {},
            ),
            input_fingerprint_payload=input_fingerprint_payload,
        )
        self._persist_graph_state(
            root,
            result.graph_state,
            node="native_code_review_resume",
            runtime_persistence_extra={
                "native_code_review_resume": {
                    "resumed": True,
                    "dataset": target,
                    "action": normalized_decision,
                    "native_status": resumed.get("native_code_review_status"),
                }
            },
        )
        projection = project_graph_state_to_workflow(
            root,
            result.graph_state,
            node="graph_gateway_native_code_review_resume",
        )
        return GraphGatewayCodeReviewResult(
            graph_state=result.graph_state,
            workflow_projection=projection,
            decision=result.decision,
            review_path=result.review_path,
            approved=result.approved,
            static_check_path=result.static_check_path,
        )

    def record_code_review(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        command: HumanCommand,
        review_path: str | Path,
        code_path: str | Path,
        code_sha256: str,
        static_check_path: str | Path | None = None,
        static_check_sha256: str | None = None,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayResult:
        """Persist a dataset code-review decision into canonical graph state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        self.validate_code_review(
            study_dir=root,
            run_id=run_id,
            dataset=target,
            code_sha256=code_sha256,
            static_check_sha256=static_check_sha256,
            spec_sha256=command.payload.get("spec_sha256"),
            input_fingerprint_payload=fingerprint,
        )
        try:
            next_state = self.load_graph_state(study_dir=root, run_id=run_id).model_copy(deep=True)
        except FileNotFoundError as exc:
            raise ValueError("Generated code must be recorded in graph state before code review.") from exc
        next_state.input_fingerprint = fingerprint
        if target not in next_state.target_datasets:
            next_state.target_datasets.append(target)
        if target not in next_state.runnable_datasets:
            next_state.runnable_datasets.append(target)
        dataset_state = next_state.datasets.get(target)
        dataset_state.input_fingerprint = fingerprint
        dataset_state.human_commands.append(command)
        dataset_state.code_state.update(
            {
                "status": "approved" if command.action == "approve" else "rejected",
                "decision": command.action,
                "reviewer": command.reviewer,
                "notes": command.notes,
                "review_path": str(Path(review_path).as_posix()),
                "code_path": str(Path(code_path).as_posix()),
                "code_sha256": code_sha256,
                "static_check_path": str(Path(static_check_path).as_posix()) if static_check_path else None,
                "static_check_sha256": static_check_sha256,
                "input_fingerprint": fingerprint,
            }
        )
        dataset_state.current_interrupt = None if command.action == "approve" else InterruptState(
            name="code_review",
            dataset=target,
            reason="Generated code was rejected and requires revision before execution.",
            payload={"review_path": str(Path(review_path).as_posix())},
        )
        dataset_state.status = "pending" if command.action == "approve" else "needs_review"
        dataset_state.updated_at = utc_now()
        _upsert_artifact(dataset_state, _artifact_ref(target, "code_review", "audit", review_path, kind="tool_log"))
        _upsert_artifact(dataset_state, _artifact_ref(target, "generated_code", "output", code_path, kind="generated_code"))
        if static_check_path:
            _upsert_artifact(dataset_state, _artifact_ref(target, "static_check", "audit", static_check_path, kind="tool_log"))
        next_state.datasets[target] = dataset_state
        next_state.human_commands.append(command)
        _roll_up_study_state(next_state, preferred_interrupt=dataset_state.current_interrupt)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="code_review")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_code_review")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def record_draft_spec_generation(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        draft_spec_path: str | Path,
        prompt_path: str | Path | None = None,
        response_path: str | Path | None = None,
        variables: list[dict[str, Any]] | None = None,
        warnings: list[str] | None = None,
        input_fingerprint_payload: dict[str, Any] | None = None,
        agent_decisions: list[dict[str, Any]] | None = None,
        agent_node_inputs: list[dict[str, Any]] | None = None,
        agent_node_outputs: list[dict[str, Any]] | None = None,
        risk_flags: list[str] | None = None,
    ) -> GraphGatewayResult:
        """Persist a generated draft spec and its review interrupt."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        draft_path = Path(draft_spec_path)
        if not draft_path.exists() or not draft_path.is_file():
            raise ValueError(f"Draft spec does not exist: {draft_path}")
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        draft_sha = f"sha256:{sha256_file(draft_path)}"
        next_state = self._load_or_create_state(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            input_fingerprint_payload=fingerprint,
        )
        dataset_state = self._dataset_state(next_state, target=target, fingerprint=fingerprint)
        terminal_followup = _assert_terminal_failure_step_allowed(dataset_state, step="draft_spec")
        if terminal_followup is not None:
            dataset_state.execution_state["terminal_failure_followup_consumed_by"] = "draft_spec"
            dataset_state.execution_state.pop("terminal_failure_review", None)
            dataset_state.execution_state.pop("terminal_failure_followup", None)
            dataset_state.execution_state.pop("next_action", None)
        dataset_state.spec_state.update(
            {
                "status": "draft_generated",
                "spec_source": "draft_spec",
                "draft_spec_path": str(draft_path.as_posix()),
                "draft_spec_sha256": draft_sha,
                "prompt_path": str(Path(prompt_path).as_posix()) if prompt_path else None,
                "response_path": str(Path(response_path).as_posix()) if response_path else None,
                "variables": variables or [],
                "warnings": warnings or [],
                "input_fingerprint": fingerprint,
                "terminal_failure_followup": terminal_followup,
            }
        )
        dataset_state.current_interrupt = InterruptState(
            name="draft_spec_review",
            dataset=target,
            reason="Generated draft spec must be reviewed before R code generation.",
            payload={"draft_spec_path": str(draft_path.as_posix())},
        )
        dataset_state.status = "needs_review"
        dataset_state.updated_at = utc_now()
        _upsert_artifact(dataset_state, _artifact_ref(target, "draft_spec", "intermediate", draft_path, kind="draft_spec"))
        if prompt_path:
            _upsert_artifact(dataset_state, _artifact_ref(target, "draft_spec_prompt", "audit", prompt_path, kind="llm_prompt"))
        if response_path:
            _upsert_artifact(dataset_state, _artifact_ref(target, "draft_spec_response", "audit", response_path, kind="llm_response"))
        if not agent_decisions and not agent_node_inputs and not agent_node_outputs:
            agent_decisions, agent_node_inputs, agent_node_outputs = _default_draft_spec_agent_io(
                study_id=study_id,
                run_id=run_id,
                target=target,
                draft_path=draft_path,
                prompt_path=Path(prompt_path) if prompt_path else None,
                response_path=Path(response_path) if response_path else None,
                variable_count=len(variables or []),
            )
        _append_agent_decisions(
            dataset_state,
            agent_decisions
            or [
                record_agent_decision(
                    agent="spec_agent",
                    node="draft_spec_generation",
                    decision="draft_spec_generated",
                    dataset=target,
                    status="needs_review",
                    reason="Generated draft spec was recorded and routed to human review.",
                    outputs={
                        "record_source": "graph_gateway_default",
                        "draft_spec_path": str(draft_path.as_posix()),
                        "variable_count": len(variables or []),
                    },
                    risk_flags=["draft_spec_requires_human_review"],
                )
            ],
        )
        _append_agent_node_io(dataset_state, inputs=agent_node_inputs or [], outputs=agent_node_outputs or [])
        _append_risk_flags(dataset_state, risk_flags or ["draft_spec_requires_human_review"])
        next_state.datasets[target] = dataset_state
        _roll_up_study_state(next_state, preferred_interrupt=dataset_state.current_interrupt)
        _sync_study_agent_decisions(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="draft_spec_generation")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_draft_spec_generation")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def record_input_spec_ready(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        input_spec_path: str | Path,
        input_fingerprint_payload: dict[str, Any] | None = None,
        agent_decisions: list[dict[str, Any]] | None = None,
        agent_node_inputs: list[dict[str, Any]] | None = None,
        agent_node_outputs: list[dict[str, Any]] | None = None,
        risk_flags: list[str] | None = None,
    ) -> GraphGatewayResult:
        """Persist an available input spec into canonical graph state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        spec_path = Path(input_spec_path)
        if not spec_path.exists() or not spec_path.is_file():
            raise ValueError(f"Input spec does not exist: {spec_path}")
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        next_state = self._load_or_create_state(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            input_fingerprint_payload=fingerprint,
        )
        dataset_state = self._dataset_state(next_state, target=target, fingerprint=fingerprint)
        terminal_followup = _assert_terminal_failure_step_allowed(dataset_state, step="finalize_inputs")
        if terminal_followup is not None:
            dataset_state.execution_state["terminal_failure_followup_consumed_by"] = "finalize_inputs"
            dataset_state.execution_state.pop("terminal_failure_review", None)
            dataset_state.execution_state.pop("terminal_failure_followup", None)
            dataset_state.execution_state.pop("next_action", None)
        dataset_state.spec_state.update(
            {
                "status": "input_spec_ready",
                "spec_source": "input_spec",
                "input_spec_path": str(spec_path.as_posix()),
                "input_spec_sha256": f"sha256:{sha256_file(spec_path)}",
                "input_fingerprint": fingerprint,
                "terminal_failure_followup": terminal_followup,
            }
        )
        dataset_state.current_interrupt = None
        dataset_state.status = "pending"
        dataset_state.updated_at = utc_now()
        _upsert_artifact(dataset_state, _artifact_ref(target, "input_spec", "source", spec_path, kind="input_spec"))
        if not agent_decisions and not agent_node_inputs and not agent_node_outputs:
            agent_decisions, agent_node_inputs, agent_node_outputs = _default_input_spec_agent_io(
                study_id=study_id,
                run_id=run_id,
                target=target,
                spec_path=spec_path,
            )
        _append_agent_decisions(
            dataset_state,
            agent_decisions
            or [
                record_agent_decision(
                    agent="evidence_agent",
                    node="input_spec_ready",
                    decision="input_spec_ready",
                    dataset=target,
                    status="pending",
                    reason="User-supplied input_spec was accepted as the authoritative spec source.",
                    outputs={
                        "record_source": "graph_gateway_default",
                        "input_spec_path": str(spec_path.as_posix()),
                        "next_action": "generate_code",
                    },
                )
            ],
        )
        _append_agent_node_io(dataset_state, inputs=agent_node_inputs or [], outputs=agent_node_outputs or [])
        _append_risk_flags(dataset_state, risk_flags or [])
        next_state.datasets[target] = dataset_state
        _roll_up_study_state(next_state)
        _sync_study_agent_decisions(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="input_spec_ready")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_input_spec_ready")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def record_approved_draft_spec_ready(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        approved_spec_path: str | Path,
        input_fingerprint_payload: dict[str, Any] | None = None,
        agent_decisions: list[dict[str, Any]] | None = None,
        agent_node_inputs: list[dict[str, Any]] | None = None,
        agent_node_outputs: list[dict[str, Any]] | None = None,
        risk_flags: list[str] | None = None,
    ) -> GraphGatewayResult:
        """Mark an already approved draft spec as ready after a terminal-failure follow-up."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        spec_path = Path(approved_spec_path)
        if not spec_path.exists() or not spec_path.is_file():
            raise ValueError(f"Approved draft spec does not exist: {spec_path}")
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        next_state = self._load_or_create_state(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            input_fingerprint_payload=fingerprint,
        )
        dataset_state = self._dataset_state(next_state, target=target, fingerprint=fingerprint)
        terminal_followup = _assert_terminal_failure_step_allowed(dataset_state, step="finalize_inputs")
        _assert_approved_draft_spec_current(
            dataset_state,
            spec_path=spec_path,
            spec_sha256=f"sha256:{sha256_file(spec_path)}",
            input_fingerprint_payload=fingerprint,
        )
        if terminal_followup is not None:
            dataset_state.execution_state["terminal_failure_followup_consumed_by"] = "finalize_inputs"
            dataset_state.execution_state.pop("terminal_failure_review", None)
            dataset_state.execution_state.pop("terminal_failure_followup", None)
            dataset_state.execution_state.pop("next_action", None)
        dataset_state.spec_state["terminal_failure_followup"] = terminal_followup
        dataset_state.current_interrupt = None
        dataset_state.status = "pending"
        dataset_state.updated_at = utc_now()
        _upsert_artifact(
            dataset_state,
            _artifact_ref(target, "approved_draft_spec", "source", spec_path, kind="input_spec"),
        )
        if not agent_decisions and not agent_node_inputs and not agent_node_outputs:
            agent_decisions, agent_node_inputs, agent_node_outputs = _default_approved_draft_spec_agent_io(
                study_id=study_id,
                run_id=run_id,
                target=target,
                approved_spec_path=spec_path,
            )
        _append_agent_decisions(
            dataset_state,
            agent_decisions
            or [
                record_agent_decision(
                    agent="evidence_agent",
                    node="approved_draft_spec_ready",
                    decision="approved_draft_spec_ready",
                    dataset=target,
                    status="pending",
                    reason="Current graph-approved draft spec was accepted as the code-generation spec source.",
                    outputs={
                        "record_source": "graph_gateway_default",
                        "approved_spec_path": str(spec_path.as_posix()),
                        "next_action": "generate_code",
                    },
                    risk_flags=["uses_approved_draft_spec"],
                )
            ],
        )
        _append_agent_node_io(dataset_state, inputs=agent_node_inputs or [], outputs=agent_node_outputs or [])
        _append_risk_flags(dataset_state, risk_flags or ["uses_approved_draft_spec"])
        next_state.datasets[target] = dataset_state
        _roll_up_study_state(next_state)
        _sync_study_agent_decisions(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="approved_draft_spec_ready")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_approved_draft_spec_ready")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def finalize_inputs(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        llm_provider: dict[str, Any],
        llm_exposure: dict[str, Any],
        llm_client_builder: Any | None = None,
        target_context_builder: Any | None = None,
        rscript_path: str | None = None,
        force_new_draft_spec: bool = False,
    ) -> GraphGatewayFinalizeInputsResult:
        """Prepare product context and persist graph-owned spec readiness state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        plan = self.dependency_gate_for_product_step(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
        )
        dependency_resolution = list(plan.dependency_resolution)
        self.validate_product_step_start(study_dir=root, run_id=run_id, dataset=target, step="finalize_inputs")
        result = compile_dataset_graph().invoke(
            {
                "study_id": study_id,
                "run_id": run_id,
                "dataset": target,
                "execution_mode": GRAPH_PRODUCT_PREPARE_MODE,
                "study_dir": str(root),
                "rscript_path": rscript_path or "",
                "dependency_resolution": dependency_resolution,
                "llm_provider": llm_provider,
                "llm_exposure": llm_exposure,
                "llm_client_builder": llm_client_builder,
                "target_context_builder": target_context_builder,
                "force_new_draft_spec": force_new_draft_spec,
                "audit_artifacts": [],
            }
        )
        if result.get("status") == "failed":
            raise ValueError(str(result.get("real_run_error") or f"Could not finalize inputs for {target}."))
        warnings = list(result.get("product_context_warnings", []))
        spec_source = str(result.get("spec_source") or "")
        if force_new_draft_spec and spec_source == "input_spec":
            raise ValueError(f"An input_spec already exists for {target}; draft spec generation is not needed.")
        fingerprint = input_fingerprint(root)
        if spec_source == "input_spec":
            input_spec_path = result.get("input_spec_path")
            if not input_spec_path:
                raise ValueError(f"Input spec detection did not return a path for {target}.")
            gateway_result = self.record_input_spec_ready(
                study_dir=root,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                input_spec_path=input_spec_path,
                input_fingerprint_payload=fingerprint,
                agent_decisions=list(result.get("agent_decisions", [])),
                agent_node_inputs=list(result.get("agent_node_inputs", [])),
                agent_node_outputs=list(result.get("agent_node_outputs", [])),
                risk_flags=list(result.get("risk_flags", [])),
            )
            projection = self._handoff_dependency_review_to_product_step(
                root=root,
                state=gateway_result.graph_state,
                gate=plan,
            )
            return GraphGatewayFinalizeInputsResult(
                graph_state=gateway_result.graph_state,
                workflow_projection=projection,
                spec_source=spec_source,
                warnings=warnings,
                dependency_review_status=gateway_result.graph_state.dependency_review_status,
                dependency_warnings=list(plan.dependency_warnings),
                input_spec_path=str(input_spec_path),
            )
        if spec_source == "approved_draft_spec":
            approved_spec_path = result.get("approved_spec_path")
            if not approved_spec_path:
                raise ValueError(f"Approved draft spec detection did not return a path for {target}.")
            gateway_result = self.record_approved_draft_spec_ready(
                study_dir=root,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                approved_spec_path=approved_spec_path,
                input_fingerprint_payload=fingerprint,
                agent_decisions=list(result.get("agent_decisions", [])),
                agent_node_inputs=list(result.get("agent_node_inputs", [])),
                agent_node_outputs=list(result.get("agent_node_outputs", [])),
                risk_flags=list(result.get("risk_flags", [])),
            )
            projection = self._handoff_dependency_review_to_product_step(
                root=root,
                state=gateway_result.graph_state,
                gate=plan,
            )
            return GraphGatewayFinalizeInputsResult(
                graph_state=gateway_result.graph_state,
                workflow_projection=projection,
                spec_source=spec_source,
                warnings=warnings,
                dependency_review_status=gateway_result.graph_state.dependency_review_status,
                dependency_warnings=list(plan.dependency_warnings),
                approved_spec_path=str(approved_spec_path),
            )

        draft_path = result.get("draft_spec_path")
        if not draft_path:
            raise ValueError(f"Draft spec generation did not produce a reviewable spec for {target}.")
        return self._record_draft_spec_generation_from_dataset_result(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            result=result,
            gate=plan,
        )

    def generate_draft_spec(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        llm_provider: dict[str, Any],
        llm_exposure: dict[str, Any],
        llm_client_builder: Any | None = None,
        target_context_builder: Any | None = None,
        rscript_path: str | None = None,
    ) -> GraphGatewayFinalizeInputsResult:
        """Generate a fresh review-required draft spec through DatasetGraph."""

        return self.finalize_inputs(
            study_dir=study_dir,
            study_id=study_id,
            run_id=run_id,
            dataset=dataset,
            llm_provider=llm_provider,
            llm_exposure=llm_exposure,
            llm_client_builder=llm_client_builder,
            target_context_builder=target_context_builder,
            rscript_path=rscript_path,
            force_new_draft_spec=True,
        )

    def start_native_draft_spec_review(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        llm_provider: dict[str, Any],
        llm_exposure: dict[str, Any],
        llm_client_builder: Any | None = None,
        target_context_builder: Any | None = None,
        rscript_path: str | None = None,
        force_new_draft_spec: bool = False,
    ) -> GraphGatewayFinalizeInputsResult:
        """Start an internal DatasetGraph native draft-spec review interrupt pilot."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        plan = self.dependency_gate_for_product_step(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
        )
        dependency_resolution = list(plan.dependency_resolution)
        self.validate_product_step_start(study_dir=root, run_id=run_id, dataset=target, step="finalize_inputs")
        dataset_graph = compile_dataset_graph(checkpointer=self._checkpointer)
        result = dataset_graph.invoke(
            {
                "study_id": study_id,
                "run_id": run_id,
                "dataset": target,
                "execution_mode": GRAPH_PRODUCT_PREPARE_MODE,
                "study_dir": str(root),
                "rscript_path": rscript_path or "",
                "dependency_resolution": dependency_resolution,
                "llm_provider": llm_provider,
                "llm_exposure": llm_exposure,
                "llm_client_builder": llm_client_builder,
                "target_context_builder": target_context_builder,
                "force_new_draft_spec": force_new_draft_spec,
                "native_draft_spec_review": True,
                "audit_artifacts": [],
            },
            config=self._dataset_config(study_id, run_id, target),
        )
        if "__interrupt__" not in result:
            raise ValueError(f"DatasetGraph did not stop at native draft_spec_review for {target}.")
        snapshot = dataset_graph.get_state(self._dataset_config(study_id, run_id, target))
        gateway_result = self._record_draft_spec_generation_from_dataset_result(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            result=snapshot.values,
            gate=plan,
            runtime_persistence_extra={
                "native_draft_spec_review_interrupt": _native_interrupt_payload(
                    snapshot,
                    boundary="draft_spec_review_pilot_only",
                )
            },
        )
        return gateway_result

    def resume_native_draft_spec_review(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        dataset: str,
        decision: str,
        reviewer: str,
        notes: str = "",
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayDraftSpecReviewResult:
        """Resume the native draft-spec pilot through the formal review artifact flow."""

        root = Path(study_dir).expanduser()
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        target = dataset.strip().upper()
        normalized_decision = decision.strip().lower()
        if normalized_decision not in {"approve", "reject"}:
            raise ValueError("Draft spec review decision must be approve or reject.")
        open_study_interrupt = _open_study_interrupt(graph_state)
        if open_study_interrupt is not None:
            raise ValueError(
                f"Study-level interrupt {open_study_interrupt.name} must be resolved before dataset draft_spec_review."
            )
        _assert_resume_command_matches_open_interrupt(
            graph_state,
            HumanCommand(
                interrupt="draft_spec_review",
                action="approve" if normalized_decision == "approve" else "reject",
                dataset=target,
                reviewer=reviewer,
                notes=notes,
            ),
        )
        dataset_graph = compile_dataset_graph(checkpointer=self._checkpointer)
        resumed = dataset_graph.invoke(
            Command(
                resume={
                    "action": normalized_decision,
                    "reviewer": reviewer,
                    "notes": notes,
                }
            ),
            config=self._dataset_config(graph_state.study_id, run_id, target),
        )
        expected_native_status = "approved" if normalized_decision == "approve" else "rejected"
        if resumed.get("native_draft_spec_review_status") != expected_native_status:
            raise ValueError(f"DatasetGraph did not resume native draft_spec_review for {target}.")
        commands = resumed.get("human_commands") or []
        if not commands:
            raise ValueError(f"DatasetGraph native draft_spec_review resume did not produce a human command for {target}.")
        command_payload = dict(commands[-1])
        result = self.review_draft_spec_from_command(
            study_dir=root,
            run_id=run_id,
            command=HumanCommand(
                interrupt=command_payload.get("interrupt", "draft_spec_review"),
                action=command_payload.get("action", normalized_decision),
                dataset=command_payload.get("dataset", target),
                reviewer=command_payload.get("reviewer", reviewer),
                notes=command_payload.get("notes", notes),
                payload=command_payload.get("payload") if isinstance(command_payload.get("payload"), dict) else {},
            ),
            input_fingerprint_payload=input_fingerprint_payload,
        )
        self._persist_graph_state(
            root,
            result.graph_state,
            node="native_draft_spec_review_resume",
            runtime_persistence_extra={
                "native_draft_spec_review_resume": {
                    "resumed": True,
                    "dataset": target,
                    "action": normalized_decision,
                    "native_status": resumed.get("native_draft_spec_review_status"),
                }
            },
        )
        projection = project_graph_state_to_workflow(
            root,
            result.graph_state,
            node="graph_gateway_native_draft_spec_review_resume",
        )
        return GraphGatewayDraftSpecReviewResult(
            graph_state=result.graph_state,
            workflow_projection=projection,
            decision=result.decision,
            review_path=result.review_path,
            approved=result.approved,
            approved_spec_path=result.approved_spec_path,
        )

    def dependency_gate_for_product_step(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        start_if_missing: bool = True,
    ) -> GraphGatewayDependencyGateResult:
        """Return a graph-owned dependency gate and fail closed if it is not open."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        try:
            graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        except FileNotFoundError:
            if not start_if_missing:
                raise
            graph_state = self.start_dependency_plan(
                study_dir=root,
                study_id=study_id,
                run_id=run_id,
                target_datasets=[target],
            ).graph_state
        if graph_state.study_id != study_id:
            raise ValueError("Graph state study_id does not match the request.")
        if target not in [item.strip().upper() for item in graph_state.target_datasets]:
            raise ValueError(f"{target} is not part of the current graph dependency plan.")
        result = _dependency_gate_result(root, graph_state)
        _assert_dependency_gate_open(result, target)
        return result

    def _record_draft_spec_generation_from_dataset_result(
        self,
        *,
        root: Path,
        study_id: str,
        run_id: str,
        target: str,
        result: dict[str, Any],
        gate: GraphGatewayDependencyGateResult,
        runtime_persistence_extra: dict[str, Any] | None = None,
    ) -> GraphGatewayFinalizeInputsResult:
        """Persist a DatasetGraph draft-spec result through the canonical gateway path."""

        if result.get("status") == "failed":
            raise ValueError(str(result.get("real_run_error") or f"Could not generate draft spec for {target}."))
        spec_source = str(result.get("spec_source") or "")
        draft_path = result.get("draft_spec_path")
        if not draft_path:
            raise ValueError(f"Draft spec generation did not produce a reviewable spec for {target}.")
        draft_variables = list(result.get("draft_spec_variables", []))
        warnings = list(result.get("product_context_warnings", []))
        prompt_path = result.get("draft_spec_prompt_path") or ""
        response_path = result.get("draft_spec_response_path") or ""
        gateway_result = self.record_draft_spec_generation(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            draft_spec_path=draft_path,
            prompt_path=prompt_path,
            response_path=response_path,
            variables=draft_variables,
            warnings=warnings,
            input_fingerprint_payload=input_fingerprint(root),
            agent_decisions=list(result.get("agent_decisions", [])),
            agent_node_inputs=list(result.get("agent_node_inputs", [])),
            agent_node_outputs=list(result.get("agent_node_outputs", [])),
            risk_flags=list(result.get("risk_flags", [])),
        )
        projection = self._handoff_dependency_review_to_product_step(
            root=root,
            state=gateway_result.graph_state,
            gate=gate,
            preferred_interrupt=gateway_result.graph_state.datasets[target].current_interrupt,
        )
        if runtime_persistence_extra:
            self._persist_graph_state(
                root,
                gateway_result.graph_state,
                node="native_draft_spec_review_interrupt",
                runtime_persistence_extra=runtime_persistence_extra,
            )
            projection = project_graph_state_to_workflow(
                root,
                gateway_result.graph_state,
                node="graph_gateway_native_draft_spec_review_interrupt",
            )
        return GraphGatewayFinalizeInputsResult(
            graph_state=gateway_result.graph_state,
            workflow_projection=projection,
            spec_source=spec_source or "draft_spec",
            warnings=warnings,
            dependency_review_status=gateway_result.graph_state.dependency_review_status,
            dependency_warnings=list(gate.dependency_warnings),
            draft_spec_path=str(draft_path),
            draft_spec_prompt_path=str(prompt_path),
            draft_spec_response_path=str(response_path),
            draft_spec_variables=draft_variables,
        )

    def review_draft_spec_from_command(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        command: HumanCommand,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayDraftSpecReviewResult:
        """Bridge a graph-native draft-spec command to the gateway artifact flow."""

        root = Path(study_dir).expanduser()
        if command.dataset is None:
            raise ValueError("Draft spec review command must include a dataset.")
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        open_study_interrupt = _open_study_interrupt(graph_state)
        if open_study_interrupt is not None:
            raise ValueError(
                f"Study-level interrupt {open_study_interrupt.name} must be resolved before dataset draft_spec_review."
            )
        _assert_resume_command_matches_open_interrupt(graph_state, command)
        if command.interrupt != "draft_spec_review":
            raise ValueError("Draft spec review command must target draft_spec_review.")
        if command.action not in {"approve", "reject"}:
            raise ValueError("Draft spec review command action must be approve or reject.")
        return self.review_draft_spec(
            study_dir=root,
            study_id=graph_state.study_id,
            run_id=run_id,
            dataset=command.dataset,
            decision=command.action,
            reviewer=command.reviewer,
            notes=command.notes,
            input_fingerprint_payload=input_fingerprint_payload,
        )

    def record_draft_spec_review(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        command: HumanCommand,
        review_path: str | Path,
        draft_spec_path: str | Path,
        approved_spec_path: str | Path | None = None,
        approved_spec_sha256: str | None = None,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayResult:
        """Persist a draft-spec review decision into canonical graph state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        if command.action == "approve" and (not approved_spec_path or not approved_spec_sha256):
            raise ValueError("Approved draft spec approval requires an approved spec artifact and hash.")
        self.validate_draft_spec_review(
            study_dir=root,
            run_id=run_id,
            dataset=target,
            draft_spec_path=draft_spec_path,
            approved_spec_path=approved_spec_path,
            approved_spec_sha256=approved_spec_sha256,
            input_fingerprint_payload=fingerprint,
        )
        try:
            next_state = self.load_graph_state(study_dir=root, run_id=run_id).model_copy(deep=True)
        except FileNotFoundError as exc:
            raise ValueError("Draft spec must be recorded in graph state before review.") from exc
        next_state.input_fingerprint = fingerprint
        if target not in next_state.target_datasets:
            next_state.target_datasets.append(target)
        if target not in next_state.runnable_datasets:
            next_state.runnable_datasets.append(target)
        dataset_state = self._dataset_state(next_state, target=target, fingerprint=fingerprint)
        draft_path = Path(draft_spec_path)
        review = Path(review_path)
        dataset_state.human_commands.append(command)
        dataset_state.spec_state.update(
            {
                "status": "approved" if command.action == "approve" else "rejected",
                "decision": command.action,
                "reviewer": command.reviewer,
                "notes": command.notes,
                "review_path": str(review.as_posix()),
                "draft_spec_path": str(draft_path.as_posix()),
                "draft_spec_sha256": f"sha256:{sha256_file(draft_path)}",
                "approved_spec_path": str(Path(approved_spec_path).as_posix()) if approved_spec_path else None,
                "approved_spec_sha256": approved_spec_sha256,
                "input_fingerprint": fingerprint,
            }
        )
        dataset_state.current_interrupt = None if command.action == "approve" else InterruptState(
            name="draft_spec_review",
            dataset=target,
            reason="Generated draft spec was rejected and requires revision before R code generation.",
            payload={"review_path": str(review.as_posix())},
        )
        dataset_state.status = "pending" if command.action == "approve" else "needs_review"
        dataset_state.updated_at = utc_now()
        _upsert_artifact(dataset_state, _artifact_ref(target, "draft_spec_review", "audit", review, kind="tool_log"))
        _upsert_artifact(dataset_state, _artifact_ref(target, "draft_spec", "intermediate", draft_path, kind="draft_spec"))
        if approved_spec_path:
            _upsert_artifact(
                dataset_state,
                _artifact_ref(target, "approved_spec", "intermediate", approved_spec_path, kind="approved_spec"),
            )
        next_state.datasets[target] = dataset_state
        next_state.human_commands.append(command)
        _roll_up_study_state(next_state, preferred_interrupt=dataset_state.current_interrupt)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="draft_spec_review")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_draft_spec_review")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def review_draft_spec(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        decision: str,
        reviewer: str,
        notes: str = "",
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayDraftSpecReviewResult:
        """Write draft-spec review artifacts and persist the graph-owned decision."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        normalized_decision = decision.strip().lower()
        if normalized_decision not in {"approve", "reject"}:
            raise ValueError("Draft spec review decision must be approve or reject.")
        run_dir = root / "runs" / run_id
        draft_path = run_dir / "specs" / f"{target.lower()}_draft_spec.json"
        if not draft_path.exists() or not draft_path.is_file():
            raise ValueError(f"Draft spec does not exist for review: {draft_path}")
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        self.validate_draft_spec_review(
            study_dir=root,
            run_id=run_id,
            dataset=target,
            draft_spec_path=draft_path,
            input_fingerprint_payload=fingerprint,
        )
        draft_payload = _read_json_if_exists(draft_path)
        approved_path: Path | None = None
        approved_spec_sha: str | None = None
        review_path = run_dir / "reviews" / f"{target.lower()}_draft_spec_review.json"
        try:
            if normalized_decision == "approve":
                approved_path = run_dir / "approved_specs" / f"{target.lower()}_approved_spec.json"
                approved_payload = dict(draft_payload)
                approved_payload["status"] = "approved_draft"
                approved_payload["approved_from_draft_path"] = str(draft_path.as_posix())
                approved_payload["approved_by"] = reviewer
                approved_payload["approved_at"] = datetime.now(UTC).isoformat(timespec="seconds").replace("+00:00", "Z")
                approved_payload["approval_notes"] = notes
                approved_payload["input_fingerprint"] = fingerprint
                approved_payload["reference_adam_policy"] = (
                    "Reference ADaM is compare/output-shape evidence only, not derivation authority."
                )
                _write_json(approved_path, approved_payload)
                approved_spec_sha = f"sha256:{sha256_file(approved_path)}"
            review_payload = {
                "study_id": study_id,
                "run_id": run_id,
                "dataset": target,
                "decision": normalized_decision,
                "approved": normalized_decision == "approve",
                "reviewer": reviewer,
                "notes": notes,
                "draft_spec_path": str(draft_path.as_posix()),
                "approved_spec_path": str(approved_path.as_posix()) if approved_path else None,
                "approved_spec_sha256": approved_spec_sha,
                "input_fingerprint": fingerprint,
                "created_at": datetime.now(UTC).isoformat(timespec="seconds").replace("+00:00", "Z"),
            }
            _write_json(review_path, review_payload)
        except Exception:
            review_path.unlink(missing_ok=True)
            if approved_path:
                approved_path.unlink(missing_ok=True)
            raise
        command = HumanCommand(
            interrupt="draft_spec_review",
            action="approve" if normalized_decision == "approve" else "reject",
            dataset=target,
            reviewer=reviewer,
            notes=notes,
            payload={
                "review_path": str(review_path.as_posix()),
                "draft_spec_path": str(draft_path.as_posix()),
                "approved_spec_path": str(approved_path.as_posix()) if approved_path else None,
                "approved_spec_sha256": approved_spec_sha,
            },
        )
        try:
            result = self.record_draft_spec_review(
                study_dir=root,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                command=command,
                review_path=review_path,
                draft_spec_path=draft_path,
                approved_spec_path=approved_path,
                approved_spec_sha256=approved_spec_sha,
                input_fingerprint_payload=fingerprint,
            )
        except Exception:
            if not self._graph_draft_spec_review_matches_review_path(
                root,
                run_id=run_id,
                dataset=target,
                review_path=review_path,
            ):
                review_path.unlink(missing_ok=True)
                if approved_path:
                    approved_path.unlink(missing_ok=True)
            raise
        return GraphGatewayDraftSpecReviewResult(
            graph_state=result.graph_state,
            workflow_projection=result.workflow_projection,
            decision=normalized_decision,
            review_path=str(review_path.as_posix()),
            approved=normalized_decision == "approve",
            approved_spec_path=str(approved_path.as_posix()) if approved_path else None,
        )

    def validate_draft_spec_review(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        dataset: str,
        draft_spec_path: str | Path,
        approved_spec_path: str | Path | None = None,
        approved_spec_sha256: str | None = None,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> None:
        """Fail closed before a draft-spec review artifact is trusted."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        try:
            graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        except FileNotFoundError as exc:
            raise ValueError("Draft spec must be recorded in graph state before review.") from exc
        dataset_state = graph_state.datasets.get(target)
        if dataset_state is None or dataset_state.spec_state.get("status") != "draft_generated":
            raise ValueError("Draft spec must be recorded in graph state before review.")
        recorded_draft_path = Path(str(dataset_state.spec_state.get("draft_spec_path") or ""))
        draft_path = Path(draft_spec_path)
        if str(recorded_draft_path.as_posix()) != str(draft_path.as_posix()):
            raise ValueError("Graph draft spec state points to a different draft spec. Regenerate the draft spec before review.")
        if not draft_path.exists() or not draft_path.is_file():
            raise ValueError(f"Draft spec does not exist: {draft_path}")
        recorded_draft_sha = dataset_state.spec_state.get("draft_spec_sha256")
        if not recorded_draft_sha:
            raise ValueError("Draft-spec graph state is missing the draft spec hash. Regenerate the draft spec before review.")
        current_draft_sha = f"sha256:{sha256_file(draft_path)}"
        if current_draft_sha != recorded_draft_sha:
            raise ValueError("Draft spec changed after graph draft generation. Regenerate the draft spec before review.")
        draft_payload = _read_json_if_exists(draft_path)
        draft_fingerprint = draft_payload.get("input_fingerprint") or {}
        if not draft_fingerprint.get("digest"):
            raise ValueError("Draft spec cannot be approved because it has no input fingerprint. Regenerate the draft spec before approval.")
        if draft_fingerprint.get("digest") != fingerprint.get("digest"):
            diff = compare_fingerprints(draft_fingerprint, fingerprint)
            raise ValueError(
                "Draft spec is stale because study inputs changed after it was generated. "
                "Regenerate and review the draft spec before approval. "
                f"Input diff: added={diff.get('added', [])}; removed={diff.get('removed', [])}; "
                f"changed={diff.get('changed_files', [])}."
            )
        if approved_spec_path:
            approved_path = Path(approved_spec_path)
            if not approved_path.exists() or not approved_path.is_file():
                raise ValueError(f"Approved draft spec does not exist: {approved_path}")
            current_approved_sha = f"sha256:{sha256_file(approved_path)}"
            if not approved_spec_sha256:
                raise ValueError("Approved draft spec approval requires an approved spec hash.")
            if current_approved_sha != approved_spec_sha256:
                raise ValueError("Approved draft spec hash does not match the review payload.")

    def validate_code_review(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        dataset: str,
        code_sha256: str,
        static_check_sha256: str | None = None,
        spec_sha256: str | None = None,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> None:
        """Fail closed before a generated-code review artifact is written."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        try:
            graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        except FileNotFoundError as exc:
            raise ValueError("Generated code must be recorded in graph state before code review.") from exc
        dataset_state = graph_state.datasets.get(target)
        if dataset_state is None or dataset_state.code_state.get("status") != "generated":
            raise ValueError("Generated code must be recorded in graph state before code review.")
        recorded_code_sha = dataset_state.code_state.get("code_sha256")
        if not recorded_code_sha:
            raise ValueError("Generated-code graph state is missing the generated code hash. Regenerate code before review.")
        if recorded_code_sha != code_sha256:
            raise ValueError("Generated code changed after graph code generation. Regenerate code before review.")
        recorded_static_sha = dataset_state.code_state.get("static_check_sha256")
        recorded_static_path = dataset_state.code_state.get("static_check_path")
        if not recorded_static_path or not recorded_static_sha:
            raise ValueError("Generated-code graph state is missing the static-check artifact. Regenerate code before review.")
        if not static_check_sha256:
            raise ValueError("Static-check artifact recorded during code generation is missing. Regenerate code before review.")
        recorded_code_path = Path(str(dataset_state.code_state.get("code_path") or ""))
        resolved_static = Path(str(recorded_static_path))
        if not resolved_static.exists() or not resolved_static.is_file():
            raise ValueError(f"Static-check artifact used during code generation no longer exists: {resolved_static}")
        if f"sha256:{sha256_file(resolved_static)}" != recorded_static_sha:
            raise ValueError("Static-check artifact changed after graph code generation. Regenerate code before review.")
        if static_check_sha256 != recorded_static_sha:
            raise ValueError("Static-check artifact changed after graph code generation. Regenerate code before review.")
        try:
            validate_static_rule_report_artifact(
                resolved_static,
                dataset=target,
                code_path=recorded_code_path,
                code_sha256=recorded_code_sha,
            )
        except StaticRuleError as exc:
            raise ValueError(str(exc)) from exc
        recorded_spec_path = dataset_state.code_state.get("spec_path")
        recorded_spec_sha = dataset_state.code_state.get("spec_sha256")
        if recorded_spec_path or recorded_spec_sha or spec_sha256:
            if not recorded_spec_path or not recorded_spec_sha:
                raise ValueError("Generated-code graph state is missing the approved spec path or hash. Regenerate code before review.")
            resolved_spec = Path(str(recorded_spec_path))
            if not resolved_spec.exists() or not resolved_spec.is_file():
                raise ValueError(f"Approved spec used for code generation no longer exists: {resolved_spec}")
            current_spec_sha = f"sha256:{sha256_file(resolved_spec)}"
            if current_spec_sha != recorded_spec_sha:
                raise ValueError("Approved spec changed after graph code generation. Regenerate code before review.")
            if spec_sha256 and spec_sha256 != recorded_spec_sha:
                raise ValueError("Approved spec hash does not match graph code generation state. Regenerate code before review.")
        _assert_dependency_artifacts_current(
            dataset_state.code_state.get("dependency_artifacts") or [],
            stale_message="Dependency artifact changed after graph code generation. Regenerate code before review.",
        )
        generated_fingerprint = dataset_state.code_state.get("input_fingerprint") or {}
        if not generated_fingerprint.get("digest"):
            raise ValueError("Generated-code graph state is missing its input fingerprint. Regenerate code before review.")
        if generated_fingerprint.get("digest") != fingerprint.get("digest"):
            diff = compare_fingerprints(generated_fingerprint, fingerprint)
            raise ValueError(
                "Study inputs changed after graph code generation. Regenerate code before review. "
                f"Input diff: added={diff.get('added', [])}; removed={diff.get('removed', [])}; "
                f"changed={diff.get('changed_files', [])}."
            )

    def record_code_generation(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        code_path: str | Path,
        code_sha256: str,
        static_check_path: str | Path | None = None,
        static_check_sha256: str | None = None,
        spec_source: str | None = None,
        spec_path: str | Path | None = None,
        spec_sha256: str | None = None,
        dependency_artifacts: list[dict[str, Any]] | None = None,
        generation_quality: dict[str, Any] | None = None,
        input_fingerprint_payload: dict[str, Any] | None = None,
        agent_decisions: list[dict[str, Any]] | None = None,
        agent_node_inputs: list[dict[str, Any]] | None = None,
        agent_node_outputs: list[dict[str, Any]] | None = None,
        risk_flags: list[str] | None = None,
    ) -> GraphGatewayResult:
        """Persist generated-code review interrupt into canonical graph state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        next_state = self._load_or_create_state(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            input_fingerprint_payload=input_fingerprint_payload,
        )
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        dataset_state = self._dataset_state(next_state, target=target, fingerprint=fingerprint)
        terminal_followup = _assert_terminal_failure_step_allowed(dataset_state, step="generate_code")
        if terminal_followup is not None:
            dataset_state.execution_state["terminal_failure_followup_consumed_by"] = "generate_code"
            dataset_state.execution_state.pop("terminal_failure_review", None)
            dataset_state.execution_state.pop("terminal_failure_followup", None)
            dataset_state.execution_state.pop("next_action", None)
        if spec_source == "approved_draft_spec":
            _assert_approved_draft_spec_current(
                dataset_state,
                spec_path=spec_path,
                spec_sha256=spec_sha256,
                input_fingerprint_payload=fingerprint,
            )
        if static_check_path is None or not static_check_sha256:
            raise ValueError("Generated-code graph state requires a static-check artifact and hash before code review.")
        resolved_static_check = Path(static_check_path)
        if not resolved_static_check.exists() or not resolved_static_check.is_file():
            raise ValueError(f"Static-check artifact does not exist: {resolved_static_check}")
        if f"sha256:{sha256_file(resolved_static_check)}" != static_check_sha256:
            raise ValueError("Static-check artifact hash does not match the generated-code payload.")
        try:
            validate_static_rule_report_artifact(
                resolved_static_check,
                dataset=target,
                code_path=Path(code_path),
                code_sha256=code_sha256,
            )
        except StaticRuleError as exc:
            raise ValueError(str(exc)) from exc
        dataset_state.code_state.update(
            {
                "status": "generated",
                "code_path": str(Path(code_path).as_posix()),
                "code_sha256": code_sha256,
                "static_check_path": str(resolved_static_check.as_posix()),
                "static_check_sha256": static_check_sha256,
                "spec_source": spec_source,
                "spec_path": str(Path(spec_path).as_posix()) if spec_path else None,
                "spec_sha256": spec_sha256,
                "dependency_artifacts": dependency_artifacts or [],
                "generation_quality": dict(generation_quality or {}),
                "input_fingerprint": fingerprint,
                "terminal_failure_followup": terminal_followup,
            }
        )
        dataset_state.current_interrupt = InterruptState(
            name="code_review",
            dataset=target,
            reason="Generated R code must be reviewed before local R execution.",
            payload={"code_path": str(Path(code_path).as_posix())},
        )
        dataset_state.status = "needs_review"
        dataset_state.updated_at = utc_now()
        _upsert_artifact(dataset_state, _artifact_ref(target, "generated_code", "output", code_path, kind="generated_code"))
        _upsert_artifact(dataset_state, _artifact_ref(target, "static_check", "audit", resolved_static_check, kind="tool_log"))
        if not agent_decisions and not agent_node_inputs and not agent_node_outputs:
            agent_decisions, agent_node_inputs, agent_node_outputs = _default_code_generation_agent_io(
                study_id=study_id,
                run_id=run_id,
                target=target,
                code_path=code_path,
                static_check_path=static_check_path,
                spec_source=spec_source,
            )
        _append_agent_decisions(
            dataset_state,
            agent_decisions
            or _default_code_generation_agent_decisions(
                target=target,
                code_path=code_path,
                static_check_path=static_check_path,
                spec_source=spec_source,
            ),
        )
        _append_agent_node_io(dataset_state, inputs=agent_node_inputs or [], outputs=agent_node_outputs or [])
        _append_risk_flags(dataset_state, risk_flags or ["static_check_limited_scope"])
        next_state.datasets[target] = dataset_state
        _roll_up_study_state(next_state, preferred_interrupt=dataset_state.current_interrupt)
        _sync_study_agent_decisions(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="code_generation")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_code_generation")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def generate_code(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        llm_provider: dict[str, Any],
        llm_exposure: dict[str, Any],
        llm_client_builder: Any | None = None,
        target_context_builder: Any | None = None,
        rscript_path: str | None = None,
    ) -> GraphGatewayCodeGenerationResult:
        """Generate R code through DatasetGraph and persist the code-review interrupt."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        plan = self.dependency_gate_for_product_step(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
        )
        dependency_resolution = list(plan.dependency_resolution)
        dependency_artifacts = _dependency_artifacts_for_dataset(dependency_resolution, target)
        self.validate_product_step_start(study_dir=root, run_id=run_id, dataset=target, step="generate_code")
        result = compile_dataset_graph().invoke(
            {
                "study_id": study_id,
                "run_id": run_id,
                "dataset": target,
                "execution_mode": GRAPH_PRODUCT_GENERATE_CODE_MODE,
                "study_dir": str(root),
                "rscript_path": rscript_path or "",
                "dependency_resolution": dependency_resolution,
                "llm_provider": llm_provider,
                "llm_exposure": llm_exposure,
                "llm_client_builder": llm_client_builder,
                "target_context_builder": target_context_builder,
                "audit_artifacts": [],
            }
        )
        return self._record_code_generation_from_dataset_result(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            result=result,
            dependency_artifacts=dependency_artifacts,
            llm_provider=llm_provider,
            gate=plan,
        )

    def _record_code_generation_from_dataset_result(
        self,
        *,
        root: Path,
        study_id: str,
        run_id: str,
        target: str,
        result: dict[str, Any],
        dependency_artifacts: list[dict[str, Any]],
        llm_provider: dict[str, Any],
        gate: GraphGatewayDependencyGateResult,
        runtime_persistence_extra: dict[str, Any] | None = None,
    ) -> GraphGatewayCodeGenerationResult:
        """Persist a DatasetGraph code-generation result through the canonical gateway path."""

        if result.get("status") == "failed":
            message = str(result.get("real_run_error") or f"Code generation failed for {target}.")
            if "No approved input_spec or approved draft spec is available" in message:
                message = (
                    f"No approved input_spec or approved draft spec is available for {target}. "
                    "Generate and approve a draft spec before generating R code."
                )
            raise ValueError(message)

        code_path = result.get("code_path")
        generated_code = result.get("generated_code")
        if not code_path or not generated_code:
            raise ValueError(f"Code generation did not produce a reviewable R script for {target}.")
        code_sha = f"sha256:{sha256_file(Path(str(code_path)))}"
        static_check_path = result.get("static_check_path")
        static_check_sha = f"sha256:{sha256_file(Path(str(static_check_path)))}" if static_check_path else None
        spec_path = result.get("approved_spec_path") or result.get("input_spec_path") or result.get("draft_spec_path")
        spec_sha = f"sha256:{sha256_file(Path(str(spec_path)))}" if spec_path else None
        gateway_result = self.record_code_generation(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            code_path=Path(str(code_path)),
            code_sha256=code_sha,
            static_check_path=Path(str(static_check_path)) if static_check_path else None,
            static_check_sha256=static_check_sha,
            spec_source=result.get("spec_source"),
            spec_path=Path(str(spec_path)) if spec_path else None,
            spec_sha256=spec_sha,
            dependency_artifacts=dependency_artifacts or [],
            generation_quality=_generation_quality_from_dataset_result(result, llm_provider=llm_provider),
            input_fingerprint_payload=input_fingerprint(root),
            agent_decisions=list(result.get("agent_decisions", [])),
            agent_node_inputs=list(result.get("agent_node_inputs", [])),
            agent_node_outputs=list(result.get("agent_node_outputs", [])),
            risk_flags=list(result.get("risk_flags", [])),
        )
        projection = self._handoff_dependency_review_to_product_step(
            root=root,
            state=gateway_result.graph_state,
            gate=gate,
            preferred_interrupt=gateway_result.graph_state.datasets[target].current_interrupt,
        )
        if runtime_persistence_extra:
            self._persist_graph_state(
                root,
                gateway_result.graph_state,
                node="native_code_review_interrupt",
                runtime_persistence_extra=runtime_persistence_extra,
            )
            projection = project_graph_state_to_workflow(
                root,
                gateway_result.graph_state,
                node="graph_gateway_native_code_review_interrupt",
            )
        context_artifact = result.get("product_context_artifact")
        return GraphGatewayCodeGenerationResult(
            graph_state=gateway_result.graph_state,
            workflow_projection=projection,
            code_path=str(code_path),
            generated_code=str(generated_code),
            static_check_path=str(static_check_path) if static_check_path else None,
            draft_spec_path=str(spec_path) if spec_path else None,
            response_path=result.get("llm_response_path"),
            parsed_response_path=result.get("parsed_response_path"),
            context_path=context_artifact.path if isinstance(context_artifact, ArtifactRef) else None,
            assumptions=list(result.get("code_assumptions", [])),
            risk_points=list(result.get("code_risk_points", [])),
            used_inputs=list(result.get("code_used_inputs", [])),
            expected_outputs=list(result.get("code_expected_outputs", [])),
            warnings=list(result.get("product_context_warnings", [])),
            dependency_review_status=gateway_result.graph_state.dependency_review_status,
            dependency_warnings=list(gate.dependency_warnings),
        )

    def record_execution(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        execution_state: dict[str, Any],
        validation_summary: dict[str, Any],
        artifacts: list[ArtifactRef],
        failures: list[FailureRecord] | None = None,
        input_fingerprint_payload: dict[str, Any] | None = None,
        agent_decisions: list[dict[str, Any]] | None = None,
        agent_node_inputs: list[dict[str, Any]] | None = None,
        agent_node_outputs: list[dict[str, Any]] | None = None,
        risk_flags: list[str] | None = None,
    ) -> GraphGatewayResult:
        """Persist graph-owned R execution result into canonical graph state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        next_state = self._load_or_create_state(
            root=root,
            study_id=study_id,
            run_id=run_id,
            target=target,
            input_fingerprint_payload=input_fingerprint_payload,
        )
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        dataset_state = self._dataset_state(next_state, target=target, fingerprint=fingerprint)
        terminal_followup = _assert_terminal_failure_step_allowed(dataset_state, step="execute")
        if terminal_followup is not None:
            dataset_state.execution_state["terminal_failure_followup_consumed_by"] = "execute"
        terminal_failure = bool(execution_state.get("terminal_failure"))
        if terminal_failure:
            dataset_state.execution_state.pop("terminal_failure_review", None)
            dataset_state.execution_state.pop("terminal_failure_followup", None)
            dataset_state.execution_state.pop("terminal_failure_followup_consumed_by", None)
        dataset_state.execution_state.update(execution_state)
        if terminal_followup is not None:
            dataset_state.execution_state["terminal_failure_followup"] = terminal_followup
            dataset_state.execution_state["terminal_failure_followup_consumed_by"] = "execute"
        dataset_state.validation_summary.update(validation_summary)
        dataset_state.current_interrupt = (
            InterruptState(
                name="terminal_failure",
                dataset=target,
                reason="R execution failed or produced an unusable output.",
                payload=execution_state,
            )
            if terminal_failure
            else None
        )
        dataset_state.status = "terminal_failure" if terminal_failure else "completed"
        dataset_state.updated_at = utc_now()
        for artifact in artifacts:
            _upsert_artifact(dataset_state, artifact)
        if failures:
            dataset_state.failures = list(failures)
        if not agent_decisions and not agent_node_inputs and not agent_node_outputs:
            agent_decisions, agent_node_inputs, agent_node_outputs = _default_execution_agent_io(
                study_id=study_id,
                run_id=run_id,
                target=target,
                execution_state=execution_state,
                validation_summary=validation_summary,
                terminal_failure=terminal_failure,
                artifacts=artifacts,
            )
        _append_agent_decisions(
            dataset_state,
            agent_decisions
            or [
                record_agent_decision(
                    agent="execution_agent",
                    node="execute_approved_code",
                    decision="r_execution_terminal_failure" if terminal_failure else "r_execution_completed",
                    dataset=target,
                    status="terminal_failure" if terminal_failure else "completed",
                    reason="Approved generated R code was executed through the graph-owned boundary.",
                    outputs={
                        "record_source": "graph_gateway_default",
                        "terminal_failure": terminal_failure,
                        "validation_status": validation_summary.get("status"),
                        "output_path": execution_state.get("output_path"),
                    },
                    risk_flags=["terminal_failure"] if terminal_failure else [],
                    artifact_ids=[artifact.artifact_id for artifact in artifacts],
                )
            ],
        )
        _append_agent_node_io(dataset_state, inputs=agent_node_inputs or [], outputs=agent_node_outputs or [])
        _append_risk_flags(dataset_state, risk_flags or (["terminal_failure"] if terminal_failure else []))
        dataset_state.result_summary = DatasetResultSummary(
            dataset=target,
            status=dataset_state.status,
            output_artifact_ids=[
                artifact.artifact_id
                for artifact in dataset_state.artifacts
                if artifact.kind == "output_adam"
            ],
            validation_status=str(validation_summary.get("status") or ""),
            compare_status="not_run",
            failure_ids=[failure.failure_id for failure in dataset_state.failures],
            metadata={"graph_product_execute": True, **execution_state},
        )
        next_state.datasets[target] = dataset_state
        _roll_up_study_state(next_state, preferred_interrupt=dataset_state.current_interrupt)
        _sync_study_agent_decisions(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="execute_approved_code")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_execute_approved_code")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def execute_approved_code(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        rscript_path: str | None = None,
    ) -> GraphGatewayExecutionResult:
        """Execute approved generated R code through the graph-owned boundary."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        try:
            self.dependency_gate_for_product_step(
                study_dir=root,
                study_id=study_id,
                run_id=run_id,
                dataset=target,
                start_if_missing=False,
            )
        except FileNotFoundError as exc:
            raise ValueError(
                "Graph state does not exist for this run. Generate code through the graph flow before execution."
            ) from exc
        self.validate_product_step_start(study_dir=root, run_id=run_id, dataset=target, step="execute")
        self._assert_approved_code_execution_ready(study_dir=root, run_id=run_id, dataset=target)
        result = compile_dataset_graph().invoke(
            {
                "study_id": study_id,
                "run_id": run_id,
                "dataset": target,
                "execution_mode": GRAPH_PRODUCT_EXECUTE_MODE,
                "study_dir": str(root),
                "rscript_path": rscript_path or "",
                "audit_artifacts": [],
            }
        )
        if result.get("failure_type") == "input_error" and result.get("real_run_error"):
            raise ValueError(str(result["real_run_error"]))

        response_status = str(
            result.get("response_status") or ("completed" if result.get("status") == "completed" else "terminal_failure")
        )
        validation_report = result.get("validation_report") or {}
        validation_status = str(result.get("real_validation_status") or validation_report.get("status") or "unknown")
        terminal_failure = bool(result.get("terminal_failure"))
        output_path = str(result.get("output_path") or "") or None
        validation_report_path = str(result.get("validation_report_path") or "") or None
        diagnostics_path = str(result.get("diagnostics_path") or "") or None
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        dataset_state = graph_state.datasets.get(target)
        generation_quality = dict(dataset_state.code_state.get("generation_quality") or {}) if dataset_state else {}
        gateway_result = self.record_execution(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            execution_state={
                "status": response_status,
                "validation_status": validation_status,
                "output_path": output_path,
                "validation_report_path": validation_report_path,
                "diagnostics_path": diagnostics_path,
                "terminal_failure": terminal_failure,
                "partial_output_usable": not terminal_failure,
                "generation_quality": generation_quality,
                "not_real_derivation": bool(generation_quality.get("not_real_derivation", False)),
            },
            validation_summary=validation_report,
            artifacts=list((result.get("real_run_artifacts") or {}).values()),
            failures=list(result.get("failure_records", [])),
            input_fingerprint_payload=input_fingerprint(root),
            agent_decisions=list(result.get("agent_decisions", [])),
            agent_node_inputs=list(result.get("agent_node_inputs", [])),
            agent_node_outputs=list(result.get("agent_node_outputs", [])),
            risk_flags=list(result.get("risk_flags", [])),
        )
        return GraphGatewayExecutionResult(
            graph_state=gateway_result.graph_state,
            workflow_projection=gateway_result.workflow_projection,
            status=response_status,
            validation_status=validation_status,
            output_path=output_path,
            validation_report_path=validation_report_path,
            diagnostics_path=diagnostics_path,
            terminal_failure=terminal_failure,
            errors=list(result.get("execution_errors", [])),
            warnings=list(result.get("execution_warnings", [])),
        )

    def record_compare(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        compare_summary: dict[str, Any],
        compare_report_path: str | Path | None = None,
        write_compare_report: bool = False,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayResult:
        """Persist reference-compare results into canonical graph state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        try:
            next_state = self.load_graph_state(study_dir=root, run_id=run_id).model_copy(deep=True)
        except FileNotFoundError as exc:
            raise ValueError("Graph state must exist before compare results can be recorded.") from exc
        if next_state.study_id != study_id:
            raise ValueError("Compare result study_id does not match graph state.")
        dataset_state = next_state.datasets.get(target)
        if dataset_state is None:
            raise ValueError("Dataset must exist in graph state before compare results can be recorded.")
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        existing_run_interrupt = next_state.current_interrupt
        summary = dict(compare_summary)
        summary["input_fingerprint"] = fingerprint
        report_path: Path | None = Path(compare_report_path) if compare_report_path else None
        if write_compare_report:
            report_path = report_path or root / "runs" / run_id / "compare" / f"{target.lower()}_compare_report.json"
            payload = dict(summary)
            payload["report_path"] = str(report_path.as_posix())
            _write_json(report_path, payload)
            summary["report_path"] = str(report_path.as_posix())
        dataset_state.compare_summary.update(summary)
        if report_path:
            if report_path.exists() and report_path.is_file():
                _upsert_artifact(
                    dataset_state,
                    _artifact_ref(target, "compare_report", "output", report_path, kind="compare_report"),
                )
        compare_artifact_ids = [
            artifact.artifact_id
            for artifact in dataset_state.artifacts
            if artifact.kind == "compare_report"
        ]
        validation_input = build_agent_node_input(
            agent="validation_agent",
            node="compare_reference_output",
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            task="Record generated-vs-reference ADaM comparison evidence.",
            inputs={
                "compare_summary_keys": sorted(summary.keys()),
                "input_fingerprint_digest": fingerprint.get("digest"),
                "generated_file": summary.get("generated_file"),
                "reference_file": summary.get("reference_file"),
                "reference_role": "comparison_evidence_only",
            },
            artifact_ids=compare_artifact_ids,
            risk_flags=["reference_compare_limited_scope"],
        )
        validation_decision = record_agent_decision(
            agent="validation_agent",
            node="compare_reference_output",
            decision="reference_compare_recorded",
            dataset=target,
            status=str(summary.get("status") or "unknown"),
            reason=(
                "Recorded generated-vs-reference ADaM comparison as validation evidence. "
                "This does not establish clinical derivation correctness."
            ),
            inputs={
                "input_fingerprint_digest": fingerprint.get("digest"),
                "reference_role": "comparison_evidence_only",
            },
            outputs={
                "compare_status": summary.get("status"),
                "report_path": str(report_path.as_posix()) if report_path else None,
            },
            risk_flags=["reference_compare_limited_scope"],
            artifact_ids=compare_artifact_ids,
        )
        validation_output = build_agent_node_output(
            agent="validation_agent",
            node="compare_reference_output",
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status=str(summary.get("status") or "unknown"),
            decision="reference_compare_recorded",
            reason=(
                "Recorded generated-vs-reference ADaM comparison as validation evidence. "
                "This does not establish clinical derivation correctness."
            ),
            outputs={
                "compare_status": summary.get("status"),
                "report_path": str(report_path.as_posix()) if report_path else None,
                "reference_role": "comparison_evidence_only",
            },
            risk_flags=["reference_compare_limited_scope"],
            artifact_ids=compare_artifact_ids,
            agent_decisions=[validation_decision],
        )
        _append_agent_node_io(dataset_state, inputs=[validation_input], outputs=[validation_output])
        _append_agent_decisions(
            dataset_state,
            list(validation_output["agent_decisions"]),
        )
        _append_risk_flags(dataset_state, ["reference_compare_limited_scope"])
        existing_summary = dataset_state.result_summary
        output_artifact_ids = [
            artifact.artifact_id
            for artifact in dataset_state.artifacts
            if artifact.kind == "output_adam"
        ]
        dataset_state.result_summary = DatasetResultSummary(
            dataset=target,
            status=existing_summary.status if existing_summary else dataset_state.status,
            output_artifact_ids=output_artifact_ids or (existing_summary.output_artifact_ids if existing_summary else []),
            audit_artifact_id=existing_summary.audit_artifact_id if existing_summary else None,
            validation_status=(
                existing_summary.validation_status
                if existing_summary
                else str(dataset_state.validation_summary.get("status") or "") or None
            ),
            compare_status=str(summary.get("status") or ""),
            failure_ids=[failure.failure_id for failure in dataset_state.failures],
            metadata={
                **(existing_summary.metadata if existing_summary else {}),
                "graph_compare_recorded": True,
                "compare_report_path": str(report_path.as_posix()) if report_path else None,
            },
        )
        dataset_state.updated_at = utc_now()
        next_state.datasets[target] = dataset_state
        if existing_run_interrupt is not None and existing_run_interrupt.status == "open" and existing_run_interrupt.dataset is None:
            next_state.current_interrupt = existing_run_interrupt
            next_state.status = "needs_review"
        else:
            _roll_up_study_state(next_state)
        _sync_study_agent_decisions(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="compare_reference_output")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_compare_reference_output")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def compare_reference_output(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        dataset: str,
    ) -> GraphGatewayCompareResult:
        """Compute generated-vs-reference compare and record it in graph state."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        run_dir = root / "runs" / run_id
        output_path = usable_generated_output_path(run_dir, target)
        reference_path = reference_adam_path(root, target)
        compare_summary = compare_dataset_files(target, output_path, reference_path)
        result = self.record_compare(
            study_dir=root,
            study_id=graph_state.study_id,
            run_id=run_id,
            dataset=target,
            compare_summary=compare_summary,
            write_compare_report=True,
            input_fingerprint_payload=input_fingerprint(root),
        )
        dataset_state = result.graph_state.datasets.get(target)
        if dataset_state is not None:
            payload = dict(dataset_state.compare_summary)
            compare_summary = {
                key: value
                for key, value in payload.items()
                if key in {
                    "dataset",
                    "status",
                    "generated_file",
                    "reference_file",
                    "row_count_generated",
                    "row_count_reference",
                    "row_count_delta",
                    "generated_only_columns",
                    "reference_only_columns",
                    "common_columns",
                    "key_columns",
                    "matched_rows",
                    "generated_only_keys",
                    "reference_only_keys",
                    "compared_cells",
                    "mismatch_count",
                    "mismatch_samples",
                    "report_path",
                    "note",
                }
            }
        return GraphGatewayCompareResult(
            graph_state=result.graph_state,
            workflow_projection=result.workflow_projection,
            compare_summary=compare_summary,
        )

    def record_terminal_failure_review(
        self,
        *,
        study_dir: str | Path,
        study_id: str,
        run_id: str,
        dataset: str,
        command: HumanCommand,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayResult:
        """Persist human triage for a graph-owned terminal failure interrupt."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        try:
            next_state = self.load_graph_state(study_dir=root, run_id=run_id).model_copy(deep=True)
        except FileNotFoundError as exc:
            raise ValueError("Graph state must exist before terminal failure review.") from exc
        if next_state.study_id != study_id:
            raise ValueError("Terminal failure review study_id does not match graph state.")
        dataset_state = next_state.datasets.get(target)
        if dataset_state is None:
            raise ValueError("Dataset must exist in graph state before terminal failure review.")
        interrupt = dataset_state.current_interrupt
        if interrupt is None or interrupt.name != "terminal_failure" or interrupt.status != "open":
            raise ValueError("Current dataset graph state is not waiting for terminal_failure review.")
        if command.interrupt != "terminal_failure" or command.dataset is None or command.dataset.strip().upper() != target:
            raise ValueError("Terminal failure review command must target the failed dataset.")
        allowed_actions = {item["action"] for item in TERMINAL_FAILURE_REVIEW_ACTIONS}
        if command.action not in allowed_actions:
            raise ValueError("Terminal failure review action is not supported.")
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        review_payload = {
            "action": command.action,
            "reviewer": command.reviewer,
            "notes": command.notes,
            "input_fingerprint": fingerprint,
            "failure_ids": [failure.failure_id for failure in dataset_state.failures],
        }
        dataset_state.human_commands.append(command)
        dataset_state.execution_state["terminal_failure_review"] = review_payload
        dataset_state.execution_state["next_action"] = _terminal_failure_next_action(command.action)
        dataset_state.execution_state.pop("terminal_failure_followup_consumed_by", None)
        resolved_actions = {"retry_execution", "skip_dataset", "continue_other_datasets"}
        dataset_state.current_interrupt = None if command.action in resolved_actions else InterruptState(
            name="terminal_failure",
            dataset=target,
            reason=_terminal_failure_reason(command.action),
            payload={
                **(interrupt.payload or {}),
                "last_review": review_payload,
                "next_action": _terminal_failure_next_action(command.action),
            },
        )
        if command.action == "retry_execution":
            dataset_state.status = "pending"
        elif command.action == "skip_dataset":
            dataset_state.status = "failed"
        elif command.action == "continue_other_datasets":
            dataset_state.status = "terminal_failure"
        else:
            dataset_state.status = "needs_review"
        dataset_state.updated_at = utc_now()
        existing_summary = dataset_state.result_summary
        dataset_state.result_summary = DatasetResultSummary(
            dataset=target,
            status=dataset_state.status,
            output_artifact_ids=existing_summary.output_artifact_ids if existing_summary else [],
            audit_artifact_id=existing_summary.audit_artifact_id if existing_summary else None,
            validation_status=(
                existing_summary.validation_status
                if existing_summary
                else str(dataset_state.validation_summary.get("status") or "") or None
            ),
            compare_status=existing_summary.compare_status if existing_summary else None,
            failure_ids=[failure.failure_id for failure in dataset_state.failures],
            metadata={
                **(existing_summary.metadata if existing_summary else {}),
                "terminal_failure_review": review_payload,
                "terminal_failure_next_action": _terminal_failure_next_action(command.action),
            },
        )
        failure_ids = [failure.failure_id for failure in dataset_state.failures]
        recommended_routes = [
            failure.recommended_route
            for failure in dataset_state.failures
            if failure.recommended_route
        ]
        terminal_next_action = _terminal_failure_next_action(command.action)
        interrupt_open = dataset_state.current_interrupt is not None and dataset_state.current_interrupt.status == "open"
        triage_input = build_agent_node_input(
            agent="diagnosis_repair_agent",
            node="terminal_failure_review",
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            task="Record human triage for a terminal dataset failure.",
            inputs={
                "failure_ids": failure_ids,
                "recommended_routes": recommended_routes,
                "human_action": command.action,
                "input_fingerprint_digest": fingerprint.get("digest"),
                "current_interrupt": "terminal_failure",
            },
            risk_flags=["terminal_failure_triage_limited_scope"],
        )
        triage_decision = record_agent_decision(
            agent="diagnosis_repair_agent",
            node="terminal_failure_review",
            decision="terminal_failure_triage_recorded",
            dataset=target,
            status=dataset_state.status,
            reason=(
                "Recorded the human terminal-failure triage decision and the next controlled product action. "
                "This does not execute repair, revise specs, or retry R automatically."
            ),
            inputs={
                "failure_ids": failure_ids,
                "recommended_routes": recommended_routes,
                "human_action": command.action,
            },
            outputs={
                "next_action": terminal_next_action,
                "interrupt_open": interrupt_open,
            },
            risk_flags=["terminal_failure_triage_limited_scope"],
        )
        triage_output = build_agent_node_output(
            agent="diagnosis_repair_agent",
            node="terminal_failure_review",
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status=dataset_state.status,
            decision="terminal_failure_triage_recorded",
            reason=(
                "Recorded the human terminal-failure triage decision and the next controlled product action. "
                "This does not execute repair, revise specs, or retry R automatically."
            ),
            outputs={
                "next_action": terminal_next_action,
                "interrupt_open": interrupt_open,
                "human_action": command.action,
            },
            risk_flags=["terminal_failure_triage_limited_scope"],
            agent_decisions=[triage_decision],
        )
        _append_agent_node_io(dataset_state, inputs=[triage_input], outputs=[triage_output])
        _append_agent_decisions(
            dataset_state,
            list(triage_output["agent_decisions"]),
        )
        _append_risk_flags(dataset_state, ["terminal_failure_triage_limited_scope"])
        next_state.datasets[target] = dataset_state
        next_state.human_commands.append(command)
        if next_state.current_interrupt is not None and next_state.current_interrupt.name == "dependency_review":
            next_state.current_interrupt = None
        _roll_up_study_state(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="terminal_failure_review")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_terminal_failure_review")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def review_terminal_failure(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        dataset: str,
        decision: str,
        reviewer: str,
        notes: str = "",
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayTerminalFailureReviewResult:
        """Validate and persist a terminal-failure triage decision."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        normalized_decision = decision.strip().lower()
        allowed = {item["action"] for item in TERMINAL_FAILURE_REVIEW_ACTIONS}
        if normalized_decision not in allowed:
            raise ValueError(
                "Terminal failure decision must be retry_execution, repair_code, revise_spec, "
                "request_new_input, skip_dataset, or continue_other_datasets."
            )
        try:
            graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        except FileNotFoundError as exc:
            raise ValueError("Graph state must exist before terminal failure review.") from exc
        if target not in graph_state.datasets:
            raise ValueError(f"Dataset is not part of this graph run: {target}")
        command = HumanCommand(
            interrupt="terminal_failure",
            action=normalized_decision,
            dataset=target,
            reviewer=reviewer,
            notes=notes,
            payload={"decision": normalized_decision},
        )
        result = self.record_terminal_failure_review(
            study_dir=root,
            study_id=graph_state.study_id,
            run_id=run_id,
            dataset=target,
            command=command,
            input_fingerprint_payload=input_fingerprint_payload or input_fingerprint(root),
        )
        reviewed_dataset = result.graph_state.datasets[target]
        current_interrupt = None
        if reviewed_dataset.current_interrupt is not None and reviewed_dataset.current_interrupt.status == "open":
            current_interrupt = reviewed_dataset.current_interrupt.name
        return GraphGatewayTerminalFailureReviewResult(
            graph_state=result.graph_state,
            workflow_projection=result.workflow_projection,
            decision=normalized_decision,
            current_interrupt=current_interrupt,
            next_action=str(reviewed_dataset.execution_state.get("next_action") or ""),
        )

    def review_terminal_failure_from_command(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        command: HumanCommand,
        input_fingerprint_payload: dict[str, Any] | None = None,
    ) -> GraphGatewayTerminalFailureReviewResult:
        """Bridge a graph-native terminal-failure command to the gateway triage flow."""

        root = Path(study_dir).expanduser()
        if command.dataset is None:
            raise ValueError("Terminal failure review command must include a dataset.")
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        open_study_interrupt = _open_study_interrupt(graph_state)
        if open_study_interrupt is not None:
            raise ValueError(
                f"Study-level interrupt {open_study_interrupt.name} must be resolved before dataset terminal_failure."
            )
        _assert_resume_command_matches_open_interrupt(graph_state, command)
        if command.interrupt != "terminal_failure":
            raise ValueError("Terminal failure review command must target terminal_failure.")
        allowed = {item["action"] for item in TERMINAL_FAILURE_REVIEW_ACTIONS}
        if command.action not in allowed:
            raise ValueError("Terminal failure review command action is not supported.")
        return self.review_terminal_failure(
            study_dir=root,
            run_id=run_id,
            dataset=command.dataset,
            decision=command.action,
            reviewer=command.reviewer,
            notes=command.notes,
            input_fingerprint_payload=input_fingerprint_payload,
        )

    def validate_product_step_start(
        self,
        *,
        study_dir: str | Path,
        run_id: str,
        dataset: str,
        step: str,
    ) -> None:
        """Fail closed when a product step would bypass terminal-failure triage."""

        root = Path(study_dir).expanduser()
        target = dataset.strip().upper()
        try:
            graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        except FileNotFoundError:
            return
        dataset_state = graph_state.datasets.get(target)
        if dataset_state is None:
            return
        _assert_terminal_failure_step_allowed(dataset_state, step=step)

    def mark_inputs_changed(self, *, study_dir: str | Path, run_id: str) -> GraphGatewayResult:
        """Mark canonical graph state stale after uploaded study evidence changes."""

        root = Path(study_dir).expanduser()
        try:
            next_state = self.load_graph_state(study_dir=root, run_id=run_id).model_copy(deep=True)
        except FileNotFoundError as exc:
            raise ValueError(f"Graph state does not exist for run: {run_id}") from exc
        new_fingerprint = input_fingerprint(root)
        diff = compare_fingerprints(next_state.input_fingerprint, new_fingerprint)
        was_stale = bool(next_state.dependency_plan.get("plan_stale")) or next_state.dependency_review_status == "stale"
        next_state.input_fingerprint = new_fingerprint
        next_state.dependency_plan["plan_stale"] = bool(diff["changed"] or was_stale)
        if diff["changed"] or not was_stale:
            next_state.dependency_plan["input_diff"] = diff
        if diff["changed"]:
            next_state.dependency_plan["stale_reason"] = "Study input files changed after prior planning or review."
        elif not was_stale:
            next_state.dependency_plan["stale_reason"] = ""
        if diff["changed"]:
            for dataset, dataset_state in sorted(next_state.datasets.items()):
                if not _has_dataset_product_progress(dataset_state):
                    dataset_state.input_fingerprint = new_fingerprint
                    continue
                _mark_dataset_stale_for_input_change(dataset_state, diff=diff, fingerprint=new_fingerprint)
            next_state.dependency_review_status = "stale"
            next_state.current_interrupt = InterruptState(
                name="dependency_review",
                reason="Study inputs changed after dependency planning or dataset review. Re-run dependency planning before continuing.",
                payload={"input_diff": diff},
            )
            next_state.status = "needs_review"
            _append_risk_flags(next_state, ["inputs_changed_after_planning"])
        else:
            _roll_up_study_state(next_state)
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="input_upload_rescan")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_input_upload_rescan")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

    def mark_all_inputs_changed(self, *, study_dir: str | Path) -> GraphGatewayInputInvalidationResult:
        """Mark every canonical graph run stale when its input fingerprint changed."""

        root = Path(study_dir).expanduser()
        new_fingerprint = input_fingerprint(root)
        latest_diff: dict[str, Any] = {
            "changed": bool(new_fingerprint.get("files")),
            "added": [item["path"] for item in new_fingerprint.get("files", [])],
            "removed": [],
            "changed_files": [],
            "old_digest": None,
            "new_digest": new_fingerprint.get("digest"),
        }
        touched: list[str] = []
        skipped: list[str] = []
        for run_id in self.list_graph_runs(study_dir=root):
            try:
                old_state = self.load_graph_state(study_dir=root, run_id=run_id)
                diff = compare_fingerprints(old_state.input_fingerprint, new_fingerprint)
                latest_diff = diff
                self.mark_inputs_changed(study_dir=root, run_id=run_id)
            except (OSError, ValueError, FileNotFoundError):
                skipped.append(run_id)
                continue
            if diff.get("changed"):
                touched.append(run_id)
        return GraphGatewayInputInvalidationResult(
            input_fingerprint=new_fingerprint,
            input_diff=latest_diff,
            touched_runs=[],
            touched_graph_runs=touched,
            skipped_graph_runs=skipped,
        )

    def mark_study_inputs_changed(self, *, study_dir: str | Path) -> GraphGatewayInputInvalidationResult:
        """Invalidate graph-owned runs and legacy projections after uploaded evidence changes."""

        root = Path(study_dir).expanduser()
        legacy_invalidation = invalidate_active_workflows(root)
        graph_invalidation = self.mark_all_inputs_changed(study_dir=root)
        return GraphGatewayInputInvalidationResult(
            input_fingerprint=legacy_invalidation.get("input_fingerprint", graph_invalidation.input_fingerprint),
            input_diff=legacy_invalidation.get("input_diff", graph_invalidation.input_diff),
            touched_runs=list(legacy_invalidation.get("touched_runs", [])),
            touched_graph_runs=graph_invalidation.touched_graph_runs,
            skipped_graph_runs=graph_invalidation.skipped_graph_runs,
        )

    def list_graph_runs(self, *, study_dir: str | Path) -> list[str]:
        """Return run ids that have canonical graph state."""

        runs_dir = Path(study_dir).expanduser() / "runs"
        if not runs_dir.exists() or not runs_dir.is_dir():
            return []
        return [
            run_dir.name
            for run_dir in sorted(item for item in runs_dir.iterdir() if item.is_dir())
            if (run_dir / "graph_state.json").is_file()
        ]

    def progress_summary(self, *, study_dir: str | Path, run_id: str) -> dict[str, Any]:
        """Return a graph-owned read model for UI progress and next actions."""

        root = Path(study_dir).expanduser()
        graph_state = self.load_graph_state(study_dir=root, run_id=run_id)
        datasets = [_dataset_progress_item(graph_state, dataset) for dataset in _progress_dataset_order(graph_state)]
        output_quality_rollup = study_output_quality_rollup(
            datasets,
            target_datasets=graph_state.target_datasets,
        )
        next_item = _study_next_action(graph_state, datasets, output_quality_rollup=output_quality_rollup)
        run_dir = root / "runs" / run_id
        workflow_state_path = run_dir / "workflow_state.json"
        return {
            "study_id": graph_state.study_id,
            "run_id": graph_state.run_id,
            "status": graph_state.status,
            "next_action": next_item["next_action"],
            "action_label": next_item["action_label"],
            "output_quality_rollup": output_quality_rollup,
            "current_interrupt": _interrupt_payload(graph_state.current_interrupt),
            "dependency_review_status": graph_state.dependency_review_status,
            "plan_stale": bool(graph_state.dependency_plan.get("plan_stale")),
            "target_datasets": list(graph_state.target_datasets),
            "runnable_datasets": list(graph_state.runnable_datasets),
            "blocked_datasets": list(graph_state.blocked_datasets),
            "review_queue": _human_review_queue_items(graph_state, datasets),
            "datasets": datasets,
            "runtime_persistence": dict(graph_state.runtime_persistence),
            "graph_state_path": str((run_dir / "graph_state.json").as_posix()),
            "workflow_state_path": str(workflow_state_path.as_posix()) if workflow_state_path.exists() else None,
        }

    def _generated_code_state(self, study_dir: Path, *, run_id: str, dataset: str) -> dict[str, Any]:
        try:
            graph_state = self.load_graph_state(study_dir=study_dir, run_id=run_id)
        except FileNotFoundError as exc:
            raise ValueError("Generated code must be recorded in graph state before code review.") from exc
        dataset_state = graph_state.datasets.get(dataset.strip().upper())
        if dataset_state is None or dataset_state.code_state.get("status") != "generated":
            raise ValueError("Generated code must be recorded in graph state before code review.")
        return dict(dataset_state.code_state)

    def _graph_code_review_matches_review_path(self, study_dir: Path, *, run_id: str, dataset: str, review_path: Path) -> bool:
        try:
            graph_state = self.load_graph_state(study_dir=study_dir, run_id=run_id)
        except FileNotFoundError:
            return False
        dataset_state = graph_state.datasets.get(dataset.strip().upper())
        if dataset_state is None:
            return False
        code_state = dataset_state.code_state
        return (
            code_state.get("status") in {"approved", "rejected"}
            and str(Path(str(code_state.get("review_path") or "")).as_posix()) == str(review_path.as_posix())
        )

    def _graph_draft_spec_review_matches_review_path(
        self,
        study_dir: Path,
        *,
        run_id: str,
        dataset: str,
        review_path: Path,
    ) -> bool:
        try:
            graph_state = self.load_graph_state(study_dir=study_dir, run_id=run_id)
        except FileNotFoundError:
            return False
        dataset_state = graph_state.datasets.get(dataset.strip().upper())
        if dataset_state is None:
            return False
        spec_state = dataset_state.spec_state
        return (
            spec_state.get("status") in {"approved", "rejected"}
            and str(Path(str(spec_state.get("review_path") or "")).as_posix()) == str(review_path.as_posix())
        )

    def _assert_approved_code_execution_ready(self, *, study_dir: Path, run_id: str, dataset: str) -> None:
        """Fail closed before execution if graph-owned code review is not current."""

        target = dataset.strip().upper()
        run_dir = study_dir / "runs" / run_id
        code_path = run_dir / "code" / f"build_{target.lower()}.R"
        review_path = run_dir / "review" / f"{target.lower()}_code_review.json"
        try:
            assert_graph_code_review_current(study_dir, run_id, target, review_path, code_path)
        except GraphExecutionError as exc:
            raise ValueError(_execution_preflight_error_message(str(exc))) from exc

    def load_graph_state(self, *, study_dir: str | Path, run_id: str) -> StudyRunState:
        """Load the durable canonical graph state for a local run."""

        path = _graph_state_path(study_dir, run_id)
        if not path.exists():
            raise FileNotFoundError(f"Graph state does not exist: {path}")
        return StudyRunState.model_validate_json(path.read_text(encoding="utf-8"))

    def get_state(self, *, study_id: str, run_id: str) -> dict[str, Any]:
        """Read the raw LangGraph checkpoint values for a run."""

        snapshot = self._graph.get_state(self._config(study_id, run_id))
        return dict(snapshot.values or {})

    def _canonical_state_from_plan(self, plan_state: dict[str, Any], *, study_dir: Path) -> StudyRunState:
        fingerprint = input_fingerprint(study_dir)
        dependency_review_status = _dependency_review_status(plan_state)
        current_interrupt = None
        if dependency_review_status in {"blocked", "warning", "review_required"}:
            current_interrupt = InterruptState(
                name="dependency_review",
                reason=_dependency_review_reason(dependency_review_status),
                payload={
                    "dependency_review_status": dependency_review_status,
                    "blocked_datasets": plan_state.get("blocked_datasets", []),
                    "dependency_warnings": plan_state.get("dependency_planning_warnings", []),
                },
            )
        datasets = {
            dataset: DatasetRunState(
                study_id=plan_state["study_id"],
                run_id=plan_state["run_id"],
                dataset=dataset,
                status="pending",
                input_fingerprint=fingerprint,
                dependency_resolution=[
                    record
                    for record in plan_state.get("dependency_resolution", [])
                    if str(record.get("target_dataset", "")).strip().upper() == dataset
                ],
                current_interrupt=(
                    InterruptState(
                        name="dependency_user_action_required",
                        dataset=dataset,
                        reason="Dataset has unresolved dependency requirements.",
                    )
                    if dataset in _blocked_dataset_names(plan_state.get("blocked_datasets", []))
                    else None
                ),
                result_summary=DatasetResultSummary(dataset=dataset, status="pending"),
            )
            for dataset in plan_state.get("target_datasets", [])
        }
        dependency_plan = {
            "dependency_graph": plan_state.get("dependency_graph", {}),
            "dataset_dependencies": plan_state.get("dataset_dependencies", {}),
            "execution_batches": plan_state.get("execution_batches", []),
            "auto_added_datasets": plan_state.get("auto_added_datasets", []),
            "unsupported_datasets": plan_state.get("unsupported_datasets", []),
            "dependency_evidence": plan_state.get("dependency_evidence", ""),
            "dependency_evidence_records": plan_state.get("dependency_evidence_records", []),
            "dependency_planning_warnings": plan_state.get("dependency_planning_warnings", []),
        }
        status = "needs_review" if current_interrupt else "pending"
        dependency_decision = record_agent_decision(
            agent="dependency_agent",
            node="dependency_plan",
            decision="dependency_plan_prepared",
            status=status,
            reason=_dependency_review_reason(dependency_review_status)
            if dependency_review_status != "accepted"
            else "Dependency plan was accepted without blocking review.",
            inputs={"requested_datasets": plan_state.get("requested_datasets", [])},
            outputs={
                "target_datasets": plan_state.get("target_datasets", []),
                "runnable_datasets": plan_state.get("runnable_datasets", []),
                "blocked_datasets": plan_state.get("blocked_datasets", []),
                "dependency_review_status": dependency_review_status,
            },
            risk_flags=[f"dependency_review_{dependency_review_status}"]
            if dependency_review_status != "accepted"
            else [],
        )
        dependency_agent_input = build_agent_node_input(
            agent="dependency_agent",
            node="dependency_plan",
            study_id=plan_state["study_id"],
            run_id=plan_state["run_id"],
            task="Prepare the study dependency plan and decide whether dependency review is needed.",
            inputs={
                "requested_datasets": plan_state.get("requested_datasets", []),
                "approved_dependency_datasets": plan_state.get("approved_dependency_datasets", []),
                "input_fingerprint_digest": fingerprint.get("digest"),
            },
            risk_flags=[f"dependency_review_{dependency_review_status}"]
            if dependency_review_status != "accepted"
            else [],
        )
        dependency_agent_output = build_agent_node_output(
            agent="dependency_agent",
            node="dependency_plan",
            study_id=plan_state["study_id"],
            run_id=plan_state["run_id"],
            status=status,
            decision="dependency_plan_prepared",
            reason=dependency_decision["reason"],
            outputs={
                "target_datasets": plan_state.get("target_datasets", []),
                "runnable_datasets": plan_state.get("runnable_datasets", []),
                "blocked_datasets": plan_state.get("blocked_datasets", []),
                "dependency_review_status": dependency_review_status,
                "execution_batches": plan_state.get("execution_batches", []),
            },
            risk_flags=[f"dependency_review_{dependency_review_status}"]
            if dependency_review_status != "accepted"
            else [],
            agent_decisions=[dependency_decision],
        )
        return StudyRunState(
            study_id=plan_state["study_id"],
            run_id=plan_state["run_id"],
            status=status,
            requested_datasets=plan_state.get("requested_datasets", []),
            target_datasets=plan_state.get("target_datasets", []),
            runnable_datasets=plan_state.get("runnable_datasets", []),
            blocked_datasets=plan_state.get("blocked_datasets", []),
            current_interrupt=current_interrupt,
            input_fingerprint=fingerprint,
            dependency_plan=dependency_plan,
            dependency_decisions=plan_state.get("dependency_decisions", []),
            dependency_resolution=plan_state.get("dependency_resolution", []),
            dependency_review_status=dependency_review_status,
            datasets=datasets,
            agent_decisions=[dependency_decision],
            agent_node_inputs=[dependency_agent_input],
            agent_node_outputs=[dependency_agent_output],
            risk_flags=[f"dependency_review_{dependency_review_status}"]
            if dependency_review_status != "accepted"
            else [],
        )

    def _persist_graph_state(
        self,
        study_dir: str | Path,
        state: StudyRunState,
        *,
        node: str,
        runtime_persistence_extra: dict[str, Any] | None = None,
    ) -> None:
        root = Path(study_dir)
        state.runtime_persistence = describe_checkpointer(
            checkpointer=self._checkpointer,
            study_dir=root,
            run_id=state.run_id,
            bundle=self._checkpointer_bundle,
        )
        if runtime_persistence_extra:
            state.runtime_persistence.update(runtime_persistence_extra)
        _sync_study_agent_decisions(state)
        _sync_study_agent_node_io(state)
        _update_agent_audit_summary(root, state)
        path = _graph_state_path(study_dir, state.run_id)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(state.model_dump_json(indent=2), encoding="utf-8")
        _write_graph_sqlite_checkpoint(study_dir, state.run_id, node=node, state=state)

    def _merge_existing_dataset_progress(self, root: Path, planned_state: StudyRunState) -> StudyRunState:
        """Preserve dataset product progress when the public prepare endpoint replans."""

        try:
            existing = self.load_graph_state(study_dir=root, run_id=planned_state.run_id)
        except FileNotFoundError:
            return planned_state
        merged = planned_state.model_copy(deep=True)
        for dataset, existing_dataset_state in existing.datasets.items():
            if not _has_dataset_product_progress(existing_dataset_state):
                continue
            planned_dataset_state = merged.datasets.get(dataset)
            preserved = existing_dataset_state.model_copy(deep=True)
            if planned_dataset_state is not None:
                preserved.dependency_resolution = list(planned_dataset_state.dependency_resolution)
            diff = compare_fingerprints(preserved.input_fingerprint, merged.input_fingerprint)
            if diff["changed"]:
                _mark_dataset_stale_for_input_change(preserved, diff=diff, fingerprint=merged.input_fingerprint)
            merged.datasets[dataset] = preserved
            if dataset not in merged.target_datasets:
                merged.target_datasets.append(dataset)
            if dataset not in merged.runnable_datasets and dataset in existing.runnable_datasets:
                merged.runnable_datasets.append(dataset)
        if _open_study_interrupt(merged) is None and existing.current_interrupt is not None and existing.current_interrupt.dataset:
            dataset = existing.current_interrupt.dataset.strip().upper()
            if _has_dataset_product_progress(merged.datasets.get(dataset)):
                merged.current_interrupt = existing.current_interrupt
                merged.status = existing.status
        if _open_study_interrupt(merged) is None:
            _roll_up_study_state(merged)
        return merged

    @staticmethod
    def _config(study_id: str, run_id: str) -> dict[str, Any]:
        return {"configurable": {"thread_id": f"{study_id}:{run_id}"}}

    @staticmethod
    def _dataset_config(study_id: str, run_id: str, dataset: str) -> dict[str, Any]:
        return {"configurable": {"thread_id": f"{study_id}:{run_id}:{dataset.strip().upper()}"}}

    def _load_or_create_state(
        self,
        *,
        root: Path,
        study_id: str,
        run_id: str,
        target: str,
        input_fingerprint_payload: dict[str, Any] | None,
    ) -> StudyRunState:
        try:
            next_state = self.load_graph_state(study_dir=root, run_id=run_id).model_copy(deep=True)
        except FileNotFoundError:
            fingerprint = input_fingerprint_payload or input_fingerprint(root)
            next_state = StudyRunState(
                study_id=study_id,
                run_id=run_id,
                status="running",
                target_datasets=[target],
                runnable_datasets=[target],
                input_fingerprint=fingerprint,
                datasets={},
            )
        fingerprint = input_fingerprint_payload or input_fingerprint(root)
        next_state.input_fingerprint = fingerprint
        if target not in next_state.target_datasets:
            next_state.target_datasets.append(target)
        if target not in next_state.runnable_datasets:
            next_state.runnable_datasets.append(target)
        return next_state

    @staticmethod
    def _dataset_state(next_state: StudyRunState, *, target: str, fingerprint: dict[str, Any]) -> DatasetRunState:
        dataset_state = next_state.datasets.get(target) or DatasetRunState(
            study_id=next_state.study_id,
            run_id=next_state.run_id,
            dataset=target,
            input_fingerprint=fingerprint,
            result_summary=DatasetResultSummary(dataset=target, status="pending"),
        )
        dataset_state.input_fingerprint = fingerprint
        return dataset_state

    def _handoff_dependency_review_to_product_step(
        self,
        *,
        root: Path,
        state: StudyRunState,
        gate: GraphGatewayDependencyGateResult,
        preferred_interrupt: InterruptState | None = None,
    ) -> dict[str, Any]:
        _clear_nonblocking_dependency_review_interrupt(state, gate)
        _roll_up_study_state(state, preferred_interrupt=preferred_interrupt)
        _sync_study_agent_decisions(state)
        self._persist_graph_state(root, state, node="dependency_gate_product_handoff")
        return project_graph_state_to_workflow(
            root,
            state,
            node="graph_gateway_dependency_gate_product_handoff",
        )


def _normalize_dataset_list(values: list[str]) -> list[str]:
    normalized = []
    for value in values:
        dataset = str(value).strip().upper()
        if dataset and dataset not in normalized:
            normalized.append(dataset)
    return normalized


def _dependency_review_status(plan_state: dict[str, Any]) -> str:
    explicit_status = plan_state.get("dependency_review_status")
    if explicit_status in {"approved", "rejected"}:
        return str(explicit_status)
    if plan_state.get("unsupported_datasets") or plan_state.get("dependency_action_required"):
        return "blocked"
    if plan_state.get("dependency_planning_warnings"):
        return "warning"
    if any(decision.get("review_required") for decision in plan_state.get("dependency_decisions", [])):
        return "review_required"
    return "accepted"


def _dependency_review_reason(status: str) -> str:
    if status == "blocked":
        return "Dependency planning found unresolved or unsupported datasets."
    if status == "warning":
        return "Dependency planning completed with warnings."
    return "Dependency planning requires human review before execution."


def _blocked_dataset_names(blocked: list[dict[str, Any]]) -> set[str]:
    return {str(item.get("dataset", "")).strip().upper() for item in blocked if item.get("dataset")}


def _dependency_gate_result(study_dir: Path, graph_state: StudyRunState) -> GraphGatewayDependencyGateResult:
    plan_payload = graph_state.dependency_plan
    unsupported = plan_payload.get("unsupported_datasets", [])
    blocked = list(graph_state.blocked_datasets) + [
        {"dataset": str(dataset), "reason": "unsupported_dataset", "blocked_by": "study_planner"}
        for dataset in unsupported
    ]
    return GraphGatewayDependencyGateResult(
        study_id=graph_state.study_id,
        run_id=graph_state.run_id,
        requested_datasets=list(graph_state.requested_datasets),
        target_datasets=list(graph_state.target_datasets),
        runnable_datasets=list(graph_state.runnable_datasets),
        blocked_datasets=blocked,
        dependency_review_status=graph_state.dependency_review_status or "accepted",
        dependency_plan=dict(plan_payload),
        dependency_decisions=list(graph_state.dependency_decisions),
        dependency_resolution=list(graph_state.dependency_resolution),
        dependency_warnings=list(plan_payload.get("dependency_planning_warnings", [])),
        workflow_state_path=str((study_dir / "runs" / graph_state.run_id / "workflow_state.json").as_posix()),
    )


def _assert_dependency_gate_open(gate: GraphGatewayDependencyGateResult, target: str) -> None:
    dataset = target.strip().upper()
    plan_payload = getattr(gate, "dependency_plan", None)
    if isinstance(plan_payload, dict) and plan_payload.get("plan_stale"):
        raise ValueError(
            f"{dataset} dependency plan is stale because study inputs changed. "
            "Re-run dependency planning before finalizing inputs or generating code."
        )
    if gate.dependency_review_status == "stale":
        raise ValueError(
            f"{dataset} dependency plan is stale because study inputs changed. "
            "Re-run dependency planning before finalizing inputs or generating code."
        )
    blocked = [
        block
        for block in gate.blocked_datasets
        if str(block.get("dataset", "")).strip().upper() == dataset
    ]
    if blocked:
        reasons = ", ".join(str(block.get("reason") or "blocked") for block in blocked)
        raise ValueError(
            f"{dataset} cannot continue until dependency issues are resolved: {reasons}. "
            "Review the dependency plan before finalizing inputs or generating code."
        )
    if dataset not in [item.strip().upper() for item in gate.runnable_datasets]:
        raise ValueError(
            f"{dataset} is not runnable in the current dependency plan. "
            "Review the dependency plan before finalizing inputs or generating code."
        )
    if gate.dependency_review_status == "warning" or gate.dependency_warnings:
        raise ValueError(
            f"{dataset} dependency plan has warnings that require review before this step. "
            f"Warnings: {'; '.join(gate.dependency_warnings)}"
        )
    blocking_decisions = [
        decision
        for decision in gate.dependency_decisions
        if str(decision.get("dataset", "")).strip().upper() == dataset
        and decision.get("review_required") is True
        and str(decision.get("source", "")) != "no_dependency_evidence"
    ]
    if blocking_decisions:
        sources = ", ".join(str(decision.get("source") or "unknown") for decision in blocking_decisions)
        raise ValueError(
            f"{dataset} dependency plan requires human review before this step. "
            f"Review-required sources: {sources}."
        )


def _clear_nonblocking_dependency_review_interrupt(state: StudyRunState, gate: GraphGatewayDependencyGateResult) -> None:
    """Clear study-level dependency review after the gate proves the target can continue."""

    interrupt = state.current_interrupt
    if interrupt is None or interrupt.name != "dependency_review" or interrupt.status != "open":
        return
    if gate.dependency_review_status != "review_required":
        return
    blocking_decisions = [
        decision
        for decision in gate.dependency_decisions
        if decision.get("review_required") is True and str(decision.get("source", "")) != "no_dependency_evidence"
    ]
    if blocking_decisions or gate.blocked_datasets or gate.dependency_warnings:
        return
    state.current_interrupt = None
    state.dependency_review_status = "accepted"
    state.dependency_plan["dependency_review_status_before_product_step"] = "review_required"
    state.dependency_plan["dependency_review_auto_accepted_reason"] = (
        "Only no_dependency_evidence decisions required review; product draft/spec/code review now carries that risk."
    )


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


def _generation_quality_from_dataset_result(result: dict[str, Any], *, llm_provider: dict[str, Any] | None = None) -> dict[str, Any]:
    provider_config = llm_provider or {}
    provider = str(result.get("llm_provider") or provider_config.get("provider") or "").strip()
    model = result.get("llm_model") if result.get("llm_model") is not None else provider_config.get("model")
    provider_alias = str(result.get("provider_alias") or "").strip()
    transport = str(result.get("transport") or "").strip()
    not_real_derivation = bool(result.get("not_real_derivation"))
    if provider.lower() == "mock" or provider_alias.lower() == "mock" or transport.lower() == "mock":
        not_real_derivation = True
    return {
        "llm_provider": provider or None,
        "llm_model": model,
        "provider_alias": provider_alias or None,
        "transport": transport or None,
        "provider_base_url": result.get("provider_base_url"),
        "not_real_derivation": not_real_derivation,
    }


def _assert_dependency_artifacts_current(records: list[Any], *, stale_message: str) -> None:
    for record in records:
        if not isinstance(record, dict):
            continue
        path = record.get("artifact_path")
        expected_sha = record.get("artifact_sha256")
        if not path or not expected_sha:
            continue
        artifact_path = Path(str(path))
        if not artifact_path.exists() or not artifact_path.is_file():
            raise ValueError(f"Dependency artifact used for code generation no longer exists: {artifact_path}")
        if f"sha256:{sha256_file(artifact_path)}" != expected_sha:
            raise ValueError(stale_message)


def _execution_preflight_error_message(message: str) -> str:
    """Keep API-facing execution approval errors stable after gateway preflight moves earlier."""

    if message.startswith("Graph state does not contain an approved code-review decision"):
        return f"Generated code must be approved before sandbox execution. {message}"
    if message.startswith("Graph code approval is stale"):
        return message.replace("Graph code approval is stale", "Code approval is stale", 1)
    if message.startswith("Generated code changed after graph approval"):
        return message.replace("Generated code changed after graph approval", "Generated code changed after approval", 1)
    return message


def _assert_approved_draft_spec_current(
    dataset_state: DatasetRunState,
    *,
    spec_path: str | Path | None,
    spec_sha256: str | None,
    input_fingerprint_payload: dict[str, Any],
) -> None:
    """Require graph-approved draft spec state before generated code can use it."""

    spec_state = dataset_state.spec_state
    if spec_state.get("status") != "approved" or spec_state.get("decision") != "approve":
        raise ValueError("Approved draft spec must be recorded in graph state before code generation.")
    if not spec_path or not spec_sha256:
        raise ValueError("Approved draft spec code generation requires an approved spec path and hash.")
    approved_path = Path(spec_path)
    if not approved_path.exists() or not approved_path.is_file():
        raise ValueError(f"Approved draft spec does not exist: {approved_path}")
    graph_path = Path(str(spec_state.get("approved_spec_path") or ""))
    if str(graph_path.as_posix()) != str(approved_path.as_posix()):
        raise ValueError("Generated-code spec path does not match graph-approved draft spec.")
    current_sha = f"sha256:{sha256_file(approved_path)}"
    if current_sha != spec_sha256 or current_sha != spec_state.get("approved_spec_sha256"):
        raise ValueError("Approved draft spec changed after graph approval. Review and approve the draft spec again.")
    approved_fingerprint = spec_state.get("input_fingerprint") or {}
    if not isinstance(approved_fingerprint, dict) or not approved_fingerprint.get("digest"):
        raise ValueError("Graph-approved draft spec is missing its input fingerprint. Review and approve the draft spec again.")
    if approved_fingerprint.get("digest") != input_fingerprint_payload.get("digest"):
        diff = compare_fingerprints(approved_fingerprint, input_fingerprint_payload)
        raise ValueError(
            "Graph-approved draft spec is stale because study inputs changed after approval. "
            f"Input diff: added={diff.get('added', [])}; removed={diff.get('removed', [])}; "
            f"changed={diff.get('changed_files', [])}."
        )


def _has_dataset_product_progress(dataset_state: DatasetRunState | None) -> bool:
    if dataset_state is None:
        return False
    if dataset_state.code_state:
        return True
    if dataset_state.execution_state or dataset_state.validation_summary or dataset_state.compare_summary:
        return True
    if dataset_state.artifacts or dataset_state.failures or dataset_state.human_commands:
        return True
    if dataset_state.spec_state:
        return True
    return dataset_state.status not in {"pending", "planned"}


def _mark_dataset_stale_for_input_change(
    dataset_state: DatasetRunState,
    *,
    diff: dict[str, Any],
    fingerprint: dict[str, Any],
) -> None:
    dataset_state.input_fingerprint = fingerprint
    dataset_state.status = "needs_review"
    interrupt_name = "code_review" if dataset_state.code_state else "draft_spec_review"
    interrupt_reason = (
        "Study inputs changed after this dataset code state was created; regenerate code before execution."
        if dataset_state.code_state
        else "Study inputs changed after this dataset spec state was created; review or regenerate the spec before code generation."
    )
    dataset_state.current_interrupt = InterruptState(
        name=interrupt_name,
        dataset=dataset_state.dataset,
        reason=interrupt_reason,
        payload={"input_diff": diff},
    )
    stale_payload = {
        "status": "stale",
        "stale_reason": "Study inputs changed after code generation or review.",
        "input_diff": diff,
    }
    if dataset_state.code_state:
        dataset_state.code_state.update(stale_payload)
    if dataset_state.spec_state:
        dataset_state.spec_state["input_diff"] = diff
        if str(dataset_state.spec_state.get("status") or "").strip() in {"approved", "input_spec_ready", "draft_generated"}:
            dataset_state.spec_state["status"] = "stale"
            dataset_state.spec_state["stale_reason"] = "Study inputs changed after spec review or input confirmation."
    _append_risk_flags(dataset_state, ["inputs_changed_after_dataset_progress"])
    dataset_state.updated_at = utc_now()


def _next_open_dataset_interrupt(
    state: StudyRunState,
    *,
    names: set[str] | None = None,
    exclude_names: set[str] | None = None,
) -> InterruptState | None:
    for dataset in sorted(state.datasets):
        interrupt = state.datasets[dataset].current_interrupt
        if interrupt is None or interrupt.status != "open":
            continue
        if names is not None and interrupt.name not in names:
            continue
        if exclude_names is not None and interrupt.name in exclude_names:
            continue
        if interrupt is not None and interrupt.status == "open":
            return interrupt
    return None


def _roll_up_study_state(
    state: StudyRunState,
    *,
    preferred_interrupt: InterruptState | None = None,
) -> None:
    """Derive study-level status from durable per-dataset graph state."""

    study_interrupt = _open_study_interrupt(state)
    if study_interrupt is not None:
        state.current_interrupt = study_interrupt
        state.status = "needs_review"
        return

    terminal_interrupt = _next_open_dataset_interrupt(state, names={"terminal_failure"})
    if terminal_interrupt is not None:
        state.current_interrupt = terminal_interrupt
        state.status = "terminal_failure"
        return

    if (
        preferred_interrupt is not None
        and preferred_interrupt.status == "open"
        and preferred_interrupt.name != "terminal_failure"
    ):
        state.current_interrupt = preferred_interrupt
    else:
        state.current_interrupt = _next_open_dataset_interrupt(state, exclude_names={"terminal_failure"})

    if state.current_interrupt is not None:
        state.status = "terminal_failure" if state.current_interrupt.name == "terminal_failure" else "needs_review"
        return

    target_statuses = [
        state.datasets[dataset].status
        for dataset in state.target_datasets
        if dataset in state.datasets
    ]
    if not target_statuses:
        state.status = "pending"
        return
    if any(status == "terminal_failure" for status in target_statuses):
        state.status = "terminal_failure"
        return
    completed_statuses = {"completed", "completed_stub"}
    if all(status in completed_statuses for status in target_statuses):
        state.status = "completed"
        return
    if any(status == "failed" for status in target_statuses):
        state.status = "failed"
        return
    if any(status == "needs_review" for status in target_statuses):
        state.status = "needs_review"
        return
    state.status = "running"


def _open_study_interrupt(state: StudyRunState) -> InterruptState | None:
    interrupt = state.current_interrupt
    if interrupt is not None and interrupt.status == "open" and interrupt.dataset is None:
        return interrupt
    return None


def _progress_dataset_order(state: StudyRunState) -> list[str]:
    datasets: list[str] = []
    for dataset in list(state.target_datasets) + sorted(state.datasets):
        normalized = str(dataset).strip().upper()
        if normalized and normalized not in datasets:
            datasets.append(normalized)
    return datasets


def _study_next_action(
    state: StudyRunState,
    datasets: list[dict[str, Any]],
    *,
    output_quality_rollup: dict[str, Any] | None = None,
) -> dict[str, str]:
    if bool(state.dependency_plan.get("plan_stale")) or state.dependency_review_status == "stale":
        return {
            "next_action": "replan_dependencies",
            "action_label": "Study inputs changed. Re-run dependency planning before continuing.",
        }
    if state.dependency_review_status in {"blocked", "warning", "review_required"}:
        return {
            "next_action": "review_dependency_plan",
            "action_label": "Review dependency plan.",
        }
    interrupt = state.current_interrupt
    if interrupt is not None and interrupt.status == "open":
        if interrupt.dataset:
            dataset = interrupt.dataset.strip().upper()
            return {
                "next_action": _action_for_interrupt(interrupt.name),
                "action_label": f"Review {dataset}: {_interrupt_label(interrupt.name)}",
            }
        return {
            "next_action": _action_for_interrupt(interrupt.name),
            "action_label": _interrupt_label(interrupt.name),
        }
    for item in datasets:
        if not item.get("blocked") and item.get("next_action") not in {"complete", "wait_for_plan", "blocked"}:
            return {
                "next_action": str(item.get("next_action") or "continue"),
                "action_label": f"{item['dataset']}: {item.get('action_label') or 'Continue workflow'}",
            }
    if state.status == "completed":
        quality = output_quality_rollup or study_output_quality_rollup(datasets, target_datasets=state.target_datasets)
        completion_quality = str(quality.get("completion_quality") or "")
        if completion_quality == "review_only_complete":
            return {
                "next_action": "review_outputs",
                "action_label": "All planned datasets have review-only/demo outputs. They are not runtime dependency evidence.",
            }
        if completion_quality == "mixed_output_quality_complete":
            return {
                "next_action": "review_outputs",
                "action_label": "Planned datasets are complete, but some outputs are review-only/demo outputs.",
            }
        return {"next_action": "complete", "action_label": "All planned datasets have real runtime outputs."}
    return {"next_action": "prepare_dependency_plan", "action_label": "Prepare or refresh the dependency plan."}


def _dataset_progress_item(state: StudyRunState, dataset: str) -> dict[str, Any]:
    target = dataset.strip().upper()
    dataset_state = state.datasets.get(target)
    block = _blocked_dataset_progress_reason(state, target)
    if dataset_state is None:
        return {
            "dataset": target,
            "status": "pending",
            "next_action": "wait_for_plan",
            "action_label": "Wait for dependency planning.",
            "blocked": bool(block),
            "blocked_reason": block,
            "current_interrupt": None,
            "spec_status": "",
            "code_status": "",
            "execution_status": "",
            "validation_status": "",
            "compare_status": "",
            "output_quality": dataset_output_quality(status="pending"),
            "warnings": [],
            "available_actions": [],
        }
    next_item = _dataset_next_action(dataset_state, blocked_reason=block)
    output_quality = dataset_output_quality(
        status=dataset_state.status,
        code_state=dataset_state.code_state,
        execution_state=dataset_state.execution_state,
        validation_summary=dataset_state.validation_summary,
    )
    return {
        "dataset": target,
        "status": dataset_state.status,
        "next_action": next_item["next_action"],
        "action_label": next_item["action_label"],
        "blocked": bool(block),
        "blocked_reason": block,
        "current_interrupt": _interrupt_payload(dataset_state.current_interrupt),
        "spec_status": str(dataset_state.spec_state.get("status") or ""),
        "code_status": str(dataset_state.code_state.get("status") or ""),
        "execution_status": str(dataset_state.execution_state.get("status") or ""),
        "validation_status": str(
            dataset_state.validation_summary.get("status")
            or dataset_state.execution_state.get("validation_status")
            or ""
        ),
        "compare_status": str(dataset_state.compare_summary.get("status") or ""),
        "output_quality": output_quality,
        "warnings": _dataset_progress_warnings(dataset_state),
        "available_actions": _available_dataset_actions(dataset_state),
    }


def _dataset_next_action(dataset_state: DatasetRunState, *, blocked_reason: str) -> dict[str, str]:
    interrupt = dataset_state.current_interrupt
    terminal_review = dataset_state.execution_state.get("terminal_failure_review")
    has_terminal_review = isinstance(terminal_review, dict)
    if (
        interrupt is not None
        and interrupt.status == "open"
        and not (interrupt.name == "terminal_failure" and has_terminal_review)
    ):
        return {
            "next_action": _action_for_interrupt(interrupt.name),
            "action_label": _interrupt_label(interrupt.name),
        }
    if blocked_reason:
        return {"next_action": "blocked", "action_label": blocked_reason}
    execution_next = str(dataset_state.execution_state.get("next_action") or "").strip()
    if execution_next:
        return {"next_action": execution_next, "action_label": _next_action_label(execution_next)}
    if dataset_state.status == "terminal_failure":
        return {
            "next_action": "review_terminal_failure",
            "action_label": "Review execution diagnostics and choose a controlled follow-up.",
        }
    if dataset_state.status in {"completed", "completed_stub"}:
        output_quality = dataset_output_quality(
            status=dataset_state.status,
            code_state=dataset_state.code_state,
            execution_state=dataset_state.execution_state,
            validation_summary=dataset_state.validation_summary,
        )
        quality_status = str(output_quality.get("quality_status") or "")
        if quality_status in {"structural_stub", "not_real_derivation"}:
            return {
                "next_action": "complete",
                "action_label": "Review-only/demo output is available. It cannot satisfy downstream runtime dependencies.",
            }
        return {"next_action": "complete", "action_label": "Real runtime output is available for review and compare."}
    spec_status = str(dataset_state.spec_state.get("status") or "").strip()
    code_status = str(dataset_state.code_state.get("status") or "").strip()
    execution_status = str(dataset_state.execution_state.get("status") or "").strip()
    if code_status == "approved":
        return {"next_action": "execute_approved_code", "action_label": "Run the approved R code locally."}
    if code_status == "generated":
        return {"next_action": "review_code", "action_label": "Review generated R code before execution."}
    if spec_status == "draft_generated":
        return {"next_action": "review_draft_spec", "action_label": "Review the generated draft spec before code generation."}
    if spec_status in {"input_spec_ready", "approved"}:
        return {"next_action": "generate_code", "action_label": "Generate R code from the approved spec evidence."}
    if spec_status == "stale" or code_status == "stale" or execution_status == "stale":
        return {"next_action": "reconfirm_inputs", "action_label": "Study inputs changed. Reconfirm inputs before continuing."}
    return {"next_action": "finalize_inputs", "action_label": "Confirm uploaded evidence and prepare the spec gate."}


def _available_dataset_actions(dataset_state: DatasetRunState) -> list[dict[str, str]]:
    interrupt = dataset_state.current_interrupt
    review = dataset_state.execution_state.get("terminal_failure_review")
    if (
        interrupt is not None
        and interrupt.name == "terminal_failure"
        and interrupt.status == "open"
        and not isinstance(review, dict)
    ):
        return [dict(item) for item in TERMINAL_FAILURE_REVIEW_ACTIONS]
    return []


def _human_review_queue_items(state: StudyRunState, datasets: list[dict[str, Any]]) -> list[dict[str, Any]]:
    items: list[dict[str, Any]] = []
    seen: set[tuple[str, str, str]] = set()
    study_interrupt = state.current_interrupt if state.current_interrupt is not None and state.current_interrupt.dataset is None else None
    _add_human_review_queue_item(
        items,
        seen,
        study_interrupt,
        scope="study",
        status=state.status,
        reason=_study_interrupt_reason(state),
    )
    if study_interrupt is None and _study_next_action_requires_dependency_review(state):
        _add_human_review_queue_item(
            items,
            seen,
            None,
            scope="study",
            status=state.status or "open",
            source="progress",
            interrupt_name="dependency_review",
            reason=_study_interrupt_reason(state),
        )
    for dataset_progress in datasets:
        dataset = str(dataset_progress.get("dataset") or "").strip().upper()
        current_interrupt = _interrupt_from_payload(dataset_progress.get("current_interrupt"))
        if _interrupt_is_reviewable_dataset_gate(dataset_progress, current_interrupt):
            reviewable_interrupt = current_interrupt
        else:
            reviewable_interrupt = None
        _add_human_review_queue_item(
            items,
            seen,
            reviewable_interrupt,
            scope="dataset",
            dataset=dataset,
            status=str(dataset_progress.get("status") or ""),
            reason=str(dataset_progress.get("action_label") or dataset_progress.get("blocked_reason") or ""),
        )
        interrupt_name = _interrupt_name_for_next_action(str(dataset_progress.get("next_action") or ""))
        if current_interrupt is None and interrupt_name:
            _add_human_review_queue_item(
                items,
                seen,
                None,
                scope="dataset",
                dataset=dataset,
                status=str(dataset_progress.get("status") or "open"),
                source="progress",
                interrupt_name=interrupt_name,
                reason=str(dataset_progress.get("action_label") or dataset_progress.get("blocked_reason") or ""),
            )
    return items


def _study_next_action_requires_dependency_review(state: StudyRunState) -> bool:
    return state.dependency_review_status in {"blocked", "warning", "review_required", "stale"} or bool(
        state.dependency_plan.get("plan_stale")
    )


def _interrupt_is_reviewable_dataset_gate(dataset_progress: dict[str, Any], interrupt: InterruptState | None) -> bool:
    if interrupt is None:
        return False
    if interrupt.name != "terminal_failure":
        return True
    return str(dataset_progress.get("next_action") or "") == "review_terminal_failure"


def _add_human_review_queue_item(
    items: list[dict[str, Any]],
    seen: set[tuple[str, str, str]],
    interrupt: InterruptState | None,
    *,
    scope: str,
    status: str,
    reason: str = "",
    dataset: str = "",
    source: str = "interrupt",
    interrupt_name: str = "",
) -> None:
    normalized_dataset = dataset.strip().upper()
    name = interrupt_name.strip()
    item_source = source
    item_reason = reason.strip()
    item_status = status.strip() or "open"
    if interrupt is not None:
        if interrupt.status != "open":
            return
        name = interrupt.name
        normalized_dataset = (interrupt.dataset or normalized_dataset).strip().upper()
        item_source = "interrupt"
        item_reason = interrupt.reason or item_reason
        item_status = interrupt.status
        scope = "dataset" if normalized_dataset else scope
    if not name:
        return
    key = (normalized_dataset or "study", name, item_source)
    if key in seen:
        return
    seen.add(key)
    items.append(
        {
            "scope": "dataset" if normalized_dataset else "study",
            "dataset": normalized_dataset,
            "name": name,
            "status": item_status,
            "source": item_source,
            "reason": item_reason,
            "action": _action_for_interrupt(name),
            "action_label": _interrupt_label(name),
        }
    )


def _study_interrupt_reason(state: StudyRunState) -> str:
    if state.current_interrupt is not None and state.current_interrupt.reason:
        return state.current_interrupt.reason
    if state.dependency_review_status:
        return f"Dependency review status: {state.dependency_review_status}."
    return ""


def _interrupt_from_payload(payload: Any) -> InterruptState | None:
    if payload is None:
        return None
    if isinstance(payload, InterruptState):
        return payload
    if isinstance(payload, dict):
        try:
            return InterruptState.model_validate(payload)
        except Exception:
            return None
    return None


def _interrupt_name_for_next_action(next_action: str) -> str:
    return {
        "review_dependency_plan": "dependency_review",
        "review_draft_spec": "draft_spec_review",
        "review_code": "code_review",
        "review_terminal_failure": "terminal_failure",
        "resolve_dependency": "dependency_user_action_required",
    }.get(next_action.strip(), "")


def _blocked_dataset_progress_reason(state: StudyRunState, dataset: str) -> str:
    if bool(state.dependency_plan.get("plan_stale")) or state.dependency_review_status == "stale":
        return "Dependency plan is stale because study inputs changed."
    blocked = [
        block
        for block in state.blocked_datasets
        if str(block.get("dataset", "")).strip().upper() == dataset
    ]
    if blocked:
        reasons = [str(block.get("reason") or "blocked") for block in blocked]
        return f"Dependency review required: {'; '.join(reasons)}."
    if state.dependency_review_status in {"blocked", "warning"}:
        return "Study-level dependency review must be resolved before product steps continue."
    blocking_sources = _blocking_review_required_sources(state.dependency_decisions, dataset)
    if blocking_sources:
        return f"Dependency plan requires human review before product steps continue: {', '.join(blocking_sources)}."
    if dataset not in [item.strip().upper() for item in state.runnable_datasets]:
        return "Dataset is not runnable in the current dependency plan."
    return ""


def _blocking_review_required_sources(decisions: list[dict[str, Any]], dataset: str) -> list[str]:
    target = dataset.strip().upper()
    sources: list[str] = []
    for decision in decisions:
        if str(decision.get("dataset", "")).strip().upper() != target:
            continue
        if decision.get("review_required") is not True:
            continue
        source = str(decision.get("source", "")).strip()
        if source == "no_dependency_evidence":
            continue
        sources.append(source or "unknown")
    return sources


def _interrupt_payload(interrupt: InterruptState | None) -> dict[str, Any] | None:
    if interrupt is None:
        return None
    return interrupt.model_dump(mode="json")


def _interrupt_label(name: str) -> str:
    return {
        "dependency_review": "Review dependency plan.",
        "draft_spec_review": "Review draft spec.",
        "code_review": "Review generated R code.",
        "terminal_failure": "Review terminal execution failure.",
        "dependency_user_action_required": "Resolve dependency requirement.",
    }.get(name, "Review required.")


def _action_for_interrupt(name: str) -> str:
    return {
        "dependency_review": "review_dependency_plan",
        "draft_spec_review": "review_draft_spec",
        "code_review": "review_code",
        "terminal_failure": "review_terminal_failure",
        "dependency_user_action_required": "resolve_dependency",
    }.get(name, "review_required")


def _next_action_label(action: str) -> str:
    return {
        "retry_approved_execution": "Retry the already approved execution step.",
        "repair_generated_code": "Regenerate or repair R code before another execution.",
        "revise_approved_spec": "Revise the approved spec before regenerating code.",
        "request_new_study_input": "Upload corrected or additional study inputs.",
        "skip_failed_dataset": "Dataset was marked to be skipped after review.",
        "continue_other_datasets": "Continue with other runnable datasets.",
    }.get(action, "Continue the graph-controlled workflow.")


def _dataset_progress_warnings(dataset_state: DatasetRunState) -> list[str]:
    warnings: list[str] = []
    output_quality = dataset_output_quality(
        status=dataset_state.status,
        code_state=dataset_state.code_state,
        execution_state=dataset_state.execution_state,
        validation_summary=dataset_state.validation_summary,
    )
    for item in output_quality.get("warnings", []):
        warning = str(item).strip()
        if warning and warning not in warnings:
            warnings.append(warning)
    for state_map in [dataset_state.spec_state, dataset_state.code_state, dataset_state.execution_state]:
        raw_warnings = state_map.get("warnings")
        if isinstance(raw_warnings, list):
            for item in raw_warnings:
                warning = str(item).strip()
                if warning and warning not in warnings:
                    warnings.append(warning)
        stale_reason = str(state_map.get("stale_reason") or "").strip()
        if stale_reason and stale_reason not in warnings:
            warnings.append(stale_reason)
    for failure in dataset_state.failures:
        message = str(getattr(failure, "message", "") or "").strip()
        if message and message not in warnings:
            warnings.append(message)
    return warnings


def _terminal_failure_next_action(action: str) -> str:
    return {
        "retry_execution": "retry_approved_execution",
        "repair_code": "repair_generated_code",
        "revise_spec": "revise_approved_spec",
        "request_new_input": "request_new_study_input",
        "skip_dataset": "skip_failed_dataset",
        "continue_other_datasets": "continue_other_datasets",
    }.get(action, "review_diagnostics")


def _terminal_failure_reason(action: str) -> str:
    return {
        "retry_execution": "Human requested a retry after reviewing terminal failure diagnostics.",
        "repair_code": "Human requested generated-code repair after terminal failure diagnostics.",
        "revise_spec": "Human determined the approved spec may need revision.",
        "request_new_input": "Human determined additional or corrected study input is required.",
    }.get(action, "Terminal failure still requires human triage.")


def _assert_terminal_failure_step_allowed(dataset_state: DatasetRunState, *, step: str) -> dict[str, Any] | None:
    interrupt = dataset_state.current_interrupt
    has_terminal_interrupt = interrupt is not None and interrupt.name == "terminal_failure" and interrupt.status == "open"
    status = dataset_state.status
    review = dataset_state.execution_state.get("terminal_failure_review")
    consumed_by = str(dataset_state.execution_state.get("terminal_failure_followup_consumed_by") or "").strip()
    if not isinstance(review, dict):
        if not has_terminal_interrupt and status != "terminal_failure":
            return None
        if step == "execute":
            raise ValueError(
                "Terminal failure must be reviewed before retrying execution. "
                "Record a terminal-failure review decision first."
            )
        raise ValueError("Terminal failure must be reviewed before continuing this dataset.")
    if consumed_by:
        if has_terminal_interrupt or status == "terminal_failure":
            raise ValueError("Terminal failure follow-up is inconsistent. Review the terminal failure again before continuing.")
        return None
    if not has_terminal_interrupt and status not in {"terminal_failure", "pending", "failed", "needs_review"}:
        return None
    action = str(review.get("action") or "").strip().lower()
    allowed_by_step = {
        "generate_code": {"repair_code"},
        "finalize_inputs": {"revise_spec", "request_new_input"},
        "draft_spec": {"revise_spec", "request_new_input"},
        "execute": {"retry_execution"},
    }
    allowed = allowed_by_step.get(step, set())
    if action not in allowed:
        expected = ", ".join(sorted(allowed)) or "a supported terminal-failure action"
        raise ValueError(
            f"Terminal failure review action is {action or 'missing'}, but {step} requires {expected}."
        )
    return {
        "action": action,
        "reviewed_at": review.get("created_at") or review.get("reviewed_at"),
        "next_action": dataset_state.execution_state.get("next_action"),
    }


def _graph_state_path(study_dir: str | Path, run_id: str) -> Path:
    return Path(study_dir) / "runs" / run_id / "graph_state.json"


def _read_json_if_exists(path: str | Path) -> dict[str, Any]:
    item = Path(path)
    if not item.exists() or not item.is_file():
        return {}
    try:
        payload = json.loads(item.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, UnicodeDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def _artifact_path(artifact: Any) -> str | None:
    if artifact is None:
        return None
    path = getattr(artifact, "path", None)
    return str(path) if path else None


def _write_json(path: str | Path, payload: dict[str, Any]) -> Path:
    item = Path(path)
    item.parent.mkdir(parents=True, exist_ok=True)
    item.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")
    return item


def _write_graph_sqlite_checkpoint(
    study_dir: str | Path,
    run_id: str,
    *,
    node: str,
    state: StudyRunState,
) -> None:
    db_path = Path(study_dir) / "runs" / run_id / "graph_checkpoints.sqlite"
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(db_path)
    try:
        conn.execute(
            """
            create table if not exists graph_checkpoints (
                id integer primary key autoincrement,
                created_at text not null,
                node text not null,
                state_json text not null
            )
            """
        )
        conn.execute(
            "insert into graph_checkpoints (created_at, node, state_json) values (?, ?, ?)",
            (
                datetime.now(UTC).isoformat(timespec="seconds").replace("+00:00", "Z"),
                node,
                state.model_dump_json(),
            ),
        )
        conn.commit()
    finally:
        conn.close()


def _native_interrupt_payload(snapshot: Any, *, boundary: str = "native_interrupt_pilot_only") -> dict[str, Any]:
    """Summarize native LangGraph interrupt state without exposing internals as product truth."""

    tasks = list(getattr(snapshot, "tasks", ()) or ())
    interrupts = []
    for task in tasks:
        for item in getattr(task, "interrupts", ()) or ():
            interrupts.append(
                {
                    "id": getattr(item, "id", None),
                    "value": getattr(item, "value", None),
                }
            )
    return {
        "enabled": True,
        "open_interrupt_count": len(interrupts),
        "next_nodes": list(getattr(snapshot, "next", ()) or ()),
        "interrupts": interrupts,
        "boundary": boundary,
    }


def _assert_resume_command_matches_open_interrupt(state: StudyRunState, command: HumanCommand) -> None:
    """Fail closed unless a human command matches the current open interrupt."""

    expected = _matching_open_interrupt(state, command)
    if expected is None:
        target = command.dataset.strip().upper() if command.dataset else "study"
        raise ValueError(
            f"Human command {command.interrupt} for {target} does not match any current open graph interrupt."
        )


def _matching_open_interrupt(state: StudyRunState, command: HumanCommand) -> InterruptState | None:
    if command.dataset:
        dataset_key = command.dataset.strip().upper()
        dataset_state = state.datasets.get(dataset_key)
        if dataset_state is None:
            return None
        interrupt = dataset_state.current_interrupt
        if (
            interrupt is not None
            and interrupt.status == "open"
            and interrupt.name == command.interrupt
            and (interrupt.dataset or "").strip().upper() == dataset_key
        ):
            return interrupt
        return None
    interrupt = state.current_interrupt
    if (
        interrupt is not None
        and interrupt.status == "open"
        and interrupt.dataset is None
        and interrupt.name == command.interrupt
    ):
        return interrupt
    return None


def _artifact_ref(dataset: str, artifact_id: str, role: str, path: str | Path, *, kind: str) -> ArtifactRef:
    artifact_path = Path(path)
    return ArtifactRef(
        artifact_id=f"{artifact_id}_{dataset.lower()}",
        kind=kind,
        path=str(artifact_path.as_posix()),
        sha256=f"sha256:{sha256_file(artifact_path)}",
        dataset=dataset,
        format=artifact_path.suffix.lower().lstrip(".") or "txt",
        role=role,
        metadata={"graph_gateway_recorded": True},
    )


def _default_draft_spec_agent_io(
    *,
    study_id: str,
    run_id: str,
    target: str,
    draft_path: str | Path,
    prompt_path: str | Path | None,
    response_path: str | Path | None,
    variable_count: int,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    output_payload = {
        "record_source": "graph_gateway_default",
        "draft_spec_path": str(Path(draft_path).as_posix()),
        "variable_count": variable_count,
    }
    artifact_ids = [f"draft_spec_{target.lower()}"]
    input_artifact_ids: list[str] = []
    if prompt_path:
        prompt_artifact_id = f"draft_spec_prompt_{target.lower()}"
        artifact_ids.append(prompt_artifact_id)
        input_artifact_ids.append(prompt_artifact_id)
    if response_path:
        response_artifact_id = f"draft_spec_response_{target.lower()}"
        artifact_ids.append(response_artifact_id)
        input_artifact_ids.append(response_artifact_id)
    decision = record_agent_decision(
        agent="spec_agent",
        node="draft_spec_generation",
        decision="draft_spec_generated",
        dataset=target,
        status="needs_review",
        reason="Generated draft spec was recorded and routed to human review.",
        outputs=output_payload,
        risk_flags=["draft_spec_requires_human_review"],
        artifact_ids=artifact_ids,
    )
    node_input = build_agent_node_input(
        agent="spec_agent",
        node="draft_spec_generation",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        task="Record a generated draft ADaM spec and route it to human review.",
        inputs={
            "spec_source": "draft_spec",
            "prompt_path": str(Path(prompt_path).as_posix()) if prompt_path else None,
            "response_path": str(Path(response_path).as_posix()) if response_path else None,
        },
        artifact_ids=input_artifact_ids,
        risk_flags=["draft_spec_requires_human_review"],
    )
    node_output = build_agent_node_output(
        agent="spec_agent",
        node="draft_spec_generation",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="needs_review",
        decision="draft_spec_generated",
        reason=decision["reason"],
        outputs=output_payload,
        risk_flags=["draft_spec_requires_human_review"],
        artifact_ids=artifact_ids,
        agent_decisions=[decision],
    )
    return [decision], [node_input], [node_output]


def _default_input_spec_agent_io(
    *,
    study_id: str,
    run_id: str,
    target: str,
    spec_path: str | Path,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    output_payload = {
        "record_source": "graph_gateway_default",
        "input_spec_path": str(Path(spec_path).as_posix()),
        "next_action": "generate_code",
    }
    decision = record_agent_decision(
        agent="evidence_agent",
        node="input_spec_ready",
        decision="input_spec_ready",
        dataset=target,
        status="pending",
        reason="User-supplied input_spec was accepted as the authoritative spec source.",
        outputs=output_payload,
        artifact_ids=[f"input_spec_{target.lower()}"],
    )
    node_input = build_agent_node_input(
        agent="evidence_agent",
        node="input_spec_ready",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        task="Record a user-supplied input spec as the authoritative derivation source.",
        inputs={"spec_source": "input_spec", "input_spec_path": str(Path(spec_path).as_posix())},
        artifact_ids=[f"input_spec_{target.lower()}"],
    )
    node_output = build_agent_node_output(
        agent="evidence_agent",
        node="input_spec_ready",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="pending",
        decision="input_spec_ready",
        reason=decision["reason"],
        outputs=output_payload,
        artifact_ids=[f"input_spec_{target.lower()}"],
        agent_decisions=[decision],
    )
    return [decision], [node_input], [node_output]


def _default_approved_draft_spec_agent_io(
    *,
    study_id: str,
    run_id: str,
    target: str,
    approved_spec_path: str | Path,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    output_payload = {
        "record_source": "graph_gateway_default",
        "approved_spec_path": str(Path(approved_spec_path).as_posix()),
        "next_action": "generate_code",
    }
    decision = record_agent_decision(
        agent="evidence_agent",
        node="approved_draft_spec_ready",
        decision="approved_draft_spec_ready",
        dataset=target,
        status="pending",
        reason="Current graph-approved draft spec was accepted as the code-generation spec source.",
        outputs=output_payload,
        risk_flags=["uses_approved_draft_spec"],
        artifact_ids=[f"approved_draft_spec_{target.lower()}"],
    )
    node_input = build_agent_node_input(
        agent="evidence_agent",
        node="approved_draft_spec_ready",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        task="Record a current graph-approved draft spec as the code-generation spec source.",
        inputs={"spec_source": "approved_draft_spec", "approved_spec_path": str(Path(approved_spec_path).as_posix())},
        artifact_ids=[f"approved_draft_spec_{target.lower()}"],
        risk_flags=["uses_approved_draft_spec"],
    )
    node_output = build_agent_node_output(
        agent="evidence_agent",
        node="approved_draft_spec_ready",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="pending",
        decision="approved_draft_spec_ready",
        reason=decision["reason"],
        outputs=output_payload,
        risk_flags=["uses_approved_draft_spec"],
        artifact_ids=[f"approved_draft_spec_{target.lower()}"],
        agent_decisions=[decision],
    )
    return [decision], [node_input], [node_output]


def _default_code_generation_agent_io(
    *,
    study_id: str,
    run_id: str,
    target: str,
    code_path: str | Path,
    static_check_path: str | Path | None,
    spec_source: str | None,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    code_payload = {
        "record_source": "graph_gateway_default",
        "code_path": str(Path(code_path).as_posix()),
        "spec_source": spec_source,
        "next_action": "review_code",
    }
    code_decision = record_agent_decision(
        agent="code_agent",
        node="code_generation",
        decision="r_code_generated",
        dataset=target,
        status="needs_review",
        reason="Generated R code was recorded and routed to human code review.",
        outputs=code_payload,
        artifact_ids=[f"generated_code_{target.lower()}"],
    )
    code_input = build_agent_node_input(
        agent="code_agent",
        node="code_generation",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        task="Record generated R code from an approved ADaM spec and route it to human code review.",
        inputs={"spec_source": spec_source},
        artifact_ids=[],
    )
    code_output = build_agent_node_output(
        agent="code_agent",
        node="code_generation",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status="needs_review",
        decision="r_code_generated",
        reason=code_decision["reason"],
        outputs=code_payload,
        artifact_ids=[f"generated_code_{target.lower()}"],
        agent_decisions=[code_decision],
    )
    decisions = [code_decision]
    inputs = [code_input]
    outputs = [code_output]
    if static_check_path:
        static_payload = {
            "record_source": "graph_gateway_default",
            "static_check_path": str(Path(static_check_path).as_posix()),
        }
        static_decision = record_agent_decision(
            agent="static_review_agent",
            node="code_generation",
            decision="static_check_recorded",
            dataset=target,
            status="warning",
            reason="A limited deterministic static-check artifact was recorded before human code review.",
            outputs=static_payload,
            risk_flags=["static_check_limited_scope"],
            artifact_ids=[f"static_check_{target.lower()}"],
        )
        static_input = build_agent_node_input(
            agent="static_review_agent",
            node="code_generation",
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            task="Record limited deterministic static checks before human code review.",
            inputs={"code_path": str(Path(code_path).as_posix())},
            artifact_ids=[f"generated_code_{target.lower()}"],
            risk_flags=["static_check_limited_scope"],
        )
        static_output = build_agent_node_output(
            agent="static_review_agent",
            node="code_generation",
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            status="warning",
            decision="static_check_recorded",
            reason=static_decision["reason"],
            outputs=static_payload,
            risk_flags=["static_check_limited_scope"],
            artifact_ids=[f"static_check_{target.lower()}"],
            agent_decisions=[static_decision],
        )
        decisions.append(static_decision)
        inputs.append(static_input)
        outputs.append(static_output)
    return decisions, inputs, outputs


def _default_code_generation_agent_decisions(
    *,
    target: str,
    code_path: str | Path,
    static_check_path: str | Path | None,
    spec_source: str | None,
) -> list[dict[str, Any]]:
    decisions = [
        record_agent_decision(
            agent="code_agent",
            node="code_generation",
            decision="r_code_generated",
            dataset=target,
            status="needs_review",
            reason="Generated R code was recorded and routed to human code review.",
            outputs={
                "record_source": "graph_gateway_default",
                "code_path": str(Path(code_path).as_posix()),
                "spec_source": spec_source,
                "next_action": "review_code",
            },
        )
    ]
    if static_check_path:
        decisions.append(
            record_agent_decision(
                agent="static_review_agent",
                node="code_generation",
                decision="static_check_recorded",
                dataset=target,
                status="warning",
                reason="A limited deterministic static-check artifact was recorded before human code review.",
                outputs={
                    "record_source": "graph_gateway_default",
                    "static_check_path": str(Path(static_check_path).as_posix()),
                },
                risk_flags=["static_check_limited_scope"],
            )
        )
    return decisions


def _default_execution_agent_io(
    *,
    study_id: str,
    run_id: str,
    target: str,
    execution_state: dict[str, Any],
    validation_summary: dict[str, Any],
    terminal_failure: bool,
    artifacts: list[ArtifactRef],
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    status = "terminal_failure" if terminal_failure else "completed"
    decision_name = "r_execution_terminal_failure" if terminal_failure else "r_execution_completed"
    risk_flags = ["terminal_failure"] if terminal_failure else []
    artifact_ids = [artifact.artifact_id for artifact in artifacts]
    output_payload = {
        "record_source": "graph_gateway_default",
        "terminal_failure": terminal_failure,
        "validation_status": validation_summary.get("status"),
        "output_path": execution_state.get("output_path"),
    }
    decision = record_agent_decision(
        agent="execution_agent",
        node="execute_approved_code",
        decision=decision_name,
        dataset=target,
        status=status,
        reason="Approved generated R code was executed through the graph-owned boundary.",
        outputs=output_payload,
        risk_flags=risk_flags,
        artifact_ids=artifact_ids,
    )
    node_input = build_agent_node_input(
        agent="execution_agent",
        node="execute_approved_code",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        task="Record approved generated R code execution through the configured execution boundary.",
        inputs={
            "code_path": execution_state.get("code_path"),
            "static_check_path": execution_state.get("static_check_path"),
            "expected_output_path": execution_state.get("expected_output_path"),
        },
        artifact_ids=[],
        risk_flags=risk_flags,
    )
    node_output = build_agent_node_output(
        agent="execution_agent",
        node="execute_approved_code",
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        status=status,
        decision=decision_name,
        reason=decision["reason"],
        outputs=output_payload,
        risk_flags=risk_flags,
        artifact_ids=artifact_ids,
        agent_decisions=[decision],
    )
    return [decision], [node_input], [node_output]


def _append_agent_decisions(dataset_state: DatasetRunState, decisions: list[dict[str, Any]]) -> None:
    existing = {
        _agent_record_key(item)
        for item in dataset_state.agent_decisions
        if isinstance(item, dict)
    }
    for decision in decisions:
        normalized = AgentDecision.model_validate(decision).model_dump(mode="json")
        key = _agent_record_key(normalized)
        if key not in existing:
            dataset_state.agent_decisions.append(normalized)
            existing.add(key)


def _append_agent_node_io(
    dataset_state: DatasetRunState,
    *,
    inputs: list[dict[str, Any]],
    outputs: list[dict[str, Any]],
) -> None:
    _append_unique_agent_io_records(dataset_state.agent_node_inputs, inputs, model=AgentNodeInput)
    _append_unique_agent_io_records(dataset_state.agent_node_outputs, outputs, model=AgentNodeOutput)


def _append_unique_agent_io_records(
    existing_records: list[dict[str, Any]],
    new_records: list[dict[str, Any]],
    *,
    model: type[AgentNodeInput] | type[AgentNodeOutput],
) -> None:
    existing_keys = {_agent_io_key(item) for item in existing_records if isinstance(item, dict)}
    for record in new_records:
        if not isinstance(record, dict):
            continue
        try:
            normalized = model.model_validate(record).model_dump(mode="json")
        except ValueError:
            continue
        key = _agent_io_key(normalized)
        if key in existing_keys:
            continue
        existing_records.append(normalized)
        existing_keys.add(key)


def _agent_io_key(record: dict[str, Any]) -> str:
    return _agent_record_key(record)


def _agent_record_key(record: dict[str, Any]) -> str:
    return json.dumps(record, sort_keys=True, separators=(",", ":"), default=str)


def _append_risk_flags(dataset_state: DatasetRunState, flags: list[str]) -> None:
    existing = set(dataset_state.risk_flags)
    for flag in flags:
        normalized = str(flag or "").strip()
        if normalized and normalized not in existing:
            dataset_state.risk_flags.append(normalized)
            existing.add(normalized)


def _sync_study_agent_decisions(state: StudyRunState) -> None:
    existing_decisions = {
        _agent_record_key(item)
        for item in state.agent_decisions
        if isinstance(item, dict)
    }
    for dataset_state in state.datasets.values():
        for decision in dataset_state.agent_decisions:
            try:
                normalized = AgentDecision.model_validate(decision).model_dump(mode="json")
            except ValueError:
                continue
            key = _agent_record_key(normalized)
            if key not in existing_decisions:
                state.agent_decisions.append(normalized)
                existing_decisions.add(key)

    existing_risk_flags = set(state.risk_flags)
    for dataset_state in state.datasets.values():
        for flag in dataset_state.risk_flags:
            normalized = str(flag or "").strip()
            if normalized and normalized not in existing_risk_flags:
                state.risk_flags.append(normalized)
                existing_risk_flags.add(normalized)


def _sync_study_agent_node_io(state: StudyRunState) -> None:
    study_inputs = [
        item
        for item in state.agent_node_inputs
        if isinstance(item, dict) and item.get("dataset") is None
    ]
    study_outputs = [
        item
        for item in state.agent_node_outputs
        if isinstance(item, dict) and item.get("dataset") is None
    ]
    state.agent_node_inputs = []
    state.agent_node_outputs = []
    _append_unique_agent_io_records(state.agent_node_inputs, study_inputs, model=AgentNodeInput)
    _append_unique_agent_io_records(state.agent_node_outputs, study_outputs, model=AgentNodeOutput)
    for dataset in sorted(state.datasets):
        dataset_state = state.datasets[dataset]
        _append_unique_agent_io_records(state.agent_node_inputs, dataset_state.agent_node_inputs, model=AgentNodeInput)
        _append_unique_agent_io_records(state.agent_node_outputs, dataset_state.agent_node_outputs, model=AgentNodeOutput)


def _update_agent_audit_summary(study_dir: Path, state: StudyRunState) -> None:
    """Persist the derived audit-agent summary into state and audit artifacts."""

    artifact_id = f"agent_summary_{state.study_id.lower()}_{state.run_id}"
    summary_path = study_dir / "runs" / state.run_id / "audit" / "agent_summary.json"
    summary = build_agent_audit_summary_from_state(
        state,
        summary_artifact_id=artifact_id,
        summary_path=summary_path,
    )
    artifact = write_agent_audit_summary(summary, path=summary_path, artifact_id=artifact_id)
    state.agent_audit_summary = summary
    _upsert_study_artifact(state, artifact)
    for dataset, dataset_state in state.datasets.items():
        dataset_summary = summary.get("datasets", {}).get(dataset)
        if isinstance(dataset_summary, dict):
            dataset_state.agent_audit_summary = dataset_summary


def _upsert_study_artifact(state: StudyRunState, artifact: ArtifactRef) -> None:
    state.artifacts = [
        existing
        for existing in state.artifacts
        if existing.artifact_id != artifact.artifact_id
    ] + [artifact]


def _upsert_artifact(dataset_state: DatasetRunState, artifact: ArtifactRef) -> None:
    dataset_state.artifacts = [
        existing
        for existing in dataset_state.artifacts
        if existing.artifact_id != artifact.artifact_id
    ] + [artifact]
