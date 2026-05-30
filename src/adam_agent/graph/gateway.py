"""Graph-native gateway for LangGraph-2 workflow entry points."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import UTC, datetime
import json
from pathlib import Path
import sqlite3
from typing import Any

from langgraph.checkpoint.memory import InMemorySaver

from adam_agent.agents import AgentDecision, build_agent_audit_summary_from_state, record_agent_decision, write_agent_audit_summary
from adam_agent.graph.dataset_graph import compile_dataset_graph
from adam_agent.graph.execution import GraphExecutionError, assert_graph_code_review_current
from adam_agent.graph.study_graph import compile_study_graph
from adam_agent.graph.workflow_state import compare_fingerprints, input_fingerprint, project_graph_state_to_workflow
from adam_agent.schemas.graph_state import DatasetRunState, HumanCommand, InterruptState, StudyRunState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary
from adam_agent.schemas.base import utc_now
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.static_rules import StaticRuleError, validate_static_rule_report_artifact


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
class GraphGatewayInputInvalidationResult:
    """Canonical graph runs touched after study input evidence changes."""

    touched_graph_runs: list[str]
    skipped_graph_runs: list[str]


class GraphGateway:
    """Single entry point for starting, resuming, and reading graph runs.

    The first migration step supports graph-native dependency planning. Later
    phases should move dataset-level draft/spec/code/execute gates behind this
    same gateway instead of adding more FastAPI-local state transitions.
    """

    def __init__(self, *, checkpointer: Any | None = None) -> None:
        self._checkpointer = checkpointer or InMemorySaver()
        self._graph = compile_study_graph(checkpointer=self._checkpointer)

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

    def resume(self, *, study_dir: str | Path, graph_state: StudyRunState, command: HumanCommand) -> GraphGatewayResult:
        """Record a human command against a graph-native interrupt.

        This is intentionally conservative in LG2.1: it persists the command
        into canonical state and projection, but does not yet advance dataset
        product nodes. Those node transitions are introduced in LG2.2.
        """

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
        dependency_resolution: list[dict[str, Any]] | None = None,
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
                "execution_mode": "graph_product_prepare",
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
        draft_variables = list(result.get("draft_spec_variables", []))
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
            input_fingerprint_payload=fingerprint,
            agent_decisions=list(result.get("agent_decisions", [])),
            risk_flags=list(result.get("risk_flags", [])),
        )
        projection = self._handoff_dependency_review_to_product_step(
            root=root,
            state=gateway_result.graph_state,
            gate=plan,
            preferred_interrupt=gateway_result.graph_state.datasets[target].current_interrupt,
        )
        return GraphGatewayFinalizeInputsResult(
            graph_state=gateway_result.graph_state,
            workflow_projection=projection,
            spec_source=spec_source or "draft_spec",
            warnings=warnings,
            dependency_review_status=gateway_result.graph_state.dependency_review_status,
            dependency_warnings=list(plan.dependency_warnings),
            draft_spec_path=str(draft_path),
            draft_spec_prompt_path=str(prompt_path),
            draft_spec_response_path=str(response_path),
            draft_spec_variables=draft_variables,
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
        dependency_resolution: list[dict[str, Any]] | None = None,
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
            dependency_resolution=dependency_resolution,
            llm_provider=llm_provider,
            llm_exposure=llm_exposure,
            llm_client_builder=llm_client_builder,
            target_context_builder=target_context_builder,
            rscript_path=rscript_path,
            force_new_draft_spec=True,
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
        input_fingerprint_payload: dict[str, Any] | None = None,
        agent_decisions: list[dict[str, Any]] | None = None,
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
        dependency_resolution: list[dict[str, Any]] | None = None,
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
                "execution_mode": "graph_product_generate_code",
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
            input_fingerprint_payload=input_fingerprint(root),
            agent_decisions=list(result.get("agent_decisions", [])),
            risk_flags=list(result.get("risk_flags", [])),
        )
        projection = self._handoff_dependency_review_to_product_step(
            root=root,
            state=gateway_result.graph_state,
            gate=plan,
            preferred_interrupt=gateway_result.graph_state.datasets[target].current_interrupt,
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
            dependency_warnings=list(plan.dependency_warnings),
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
                "execution_mode": "graph_product_execute",
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
            },
            validation_summary=validation_report,
            artifacts=list((result.get("real_run_artifacts") or {}).values()),
            failures=list(result.get("failure_records", [])),
            input_fingerprint_payload=input_fingerprint(root),
            agent_decisions=list(result.get("agent_decisions", [])),
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
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="compare_reference_output")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_compare_reference_output")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

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
        allowed_actions = {
            "retry_execution",
            "repair_code",
            "revise_spec",
            "request_new_input",
            "skip_dataset",
            "continue_other_datasets",
        }
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
        allowed = {
            "retry_execution",
            "repair_code",
            "revise_spec",
            "request_new_input",
            "skip_dataset",
            "continue_other_datasets",
        }
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
        touched: list[str] = []
        skipped: list[str] = []
        for run_id in self.list_graph_runs(study_dir=root):
            try:
                old_state = self.load_graph_state(study_dir=root, run_id=run_id)
                diff = compare_fingerprints(old_state.input_fingerprint, new_fingerprint)
                self.mark_inputs_changed(study_dir=root, run_id=run_id)
            except (OSError, ValueError, FileNotFoundError):
                skipped.append(run_id)
                continue
            if diff.get("changed"):
                touched.append(run_id)
        return GraphGatewayInputInvalidationResult(
            touched_graph_runs=touched,
            skipped_graph_runs=skipped,
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
            agent_decisions=[
                record_agent_decision(
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
            ],
            risk_flags=[f"dependency_review_{dependency_review_status}"]
            if dependency_review_status != "accepted"
            else [],
        )

    def _persist_graph_state(self, study_dir: str | Path, state: StudyRunState, *, node: str) -> None:
        root = Path(study_dir)
        _sync_study_agent_decisions(state)
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


def _append_agent_decisions(dataset_state: DatasetRunState, decisions: list[dict[str, Any]]) -> None:
    existing = {
        (
            str(item.get("agent")),
            str(item.get("node")),
            str(item.get("decision")),
            str(item.get("dataset")),
            str(item.get("created_at")),
        )
        for item in dataset_state.agent_decisions
        if isinstance(item, dict)
    }
    for decision in decisions:
        normalized = AgentDecision.model_validate(decision).model_dump(mode="json")
        key = (
            str(normalized.get("agent")),
            str(normalized.get("node")),
            str(normalized.get("decision")),
            str(normalized.get("dataset")),
            str(normalized.get("created_at")),
        )
        if key not in existing:
            dataset_state.agent_decisions.append(normalized)
            existing.add(key)


def _append_risk_flags(dataset_state: DatasetRunState, flags: list[str]) -> None:
    existing = set(dataset_state.risk_flags)
    for flag in flags:
        normalized = str(flag or "").strip()
        if normalized and normalized not in existing:
            dataset_state.risk_flags.append(normalized)
            existing.add(normalized)


def _sync_study_agent_decisions(state: StudyRunState) -> None:
    existing_decisions = {
        (
            str(item.get("agent")),
            str(item.get("node")),
            str(item.get("decision")),
            str(item.get("dataset")),
            str(item.get("created_at")),
        )
        for item in state.agent_decisions
        if isinstance(item, dict)
    }
    for dataset_state in state.datasets.values():
        for decision in dataset_state.agent_decisions:
            try:
                normalized = AgentDecision.model_validate(decision).model_dump(mode="json")
            except ValueError:
                continue
            key = (
                str(normalized.get("agent")),
                str(normalized.get("node")),
                str(normalized.get("decision")),
                str(normalized.get("dataset")),
                str(normalized.get("created_at")),
            )
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
