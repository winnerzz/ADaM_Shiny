"""Graph-native gateway for LangGraph-2 workflow entry points."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
import sqlite3
from typing import Any

from langgraph.checkpoint.memory import InMemorySaver

from adam_agent.graph.study_graph import compile_study_graph
from adam_agent.graph.workflow_state import compare_fingerprints, input_fingerprint, project_graph_state_to_workflow
from adam_agent.schemas.graph_state import DatasetRunState, HumanCommand, InterruptState, StudyRunState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary
from adam_agent.schemas.base import utc_now
from adam_agent.tools.artifacts import sha256_file


@dataclass(frozen=True)
class GraphGatewayResult:
    """Graph state plus its UI projection."""

    graph_state: StudyRunState
    workflow_projection: dict[str, Any]


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
        next_state.current_interrupt = None
        next_state.status = "running" if command.action == "approve" else "needs_review"
        next_state.updated_at = utc_now()
        if command.dataset:
            dataset_key = command.dataset.strip().upper()
            dataset_state = next_state.datasets.get(dataset_key)
            if dataset_state is not None:
                dataset_state.human_commands.append(command)
                dataset_state.current_interrupt = None if command.action == "approve" else dataset_state.current_interrupt
                dataset_state.updated_at = utc_now()
        self._persist_graph_state(study_dir, next_state, node=f"resume_{command.interrupt}")
        projection = project_graph_state_to_workflow(study_dir, next_state, node=f"graph_gateway_resume_{command.interrupt}")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

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
        next_state.current_interrupt = None
        next_state.status = "running" if command.action == "approve" else "needs_review"
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="code_review")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_code_review")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

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
        if recorded_static_sha and not static_check_sha256:
            raise ValueError("Static-check artifact recorded during code generation is missing. Regenerate code before review.")
        if static_check_sha256 and recorded_static_sha and static_check_sha256 != recorded_static_sha:
            raise ValueError("Static-check artifact changed after graph code generation. Regenerate code before review.")
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
        dataset_state.code_state.update(
            {
                "status": "generated",
                "code_path": str(Path(code_path).as_posix()),
                "code_sha256": code_sha256,
                "static_check_path": str(Path(static_check_path).as_posix()) if static_check_path else None,
                "static_check_sha256": static_check_sha256,
                "spec_source": spec_source,
                "spec_path": str(Path(spec_path).as_posix()) if spec_path else None,
                "spec_sha256": spec_sha256,
                "dependency_artifacts": dependency_artifacts or [],
                "input_fingerprint": fingerprint,
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
        if static_check_path:
            _upsert_artifact(dataset_state, _artifact_ref(target, "static_check", "audit", static_check_path, kind="tool_log"))
        next_state.datasets[target] = dataset_state
        next_state.current_interrupt = InterruptState(
            name="code_review",
            dataset=target,
            reason="Generated R code must be reviewed before local R execution.",
            payload={"code_path": str(Path(code_path).as_posix())},
        )
        next_state.status = "needs_review"
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="code_generation")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_code_generation")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

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
        terminal_failure = bool(execution_state.get("terminal_failure"))
        dataset_state.execution_state.update(execution_state)
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
        next_state.current_interrupt = dataset_state.current_interrupt
        next_state.status = "terminal_failure" if terminal_failure else "running"
        next_state.updated_at = utc_now()
        self._persist_graph_state(root, next_state, node="execute_approved_code")
        projection = project_graph_state_to_workflow(root, next_state, node="graph_gateway_execute_approved_code")
        return GraphGatewayResult(graph_state=next_state, workflow_projection=projection)

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
        )

    def _persist_graph_state(self, study_dir: str | Path, state: StudyRunState, *, node: str) -> None:
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
                preserved.input_fingerprint = merged.input_fingerprint
                preserved.status = "needs_review"
                preserved.current_interrupt = InterruptState(
                    name="code_review",
                    dataset=dataset,
                    reason="Study inputs changed after this dataset product state was created; regenerate code before execution.",
                    payload={"input_diff": diff},
                )
                preserved.code_state.update(
                    {
                        "status": "stale",
                        "stale_reason": "Study inputs changed after code generation or review.",
                        "input_diff": diff,
                    }
                )
            merged.datasets[dataset] = preserved
            if dataset not in merged.target_datasets:
                merged.target_datasets.append(dataset)
            if dataset not in merged.runnable_datasets and dataset in existing.runnable_datasets:
                merged.runnable_datasets.append(dataset)
        if existing.current_interrupt is not None and existing.current_interrupt.dataset:
            dataset = existing.current_interrupt.dataset.strip().upper()
            if _has_dataset_product_progress(merged.datasets.get(dataset)):
                merged.current_interrupt = existing.current_interrupt
                merged.status = existing.status
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


def _graph_state_path(study_dir: str | Path, run_id: str) -> Path:
    return Path(study_dir) / "runs" / run_id / "graph_state.json"


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


def _upsert_artifact(dataset_state: DatasetRunState, artifact: ArtifactRef) -> None:
    dataset_state.artifacts = [
        existing
        for existing in dataset_state.artifacts
        if existing.artifact_id != artifact.artifact_id
    ] + [artifact]
