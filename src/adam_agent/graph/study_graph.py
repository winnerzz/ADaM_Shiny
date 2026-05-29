"""Study-level LangGraph skeleton."""

from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor
import json
from pathlib import Path
from typing import Any

from langgraph.graph import END, START, StateGraph

from adam_agent.graph.dataset_graph import compile_dataset_graph
from adam_agent.graph.dependency_resolution import (
    approved_dependency_targets,
    available_dependency_targets,
    blocked_dependency_targets,
    missing_dependency_blocks,
    resolve_dependency_availability,
    unresolved_dependency_targets,
)
from adam_agent.graph.dependencies import plan_dataset_dependencies
from adam_agent.graph.state import DatasetGraphState, DatasetTask, StudyGraphState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.states import DatasetResultSummary
from adam_agent.tools.artifacts import sha256_file


def initialize_study(state: StudyGraphState) -> StudyGraphState:
    """Initialize the study-level run."""

    return {
        "status": "running",
        "dataset_results": [],
        "blocked_datasets": [],
        "audit_artifacts": [],
    }


def plan_datasets(state: StudyGraphState) -> StudyGraphState:
    """Create the MVP dependency plan."""

    plan = plan_dataset_dependencies(state.get("target_datasets"), study_dir=state.get("study_dir") or None)
    scenarios = state.get("stub_scenarios", {})
    approved_dependency_datasets = state.get("approved_dependency_datasets", [])
    resolution_scope = _resolution_scope_datasets(plan.requested_datasets, approved_dependency_datasets)
    dependency_resolutions = resolve_dependency_availability(
        plan.dependencies,
        requested_datasets=plan.requested_datasets,
        resolution_scope_datasets=resolution_scope,
        study_dir=state.get("study_dir") or None,
        run_id=state["run_id"],
        approved_dependency_datasets=approved_dependency_datasets,
    )
    dependency_resolution_dicts = [record.as_dict() for record in dependency_resolutions]
    requested_set = set(plan.requested_datasets)
    satisfied_dependency_datasets = available_dependency_targets(dependency_resolutions)
    runnable_datasets = _runnable_datasets(
        plan.target_datasets,
        requested_set,
        dependency_resolutions,
        dependencies=plan.dependencies,
        satisfied_dependency_datasets=satisfied_dependency_datasets,
    )

    foundation_tasks = [
        _make_dataset_task(
            state,
            dataset,
            scenarios.get(dataset, "success"),
            _dependency_status(plan.dependencies.get(dataset, [])),
            _dependency_resolution_for_dataset(dataset, dependency_resolution_dicts),
        )
        for dataset in plan.foundation_datasets
        if dataset in runnable_datasets
    ]
    downstream_tasks = [
        _make_dataset_task(
            state,
            dataset,
            scenarios.get(dataset, "code_error_then_success" if dataset == "ADAE" else "success"),
            _dependency_status(plan.dependencies.get(dataset, [])),
            _dependency_resolution_for_dataset(dataset, dependency_resolution_dicts),
        )
        for dataset in plan.downstream_datasets
        if dataset in runnable_datasets
    ]
    unsupported_results = [
        DatasetResultSummary(
            dataset=dataset,
            status="failed",
            validation_status="unsupported_dataset",
            compare_status="not_run_stub",
            failure_ids=["unsupported_dataset"],
        )
        for dataset in plan.unsupported_datasets
    ]
    unsupported_blocked = [
        {
            "dataset": dataset,
            "reason": "unsupported_dataset",
            "blocked_by": "study_planner",
        }
        for dataset in plan.unsupported_datasets
    ]
    direct_dependency_blocks = missing_dependency_blocks(
        dependency_resolutions,
        reportable_datasets=resolution_scope,
    )
    dependency_blocks = direct_dependency_blocks + blocked_dependency_targets(
        target_datasets=plan.target_datasets,
        candidate_datasets=plan.requested_datasets,
        runnable_datasets=runnable_datasets,
        direct_blocks=direct_dependency_blocks,
        dependencies=plan.dependencies,
        satisfied_dependency_datasets=satisfied_dependency_datasets,
    )
    dependency_blocked_results = [
        DatasetResultSummary(
            dataset=block["dataset"],
            status="failed",
            validation_status=block["reason"],
            compare_status="not_run_stub",
            failure_ids=[block["reason"]],
        )
        for block in dependency_blocks
    ]

    planned_state: StudyGraphState = {
        "requested_datasets": plan.requested_datasets,
        "target_datasets": plan.target_datasets,
        "auto_added_datasets": plan.auto_added_datasets,
        "runnable_datasets": runnable_datasets,
        "unsupported_datasets": plan.unsupported_datasets,
        "foundation_datasets": plan.foundation_datasets,
        "downstream_datasets": plan.downstream_datasets,
        "dependency_graph": plan.dependency_graph,
        "dataset_dependencies": plan.dependencies,
        "dependency_decisions": [decision.as_dict() for decision in plan.decisions],
        "dependency_resolution": dependency_resolution_dicts,
        "dependency_action_required": bool(dependency_blocks),
        "dependency_evidence": plan.evidence,
        "dependency_evidence_records": [record.as_dict() for record in plan.evidence_records],
        "dependency_planning_warnings": plan.planning_warnings,
        "execution_batches": _filter_execution_batches(plan.execution_batches, runnable_datasets),
        "satisfied_dependency_datasets": satisfied_dependency_datasets,
        "dataset_tasks": foundation_tasks + downstream_tasks,
        "downstream_tasks": downstream_tasks,
        "dataset_results": unsupported_results + dependency_blocked_results,
        "blocked_datasets": unsupported_blocked + dependency_blocks,
    }
    if state.get("graph_gateway_mode") == "plan_only":
        planned_state["status"] = "needs_review" if _needs_dependency_review(planned_state) else "planned"
        planned_state["current_interrupt"] = (
            "dependency_review" if _needs_dependency_review(planned_state) else None
        )
    return planned_state


def run_dependency_batches(state: StudyGraphState) -> StudyGraphState:
    """Run dependency batches and block only datasets with failed dependencies."""

    task_by_dataset = {task["dataset"]: task for task in state.get("dataset_tasks", [])}
    dependencies = state.get("dataset_dependencies", {})
    dataset_results: list[DatasetResultSummary] = []
    audit_artifacts: list[ArtifactRef] = []
    agent_decisions: list[dict[str, object]] = []
    risk_flags: list[str] = []
    blocked_datasets = []
    completed: set[str] = set()
    failed: set[str] = set()
    satisfied: set[str] = set(state.get("satisfied_dependency_datasets", []))

    for batch in state.get("execution_batches", []):
        runnable: list[DatasetTask] = []
        for dataset in batch:
            failed_dependencies = [dependency for dependency in dependencies.get(dataset, []) if dependency in failed]
            missing_dependencies = [
                dependency
                for dependency in dependencies.get(dataset, [])
                if dependency not in completed and dependency not in failed and dependency not in satisfied
            ]
            if failed_dependencies or missing_dependencies:
                blocked_by = failed_dependencies or missing_dependencies
                blocked_record = {
                    "dataset": dataset,
                    "reason": "blocked_by_dependency",
                    "blocked_by": ",".join(blocked_by),
                }
                blocked_datasets.append(blocked_record)
                dataset_results.append(
                    DatasetResultSummary(
                        dataset=dataset,
                        status="failed",
                        validation_status=blocked_record["reason"],
                        compare_status="not_run_stub",
                        failure_ids=[blocked_record["reason"]],
                    )
                )
                failed.add(dataset)
                continue

            task = task_by_dataset.get(dataset)
            if task:
                runnable.append(task)

        for result in _run_dataset_tasks(runnable):
            summary = result["summary"]
            dataset_results.append(summary)
            audit_artifacts.extend(result.get("audit_artifacts", []))
            agent_decisions.extend(result.get("agent_decisions", []))
            risk_flags.extend(result.get("risk_flags", []))
            if summary.status in {"completed", "completed_stub"}:
                completed.add(summary.dataset)
            else:
                failed.add(summary.dataset)

    return {
        "dataset_results": dataset_results,
        "audit_artifacts": audit_artifacts,
        "agent_decisions": agent_decisions,
        "risk_flags": risk_flags,
        "blocked_datasets": blocked_datasets,
    }


def reduce_dataset_results(state: StudyGraphState) -> StudyGraphState:
    """Summarize dataset outcomes into study-level status."""

    results = state.get("dataset_results", [])
    status = "completed"
    if any(result.status == "failed" for result in results):
        status = "failed"
    return {"status": status}


def write_audit_manifest_stub(state: StudyGraphState) -> StudyGraphState:
    """Represent a study-level audit manifest as an artifact reference."""

    planning_artifacts = _write_dependency_planning_artifacts(state)
    artifact = _write_study_audit_manifest(
        state,
        planning_artifacts=planning_artifacts,
        audit_artifacts=state.get("audit_artifacts", [])
        + [planning_artifacts["plan_artifact"], planning_artifacts["review_artifact"]],
    )
    return {
        "audit_manifest": artifact,
        "audit_artifacts": [
            planning_artifacts["plan_artifact"],
            planning_artifacts["review_artifact"],
            artifact,
        ],
        "dependency_review_status": planning_artifacts["review_status"],
        "dependency_plan_artifact": planning_artifacts["plan_artifact"],
        "dependency_review_artifact": planning_artifacts["review_artifact"],
    }


def _write_study_audit_manifest(
    state: StudyGraphState,
    *,
    planning_artifacts: dict[str, Any],
    audit_artifacts: list[ArtifactRef],
) -> ArtifactRef:
    """Write the study-level audit manifest when a study directory is available."""

    metadata = {
        "stub": False,
        "manifest_scope": "study",
        "requested_datasets": state.get("requested_datasets", []),
        "target_datasets": state.get("target_datasets", []),
        "auto_added_datasets": state.get("auto_added_datasets", []),
        "runnable_datasets": state.get("runnable_datasets", []),
        "unsupported_datasets": state.get("unsupported_datasets", []),
        "datasets": [result.dataset for result in state.get("dataset_results", [])],
        "dataset_dependencies": state.get("dataset_dependencies", {}),
        "dependency_decisions": state.get("dependency_decisions", []),
        "dependency_resolution": state.get("dependency_resolution", []),
        "dependency_action_required": state.get("dependency_action_required", False),
        "dependency_evidence": state.get("dependency_evidence", ""),
        "dependency_evidence_records": state.get("dependency_evidence_records", []),
        "dependency_planning_warnings": state.get("dependency_planning_warnings", []),
        "execution_batches": state.get("execution_batches", []),
        "blocked_datasets": state.get("blocked_datasets", []),
        "dependency_review_status": planning_artifacts["review_status"],
        "dependency_plan_artifact_id": planning_artifacts["plan_artifact"].artifact_id,
        "dependency_review_artifact_id": planning_artifacts["review_artifact"].artifact_id,
        "agent_decisions": state.get("agent_decisions", []),
        "risk_flags": state.get("risk_flags", []),
    }
    study_dir = state.get("study_dir")
    relative_path = f"runs/{state['run_id']}/audit/manifest.json"

    if study_dir:
        manifest_path = Path(study_dir) / "runs" / state["run_id"] / "audit" / "manifest.json"
        manifest_path.parent.mkdir(parents=True, exist_ok=True)
        payload = {
            "study_id": state["study_id"],
            "run_id": state["run_id"],
            "status": state.get("status", "unknown"),
            "phase": "phase7_study_orchestration",
            "artifacts": [artifact.model_dump(mode="json") for artifact in audit_artifacts],
            **metadata,
        }
        manifest_path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")
        return ArtifactRef(
            artifact_id=f"manifest_{state['study_id'].lower()}_{state['run_id']}_study",
            kind="audit_manifest",
            path=str(manifest_path.as_posix()),
            sha256=f"sha256:{sha256_file(manifest_path)}",
            format="json",
            role="audit",
            metadata=metadata,
        )

    return ArtifactRef(
        artifact_id=f"manifest_{state['study_id'].lower()}_{state['run_id']}_study_stub",
        kind="audit_manifest",
        path=relative_path,
        format="json",
        role="audit",
        metadata={**metadata, "stub": True},
    )


def _write_dependency_planning_artifacts(state: StudyGraphState) -> dict[str, Any]:
    """Write dependency planning artifacts when a study directory is available."""

    review_status = _dependency_review_status(state)
    plan_payload = _dependency_plan_payload(state, review_status)
    review_markdown = _dependency_review_markdown(state, review_status)
    study_dir = state.get("study_dir")
    relative_plan_path = f"runs/{state['run_id']}/planning/dependency_plan.json"
    relative_review_path = f"runs/{state['run_id']}/planning/dependency_review.md"

    if study_dir:
        planning_dir = Path(study_dir) / "runs" / state["run_id"] / "planning"
        planning_dir.mkdir(parents=True, exist_ok=True)
        plan_path = planning_dir / "dependency_plan.json"
        review_path = planning_dir / "dependency_review.md"
        plan_path.write_text(json.dumps(plan_payload, indent=2, sort_keys=True), encoding="utf-8")
        review_path.write_text(review_markdown, encoding="utf-8")
        plan_artifact = _planning_artifact_ref(
            state,
            artifact_id=f"dependency_plan_{state['study_id'].lower()}_{state['run_id']}",
            path=plan_path,
            format="json",
            review_status=review_status,
        )
        review_artifact = _planning_artifact_ref(
            state,
            artifact_id=f"dependency_review_{state['study_id'].lower()}_{state['run_id']}",
            path=review_path,
            format="md",
            review_status=review_status,
        )
    else:
        plan_artifact = _planning_artifact_ref(
            state,
            artifact_id=f"dependency_plan_{state['study_id'].lower()}_{state['run_id']}",
            path=relative_plan_path,
            format="json",
            review_status=review_status,
        )
        review_artifact = _planning_artifact_ref(
            state,
            artifact_id=f"dependency_review_{state['study_id'].lower()}_{state['run_id']}",
            path=relative_review_path,
            format="md",
            review_status=review_status,
        )

    return {
        "review_status": review_status,
        "plan_artifact": plan_artifact,
        "review_artifact": review_artifact,
    }


def _dependency_plan_payload(state: StudyGraphState, review_status: str) -> dict[str, Any]:
    return {
        "study_id": state["study_id"],
        "run_id": state["run_id"],
        "requested_datasets": state.get("requested_datasets", []),
        "target_datasets": state.get("target_datasets", []),
        "auto_added_datasets": state.get("auto_added_datasets", []),
        "unsupported_datasets": state.get("unsupported_datasets", []),
        "foundation_datasets": state.get("foundation_datasets", []),
        "downstream_datasets": state.get("downstream_datasets", []),
        "runnable_datasets": state.get("runnable_datasets", []),
        "dataset_dependencies": state.get("dataset_dependencies", {}),
        "dependency_graph": state.get("dependency_graph", {}),
        "dependency_decisions": state.get("dependency_decisions", []),
        "dependency_resolution": state.get("dependency_resolution", []),
        "dependency_action_required": state.get("dependency_action_required", False),
        "dependency_evidence": state.get("dependency_evidence", ""),
        "dependency_evidence_records": state.get("dependency_evidence_records", []),
        "dependency_planning_warnings": state.get("dependency_planning_warnings", []),
        "execution_batches": state.get("execution_batches", []),
        "blocked_datasets": state.get("blocked_datasets", []),
        "review_status": review_status,
        "review_status_reason": _dependency_review_status_reason(state, review_status),
    }


def _dependency_review_markdown(state: StudyGraphState, review_status: str) -> str:
    lines = [
        f"# Dependency Plan Review - {state['study_id']} / {state['run_id']}",
        "",
        f"Review status: {review_status}",
        "",
        "## Requested Datasets",
        _csv_or_none(state.get("requested_datasets", [])),
        "",
        "## Final Target Datasets",
        _csv_or_none(state.get("target_datasets", [])),
        "",
        "## Auto-Added Datasets",
        _csv_or_none(state.get("auto_added_datasets", [])),
        "",
        "## Execution Batches",
    ]
    batches = state.get("execution_batches", [])
    if batches:
        for index, batch in enumerate(batches, start=1):
            lines.append(f"- Batch {index}: {_csv_or_none(batch)}")
    else:
        lines.append("- None")

    lines.extend(["", "## Dataset Dependencies"])
    dependencies = state.get("dataset_dependencies", {})
    if dependencies:
        for dataset in state.get("target_datasets", sorted(dependencies)):
            if dataset in dependencies:
                lines.append(f"- {dataset}: {_csv_or_none(dependencies.get(dataset, []))}")
    else:
        lines.append("- None")

    lines.extend(["", "## Dependency Resolution"])
    resolutions = state.get("dependency_resolution", [])
    if resolutions:
        for record in resolutions:
            target = record.get("target_dataset", "UNKNOWN")
            required = record.get("required_dataset", "UNKNOWN")
            status = record.get("resolution_status", "unknown")
            artifact = record.get("artifact_path") or "None"
            reason = record.get("reason", "")
            lines.append(f"- {target} requires {required}: status={status}; artifact={artifact}; reason={reason}")
    else:
        lines.append("- None")

    lines.extend(["", "## Decisions"])
    decisions = state.get("dependency_decisions", [])
    if decisions:
        for decision in decisions:
            dataset = decision.get("dataset", "UNKNOWN")
            source = decision.get("source", "unknown")
            confidence = decision.get("confidence", "unknown")
            review_required = decision.get("review_required", "unknown")
            reason = decision.get("reason", "")
            dependencies_text = _csv_or_none(decision.get("dependencies", []))
            lines.append(
                f"- {dataset}: dependencies={dependencies_text}; source={source}; "
                f"confidence={confidence}; review_required={review_required}; reason={reason}"
            )
    else:
        lines.append("- None")

    lines.extend(["", "## Warnings"])
    warnings = state.get("dependency_planning_warnings", [])
    if warnings:
        for warning in warnings:
            lines.append(f"- {warning}")
    else:
        lines.append("- None")

    lines.extend(["", "## Unsupported Datasets"])
    unsupported = state.get("unsupported_datasets", [])
    if unsupported:
        for dataset in unsupported:
            lines.append(f"- {dataset}")
    else:
        lines.append("- None")

    lines.extend(["", "## Human Review Meaning"])
    lines.append(_dependency_review_status_reason(state, review_status))
    lines.append("")
    return "\n".join(lines)


def _dependency_review_status(state: StudyGraphState) -> str:
    if state.get("unsupported_datasets"):
        return "blocked"
    if state.get("dependency_action_required"):
        return "blocked"
    if state.get("dependency_planning_warnings"):
        return "warning"
    if any(decision.get("review_required") for decision in state.get("dependency_decisions", [])):
        return "review_required"
    return "accepted"


def _needs_dependency_review(state: StudyGraphState) -> bool:
    return _dependency_review_status(state) in {"blocked", "warning", "review_required"}


def route_after_plan(state: StudyGraphState) -> str:
    """Allow graph-native gateway calls to stop after planning."""

    if state.get("graph_gateway_mode") == "plan_only":
        return "stop_after_plan"
    return "run_batches"


def _dependency_review_status_reason(state: StudyGraphState, review_status: str) -> str:
    if review_status == "blocked":
        return "Unsupported targets or unresolved dependency requirements need user action before a full run can be considered valid."
    if review_status == "warning":
        return "The plan can run, but dependency evidence has warnings that should be reviewed before trusting the study-level plan."
    if review_status == "review_required":
        return "At least one dependency decision uses MVP fallback or file-derived evidence that should be checked by a human reviewer."
    return "No dependency warning or unsupported dataset was found in this MVP planning pass."


def _planning_artifact_ref(
    state: StudyGraphState,
    *,
    artifact_id: str,
    path: str | Path,
    format: str,
    review_status: str,
) -> ArtifactRef:
    path_value = str(Path(path).as_posix()) if isinstance(path, Path) else path
    sha256 = None
    if isinstance(path, Path) and path.exists():
        sha256 = f"sha256:{sha256_file(path)}"
    return ArtifactRef(
        artifact_id=artifact_id,
        kind="audit_manifest",
        path=path_value,
        sha256=sha256,
        format=format,
        role="audit",
        metadata={
            "planning_artifact": True,
            "review_status": review_status,
            "requested_datasets": state.get("requested_datasets", []),
            "target_datasets": state.get("target_datasets", []),
        },
    )


def _csv_or_none(values: object) -> str:
    if not values:
        return "None"
    if isinstance(values, list):
        return ", ".join(str(value) for value in values) if values else "None"
    return str(values)


def _runnable_datasets(
    target_datasets: list[str],
    requested_datasets: set[str],
    dependency_resolutions: list[Any],
    *,
    dependencies: dict[str, list[str]],
    satisfied_dependency_datasets: list[str],
) -> list[str]:
    approved_dependencies = set(approved_dependency_targets(dependency_resolutions))
    unresolved_targets = set(unresolved_dependency_targets(dependency_resolutions))
    candidate_datasets = requested_datasets | approved_dependencies
    satisfied = set(satisfied_dependency_datasets)
    runnable = []
    changed = True
    while changed:
        changed = False
        for dataset in target_datasets:
            if dataset in runnable or dataset not in candidate_datasets or dataset in unresolved_targets:
                continue
            required = dependencies.get(dataset, [])
            if all(dependency in satisfied or dependency in runnable for dependency in required):
                runnable.append(dataset)
                changed = True
    return runnable


def _resolution_scope_datasets(requested_datasets: list[str], approved_dependency_datasets: list[str]) -> list[str]:
    scope = []
    for dataset in requested_datasets + approved_dependency_datasets:
        normalized = str(dataset).strip().upper()
        if normalized and normalized not in scope:
            scope.append(normalized)
    return scope


def _dependency_resolution_for_dataset(
    dataset: str,
    dependency_resolution: list[dict[str, object]],
) -> list[dict[str, object]]:
    target = dataset.strip().upper()
    return [
        record
        for record in dependency_resolution
        if str(record.get("target_dataset", "")).strip().upper() == target
    ]


def _filter_execution_batches(execution_batches: list[list[str]], runnable_datasets: list[str]) -> list[list[str]]:
    runnable_set = set(runnable_datasets)
    filtered = []
    for batch in execution_batches:
        runnable_batch = [dataset for dataset in batch if dataset in runnable_set]
        if runnable_batch:
            filtered.append(runnable_batch)
    return filtered


def _make_dataset_task(
    state: StudyGraphState,
    dataset: str,
    scenario: str,
    dependency_status: str,
    dependency_resolution: list[dict[str, object]],
) -> DatasetTask:
    return {
        "study_id": state["study_id"],
        "run_id": state["run_id"],
        "dataset": dataset,
        "stub_scenario": scenario,
        "dependency_status": dependency_status,
        "max_repair_attempts": 3,
        "execution_mode": state.get("execution_mode", "stub"),
        "study_dir": state.get("study_dir", ""),
        "rscript_path": state.get("rscript_path", ""),
        "dependency_resolution": dependency_resolution,
        "llm_exposure": state.get("llm_exposure", {}),
        "llm_provider": state.get("llm_provider", {}),
    }


def _dependency_status(dependencies: list[str]) -> str:
    if not dependencies:
        return "foundation"
    if dependencies == ["ADSL"]:
        return "depends_on_adsl"
    return "depends_on_adam"


def _run_dataset_tasks(tasks: list[DatasetTask]) -> list[DatasetGraphState]:
    if not tasks:
        return []
    if len(tasks) == 1:
        return [_invoke_dataset_task(tasks[0])]
    with ThreadPoolExecutor(max_workers=len(tasks)) as executor:
        return list(executor.map(_invoke_dataset_task, tasks))


def _invoke_dataset_task(task: DatasetTask) -> DatasetGraphState:
    dataset_graph = compile_dataset_graph()
    return dataset_graph.invoke(task)


def build_study_graph():
    """Build the study-level skeleton graph."""

    graph = StateGraph(StudyGraphState)
    graph.add_node("initialize_study", initialize_study)
    graph.add_node("plan_datasets", plan_datasets)
    graph.add_node("run_dependency_batches", run_dependency_batches)
    graph.add_node("reduce_dataset_results", reduce_dataset_results)
    graph.add_node("write_audit_manifest_stub", write_audit_manifest_stub)

    graph.add_edge(START, "initialize_study")
    graph.add_edge("initialize_study", "plan_datasets")
    graph.add_conditional_edges(
        "plan_datasets",
        route_after_plan,
        {
            "stop_after_plan": END,
            "run_batches": "run_dependency_batches",
        },
    )
    graph.add_edge("run_dependency_batches", "reduce_dataset_results")
    graph.add_edge("reduce_dataset_results", "write_audit_manifest_stub")
    graph.add_edge("write_audit_manifest_stub", END)
    return graph


def compile_study_graph(checkpointer=None):
    """Compile the study-level graph with an optional parent checkpointer."""

    return build_study_graph().compile(checkpointer=checkpointer, name="study_graph")
