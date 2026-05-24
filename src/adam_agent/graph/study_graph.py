"""Study-level LangGraph skeleton."""

from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor

from langgraph.graph import END, START, StateGraph

from adam_agent.graph.dataset_graph import compile_dataset_graph
from adam_agent.graph.dependencies import plan_dataset_dependencies
from adam_agent.graph.state import DatasetGraphState, DatasetTask, StudyGraphState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.states import DatasetResultSummary


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

    foundation_tasks = [
        _make_dataset_task(state, dataset, scenarios.get(dataset, "success"), _dependency_status(plan.dependencies.get(dataset, [])))
        for dataset in plan.foundation_datasets
    ]
    downstream_tasks = [
        _make_dataset_task(
            state,
            dataset,
            scenarios.get(dataset, "code_error_then_success" if dataset == "ADAE" else "success"),
            _dependency_status(plan.dependencies.get(dataset, [])),
        )
        for dataset in plan.downstream_datasets
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

    return {
        "requested_datasets": plan.requested_datasets,
        "target_datasets": plan.target_datasets,
        "auto_added_datasets": plan.auto_added_datasets,
        "unsupported_datasets": plan.unsupported_datasets,
        "foundation_datasets": plan.foundation_datasets,
        "downstream_datasets": plan.downstream_datasets,
        "dependency_graph": plan.dependency_graph,
        "dataset_dependencies": plan.dependencies,
        "dependency_decisions": [decision.as_dict() for decision in plan.decisions],
        "dependency_evidence": plan.evidence,
        "dependency_evidence_records": [record.as_dict() for record in plan.evidence_records],
        "dependency_planning_warnings": plan.planning_warnings,
        "execution_batches": plan.execution_batches,
        "dataset_tasks": foundation_tasks + downstream_tasks,
        "downstream_tasks": downstream_tasks,
        "dataset_results": unsupported_results,
        "blocked_datasets": unsupported_blocked,
    }


def run_dependency_batches(state: StudyGraphState) -> StudyGraphState:
    """Run dependency batches and block only datasets with failed dependencies."""

    task_by_dataset = {task["dataset"]: task for task in state.get("dataset_tasks", [])}
    dependencies = state.get("dataset_dependencies", {})
    dataset_results: list[DatasetResultSummary] = []
    audit_artifacts: list[ArtifactRef] = []
    blocked_datasets = []
    completed: set[str] = set()
    failed: set[str] = set()

    for batch in state.get("execution_batches", []):
        runnable: list[DatasetTask] = []
        for dataset in batch:
            failed_dependencies = [dependency for dependency in dependencies.get(dataset, []) if dependency in failed]
            missing_dependencies = [
                dependency
                for dependency in dependencies.get(dataset, [])
                if dependency not in completed and dependency not in failed
            ]
            if failed_dependencies or missing_dependencies:
                blocked_by = failed_dependencies or missing_dependencies
                blocked_record = {
                    "dataset": dataset,
                    "reason": "blocked_by_adsl" if blocked_by == ["ADSL"] else "blocked_by_dependency",
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
            if summary.status == "completed":
                completed.add(summary.dataset)
            else:
                failed.add(summary.dataset)

    return {
        "dataset_results": dataset_results,
        "audit_artifacts": audit_artifacts,
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

    artifact = ArtifactRef(
        artifact_id=f"audit_{state['study_id'].lower()}_{state['run_id']}_study_stub",
        kind="audit_manifest",
        path=f"runs/{state['run_id']}/study_audit_stub.json",
        format="json",
        role="audit",
        metadata={
            "stub": True,
            "requested_datasets": state.get("requested_datasets", []),
            "auto_added_datasets": state.get("auto_added_datasets", []),
            "unsupported_datasets": state.get("unsupported_datasets", []),
            "datasets": [result.dataset for result in state.get("dataset_results", [])],
            "dataset_dependencies": state.get("dataset_dependencies", {}),
            "dependency_decisions": state.get("dependency_decisions", []),
            "dependency_evidence": state.get("dependency_evidence", ""),
            "dependency_evidence_records": state.get("dependency_evidence_records", []),
            "dependency_planning_warnings": state.get("dependency_planning_warnings", []),
            "execution_batches": state.get("execution_batches", []),
            "blocked_datasets": state.get("blocked_datasets", []),
        },
    )
    return {
        "audit_manifest": artifact,
        "audit_artifacts": [artifact],
    }


def _make_dataset_task(
    state: StudyGraphState,
    dataset: str,
    scenario: str,
    dependency_status: str,
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
    graph.add_edge("plan_datasets", "run_dependency_batches")
    graph.add_edge("run_dependency_batches", "reduce_dataset_results")
    graph.add_edge("reduce_dataset_results", "write_audit_manifest_stub")
    graph.add_edge("write_audit_manifest_stub", END)
    return graph


def compile_study_graph(checkpointer=None):
    """Compile the study-level graph with an optional parent checkpointer."""

    return build_study_graph().compile(checkpointer=checkpointer, name="study_graph")
