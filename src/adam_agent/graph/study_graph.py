"""Study-level LangGraph skeleton."""

from __future__ import annotations

from langgraph.graph import END, START, StateGraph

from adam_agent.graph.dataset_graph import compile_dataset_graph
from adam_agent.graph.routing import route_after_foundation
from adam_agent.graph.state import DatasetGraphState, DatasetTask, StudyGraphState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.states import DatasetResultSummary


FOUNDATION_DATASETS = ["ADSL"]


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

    targets = state.get("target_datasets") or ["ADSL", "ADAE"]
    if any(dataset not in FOUNDATION_DATASETS for dataset in targets) and "ADSL" not in targets:
        targets = ["ADSL", *targets]

    foundation = [dataset for dataset in targets if dataset in FOUNDATION_DATASETS]

    downstream = [dataset for dataset in targets if dataset not in foundation]
    if not downstream and len(targets) == 1 and targets[0] == "ADSL":
        downstream = []

    dependency_graph = {"ADSL": downstream}
    scenarios = state.get("stub_scenarios", {})

    foundation_tasks = [
        _make_dataset_task(state, dataset, scenarios.get(dataset, "success"), "foundation")
        for dataset in foundation
    ]
    downstream_tasks = [
        _make_dataset_task(
            state,
            dataset,
            scenarios.get(dataset, "code_error_then_success" if dataset == "ADAE" else "success"),
            "depends_on_adsl",
        )
        for dataset in downstream
    ]

    return {
        "target_datasets": targets,
        "foundation_datasets": foundation,
        "downstream_datasets": downstream,
        "dependency_graph": dependency_graph,
        "dataset_tasks": foundation_tasks,
        "downstream_tasks": downstream_tasks,
    }


def run_foundation_datasets(state: StudyGraphState) -> StudyGraphState:
    """Run foundation datasets before downstream dispatch."""

    dataset_graph = compile_dataset_graph()
    dataset_results: list[DatasetResultSummary] = []
    audit_artifacts: list[ArtifactRef] = []

    for task in state.get("dataset_tasks", []):
        result = dataset_graph.invoke(task)
        dataset_results.append(result["summary"])
        audit_artifacts.extend(result.get("audit_artifacts", []))

    route = "foundation_ready"
    if any(result.status != "completed" for result in dataset_results):
        route = "foundation_failed"

    return {
        "dataset_results": dataset_results,
        "audit_artifacts": audit_artifacts,
        "route": route,
    }


def run_downstream_dataset(state: DatasetGraphState) -> StudyGraphState:
    """Run one downstream dataset and return study-level reducer updates."""

    dataset_graph = compile_dataset_graph()
    result = dataset_graph.invoke(state)
    return {
        "dataset_results": [result["summary"]],
        "audit_artifacts": result.get("audit_artifacts", []),
    }


def mark_downstream_blocked(state: StudyGraphState) -> StudyGraphState:
    """Mark downstream datasets blocked when ADSL failed."""

    blocked = [
        {
            "dataset": dataset,
            "reason": "blocked_by_adsl",
            "blocked_by": "ADSL",
        }
        for dataset in state.get("downstream_datasets", [])
    ]
    blocked_results = [
        DatasetResultSummary(
            dataset=item["dataset"],
            status="failed",
            validation_status="blocked_by_adsl",
            compare_status="not_run_stub",
            failure_ids=["blocked_by_adsl"],
        )
        for item in blocked
    ]
    return {
        "blocked_datasets": blocked,
        "dataset_results": blocked_results,
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
            "datasets": [result.dataset for result in state.get("dataset_results", [])],
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


def build_study_graph():
    """Build the study-level skeleton graph."""

    graph = StateGraph(StudyGraphState)
    graph.add_node("initialize_study", initialize_study)
    graph.add_node("plan_datasets", plan_datasets)
    graph.add_node("run_foundation_datasets", run_foundation_datasets)
    graph.add_node("run_downstream_dataset", run_downstream_dataset)
    graph.add_node("mark_downstream_blocked", mark_downstream_blocked)
    graph.add_node("reduce_dataset_results", reduce_dataset_results)
    graph.add_node("write_audit_manifest_stub", write_audit_manifest_stub)

    graph.add_edge(START, "initialize_study")
    graph.add_edge("initialize_study", "plan_datasets")
    graph.add_edge("plan_datasets", "run_foundation_datasets")
    graph.add_conditional_edges(
        "run_foundation_datasets",
        route_after_foundation,
        ["run_downstream_dataset", "mark_downstream_blocked", "reduce_dataset_results"],
    )
    graph.add_edge("mark_downstream_blocked", "reduce_dataset_results")
    graph.add_edge("run_downstream_dataset", "reduce_dataset_results")
    graph.add_edge("reduce_dataset_results", "write_audit_manifest_stub")
    graph.add_edge("write_audit_manifest_stub", END)
    return graph


def compile_study_graph(checkpointer=None):
    """Compile the study-level graph with an optional parent checkpointer."""

    return build_study_graph().compile(checkpointer=checkpointer, name="study_graph")
