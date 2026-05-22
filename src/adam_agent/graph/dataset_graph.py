"""Dataset-level LangGraph skeleton."""

from __future__ import annotations

from langgraph.graph import END, START, StateGraph

from adam_agent.graph.routing import route_after_risk, route_after_sandbox
from adam_agent.graph.state import DatasetGraphState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary


def prepare_dataset(state: DatasetGraphState) -> DatasetGraphState:
    """Initialize one dataset run."""

    return {
        "status": "running",
        "repair_attempts": state.get("repair_attempts", 0),
        "max_repair_attempts": state.get("max_repair_attempts", 3),
        "sandbox_runs": state.get("sandbox_runs", 0),
    }


def draft_lineage_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend lineage was drafted."""

    return {"lineage_ready": True}


def draft_spec_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend a draft spec was produced."""

    return {"draft_spec_ready": True}


def route_risk_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Flag higher-risk datasets for a stub human review path."""

    return {
        "human_review_required": state.get("dataset") != "ADSL",
        "route": "human_review" if state.get("dataset") != "ADSL" else "continue",
    }


def human_review_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend a human review checkpoint approved the stub spec."""

    return {"human_review_required": False, "route": "continue"}


def generate_code_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend R code was generated."""

    dataset = state["dataset"]
    return {"generated_code": f"# stub generated code for {dataset}"}


def run_sandbox_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend code ran in the R sandbox."""

    sandbox_runs = state.get("sandbox_runs", 0) + 1
    scenario = state.get("stub_scenario", "success")

    if scenario == "fail_adsl":
        return {
            "sandbox_runs": sandbox_runs,
            "status": "failed",
            "failure_type": "sandbox_error",
        }

    if scenario == "code_error_then_success" and state.get("repair_attempts", 0) == 0:
        return {
            "sandbox_runs": sandbox_runs,
            "status": "running",
            "failure_type": "code_error",
        }

    if scenario == "spec_error_then_success" and state.get("repair_attempts", 0) == 0:
        return {
            "sandbox_runs": sandbox_runs,
            "status": "running",
            "failure_type": "spec_error",
        }

    return {
        "sandbox_runs": sandbox_runs,
        "status": "completed",
        "failure_type": None,
    }


def classify_result_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Classify the stub sandbox result into a single route."""

    failure_type = state.get("failure_type")
    if failure_type == "code_error":
        return {"route": "repair_code"}
    if failure_type == "spec_error":
        return {"route": "revise_spec"}
    if failure_type:
        return {"route": "fail"}
    return {"route": "success"}


def repair_code_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend generated code was repaired."""

    return {
        "repair_attempts": state.get("repair_attempts", 0) + 1,
        "failure_type": None,
        "route": "continue",
    }


def revise_spec_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend the draft spec was revised after diagnosis."""

    return {
        "repair_attempts": state.get("repair_attempts", 0) + 1,
        "failure_type": None,
        "draft_spec_ready": True,
        "route": "continue",
    }


def summarize_dataset(state: DatasetGraphState) -> DatasetGraphState:
    """Create the dataset-level summary returned to the study graph."""

    status = "failed" if state.get("failure_type") else "completed"
    dataset = state["dataset"]
    failure_ids: list[str] = []

    if state.get("failure_type"):
        failure = FailureRecord(
            failure_id=f"failure_{dataset.lower()}_stub",
            dataset=dataset,
            node="run_sandbox_stub",
            failure_type="sandbox_error",
            message=f"{dataset} failed in stub sandbox",
            recommended_route="fail",
            repair_attempt=state.get("repair_attempts", 0),
        )
        failure_ids.append(failure.failure_id)

    summary = DatasetResultSummary(
        dataset=dataset,
        status=status,
        validation_status="not_run_stub" if status == "failed" else "passed_stub",
        compare_status="not_run_stub",
        failure_ids=failure_ids,
    )
    audit_artifact = ArtifactRef(
        artifact_id=f"audit_{dataset.lower()}_dataset_stub",
        kind="audit_manifest",
        path=f"runs/{state['run_id']}/{dataset.lower()}_audit_stub.json",
        dataset=dataset,
        format="json",
        role="audit",
        metadata={
            "repair_attempts": state.get("repair_attempts", 0),
            "sandbox_runs": state.get("sandbox_runs", 0),
            "stub": True,
        },
    )
    return {
        "status": status,
        "summary": summary,
        "audit_artifacts": [audit_artifact],
    }


def build_dataset_graph():
    """Build the dataset-level skeleton graph."""

    graph = StateGraph(DatasetGraphState)
    graph.add_node("prepare_dataset", prepare_dataset)
    graph.add_node("draft_lineage_stub", draft_lineage_stub)
    graph.add_node("draft_spec_stub", draft_spec_stub)
    graph.add_node("route_risk_stub", route_risk_stub)
    graph.add_node("human_review_stub", human_review_stub)
    graph.add_node("generate_code_stub", generate_code_stub)
    graph.add_node("run_sandbox_stub", run_sandbox_stub)
    graph.add_node("classify_result_stub", classify_result_stub)
    graph.add_node("repair_code_stub", repair_code_stub)
    graph.add_node("revise_spec_stub", revise_spec_stub)
    graph.add_node("summarize_dataset", summarize_dataset)

    graph.add_edge(START, "prepare_dataset")
    graph.add_edge("prepare_dataset", "draft_lineage_stub")
    graph.add_edge("draft_lineage_stub", "draft_spec_stub")
    graph.add_edge("draft_spec_stub", "route_risk_stub")
    graph.add_conditional_edges(
        "route_risk_stub",
        route_after_risk,
        {
            "continue": "generate_code_stub",
            "human_review": "human_review_stub",
        },
    )
    graph.add_edge("human_review_stub", "generate_code_stub")
    graph.add_edge("generate_code_stub", "run_sandbox_stub")
    graph.add_edge("run_sandbox_stub", "classify_result_stub")
    graph.add_conditional_edges(
        "classify_result_stub",
        route_after_sandbox,
        {
            "success": "summarize_dataset",
            "repair_code": "repair_code_stub",
            "revise_spec": "revise_spec_stub",
            "fail": "summarize_dataset",
        },
    )
    graph.add_edge("repair_code_stub", "run_sandbox_stub")
    graph.add_edge("revise_spec_stub", "generate_code_stub")
    graph.add_edge("summarize_dataset", END)
    return graph


def compile_dataset_graph():
    """Compile the dataset-level skeleton graph without its own checkpointer."""

    return build_dataset_graph().compile(name="dataset_graph")
