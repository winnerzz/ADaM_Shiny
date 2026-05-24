"""Dataset-level LangGraph skeleton."""

from __future__ import annotations

from langgraph.graph import END, START, StateGraph

from adam_agent.adsl.runner import run_adsl_minimal
from adam_agent.downstream.runner import run_downstream_adam
from adam_agent.graph.routing import route_after_risk, route_after_sandbox
from adam_agent.graph.state import DatasetGraphState
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary


def prepare_dataset(state: DatasetGraphState) -> DatasetGraphState:
    """Initialize one dataset run."""

    if state.get("dataset") == "ADSL" and state.get("execution_mode") == "real_adsl_minimal":
        return run_adsl_minimal_node(state)
    if state.get("dataset") != "ADSL" and state.get("execution_mode") == "llm_downstream_stubbed":
        return run_llm_downstream_stubbed_node(state)

    return {
        "status": "running",
        "repair_attempts": state.get("repair_attempts", 0),
        "max_repair_attempts": state.get("max_repair_attempts", 3),
        "sandbox_runs": state.get("sandbox_runs", 0),
    }


def run_llm_downstream_stubbed_node(state: DatasetGraphState) -> DatasetGraphState:
    """Run the generic downstream LLM/R service with mock boundaries."""

    study_dir = state.get("study_dir")
    if not study_dir:
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": "execution_mode=llm_downstream_stubbed requires study_dir",
            "real_run_artifacts": {},
            "real_validation_status": "not_run",
            "sandbox_runs": 0,
        }

    try:
        result = run_downstream_adam(
            study_dir=study_dir,
            study_id=state["study_id"],
            run_id=state["run_id"],
            target_dataset=state["dataset"],
            dependency_resolution=state.get("dependency_resolution", []),
        )
    except Exception as exc:
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": str(exc),
            "real_run_artifacts": {},
            "real_validation_status": "not_run",
            "sandbox_runs": 0,
        }

    return {
        "status": result.status,
        "failure_type": None if result.status in {"completed", "completed_stub"} else "sandbox_error",
        "route": "success" if result.status in {"completed", "completed_stub"} else "fail",
        "real_run_completed": result.status in {"completed", "completed_stub"},
        "real_run_error": result.error or "; ".join(result.validation_report.get("errors", [])),
        "real_run_artifacts": result.artifacts,
        "real_validation_status": result.validation_status,
        "real_run_metadata": {
            "stubbed_r_execution": result.validation_report.get("stubbed_r_execution", False),
            "llm_provider": result.validation_report.get("llm_provider"),
            "llm_model": result.validation_report.get("llm_model"),
            "not_real_derivation": result.validation_report.get("not_real_derivation", False),
        },
        "audit_artifacts": [artifact for key, artifact in result.artifacts.items() if key in {"llm_context", "llm_response", "llm_parsed_response", "validation_report"}],
        "sandbox_runs": 1,
    }


def run_adsl_minimal_node(state: DatasetGraphState) -> DatasetGraphState:
    """Run the real Phase 5 ADSL minimal service from the dataset graph."""

    study_dir = state.get("study_dir")
    if not study_dir:
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": "execution_mode=real_adsl_minimal requires study_dir",
        }

    try:
        result = run_adsl_minimal(
            study_dir,
            run_id=state["run_id"],
            rscript_path=state.get("rscript_path") or None,
            study_id=state["study_id"],
            manifest_name="adsl_manifest.json",
        )
    except Exception as exc:
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": str(exc),
            "real_run_artifacts": {},
            "real_validation_status": "not_run",
            "failure_records": [],
            "recommended_route": "fail",
            "sandbox_runs": 0,
        }
    failure_records = [result.failure_record] if result.failure_record else []
    return {
        "status": result.status,
        "failure_type": None if result.status == "completed" else (result.failure_record.failure_type if result.failure_record else "sandbox_error"),
        "route": "success" if result.status == "completed" else "fail",
        "real_run_completed": result.status == "completed",
        "real_run_error": "" if result.status == "completed" else (result.failure_record.message if result.failure_record else result.r_result.stderr),
        "real_run_artifacts": result.artifacts,
        "real_validation_status": result.validation_report["status"],
        "failure_records": failure_records,
        "recommended_route": result.failure_record.recommended_route if result.failure_record else None,
        "audit_artifacts": [result.manifest],
        "sandbox_runs": 1,
    }


def draft_lineage_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend lineage was drafted."""

    if state.get("execution_mode") == "real_adsl_minimal" and state.get("dataset") == "ADSL":
        return {}
    if state.get("execution_mode") == "llm_downstream_stubbed" and state.get("dataset") != "ADSL":
        return {}
    return {"lineage_ready": True}


def draft_spec_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend a draft spec was produced."""

    if state.get("execution_mode") == "real_adsl_minimal" and state.get("dataset") == "ADSL":
        return {}
    if state.get("execution_mode") == "llm_downstream_stubbed" and state.get("dataset") != "ADSL":
        return {}
    return {"draft_spec_ready": True}


def route_risk_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Flag higher-risk datasets for a stub human review path."""

    if state.get("execution_mode") == "real_adsl_minimal" and state.get("dataset") == "ADSL":
        return {"human_review_required": False, "route": state.get("route", "success")}
    if state.get("execution_mode") == "llm_downstream_stubbed" and state.get("dataset") != "ADSL":
        return {"human_review_required": False, "route": state.get("route", "success")}
    return {
        "human_review_required": state.get("dataset") != "ADSL",
        "route": "human_review" if state.get("dataset") != "ADSL" else "continue",
    }


def human_review_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend a human review checkpoint approved the stub spec."""

    return {"human_review_required": False, "route": "continue"}


def generate_code_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend R code was generated."""

    if state.get("execution_mode") == "real_adsl_minimal" and state.get("dataset") == "ADSL":
        return {}
    if state.get("execution_mode") == "llm_downstream_stubbed" and state.get("dataset") != "ADSL":
        return {}
    dataset = state["dataset"]
    return {"generated_code": f"# stub generated code for {dataset}"}


def run_sandbox_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend code ran in the R sandbox."""

    if state.get("execution_mode") == "real_adsl_minimal" and state.get("dataset") == "ADSL":
        return {}
    if state.get("execution_mode") == "llm_downstream_stubbed" and state.get("dataset") != "ADSL":
        return {}
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

    if state.get("execution_mode") == "real_adsl_minimal" and state.get("dataset") == "ADSL":
        return summarize_real_adsl_minimal(state)
    if state.get("execution_mode") == "llm_downstream_stubbed" and state.get("dataset") != "ADSL":
        return summarize_real_downstream(state)

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


def summarize_real_downstream(state: DatasetGraphState) -> DatasetGraphState:
    """Create a DatasetResultSummary for the generic downstream service."""

    dataset = state["dataset"]
    run_metadata = state.get("real_run_metadata", {})
    is_stubbed = bool(run_metadata.get("stubbed_r_execution") or run_metadata.get("not_real_derivation"))
    status = "completed_stub" if state.get("real_run_completed") and is_stubbed else "completed" if state.get("real_run_completed") else "failed"
    artifacts = state.get("real_run_artifacts", {})
    output_artifact_ids = []
    if "output_adam" in artifacts:
        output_artifact_ids.append(artifacts["output_adam"].artifact_id)
    failure_ids = [] if status in {"completed", "completed_stub"} else [f"failure_{dataset.lower()}_llm_downstream"]
    audit_artifact_id = None
    if state.get("audit_artifacts"):
        audit_artifact_id = state["audit_artifacts"][-1].artifact_id

    summary = DatasetResultSummary(
        dataset=dataset,
        status=status,
        output_artifact_ids=output_artifact_ids,
        audit_artifact_id=audit_artifact_id,
        validation_status="structural_stub_pass" if status == "completed_stub" else state.get("real_validation_status", "unknown"),
        compare_status="not_run",
        failure_ids=failure_ids,
        metadata={
            "stubbed_r_execution": is_stubbed,
            "llm_provider": run_metadata.get("llm_provider"),
            "llm_model": run_metadata.get("llm_model"),
            "not_real_derivation": is_stubbed,
            "summary_status_note": status,
        },
    )
    return {
        "status": status,
        "summary": summary,
    }


def summarize_real_adsl_minimal(state: DatasetGraphState) -> DatasetGraphState:
    """Create a DatasetResultSummary for the real Phase 5 ADSL service."""

    status = "completed" if state.get("real_run_completed") else "failed"
    artifacts = state.get("real_run_artifacts", {})
    output_artifact_ids = []
    if "output_adsl" in artifacts:
        output_artifact_ids.append(artifacts["output_adsl"].artifact_id)
    failure_ids = [record.failure_id for record in state.get("failure_records", [])]
    if status == "failed" and not failure_ids:
        failure_ids = ["failure_adsl_real_minimal"]

    summary = DatasetResultSummary(
        dataset="ADSL",
        status=status,
        output_artifact_ids=output_artifact_ids,
        audit_artifact_id=state.get("audit_artifacts", [None])[-1].artifact_id if state.get("audit_artifacts") else None,
        validation_status=state.get("real_validation_status", "unknown"),
        compare_status="skipped",
        failure_ids=failure_ids,
    )
    return {
        "status": status,
        "summary": summary,
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
