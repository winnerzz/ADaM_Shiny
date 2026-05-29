"""Dataset-level LangGraph skeleton."""

from __future__ import annotations

from langgraph.graph import END, START, StateGraph

from adam_agent.downstream.runner import DownstreamRunResult, run_downstream_adam
from adam_agent.graph.routing import route_after_risk, route_after_sandbox
from adam_agent.graph.state import DatasetGraphState
from adam_agent.llm.clients import LLMProviderConfig, build_llm_client
from adam_agent.llm.mock_code import default_mock_generated_code_response
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.llm import LLMExposureConfig
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary
from adam_agent.tools.r_runner import LocalRRunner


def _is_llm_downstream_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") in {
        "llm_downstream_stubbed",
        "llm_downstream_provider",
        "llm_downstream_r_sandbox",
    }


def _is_retired_adsl_template_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") == "real_adsl_minimal"


def _skips_stub_nodes(state: DatasetGraphState) -> bool:
    return _is_llm_downstream_mode(state) or _is_retired_adsl_template_mode(state)


def prepare_dataset(state: DatasetGraphState) -> DatasetGraphState:
    """Initialize one dataset run."""

    if state.get("execution_mode") == "llm_downstream_stubbed":
        return run_llm_downstream_stubbed_node(state)
    if state.get("execution_mode") == "llm_downstream_provider":
        return run_llm_downstream_provider_node(state)
    if state.get("execution_mode") == "llm_downstream_r_sandbox":
        return run_llm_downstream_r_sandbox_node(state)
    if _is_retired_adsl_template_mode(state):
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": (
                "execution_mode=real_adsl_minimal is retired from DatasetGraph. "
                "Use the unified ADaM split flow or an llm_downstream_* execution mode."
            ),
            "real_run_artifacts": {},
            "real_validation_status": "not_run",
            "sandbox_runs": 0,
        }

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

    return _downstream_result_state(result)


def _downstream_result_state(result: DownstreamRunResult) -> DatasetGraphState:
    """Convert a downstream service result into DatasetGraph runtime state."""

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
            "provider_alias": result.validation_report.get("provider_alias"),
            "transport": result.validation_report.get("transport"),
            "provider_base_url": result.validation_report.get("provider_base_url"),
            "external_relay": result.validation_report.get("external_relay", False),
            "risk_flags": result.validation_report.get("risk_flags", []),
            "not_real_derivation": result.validation_report.get("not_real_derivation", False),
            "failure_root_cause": result.failure_records[-1].root_cause if result.failure_records else None,
            "recommended_route": result.failure_records[-1].recommended_route if result.failure_records else None,
            "repair_attempts_used": result.validation_report.get("repair_attempt", 0),
        },
        "failure_records": result.failure_records,
        "recommended_route": result.failure_records[-1].recommended_route if result.failure_records else None,
        "audit_artifacts": [
            artifact
            for key, artifact in result.artifacts.items()
            if key in {"llm_context", "llm_response", "llm_parsed_response", "validation_report", "failure_report"}
        ],
        "sandbox_runs": 1,
    }


def run_llm_downstream_provider_node(state: DatasetGraphState) -> DatasetGraphState:
    """Run the generic downstream service with a configured LLM provider."""

    return _run_llm_downstream_provider_node(state, use_local_r=False)


def run_llm_downstream_r_sandbox_node(state: DatasetGraphState) -> DatasetGraphState:
    """Run the generic downstream service with configured LLM and local Rscript."""

    return _run_llm_downstream_provider_node(state, use_local_r=True)


def _run_llm_downstream_provider_node(state: DatasetGraphState, *, use_local_r: bool) -> DatasetGraphState:
    """Run one downstream target through configured LLM and selected R boundary."""

    study_dir = state.get("study_dir")
    if not study_dir:
        mode = state.get("execution_mode", "llm_downstream_provider")
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": f"execution_mode={mode} requires study_dir",
            "real_run_artifacts": {},
            "real_validation_status": "not_run",
            "sandbox_runs": 0,
        }

    try:
        provider_config = LLMProviderConfig(**state.get("llm_provider", {}))
        exposure = LLMExposureConfig.model_validate(state.get("llm_exposure", {}))
        llm_client = build_llm_client(provider_config)
        if provider_config.provider.strip().lower() == "mock":
            llm_client = _provider_mode_default_mock_client(state["dataset"])
        r_runner = LocalRRunner(state.get("rscript_path") or None) if use_local_r else None
        result = run_downstream_adam(
            study_dir=study_dir,
            study_id=state["study_id"],
            run_id=state["run_id"],
            target_dataset=state["dataset"],
            dependency_resolution=state.get("dependency_resolution", []),
            llm_client=llm_client,
            exposure=exposure,
            provider=provider_config.provider,
            model=provider_config.model,
            r_runner=r_runner,
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
            "real_run_metadata": _provider_failure_metadata(state.get("llm_provider", {})),
            "sandbox_runs": 0,
        }

    return _downstream_result_state(result)


def _provider_failure_metadata(provider_payload: dict[str, object]) -> dict[str, object]:
    provider_config = LLMProviderConfig(**provider_payload)
    base_url = provider_config.base_url
    provider = provider_config.provider.strip().lower()
    if base_url is None:
        if provider in {"openai", "openai-compatible"}:
            base_url = "https://api.openai.com/v1"
        elif provider == "deepseek":
            base_url = "https://api.deepseek.com/v1"
        elif provider == "qwen":
            base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1"
        elif provider in {"anthropic", "claude", "anthropic-messages"}:
            base_url = "https://api.anthropic.com"
    return {
        "stubbed_r_execution": False,
        "llm_provider": provider_config.provider,
        "llm_model": provider_config.model,
        "provider_alias": provider_config.provider,
        "provider_base_url": base_url,
        "external_relay": bool(provider_config.base_url),
        "risk_flags": ["provider_config_failed"],
        "not_real_derivation": True,
    }


def _provider_mode_default_mock_client(dataset: str):
    """Return a contract-valid mock when provider mode is used with mock config."""

    from adam_agent.llm.clients import MockLLMClient

    return MockLLMClient(fixed_response_text=_default_mock_generated_code_response(dataset))


def _default_mock_generated_code_response(target: str) -> str:
    return default_mock_generated_code_response(target)


def draft_lineage_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend lineage was drafted."""

    if _skips_stub_nodes(state):
        return {}
    return {"lineage_ready": True}


def draft_spec_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend a draft spec was produced."""

    if _skips_stub_nodes(state):
        return {}
    return {"draft_spec_ready": True}


def route_risk_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Flag higher-risk datasets for a stub human review path."""

    if _skips_stub_nodes(state):
        return {"human_review_required": False, "route": state.get("route", "success")}
    human_review_required = bool(state.get("human_review_required", False))
    return {
        "human_review_required": human_review_required,
        "route": "human_review" if human_review_required else "continue",
    }


def human_review_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend a human review checkpoint approved the stub spec."""

    return {"human_review_required": False, "route": "continue"}


def generate_code_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend R code was generated."""

    if _skips_stub_nodes(state):
        return {}
    dataset = state["dataset"]
    return {"generated_code": f"# stub generated code for {dataset}"}


def run_sandbox_stub(state: DatasetGraphState) -> DatasetGraphState:
    """Pretend code ran in the R sandbox."""

    if _skips_stub_nodes(state):
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

    if _is_llm_downstream_mode(state):
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
    is_stubbed = bool(run_metadata.get("stubbed_r_execution"))
    status = "completed_stub" if state.get("real_run_completed") and is_stubbed else "completed" if state.get("real_run_completed") else "failed"
    artifacts = state.get("real_run_artifacts", {})
    output_artifact_ids = []
    if "output_adam" in artifacts:
        output_artifact_ids.append(artifacts["output_adam"].artifact_id)
    failure_ids = [record.failure_id for record in state.get("failure_records", [])]
    if status not in {"completed", "completed_stub"} and not failure_ids:
        failure_ids = [f"failure_{dataset.lower()}_llm_downstream"]
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
            "provider_alias": run_metadata.get("provider_alias"),
            "transport": run_metadata.get("transport"),
            "provider_base_url": run_metadata.get("provider_base_url"),
            "external_relay": run_metadata.get("external_relay", False),
            "risk_flags": run_metadata.get("risk_flags", []),
            "not_real_derivation": bool(run_metadata.get("not_real_derivation", False)),
            "failure_root_cause": run_metadata.get("failure_root_cause"),
            "recommended_route": run_metadata.get("recommended_route"),
            "repair_attempts_used": run_metadata.get("repair_attempts_used", 0),
            "summary_status_note": status,
        },
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

