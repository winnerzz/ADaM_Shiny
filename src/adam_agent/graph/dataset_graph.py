"""Dataset-level LangGraph skeleton."""

from __future__ import annotations

import json
from pathlib import Path
from langgraph.graph import END, START, StateGraph

from adam_agent.agents import build_agent_node_input, build_agent_node_output, record_agent_decision
from adam_agent.downstream.runner import DownstreamRunResult, run_downstream_adam
from adam_agent.graph.execution import GraphExecutionError, execute_approved_r_code
from adam_agent.graph.execution_modes import (
    GRAPH_PRODUCT_EXECUTE_MODE,
    GRAPH_PRODUCT_GENERATE_CODE_MODE,
    GRAPH_PRODUCT_MODES,
    GRAPH_PRODUCT_PREPARE_MODE,
    LEGACY_STUB_MODE,
    LLM_DOWNSTREAM_MODES,
    LLM_DOWNSTREAM_PROVIDER_MODE,
    LLM_DOWNSTREAM_R_SANDBOX_MODE,
    LLM_DOWNSTREAM_STUBBED_MODE,
    RETIRED_ADSL_TEMPLATE_MODE,
    format_execution_modes,
)
from adam_agent.graph.routing import route_after_risk, route_after_sandbox
from adam_agent.graph.state import DatasetGraphState
from adam_agent.graph.workflow_state import compare_fingerprints, input_fingerprint, utc_timestamp
from adam_agent.llm.clients import (
    LLMClientConfigError,
    LLMProviderConfig,
    LLMProviderResponseError,
    MockLLMClient,
    build_llm_client,
)
from adam_agent.llm.context import build_target_llm_context, write_llm_context_package
from adam_agent.llm.draft_spec import (
    DraftSpecGenerationError,
    default_mock_draft_spec_response,
    generate_draft_spec_from_evidence,
)
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
from adam_agent.schemas.graph_state import StudyRunState
from adam_agent.schemas.llm import LLMExposureConfig
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.sandbox import LocalRscriptSandboxRunner
from adam_agent.tools.static_rules import (
    StaticRuleError,
    assert_no_blocking_static_findings,
    run_generated_r_static_checks,
    write_static_rule_report,
)


def _is_llm_downstream_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") in LLM_DOWNSTREAM_MODES


def _is_retired_adsl_template_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") == RETIRED_ADSL_TEMPLATE_MODE


def _is_graph_product_prepare_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") == GRAPH_PRODUCT_PREPARE_MODE


def _is_graph_product_generate_code_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") == GRAPH_PRODUCT_GENERATE_CODE_MODE


def _is_graph_product_execute_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") == GRAPH_PRODUCT_EXECUTE_MODE


def _is_legacy_stub_mode(state: DatasetGraphState) -> bool:
    return state.get("execution_mode") == LEGACY_STUB_MODE


def _is_legacy_stub_graph_enabled(state: DatasetGraphState) -> bool:
    return bool(state.get("legacy_stub_graph_enabled"))


def _skips_stub_nodes(state: DatasetGraphState) -> bool:
    return (
        _is_llm_downstream_mode(state)
        or _is_retired_adsl_template_mode(state)
        or _is_graph_product_prepare_mode(state)
        or _is_graph_product_generate_code_mode(state)
        or _is_graph_product_execute_mode(state)
    )


def prepare_dataset(state: DatasetGraphState) -> DatasetGraphState:
    """Initialize one dataset run."""

    if _is_graph_product_prepare_mode(state) or _is_graph_product_generate_code_mode(state):
        return prepare_product_context_node(state)
    if state.get("execution_mode") == LLM_DOWNSTREAM_STUBBED_MODE:
        return run_llm_downstream_stubbed_node(state)
    if state.get("execution_mode") == LLM_DOWNSTREAM_PROVIDER_MODE:
        return run_llm_downstream_provider_node(state)
    if state.get("execution_mode") == LLM_DOWNSTREAM_R_SANDBOX_MODE:
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

    if _is_graph_product_execute_mode(state):
        return {
            "status": "running",
            "repair_attempts": state.get("repair_attempts", 0),
            "max_repair_attempts": state.get("max_repair_attempts", 3),
            "sandbox_runs": state.get("sandbox_runs", 0),
        }

    if _is_legacy_stub_mode(state):
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": (
                "execution_mode=stub is available only through the explicit "
                "legacy/test dataset graph compiler."
            ),
            "real_run_artifacts": {},
            "real_validation_status": "not_run",
            "sandbox_runs": 0,
        }

    if not _is_legacy_stub_mode(state):
        mode = state.get("execution_mode") or "missing"
        allowed_product_modes = format_execution_modes(GRAPH_PRODUCT_MODES)
        allowed_downstream_modes = format_execution_modes(LLM_DOWNSTREAM_MODES)
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": (
                f"DatasetGraph requires an explicit execution_mode; got {mode}. "
                f"Use {allowed_product_modes}, {allowed_downstream_modes}, "
                f"or explicit legacy/test mode {LEGACY_STUB_MODE}."
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


def prepare_legacy_stub_dataset(state: DatasetGraphState) -> DatasetGraphState:
    """Prepare the explicit legacy/test stub graph."""

    if not _is_legacy_stub_mode(state):
        return prepare_dataset(state)
    return {
        "status": "running",
        "repair_attempts": state.get("repair_attempts", 0),
        "max_repair_attempts": state.get("max_repair_attempts", 3),
        "sandbox_runs": state.get("sandbox_runs", 0),
        "legacy_stub_graph_enabled": True,
    }


def prepare_product_context_node(state: DatasetGraphState) -> DatasetGraphState:
    """Prepare real product context and stop at the first spec gate."""

    study_dir = state.get("study_dir")
    evidence_input = _evidence_agent_input(state)
    if not study_dir:
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": f"execution_mode={GRAPH_PRODUCT_PREPARE_MODE} requires study_dir",
            "real_run_artifacts": {},
            "real_validation_status": "not_run",
            "sandbox_runs": 0,
        }

    try:
        exposure = LLMExposureConfig.model_validate(state.get("llm_exposure", {}))
        context = _build_target_context_for_state(state)(
            study_id=state["study_id"],
            run_id=state["run_id"],
            target_dataset=state["dataset"],
            study_dir=study_dir,
            dependency_resolution=state.get("dependency_resolution", []),
            exposure=exposure,
            rscript_path=state.get("rscript_path") or None,
        )
        context_artifact = write_llm_context_package(context, study_dir)
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

    force_new_draft_spec = bool(state.get("force_new_draft_spec")) or _terminal_failure_requires_new_draft_spec(
        Path(study_dir), state["run_id"], state["dataset"]
    )

    if context.target_spec is not None and not force_new_draft_spec:
        evidence_output = _evidence_agent_output(
            state,
            decision="input_spec_ready",
            status="needs_review",
            reason="A user-supplied input_spec was found; draft spec generation is not needed.",
            outputs={"spec_source": "input_spec", "next_action": "generate_code"},
            artifact_ids=[context_artifact.artifact_id],
        )
        return {
            "status": "needs_review",
            "route": "human_review",
            "current_interrupt": "code_generation_ready",
            "product_context_ready": True,
            "product_context_warnings": context.warnings,
            "product_context_artifact": context_artifact,
            "product_context": context.as_dict(),
            "spec_source": "input_spec",
            "input_spec_path": context.target_spec.get("path"),
            "draft_spec_required": False,
            "next_action": "generate_code",
            "agent_node_inputs": [evidence_input],
            "agent_node_outputs": [evidence_output],
            "agent_decisions": list(evidence_output["agent_decisions"]),
            "sandbox_runs": 0,
        }

    approved_payload = None
    if not force_new_draft_spec:
        try:
            approved_payload = _approved_draft_spec_payload(Path(study_dir), state["run_id"], state["dataset"])
        except ValueError as exc:
            return _product_failure(
                "spec_error",
                str(exc),
                current_interrupt="draft_spec_review",
                next_action="regenerate_draft_spec",
            )
    if approved_payload is not None:
        context.target_spec = approved_payload
        context.warnings.append(
            "Generated code can use a user-approved draft spec because no approved input_spec was supplied."
        )
        context_artifact = write_llm_context_package(context, study_dir)
        evidence_output = _evidence_agent_output(
            state,
            decision="approved_draft_spec_ready",
            status="needs_review",
            reason="A graph-approved draft spec was found for code generation.",
            outputs={"spec_source": "approved_draft_spec", "next_action": "generate_code"},
            artifact_ids=[context_artifact.artifact_id],
        )
        return {
            "status": "needs_review",
            "route": "human_review",
            "current_interrupt": "code_generation_ready",
            "product_context_ready": True,
            "product_context_warnings": context.warnings,
            "product_context_artifact": context_artifact,
            "product_context": context.as_dict(),
            "spec_source": "approved_draft_spec",
            "draft_spec_required": True,
            "approved_spec_path": approved_payload.get("path"),
            "next_action": "generate_code",
            "agent_node_inputs": [evidence_input],
            "agent_node_outputs": [evidence_output],
            "agent_decisions": list(evidence_output["agent_decisions"]),
            "sandbox_runs": 0,
        }

    evidence_output = _evidence_agent_output(
        state,
        decision="draft_spec_required",
        status="needs_review",
        reason="No approved input_spec or current approved draft spec was found.",
        outputs={"spec_source": "missing_input_spec", "next_action": "draft_spec"},
        risk_flags=["missing_input_spec"],
        artifact_ids=[context_artifact.artifact_id],
    )
    return {
        "status": "needs_review",
        "route": "human_review",
        "current_interrupt": "draft_spec_review",
        "product_context_ready": True,
        "product_context_warnings": context.warnings,
        "product_context_artifact": context_artifact,
        "product_context": context.as_dict(),
        "spec_source": "missing_input_spec",
        "draft_spec_required": True,
        "next_action": "draft_spec",
        "agent_node_inputs": [evidence_input],
        "agent_node_outputs": [evidence_output],
        "agent_decisions": list(evidence_output["agent_decisions"]),
        "risk_flags": ["missing_input_spec"],
        "sandbox_runs": 0,
    }


def _evidence_agent_input(state: DatasetGraphState) -> dict[str, object]:
    return build_agent_node_input(
        agent="evidence_agent",
        node="prepare_product_context",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        task="Prepare the explicit evidence context and decide which spec gate should run next.",
        inputs={
            "execution_mode": state.get("execution_mode"),
            "dependency_resolution_count": len(state.get("dependency_resolution", [])),
            "force_new_draft_spec": bool(state.get("force_new_draft_spec")),
        },
        risk_flags=list(state.get("risk_flags", [])),
        evidence_bundle_id=state.get("evidence_bundle_id"),
        reference_query_ids=[
            str(item.get("query_id"))
            for item in state.get("reference_queries", [])
            if isinstance(item, dict) and item.get("query_id")
        ],
    )


def _evidence_agent_output(
    state: DatasetGraphState,
    *,
    decision: str,
    status: str,
    reason: str,
    outputs: dict[str, object],
    artifact_ids: list[str],
    risk_flags: list[str] | None = None,
) -> dict[str, object]:
    return build_agent_node_output(
        agent="evidence_agent",
        node="prepare_product_context",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        status=status,
        decision=decision,
        reason=reason,
        outputs=outputs,
        artifact_ids=artifact_ids,
        risk_flags=risk_flags or [],
    )


def draft_spec_agent_node(state: DatasetGraphState) -> DatasetGraphState:
    """Generate a review-required draft spec from graph-prepared context."""

    if not _is_graph_product_prepare_mode(state):
        return {}
    if not state.get("draft_spec_required"):
        return {}
    study_dir = state.get("study_dir")
    if not study_dir:
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_error": "draft_spec_agent requires study_dir",
        }

    target = state["dataset"]
    context_dict = dict(state.get("product_context", {}))
    spec_input = _spec_agent_input(state, context_dict)
    try:
        provider_config = LLMProviderConfig(**state.get("llm_provider", {}))
        exposure = LLMExposureConfig.model_validate(state.get("llm_exposure", {}))
        llm_client = _build_llm_client_for_state(state, provider_config)
        if provider_config.provider.strip().lower() == "mock":
            llm_client = MockLLMClient(fixed_response_text=default_mock_draft_spec_response(target, context_dict))
        draft_result = generate_draft_spec_from_evidence(
            study_id=state["study_id"],
            run_id=state["run_id"],
            target_dataset=target,
            study_dir=study_dir,
            context_dict=context_dict,
            llm_client=llm_client,
            provider=provider_config.provider,
            model=provider_config.model,
            exposure=exposure,
            max_tokens=provider_config.max_tokens,
        )
    except (DraftSpecGenerationError, LLMClientConfigError, LLMProviderResponseError, ValueError) as exc:
        return {
            "status": "failed",
            "failure_type": "draft_spec_error",
            "route": "fail",
            "real_run_error": str(exc),
            "real_validation_status": "not_run",
        }

    fingerprint = input_fingerprint(study_dir)
    _merge_json_artifact(
        draft_result.spec_artifact.path,
        {
            "input_fingerprint": fingerprint,
            "reference_adam_policy": "Reference ADaM is compare/output-shape evidence only, not derivation authority.",
        },
    )
    spec_path = Path(draft_result.spec_artifact.path)
    spec_artifact = draft_result.spec_artifact.model_copy(
        update={"sha256": f"sha256:{sha256_file(spec_path)}"}
    )
    artifact_ids = [
        draft_result.prompt_artifact.artifact_id,
        draft_result.response_artifact.artifact_id,
        spec_artifact.artifact_id,
    ]
    spec_output = _spec_agent_output(
        state,
        decision="draft_spec_generated",
        status="needs_review",
        reason="Generated a review-required draft spec from uploaded evidence.",
        outputs={
            "draft_spec_path": spec_artifact.path,
            "variable_count": len(draft_result.spec.variables),
            "next_action": "review_draft_spec",
        },
        risk_flags=["draft_spec_requires_human_review"],
        artifact_ids=artifact_ids,
    )
    return {
        "status": "needs_review",
        "route": "human_review",
        "current_interrupt": "draft_spec_review",
        "spec_source": "draft_spec",
        "draft_spec_required": True,
        "draft_spec_path": spec_artifact.path,
        "draft_spec_prompt_path": draft_result.prompt_artifact.path,
        "draft_spec_response_path": draft_result.response_artifact.path,
        "draft_spec_variables": [variable.model_dump(mode="json") for variable in draft_result.spec.variables],
        "next_action": "review_draft_spec",
        "product_context_warnings": state.get("product_context_warnings", []) + draft_result.warnings,
        "audit_artifacts": [
            draft_result.prompt_artifact,
            draft_result.response_artifact,
            spec_artifact,
        ],
        "agent_node_inputs": [spec_input],
        "agent_node_outputs": [spec_output],
        "agent_decisions": list(spec_output["agent_decisions"]),
        "risk_flags": ["draft_spec_requires_human_review"],
    }


def _spec_agent_input(state: DatasetGraphState, context_dict: dict[str, object]) -> dict[str, object]:
    context_artifact = state.get("product_context_artifact")
    artifact_ids = []
    if context_artifact is not None:
        artifact_id = (
            context_artifact.get("artifact_id")
            if isinstance(context_artifact, dict)
            else getattr(context_artifact, "artifact_id", None)
        )
        if artifact_id:
            artifact_ids.append(str(artifact_id))
    return build_agent_node_input(
        agent="spec_agent",
        node="draft_spec_agent",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        task="Draft a review-required ADaM specification from prepared evidence when no approved input spec exists.",
        inputs={
            "spec_source": state.get("spec_source"),
            "context_keys": sorted(str(key) for key in context_dict.keys()),
            "warning_count": len(state.get("product_context_warnings", [])),
        },
        artifact_ids=artifact_ids,
        risk_flags=list(state.get("risk_flags", [])) + ["draft_spec_requires_human_review"],
        evidence_bundle_id=state.get("evidence_bundle_id"),
        reference_query_ids=[
            str(item.get("query_id"))
            for item in state.get("reference_queries", [])
            if isinstance(item, dict) and item.get("query_id")
        ],
    )


def _spec_agent_output(
    state: DatasetGraphState,
    *,
    decision: str,
    status: str,
    reason: str,
    outputs: dict[str, object],
    artifact_ids: list[str],
    risk_flags: list[str] | None = None,
) -> dict[str, object]:
    return build_agent_node_output(
        agent="spec_agent",
        node="draft_spec_agent",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        status=status,
        decision=decision,
        reason=reason,
        outputs=outputs,
        artifact_ids=artifact_ids,
        risk_flags=risk_flags or [],
    )


def generate_r_code_agent_node(state: DatasetGraphState) -> DatasetGraphState:
    """Generate R code and stop at graph-native code review."""

    if not _is_graph_product_generate_code_mode(state):
        return {}
    study_dir = state.get("study_dir")
    if not study_dir:
        return _product_failure("input_error", "generate_r_code_agent requires study_dir")
    target = state["dataset"]
    context_dict = dict(state.get("product_context", {}))
    if not context_dict:
        return _product_failure("input_error", "generate_r_code_agent requires product_context")

    spec_source = state.get("spec_source")
    if spec_source == "missing_input_spec":
        return _product_failure(
            "spec_error",
            f"No approved input_spec or approved draft spec is available for {target}.",
            current_interrupt="draft_spec_review",
            next_action="review_draft_spec",
        )
    if spec_source == "draft_spec":
        return _product_failure(
            "spec_error",
            f"Draft spec for {target} must be approved before code generation.",
            current_interrupt="draft_spec_review",
            next_action="review_draft_spec",
        )
    target_spec = context_dict.get("target_spec")
    if spec_source not in {"input_spec", "approved_draft_spec"} or not isinstance(target_spec, dict):
        return _product_failure(
            "spec_error",
            f"No approved input_spec or approved draft spec is available for {target}.",
            current_interrupt="draft_spec_review",
            next_action="review_draft_spec",
        )

    code_input = _code_agent_input(state, context_dict)
    try:
        provider_config = LLMProviderConfig(**state.get("llm_provider", {}))
        exposure = LLMExposureConfig.model_validate(state.get("llm_exposure", {}))
        llm_client = _build_llm_client_for_state(state, provider_config)
        if provider_config.provider.strip().lower() == "mock":
            llm_client = MockLLMClient(fixed_response_text=default_mock_generated_code_response(target))
        compact_prompt = compact_prompt_from_context(context_dict)
        prompt_artifact = write_compact_prompt_artifact(
            study_id=state["study_id"],
            run_id=state["run_id"],
            target_dataset=target,
            study_dir=study_dir,
            prompt=compact_prompt,
            source_context_artifact_id=_artifact_id(state.get("product_context_artifact")),
        )
        llm_response = llm_client.generate(
            _llm_request_for_code_generation(
                prompt=compact_prompt,
                target=target,
                study_id=state["study_id"],
                run_id=state["run_id"],
                provider_config=provider_config,
                exposure=exposure,
                context_dict=context_dict,
                prompt_artifact=prompt_artifact,
            )
        )
        package = parse_generated_code_response(llm_response.response_text, expected_dataset=target)
        call_record = llm_response.call_record
        llm_provider = _llm_call_field(call_record, "provider", provider_config.provider)
        llm_model = _llm_call_field(call_record, "model", provider_config.model)
        provider_alias = _llm_call_field(call_record, "provider_alias", None)
        transport = _llm_call_field(call_record, "transport", None)
        provider_base_url = _llm_call_field(call_record, "provider_base_url", None)
        not_real_derivation = _not_real_generation(
            provider_config,
            provider=llm_provider,
            provider_alias=provider_alias,
            transport=transport,
        )
        artifacts = write_generated_code_artifacts(
            study_id=state["study_id"],
            run_id=state["run_id"],
            study_dir=study_dir,
            package=package,
            response_text=llm_response.response_text,
        )
        static_check_path = _write_static_check_report(
            study_dir=Path(study_dir),
            run_id=state["run_id"],
            study_id=state["study_id"],
            target=target,
            code_path=Path(artifacts.code_artifact.path),
            required_identifiers=_required_identifiers_from_spec(target_spec),
            required_identifier_source_id=_spec_source_id(target_spec),
        )
    except (LLMGeneratedCodeError, LLMProviderResponseError, StaticRuleError, ValueError) as exc:
        return _product_failure("code_generation_error", str(exc), next_action="generate_code")

    code_artifact_ids = [
        prompt_artifact.artifact_id,
        artifacts.response_artifact.artifact_id,
        artifacts.package_artifact.artifact_id,
        artifacts.code_artifact.artifact_id,
    ]
    static_artifact = _tool_log_artifact(state, static_check_path, kind_id="static_check")
    code_output = _code_agent_output(
        state,
        decision="r_code_generated",
        status="needs_review",
        reason="Generated R code from an approved spec and stopped before execution.",
        outputs={
            "code_path": artifacts.code_artifact.path,
            "spec_source": spec_source,
            "next_action": "review_code",
        },
        risk_flags=package.risk_points,
        artifact_ids=code_artifact_ids,
    )
    static_input = _static_review_agent_input(
        state,
        code_artifact_id=artifacts.code_artifact.artifact_id,
        target_spec=target_spec,
    )
    static_output = _static_review_agent_output(
        state,
        decision="static_check_passed_for_review",
        status="warning",
        reason="Limited deterministic static checks passed before human code review.",
        outputs={"static_check_path": str(static_check_path.as_posix())},
        risk_flags=["static_check_limited_scope"],
        artifact_ids=[static_artifact.artifact_id],
    )
    return {
        "status": "needs_review",
        "route": "human_review",
        "current_interrupt": "code_review",
        "generated_code": package.r_code,
        "code_path": artifacts.code_artifact.path,
        "llm_response_path": artifacts.response_artifact.path,
        "parsed_response_path": artifacts.package_artifact.path,
        "static_check_path": str(static_check_path.as_posix()),
        "llm_provider": llm_provider,
        "llm_model": llm_model,
        "provider_alias": provider_alias,
        "transport": transport,
        "provider_base_url": provider_base_url,
        "not_real_derivation": not_real_derivation,
        "code_assumptions": package.assumptions,
        "code_risk_points": package.risk_points,
        "code_used_inputs": package.used_inputs,
        "code_expected_outputs": package.expected_outputs,
        "next_action": "review_code",
        "audit_artifacts": [
            prompt_artifact,
            artifacts.response_artifact,
            artifacts.package_artifact,
            artifacts.code_artifact,
            static_artifact,
        ],
        "agent_node_inputs": [code_input, static_input],
        "agent_node_outputs": [code_output, static_output],
        "agent_decisions": list(code_output["agent_decisions"]) + list(static_output["agent_decisions"]),
        "risk_flags": package.risk_points + ["static_check_limited_scope"],
    }


def _code_agent_input(state: DatasetGraphState, context_dict: dict[str, object]) -> dict[str, object]:
    return build_agent_node_input(
        agent="code_agent",
        node="generate_r_code_agent",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        task="Generate review-required R code from an approved ADaM specification and prepared study context.",
        inputs={
            "spec_source": state.get("spec_source"),
            "context_keys": sorted(str(key) for key in context_dict.keys()),
            "datasets_included": _datasets_included(context_dict),
            "variables_included_count": len(_variables_included(context_dict)),
        },
        artifact_ids=[item for item in [_artifact_id(state.get("product_context_artifact"))] if item],
        risk_flags=list(state.get("risk_flags", [])),
        evidence_bundle_id=state.get("evidence_bundle_id"),
        reference_query_ids=[
            str(item.get("query_id"))
            for item in state.get("reference_queries", [])
            if isinstance(item, dict) and item.get("query_id")
        ],
    )


def _code_agent_output(
    state: DatasetGraphState,
    *,
    decision: str,
    status: str,
    reason: str,
    outputs: dict[str, object],
    artifact_ids: list[str],
    risk_flags: list[str] | None = None,
) -> dict[str, object]:
    return build_agent_node_output(
        agent="code_agent",
        node="generate_r_code_agent",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        status=status,
        decision=decision,
        reason=reason,
        outputs=outputs,
        artifact_ids=artifact_ids,
        risk_flags=risk_flags or [],
    )


def _static_review_agent_input(
    state: DatasetGraphState,
    *,
    code_artifact_id: str,
    target_spec: dict[str, object],
) -> dict[str, object]:
    return build_agent_node_input(
        agent="static_review_agent",
        node="generate_r_code_agent",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        task="Run limited deterministic static checks on generated R code before human code review.",
        inputs={
            "code_artifact_id": code_artifact_id,
            "required_identifier_count": len(_required_identifiers_from_spec(target_spec)),
            "required_identifier_source_id": _spec_source_id(target_spec),
            "scope": "limited_policy_check",
        },
        artifact_ids=[code_artifact_id],
        risk_flags=["static_check_limited_scope"],
    )


def _static_review_agent_output(
    state: DatasetGraphState,
    *,
    decision: str,
    status: str,
    reason: str,
    outputs: dict[str, object],
    artifact_ids: list[str],
    risk_flags: list[str] | None = None,
) -> dict[str, object]:
    return build_agent_node_output(
        agent="static_review_agent",
        node="generate_r_code_agent",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        status=status,
        decision=decision,
        reason=reason,
        outputs=outputs,
        artifact_ids=artifact_ids,
        risk_flags=risk_flags or [],
    )


def execute_approved_code_node(state: DatasetGraphState) -> DatasetGraphState:
    """Run approved generated R through the graph-owned execution boundary."""

    if not _is_graph_product_execute_mode(state):
        return {}
    study_dir = state.get("study_dir")
    if not study_dir:
        return _product_failure("input_error", "execute_approved_code requires study_dir")
    target = state["dataset"]
    execution_input = _execution_agent_input(state)
    try:
        result = execute_approved_r_code(
            study_dir=study_dir,
            study_id=state["study_id"],
            run_id=state["run_id"],
            dataset=target,
            rscript_path=state.get("rscript_path") or None,
        )
    except GraphExecutionError as exc:
        return _product_failure(
            "input_error",
            str(exc),
            current_interrupt="code_review",
            next_action="review_code",
        )

    artifact_ids = [artifact.artifact_id for artifact in result.artifacts.values()]
    execution_output = _execution_agent_output(
        state,
        decision="r_execution_completed" if not result.terminal_failure else "r_execution_terminal_failure",
        status="completed" if not result.terminal_failure else "terminal_failure",
        reason="Executed approved generated R code in the configured R boundary.",
        outputs={
            "output_path": result.output_path or "",
            "validation_status": result.validation_status,
            "terminal_failure": result.terminal_failure,
        },
        risk_flags=["terminal_failure"] if result.terminal_failure else [],
        artifact_ids=artifact_ids,
    )
    return {
        "status": "completed" if not result.terminal_failure else "failed",
        "route": "success" if not result.terminal_failure else "fail",
        "failure_type": "sandbox_error" if result.terminal_failure else None,
        "response_status": result.response_status,
        "real_run_completed": not result.terminal_failure,
        "real_run_error": "; ".join(result.errors),
        "real_validation_status": result.validation_status,
        "real_run_artifacts": result.artifacts,
        "failure_records": result.failure_records,
        "output_path": result.output_path or "",
        "validation_report_path": result.validation_report_path,
        "diagnostics_path": result.diagnostics_path or "",
        "terminal_failure": result.terminal_failure,
        "execution_errors": result.errors,
        "execution_warnings": result.warnings,
        "validation_report": result.validation_report,
        "current_interrupt": "terminal_failure" if result.terminal_failure else None,
        "next_action": "review_diagnostics" if result.terminal_failure else "review_output",
        "audit_artifacts": list(result.artifacts.values()),
        "agent_node_inputs": [execution_input],
        "agent_node_outputs": [execution_output],
        "agent_decisions": list(execution_output["agent_decisions"]),
        "risk_flags": ["terminal_failure"] if result.terminal_failure else [],
        "sandbox_runs": 1,
    }


def _execution_agent_input(state: DatasetGraphState) -> dict[str, object]:
    artifact_ids = [
        item
        for item in [
            _artifact_id(state.get("product_context_artifact")),
            _artifact_id(state.get("generated_code_artifact")),
            _artifact_id(state.get("approved_code_artifact")),
        ]
        if item
    ]
    return build_agent_node_input(
        agent="execution_agent",
        node="execute_approved_code",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        task="Execute approved generated R code in the configured R boundary and report validation status.",
        inputs={
            "code_path": state.get("code_path"),
            "static_check_path": state.get("static_check_path"),
            "rscript_path_provided": bool(state.get("rscript_path")),
        },
        artifact_ids=artifact_ids,
        risk_flags=list(state.get("risk_flags", [])),
    )


def _execution_agent_output(
    state: DatasetGraphState,
    *,
    decision: str,
    status: str,
    reason: str,
    outputs: dict[str, object],
    artifact_ids: list[str],
    risk_flags: list[str] | None = None,
) -> dict[str, object]:
    return build_agent_node_output(
        agent="execution_agent",
        node="execute_approved_code",
        study_id=state["study_id"],
        run_id=state["run_id"],
        dataset=state["dataset"],
        status=status,
        decision=decision,
        reason=reason,
        outputs=outputs,
        artifact_ids=artifact_ids,
        risk_flags=risk_flags or [],
    )


def run_llm_downstream_stubbed_node(state: DatasetGraphState) -> DatasetGraphState:
    """Run the generic downstream LLM/R service with mock boundaries."""

    study_dir = state.get("study_dir")
    if not study_dir:
        return {
            "status": "failed",
            "failure_type": "input_error",
            "route": "fail",
            "real_run_completed": False,
            "real_run_error": f"execution_mode={LLM_DOWNSTREAM_STUBBED_MODE} requires study_dir",
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
            if key in {"llm_context", "llm_response", "llm_parsed_response", "static_check", "validation_report", "failure_report"}
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
        mode = state.get("execution_mode", LLM_DOWNSTREAM_PROVIDER_MODE)
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
        run_dir = Path(study_dir) / "runs" / state["run_id"]
        output_path = run_dir / "outputs" / f"{state['dataset'].strip().lower()}.csv"
        r_runner = (
            LocalRscriptSandboxRunner(
                run_dir=run_dir,
                rscript_path=state.get("rscript_path") or None,
                allowed_output_paths=[output_path],
            )
            if use_local_r
            else None
        )
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

    if _is_graph_product_prepare_mode(state) or _is_graph_product_generate_code_mode(state) or _is_graph_product_execute_mode(state):
        return {}
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

    if scenario in {"sandbox_failure", "fail_adsl"}:
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

    if _is_graph_product_prepare_mode(state) or _is_graph_product_generate_code_mode(state) or _is_graph_product_execute_mode(state):
        return summarize_product_prepare(state)
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


def summarize_product_prepare(state: DatasetGraphState) -> DatasetGraphState:
    """Summarize the graph-native product preparation gate without running R."""

    dataset = state["dataset"]
    status = state.get("status", "needs_review")
    context_artifact = state.get("product_context_artifact")
    output_artifact_ids = [context_artifact.artifact_id] if context_artifact is not None else []
    failure_ids = []
    if state.get("failure_type"):
        failure_ids.append(f"failure_{dataset.lower()}_product_prepare")
    summary = DatasetResultSummary(
        dataset=dataset,
        status=status,
        output_artifact_ids=output_artifact_ids,
        validation_status="not_run",
        compare_status="not_run",
        failure_ids=failure_ids,
        metadata={
            "graph_product_prepare": True,
            "current_interrupt": state.get("current_interrupt"),
            "spec_source": state.get("spec_source"),
            "draft_spec_required": state.get("draft_spec_required", False),
            "next_action": state.get("next_action"),
            "input_spec_path": state.get("input_spec_path"),
            "approved_spec_path": state.get("approved_spec_path"),
            "draft_spec_path": state.get("draft_spec_path"),
            "draft_spec_prompt_path": state.get("draft_spec_prompt_path"),
            "draft_spec_response_path": state.get("draft_spec_response_path"),
            "code_path": state.get("code_path"),
            "llm_response_path": state.get("llm_response_path"),
            "parsed_response_path": state.get("parsed_response_path"),
            "static_check_path": state.get("static_check_path"),
            "output_path": state.get("output_path"),
            "validation_report_path": state.get("validation_report_path"),
            "diagnostics_path": state.get("diagnostics_path"),
            "response_status": state.get("response_status"),
            "terminal_failure": state.get("terminal_failure", False),
            "execution_errors": state.get("execution_errors", []),
            "execution_warnings": state.get("execution_warnings", []),
            "code_assumptions": state.get("code_assumptions", []),
            "code_risk_points": state.get("code_risk_points", []),
            "code_used_inputs": state.get("code_used_inputs", []),
            "code_expected_outputs": state.get("code_expected_outputs", []),
            "warnings": state.get("product_context_warnings", []),
            "agent_decisions": state.get("agent_decisions", []),
            "agent_node_inputs": state.get("agent_node_inputs", []),
            "agent_node_outputs": state.get("agent_node_outputs", []),
            "risk_flags": state.get("risk_flags", []),
        },
    )
    artifacts = [context_artifact] if context_artifact is not None else []
    return {
        "status": status,
        "summary": summary,
        "audit_artifacts": artifacts,
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


def build_dataset_graph(*, include_legacy_stub_chain: bool = False):
    """Build the dataset-level graph.

    The default graph is the product graph. The old synthetic stub chain is
    available only through the explicit legacy/test compiler below.
    """

    graph = StateGraph(DatasetGraphState)
    graph.add_node(
        "prepare_dataset",
        prepare_legacy_stub_dataset if include_legacy_stub_chain else prepare_dataset,
    )
    graph.add_node("draft_spec_agent", draft_spec_agent_node)
    graph.add_node("generate_r_code_agent", generate_r_code_agent_node)
    graph.add_node("execute_approved_code", execute_approved_code_node)
    graph.add_node("summarize_dataset", summarize_dataset)
    if include_legacy_stub_chain:
        graph.add_node("draft_lineage_stub", draft_lineage_stub)
        graph.add_node("draft_spec_stub", draft_spec_stub)
        graph.add_node("route_risk_stub", route_risk_stub)
        graph.add_node("human_review_stub", human_review_stub)
        graph.add_node("generate_code_stub", generate_code_stub)
        graph.add_node("run_sandbox_stub", run_sandbox_stub)
        graph.add_node("classify_result_stub", classify_result_stub)
        graph.add_node("repair_code_stub", repair_code_stub)
        graph.add_node("revise_spec_stub", revise_spec_stub)

    graph.add_edge(START, "prepare_dataset")
    route_map = {
        "draft_spec_agent": "draft_spec_agent",
        "generate_r_code_agent": "generate_r_code_agent",
        "execute_approved_code": "execute_approved_code",
        "summarize": "summarize_dataset",
    }
    if include_legacy_stub_chain:
        route_map["stub_chain"] = "draft_lineage_stub"
    graph.add_conditional_edges("prepare_dataset", route_after_product_context, route_map)
    graph.add_edge("draft_spec_agent", "summarize_dataset")
    graph.add_edge("generate_r_code_agent", "summarize_dataset")
    graph.add_edge("execute_approved_code", "summarize_dataset")
    if include_legacy_stub_chain:
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
        graph.add_conditional_edges(
            "human_review_stub",
            route_after_product_prepare_review,
            {
                "summarize": "summarize_dataset",
                "continue": "generate_code_stub",
            },
        )
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


def route_after_product_prepare_review(state: DatasetGraphState) -> str:
    """Stop graph-native product preparation at its review gate."""

    if _is_graph_product_prepare_mode(state) or _is_graph_product_generate_code_mode(state) or _is_graph_product_execute_mode(state):
        return "summarize"
    return "continue"


def route_after_product_context(state: DatasetGraphState) -> str:
    """Route missing-spec product contexts through draft spec generation."""

    if state.get("status") == "failed":
        return "summarize"
    if _is_llm_downstream_mode(state) or _is_retired_adsl_template_mode(state):
        return "summarize"
    if _is_graph_product_execute_mode(state):
        return "execute_approved_code"
    if _is_graph_product_generate_code_mode(state):
        return "generate_r_code_agent"
    if _is_graph_product_prepare_mode(state) and state.get("spec_source") == "missing_input_spec":
        return "draft_spec_agent"
    if _is_graph_product_prepare_mode(state):
        return "summarize"
    if _is_legacy_stub_mode(state) and _is_legacy_stub_graph_enabled(state):
        return "stub_chain"
    return "summarize"


def _llm_request_for_code_generation(
    *,
    prompt: str,
    target: str,
    study_id: str,
    run_id: str,
    provider_config: LLMProviderConfig,
    exposure: LLMExposureConfig,
    context_dict: dict[str, object],
    prompt_artifact: ArtifactRef,
):
    from adam_agent.llm.clients import LLMRequest

    return LLMRequest(
        prompt=prompt,
        system_prompt=(
            "You generate R code for ADaM dataset creation. Return only JSON with keys "
            "dataset, r_code, assumptions, risk_points, used_inputs, expected_outputs. "
            "Do not include markdown fences. Do not use network, shell, install.packages, "
            "or filesystem writes outside the runtime output path."
        ),
        provider=provider_config.provider,
        model=provider_config.model,
        exposure=exposure,
        node="generate_downstream_code_for_review",
        call_id=f"llm_{run_id}_{target.lower()}_generate_code",
        max_tokens=provider_config.max_tokens,
        datasets_included=_datasets_included(context_dict),
        variables_included=_variables_included(context_dict),
        sample_row_counts=_sample_row_counts(context_dict),
        subject_level_data_included=bool(_sample_row_counts(context_dict)),
        prompt_artifact_id=prompt_artifact.artifact_id,
        response_artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{target.lower()}",
        redaction_policy="phase8_code_generation_review_policy",
    )


def _llm_call_field(call_record: object, name: str, default: object | None) -> object | None:
    return getattr(call_record, name, default)


def _not_real_generation(
    provider_config: LLMProviderConfig,
    *,
    provider: object | None,
    provider_alias: object | None,
    transport: object | None,
) -> bool:
    configured_provider = provider_config.provider.strip().lower()
    provider_value = str(provider or "").strip().lower()
    provider_alias_value = str(provider_alias or "").strip().lower()
    transport_value = str(transport or "").strip().lower()
    return configured_provider == "mock" or provider_value == "mock" or provider_alias_value == "mock" or transport_value == "mock"


def _datasets_included(context: dict[str, object]) -> list[str]:
    datasets = list((context.get("source_dataset_profiles") or {}).keys())
    datasets.extend((context.get("resolved_dependencies") or {}).keys())
    return [str(dataset) for dataset in datasets]


def _variables_included(context: dict[str, object]) -> list[str]:
    variables: list[str] = []
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        profiles = context.get(section) or {}
        if not isinstance(profiles, dict):
            continue
        for profile in profiles.values():
            if not isinstance(profile, dict):
                continue
            for column in profile.get("columns", []):
                column_name = str(column)
                if column_name not in variables:
                    variables.append(column_name)
    return variables


def _sample_row_counts(context: dict[str, object]) -> dict[str, int]:
    counts: dict[str, int] = {}
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        profiles = context.get(section) or {}
        if not isinstance(profiles, dict):
            continue
        for dataset, profile in profiles.items():
            if not isinstance(profile, dict):
                continue
            sample_rows = profile.get("sample_rows", [])
            if sample_rows:
                counts[str(dataset)] = min(len(sample_rows), MAX_SAMPLE_ROWS_IN_PROMPT)
    return counts


def _write_static_check_report(
    *,
    study_dir: Path,
    run_id: str,
    study_id: str,
    target: str,
    code_path: Path,
    required_identifiers: list[str] | None = None,
    required_identifier_source_id: str | None = None,
) -> Path:
    static_dir = study_dir / "runs" / run_id / "static_checks"
    static_dir.mkdir(parents=True, exist_ok=True)
    path = static_dir / f"{target.lower()}_static_check.json"
    report = run_generated_r_static_checks(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        code_path=code_path,
        expected_output_path=f"outputs/{target.lower()}.csv",
        required_identifiers=required_identifiers,
        required_identifier_source_id=required_identifier_source_id,
    )
    write_static_rule_report(report, path=path)
    assert_no_blocking_static_findings(report)
    return path


def _required_identifiers_from_spec(target_spec: dict[str, object] | None) -> list[str]:
    if not isinstance(target_spec, dict):
        return []
    variables = target_spec.get("variables")
    if not isinstance(variables, list):
        return []
    identifiers: list[str] = []
    for item in variables:
        if not isinstance(item, dict):
            continue
        name = item.get("variable") or item.get("name") or item.get("Variable")
        text = str(name or "").strip().upper()
        if text and text not in identifiers:
            identifiers.append(text)
    return identifiers[:50]


def _spec_source_id(target_spec: dict[str, object] | None) -> str | None:
    if not isinstance(target_spec, dict):
        return None
    artifact_id = str(target_spec.get("artifact_id") or "").strip()
    if artifact_id:
        return artifact_id
    path = str(target_spec.get("path") or "").strip()
    if path:
        return Path(path).as_posix()
    return None


def _tool_log_artifact(state: DatasetGraphState, path: Path, *, kind_id: str) -> ArtifactRef:
    dataset = state["dataset"]
    return ArtifactRef(
        artifact_id=f"{kind_id}_{state['study_id'].lower()}_{state['run_id']}_{dataset.lower()}",
        kind="tool_log",
        path=str(path.as_posix()),
        sha256=f"sha256:{sha256_file(path)}",
        dataset=dataset,
        format="json",
        role="audit",
        metadata={kind_id: True},
    )


def _artifact_id(value: object) -> str | None:
    return value.artifact_id if isinstance(value, ArtifactRef) else None


def _build_llm_client_for_state(state: DatasetGraphState, provider_config: LLMProviderConfig):
    builder = state.get("llm_client_builder")
    if builder is not None and callable(builder):
        return builder(provider_config)
    return build_llm_client(provider_config)


def _build_target_context_for_state(state: DatasetGraphState):
    builder = state.get("target_context_builder")
    if builder is not None and callable(builder):
        return builder
    return build_target_llm_context


def _product_failure(
    failure_type: str,
    message: str,
    *,
    current_interrupt: str | None = None,
    next_action: str | None = None,
) -> DatasetGraphState:
    return {
        "status": "failed",
        "failure_type": failure_type,
        "route": "fail",
        "current_interrupt": current_interrupt,
        "next_action": next_action or "fix_error",
        "real_run_completed": False,
        "real_run_error": message,
        "real_run_artifacts": {},
        "real_validation_status": "not_run",
    }


def _approved_draft_spec_payload(study_dir: Path, run_id: str, target: str) -> dict[str, object] | None:
    target_lower = target.strip().lower()
    graph_spec_state = _approved_draft_spec_state(study_dir, run_id, target)
    approved_path = study_dir / "runs" / run_id / "approved_specs" / f"{target_lower}_approved_spec.json"
    review_path = study_dir / "runs" / run_id / "reviews" / f"{target_lower}_draft_spec_review.json"
    if not approved_path.exists() or not approved_path.is_file():
        return None
    review = _read_json_if_exists(review_path)
    if review.get("decision") != "approve" or review.get("approved") is not True:
        return None
    if not graph_spec_state:
        raise ValueError("Approved draft spec must be recorded in graph state before code generation.")
    graph_review_path = graph_spec_state.get("review_path")
    graph_approved_path = graph_spec_state.get("approved_spec_path")
    if str(Path(str(graph_review_path or "")).as_posix()) != str(review_path.as_posix()):
        raise ValueError("Graph draft-spec approval points to a different review artifact. Review the draft spec again.")
    if str(Path(str(graph_approved_path or "")).as_posix()) != str(approved_path.as_posix()):
        raise ValueError("Graph draft-spec approval points to a different approved spec. Review the draft spec again.")
    parsed = _read_json_if_exists(approved_path)
    approved_fingerprint = parsed.get("input_fingerprint") or review.get("input_fingerprint")
    current_fingerprint = input_fingerprint(study_dir)
    if not isinstance(approved_fingerprint, dict) or not approved_fingerprint.get("digest"):
        raise ValueError(
            "Approved draft spec is missing its input fingerprint. "
            "Regenerate and approve the draft spec before generating R code."
        )
    if approved_fingerprint.get("digest") != current_fingerprint.get("digest"):
        diff = compare_fingerprints(approved_fingerprint, current_fingerprint)
        raise ValueError(
            "Approved draft spec is stale because study inputs changed after approval. "
            "Regenerate and approve the draft spec before generating R code. "
            f"Input diff: added={diff.get('added', [])}; removed={diff.get('removed', [])}; "
            f"changed={diff.get('changed_files', [])}."
        )
    approved_spec_sha = review.get("approved_spec_sha256")
    if not approved_spec_sha:
        raise ValueError(
            "Approved draft spec review is missing the approved spec hash. "
            "Review and approve the draft spec again before generating R code."
        )
    current_spec_sha = f"sha256:{sha256_file(approved_path)}"
    if approved_spec_sha != current_spec_sha:
        raise ValueError("Approved draft spec changed after approval. Review and approve the draft spec again.")
    graph_approved_sha = graph_spec_state.get("approved_spec_sha256")
    if graph_approved_sha != current_spec_sha:
        raise ValueError("Graph draft-spec approval is stale. Review and approve the draft spec again.")
    return {
        "artifact_id": f"approved_draft_spec_{target_lower}",
        "path": str(approved_path.as_posix()),
        "format": "json",
        "sha256": f"sha256:{sha256_file(approved_path)}",
        "text": _read_text_if_exists(approved_path, limit_chars=500000),
        "json": parsed,
        "draft": True,
        "approved": True,
        "source": "user_approved_draft_spec",
        "review_path": str(review_path.as_posix()),
    }


def _terminal_failure_requires_new_draft_spec(study_dir: Path, run_id: str, target: str) -> bool:
    graph_state_path = study_dir / "runs" / run_id / "graph_state.json"
    if not graph_state_path.exists() or not graph_state_path.is_file():
        return False
    try:
        graph_state = StudyRunState.model_validate_json(graph_state_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return False
    dataset_state = graph_state.datasets.get(target.strip().upper())
    if dataset_state is None:
        return False
    if dataset_state.execution_state.get("terminal_failure_followup_consumed_by"):
        return False
    review = dataset_state.execution_state.get("terminal_failure_review")
    if not isinstance(review, dict):
        return False
    action = str(review.get("action") or "").strip().lower()
    return action in {"revise_spec", "request_new_input"}


def _approved_draft_spec_state(study_dir: Path, run_id: str, target: str) -> dict[str, object]:
    graph_state_path = study_dir / "runs" / run_id / "graph_state.json"
    if not graph_state_path.exists() or not graph_state_path.is_file():
        return {}
    try:
        graph_state = StudyRunState.model_validate_json(graph_state_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    dataset_state = graph_state.datasets.get(target.strip().upper())
    if dataset_state is None:
        return {}
    spec_state = dict(dataset_state.spec_state)
    if spec_state.get("status") != "approved" or spec_state.get("decision") != "approve":
        return {}
    return spec_state


def _read_json_if_exists(path: str | Path) -> dict[str, object]:
    artifact_path = Path(path)
    if not artifact_path.exists() or not artifact_path.is_file():
        return {}
    try:
        payload = json.loads(artifact_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, UnicodeDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def _read_text_if_exists(path: str | Path, *, limit_chars: int) -> str:
    artifact_path = Path(path)
    if not artifact_path.exists() or not artifact_path.is_file():
        return ""
    try:
        return artifact_path.read_text(encoding="utf-8", errors="replace")[:limit_chars]
    except OSError:
        return ""


def _write_json(path: str | Path, payload: dict[str, object]) -> None:
    artifact_path = Path(path)
    artifact_path.parent.mkdir(parents=True, exist_ok=True)
    artifact_path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")


def _merge_json_artifact(path: str | Path, payload: dict[str, object]) -> None:
    current = _read_json_if_exists(path)
    current.update(payload)
    _write_json(path, current)


def compile_dataset_graph():
    """Compile the product dataset graph without its own checkpointer."""

    return build_dataset_graph().compile(name="dataset_graph")


def compile_legacy_stub_dataset_graph():
    """Compile the explicit legacy/test dataset graph that includes stub nodes."""

    return build_dataset_graph(include_legacy_stub_chain=True).compile(name="legacy_stub_dataset_graph")

