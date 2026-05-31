"""Runtime state for Phase 3 LangGraph skeletons."""

from __future__ import annotations

import operator
from typing import Annotated, Literal, TypedDict

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary


GraphRunStatus = Literal[
    "pending",
    "planned",
    "running",
    "needs_review",
    "blocked",
    "completed",
    "completed_stub",
    "failed",
]
StubScenario = Literal["success", "code_error_then_success", "spec_error_then_success", "sandbox_failure", "fail_adsl"]
DatasetRoute = Literal["continue", "human_review", "repair_code", "revise_spec", "success", "fail"]


class DatasetTask(TypedDict, total=False):
    """Minimal payload sent from StudyGraph to one DatasetGraph run."""

    study_id: str
    run_id: str
    dataset: str
    stub_scenario: StubScenario
    dependency_status: str
    max_repair_attempts: int
    execution_mode: str
    study_dir: str
    rscript_path: str
    dependency_resolution: list[dict[str, object]]
    llm_exposure: dict[str, object]
    llm_provider: dict[str, object]


class BlockedDataset(TypedDict):
    """Study-level record for a dataset skipped because a dependency failed."""

    dataset: str
    reason: str
    blocked_by: str


class DatasetGraphState(TypedDict, total=False):
    """In-flight state for one dataset subgraph."""

    study_id: str
    run_id: str
    dataset: str
    stub_scenario: StubScenario
    dependency_status: str
    status: GraphRunStatus
    repair_attempts: int
    max_repair_attempts: int
    route: DatasetRoute
    failure_type: str | None
    lineage_ready: bool
    draft_spec_ready: bool
    human_review_required: bool
    generated_code: str
    sandbox_runs: int
    execution_mode: str
    study_dir: str
    rscript_path: str
    current_interrupt: str | None
    product_context_ready: bool
    product_context_warnings: list[str]
    product_context_artifact: ArtifactRef
    product_context: dict[str, object]
    spec_source: str
    input_spec_path: str
    approved_spec_path: str
    draft_spec_path: str
    draft_spec_prompt_path: str
    draft_spec_response_path: str
    draft_spec_variables: list[dict[str, object]]
    draft_spec_required: bool
    generated_code: str
    code_path: str
    output_path: str
    validation_report_path: str
    diagnostics_path: str
    response_status: str
    terminal_failure: bool
    execution_errors: list[str]
    execution_warnings: list[str]
    validation_report: dict[str, object]
    llm_response_path: str
    parsed_response_path: str
    static_check_path: str
    code_assumptions: list[str]
    code_risk_points: list[str]
    code_used_inputs: list[str]
    code_expected_outputs: list[str]
    next_action: str
    dependency_resolution: list[dict[str, object]]
    llm_exposure: dict[str, object]
    llm_provider: dict[str, object]
    llm_client_builder: object
    target_context_builder: object
    force_new_draft_spec: bool
    native_draft_spec_review: bool
    native_draft_spec_review_status: str
    native_draft_spec_review_resume: dict[str, object]
    native_code_review: bool
    native_code_review_status: str
    native_code_review_resume: dict[str, object]
    native_full_loop: bool
    native_terminal_failure_review: bool
    native_terminal_failure_review_status: str
    native_terminal_failure_review_resume: dict[str, object]
    human_commands: Annotated[list[dict[str, object]], operator.add]
    legacy_stub_graph_enabled: bool
    evidence_bundle_id: str
    reference_queries: list[dict[str, object]]
    agent_node_inputs: Annotated[list[dict[str, object]], operator.add]
    agent_node_outputs: Annotated[list[dict[str, object]], operator.add]
    real_run_completed: bool
    real_run_error: str
    real_run_artifacts: dict[str, ArtifactRef]
    real_validation_status: str
    real_run_metadata: dict[str, object]
    failure_records: list[FailureRecord]
    recommended_route: str | None
    summary: DatasetResultSummary
    audit_artifacts: Annotated[list[ArtifactRef], operator.add]
    agent_decisions: Annotated[list[dict[str, object]], operator.add]
    risk_flags: Annotated[list[str], operator.add]


class StudyGraphState(TypedDict, total=False):
    """In-flight state for the study-level graph."""

    study_id: str
    run_id: str
    status: GraphRunStatus
    target_datasets: list[str]
    requested_datasets: list[str]
    auto_added_datasets: list[str]
    unsupported_datasets: list[str]
    stub_scenarios: dict[str, StubScenario]
    execution_mode: str
    study_dir: str
    rscript_path: str
    llm_exposure: dict[str, object]
    llm_provider: dict[str, object]
    graph_gateway_mode: str
    current_interrupt: str | None
    dependency_graph: dict[str, list[str]]
    dataset_dependencies: dict[str, list[str]]
    dependency_decisions: list[dict[str, object]]
    dependency_resolution: list[dict[str, object]]
    dependency_action_required: bool
    approved_dependency_datasets: list[str]
    runnable_datasets: list[str]
    satisfied_dependency_datasets: list[str]
    dependency_evidence: str
    dependency_evidence_records: list[dict[str, object]]
    dependency_planning_warnings: list[str]
    dependency_review_status: str
    dependency_plan_artifact: ArtifactRef
    dependency_review_artifact: ArtifactRef
    human_commands: Annotated[list[dict[str, object]], operator.add]
    native_dependency_review_resume: dict[str, object]
    execution_batches: list[list[str]]
    foundation_datasets: list[str]
    downstream_datasets: list[str]
    dataset_tasks: list[DatasetTask]
    downstream_tasks: list[DatasetTask]
    dataset_results: Annotated[list[DatasetResultSummary], operator.add]
    blocked_datasets: Annotated[list[BlockedDataset], operator.add]
    audit_artifacts: Annotated[list[ArtifactRef], operator.add]
    agent_decisions: Annotated[list[dict[str, object]], operator.add]
    agent_node_inputs: Annotated[list[dict[str, object]], operator.add]
    agent_node_outputs: Annotated[list[dict[str, object]], operator.add]
    risk_flags: Annotated[list[str], operator.add]
    agent_audit_summary: dict[str, object]
    audit_manifest: ArtifactRef
