"""Runtime state for Phase 3 LangGraph skeletons."""

from __future__ import annotations

import operator
from typing import Annotated, Literal, TypedDict

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.routing import FailureRecord
from adam_agent.schemas.states import DatasetResultSummary


GraphRunStatus = Literal["pending", "running", "blocked", "completed", "failed"]
StubScenario = Literal["success", "code_error_then_success", "spec_error_then_success", "fail_adsl"]
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
    real_run_completed: bool
    real_run_error: str
    real_run_artifacts: dict[str, ArtifactRef]
    real_validation_status: str
    failure_records: list[FailureRecord]
    recommended_route: str | None
    summary: DatasetResultSummary
    audit_artifacts: Annotated[list[ArtifactRef], operator.add]


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
    dependency_graph: dict[str, list[str]]
    dataset_dependencies: dict[str, list[str]]
    dependency_decisions: list[dict[str, object]]
    dependency_evidence: str
    dependency_evidence_records: list[dict[str, object]]
    dependency_planning_warnings: list[str]
    dependency_review_status: str
    dependency_plan_artifact: ArtifactRef
    dependency_review_artifact: ArtifactRef
    execution_batches: list[list[str]]
    foundation_datasets: list[str]
    downstream_datasets: list[str]
    dataset_tasks: list[DatasetTask]
    downstream_tasks: list[DatasetTask]
    dataset_results: Annotated[list[DatasetResultSummary], operator.add]
    blocked_datasets: Annotated[list[BlockedDataset], operator.add]
    audit_artifacts: Annotated[list[ArtifactRef], operator.add]
    audit_manifest: ArtifactRef
