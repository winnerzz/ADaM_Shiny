"""Dependency availability and user-decision records for study orchestration."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


ALLOWED_DEPENDENCY_ACTIONS = [
    "provide_existing_dataset",
    "approve_system_generation",
    "skip_target",
    "pause_run",
]


@dataclass(frozen=True)
class DependencyResolution:
    """Availability and decision status for one target-to-dependency edge."""

    target_dataset: str
    required_dataset: str
    available: bool
    artifact_path: str | None
    artifact_source: str | None
    resolution_status: str
    allowed_actions: list[str]
    selected_action: str | None
    reason: str

    def as_dict(self) -> dict[str, object]:
        return {
            "target_dataset": self.target_dataset,
            "required_dataset": self.required_dataset,
            "available": self.available,
            "artifact_path": self.artifact_path,
            "artifact_source": self.artifact_source,
            "resolution_status": self.resolution_status,
            "allowed_actions": self.allowed_actions,
            "selected_action": self.selected_action,
            "reason": self.reason,
        }


def resolve_dependency_availability(
    dependencies: dict[str, list[str]],
    *,
    requested_datasets: list[str],
    resolution_scope_datasets: list[str] | None = None,
    study_dir: str | Path | None = None,
    run_id: str | None = None,
    approved_dependency_datasets: list[str] | None = None,
) -> list[DependencyResolution]:
    """Resolve whether required dependency datasets are already available."""

    approvals = _normalize(approved_dependency_datasets or [])
    requested = _normalize(requested_datasets)
    resolution_scope = _normalize(resolution_scope_datasets or requested)
    requested_set = set(requested)
    records: list[DependencyResolution] = []
    for target in resolution_scope:
        for required in dependencies.get(target, []):
            artifact = _find_dependency_artifact(required, study_dir=study_dir, run_id=run_id)
            if artifact:
                records.append(
                    DependencyResolution(
                        target_dataset=target,
                        required_dataset=required,
                        available=True,
                        artifact_path=str(artifact[0].as_posix()),
                        artifact_source=artifact[1],
                        resolution_status="available",
                        allowed_actions=[],
                        selected_action="use_existing_dataset",
                        reason=f"{target} depends on {required}; a usable {required} artifact was found.",
                    )
                )
                continue
            if required in requested_set:
                records.append(
                    DependencyResolution(
                        target_dataset=target,
                        required_dataset=required,
                        available=False,
                        artifact_path=None,
                        artifact_source=None,
                        resolution_status="requested_for_system_generation",
                        allowed_actions=[],
                        selected_action="requested_dataset",
                        reason=f"{target} depends on {required}; {required} was explicitly requested in this run.",
                    )
                )
                continue
            if required in approvals:
                records.append(
                    DependencyResolution(
                        target_dataset=target,
                        required_dataset=required,
                        available=False,
                        artifact_path=None,
                        artifact_source=None,
                        resolution_status="approved_for_system_generation",
                        allowed_actions=[],
                        selected_action="approve_system_generation",
                        reason=f"{target} depends on {required}; system generation was explicitly approved.",
                    )
                )
                continue
            records.append(
                DependencyResolution(
                    target_dataset=target,
                    required_dataset=required,
                    available=False,
                    artifact_path=None,
                    artifact_source=None,
                    resolution_status="user_action_required",
                    allowed_actions=list(ALLOWED_DEPENDENCY_ACTIONS),
                    selected_action=None,
                    reason=f"{target} depends on {required}, but no usable {required} artifact was found.",
                )
            )
    return records


def approved_dependency_targets(resolutions: list[DependencyResolution]) -> list[str]:
    """Datasets explicitly approved for system generation."""

    approved: list[str] = []
    for record in resolutions:
        if record.resolution_status == "approved_for_system_generation" and record.required_dataset not in approved:
            approved.append(record.required_dataset)
    return approved


def available_dependency_targets(resolutions: list[DependencyResolution]) -> list[str]:
    """Dependency datasets already satisfied by existing artifacts."""

    available: list[str] = []
    for record in resolutions:
        if record.resolution_status == "available" and record.required_dataset not in available:
            available.append(record.required_dataset)
    return available


def unresolved_dependency_targets(resolutions: list[DependencyResolution]) -> list[str]:
    """Target datasets that cannot run until the user resolves dependencies."""

    targets: list[str] = []
    for record in resolutions:
        if record.resolution_status == "user_action_required" and record.target_dataset not in targets:
            targets.append(record.target_dataset)
    return targets


def missing_dependency_blocks(
    resolutions: list[DependencyResolution],
    *,
    reportable_datasets: list[str] | None = None,
) -> list[dict[str, str]]:
    """Return block records for unresolved dependency requirements."""

    reportable = set(_normalize(reportable_datasets)) if reportable_datasets is not None else None
    blocked_by_dataset: dict[str, list[str]] = {}
    for record in resolutions:
        if record.resolution_status != "user_action_required":
            continue
        if reportable is not None and record.target_dataset not in reportable:
            continue
        blocked_by_dataset.setdefault(record.target_dataset, [])
        if record.required_dataset not in blocked_by_dataset[record.target_dataset]:
            blocked_by_dataset[record.target_dataset].append(record.required_dataset)
    return [
        {
            "dataset": dataset,
            "reason": "dependency_user_action_required",
            "blocked_by": ",".join(blocked_by),
        }
        for dataset, blocked_by in blocked_by_dataset.items()
    ]


def blocked_dependency_targets(
    *,
    target_datasets: list[str],
    candidate_datasets: list[str],
    runnable_datasets: list[str],
    direct_blocks: list[dict[str, str]],
    dependencies: dict[str, list[str]],
    satisfied_dependency_datasets: list[str],
) -> list[dict[str, str]]:
    """Return candidate datasets blocked because another dependency cannot run."""

    candidate_set = set(_normalize(candidate_datasets))
    runnable_set = set(_normalize(runnable_datasets))
    satisfied_set = set(_normalize(satisfied_dependency_datasets))
    directly_blocked = {block["dataset"] for block in direct_blocks}
    transitive_blocks: list[dict[str, str]] = []
    for dataset in _normalize(target_datasets):
        if dataset not in candidate_set or dataset in runnable_set or dataset in directly_blocked:
            continue
        blockers = [
            dependency
            for dependency in dependencies.get(dataset, [])
            if dependency not in runnable_set and dependency not in satisfied_set
        ]
        if blockers:
            transitive_blocks.append(
                {
                    "dataset": dataset,
                    "reason": "dependency_user_action_required",
                    "blocked_by": ",".join(blockers),
                }
            )
    return transitive_blocks


def _find_dependency_artifact(
    dataset: str,
    *,
    study_dir: str | Path | None,
    run_id: str | None,
) -> tuple[Path, str] | None:
    if not study_dir:
        return None
    root = Path(study_dir)
    candidates: list[tuple[Path, str]] = []
    lower = dataset.lower()
    upper = dataset.upper()
    for suffix in [".csv", ".sas7bdat"]:
        candidates.append((root / "reference_adam" / f"{lower}{suffix}", "reference_adam"))
        candidates.append((root / "reference_adam" / f"{upper}{suffix}", "reference_adam"))
        if run_id:
            candidates.append((root / "runs" / run_id / "outputs" / f"{lower}{suffix}", "run_output"))
            candidates.append((root / "runs" / run_id / "outputs" / f"{upper}{suffix}", "run_output"))
    for path, source in candidates:
        if path.exists() and path.is_file():
            return path, source
    return None


def _normalize(values: list[str]) -> list[str]:
    normalized: list[str] = []
    for value in values:
        dataset = str(value).strip().upper()
        if dataset and dataset not in normalized:
            normalized.append(dataset)
    return normalized
