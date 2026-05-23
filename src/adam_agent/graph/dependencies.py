"""MVP dataset dependency planning for study-level orchestration."""

from __future__ import annotations

from dataclasses import dataclass


FOUNDATION_DATASETS = ["ADSL"]
DEFAULT_ADSL_DEPENDENT_DATASETS = ["ADAE", "ADCM", "ADLB", "ADEX", "ADEG"]


@dataclass(frozen=True)
class DependencyDecision:
    """Evidence-bearing dependency decision for one dataset."""

    dataset: str
    dependencies: list[str]
    source: str
    confidence: float
    review_required: bool
    reason: str

    def as_dict(self) -> dict[str, object]:
        return {
            "dataset": self.dataset,
            "dependencies": self.dependencies,
            "source": self.source,
            "confidence": self.confidence,
            "review_required": self.review_required,
            "reason": self.reason,
        }


@dataclass(frozen=True)
class DatasetDependencyPlan:
    """Explicit study-level dataset dependency plan."""

    requested_datasets: list[str]
    target_datasets: list[str]
    auto_added_datasets: list[str]
    foundation_datasets: list[str]
    downstream_datasets: list[str]
    unsupported_datasets: list[str]
    dependencies: dict[str, list[str]]
    decisions: list[DependencyDecision]
    dependency_graph: dict[str, list[str]]
    evidence: str


def plan_dataset_dependencies(requested_datasets: list[str] | None) -> DatasetDependencyPlan:
    """Create the Phase 7.1 default dependency plan.

    This is an MVP orchestration default, not a production derivation rule.
    Later phases should let approved specs, legacy SAS, define.xml, and human
    decisions override this fallback.
    """

    requested = _normalize_datasets(requested_datasets or ["ADSL", "ADAE"])
    auto_added: list[str] = []
    targets = list(requested)

    if any(_depends_on_adsl(dataset) for dataset in requested) and "ADSL" not in targets:
        targets = ["ADSL", *targets]
        auto_added.append("ADSL")

    foundation = [dataset for dataset in targets if dataset in FOUNDATION_DATASETS]
    unsupported = [dataset for dataset in targets if _is_unsupported_target(dataset)]
    downstream = [dataset for dataset in targets if dataset not in foundation and dataset not in unsupported]
    decisions = [_dependency_decision(dataset, is_auto_added=dataset in auto_added) for dataset in targets]
    dependencies = {decision.dataset: decision.dependencies for decision in decisions}
    dependency_graph: dict[str, list[str]] = {}
    if "ADSL" in targets:
        dependency_graph["ADSL"] = [dataset for dataset in downstream if dependencies.get(dataset) == ["ADSL"]]

    return DatasetDependencyPlan(
        requested_datasets=requested,
        target_datasets=targets,
        auto_added_datasets=auto_added,
        foundation_datasets=foundation,
        downstream_datasets=downstream,
        unsupported_datasets=unsupported,
        dependencies=dependencies,
        decisions=decisions,
        dependency_graph=dependency_graph,
        evidence="phase7_mvp_fallback_adsl_foundation",
    )


def _normalize_datasets(datasets: list[str]) -> list[str]:
    normalized: list[str] = []
    for dataset in datasets:
        value = str(dataset).strip().upper()
        if value and value not in normalized:
            normalized.append(value)
    return normalized or ["ADSL"]


def _depends_on_adsl(dataset: str) -> bool:
    if dataset == "ADSL":
        return False
    if dataset in DEFAULT_ADSL_DEPENDENT_DATASETS:
        return True
    return dataset.startswith("AD")


def _is_unsupported_target(dataset: str) -> bool:
    return dataset != "ADSL" and not dataset.startswith("AD")


def _dependency_decision(dataset: str, *, is_auto_added: bool) -> DependencyDecision:
    if dataset == "ADSL":
        return DependencyDecision(
            dataset=dataset,
            dependencies=[],
            source="mvp_foundation_default",
            confidence=0.7 if is_auto_added else 0.8,
            review_required=False,
            reason="ADSL is the Phase 7.1 foundation dataset for study orchestration.",
        )
    if dataset in DEFAULT_ADSL_DEPENDENT_DATASETS:
        return DependencyDecision(
            dataset=dataset,
            dependencies=["ADSL"],
            source="mvp_common_adam_fallback",
            confidence=0.65,
            review_required=True,
            reason=f"{dataset} commonly needs subject-level ADSL variables, but this fallback must be reviewed against study evidence.",
        )
    if dataset.startswith("AD"):
        return DependencyDecision(
            dataset=dataset,
            dependencies=["ADSL"],
            source="mvp_unknown_adam_fallback",
            confidence=0.4,
            review_required=True,
            reason=f"{dataset} is an unknown ADaM dataset in this MVP; ADSL dependency is a conservative fallback that requires review.",
        )
    return DependencyDecision(
        dataset=dataset,
        dependencies=[],
        source="mvp_no_default_dependency",
        confidence=0.2,
        review_required=True,
        reason=f"{dataset} has no built-in dependency rule in Phase 7.1 and requires review.",
    )
