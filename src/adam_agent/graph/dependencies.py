"""MVP dataset dependency planning for study-level orchestration."""

from __future__ import annotations

import json
import re
import csv
from dataclasses import dataclass
from pathlib import Path
from typing import Any


FOUNDATION_DATASETS = ["ADSL"]
DEPENDENCY_SOURCE_PRIORITY = {
    "input_spec_dependency": 0,
    "legacy_sas_dependency": 1,
    "define_xml_dependency": 2,
}
_NON_DATASET_AD_TOKENS = {"ADDATA", "ADAM", "ADAMS", "ADAMDATA", "ADSLIB"}


@dataclass(frozen=True)
class DependencyEvidence:
    """File-backed evidence that one ADaM dataset depends on another."""

    dataset: str
    dependency: str
    source: str
    evidence_id: str
    confidence: float
    detail: str

    def as_dict(self) -> dict[str, object]:
        return {
            "dataset": self.dataset,
            "dependency": self.dependency,
            "source": self.source,
            "evidence_id": self.evidence_id,
            "confidence": self.confidence,
            "detail": self.detail,
        }


@dataclass(frozen=True)
class DependencyDecision:
    """Evidence-bearing dependency decision for one dataset."""

    dataset: str
    dependencies: list[str]
    source: str
    confidence: float
    review_required: bool
    reason: str
    evidence_ids: list[str]

    def as_dict(self) -> dict[str, object]:
        return {
            "dataset": self.dataset,
            "dependencies": self.dependencies,
            "source": self.source,
            "confidence": self.confidence,
            "review_required": self.review_required,
            "reason": self.reason,
            "evidence_ids": self.evidence_ids,
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
    execution_batches: list[list[str]]
    evidence: str
    evidence_records: list[DependencyEvidence]
    planning_warnings: list[str]


def plan_dataset_dependencies(requested_datasets: list[str] | None, *, study_dir: str | Path | None = None) -> DatasetDependencyPlan:
    """Create the Phase 7 dataset dependency plan.

    User-provided spec/SAS/define evidence wins when it clearly names an ADaM
    dependency. The MVP fallback remains a last-resort ordering default, not a
    production derivation rule.
    """

    requested = _normalize_datasets(requested_datasets or ["ADSL", "ADAE"])
    evidence_records, planning_warnings = collect_dependency_evidence(study_dir)
    evidence_by_dataset = _evidence_by_dataset(evidence_records)
    input_spec_present = _input_spec_present(study_dir)
    auto_added: list[str] = []
    targets = list(requested)

    changed = True
    while changed:
        changed = False
        for dataset in list(targets):
            if _is_unsupported_target(dataset):
                continue
            for dependency in _dependencies_for_dataset(dataset, evidence_by_dataset, study_dir=study_dir):
                if dependency not in targets:
                    targets.append(dependency)
                    auto_added.append(dependency)
                    changed = True

    unsupported = [dataset for dataset in targets if _is_unsupported_target(dataset)]
    supported_targets = [dataset for dataset in targets if dataset not in unsupported]
    ordered_supported, execution_batches, cycle_warnings = _topological_order(supported_targets, evidence_by_dataset, study_dir=study_dir)
    planning_warnings.extend(cycle_warnings)
    ordered_targets = ordered_supported + [dataset for dataset in targets if dataset in unsupported]
    if input_spec_present:
        planning_warnings.extend(_input_spec_gap_warnings(ordered_supported, evidence_by_dataset, study_dir=study_dir))

    decisions = [
        _dependency_decision(dataset, evidence_by_dataset, is_auto_added=dataset in auto_added, study_dir=study_dir)
        for dataset in ordered_targets
    ]
    dependencies = {decision.dataset: decision.dependencies for decision in decisions}
    foundation = [dataset for dataset in ordered_supported if not dependencies.get(dataset)]
    downstream = [dataset for dataset in ordered_supported if dependencies.get(dataset)]
    unsupported = [dataset for dataset in targets if _is_unsupported_target(dataset)]
    dependency_graph = _reverse_dependency_graph(ordered_supported, dependencies)
    evidence_summary = _evidence_summary(decisions)

    return DatasetDependencyPlan(
        requested_datasets=requested,
        target_datasets=ordered_targets,
        auto_added_datasets=auto_added,
        foundation_datasets=foundation,
        downstream_datasets=downstream,
        unsupported_datasets=unsupported,
        dependencies=dependencies,
        decisions=decisions,
        dependency_graph=dependency_graph,
        execution_batches=execution_batches,
        evidence=evidence_summary,
        evidence_records=evidence_records,
        planning_warnings=planning_warnings,
    )


def collect_dependency_evidence(study_dir: str | Path | None) -> tuple[list[DependencyEvidence], list[str]]:
    """Collect conservative dependency evidence from user-provided study files."""

    if not study_dir:
        return [], []

    root = Path(study_dir)
    if not root.exists():
        return [], [f"Study directory does not exist for dependency evidence scan: {root}"]

    evidence: list[DependencyEvidence] = []
    warnings: list[str] = []
    input_spec_dir = root / "input_spec"
    if _folder_has_files(input_spec_dir):
        evidence.extend(_scan_input_spec_dependencies(input_spec_dir, warnings))
        secondary_warnings: list[str] = []
        secondary_evidence = _collect_secondary_dependency_evidence(root, secondary_warnings)
        warnings.extend(_dependency_conflict_warnings(evidence, secondary_evidence))
        if secondary_warnings:
            warnings.extend(f"Secondary dependency validation warning: {warning}" for warning in secondary_warnings)
        return _dedupe_evidence(evidence), warnings

    evidence.extend(_collect_secondary_dependency_evidence(root, warnings))
    return _dedupe_evidence(evidence), warnings


def _input_spec_present(study_dir: str | Path | None) -> bool:
    if not study_dir:
        return False
    return _folder_has_files(Path(study_dir) / "input_spec")


def _target_input_spec_present(study_dir: str | Path | None, dataset: str) -> bool:
    """Return whether a user supplied an input spec that appears to own this target."""

    if not study_dir:
        return False
    folder = Path(study_dir) / "input_spec"
    if not folder.exists():
        return False
    target = dataset.strip().upper()
    for path in sorted(item for item in folder.iterdir() if item.is_file()):
        if _dataset_from_path(path) == target:
            return True
        if path.suffix.lower() == ".csv" and _csv_spec_mentions_dataset(path, target):
            return True
        if path.suffix.lower() == ".json" and _json_spec_dataset(path) == target:
            return True
    return False


def _normalize_datasets(datasets: list[str]) -> list[str]:
    normalized: list[str] = []
    for dataset in datasets:
        value = str(dataset).strip().upper()
        if value and value not in normalized:
            normalized.append(value)
    return normalized or ["ADSL"]


def _dependencies_for_dataset(
    dataset: str,
    evidence_by_dataset: dict[str, list[DependencyEvidence]],
    *,
    study_dir: str | Path | None = None,
) -> list[str]:
    evidence_dependencies = _evidence_dependencies_for_dataset(dataset, evidence_by_dataset)
    if evidence_dependencies:
        return evidence_dependencies
    return _fallback_dependencies_for_dataset(dataset, study_dir=study_dir)


def _fallback_dependencies_for_dataset(dataset: str, *, study_dir: str | Path | None = None) -> list[str]:
    return []


def _is_unsupported_target(dataset: str) -> bool:
    return dataset != "ADSL" and not dataset.startswith("AD")


def _dependency_decision(
    dataset: str,
    evidence_by_dataset: dict[str, list[DependencyEvidence]],
    *,
    is_auto_added: bool,
    study_dir: str | Path | None = None,
) -> DependencyDecision:
    evidence = evidence_by_dataset.get(dataset, [])
    if evidence:
        dependencies = _evidence_dependencies_for_dataset(dataset, evidence_by_dataset)
        primary = min(evidence, key=lambda item: DEPENDENCY_SOURCE_PRIORITY.get(item.source, 99))
        is_input_spec = primary.source == "input_spec_dependency"
        return DependencyDecision(
            dataset=dataset,
            dependencies=dependencies,
            source=primary.source,
            confidence=max(item.confidence for item in evidence),
            review_required=not is_input_spec,
            reason=(
                f"{dataset} dependency follows the user-provided input_spec. Secondary evidence only creates a warning when it conflicts."
                if is_input_spec
                else f"{dataset} dependency was inferred from user-provided {primary.source.replace('_dependency', '')} evidence and should be reviewed against the approved study contract."
            ),
            evidence_ids=[item.evidence_id for item in evidence if item.dependency in dependencies],
        )
    if dataset == "ADSL":
        return DependencyDecision(
            dataset=dataset,
            dependencies=[],
            source="ad_target_no_adam_dependency",
            confidence=0.7 if is_auto_added else 0.8,
            review_required=False,
            reason=(
                "ADSL has no upstream ADaM dependency in the current plan, so it can run in the first execution batch "
                "through the same unified ADaM spec/code/review/execute flow as other targets."
            ),
            evidence_ids=[],
        )
    if _target_input_spec_present(study_dir, dataset):
        return DependencyDecision(
            dataset=dataset,
            dependencies=[],
            source="input_spec_no_adam_dependency",
            confidence=0.85,
            review_required=False,
            reason=(
                f"{dataset} has a user-provided input_spec and no ADaM dependency was found in dependency-relevant "
                "spec fields, so the system will not impose an ADSL fallback."
            ),
            evidence_ids=[],
        )
    if dataset.startswith("AD"):
        return DependencyDecision(
            dataset=dataset,
            dependencies=[],
            source="no_dependency_evidence",
            confidence=0.35,
            review_required=True,
            reason=(
                f"No user spec, legacy SAS, or define evidence identified an ADaM dependency for {dataset}. "
                "The system will not invent an ADSL dependency; draft spec/code review must verify whether upstream ADaM inputs are required."
            ),
            evidence_ids=[],
        )
    return DependencyDecision(
        dataset=dataset,
        dependencies=[],
        source="mvp_no_default_dependency",
        confidence=0.2,
        review_required=True,
        reason=f"{dataset} has no built-in dependency rule in Phase 7.1 and requires review.",
        evidence_ids=[],
    )


def _evidence_by_dataset(evidence_records: list[DependencyEvidence]) -> dict[str, list[DependencyEvidence]]:
    grouped: dict[str, list[DependencyEvidence]] = {}
    for record in evidence_records:
        grouped.setdefault(record.dataset, []).append(record)
    return grouped


def _input_spec_gap_warnings(
    supported_targets: list[str],
    evidence_by_dataset: dict[str, list[DependencyEvidence]],
    *,
    study_dir: str | Path | None = None,
) -> list[str]:
    warnings: list[str] = []
    for dataset in supported_targets:
        if dataset == "ADSL" or dataset in evidence_by_dataset or _target_input_spec_present(study_dir, dataset):
            continue
        warnings.append(
            f"Input spec is present, but no dependency evidence was extracted for {dataset}; "
            "not imposing a default ADSL dependency. Draft spec/code review must verify whether upstream ADaM inputs are required."
        )
    return warnings


def _collect_secondary_dependency_evidence(root: Path, warnings: list[str]) -> list[DependencyEvidence]:
    evidence: list[DependencyEvidence] = []
    evidence.extend(_scan_text_dependency_folder(root / "legacy_code", "legacy_sas_dependency", 0.8, warnings))
    evidence.extend(_scan_text_dependency_folder(root / "input_define", "define_xml_dependency", 0.75, warnings))
    return evidence


def _dependency_conflict_warnings(
    spec_evidence: list[DependencyEvidence],
    secondary_evidence: list[DependencyEvidence],
) -> list[str]:
    spec_dependencies = {
        dataset: set(_evidence_dependencies_for_dataset(dataset, _evidence_by_dataset(spec_evidence)))
        for dataset in {record.dataset for record in spec_evidence}
    }
    warnings: list[str] = []
    for record in secondary_evidence:
        if record.dataset not in spec_dependencies:
            continue
        if record.dependency not in spec_dependencies[record.dataset]:
            warnings.append(
                f"Dependency conflict: input_spec for {record.dataset} does not include "
                f"{record.dependency}, but {record.source} evidence {record.evidence_id} references it."
            )
    return warnings


def _evidence_dependencies_for_dataset(
    dataset: str,
    evidence_by_dataset: dict[str, list[DependencyEvidence]],
) -> list[str]:
    dependencies: list[str] = []
    for record in sorted(
        evidence_by_dataset.get(dataset, []),
        key=lambda item: (item.dependency not in FOUNDATION_DATASETS, DEPENDENCY_SOURCE_PRIORITY.get(item.source, 99), item.dependency),
    ):
        if record.dependency != dataset and record.dependency not in dependencies:
            dependencies.append(record.dependency)
    return dependencies


def _topological_order(
    supported_targets: list[str],
    evidence_by_dataset: dict[str, list[DependencyEvidence]],
    *,
    study_dir: str | Path | None = None,
) -> tuple[list[str], list[list[str]], list[str]]:
    dependencies = {
        dataset: _dependencies_for_dataset(dataset, evidence_by_dataset, study_dir=study_dir)
        for dataset in supported_targets
    }
    remaining = list(supported_targets)
    ordered: list[str] = []
    batches: list[list[str]] = []
    warnings: list[str] = []

    while remaining:
        ready = [
            dataset
            for dataset in remaining
            if all(dependency not in remaining for dependency in dependencies.get(dataset, []))
        ]
        if not ready:
            warnings.append(f"Dependency cycle or unresolved ADaM dependency detected among: {', '.join(remaining)}")
            ready = list(remaining)
        batches.append(ready)
        ordered.extend(ready)
        remaining = [dataset for dataset in remaining if dataset not in ready]

    return ordered, batches, warnings


def _reverse_dependency_graph(targets: list[str], dependencies: dict[str, list[str]]) -> dict[str, list[str]]:
    graph: dict[str, list[str]] = {}
    target_set = set(targets)
    for dataset in targets:
        for dependency in dependencies.get(dataset, []):
            if dependency in target_set:
                graph.setdefault(dependency, []).append(dataset)
    return graph


def _evidence_summary(decisions: list[DependencyDecision]) -> str:
    sources = {decision.source for decision in decisions}
    if any(source in sources for source in DEPENDENCY_SOURCE_PRIORITY):
        return "user_evidence"
    if "no_dependency_evidence" in sources:
        return "no_dependency_evidence"
    return "no_adam_dependency"


def _scan_input_spec_dependencies(folder: Path, warnings: list[str]) -> list[DependencyEvidence]:
    evidence: list[DependencyEvidence] = []
    if not folder.exists():
        return evidence
    for path in sorted(item for item in folder.iterdir() if item.is_file()):
        if path.suffix.lower() == ".json":
            evidence.extend(_scan_json_spec_dependencies(path, warnings))
            continue
        if path.suffix.lower() == ".csv":
            evidence.extend(_scan_csv_spec_dependencies(path, warnings))
            continue
        dataset = _dataset_from_path(path)
        if not dataset:
            continue
        evidence.extend(
            _evidence_from_text(
                dataset=dataset,
                text=_read_text(path, warnings),
                source="input_spec_dependency",
                confidence=0.85,
                evidence_prefix=f"input_spec:{path.name}",
                detail_prefix=f"ADaM dependency token found in input spec file {path.name}",
            )
        )
    return evidence


def _scan_csv_spec_dependencies(path: Path, warnings: list[str]) -> list[DependencyEvidence]:
    """Scan structured CSV specs without treating labels as dependency evidence."""

    dataset_from_name = _dataset_from_path(path)
    try:
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            rows = list(csv.DictReader(handle))
    except UnicodeDecodeError:
        try:
            with path.open("r", encoding="latin-1", newline="") as handle:
                rows = list(csv.DictReader(handle))
        except (OSError, csv.Error) as exc:
            warnings.append(f"Could not parse input spec CSV {path.name}: {exc}")
            return _scan_unstructured_spec_file(path, warnings, dataset_from_name, confidence=0.8)
    except (OSError, csv.Error) as exc:
        warnings.append(f"Could not parse input spec CSV {path.name}: {exc}")
        return _scan_unstructured_spec_file(path, warnings, dataset_from_name, confidence=0.8)

    if not rows:
        return []

    texts_by_dataset: dict[str, list[str]] = {}
    for row in rows:
        row_dataset = _normalize_dataset_token(_first_present(row, ["dataset", "domain", "adataset"])) or dataset_from_name
        if not row_dataset:
            continue
        candidate_texts: list[str] = []
        for key, value in row.items():
            if _is_dependency_evidence_column(key):
                candidate_texts.extend(_flatten_strings(value))
        if candidate_texts:
            texts_by_dataset.setdefault(row_dataset, []).extend(candidate_texts)

    evidence: list[DependencyEvidence] = []
    for dataset, texts in texts_by_dataset.items():
        evidence.extend(
            _evidence_from_text(
                dataset=dataset,
                text="\n".join(texts),
                source="input_spec_dependency",
                confidence=0.85,
                evidence_prefix=f"input_spec:{path.name}",
                detail_prefix=f"ADaM dependency token found in structured input spec file {path.name}",
            )
        )
    return evidence


def _scan_unstructured_spec_file(
    path: Path,
    warnings: list[str],
    dataset: str | None,
    *,
    confidence: float,
) -> list[DependencyEvidence]:
    if not dataset:
        return []
    return _evidence_from_text(
        dataset=dataset,
        text=_read_text(path, warnings),
        source="input_spec_dependency",
        confidence=confidence,
        evidence_prefix=f"input_spec:{path.name}",
        detail_prefix=f"ADaM dependency token found in unstructured input spec file {path.name}",
    )


def _scan_json_spec_dependencies(path: Path, warnings: list[str]) -> list[DependencyEvidence]:
    text = _read_text(path, warnings)
    if not text:
        return []
    try:
        payload = json.loads(text)
    except json.JSONDecodeError as exc:
        warnings.append(f"Could not parse input spec JSON {path.name}: {exc}")
        dataset = _dataset_from_path(path)
        if not dataset:
            return []
        return _evidence_from_text(
            dataset=dataset,
            text=text,
            source="input_spec_dependency",
            confidence=0.8,
            evidence_prefix=f"input_spec:{path.name}",
            detail_prefix=f"ADaM dependency token found in unparsed input spec file {path.name}",
        )

    dataset = _normalize_dataset_token(_string_value(payload.get("dataset"))) if isinstance(payload, dict) else None
    dataset = dataset or _dataset_from_path(path)
    if not dataset:
        return []

    candidate_texts: list[str] = []
    if isinstance(payload, dict):
        for key in ["source_domains", "source_datasets", "source_variables", "derivation", "dependencies"]:
            candidate_texts.extend(_flatten_strings(payload.get(key)))
        variables = payload.get("variables", [])
        if isinstance(variables, list):
            for variable in variables:
                if isinstance(variable, dict):
                    for key in ["source_domains", "source_datasets", "source_variables", "derivation", "dependencies"]:
                        candidate_texts.extend(_flatten_strings(variable.get(key)))
    if not candidate_texts:
        candidate_texts = [text]

    return _evidence_from_text(
        dataset=dataset,
        text="\n".join(candidate_texts),
        source="input_spec_dependency",
        confidence=0.9 if isinstance(payload, dict) else 0.8,
        evidence_prefix=f"input_spec:{path.name}",
        detail_prefix=f"ADaM dependency token found in input spec file {path.name}",
    )


def _scan_text_dependency_folder(
    folder: Path,
    source: str,
    confidence: float,
    warnings: list[str],
) -> list[DependencyEvidence]:
    evidence: list[DependencyEvidence] = []
    if not folder.exists():
        return evidence
    for path in sorted(item for item in folder.iterdir() if item.is_file()):
        dataset = _dataset_from_path(path)
        text = _read_text(path, warnings)
        if source == "define_xml_dependency" and not dataset:
            evidence.extend(
                _define_xml_like_evidence(
                    text=text,
                    confidence=confidence,
                    evidence_prefix=f"{source}:{path.name}",
                    detail_prefix=f"ADaM dependency token found in {path.name}",
                )
            )
            continue
        if not dataset:
            continue
        if source == "legacy_sas_dependency":
            text = "\n".join(line for line in text.splitlines() if _looks_like_sas_dependency_line(line))
        evidence.extend(
            _evidence_from_text(
                dataset=dataset,
                text=text,
                source=source,
                confidence=confidence,
                evidence_prefix=f"{source}:{path.name}",
                detail_prefix=f"ADaM dependency token found in {path.name}",
            )
        )
    return evidence


def _define_xml_like_evidence(
    *,
    text: str,
    confidence: float,
    evidence_prefix: str,
    detail_prefix: str,
) -> list[DependencyEvidence]:
    """Extract simple dataset-to-ADaM references from define.xml-like text."""

    records: list[DependencyEvidence] = []
    block_pattern = re.compile(
        r"<ItemGroupDef\b(?P<attrs>[^>]*)>(?P<body>.*?)</ItemGroupDef>",
        re.IGNORECASE | re.DOTALL,
    )
    for match in block_pattern.finditer(text):
        dataset = _dataset_from_string(match.group("attrs"))
        if not dataset:
            continue
        for dependency in _adam_tokens(match.group("body")):
            if dependency == dataset:
                continue
            records.append(
                DependencyEvidence(
                    dataset=dataset,
                    dependency=dependency,
                    source="define_xml_dependency",
                    evidence_id=f"{evidence_prefix}:{dataset}->{dependency}",
                    confidence=confidence,
                    detail=f"{detail_prefix}: {dataset} references {dependency}",
                )
            )
    return records


def _evidence_from_text(
    *,
    dataset: str,
    text: str,
    source: str,
    confidence: float,
    evidence_prefix: str,
    detail_prefix: str,
) -> list[DependencyEvidence]:
    dependencies = _adam_tokens(text)
    records: list[DependencyEvidence] = []
    for dependency in dependencies:
        if dependency == dataset:
            continue
        records.append(
            DependencyEvidence(
                dataset=dataset,
                dependency=dependency,
                source=source,
                evidence_id=f"{evidence_prefix}->{dependency}",
                confidence=confidence,
                detail=f"{detail_prefix}: {dependency}",
            )
        )
    return records


def _dedupe_evidence(records: list[DependencyEvidence]) -> list[DependencyEvidence]:
    deduped: dict[tuple[str, str, str, str], DependencyEvidence] = {}
    for record in records:
        key = (record.dataset, record.dependency, record.source, record.evidence_id)
        deduped[key] = record
    return list(deduped.values())


def _adam_tokens(text: str) -> list[str]:
    tokens: list[str] = []
    for match in re.finditer(r"\bAD[A-Z0-9_]{1,}\b", text.upper()):
        if match.end() < len(text) and text[match.end()] == ".":
            continue
        token = match.group(0).split(".")[-1]
        if token in _NON_DATASET_AD_TOKENS:
            continue
        if token not in tokens:
            tokens.append(token)
    return tokens


def _dataset_from_path(path: Path) -> str | None:
    return _dataset_from_string(path.stem)


def _dataset_from_string(value: str) -> str | None:
    ads_spec_match = re.search(r"\bADS[_\-\s]+(?P<dataset>AD[A-Z0-9]{1,})(?:[_\-\s]+FULL)?\b", value.upper())
    if ads_spec_match:
        token = _normalize_dataset_token(ads_spec_match.group("dataset"))
        if token:
            return token
    for part in re.split(r"[^A-Za-z0-9]+", value.upper()):
        token = _normalize_dataset_token(part)
        if token:
            return token
    return None


def _normalize_dataset_token(value: str | None) -> str | None:
    if not value:
        return None
    token = value.strip().upper()
    if token in _NON_DATASET_AD_TOKENS:
        return None
    return token if token.startswith("AD") and len(token) > 2 else None


def _string_value(value: Any) -> str | None:
    return value if isinstance(value, str) else None


def _flatten_strings(value: Any) -> list[str]:
    if value is None:
        return []
    if isinstance(value, str):
        return [value]
    if isinstance(value, (int, float, bool)):
        return [str(value)]
    if isinstance(value, dict):
        strings: list[str] = []
        for item in value.values():
            strings.extend(_flatten_strings(item))
        return strings
    if isinstance(value, list):
        strings = []
        for item in value:
            strings.extend(_flatten_strings(item))
        return strings
    return []


def _first_present(row: dict[str, Any], keys: list[str]) -> str | None:
    normalized = {str(key).strip().lower(): value for key, value in row.items()}
    for key in keys:
        value = normalized.get(key.lower())
        if isinstance(value, str) and value.strip():
            return value.strip()
    return None


def _is_dependency_evidence_column(key: str | None) -> bool:
    if key is None:
        return False
    normalized = re.sub(r"[^a-z0-9]+", "_", key.strip().lower()).strip("_")
    if not normalized:
        return False
    include_fragments = [
        "source",
        "derivation",
        "dependency",
        "dependencies",
        "origin",
        "method",
        "algorithm",
        "logic",
        "comment",
    ]
    exclude_exact = {
        "label",
        "variable_label",
        "dataset_label",
        "description",
        "type",
        "format",
        "length",
    }
    return normalized not in exclude_exact and any(fragment in normalized for fragment in include_fragments)


def _looks_like_sas_dependency_line(line: str) -> bool:
    lower = f" {line.lower()} "
    return bool(re.search(r"\b(merge|set|join|from|update|modify)\b", lower))


def _read_text(path: Path, warnings: list[str]) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        try:
            return path.read_text(encoding="latin-1")
        except OSError as exc:
            warnings.append(f"Could not read dependency evidence file {path.name}: {exc}")
            return ""
    except OSError as exc:
        warnings.append(f"Could not read dependency evidence file {path.name}: {exc}")
        return ""


def _folder_has_files(folder: Path) -> bool:
    return folder.exists() and any(path.is_file() for path in folder.iterdir())


def _csv_spec_mentions_dataset(path: Path, dataset: str) -> bool:
    try:
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            rows = list(csv.DictReader(handle))
    except UnicodeDecodeError:
        try:
            with path.open("r", encoding="latin-1", newline="") as handle:
                rows = list(csv.DictReader(handle))
        except (OSError, csv.Error):
            return False
    except (OSError, csv.Error):
        return False
    for row in rows:
        row_dataset = _normalize_dataset_token(_first_present(row, ["dataset", "domain", "adataset"]))
        if row_dataset == dataset:
            return True
    return False


def _json_spec_dataset(path: Path) -> str | None:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except UnicodeDecodeError:
        try:
            payload = json.loads(path.read_text(encoding="latin-1"))
        except (OSError, json.JSONDecodeError):
            return None
    except (OSError, json.JSONDecodeError):
        return None
    if not isinstance(payload, dict):
        return None
    return _normalize_dataset_token(_string_value(payload.get("dataset")))
