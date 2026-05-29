"""Build auditable LLM context packages for target ADaM generation."""

from __future__ import annotations

import csv
import io
import json
import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.llm import LLMExposureConfig
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.r_runner import LocalRRunner, RRunRequest
from adam_agent.tools.sdtm_reader import DatasetProfile, SDTMReader
from adam_agent.tools.study_inputs import StudyInputScanner


@dataclass
class LLMContextPackage:
    """Structured prompt payload before provider-specific rendering."""

    study_id: str
    run_id: str
    target_dataset: str
    target_spec: dict[str, Any] | None
    source_dataset_profiles: dict[str, dict[str, Any]]
    resolved_dependencies: dict[str, dict[str, Any]]
    runtime_contract: dict[str, Any]
    exposure: dict[str, Any]
    warnings: list[str] = field(default_factory=list)

    def as_dict(self) -> dict[str, Any]:
        return {
            "study_id": self.study_id,
            "run_id": self.run_id,
            "target_dataset": self.target_dataset,
            "target_spec": self.target_spec,
            "source_dataset_profiles": self.source_dataset_profiles,
            "resolved_dependencies": self.resolved_dependencies,
            "runtime_contract": self.runtime_contract,
            "exposure": self.exposure,
            "warnings": self.warnings,
        }


def build_target_llm_context(
    *,
    study_id: str,
    run_id: str,
    target_dataset: str,
    study_dir: str | Path,
    dependency_resolution: list[dict[str, Any]],
    exposure: LLMExposureConfig | None = None,
    source_datasets: list[str] | None = None,
    rscript_path: str | None = None,
) -> LLMContextPackage:
    """Build the target generation context without calling any LLM provider."""

    target = target_dataset.strip().upper()
    root = Path(study_dir)
    run_dir = root / "runs" / run_id
    exposure_config = exposure or LLMExposureConfig()
    sample_rows = exposure_config.sample_rows_per_dataset if exposure_config.mode != "metadata_only" else 0
    warnings: list[str] = []

    input_index = StudyInputScanner(root, study_id=study_id).scan()
    warnings.extend(input_index.warnings)
    warnings.extend(f"Invalid input file skipped: {item.path} ({item.reason})" for item in input_index.invalid_files)
    target_dependency_resolution = [
        record
        for record in dependency_resolution
        if str(record.get("target_dataset", "")).strip().upper() == target
    ]

    reader = SDTMReader()
    source_profiles: dict[str, dict[str, Any]] = {}
    allowed_sources = _normalize_optional_set(source_datasets)
    for dataset, artifact in sorted(input_index.input_sdtm.items()):
        if allowed_sources is not None and dataset not in allowed_sources:
            continue
        profile = _profile_artifact(
            reader,
            artifact,
            sample_rows=sample_rows,
            warnings=warnings,
            run_dir=run_dir,
            rscript_path=rscript_path,
        )
        if profile is not None:
            source_profiles[dataset] = profile

    resolved_dependencies = _resolved_dependency_profiles(
        reader=reader,
        dependency_resolution=target_dependency_resolution,
        sample_rows=sample_rows,
        warnings=warnings,
        run_dir=run_dir,
        rscript_path=rscript_path,
    )

    target_spec = _target_spec_payload(
        input_specs=input_index.input_spec,
        target_dataset=target,
        warnings=warnings,
    )

    return LLMContextPackage(
        study_id=study_id,
        run_id=run_id,
        target_dataset=target,
        target_spec=target_spec,
        source_dataset_profiles=source_profiles,
        resolved_dependencies=resolved_dependencies,
        runtime_contract=_runtime_contract(root, run_id, target),
        exposure=_exposure_payload(exposure_config, sample_rows),
        warnings=warnings,
    )


def write_llm_context_package(package: LLMContextPackage, study_dir: str | Path) -> ArtifactRef:
    """Write the context package as an audit artifact under runs/{run_id}/llm."""

    target_lower = package.target_dataset.lower()
    context_path = Path(study_dir) / "runs" / package.run_id / "llm" / f"{target_lower}_context.json"
    context_path.parent.mkdir(parents=True, exist_ok=True)
    context_path.write_text(json.dumps(package.as_dict(), indent=2, sort_keys=True), encoding="utf-8")
    return ArtifactRef(
        artifact_id=f"llm_context_{package.study_id.lower()}_{package.run_id}_{target_lower}",
        kind="llm_prompt",
        path=str(context_path.as_posix()),
        sha256=f"sha256:{sha256_file(context_path)}",
        dataset=package.target_dataset,
        format="json",
        role="audit",
        metadata={
            "context_package": True,
            "target_dataset": package.target_dataset,
            "source_dataset_count": len(package.source_dataset_profiles),
            "resolved_dependency_count": len(package.resolved_dependencies),
        },
    )


def _profile_artifact(
    reader: SDTMReader,
    artifact: ArtifactRef,
    *,
    sample_rows: int,
    warnings: list[str],
    run_dir: Path,
    rscript_path: str | None = None,
) -> dict[str, Any] | None:
    path = Path(artifact.path)
    if not path.exists() or not path.is_file():
        warnings.append(f"Artifact missing or not a file: {artifact.path}")
        return None
    try:
        profile = reader.profile(path, dataset=artifact.dataset, sample_rows=sample_rows)
    except Exception as exc:  # pragma: no cover - defensive boundary
        warnings.append(f"Could not profile artifact {artifact.path}: {exc}")
        return None
    if profile.format == "sas7bdat" and profile.status == "not_implemented_yet":
        profile = _profile_sas7bdat_with_r(profile, sample_rows=sample_rows, run_dir=run_dir, rscript_path=rscript_path)
    if profile.status != "ok":
        warnings.append(f"Profile not fully available for {artifact.dataset or path.stem.upper()}: {profile.message}")
    return _profile_payload(profile, artifact, run_dir=run_dir)


def _profile_payload(profile: DatasetProfile, artifact: ArtifactRef, *, run_dir: Path) -> dict[str, Any]:
    read_path = _run_relative_path(profile.path, run_dir=run_dir)
    return {
        "artifact_id": artifact.artifact_id,
        "path": profile.path,
        "read_path": read_path,
        "run_relative_path": read_path,
        "format": profile.format,
        "status": profile.status,
        "columns": profile.columns,
        "row_count": profile.row_count,
        "sample_rows": profile.sample_rows,
        "message": profile.message,
        "sha256": artifact.sha256,
    }


def _resolved_dependency_profiles(
    *,
    reader: SDTMReader,
    dependency_resolution: list[dict[str, Any]],
    sample_rows: int,
    warnings: list[str],
    run_dir: Path,
    rscript_path: str | None = None,
) -> dict[str, dict[str, Any]]:
    dependencies: dict[str, dict[str, Any]] = {}
    for record in dependency_resolution:
        if record.get("resolution_status") != "available":
            continue
        if record.get("artifact_source") == "reference_adam":
            warnings.append(
                f"Reference ADaM {record.get('required_dataset')} is available only for comparison/output-shape evidence; "
                "it is not exposed as a derivation runtime dependency."
            )
            continue
        dataset = str(record.get("required_dataset", "")).strip().upper()
        artifact_path = record.get("artifact_path")
        if not dataset or not artifact_path or dataset in dependencies:
            continue
        artifact = ArtifactRef(
            artifact_id=f"resolved_dependency_{dataset.lower()}",
            kind="reference_adam" if record.get("artifact_source") == "reference_adam" else "output_adam",
            path=str(artifact_path),
            sha256=_sha256_if_file(artifact_path),
            dataset=dataset,
            format=Path(str(artifact_path)).suffix.lower().lstrip("."),
            role="reference" if record.get("artifact_source") == "reference_adam" else "output",
            metadata={"artifact_source": record.get("artifact_source")},
        )
        profile = _profile_artifact(
            reader,
            artifact,
            sample_rows=sample_rows,
            warnings=warnings,
            run_dir=run_dir,
            rscript_path=rscript_path,
        )
        if profile is not None:
            profile["artifact_source"] = record.get("artifact_source")
            profile["required_by"] = record.get("target_dataset")
            dependencies[dataset] = profile
    return dependencies


def _target_spec_payload(
    *,
    input_specs: dict[str, ArtifactRef],
    target_dataset: str,
    warnings: list[str],
) -> dict[str, Any] | None:
    artifact = _select_target_spec(input_specs, target_dataset)
    if artifact is None:
        warnings.append(f"No input_spec artifact found for target dataset {target_dataset}.")
        return None
    path = Path(artifact.path)
    if not path.exists() or not path.is_file():
        warnings.append(f"Input spec artifact is missing: {artifact.path}")
        return None
    text = _read_text(path, warnings)
    parsed_json = None
    if path.suffix.lower() == ".json" and text:
        try:
            parsed_json = json.loads(text)
        except json.JSONDecodeError as exc:
            warnings.append(f"Input spec JSON could not be parsed for {target_dataset}: {exc}")
    return {
        "artifact_id": artifact.artifact_id,
        "path": artifact.path,
        "format": artifact.format,
        "sha256": artifact.sha256,
        "text": text,
        "json": parsed_json,
    }


def _select_target_spec(input_specs: dict[str, ArtifactRef], target_dataset: str) -> ArtifactRef | None:
    target = target_dataset.lower()
    preferred_keys = [
        target,
        f"ads_{target}_full",
        f"ads_{target}",
        f"{target}_spec",
        f"{target}_approved_spec",
        f"{target}_input_spec",
    ]
    for key in preferred_keys:
        if key in input_specs:
            return input_specs[key]
    for key, artifact in sorted(input_specs.items()):
        if target in key:
            return artifact
    return None


def _runtime_contract(study_dir: Path, run_id: str, target_dataset: str) -> dict[str, Any]:
    run_dir = study_dir / "runs" / run_id
    output_path = run_dir / "outputs" / f"{target_dataset.lower()}.csv"
    code_path = run_dir / "code" / f"build_{target_dataset.lower()}.R"
    return {
        "language": "R",
        "run_dir": str(run_dir.as_posix()),
        "working_directory": str(run_dir.as_posix()),
        "code_path": str(code_path.as_posix()),
        "output_path": str(output_path.as_posix()),
        "relative_output_path": f"runs/{run_id}/outputs/{target_dataset.lower()}.csv",
        "runtime_output_path": f"outputs/{target_dataset.lower()}.csv",
        "no_network": True,
        "write_only_to_run_dir": True,
        "input_path_policy": "Use profile read_path values exactly; do not search recursively or assume an inputs folder.",
        "csv_read_policy": (
            "Read CSV inputs with stringsAsFactors=FALSE, check.names=FALSE, "
            "colClasses='character' so identifiers such as USUBJID stay unchanged."
        ),
        "sas7bdat_read_policy": (
            "Read sas7bdat inputs with haven::read_sas(read_path), then convert to data.frame. "
            "Do not try to read sas7bdat with read.csv."
        ),
    }


def _profile_sas7bdat_with_r(
    profile: DatasetProfile,
    *,
    sample_rows: int,
    run_dir: Path,
    rscript_path: str | None = None,
) -> DatasetProfile:
    runner = LocalRRunner(rscript_path=rscript_path)
    if not runner.rscript_path:
        return DatasetProfile(
            dataset=profile.dataset,
            path=profile.path,
            format=profile.format,
            status="not_implemented_yet",
            message="sas7bdat metadata profiling requires Rscript with the R package 'haven'.",
        )
    profile_dir = run_dir / "llm" / ".sas7bdat_profile"
    r_code = f'''
if (!requireNamespace("haven", quietly = TRUE)) {{
  stop("Reading sas7bdat requires the R package 'haven'.")
}}
data <- haven::read_sas({_r_string(profile.path)})
sample <- utils::head(data, {max(0, sample_rows)})
sample[] <- lapply(sample, as.character)
cat("__COLUMNS__\\n")
cat(paste(names(data), collapse = "\\t"), "\\n", sep = "")
cat("__ROW_COUNT__\\n")
cat(nrow(data), "\\n", sep = "")
cat("__SAMPLE_CSV__\\n")
utils::write.csv(sample, stdout(), row.names = FALSE, na = "")
'''
    result = runner.run(
        RRunRequest(
            code=r_code,
            dataset=profile.dataset,
            run_id="llm_profile",
            working_dir=str(profile_dir),
            script_path=str(profile_dir / f"profile_{profile.dataset.lower()}.R"),
            timeout_seconds=60,
        )
    )
    if not result.success:
        return DatasetProfile(
            dataset=profile.dataset,
            path=profile.path,
            format=profile.format,
            status="error",
            message=result.stderr.strip() or result.stdout.strip() or "R sas7bdat profiling failed",
        )
    try:
        columns_text = _between_markers(result.stdout, "__COLUMNS__", "__ROW_COUNT__").strip()
        row_count_text = _between_markers(result.stdout, "__ROW_COUNT__", "__SAMPLE_CSV__").strip()
        sample_csv = result.stdout.split("__SAMPLE_CSV__", 1)[1].strip()
    except IndexError:
        return DatasetProfile(
            dataset=profile.dataset,
            path=profile.path,
            format=profile.format,
            status="error",
            message="R sas7bdat profiling returned unreadable output.",
        )
    columns = columns_text.split("\t") if columns_text else []
    try:
        row_count = int(row_count_text)
    except ValueError:
        row_count = None
    rows: list[dict[str, str]] = []
    if sample_csv:
        try:
            reader = csv.DictReader(io.StringIO(sample_csv))
            rows = [{column: _string_cell(row.get(column)) for column in columns} for row in reader]
        except csv.Error:
            rows = []
    return DatasetProfile(
        dataset=profile.dataset,
        path=profile.path,
        format=profile.format,
        status="ok",
        columns=columns,
        row_count=row_count,
        sample_rows=rows,
        message="Profiled sas7bdat through local R haven.",
    )


def _exposure_payload(exposure: LLMExposureConfig, sample_rows: int) -> dict[str, Any]:
    return {
        "mode": exposure.mode,
        "data_classification": exposure.data_classification,
        "external_api_allowed": exposure.external_api_allowed,
        "sample_rows_per_dataset": sample_rows,
        "include_reference_rows": exposure.include_reference_rows,
        "full_data_included": False,
    }


def _read_text(path: Path, warnings: list[str]) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        try:
            return path.read_text(encoding="latin-1")
        except OSError as exc:
            warnings.append(f"Could not read text artifact {path.name}: {exc}")
            return ""
    except OSError as exc:
        warnings.append(f"Could not read text artifact {path.name}: {exc}")
        return ""


def _normalize_optional_set(values: list[str] | None) -> set[str] | None:
    if values is None:
        return None
    return {str(value).strip().upper() for value in values if str(value).strip()}


def _sha256_if_file(path: object) -> str | None:
    file_path = Path(str(path))
    if file_path.exists() and file_path.is_file():
        return f"sha256:{sha256_file(file_path)}"
    return None


def _run_relative_path(path: str | Path, *, run_dir: Path) -> str:
    """Return a path that generated R can read from the run working directory."""

    source = Path(path)
    try:
        return Path(os.path.relpath(source.resolve(), run_dir.resolve())).as_posix()
    except ValueError:
        return source.resolve().as_posix()


def _between_markers(text: str, start: str, end: str) -> str:
    return text.split(start, 1)[1].split(end, 1)[0]


def _string_cell(value: Any) -> str:
    if value is None:
        return ""
    return str(value)


def _r_string(value: str) -> str:
    return '"' + value.replace("\\", "/").replace('"', '\\"') + '"'
