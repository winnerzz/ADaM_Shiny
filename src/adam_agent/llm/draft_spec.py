"""Draft ADaM specs from auxiliary evidence when no input spec is supplied."""

from __future__ import annotations

import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from pydantic import ValidationError

from adam_agent.llm.clients import LLMClient, LLMRequest
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.llm import LLMCallRecord, LLMExposureConfig
from adam_agent.schemas.specs import SpecDocument, SpecVariable
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.sdtm_reader import SDTMReader
from adam_agent.tools.study_inputs import StudyInputScanner


MAX_TEXT_CHARS = 9000
MAX_EVIDENCE_FILES = 4
SPEC_TYPES = {"character", "numeric", "date", "datetime", "time", "boolean", "unknown"}


class DraftSpecGenerationError(ValueError):
    """Raised when an LLM draft-spec response cannot be converted to a spec."""


@dataclass(frozen=True)
class DraftSpecGenerationResult:
    """Artifacts and payload produced by the draft-spec generation step."""

    spec: SpecDocument
    prompt_artifact: ArtifactRef
    response_artifact: ArtifactRef
    spec_artifact: ArtifactRef
    target_spec_payload: dict[str, Any]
    call_record: LLMCallRecord | None
    warnings: list[str]


def generate_draft_spec_from_evidence(
    *,
    study_id: str,
    run_id: str,
    target_dataset: str,
    study_dir: str | Path,
    context_dict: dict[str, Any],
    llm_client: LLMClient,
    provider: str,
    model: str,
    exposure: LLMExposureConfig,
    max_tokens: int | None = None,
) -> DraftSpecGenerationResult:
    """Generate and persist a review-required draft spec from available evidence."""

    target = target_dataset.strip().upper()
    root = Path(study_dir)
    evidence_text, evidence_warnings = _auxiliary_evidence_text(root, study_id=study_id, target=target, run_id=run_id)
    prompt = _draft_spec_prompt(target=target, context=context_dict, evidence_text=evidence_text)
    prompt_artifact = _write_text_artifact(
        root=root,
        run_id=run_id,
        target=target,
        name=f"{target.lower()}_draft_spec_prompt.txt",
        text=prompt,
        artifact_id=f"llm_prompt_draft_spec_{study_id.lower()}_{run_id}_{target.lower()}",
        kind="llm_prompt",
        role="audit",
        folder="llm",
    )
    response = llm_client.generate(
        LLMRequest(
            prompt=prompt,
            system_prompt=_draft_spec_system_prompt(target),
            provider=provider,
            model=model,
            exposure=exposure,
            node="draft_spec_from_evidence",
            call_id=f"llm_{run_id}_{target.lower()}_draft_spec",
            max_tokens=max_tokens,
            datasets_included=_datasets_included(context_dict),
            variables_included=_variables_included(context_dict),
            sample_row_counts=_sample_row_counts(context_dict),
            subject_level_data_included=bool(_sample_row_counts(context_dict)),
            prompt_artifact_id=prompt_artifact.artifact_id,
            response_artifact_id=f"llm_response_draft_spec_{study_id.lower()}_{run_id}_{target.lower()}",
            redaction_policy="phase8_draft_spec_from_auxiliary_evidence",
        )
    )
    response_artifact = _write_text_artifact(
        root=root,
        run_id=run_id,
        target=target,
        name=f"{target.lower()}_draft_spec_response.json",
        text=response.response_text,
        artifact_id=f"llm_response_draft_spec_{study_id.lower()}_{run_id}_{target.lower()}",
        kind="llm_response",
        role="audit",
        folder="llm",
    )
    spec = parse_draft_spec_response(response.response_text, expected_dataset=target)
    spec_text = json.dumps(spec.model_dump(mode="json"), indent=2, sort_keys=True)
    spec_artifact = _write_text_artifact(
        root=root,
        run_id=run_id,
        target=target,
        name=f"{target.lower()}_draft_spec.json",
        text=spec_text,
        artifact_id=f"draft_spec_{study_id.lower()}_{run_id}_{target.lower()}",
        kind="draft_spec",
        role="intermediate",
        folder="specs",
        metadata={
            "source": "llm_from_auxiliary_evidence",
            "review_required": True,
            "prompt_artifact_id": prompt_artifact.artifact_id,
            "response_artifact_id": response_artifact.artifact_id,
        },
    )
    return DraftSpecGenerationResult(
        spec=spec,
        prompt_artifact=prompt_artifact,
        response_artifact=response_artifact,
        spec_artifact=spec_artifact,
        target_spec_payload={
            "artifact_id": spec_artifact.artifact_id,
            "path": spec_artifact.path,
            "format": "json",
            "sha256": spec_artifact.sha256,
            "text": spec_text,
            "json": spec.model_dump(mode="json"),
            "draft": True,
            "source": "llm_from_auxiliary_evidence",
        },
        call_record=response.call_record,
        warnings=[
            f"No approved input_spec was supplied for {target}; generated a review-required draft spec from auxiliary evidence.",
            *evidence_warnings,
        ],
    )


def parse_draft_spec_response(response_text: str, *, expected_dataset: str) -> SpecDocument:
    """Parse the strict draft-spec JSON contract returned by an LLM."""

    payload = _json_payload(response_text)
    if not isinstance(payload, dict):
        raise DraftSpecGenerationError("Draft spec response must be a JSON object.")
    dataset = str(payload.get("dataset") or "").strip().upper()
    expected = expected_dataset.strip().upper()
    if dataset != expected:
        raise DraftSpecGenerationError(f"Draft spec dataset {dataset or '<missing>'} does not match expected {expected}.")
    variables_payload = payload.get("variables")
    if not isinstance(variables_payload, list) or not variables_payload:
        raise DraftSpecGenerationError("Draft spec response requires a non-empty variables array.")
    variables = [_spec_variable(item, dataset=expected, index=index) for index, item in enumerate(variables_payload, start=1)]
    try:
        return SpecDocument(dataset=expected, status="draft", variables=variables)
    except ValidationError as exc:
        raise DraftSpecGenerationError(f"Draft spec response failed schema validation: {exc}") from exc


def default_mock_draft_spec_response(target_dataset: str, context_dict: dict[str, Any]) -> str:
    """Return a deterministic draft spec for offline/mock mode."""

    target = target_dataset.strip().upper()
    columns = _candidate_columns(context_dict)
    variables = []
    for column in columns[:20]:
        variables.append(
            {
                "variable": column,
                "label": column,
                "type": "character",
                "source_domains": _source_domains_for_column(context_dict, column),
                "source_variables": [column],
                "derivation": "Draft candidate from available input profiles. Requires human review before production use.",
                "confidence": 0.35,
                "review_required": True,
                "review_reasons": ["No approved input_spec was supplied; this is a mock draft from auxiliary evidence."],
                "risk_level": "high",
                "assumptions": ["Mock mode does not infer production derivation logic."],
            }
        )
    if not variables:
        variables.append(
            {
                "variable": "USUBJID",
                "label": "Unique Subject Identifier",
                "type": "character",
                "source_domains": [],
                "source_variables": ["USUBJID"],
                "derivation": "Draft candidate identifier. Requires human review.",
                "confidence": 0.25,
                "review_required": True,
                "review_reasons": ["No approved input_spec was supplied and source columns were unavailable."],
                "risk_level": "high",
            }
        )
    return json.dumps({"dataset": target, "variables": variables}, separators=(",", ":"))


def _draft_spec_system_prompt(target: str) -> str:
    return (
        "Return only one valid JSON object. Do not use markdown. "
        f"The dataset field must be {target}. "
        "Top-level keys: dataset, variables. variables is an array. "
        "Each variable should include variable, label, type, source_domains, source_variables, derivation, "
        "confidence, review_required, review_reasons, risk_level, assumptions. "
        "This is a draft spec, not an approved spec; mark uncertain variables review_required=true."
    )


def _draft_spec_prompt(*, target: str, context: dict[str, Any], evidence_text: str) -> str:
    return (
        "Create a review-required draft ADaM variable spec from the evidence below. "
        "Use reference ADaM only for output shape and comparison evidence; do not treat it alone as proof of derivation logic. "
        "Use legacy SAS/R code as derivation evidence when it clearly shows source data, joins, or formulas. "
        "If evidence is weak, still include a candidate variable only when useful, but lower confidence and add review reasons.\n\n"
        f"## Target\n{target}\n\n"
        "## Current Codegen Context\n"
        f"{_compact_context_summary(context)}\n\n"
        "## Auxiliary Evidence\n"
        f"{evidence_text or 'No auxiliary evidence files were readable.'}\n"
    )


def _auxiliary_evidence_text(root: Path, *, study_id: str, target: str, run_id: str) -> tuple[str, list[str]]:
    warnings: list[str] = []
    scanner = StudyInputScanner(root, study_id=study_id).scan()
    warnings.extend(scanner.warnings)
    sections: list[str] = []

    reader = SDTMReader()
    target_reference = scanner.reference_adam.get(target)
    if target_reference is not None:
        try:
            profile = reader.profile(target_reference.path, dataset=target, sample_rows=0)
            sections.append(
                "\n".join(
                    [
                        f"### Reference ADaM Shape: {target_reference.path}",
                        f"status={profile.status}; format={profile.format}; rows={profile.row_count if profile.row_count is not None else 'unknown'}",
                        "columns=" + (", ".join(profile.columns) if profile.columns else "(columns unavailable)"),
                        f"note={profile.message}" if profile.message else "",
                    ]
                ).strip()
            )
        except Exception as exc:  # pragma: no cover - defensive evidence boundary
            warnings.append(f"Could not profile reference ADaM for {target}: {exc}")

    for path in _matching_text_artifacts(scanner.legacy_code, target):
        sections.append(f"### Legacy Code: {Path(path).name}\n{_read_text_limited(Path(path), warnings)}")

    for path in _matching_text_artifacts(scanner.input_define, target):
        sections.append(f"### Define/Metadata: {Path(path).name}\n{_read_text_limited(Path(path), warnings)}")

    return "\n\n".join(sections), warnings


def _matching_text_artifacts(artifacts: dict[str, Any], target: str) -> list[str]:
    selected: list[str] = []
    fallback: list[str] = []
    for artifact in artifacts.values():
        path = Path(artifact.path)
        text = _safe_read(path)
        haystack = f"{path.stem}\n{text}".upper()
        if target in haystack:
            selected.append(artifact.path)
        elif len(fallback) < MAX_EVIDENCE_FILES:
            fallback.append(artifact.path)
    return (selected or fallback)[:MAX_EVIDENCE_FILES]


def _compact_context_summary(context: dict[str, Any]) -> str:
    lines = []
    for title, section in [
        ("SDTM source profiles", context.get("source_dataset_profiles", {})),
        ("Resolved ADaM dependencies", context.get("resolved_dependencies", {})),
    ]:
        lines.append(f"### {title}")
        if not isinstance(section, dict) or not section:
            lines.append("(none)")
            continue
        for dataset, profile in sorted(section.items()):
            columns = profile.get("columns", []) if isinstance(profile, dict) else []
            read_path = profile.get("read_path") if isinstance(profile, dict) else ""
            status = profile.get("status") if isinstance(profile, dict) else "unknown"
            lines.append(f"- {dataset}: status={status}; read_path={read_path}; columns={', '.join(columns) if columns else '(unavailable)'}")
    warnings = [str(item) for item in context.get("warnings", []) if str(item).strip()]
    if warnings:
        lines.append("### Existing warnings")
        lines.extend(f"- {warning}" for warning in warnings)
    return "\n".join(lines)


def _spec_variable(payload: Any, *, dataset: str, index: int) -> SpecVariable:
    if not isinstance(payload, dict):
        raise DraftSpecGenerationError("Each draft spec variable must be a JSON object.")
    variable = str(payload.get("variable") or payload.get("Variable") or "").strip().upper()
    if not variable:
        raise DraftSpecGenerationError("Each draft spec variable requires a variable name.")
    evidence_ids = _string_list(payload.get("evidence_ids")) or [f"ev_{dataset.lower()}_{variable.lower()}_llm_draft_{index}"]
    review_reasons = _string_list(payload.get("review_reasons"))
    if "No approved input_spec was supplied; generated from auxiliary evidence." not in review_reasons:
        review_reasons.append("No approved input_spec was supplied; generated from auxiliary evidence.")
    return SpecVariable(
        variable=variable,
        label=str(payload.get("label") or payload.get("Label") or variable).strip(),
        type=_spec_type(payload.get("type") or payload.get("Type")),
        source_domains=_upper_list(payload.get("source_domains")),
        source_variables=_upper_list(payload.get("source_variables") or payload.get("source_columns")),
        derivation=str(payload.get("derivation") or "Draft candidate from auxiliary evidence. Requires human review.").strip(),
        evidence_ids=evidence_ids,
        confidence=_confidence(payload.get("confidence")),
        review_required=True,
        review_reasons=review_reasons,
        risk_level=_risk_level(payload.get("risk_level")),
        approval_status="draft",
        assumptions=_string_list(payload.get("assumptions")),
    )


def _json_payload(text: str) -> Any:
    raw = text.strip()
    if raw.startswith("```"):
        raw = re.sub(r"^```(?:json)?\s*", "", raw, flags=re.IGNORECASE).strip()
        raw = re.sub(r"\s*```$", "", raw).strip()
    try:
        return json.loads(raw)
    except json.JSONDecodeError as exc:
        raise DraftSpecGenerationError(f"Draft spec response is not valid JSON: {exc}") from exc


def _write_text_artifact(
    *,
    root: Path,
    run_id: str,
    target: str,
    name: str,
    text: str,
    artifact_id: str,
    kind: str,
    role: str,
    folder: str,
    metadata: dict[str, Any] | None = None,
) -> ArtifactRef:
    path = root / "runs" / run_id / folder / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return ArtifactRef(
        artifact_id=artifact_id,
        kind=kind,
        path=str(path.as_posix()),
        sha256=f"sha256:{sha256_file(path)}",
        dataset=target,
        format=path.suffix.lower().lstrip("."),
        role=role,
        metadata=metadata or {},
    )


def _datasets_included(context: dict[str, Any]) -> list[str]:
    datasets = list(context.get("source_dataset_profiles", {}).keys())
    datasets.extend(context.get("resolved_dependencies", {}).keys())
    return datasets


def _variables_included(context: dict[str, Any]) -> list[str]:
    variables: list[str] = []
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        for profile in context.get(section, {}).values():
            for column in profile.get("columns", []):
                if column not in variables:
                    variables.append(column)
    return variables


def _sample_row_counts(context: dict[str, Any]) -> dict[str, int]:
    counts: dict[str, int] = {}
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        for dataset, profile in context.get(section, {}).items():
            rows = profile.get("sample_rows", [])
            if rows:
                counts[dataset] = len(rows)
    return counts


def _candidate_columns(context: dict[str, Any]) -> list[str]:
    columns: list[str] = []
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        for profile in context.get(section, {}).values():
            for column in profile.get("columns", []):
                normalized = str(column).strip().upper()
                if normalized and normalized not in columns:
                    columns.append(normalized)
    if "USUBJID" in columns:
        columns.remove("USUBJID")
        columns.insert(0, "USUBJID")
    return columns


def _source_domains_for_column(context: dict[str, Any], column: str) -> list[str]:
    domains: list[str] = []
    for dataset, profile in context.get("source_dataset_profiles", {}).items():
        columns = {str(item).upper() for item in profile.get("columns", [])}
        if column.upper() in columns:
            domains.append(str(dataset).upper())
    return domains


def _string_list(value: Any) -> list[str]:
    if isinstance(value, list):
        return [str(item).strip() for item in value if str(item).strip()]
    if isinstance(value, str) and value.strip():
        return [item.strip() for item in re.split(r"[,;]", value) if item.strip()]
    return []


def _upper_list(value: Any) -> list[str]:
    return [item.upper() for item in _string_list(value)]


def _spec_type(value: Any) -> str:
    text = str(value or "unknown").strip().lower()
    aliases = {"char": "character", "string": "character", "num": "numeric", "number": "numeric", "integer": "numeric"}
    normalized = aliases.get(text, text)
    return normalized if normalized in SPEC_TYPES else "unknown"


def _risk_level(value: Any) -> str:
    text = str(value or "high").strip().lower()
    return text if text in {"low", "medium", "high"} else "high"


def _confidence(value: Any) -> float:
    try:
        number = float(value)
    except (TypeError, ValueError):
        return 0.35
    return min(max(number, 0.0), 1.0)


def _read_text_limited(path: Path, warnings: list[str]) -> str:
    text = _safe_read(path)
    if not text:
        warnings.append(f"Could not read auxiliary evidence file: {path.name}")
        return ""
    return text[:MAX_TEXT_CHARS] + ("\n[truncated]" if len(text) > MAX_TEXT_CHARS else "")


def _safe_read(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        try:
            return path.read_text(encoding="latin-1")
        except OSError:
            return ""
    except OSError:
        return ""
