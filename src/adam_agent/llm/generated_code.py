"""Parse LLM code-generation responses and write auditable code artifacts."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.tools.artifacts import sha256_file


class LLMGeneratedCodeError(ValueError):
    """Raised when an LLM response does not satisfy the code output contract."""


@dataclass(frozen=True)
class GeneratedCodePackage:
    """Parsed LLM output for one target ADaM dataset."""

    dataset: str
    r_code: str
    assumptions: list[str] = field(default_factory=list)
    risk_points: list[str] = field(default_factory=list)
    used_inputs: list[str] = field(default_factory=list)
    expected_outputs: list[str] = field(default_factory=list)
    raw_payload: dict[str, Any] = field(default_factory=dict)

    def as_dict(self) -> dict[str, Any]:
        return {
            "dataset": self.dataset,
            "r_code": self.r_code,
            "assumptions": self.assumptions,
            "risk_points": self.risk_points,
            "used_inputs": self.used_inputs,
            "expected_outputs": self.expected_outputs,
        }


@dataclass(frozen=True)
class GeneratedCodeArtifacts:
    """Artifact refs written from one parsed LLM response."""

    response_artifact: ArtifactRef
    code_artifact: ArtifactRef
    package_artifact: ArtifactRef

    def as_list(self) -> list[ArtifactRef]:
        return [self.response_artifact, self.code_artifact, self.package_artifact]


def parse_generated_code_response(response_text: str, *, expected_dataset: str | None = None) -> GeneratedCodePackage:
    """Parse the strict JSON output contract returned by an LLM."""

    try:
        payload = json.loads(response_text)
    except json.JSONDecodeError as exc:
        raise LLMGeneratedCodeError(f"LLM response is not valid JSON: {exc}") from exc
    if not isinstance(payload, dict):
        raise LLMGeneratedCodeError("LLM response must be a JSON object.")

    dataset = _required_str(payload, "dataset").upper()
    if expected_dataset and dataset != expected_dataset.strip().upper():
        raise LLMGeneratedCodeError(f"LLM response dataset {dataset} does not match expected {expected_dataset}.")

    r_code = _required_str(payload, "r_code")
    metadata_warnings: list[str] = []
    assumptions = _metadata_string_list(payload.get("assumptions", []), "assumptions", metadata_warnings)
    risk_points = _metadata_string_list(payload.get("risk_points", []), "risk_points", metadata_warnings)
    used_inputs = _metadata_string_list(payload.get("used_inputs", []), "used_inputs", metadata_warnings)
    expected_outputs = _metadata_string_list(payload.get("expected_outputs", []), "expected_outputs", metadata_warnings)

    return GeneratedCodePackage(
        dataset=dataset,
        r_code=r_code,
        assumptions=assumptions,
        risk_points=risk_points + metadata_warnings,
        used_inputs=used_inputs,
        expected_outputs=expected_outputs,
        raw_payload=payload,
    )


def write_generated_code_artifacts(
    *,
    study_id: str,
    run_id: str,
    study_dir: str | Path,
    package: GeneratedCodePackage,
    response_text: str,
    attempt_label: str | None = None,
) -> GeneratedCodeArtifacts:
    """Write the raw response, parsed package, and R script under the run dir."""

    root = Path(study_dir)
    dataset_lower = package.dataset.lower()
    suffix = f"_{attempt_label}" if attempt_label else ""
    llm_dir = root / "runs" / run_id / "llm"
    code_dir = root / "runs" / run_id / "code"
    llm_dir.mkdir(parents=True, exist_ok=True)
    code_dir.mkdir(parents=True, exist_ok=True)

    response_path = llm_dir / f"{dataset_lower}_response{suffix}.json"
    package_path = llm_dir / f"{dataset_lower}_parsed_response{suffix}.json"
    code_path = code_dir / f"build_{dataset_lower}{suffix}.R"

    response_path.write_text(response_text, encoding="utf-8")
    package_path.write_text(json.dumps(package.as_dict(), indent=2, sort_keys=True), encoding="utf-8")
    code_path.write_text(package.r_code, encoding="utf-8")

    response_artifact = ArtifactRef(
        artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{dataset_lower}{suffix}",
        kind="llm_response",
        path=str(response_path.as_posix()),
        sha256=f"sha256:{sha256_file(response_path)}",
        dataset=package.dataset,
        format="json",
        role="audit",
        metadata={
            "parsed": True,
            "assumption_count": len(package.assumptions),
            "risk_point_count": len(package.risk_points),
            "attempt_label": attempt_label or "initial",
        },
    )
    code_artifact = ArtifactRef(
        artifact_id=f"generated_code_{study_id.lower()}_{run_id}_{dataset_lower}{suffix}",
        kind="generated_code",
        path=str(code_path.as_posix()),
        sha256=f"sha256:{sha256_file(code_path)}",
        dataset=package.dataset,
        format="R",
        role="output",
        metadata={
            "source": "llm_response",
            "used_inputs": package.used_inputs,
            "expected_outputs": package.expected_outputs,
            "attempt_label": attempt_label or "initial",
        },
    )
    package_artifact = ArtifactRef(
        artifact_id=f"llm_parsed_response_{study_id.lower()}_{run_id}_{dataset_lower}{suffix}",
        kind="tool_log",
        path=str(package_path.as_posix()),
        sha256=f"sha256:{sha256_file(package_path)}",
        dataset=package.dataset,
        format="json",
        role="audit",
        metadata={"parsed_llm_response": True, "attempt_label": attempt_label or "initial"},
    )
    return GeneratedCodeArtifacts(
        response_artifact=response_artifact,
        code_artifact=code_artifact,
        package_artifact=package_artifact,
    )


def _required_str(payload: dict[str, Any], key: str) -> str:
    value = payload.get(key)
    if not isinstance(value, str) or not value.strip():
        raise LLMGeneratedCodeError(f"LLM response requires non-empty string field: {key}")
    return value.strip()


def _metadata_string_list(value: Any, key: str, warnings: list[str]) -> list[str]:
    if value is None:
        return []
    if isinstance(value, str):
        warnings.append(f"LLM response field {key} was a string and was normalized to a one-item list.")
        text = value.strip()
        return [text] if text else []
    if not isinstance(value, list):
        warnings.append(f"LLM response field {key} was normalized to a list of strings.")
        text = _metadata_item_to_string(value)
        return [text] if text else []
    result: list[str] = []
    normalized = False
    for item in value:
        if not isinstance(item, str):
            normalized = True
        text = _metadata_item_to_string(item)
        if text:
            result.append(text)
    if normalized:
        warnings.append(f"LLM response field {key} contained non-string items and was normalized to strings.")
    return result


def _metadata_item_to_string(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, str):
        return value.strip()
    if isinstance(value, (dict, list)):
        return json.dumps(value, ensure_ascii=True, sort_keys=True)
    return str(value).strip()
