"""Small deterministic static checks for generated R code.

The checker is intentionally policy-driven. It knows about generic contracts
such as unsafe calls, required output paths, and required identifiers supplied
by the caller; it does not hard-code dataset-specific ADaM derivation rules.
These checks are guardrails before human code review, not a full CDISC/P21
compliance engine.
"""

from __future__ import annotations

from dataclasses import dataclass, field
import json
import re
from pathlib import Path
from typing import Any, Literal

from adam_agent.tools.artifacts import sha256_file


StaticRuleSeverity = Literal["error", "warning", "info"]
StaticRuleCategory = Literal["artifact_contract", "execution_boundary", "spec_contract", "standards_pack"]
StaticRuleSourceType = Literal["system_contract", "approved_spec", "standards_pack", "user_policy"]

STATIC_RULE_SEVERITIES = {"error", "warning", "info"}
STATIC_RULE_CATEGORIES = {"artifact_contract", "execution_boundary", "spec_contract", "standards_pack"}
STATIC_RULE_SOURCE_TYPES = {"system_contract", "approved_spec", "standards_pack", "user_policy"}
SOURCE_ID_REQUIRED_TYPES = {"approved_spec", "standards_pack", "user_policy"}


class StaticRuleError(ValueError):
    """Raised when blocking static rules fail."""


@dataclass(frozen=True)
class StaticRuleFinding:
    """One deterministic static-rule finding."""

    rule_id: str
    severity: StaticRuleSeverity
    message: str
    category: StaticRuleCategory
    source_type: StaticRuleSourceType
    confidence: str = "high"
    evidence: str = ""
    source_id: str = ""

    def as_dict(self) -> dict[str, str]:
        return {
            "rule_id": self.rule_id,
            "severity": self.severity,
            "message": self.message,
            "category": self.category,
            "source_type": self.source_type,
            "confidence": self.confidence,
            "evidence": self.evidence,
            "source_id": self.source_id,
        }


@dataclass(frozen=True)
class StaticRulePackItem:
    """One admitted source-backed rule-pack item.

    This object records rule provenance only. Rule execution remains a separate
    later step so clinical requirements cannot slip into the generic engine.
    """

    rule_id: str
    description: str
    severity: StaticRuleSeverity
    source: str
    version: str
    scope: tuple[str, ...]
    evidence: str
    enabled: bool = True

    @property
    def source_id(self) -> str:
        return f"{self.source}:{self.version}:{self.rule_id}"

    def as_dict(self) -> dict[str, Any]:
        return {
            "rule_id": self.rule_id,
            "description": self.description,
            "severity": self.severity,
            "source": self.source,
            "version": self.version,
            "scope": list(self.scope),
            "evidence": self.evidence,
            "enabled": self.enabled,
            "source_id": self.source_id,
        }


@dataclass(frozen=True)
class StaticRulePack:
    """Admitted standards/company rule pack with explicit provenance."""

    pack_id: str
    source: str
    version: str
    scope: tuple[str, ...]
    rules: tuple[StaticRulePackItem, ...]

    def as_dict(self) -> dict[str, Any]:
        return {
            "pack_id": self.pack_id,
            "source": self.source,
            "version": self.version,
            "scope": list(self.scope),
            "rules": [rule.as_dict() for rule in self.rules],
        }


@dataclass(frozen=True)
class StaticRulePolicy:
    """Configurable rule policy for one generated-code contract."""

    policy_id: str = "generated_r_contract_v1"
    forbidden_calls: tuple[str, ...] = ()
    required_output_paths: tuple[str, ...] = ()
    required_identifiers: tuple[str, ...] = ()
    required_identifier_severity: StaticRuleSeverity = "warning"
    required_identifier_source_type: StaticRuleSourceType = "approved_spec"
    required_identifier_source_id: str = ""

    def as_dict(self) -> dict[str, Any]:
        return {
            "policy_id": self.policy_id,
            "forbidden_calls": list(self.forbidden_calls),
            "required_output_paths": list(self.required_output_paths),
            "required_identifiers": list(self.required_identifiers),
            "required_identifier_severity": self.required_identifier_severity,
            "required_identifier_source_type": self.required_identifier_source_type,
            "required_identifier_source_id": self.required_identifier_source_id,
            "rule_governance": {
                "engine_scope": "generic_contracts_only",
                "demo_observation_policy": "Demo observations can become static rules only after promotion into source-backed rule packs.",
                "clinical_rule_policy": "clinical/domain rules require approved specs or versioned standards packs",
            },
        }


@dataclass(frozen=True)
class StaticRuleReport:
    """Structured static check report for one generated R script."""

    study_id: str
    run_id: str
    dataset: str
    status: Literal["pass", "blocked", "warning"]
    implemented: bool
    code_path: str
    policy: StaticRulePolicy
    findings: list[StaticRuleFinding] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    notes: list[str] = field(default_factory=list)

    @property
    def blocking_errors(self) -> list[StaticRuleFinding]:
        return [finding for finding in self.findings if finding.severity == "error"]

    def as_dict(self) -> dict[str, Any]:
        return {
            "study_id": self.study_id,
            "run_id": self.run_id,
            "dataset": self.dataset,
            "status": self.status,
            "implemented": self.implemented,
            "code_path": self.code_path,
            "code_sha256": f"sha256:{sha256_file(self.code_path)}" if Path(self.code_path).exists() else None,
            "policy": self.policy.as_dict(),
            "expected_output_path": self.policy.required_output_paths[0] if self.policy.required_output_paths else None,
            "expected_output_paths": list(self.policy.required_output_paths),
            "findings": [finding.as_dict() for finding in self.findings],
            "blocking_errors": [finding.as_dict() for finding in self.blocking_errors],
            "warnings": list(self.warnings),
            "notes": list(self.notes),
            "non_compliance_disclaimer": "These checks do not prove full CDISC, ADaM IG, P21, or company-standard compliance.",
        }


DEFAULT_FORBIDDEN_R_CALLS = (
    "system",
    "system2",
    "shell",
    "unlink",
    "file.remove",
    "download.file",
    "install.packages",
)


def run_generated_r_static_checks(
    *,
    study_id: str,
    run_id: str,
    dataset: str,
    code_path: str | Path,
    expected_output_path: str | None = None,
    required_identifiers: list[str] | tuple[str, ...] | None = None,
    required_identifier_source_id: str | None = None,
    policy: StaticRulePolicy | None = None,
) -> StaticRuleReport:
    """Run narrow static checks against generated R code."""

    path = Path(code_path)
    target = dataset.strip().upper()
    active_policy = _merge_policy(
        policy,
        expected_output_path=expected_output_path,
        required_identifiers=required_identifiers,
        required_identifier_source_id=required_identifier_source_id,
    )
    _validate_policy_governance(active_policy)
    findings: list[StaticRuleFinding] = []
    if not path.exists() or not path.is_file():
        findings.append(
            StaticRuleFinding(
                rule_id="R_FILE_EXISTS",
                severity="error",
                message=f"Generated R script does not exist: {path}",
                category="artifact_contract",
                source_type="system_contract",
                evidence=str(path.as_posix()),
            )
        )
        return _report(study_id, run_id, target, path, active_policy, findings)

    text = path.read_text(encoding="utf-8", errors="replace")
    code_for_calls = _strip_r_comments_and_strings(text)
    code_for_identifiers = _strip_r_comments(text)
    findings.extend(_dangerous_call_findings(code_for_calls, active_policy.forbidden_calls))
    string_literals = _r_string_literals(text)
    for output_path in active_policy.required_output_paths:
        if not _writes_required_output(string_literals, output_path):
            findings.append(
                StaticRuleFinding(
                    rule_id="R_REQUIRED_OUTPUT_PATH",
                    severity="error",
                    message=f"Generated R code does not visibly write required output path: {output_path}.",
                    category="artifact_contract",
                    source_type="system_contract",
                    evidence=output_path,
                )
            )
    for identifier in active_policy.required_identifiers:
        if not _contains_identifier(code_for_identifiers, identifier):
            findings.append(
                StaticRuleFinding(
                    rule_id="R_REQUIRED_IDENTIFIER_REFERENCE",
                    severity=active_policy.required_identifier_severity,
                    message=f"Generated R code does not visibly reference required identifier: {identifier}.",
                    category="spec_contract",
                    source_type=active_policy.required_identifier_source_type,
                    confidence="medium",
                    evidence=identifier,
                    source_id=active_policy.required_identifier_source_id,
                )
            )
    return _report(study_id, run_id, target, path, active_policy, findings)


def write_static_rule_report(report: StaticRuleReport, *, path: str | Path) -> Path:
    """Write a static-rule report as JSON."""

    output = Path(path)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(report.as_dict(), indent=2, sort_keys=True), encoding="utf-8")
    return output


def assert_no_blocking_static_findings(report: StaticRuleReport) -> None:
    """Fail closed when static checks find blocking errors."""

    errors = report.blocking_errors
    if errors:
        message = "; ".join(f"{finding.rule_id}: {finding.message}" for finding in errors)
        raise StaticRuleError(message)


def load_static_rule_pack(path: str | Path) -> StaticRulePack:
    """Load and validate one explicit static rule pack."""

    pack_path = Path(path)
    if not pack_path.exists() or not pack_path.is_file():
        raise StaticRuleError(f"Static rule pack does not exist: {pack_path}")
    try:
        payload = json.loads(pack_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise StaticRuleError(f"Static rule pack is not valid JSON: {pack_path}") from exc
    return validate_static_rule_pack_payload(payload)


def validate_static_rule_pack_payload(payload: Any) -> StaticRulePack:
    """Validate rule-pack admission metadata before any rule can affect a run."""

    if not isinstance(payload, dict):
        raise StaticRuleError("Static rule pack must be a JSON object.")
    pack_id = _required_text(payload, "pack_id", context="Static rule pack")
    source = _required_text(payload, "source", context=f"Static rule pack {pack_id}")
    version = _required_text(payload, "version", context=f"Static rule pack {pack_id}")
    scope = _required_scope(payload.get("scope"), context=f"Static rule pack {pack_id}")
    rules_payload = payload.get("rules")
    if not isinstance(rules_payload, list) or not rules_payload:
        raise StaticRuleError(f"Static rule pack {pack_id} must include at least one rule item.")
    rules = tuple(_validate_rule_pack_item(item, pack_id=pack_id, index=index) for index, item in enumerate(rules_payload))
    rule_ids = [rule.rule_id for rule in rules]
    duplicates = sorted({rule_id for rule_id in rule_ids if rule_ids.count(rule_id) > 1})
    if duplicates:
        raise StaticRuleError(f"Static rule pack {pack_id} has duplicate rule_id values: {', '.join(duplicates)}.")
    return StaticRulePack(pack_id=pack_id, source=source, version=version, scope=scope, rules=rules)


def validate_static_rule_report_artifact(
    path: str | Path,
    *,
    dataset: str,
    code_path: str | Path,
    code_sha256: str,
) -> dict[str, Any]:
    """Validate that a static-rule artifact is complete and bound to current code."""

    report_path = Path(path)
    expected_code_path = Path(code_path)
    target = dataset.strip().upper()
    if not report_path.exists() or not report_path.is_file():
        raise StaticRuleError(f"Static-check artifact does not exist: {report_path}")
    if not expected_code_path.exists() or not expected_code_path.is_file():
        raise StaticRuleError(f"Generated R script does not exist: {expected_code_path}")
    current_code_sha = f"sha256:{sha256_file(expected_code_path)}"
    if current_code_sha != code_sha256:
        raise StaticRuleError("Generated R script hash does not match the supplied code hash.")
    try:
        payload = json.loads(report_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise StaticRuleError(f"Static-check artifact is not valid JSON: {report_path}") from exc
    if not isinstance(payload, dict):
        raise StaticRuleError(f"Static-check artifact must be a JSON object: {report_path}")
    _require_static_report_schema(payload)
    status = str(payload.get("status") or "").strip().lower()
    if status == "blocked" or payload.get("blocking_errors"):
        raise StaticRuleError("Static-check artifact has blocking findings. Regenerate or repair code before review.")
    if status not in {"pass", "warning"}:
        raise StaticRuleError(f"Static-check artifact has invalid status: {status or '<missing>'}")
    if payload.get("implemented") is not True:
        raise StaticRuleError("Static-check artifact must come from an implemented static-rule check.")
    report_dataset = str(payload.get("dataset") or "").strip().upper()
    if report_dataset != target:
        raise StaticRuleError(f"Static-check artifact dataset {report_dataset or '<missing>'} does not match {target}.")
    report_code_sha = str(payload.get("code_sha256") or "").strip()
    if report_code_sha != code_sha256:
        raise StaticRuleError("Static-check artifact is not bound to the current generated R code hash.")
    report_code_path = Path(str(payload.get("code_path") or ""))
    if not _same_existing_path(report_code_path, expected_code_path):
        raise StaticRuleError("Static-check artifact is not bound to the current generated R code path.")
    policy = payload.get("policy")
    if not isinstance(policy, dict) or not str(policy.get("policy_id") or "").strip():
        raise StaticRuleError("Static-check artifact is missing its static-rule policy.")
    return payload


def _validate_rule_pack_item(item: Any, *, pack_id: str, index: int) -> StaticRulePackItem:
    context = f"Static rule pack {pack_id} rule[{index}]"
    if not isinstance(item, dict):
        raise StaticRuleError(f"{context} must be an object.")
    rule_id = _required_text(item, "rule_id", context=context)
    description = _required_text(item, "description", context=context)
    severity = _required_text(item, "severity", context=context)
    if severity not in STATIC_RULE_SEVERITIES:
        raise StaticRuleError(f"{context} has invalid severity: {severity}.")
    source = _required_text(item, "source", context=context)
    version = _required_text(item, "version", context=context)
    scope = _required_scope(item.get("scope"), context=context)
    evidence = _required_text(item, "evidence", context=context)
    enabled = item.get("enabled", True)
    if not isinstance(enabled, bool):
        raise StaticRuleError(f"{context} enabled must be a boolean.")
    return StaticRulePackItem(
        rule_id=rule_id,
        description=description,
        severity=severity,  # type: ignore[arg-type]
        source=source,
        version=version,
        scope=scope,
        evidence=evidence,
        enabled=enabled,
    )


def _required_text(payload: dict[str, Any], key: str, *, context: str) -> str:
    raw_value = payload.get(key)
    if not isinstance(raw_value, str):
        raise StaticRuleError(f"{context} must include {key} as a non-empty string.")
    value = raw_value.strip()
    if not value:
        raise StaticRuleError(f"{context} must include {key} as a non-empty string.")
    return value


def _required_scope(value: Any, *, context: str) -> tuple[str, ...]:
    if not isinstance(value, list):
        raise StaticRuleError(f"{context} must include scope as a non-empty list.")
    if any(not isinstance(item, str) for item in value):
        raise StaticRuleError(f"{context} scope values must be strings.")
    scope = tuple(item.strip() for item in value if item.strip())
    if not scope:
        raise StaticRuleError(f"{context} must include scope as a non-empty list.")
    return scope


def _merge_policy(
    policy: StaticRulePolicy | None,
    *,
    expected_output_path: str | None,
    required_identifiers: list[str] | tuple[str, ...] | None,
    required_identifier_source_id: str | None,
) -> StaticRulePolicy:
    base = policy or StaticRulePolicy(forbidden_calls=DEFAULT_FORBIDDEN_R_CALLS)
    output_paths = list(base.required_output_paths)
    if expected_output_path:
        output_paths.append(_normalize_output_path(expected_output_path))
    identifiers = list(base.required_identifiers)
    identifiers.extend(_normalized_identifiers(required_identifiers or []))
    return StaticRulePolicy(
        policy_id=base.policy_id,
        forbidden_calls=tuple(dict.fromkeys(base.forbidden_calls or DEFAULT_FORBIDDEN_R_CALLS)),
        required_output_paths=tuple(dict.fromkeys(_normalize_output_path(path) for path in output_paths if str(path).strip())),
        required_identifiers=tuple(dict.fromkeys(identifiers)),
        required_identifier_severity=base.required_identifier_severity,
        required_identifier_source_type=base.required_identifier_source_type,
        required_identifier_source_id=str(required_identifier_source_id or base.required_identifier_source_id or "").strip(),
    )


def _dangerous_call_findings(code_without_strings: str, forbidden_calls: tuple[str, ...]) -> list[StaticRuleFinding]:
    findings = []
    for call in forbidden_calls:
        pattern = re.compile(rf"(?<![A-Za-z0-9_.]){re.escape(call)}\s*\(", flags=re.IGNORECASE)
        match = pattern.search(code_without_strings)
        if match:
            findings.append(
                StaticRuleFinding(
                    rule_id="R_FORBIDDEN_CALL",
                    severity="error",
                    message=f"Generated R code uses forbidden call: {call}().",
                    category="execution_boundary",
                    source_type="system_contract",
                    evidence=match.group(0),
                )
            )
    return findings


def _writes_required_output(string_literals: list[str], required_output_path: str) -> bool:
    normalized_expected = _normalize_output_path(required_output_path)
    return any(_normalize_output_path(value).endswith(normalized_expected) for value in string_literals)


def _r_string_literals(code: str) -> list[str]:
    pattern = re.compile(r"""(["'])(.*?)(?<!\\)\1""", flags=re.DOTALL)
    return [match.group(2).replace("\\/", "/") for match in pattern.finditer(code)]


def _strip_r_comments_and_strings(code: str) -> str:
    result: list[str] = []
    in_string: str | None = None
    escaped = False
    index = 0
    while index < len(code):
        character = code[index]
        if in_string:
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == in_string:
                in_string = None
            result.append(" ")
            index += 1
            continue
        if character in {"'", '"'}:
            in_string = character
            result.append(" ")
            index += 1
            continue
        if character == "#":
            while index < len(code) and code[index] not in {"\r", "\n"}:
                result.append(" ")
                index += 1
            continue
        result.append(character)
        index += 1
    return "".join(result)


def _strip_r_comments(code: str) -> str:
    result: list[str] = []
    in_string: str | None = None
    escaped = False
    index = 0
    while index < len(code):
        character = code[index]
        if in_string:
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == in_string:
                in_string = None
            result.append(character)
            index += 1
            continue
        if character in {"'", '"'}:
            in_string = character
            result.append(character)
            index += 1
            continue
        if character == "#":
            while index < len(code) and code[index] not in {"\r", "\n"}:
                result.append(" ")
                index += 1
            continue
        result.append(character)
        index += 1
    return "".join(result)


def _contains_identifier(code_without_strings: str, identifier: str) -> bool:
    if not identifier:
        return True
    pattern = re.compile(rf"(?<![A-Za-z0-9_.]){re.escape(identifier)}(?![A-Za-z0-9_.])", flags=re.IGNORECASE)
    return bool(pattern.search(code_without_strings))


def _normalized_identifiers(values: list[str] | tuple[str, ...]) -> list[str]:
    identifiers: list[str] = []
    for value in values:
        text = str(value).strip().upper()
        if text and re.fullmatch(r"[A-Z][A-Z0-9_]*", text):
            identifiers.append(text)
    return identifiers


def _normalize_output_path(path: str | Path) -> str:
    return str(path).replace("\\", "/").strip().strip("'\"").lstrip("./")


def _require_static_report_schema(payload: dict[str, Any]) -> None:
    required_keys = {
        "study_id",
        "run_id",
        "dataset",
        "status",
        "implemented",
        "code_path",
        "code_sha256",
        "policy",
        "findings",
        "blocking_errors",
        "warnings",
        "notes",
        "non_compliance_disclaimer",
    }
    missing = sorted(key for key in required_keys if key not in payload)
    if missing:
        raise StaticRuleError(f"Static-check artifact is missing required fields: {', '.join(missing)}.")
    for key in ("findings", "blocking_errors", "warnings", "notes"):
        if not isinstance(payload.get(key), list):
            raise StaticRuleError(f"Static-check artifact field must be a list: {key}.")
    for key in ("findings", "blocking_errors"):
        for index, item in enumerate(payload.get(key) or []):
            _validate_static_finding_payload(item, field_path=f"{key}[{index}]")


def _same_existing_path(left: Path, right: Path) -> bool:
    if not str(left):
        return False
    try:
        return left.resolve() == right.resolve()
    except OSError:
        return left.as_posix() == right.as_posix()


def _validate_policy_governance(policy: StaticRulePolicy) -> None:
    source_type = str(policy.required_identifier_source_type or "").strip()
    if source_type not in STATIC_RULE_SOURCE_TYPES:
        raise StaticRuleError(f"Static-rule policy has invalid required identifier source_type: {source_type or '<missing>'}.")
    if policy.required_identifier_severity not in STATIC_RULE_SEVERITIES:
        raise StaticRuleError(
            f"Static-rule policy has invalid required identifier severity: {policy.required_identifier_severity}."
        )
    if policy.required_identifiers and source_type in SOURCE_ID_REQUIRED_TYPES and not policy.required_identifier_source_id:
        raise StaticRuleError(
            "Static-rule policy required identifiers from approved_spec, standards_pack, or user_policy must include source_id. "
            "Regenerate the static-check artifact from the current code/spec context."
        )


def _validate_static_finding_payload(item: Any, *, field_path: str) -> None:
    if not isinstance(item, dict):
        raise StaticRuleError(f"Static-check artifact {field_path} must be an object.")
    missing = [
        field
        for field in ("rule_id", "severity", "category", "source_type", "confidence", "evidence")
        if field not in item
    ]
    if missing:
        raise StaticRuleError(
            f"Static-check artifact {field_path} is missing rule-governance fields: {', '.join(missing)}. "
            "Regenerate the static-check artifact with the current rule-governance schema."
        )
    severity = str(item.get("severity") or "").strip()
    category = str(item.get("category") or "").strip()
    source_type = str(item.get("source_type") or "").strip()
    source_id = str(item.get("source_id") or "").strip()
    if severity not in STATIC_RULE_SEVERITIES:
        raise StaticRuleError(f"Static-check artifact {field_path} has invalid severity: {severity or '<missing>'}.")
    if category not in STATIC_RULE_CATEGORIES:
        raise StaticRuleError(f"Static-check artifact {field_path} has invalid category: {category or '<missing>'}.")
    if source_type not in STATIC_RULE_SOURCE_TYPES:
        raise StaticRuleError(f"Static-check artifact {field_path} has invalid source_type: {source_type or '<missing>'}.")
    if source_type in SOURCE_ID_REQUIRED_TYPES and not source_id:
        raise StaticRuleError(
            f"Static-check artifact {field_path} uses source_type={source_type} but has no source_id. "
            "Regenerate the static-check artifact from the current code/spec context."
        )


def _report(
    study_id: str,
    run_id: str,
    dataset: str,
    code_path: Path,
    policy: StaticRulePolicy,
    findings: list[StaticRuleFinding],
) -> StaticRuleReport:
    has_error = any(finding.severity == "error" for finding in findings)
    has_warning = any(finding.severity == "warning" for finding in findings)
    status: Literal["pass", "blocked", "warning"] = "blocked" if has_error else "warning" if has_warning else "pass"
    warnings = [
        "Static rules are limited guardrails before human code review.",
        "This report does not prove full CDISC/ADaM IG/P21 compliance.",
    ]
    return StaticRuleReport(
        study_id=study_id,
        run_id=run_id,
        dataset=dataset,
        status=status,
        implemented=True,
        code_path=str(code_path.as_posix()),
        policy=policy,
        findings=findings,
        warnings=warnings,
        notes=[
            "Blocking checks currently cover generic R safety calls and caller-provided output contracts.",
            "Identifier checks are caller-provided visibility checks, not proof of clinical derivation correctness.",
            "Demo observations must be promoted into source-backed rule packs before becoming static rules.",
            "Additional standards-aware policies belong in later LG2.5 increments.",
        ],
    )
