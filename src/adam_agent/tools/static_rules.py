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


class StaticRuleError(ValueError):
    """Raised when blocking static rules fail."""


@dataclass(frozen=True)
class StaticRuleFinding:
    """One deterministic static-rule finding."""

    rule_id: str
    severity: StaticRuleSeverity
    message: str
    confidence: str = "high"
    evidence: str = ""

    def as_dict(self) -> dict[str, str]:
        return {
            "rule_id": self.rule_id,
            "severity": self.severity,
            "message": self.message,
            "confidence": self.confidence,
            "evidence": self.evidence,
        }


@dataclass(frozen=True)
class StaticRulePolicy:
    """Configurable rule policy for one generated-code contract."""

    policy_id: str = "generated_r_contract_v1"
    forbidden_calls: tuple[str, ...] = ()
    required_output_paths: tuple[str, ...] = ()
    required_identifiers: tuple[str, ...] = ()
    required_identifier_severity: StaticRuleSeverity = "warning"

    def as_dict(self) -> dict[str, Any]:
        return {
            "policy_id": self.policy_id,
            "forbidden_calls": list(self.forbidden_calls),
            "required_output_paths": list(self.required_output_paths),
            "required_identifiers": list(self.required_identifiers),
            "required_identifier_severity": self.required_identifier_severity,
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
    policy: StaticRulePolicy | None = None,
) -> StaticRuleReport:
    """Run narrow static checks against generated R code."""

    path = Path(code_path)
    target = dataset.strip().upper()
    active_policy = _merge_policy(
        policy,
        expected_output_path=expected_output_path,
        required_identifiers=required_identifiers,
    )
    findings: list[StaticRuleFinding] = []
    if not path.exists() or not path.is_file():
        findings.append(
            StaticRuleFinding(
                rule_id="R_FILE_EXISTS",
                severity="error",
                message=f"Generated R script does not exist: {path}",
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
                    confidence="medium",
                    evidence=identifier,
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


def _merge_policy(
    policy: StaticRulePolicy | None,
    *,
    expected_output_path: str | None,
    required_identifiers: list[str] | tuple[str, ...] | None,
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


def _same_existing_path(left: Path, right: Path) -> bool:
    if not str(left):
        return False
    try:
        return left.resolve() == right.resolve()
    except OSError:
        return left.as_posix() == right.as_posix()


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
            "Additional standards-aware policies belong in later LG2.5 increments.",
        ],
    )
