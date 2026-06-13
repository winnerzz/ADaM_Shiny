"""Import external Codex-authored R code packages into Studio review flow."""

from __future__ import annotations

import json
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from adam_agent.agents.contracts import build_agent_node_input, build_agent_node_output, record_agent_decision
from adam_agent.schemas.codex_package import (
    CodexPackageManifest,
    build_codex_package_manifest,
)
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.static_rules import (
    StaticRuleError,
    assert_no_blocking_static_findings,
    run_generated_r_static_checks,
    write_static_rule_report,
)


class ExternalCodePackageError(RuntimeError):
    """Raised when an external package cannot be imported safely."""


@dataclass(frozen=True)
class ExternalCodePackageImportRequest:
    """Request to import one Codex-authored dataset script."""

    study_dir: str
    study_id: str
    run_id: str
    dataset: str
    staging_root: str
    source_workspace: str | None = None
    package_id: str | None = None
    codex_thread_id: str | None = None
    code_path: str | None = None
    assumptions_path: str | None = None
    rscript_path: str | None = None
    required_identifiers: list[str] | None = None
    required_identifier_source_id: str | None = None


@dataclass(frozen=True)
class ExternalCodePackageImportResult:
    """Imported external package files ready for graph code review."""

    dataset: str
    code_path: str
    code_sha256: str
    static_check_path: str
    static_check_sha256: str
    manifest_path: str
    package_manifest_path: str
    assumptions: list[str]
    risk_points: list[str]
    used_inputs: list[str]
    expected_outputs: list[str]
    warnings: list[str]
    agent_decisions: list[dict[str, Any]]
    agent_node_inputs: list[dict[str, Any]]
    agent_node_outputs: list[dict[str, Any]]
    risk_flags: list[str]
    code_agent_metadata: dict[str, Any]


def import_external_code_package(request: ExternalCodePackageImportRequest) -> ExternalCodePackageImportResult:
    """Import one external R script as generated-code evidence for review.

    The imported code is not executed and is not approved. It only enters the
    same code-review gate used by graph-generated code.
    """

    study_dir = Path(request.study_dir).expanduser()
    staging_root = Path(request.staging_root).expanduser()
    if not study_dir.exists() or not study_dir.is_dir():
        raise ExternalCodePackageError(f"study_dir does not exist: {study_dir}")
    if not staging_root.exists() or not staging_root.is_dir():
        raise ExternalCodePackageError(f"Codex package staging_root does not exist: {staging_root}")

    target = request.dataset.strip().upper()
    source_code = _resolve_source_code(staging_root, target, request.code_path)
    package_id = request.package_id or f"codex_external_r_{request.study_id.lower()}_{request.run_id}_{target.lower()}"
    source_workspace = request.source_workspace or str(staging_root.parent.as_posix())

    package_manifest = build_codex_package_manifest(
        package_id=package_id,
        package_type="r_code_authoring",
        study_id=request.study_id,
        run_id=request.run_id,
        target_datasets=[target],
        source_workspace=source_workspace,
        staging_root=str(staging_root.as_posix()),
        codex_thread_id=request.codex_thread_id,
        assumptions_path=request.assumptions_path,
        artifacts=[
            {
                "path": _relative_to_root(source_code, staging_root),
                "role": "code",
                "required": True,
                "description": f"Dataset R script imported for {target}.",
            }
        ],
        warnings=[
            "External Codex package is authoring evidence only; Studio review and execution remain required.",
        ],
    )
    readiness = package_manifest.readiness()
    if not readiness.ready:
        raise ExternalCodePackageError(
            "External Codex package is missing required files: "
            + "; ".join(readiness.missing_disk_paths or readiness.missing_manifest_paths)
        )

    run_dir = study_dir / "runs" / request.run_id
    import_dir = run_dir / "external_code" / target.lower()
    official_code_dir = run_dir / "code"
    import_dir.mkdir(parents=True, exist_ok=True)
    official_code_dir.mkdir(parents=True, exist_ok=True)
    imported_source_copy_path = import_dir / f"source_build_{target.lower()}.R"
    imported_code_path = official_code_dir / f"build_{target.lower()}.R"
    shutil.copy2(source_code, imported_source_copy_path)
    shutil.copy2(source_code, imported_code_path)

    package_manifest_path = import_dir / "codex_package_manifest.json"
    package_manifest_path.write_text(package_manifest.model_dump_json(indent=2), encoding="utf-8")

    static_check_path = run_dir / "static_checks" / f"{target.lower()}_external_static_check.json"
    try:
        static_report = run_generated_r_static_checks(
            study_id=request.study_id,
            run_id=request.run_id,
            dataset=target,
            code_path=imported_code_path,
            expected_output_path=f"outputs/{target.lower()}.csv",
            required_identifiers=request.required_identifiers or [],
            required_identifier_source_id=request.required_identifier_source_id,
            rscript_path=request.rscript_path,
        )
        write_static_rule_report(static_report, path=static_check_path)
        assert_no_blocking_static_findings(static_report)
    except StaticRuleError as exc:
        raise ExternalCodePackageError(str(exc)) from exc

    assumptions = _read_assumptions(staging_root, package_manifest.assumptions_path)
    manifest_path = import_dir / "import_manifest.json"
    imported_code_sha = f"sha256:{sha256_file(imported_code_path)}"
    static_sha = f"sha256:{sha256_file(static_check_path)}"
    payload = {
        "study_id": request.study_id,
        "run_id": request.run_id,
        "dataset": target,
        "source": "external_codex_package",
        "staging_root": str(staging_root.as_posix()),
        "source_code_path": str(source_code.as_posix()),
        "imported_source_copy_path": str(imported_source_copy_path.as_posix()),
        "imported_code_path": str(imported_code_path.as_posix()),
        "imported_code_sha256": imported_code_sha,
        "static_check_path": str(static_check_path.as_posix()),
        "static_check_sha256": static_sha,
        "package_manifest_path": str(package_manifest_path.as_posix()),
        "review_required": True,
        "official_output_created": False,
        "not_production": True,
    }
    manifest_path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")

    warnings = [
        "Imported external Codex code must be reviewed before execution.",
        "Static checks are generic guardrails and do not prove clinical derivation correctness.",
    ]
    risk_points = [
        "R code was authored outside Studio and imported as review-required evidence.",
    ]
    used_inputs = [str(staging_root.as_posix())]
    expected_outputs = [f"outputs/{target.lower()}.csv"]
    decisions, node_inputs, node_outputs = _agent_io(
        request=request,
        target=target,
        code_path=imported_code_path,
        static_check_path=static_check_path,
        package_manifest_path=package_manifest_path,
        import_manifest_path=manifest_path,
    )

    return ExternalCodePackageImportResult(
        dataset=target,
        code_path=str(imported_code_path.as_posix()),
        code_sha256=imported_code_sha,
        static_check_path=str(static_check_path.as_posix()),
        static_check_sha256=static_sha,
        manifest_path=str(manifest_path.as_posix()),
        package_manifest_path=str(package_manifest_path.as_posix()),
        assumptions=assumptions,
        risk_points=risk_points,
        used_inputs=used_inputs,
        expected_outputs=expected_outputs,
        warnings=warnings,
        agent_decisions=decisions,
        agent_node_inputs=node_inputs,
        agent_node_outputs=node_outputs,
        risk_flags=["external_codex_code_review_required", "static_check_limited_scope"],
        code_agent_metadata={
            "source": "external_codex_package",
            "package_id": package_id,
            "codex_thread_id": request.codex_thread_id,
            "package_manifest_path": str(package_manifest_path.as_posix()),
            "import_manifest_path": str(manifest_path.as_posix()),
            "official_output_created": False,
            "review_required": True,
            "not_production": True,
        },
    )


def _resolve_source_code(staging_root: Path, target: str, requested_code_path: str | None) -> Path:
    if requested_code_path:
        candidate = Path(requested_code_path)
        if not candidate.is_absolute():
            candidate = staging_root / candidate
        candidate = candidate.resolve(strict=False)
        if not candidate.exists() or not candidate.is_file():
            raise ExternalCodePackageError(f"Requested R code file does not exist: {candidate}")
        _assert_under(candidate, staging_root)
        return candidate

    candidates = [
        staging_root / "R" / f"{target.lower()}.R",
        staging_root / "R" / f"{target.upper()}.R",
        staging_root / "R" / f"build_{target.lower()}.R",
        staging_root / "R" / f"{target.lower()}_build.R",
        staging_root / "R" / f"20_{target.lower()}.R",
        staging_root / "R" / f"10_{target.lower()}.R",
    ]
    pattern = re.compile(rf"(^|[_-]){re.escape(target.lower())}([_.-]|$)")
    if (staging_root / "R").exists():
        candidates.extend(
            path
            for path in sorted((staging_root / "R").glob("*.R"))
            if pattern.search(path.name.lower())
        )
    for candidate in candidates:
        if candidate.exists() and candidate.is_file():
            _assert_under(candidate.resolve(strict=False), staging_root)
            return candidate.resolve(strict=False)
    raise ExternalCodePackageError(f"No R script for {target} was found under {staging_root / 'R'}.")


def _relative_to_root(path: Path, root: Path) -> str:
    _assert_under(path, root)
    return str(path.resolve(strict=False).relative_to(root.resolve(strict=False)).as_posix())


def _assert_under(path: Path, root: Path) -> None:
    try:
        path.resolve(strict=False).relative_to(root.resolve(strict=False))
    except ValueError as exc:
        raise ExternalCodePackageError(f"Path escapes Codex package staging_root: {path}") from exc


def _read_assumptions(staging_root: Path, assumptions_path: str | None) -> list[str]:
    if not assumptions_path:
        return []
    path = staging_root / assumptions_path
    if not path.exists() or not path.is_file():
        return []
    try:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return []
    assumptions = []
    for line in lines:
        cleaned = line.strip().lstrip("-*").strip()
        if cleaned:
            assumptions.append(cleaned)
    return assumptions[:50]


def _agent_io(
    *,
    request: ExternalCodePackageImportRequest,
    target: str,
    code_path: Path,
    static_check_path: Path,
    package_manifest_path: Path,
    import_manifest_path: Path,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    output_payload = {
        "source": "external_codex_package",
        "code_path": str(code_path.as_posix()),
        "static_check_path": str(static_check_path.as_posix()),
        "package_manifest_path": str(package_manifest_path.as_posix()),
        "import_manifest_path": str(import_manifest_path.as_posix()),
        "next_action": "review_code",
    }
    decision = record_agent_decision(
        agent="code_agent",
        node="external_codex_code_import",
        decision="external_r_code_imported",
        dataset=target,
        status="needs_review",
        reason="External Codex-authored R code was imported and routed to Studio code review.",
        outputs=output_payload,
        risk_flags=["external_codex_code_review_required"],
        artifact_ids=[f"external_codex_code_{target.lower()}", f"external_codex_manifest_{target.lower()}"],
    )
    node_input = build_agent_node_input(
        agent="code_agent",
        node="external_codex_code_import",
        study_id=request.study_id,
        run_id=request.run_id,
        dataset=target,
        task="Import one external Codex-authored R script as review-required code evidence.",
        inputs={
            "staging_root": str(Path(request.staging_root).expanduser().as_posix()),
            "code_path": request.code_path,
            "codex_thread_id": request.codex_thread_id,
        },
        risk_flags=["external_codex_code_review_required"],
        artifact_ids=[],
    )
    node_output = build_agent_node_output(
        agent="code_agent",
        node="external_codex_code_import",
        study_id=request.study_id,
        run_id=request.run_id,
        dataset=target,
        status="needs_review",
        decision="external_r_code_imported",
        reason=decision["reason"],
        outputs=output_payload,
        risk_flags=["external_codex_code_review_required"],
        artifact_ids=[f"external_codex_code_{target.lower()}", f"external_codex_manifest_{target.lower()}"],
        agent_decisions=[decision],
    )
    return [decision], [node_input], [node_output]
