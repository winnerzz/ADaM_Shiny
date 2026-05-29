"""Graph-owned execution boundary for approved generated R code."""

from __future__ import annotations

from dataclasses import dataclass, field
import json
from pathlib import Path
from typing import Any

from adam_agent.downstream.diagnostics import diagnose_downstream_failure, write_downstream_failure_report
from adam_agent.graph.workflow_state import compare_fingerprints, input_fingerprint
from adam_agent.schemas.graph_state import StudyRunState
from adam_agent.schemas.artifacts import ArtifactKind, ArtifactRef, ArtifactRole
from adam_agent.schemas.routing import FailureRecord
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.r_runner import LocalRRunner, RRunRequest


class GraphExecutionError(RuntimeError):
    """Raised when approved-code execution cannot safely start."""


@dataclass(frozen=True)
class ApprovedCodeExecutionResult:
    """Result of running one approved generated R script."""

    study_id: str
    run_id: str
    dataset: str
    response_status: str
    validation_status: str
    output_path: str | None
    validation_report_path: str
    diagnostics_path: str | None
    terminal_failure: bool
    errors: list[str]
    warnings: list[str]
    validation_report: dict[str, Any]
    failure_records: list[FailureRecord] = field(default_factory=list)
    artifacts: dict[str, ArtifactRef] = field(default_factory=dict)


def execute_approved_r_code(
    *,
    study_dir: str | Path,
    study_id: str,
    run_id: str,
    dataset: str,
    rscript_path: str | None = None,
) -> ApprovedCodeExecutionResult:
    """Run approved generated R code and write validation/diagnostic artifacts."""

    root = Path(study_dir).expanduser()
    target = dataset.strip().upper()
    run_dir = root / "runs" / run_id
    code_path = run_dir / "code" / f"build_{target.lower()}.R"
    output_path = run_dir / "outputs" / f"{target.lower()}.csv"
    review_path = run_dir / "review" / f"{target.lower()}_code_review.json"
    if not code_path.exists() or not code_path.is_file():
        raise GraphExecutionError(f"Generated R code does not exist: {code_path}")
    assert_code_review_current(root, run_id, target, review_path, code_path)
    assert_graph_code_review_current(root, run_id, target, review_path, code_path)
    if output_path.exists():
        output_path.unlink()

    runner = LocalRRunner(rscript_path)
    r_result = runner.run(
        RRunRequest(
            code="",
            dataset=target,
            run_id=run_id,
            working_dir=str(run_dir),
            script_path=str(code_path),
        )
    )
    validation_report = validate_executed_code(
        target=target,
        output_path=output_path,
        r_exit_code=r_result.exit_code,
        r_stdout=r_result.stdout,
        r_stderr=r_result.stderr,
    )
    validation_path = run_dir / "validation" / f"{target.lower()}_validation_report.json"
    _write_json(validation_path, validation_report)

    failure_records: list[FailureRecord] = []
    diagnostics_artifact: ArtifactRef | None = None
    terminal_failure = validation_report["status"] == "fail"
    usable_output_path = output_path if not terminal_failure and output_path.exists() else None
    if terminal_failure:
        failure = diagnose_downstream_failure(
            dataset=target,
            stage="r_sandbox" if r_result.exit_code != 0 else "validation",
            message="; ".join(validation_report["errors"]),
            r_result=r_result,
            validation_report=validation_report,
            artifact_ids=[],
            repair_attempt=0,
        )
        failure_records.append(failure)
        diagnostics_artifact = write_downstream_failure_report(
            study_dir=root,
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            failure_records=failure_records,
            validation_report=validation_report,
            status="terminal_failure",
        )

    artifacts = {
        "validation_report": _artifact_ref(
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            kind_id="validation_report",
            kind="validation_report",
            role="output",
            path=validation_path,
        )
    }
    if usable_output_path is not None:
        artifacts["output_adam"] = _artifact_ref(
            study_id=study_id,
            run_id=run_id,
            dataset=target,
            kind_id="output_adam",
            kind="output_adam",
            role="output",
            path=usable_output_path,
        )
    if diagnostics_artifact is not None:
        artifacts["failure_report"] = diagnostics_artifact

    return ApprovedCodeExecutionResult(
        study_id=study_id,
        run_id=run_id,
        dataset=target,
        response_status="completed" if validation_report["status"] == "pass" else "terminal_failure",
        validation_status=validation_report["status"],
        output_path=str(usable_output_path.as_posix()) if usable_output_path else None,
        validation_report_path=str(validation_path.as_posix()),
        diagnostics_path=str(Path(diagnostics_artifact.path).as_posix()) if diagnostics_artifact else None,
        terminal_failure=terminal_failure,
        errors=list(validation_report["errors"]),
        warnings=list(validation_report["warnings"]),
        validation_report=validation_report,
        failure_records=failure_records,
        artifacts=artifacts,
    )


def assert_code_review_current(
    study_dir: Path,
    run_id: str,
    target: str,
    review_path: Path,
    code_path: Path,
) -> None:
    """Fail closed unless code approval matches current inputs, code, and spec."""

    payload = _read_json_if_exists(review_path)
    if payload.get("decision") != "approve" or payload.get("approved") is not True:
        raise GraphExecutionError(f"Generated code must be approved before sandbox execution: {review_path}")
    approved_fingerprint = payload.get("input_fingerprint")
    current_fingerprint = input_fingerprint(study_dir)
    if not approved_fingerprint or not approved_fingerprint.get("digest"):
        raise GraphExecutionError("Code approval is missing its input fingerprint. Review the generated code again.")
    if approved_fingerprint.get("digest") != current_fingerprint.get("digest"):
        diff = compare_fingerprints(approved_fingerprint, current_fingerprint)
        raise GraphExecutionError(
            "Code approval is stale because study inputs changed after review. "
            f"Input diff: added={diff.get('added', [])}; removed={diff.get('removed', [])}; "
            f"changed={diff.get('changed_files', [])}."
        )
    approved_code_sha = payload.get("code_sha256")
    if not approved_code_sha:
        raise GraphExecutionError("Code approval is missing the generated code hash. Review the generated code again.")
    current_code_sha = f"sha256:{sha256_file(code_path)}"
    if approved_code_sha != current_code_sha:
        raise GraphExecutionError("Generated code changed after approval. Review the generated code again.")

    static_check_path = Path(str(payload.get("static_check_path") or ""))
    approved_static_sha = payload.get("static_check_sha256")
    if static_check_path.exists() and approved_static_sha:
        current_static_sha = f"sha256:{sha256_file(static_check_path)}"
        if approved_static_sha != current_static_sha:
            raise GraphExecutionError("Static-check artifact changed after approval. Review the generated code again.")

    spec_path = payload.get("spec_path")
    spec_sha = payload.get("spec_sha256")
    if spec_path or spec_sha:
        if not spec_path or not spec_sha:
            raise GraphExecutionError("Code approval is missing the approved spec path or hash. Review the generated code again.")
        resolved_spec = Path(str(spec_path))
        if not resolved_spec.exists() or not resolved_spec.is_file():
            raise GraphExecutionError(f"Approved spec used for code generation no longer exists: {resolved_spec}")
        current_spec_sha = f"sha256:{sha256_file(resolved_spec)}"
        if current_spec_sha != spec_sha:
            raise GraphExecutionError("Approved spec changed after code approval. Regenerate and review the code again.")


def assert_graph_code_review_current(
    study_dir: Path,
    run_id: str,
    target: str,
    review_path: Path,
    code_path: Path,
) -> None:
    """Require canonical graph state to approve the exact code being executed."""

    graph_state_path = study_dir / "runs" / run_id / "graph_state.json"
    if not graph_state_path.exists() or not graph_state_path.is_file():
        raise GraphExecutionError("Graph state is missing. Regenerate and approve code through the graph flow before execution.")
    try:
        graph_state = StudyRunState.model_validate_json(graph_state_path.read_text(encoding="utf-8"))
    except Exception as exc:  # noqa: BLE001 - convert malformed durable state to a fail-closed boundary error.
        raise GraphExecutionError(f"Graph state cannot be read for execution approval: {exc}") from exc
    dataset_state = graph_state.datasets.get(target.strip().upper())
    if dataset_state is None:
        raise GraphExecutionError(f"Graph state has no dataset state for {target}. Regenerate and approve code before execution.")
    code_state = dataset_state.code_state
    if code_state.get("status") != "approved" or code_state.get("decision") != "approve":
        raise GraphExecutionError("Graph state does not contain an approved code-review decision for this dataset.")
    current_fingerprint = input_fingerprint(study_dir)
    graph_fingerprint = code_state.get("input_fingerprint") or {}
    if not graph_fingerprint.get("digest"):
        raise GraphExecutionError("Graph code approval is missing its input fingerprint. Review the generated code again.")
    if graph_fingerprint.get("digest") != current_fingerprint.get("digest"):
        diff = compare_fingerprints(graph_fingerprint, current_fingerprint)
        raise GraphExecutionError(
            "Graph code approval is stale because study inputs changed after review. "
            f"Input diff: added={diff.get('added', [])}; removed={diff.get('removed', [])}; "
            f"changed={diff.get('changed_files', [])}."
        )
    graph_review_path = Path(str(code_state.get("review_path") or ""))
    if str(graph_review_path.as_posix()) != str(review_path.as_posix()):
        raise GraphExecutionError("Graph code approval points to a different review artifact. Review the generated code again.")
    graph_code_sha = code_state.get("code_sha256")
    if not graph_code_sha:
        raise GraphExecutionError("Graph code approval is missing the generated code hash. Review the generated code again.")
    if graph_code_sha != f"sha256:{sha256_file(code_path)}":
        raise GraphExecutionError("Generated code changed after graph approval. Review the generated code again.")
    static_check_path = Path(str(code_state.get("static_check_path") or ""))
    graph_static_sha = code_state.get("static_check_sha256")
    if static_check_path.exists() and graph_static_sha:
        if graph_static_sha != f"sha256:{sha256_file(static_check_path)}":
            raise GraphExecutionError("Static-check artifact changed after graph approval. Review the generated code again.")
    graph_spec_path = code_state.get("spec_path")
    graph_spec_sha = code_state.get("spec_sha256")
    if graph_spec_path or graph_spec_sha:
        if not graph_spec_path or not graph_spec_sha:
            raise GraphExecutionError("Graph code approval is missing the approved spec path or hash. Review the generated code again.")
        resolved_spec = Path(str(graph_spec_path))
        if not resolved_spec.exists() or not resolved_spec.is_file():
            raise GraphExecutionError(f"Approved spec used for graph code generation no longer exists: {resolved_spec}")
        if f"sha256:{sha256_file(resolved_spec)}" != graph_spec_sha:
            raise GraphExecutionError("Approved spec changed after graph approval. Regenerate and review the code again.")
    _assert_dependency_artifacts_current(code_state.get("dependency_artifacts") or [])


def validate_executed_code(
    *,
    target: str,
    output_path: Path,
    r_exit_code: int,
    r_stdout: str,
    r_stderr: str,
) -> dict[str, Any]:
    """Validate the execution boundary without claiming CDISC compliance."""

    errors: list[str] = []
    checks = [
        {"name": "r_exit_code_zero", "pass": r_exit_code == 0},
        {"name": "output_file_exists", "pass": output_path.exists() and output_path.is_file()},
    ]
    if r_exit_code != 0:
        errors.append(r_stderr or "R execution failed.")
    if not output_path.exists():
        errors.append(f"Expected output file was not written: {output_path.as_posix()}")
    terminal_failure = bool(errors)
    return {
        "dataset": target,
        "status": "fail" if errors else "pass",
        "checks": checks,
        "warnings": [],
        "errors": errors,
        "output_path": str(output_path.as_posix()),
        "r_exit_code": r_exit_code,
        "r_stdout": r_stdout,
        "r_stderr": r_stderr,
        "stubbed_r_execution": False,
        "not_real_derivation": False,
        "terminal_failure": terminal_failure,
        "partial_output_usable": not terminal_failure,
    }


def _assert_dependency_artifacts_current(records: list[Any]) -> None:
    for record in records:
        if not isinstance(record, dict):
            continue
        path = record.get("artifact_path")
        expected_sha = record.get("artifact_sha256")
        if not path or not expected_sha:
            continue
        artifact_path = Path(str(path))
        if not artifact_path.exists() or not artifact_path.is_file():
            raise GraphExecutionError(f"Dependency artifact used for code generation no longer exists: {artifact_path}")
        if f"sha256:{sha256_file(artifact_path)}" != expected_sha:
            raise GraphExecutionError("Dependency artifact changed after graph approval. Regenerate and review the code again.")


def _artifact_ref(
    *,
    study_id: str,
    run_id: str,
    dataset: str,
    kind_id: str,
    kind: ArtifactKind,
    role: ArtifactRole,
    path: Path,
) -> ArtifactRef:
    return ArtifactRef(
        artifact_id=f"{kind_id}_{study_id.lower()}_{run_id}_{dataset.lower()}",
        kind=kind,
        path=str(path.as_posix()),
        sha256=f"sha256:{sha256_file(path)}",
        dataset=dataset,
        format=path.suffix.lower().lstrip(".") or "txt",
        role=role,
        metadata={"graph_execution": True},
    )


def _read_json_if_exists(path: Path) -> dict[str, Any]:
    if not path.exists() or not path.is_file():
        return {}
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, UnicodeDecodeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")
