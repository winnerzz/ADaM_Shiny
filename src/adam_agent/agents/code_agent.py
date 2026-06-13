"""Code Agent V1: build a reviewable R code package before official execution."""

from __future__ import annotations

import json
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Protocol

from adam_agent.llm.clients import LLMProviderConfig, MockLLMClient, build_llm_client
from adam_agent.llm.generated_code import (
    GeneratedCodePackage,
    LLMGeneratedCodeError,
    parse_generated_code_response,
    write_generated_code_artifacts,
)
from adam_agent.llm.mock_code import default_mock_generated_code_response
from adam_agent.llm.prompt_compaction import compact_prompt_from_context, write_compact_prompt_artifact
from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.schemas.code_agent import (
    CodeAgentAttempt,
    CodeAgentFailureClassification,
    CodeAgentResult,
    CodeAgentTask,
)
from adam_agent.schemas.llm import LLMExposureConfig
from adam_agent.tools.artifacts import sha256_file
from adam_agent.tools.r_runner import RRunRequest, RRunResult
from adam_agent.tools.sandbox import LocalRscriptSandboxRunner, SandboxRunner
from adam_agent.tools.static_rules import (
    StaticRuleError,
    assert_no_blocking_static_findings,
    run_generated_r_static_checks,
    write_static_rule_report,
)


LLMClientBuilder = Callable[[LLMProviderConfig], Any]


class SandboxRunnerFactory(Protocol):
    """Build a sandbox runner for one Code Agent attempt directory."""

    def __call__(
        self,
        *,
        run_dir: Path,
        rscript_path: str | None,
        allowed_output_paths: list[Path],
    ) -> SandboxRunner:
        ...


@dataclass(frozen=True)
class _AttemptDraft:
    attempt_index: int
    response_text: str
    package: GeneratedCodePackage
    response_path: Path
    parsed_response_path: Path
    attempt_code_path: Path
    final_artifacts: Any
    call_record: Any


def build_code_package(
    task: CodeAgentTask,
    *,
    llm_client_builder: LLMClientBuilder = build_llm_client,
    sandbox_runner_factory: SandboxRunnerFactory | None = None,
) -> CodeAgentResult:
    """Generate, check, trial-run, and package R code for human review.

    The returned code is review evidence only. Official ADaM output remains
    owned by the approved-code execution step.
    """

    root = Path(task.study_dir).expanduser()
    run_dir = root / "runs" / task.run_id
    target = task.dataset
    target_lower = target.lower()
    agent_dir = run_dir / "code_agent" / target_lower
    trial_root = run_dir / "_ca" / target_lower
    agent_dir.mkdir(parents=True, exist_ok=True)
    trial_root.mkdir(parents=True, exist_ok=True)

    provider_config = LLMProviderConfig(**task.llm_provider)
    exposure = LLMExposureConfig.model_validate(task.llm_exposure)
    llm_client = llm_client_builder(provider_config)
    if provider_config.provider.strip().lower() == "mock":
        llm_client = MockLLMClient(fixed_response_text=default_mock_generated_code_response(target))

    compact_prompt = compact_prompt_from_context(task.context)
    prompt_artifact = write_compact_prompt_artifact(
        study_id=task.study_id,
        run_id=task.run_id,
        target_dataset=target,
        study_dir=root,
        prompt=compact_prompt,
        source_context_artifact_id=task.context_artifact_id,
    )

    attempts: list[CodeAgentAttempt] = []
    final_draft: _AttemptDraft | None = None
    failure_classification: CodeAgentFailureClassification | None = None
    repair_reason = ""
    last_errors: list[str] = []
    last_warnings: list[str] = []
    last_runtime_report_path: Path | None = None
    last_trial_output_path: Path | None = None
    trial_run_status = "not_run"

    for attempt_index in range(1, task.max_attempts + 1):
        attempt_dir = trial_root / f"a{attempt_index:02d}"
        attempt_dir.mkdir(parents=True, exist_ok=True)
        draft = _draft_attempt(
            task=task,
            attempt_index=attempt_index,
            attempt_dir=attempt_dir,
            compact_prompt=compact_prompt,
            prompt_artifact_id=prompt_artifact.artifact_id,
            llm_client=llm_client,
            provider_config=provider_config,
            exposure=exposure,
            repair_reason=repair_reason,
        )
        final_draft = draft

        static_path = attempt_dir / "static_check.json"
        try:
            static_report = run_generated_r_static_checks(
                study_id=task.study_id,
                run_id=task.run_id,
                dataset=target,
                code_path=draft.attempt_code_path,
                expected_output_path=f"outputs/{target_lower}.csv",
                required_identifiers=task.required_identifiers,
                required_identifier_source_id=task.required_identifier_source_id,
                rscript_path=task.rscript_path,
            )
            write_static_rule_report(static_report, path=static_path)
            assert_no_blocking_static_findings(static_report)
        except StaticRuleError as exc:
            classification = _classify_failure(
                stage="static_check",
                message=str(exc),
                exit_code=None,
                attempts_remaining=attempt_index < task.max_attempts,
            )
            attempts.append(
                CodeAgentAttempt(
                    attempt=attempt_index,
                    status="static_failed" if classification.repairable else "blocked",
                    code_path=str(draft.attempt_code_path.as_posix()),
                    response_path=str(draft.response_path.as_posix()),
                    parsed_response_path=str(draft.parsed_response_path.as_posix()),
                    static_check_path=str(static_path.as_posix()) if static_path.exists() else None,
                    errors=[str(exc)],
                    repair_reason=repair_reason,
                    failure_classification=classification,
                )
            )
            failure_classification = classification
            last_errors = [str(exc)]
            if not classification.repairable:
                break
            repair_reason = _repair_reason_from_failure(classification, last_errors, [])
            continue

        runtime_report_path, trial_output_path, r_result = _trial_run_attempt(
            task=task,
            attempt_dir=attempt_dir,
            code_path=draft.attempt_code_path,
            sandbox_runner_factory=sandbox_runner_factory or _default_sandbox_runner,
        )
        last_runtime_report_path = runtime_report_path
        last_trial_output_path = trial_output_path
        runtime_errors, runtime_warnings, trial_run_status = _runtime_messages(
            r_result,
            trial_output_path=trial_output_path,
        )
        if r_result.success and trial_output_path.exists():
            attempts.append(
                CodeAgentAttempt(
                    attempt=attempt_index,
                    status="trial_passed",
                    code_path=str(draft.attempt_code_path.as_posix()),
                    response_path=str(draft.response_path.as_posix()),
                    parsed_response_path=str(draft.parsed_response_path.as_posix()),
                    static_check_path=str(static_path.as_posix()),
                    runtime_report_path=str(runtime_report_path.as_posix()),
                    trial_output_path=str(trial_output_path.as_posix()),
                    static_status=static_report.status,
                    runtime_status="pass",
                    exit_code=r_result.exit_code,
                    warnings=runtime_warnings,
                    repair_reason=repair_reason,
                )
            )
            failure_classification = None
            last_errors = []
            last_warnings = runtime_warnings
            break

        classification = _classify_failure(
            stage="runtime",
            message="; ".join(runtime_errors + runtime_warnings),
            exit_code=r_result.exit_code,
            attempts_remaining=attempt_index < task.max_attempts,
        )
        attempts.append(
            CodeAgentAttempt(
                attempt=attempt_index,
                status=_attempt_status_from_runtime_failure(classification),
                code_path=str(draft.attempt_code_path.as_posix()),
                response_path=str(draft.response_path.as_posix()),
                parsed_response_path=str(draft.parsed_response_path.as_posix()),
                static_check_path=str(static_path.as_posix()),
                runtime_report_path=str(runtime_report_path.as_posix()),
                trial_output_path=str(trial_output_path.as_posix()) if trial_output_path.exists() else None,
                static_status=static_report.status,
                runtime_status="fail",
                exit_code=r_result.exit_code,
                errors=runtime_errors,
                warnings=runtime_warnings,
                repair_reason=repair_reason,
                failure_classification=classification,
            )
        )
        failure_classification = classification
        last_errors = runtime_errors
        last_warnings = runtime_warnings
        if not classification.repairable:
            break
        repair_reason = _repair_reason_from_failure(classification, runtime_errors, runtime_warnings)

    if final_draft is None:
        raise LLMGeneratedCodeError("Code Agent did not produce any draft code.")

    final_attempt = attempts[-1] if attempts else None
    review_ready = bool(final_attempt and final_attempt.static_check_path)
    if final_attempt and final_attempt.status == "blocked":
        review_ready = False
    final_static_path = _copy_final_static_check(
        task=task,
        attempt=final_attempt,
        run_dir=run_dir,
        final_code_path=Path(final_draft.final_artifacts.code_artifact.path),
        require_nonblocking=review_ready,
    )
    review_package_path = _write_review_package(
        task=task,
        agent_dir=agent_dir,
        prompt_artifact=prompt_artifact,
        final_draft=final_draft,
        final_static_path=final_static_path,
        attempts=attempts,
        failure_classification=failure_classification,
        trial_run_status=trial_run_status,
        trial_runtime_report_path=last_runtime_report_path,
        trial_output_path=last_trial_output_path if last_trial_output_path and last_trial_output_path.exists() else None,
    )
    review_markdown_path = _write_review_markdown(
        task=task,
        agent_dir=agent_dir,
        final_draft=final_draft,
        attempts=attempts,
        trial_run_status=trial_run_status,
        failure_classification=failure_classification,
    )

    call_record = final_draft.call_record
    llm_provider = _llm_call_field(call_record, "provider", provider_config.provider)
    llm_model = _llm_call_field(call_record, "model", provider_config.model)
    provider_alias = _llm_call_field(call_record, "provider_alias", None)
    transport = _llm_call_field(call_record, "transport", None)
    provider_base_url = _llm_call_field(call_record, "provider_base_url", None)

    risk_points = list(final_draft.package.risk_points)
    if not review_ready:
        risk_points.append("Code Agent package is blocked and should not be approved without resolving the failure.")
    elif trial_run_status != "pass":
        risk_points.append("Trial run did not prove runtime success; review package before approval.")
    if last_errors:
        risk_points.extend(f"Code Agent failure evidence: {item}" for item in last_errors[:3])

    package_artifact = _artifact_ref(
        task=task,
        kind_id="code_agent_package",
        kind="tool_log",
        path=review_package_path,
        role="audit",
        format="json",
        metadata={
            "code_agent": True,
            "attempt_count": len(attempts),
            "trial_run_status": trial_run_status,
        },
    )
    review_artifact = _artifact_ref(
        task=task,
        kind_id="code_agent_review",
        kind="tool_log",
        path=review_markdown_path,
        role="audit",
        format="md",
        metadata={"code_agent_review": True},
    )

    return CodeAgentResult(
        status="ready_for_review" if review_ready else "blocked",
        ready_for_human_review=review_ready,
        final_code_path=final_draft.final_artifacts.code_artifact.path,
        final_code=final_draft.package.r_code,
        static_check_path=str(final_static_path.as_posix()),
        response_path=final_draft.final_artifacts.response_artifact.path,
        parsed_response_path=final_draft.final_artifacts.package_artifact.path,
        prompt_path=prompt_artifact.path,
        review_package_path=str(review_package_path.as_posix()),
        review_markdown_path=str(review_markdown_path.as_posix()),
        attempts=attempts,
        assumptions=list(final_draft.package.assumptions),
        risk_points=risk_points,
        used_inputs=list(final_draft.package.used_inputs),
        expected_outputs=list(final_draft.package.expected_outputs),
        trial_run_status=trial_run_status,
        trial_runtime_report_path=str(last_runtime_report_path.as_posix()) if last_runtime_report_path else None,
        trial_output_path=str(last_trial_output_path.as_posix()) if last_trial_output_path and last_trial_output_path.exists() else None,
        failure_classification=failure_classification,
        llm_provider=str(llm_provider) if llm_provider is not None else None,
        llm_model=str(llm_model) if llm_model is not None else None,
        provider_alias=str(provider_alias) if provider_alias is not None else None,
        transport=str(transport) if transport is not None else None,
        provider_base_url=str(provider_base_url) if provider_base_url is not None else None,
        not_real_derivation=_not_real_generation(
            provider_config,
            provider=llm_provider,
            provider_alias=provider_alias,
            transport=transport,
        ),
        artifact_refs=[
            prompt_artifact,
            final_draft.final_artifacts.response_artifact,
            final_draft.final_artifacts.package_artifact,
            final_draft.final_artifacts.code_artifact,
            _artifact_ref(
                task=task,
                kind_id="code_agent_static_check",
                kind="tool_log",
                path=final_static_path,
                role="audit",
                format="json",
                metadata={"static_check": True, "source": "code_agent"},
            ),
            package_artifact,
            review_artifact,
        ],
    )


def _draft_attempt(
    *,
    task: CodeAgentTask,
    attempt_index: int,
    attempt_dir: Path,
    compact_prompt: str,
    prompt_artifact_id: str,
    llm_client: Any,
    provider_config: LLMProviderConfig,
    exposure: LLMExposureConfig,
    repair_reason: str,
) -> _AttemptDraft:
    prompt = compact_prompt
    if repair_reason:
        prompt = (
            f"{compact_prompt.rstrip()}\n\n"
            "## Code Agent repair request\n"
            f"{repair_reason}\n"
            "Return the same strict JSON contract with corrected R code.\n"
        )
        (attempt_dir / "repair_prompt.md").write_text(prompt, encoding="utf-8")
    response = llm_client.generate(
        _llm_request_for_code_generation(
            prompt=prompt,
            target=task.dataset,
            study_id=task.study_id,
            run_id=task.run_id,
            provider_config=provider_config,
            exposure=exposure,
            context_dict=task.context,
            prompt_artifact_id=prompt_artifact_id,
            attempt_index=attempt_index,
        )
    )
    package = parse_generated_code_response(response.response_text, expected_dataset=task.dataset)
    response_path = attempt_dir / "response.json"
    parsed_path = attempt_dir / "parsed_response.json"
    attempt_code_path = attempt_dir / "code.R"
    response_path.write_text(response.response_text, encoding="utf-8")
    parsed_path.write_text(json.dumps(package.as_dict(), indent=2, sort_keys=True), encoding="utf-8")
    attempt_code_path.write_text(package.r_code, encoding="utf-8")
    final_artifacts = write_generated_code_artifacts(
        study_id=task.study_id,
        run_id=task.run_id,
        study_dir=task.study_dir,
        package=package,
        response_text=response.response_text,
    )
    return _AttemptDraft(
        attempt_index=attempt_index,
        response_text=response.response_text,
        package=package,
        response_path=response_path,
        parsed_response_path=parsed_path,
        attempt_code_path=attempt_code_path,
        final_artifacts=final_artifacts,
        call_record=response.call_record,
    )


def _llm_request_for_code_generation(
    *,
    prompt: str,
    target: str,
    study_id: str,
    run_id: str,
    provider_config: LLMProviderConfig,
    exposure: LLMExposureConfig,
    context_dict: dict[str, Any],
    prompt_artifact_id: str,
    attempt_index: int,
):
    from adam_agent.llm.clients import LLMRequest
    from adam_agent.llm.prompt_compaction import MAX_SAMPLE_ROWS_IN_PROMPT

    return LLMRequest(
        prompt=prompt,
        system_prompt=(
            "You are the Code Agent for ADaM dataset creation. Return only JSON with keys "
            "dataset, r_code, assumptions, risk_points, used_inputs, expected_outputs. "
            "Do not include markdown fences. Do not use network, shell, install.packages, "
            "or filesystem writes outside the runtime output path."
        ),
        provider=provider_config.provider,
        model=provider_config.model,
        exposure=exposure,
        node="code_agent_build_code_package",
        call_id=f"llm_{run_id}_{target.lower()}_code_agent_attempt_{attempt_index:02d}",
        max_tokens=provider_config.max_tokens,
        datasets_included=_datasets_included(context_dict),
        variables_included=_variables_included(context_dict),
        sample_row_counts=_sample_row_counts(context_dict, max_rows=MAX_SAMPLE_ROWS_IN_PROMPT),
        subject_level_data_included=bool(_sample_row_counts(context_dict, max_rows=MAX_SAMPLE_ROWS_IN_PROMPT)),
        prompt_artifact_id=prompt_artifact_id,
        response_artifact_id=f"llm_response_{study_id.lower()}_{run_id}_{target.lower()}_code_agent_attempt_{attempt_index:02d}",
        redaction_policy="code_agent_v1_code_generation_review_policy",
    )


def _trial_run_attempt(
    *,
    task: CodeAgentTask,
    attempt_dir: Path,
    code_path: Path,
    sandbox_runner_factory: SandboxRunnerFactory,
) -> tuple[Path, Path, RRunResult]:
    _prepare_trial_workspace(Path(task.study_dir), attempt_dir)
    output_path = attempt_dir / "outputs" / f"{task.dataset.lower()}.csv"
    output_path.parent.mkdir(parents=True, exist_ok=True)
    if output_path.exists():
        output_path.unlink()
    trial_script = attempt_dir / f"build_{task.dataset.lower()}.R"
    shutil.copy2(code_path, trial_script)
    runner = sandbox_runner_factory(
        run_dir=attempt_dir,
        rscript_path=task.rscript_path,
        allowed_output_paths=[output_path],
    )
    boundary = runner.boundary().as_dict() if hasattr(runner, "boundary") else {
        "backend_name": getattr(runner, "backend_name", runner.__class__.__name__),
        "hardened": bool(getattr(runner, "hardened", False)),
        "run_dir": str(attempt_dir.as_posix()),
        "network_disabled": False,
        "notes": ["Sandbox runner does not expose detailed boundary metadata."],
    }
    result = runner.run(
        RRunRequest(
            code="",
            dataset=task.dataset,
            run_id=task.run_id,
            working_dir=str(attempt_dir),
            script_path=str(trial_script),
        )
    )
    runtime_report_path = attempt_dir / "runtime_report.json"
    runtime_report_path.write_text(
        json.dumps(
            {
                "study_id": task.study_id,
                "run_id": task.run_id,
                "dataset": task.dataset,
                "trial_run": True,
                "official_output": False,
                "exit_code": result.exit_code,
                "success": result.success,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "trial_output_path": str(output_path.as_posix()) if output_path.exists() else None,
                "expected_official_output_path": str(
                    (Path(task.study_dir) / "runs" / task.run_id / "outputs" / f"{task.dataset.lower()}.csv").as_posix()
                ),
                "sandbox": boundary,
            },
            indent=2,
            sort_keys=True,
        ),
        encoding="utf-8",
    )
    return runtime_report_path, output_path, result


def _prepare_trial_workspace(study_dir: Path, attempt_dir: Path) -> None:
    shared_relative_root = _trial_prompt_relative_root(attempt_dir)
    for folder_name in ["input_sdtm", "input_define", "input_spec", "legacy_code", "reference_adam"]:
        source = study_dir / folder_name
        if not source.exists() or not source.is_dir():
            continue
        _copy_trial_input_folder(source=source, target=attempt_dir / folder_name, sandbox_root=attempt_dir)
        _copy_trial_input_folder(
            source=source,
            target=shared_relative_root / folder_name,
            sandbox_root=shared_relative_root,
        )

    run_outputs = study_dir / "runs" / _run_id_from_attempt_dir(attempt_dir) / "outputs"
    if run_outputs.exists() and run_outputs.is_dir():
        dependency_target = attempt_dir / "outputs"
        dependency_target.mkdir(parents=True, exist_ok=True)
        for item in run_outputs.iterdir():
            if item.is_file() and not (dependency_target / item.name).exists():
                shutil.copy2(item, dependency_target / item.name)


def _trial_prompt_relative_root(attempt_dir: Path) -> Path:
    """Return the root used by generated code that follows the official run-dir contract.

    Official R code runs from runs/{run_id}; Code Agent trial code runs from
    runs/{run_id}/_ca/{dataset}/a01. Prompts may therefore contain paths such as
    ../../input_sdtm/dm.csv, which resolve to runs/{run_id}/_ca/input_sdtm
    during a trial run.
    """

    if len(attempt_dir.parents) < 2:
        return attempt_dir
    return attempt_dir.parents[1]


def _copy_trial_input_folder(*, source: Path, target: Path, sandbox_root: Path) -> None:
    sandbox_root = sandbox_root.resolve()
    target = target.resolve()
    if not target.is_relative_to(sandbox_root):
        raise ValueError(f"Refusing to prepare trial input outside sandbox root: {target}")
    if target == sandbox_root:
        raise ValueError(f"Refusing to replace trial sandbox root: {target}")
    if target.exists():
        if target.is_dir() and not target.is_symlink():
            shutil.rmtree(target)
        else:
            target.unlink()
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copytree(source, target)


def _run_id_from_attempt_dir(attempt_dir: Path) -> str:
    parts = attempt_dir.parts
    for index, part in enumerate(parts):
        if part == "runs" and index + 1 < len(parts):
            return parts[index + 1]
    return ""


def _default_sandbox_runner(
    *,
    run_dir: Path,
    rscript_path: str | None,
    allowed_output_paths: list[Path],
) -> LocalRscriptSandboxRunner:
    return LocalRscriptSandboxRunner(
        run_dir=run_dir,
        rscript_path=rscript_path,
        allowed_output_paths=allowed_output_paths,
    )


def _runtime_messages(r_result: RRunResult, *, trial_output_path: Path) -> tuple[list[str], list[str], str]:
    errors: list[str] = []
    warnings: list[str] = []
    if r_result.exit_code == 127:
        errors.append("Rscript is not available for Code Agent trial run.")
        return errors, warnings, "environment_unavailable"
    if r_result.exit_code == 125:
        errors.append(r_result.stderr.strip() or "Sandbox preflight failed.")
        return errors, warnings, "sandbox_preflight_failed"
    if not r_result.success:
        errors.append(r_result.stderr.strip() or r_result.stdout.strip() or f"R exited with code {r_result.exit_code}.")
        return errors, warnings, "runtime_failed"
    if not trial_output_path.exists():
        errors.append(f"Trial run did not write expected output: {trial_output_path.as_posix()}")
        return errors, warnings, "missing_trial_output"
    return errors, warnings, "pass"


def _classify_failure(
    *,
    stage: str,
    message: str,
    exit_code: int | None,
    attempts_remaining: bool,
) -> CodeAgentFailureClassification:
    text = message.lower()
    environment_markers = [
        "rscript is not available",
        "failed to start rscript",
        "requires the r package",
        "unable to allocate memory",
        "memory",
        "timed out",
    ]
    input_markers = [
        "missing source",
        "missing required variable",
        "input_sdtm",
        "contains no rows",
        "does not exist",
        "no such file",
    ]
    if any(marker in text for marker in environment_markers) or exit_code in {124, 126, 127}:
        return CodeAgentFailureClassification(
            category="environment_error",
            repairable=False,
            reason=message,
            next_action="fix_runtime_environment",
        )
    if any(marker in text for marker in input_markers):
        return CodeAgentFailureClassification(
            category="input_or_spec_error",
            repairable=False,
            reason=message,
            next_action="review_inputs_or_spec",
        )
    repairable = attempts_remaining
    return CodeAgentFailureClassification(
        category="code_error" if stage == "runtime" else "static_contract_error",
        repairable=repairable,
        reason=message,
        next_action="repair_code" if repairable else "human_review_code_package",
    )


def _attempt_status_from_runtime_failure(classification: CodeAgentFailureClassification) -> str:
    if classification.category == "environment_error":
        return "environment_unavailable"
    if not classification.repairable:
        return "blocked"
    return "runtime_failed"


def _repair_reason_from_failure(
    classification: CodeAgentFailureClassification,
    errors: list[str],
    warnings: list[str],
) -> str:
    details = "; ".join(item for item in errors + warnings if item)
    return (
        f"The previous attempt failed as {classification.category}. "
        f"Reason: {classification.reason}. Details: {details}. "
        "Repair only code-level issues. Do not invent missing source data or clinical logic."
    )


def _copy_final_static_check(
    *,
    task: CodeAgentTask,
    attempt: CodeAgentAttempt | None,
    run_dir: Path,
    final_code_path: Path,
    require_nonblocking: bool,
) -> Path:
    static_dir = run_dir / "static_checks"
    static_dir.mkdir(parents=True, exist_ok=True)
    final_static_path = static_dir / f"{task.dataset.lower()}_static_check.json"
    attempt_static_path = Path(str(attempt.static_check_path)) if attempt and attempt.static_check_path else None
    if attempt_static_path and attempt_static_path.exists():
        report = run_generated_r_static_checks(
            study_id=task.study_id,
            run_id=task.run_id,
            dataset=task.dataset,
            code_path=final_code_path,
            expected_output_path=f"outputs/{task.dataset.lower()}.csv",
            required_identifiers=task.required_identifiers,
            required_identifier_source_id=task.required_identifier_source_id,
            rscript_path=task.rscript_path,
        )
        write_static_rule_report(report, path=final_static_path)
        if require_nonblocking:
            assert_no_blocking_static_findings(report)
        return final_static_path
    raise StaticRuleError("Code Agent did not produce a usable static-check artifact.")


def _write_review_package(
    *,
    task: CodeAgentTask,
    agent_dir: Path,
    prompt_artifact: ArtifactRef,
    final_draft: _AttemptDraft,
    final_static_path: Path,
    attempts: list[CodeAgentAttempt],
    failure_classification: CodeAgentFailureClassification | None,
    trial_run_status: str,
    trial_runtime_report_path: Path | None,
    trial_output_path: Path | None,
) -> Path:
    path = agent_dir / "package.json"
    payload = {
        "study_id": task.study_id,
        "run_id": task.run_id,
        "dataset": task.dataset,
        "code_agent_version": "v1",
        "review_required": True,
        "official_output_created": False,
        "official_output_policy": "Official ADaM output is created only after human code approval.",
        "spec_source": task.spec_source,
        "approved_spec_path": task.approved_spec_path,
        "prompt_path": prompt_artifact.path,
        "final_code_path": final_draft.final_artifacts.code_artifact.path,
        "static_check_path": str(final_static_path.as_posix()),
        "response_path": final_draft.final_artifacts.response_artifact.path,
        "parsed_response_path": final_draft.final_artifacts.package_artifact.path,
        "trial_run_status": trial_run_status,
        "trial_runtime_report_path": str(trial_runtime_report_path.as_posix()) if trial_runtime_report_path else None,
        "trial_output_path": str(trial_output_path.as_posix()) if trial_output_path else None,
        "attempts": [attempt.model_dump(mode="json") for attempt in attempts],
        "failure_classification": failure_classification.model_dump(mode="json") if failure_classification else None,
        "assumptions": final_draft.package.assumptions,
        "risk_points": final_draft.package.risk_points,
        "used_inputs": final_draft.package.used_inputs,
        "expected_outputs": final_draft.package.expected_outputs,
    }
    path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")
    return path


def _write_review_markdown(
    *,
    task: CodeAgentTask,
    agent_dir: Path,
    final_draft: _AttemptDraft,
    attempts: list[CodeAgentAttempt],
    trial_run_status: str,
    failure_classification: CodeAgentFailureClassification | None,
) -> Path:
    path = agent_dir / "review.md"
    lines = [
        f"# Code Agent Review: {task.dataset}",
        "",
        f"- Run: {task.run_id}",
        f"- Final code: `{final_draft.final_artifacts.code_artifact.path}`",
        f"- Trial status: `{trial_run_status}`",
        f"- Attempts: {len(attempts)}",
        "- Official output created: `false`",
        "",
        "Trial output is evidence only. The official ADaM output is created after human code approval.",
    ]
    if failure_classification:
        lines.extend(
            [
                "",
                "## Failure Classification",
                f"- Category: `{failure_classification.category}`",
                f"- Repairable: `{failure_classification.repairable}`",
                f"- Next action: `{failure_classification.next_action}`",
                f"- Reason: {failure_classification.reason}",
            ]
        )
    if final_draft.package.risk_points:
        lines.extend(["", "## Risk Points"])
        lines.extend(f"- {item}" for item in final_draft.package.risk_points)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path


def _artifact_ref(
    *,
    task: CodeAgentTask,
    kind_id: str,
    kind: str,
    path: Path,
    role: str,
    format: str,
    metadata: dict[str, Any],
) -> ArtifactRef:
    return ArtifactRef(
        artifact_id=f"{kind_id}_{task.study_id.lower()}_{task.run_id}_{task.dataset.lower()}",
        kind=kind,  # type: ignore[arg-type]
        path=str(path.as_posix()),
        sha256=f"sha256:{sha256_file(path)}",
        dataset=task.dataset,
        format=format,
        role=role,  # type: ignore[arg-type]
        metadata=metadata,
    )


def _datasets_included(context: dict[str, Any]) -> list[str]:
    datasets = list((context.get("source_dataset_profiles") or {}).keys())
    datasets.extend((context.get("resolved_dependencies") or {}).keys())
    return [str(dataset) for dataset in datasets]


def _variables_included(context: dict[str, Any]) -> list[str]:
    variables: list[str] = []
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        profiles = context.get(section) or {}
        if not isinstance(profiles, dict):
            continue
        for profile in profiles.values():
            if not isinstance(profile, dict):
                continue
            for column in profile.get("columns", []):
                column_name = str(column)
                if column_name not in variables:
                    variables.append(column_name)
    return variables


def _sample_row_counts(context: dict[str, Any], *, max_rows: int) -> dict[str, int]:
    counts: dict[str, int] = {}
    for section in ["source_dataset_profiles", "resolved_dependencies"]:
        profiles = context.get(section) or {}
        if not isinstance(profiles, dict):
            continue
        for dataset, profile in profiles.items():
            if not isinstance(profile, dict):
                continue
            sample_rows = profile.get("sample_rows", [])
            if sample_rows:
                counts[str(dataset)] = min(len(sample_rows), max_rows)
    return counts


def _llm_call_field(call_record: object, name: str, default: object | None) -> object | None:
    return getattr(call_record, name, default)


def _not_real_generation(
    provider_config: LLMProviderConfig,
    *,
    provider: object | None,
    provider_alias: object | None,
    transport: object | None,
) -> bool:
    configured_provider = provider_config.provider.strip().lower()
    provider_value = str(provider or "").strip().lower()
    provider_alias_value = str(provider_alias or "").strip().lower()
    transport_value = str(transport or "").strip().lower()
    return configured_provider == "mock" or provider_value == "mock" or provider_alias_value == "mock" or transport_value == "mock"
