"""Tests for the Phase 8.1 FastAPI backend boundary."""

from __future__ import annotations

import ast
import csv
import inspect
import json
import sys
import textwrap
import unittest
import uuid
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app
    from adam_agent.graph.workflow_state import input_fingerprint, workflow_projection_consistency
    from adam_agent.tools.artifacts import sha256_file
    from adam_agent.tools.static_rules import run_generated_r_static_checks, write_static_rule_report
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app
    from adam_agent.graph.workflow_state import input_fingerprint, workflow_projection_consistency
    from adam_agent.tools.artifacts import sha256_file
    from adam_agent.tools.static_rules import run_generated_r_static_checks, write_static_rule_report


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


def _write_static_check_for_code(study_dir: Path, run_id: str, dataset: str, code_path: Path) -> tuple[Path, str]:
    target = dataset.strip().upper()
    static_path = study_dir / "runs" / run_id / "static_checks" / f"{target.lower()}_static_check.json"
    report = run_generated_r_static_checks(
        study_id=study_dir.name,
        run_id=run_id,
        dataset=target,
        code_path=code_path,
        expected_output_path=f"outputs/{target.lower()}.csv",
    )
    write_static_rule_report(report, path=static_path)
    return static_path, f"sha256:{sha256_file(static_path)}"


def _assert_compatibility_projection(testcase: unittest.TestCase, payload: dict) -> tuple[dict, dict]:
    testcase.assertEqual(payload["workflow_control"], "graph_gateway_compatibility_shim")
    graph_path = Path(payload["graph_state_path"])
    workflow_path = Path(payload["workflow_state_path"])
    testcase.assertTrue(graph_path.exists())
    testcase.assertTrue(workflow_path.exists())
    graph_state = json.loads(graph_path.read_text(encoding="utf-8"))
    workflow_state = json.loads(workflow_path.read_text(encoding="utf-8"))
    consistency = workflow_projection_consistency(workflow_state, graph_state)
    testcase.assertTrue(consistency["consistent"], consistency["mismatches"])
    return graph_state, workflow_state


def _assert_run_projection(testcase: unittest.TestCase, study_dir: Path, run_id: str) -> tuple[dict, dict]:
    run_dir = study_dir / "runs" / run_id
    graph_path = run_dir / "graph_state.json"
    workflow_path = run_dir / "workflow_state.json"
    testcase.assertTrue(graph_path.exists())
    testcase.assertTrue(workflow_path.exists())
    graph_state = json.loads(graph_path.read_text(encoding="utf-8"))
    workflow_state = json.loads(workflow_path.read_text(encoding="utf-8"))
    consistency = workflow_projection_consistency(workflow_state, graph_state)
    testcase.assertTrue(consistency["consistent"], consistency["mismatches"])
    return graph_state, workflow_state


class Phase8ApiTests(unittest.TestCase):
    def test_health_endpoint(self) -> None:
        client = TestClient(create_app())

        response = client.get("/health")

        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json(), {"status": "ok"})

    def test_product_service_wrappers_delegate_state_changes_to_gateway_methods(self) -> None:
        from adam_agent.api import service

        wrappers = {
            service.persist_dependency_review: "review_dependency",
            service.finalize_dataset_inputs: "finalize_inputs",
            service.generate_dataset_draft_spec: "generate_draft_spec",
            service.generate_dataset_code: "generate_code",
            service.persist_draft_spec_review: "review_draft_spec",
            service.persist_code_review: "review_code",
            service.execute_approved_dataset_code: "execute_approved_code",
            service.persist_terminal_failure_review: "review_terminal_failure",
        }

        for wrapper, gateway_method in wrappers.items():
            tree = ast.parse(textwrap.dedent(inspect.getsource(wrapper)))
            called_names: set[str] = set()
            for node in ast.walk(tree):
                if not isinstance(node, ast.Call):
                    continue
                if isinstance(node.func, ast.Attribute):
                    called_names.add(node.func.attr)
                elif isinstance(node.func, ast.Name):
                    called_names.add(node.func.id)
            self.assertNotIn("validate_product_step_start", called_names)
            self.assertNotIn("dependency_gate_for_product_step", called_names)
            forbidden_low_level_recorders = {
                "record_code_generation",
                "record_input_spec_ready",
                "record_draft_spec_generation",
                "record_approved_draft_spec_ready",
                "record_execution",
                "record_code_review",
                "record_draft_spec_review",
                "record_terminal_failure_review",
                "update_workflow_state",
                "mark_workflow_inputs_current",
                "mark_workflow_inputs_stale",
                "load_graph_state",
                "resume",
            }
            self.assertTrue(
                forbidden_low_level_recorders.isdisjoint(called_names),
                f"{wrapper.__name__} must use GraphGateway product methods, not low-level recorders or workflow writes.",
            )
            self.assertIn(
                gateway_method,
                called_names,
                f"{wrapper.__name__} must delegate to GraphGateway.{gateway_method}().",
            )

    def test_legacy_run_endpoint_owns_only_remaining_service_workflow_writes(self) -> None:
        from adam_agent.api import service

        allowed_helpers = {
            "_write_legacy_run_blocked_workflow_state",
            "_write_legacy_run_completion_workflow_state",
        }
        direct_update_callers: list[str] = []
        for name, obj in vars(service).items():
            if name.startswith("__") or not inspect.isfunction(obj) or obj.__module__ != service.__name__:
                continue
            tree = ast.parse(textwrap.dedent(inspect.getsource(obj)))
            calls_update = any(
                isinstance(node, ast.Call)
                and (
                    (isinstance(node.func, ast.Name) and node.func.id == "update_workflow_state")
                    or (isinstance(node.func, ast.Attribute) and node.func.attr == "update_workflow_state")
                )
                for node in ast.walk(tree)
            )
            if calls_update:
                direct_update_callers.append(name)

        self.assertEqual(
            sorted(direct_update_callers),
            sorted(allowed_helpers),
            "Only legacy `/runs` compatibility helpers may write workflow_state directly from the service layer.",
        )

    def test_run_study_from_request_delegates_legacy_workflow_writes(self) -> None:
        from adam_agent.api import service

        tree = ast.parse(textwrap.dedent(inspect.getsource(service.run_study_from_request)))
        called_names: set[str] = set()
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call):
                continue
            if isinstance(node.func, ast.Attribute):
                called_names.add(node.func.attr)
            elif isinstance(node.func, ast.Name):
                called_names.add(node.func.id)

        self.assertNotIn("update_workflow_state", called_names)
        self.assertIn("_write_legacy_run_blocked_workflow_state", called_names)
        self.assertIn("_write_legacy_run_completion_workflow_state", called_names)

    def test_legacy_run_workflow_helpers_are_only_called_by_legacy_run_endpoint(self) -> None:
        from adam_agent.api import service

        legacy_helpers = {
            "_write_legacy_run_blocked_workflow_state",
            "_write_legacy_run_completion_workflow_state",
        }
        callers: dict[str, list[str]] = {helper: [] for helper in legacy_helpers}
        for name, obj in vars(service).items():
            if name.startswith("__") or not inspect.isfunction(obj) or obj.__module__ != service.__name__:
                continue
            if name in legacy_helpers:
                continue
            tree = ast.parse(textwrap.dedent(inspect.getsource(obj)))
            called_names: set[str] = set()
            for node in ast.walk(tree):
                if not isinstance(node, ast.Call):
                    continue
                if isinstance(node.func, ast.Attribute):
                    called_names.add(node.func.attr)
                elif isinstance(node.func, ast.Name):
                    called_names.add(node.func.id)
            for helper in legacy_helpers:
                if helper in called_names:
                    callers[helper].append(name)

        self.assertEqual(
            callers,
            {
                "_write_legacy_run_blocked_workflow_state": ["run_study_from_request"],
                "_write_legacy_run_completion_workflow_state": ["run_study_from_request"],
            },
            "Legacy `/runs` workflow helpers must not become reusable service-layer state writers.",
        )

    def test_review_summary_read_model_helpers_do_not_record_compare(self) -> None:
        from adam_agent.api import service

        called_names: set[str] = set()
        for helper in [service.build_run_review_summary, service._dataset_review]:
            tree = ast.parse(textwrap.dedent(inspect.getsource(helper)))
            for node in ast.walk(tree):
                if not isinstance(node, ast.Call):
                    continue
                if isinstance(node.func, ast.Attribute):
                    called_names.add(node.func.attr)
                elif isinstance(node.func, ast.Name):
                    called_names.add(node.func.id)

        forbidden_writes = {"record_compare", "_record_compare_in_graph_state", "update_workflow_state"}
        self.assertTrue(
            forbidden_writes.isdisjoint(called_names),
            "review-summary read-model helpers must not mutate graph/workflow state.",
        )

    def test_compare_endpoint_delegates_stateful_compare_to_gateway(self) -> None:
        from adam_agent.api import service

        tree = ast.parse(textwrap.dedent(inspect.getsource(service.compare_dataset_with_reference)))
        called_names: set[str] = set()
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call):
                continue
            if isinstance(node.func, ast.Attribute):
                called_names.add(node.func.attr)
            elif isinstance(node.func, ast.Name):
                called_names.add(node.func.id)

        self.assertIn("compare_reference_output", called_names)
        self.assertNotIn("record_compare", called_names)

    def test_upload_endpoint_delegates_input_invalidation_to_gateway(self) -> None:
        from adam_agent.api import service

        tree = ast.parse(textwrap.dedent(inspect.getsource(service.save_uploaded_file_bytes)))
        called_names: set[str] = set()
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call):
                continue
            if isinstance(node.func, ast.Attribute):
                called_names.add(node.func.attr)
            elif isinstance(node.func, ast.Name):
                called_names.add(node.func.id)

        self.assertIn("mark_study_inputs_changed", called_names)
        self.assertNotIn("invalidate_active_workflows", called_names)
        self.assertNotIn("mark_all_inputs_changed", called_names)

    def test_index_serves_local_web_ui(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        self.assertIn("text/html", response.headers["content-type"])
        self.assertIn("ADaM Agent Studio", response.text)
        self.assertIn("Study Dashboard", response.text)
        self.assertIn("Dependency Map", response.text)
        self.assertIn("Dataset Execution Cards", response.text)
        self.assertIn("operationBanner", response.text)
        self.assertIn("globalStatusDetail", response.text)
        self.assertIn("studyProgressPanel", response.text)
        self.assertIn("humanReviewQueuePanel", response.text)
        self.assertIn("Human Review Queue", response.text)
        self.assertIn("renderHumanReviewQueue", response.text)
        self.assertIn("humanReviewQueueItems", response.text)
        self.assertIn("reviewQueueActionText", response.text)
        self.assertIn("agentAuditPanel", response.text)
        self.assertIn("Agent Audit", response.text)
        self.assertIn("renderAgentAuditPanel", response.text)
        self.assertIn("activeAgentDecisions", response.text)
        self.assertIn("readableAgentName", response.text)
        self.assertIn("readableDecisionName", response.text)
        self.assertIn("readableRiskFlag", response.text)
        self.assertIn("specActionHints", response.text)
        self.assertIn("generationActionHints", response.text)
        self.assertIn("Study Progress", response.text)
        self.assertIn("studyProgressSummary", response.text)
        self.assertIn("studyNextActionPill", response.text)
        self.assertIn("renderActionAvailability", response.text)
        self.assertIn("aria-disabled-reason", response.text)
        self.assertIn("graphInterruptLabel", response.text)
        self.assertIn("generation plan", response.text)
        self.assertIn("nextActionText", response.text)
        self.assertIn("Try With Shiny Demo Data", response.text)
        self.assertIn("Use My Study Files", response.text)
        self.assertIn("Generate R Code", response.text)
        self.assertIn("Approve And Run Locally", response.text)
        self.assertIn("finalize-inputs", response.text)
        self.assertIn("finalizedInputsByDataset", response.text)
        self.assertIn("Audit Timeline", response.text)
        self.assertIn("Advanced settings and audit files", response.text)
        self.assertIn("Upload Define", response.text)
        self.assertIn("Upload Legacy Code", response.text)
        self.assertIn("Add another ADaM target", response.text)
        self.assertIn("addTargetButton", response.text)
        self.assertIn("Select one or more ADaM datasets to plan together", response.text)
        self.assertIn("selectedTargetsForPlan", response.text)
        self.assertIn("data-target-toggle", response.text)
        self.assertIn("data-target-view", response.text)
        self.assertIn("Planned targets:", response.text)
        self.assertIn("Real LLM API", response.text)
        self.assertIn("testLlmButton", response.text)
        self.assertIn("timeout_seconds: 300", response.text)
        self.assertIn("generatedByDataset", response.text)
        self.assertIn("reviewByDataset", response.text)
        self.assertIn("executionByDataset", response.text)
        self.assertIn("touched_graph_runs", response.text)
        self.assertIn("skipped_graph_runs", response.text)
        self.assertIn("refreshGraphState", response.text)
        self.assertIn("canApproveGeneratedCode", response.text)
        self.assertIn("Generated-code state exists", response.text)
        self.assertIn("data-card-target", response.text)
        self.assertIn("resetActiveDatasetView", response.text)
        self.assertNotIn("resetGeneratedState", response.text)
        self.assertNotIn("Create / Open Study", response.text)
        self.assertNotIn("Run Approved Code In Sandbox", response.text)

    def test_index_exposes_graph_state_progress_panel(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        progress_body = html.split("function studyProgressSummary(targets, runnable, blocked)", 1)[1].split("function graphInterruptLabel()", 1)[0]
        self.assertIn("state.graphState?.status", progress_body)
        self.assertIn("graphInterruptLabel()", progress_body)
        self.assertIn("datasetStatus(active, runnable, blocked)", progress_body)
        self.assertIn("nextActionText(active, activeStatus", progress_body)
        self.assertIn("targetSpecGateSatisfied(active)", html)
        self.assertIn("generatedFor(active)?.status === 'stale'", html)
        self.assertIn("executionFor(active)?.status === 'terminal_failure'", html)

    def test_index_exposes_human_review_queue_from_graph_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        queue_body = html.split("function renderHumanReviewQueue()", 1)[1].split("function graphInterruptLabel()", 1)[0]
        self.assertIn("state.graphState", queue_body)
        self.assertIn("graph.current_interrupt", queue_body)
        self.assertIn("const safeDatasetState = datasetState || {};", queue_body)
        self.assertIn("safeDatasetState.current_interrupt", queue_body)
        self.assertIn("safeDatasetState.status", queue_body)
        self.assertIn("Review dependency plan before product steps continue.", queue_body)
        self.assertIn("Review generated R code before local execution.", queue_body)
        self.assertIn("Review diagnostics and choose repair, retry, or skip.", queue_body)
        self.assertNotIn("JSON.stringify", queue_body)

    def test_index_hides_technical_paths_outside_advanced_artifact_view(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        draft_pane_body = html.split("function renderDraftSpecPane()", 1)[1].split("function renderGraphAwareDashboard()", 1)[0]
        draft_notice_body = html.split("function draftSpecNotice(generated)", 1)[1].split("function resultWorkspace(review)", 1)[0]
        advanced_body = html.split("function renderAdvanced()", 1)[1].split("function renderTimeline()", 1)[0]
        self.assertIn("artifactRecordedNote('Input spec artifact')", draft_pane_body)
        self.assertIn("artifactRecordedNote('Approved draft-spec artifact')", draft_pane_body)
        self.assertIn("artifactRecordedNote('Draft-spec artifact')", draft_pane_body)
        self.assertIn("Technical path is available under Advanced settings and audit files", html)
        self.assertIn("<th>Path</th>", advanced_body)
        self.assertNotIn("escapeHtml(finalized.input_spec_path)", draft_pane_body)
        self.assertNotIn("escapeHtml(finalized.approved_spec_path", draft_pane_body)
        self.assertNotIn("escapeHtml(draft.spec_path)", draft_pane_body)
        self.assertNotIn("escapeHtml(generated.draft_spec_path)", draft_notice_body)

    def test_index_exposes_agent_audit_from_graph_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        audit_body = html.split("function renderAgentAuditPanel()", 1)[1].split("function hasReferenceAdamEvidence", 1)[0]
        self.assertIn("state.graphState", audit_body)
        self.assertIn("graph.datasets?.[target]?.risk_flags", audit_body)
        self.assertIn("datasetState?.agent_decisions", audit_body)
        self.assertIn("graph.agent_decisions", audit_body)
        self.assertIn("validation_agent: 'Validation'", audit_body)
        self.assertIn("diagnosis_repair_agent: 'Diagnosis / repair'", audit_body)
        self.assertIn("readableDecisionName(decision.decision)", audit_body)
        self.assertIn("readableRiskFlag", audit_body)
        self.assertIn("Reference compare limited scope", audit_body)
        self.assertNotIn("JSON.stringify", audit_body)

    def test_index_explains_disabled_actions_from_existing_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        action_body = html.split("function actionAvailability()", 1)[1].split("function reviewFor(dataset)", 1)[0]
        self.assertIn("state.plan", action_body)
        self.assertIn("activeDependencyBlock()", action_body)
        self.assertIn("targetSpecGateSatisfied(target)", action_body)
        self.assertIn("generatedFor(target)", action_body)
        self.assertIn("executionFor(target)", action_body)
        self.assertIn("canApproveGeneratedCode(target)", action_body)
        self.assertIn("Clicking will prepare the dependency plan first", action_body)
        self.assertIn("Generated-code metadata exists", action_body)
        self.assertIn("function setButtonAvailability(id, item)", html)
        self.assertIn("button.setAttribute('aria-disabled-reason', item.reason)", html)
        self.assertIn("button.dataset.actionReady = String(Boolean(item.ready))", html)
        self.assertNotIn("button.disabled = !item.ready", html)
        dashboard_body = html.split("function renderGraphAwareDashboard()", 1)[1].split("function renderStudyProgress", 1)[0]
        self.assertIn("renderActionAvailability()", dashboard_body)

    def test_index_recovers_dependency_plan_projection_from_graph_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        apply_graph_body = html.split("function applyGraphState(graph)", 1)[1].split("function planFromGraphState(graph)", 1)[0]
        self.assertIn("const recoveredPlan = planFromGraphState(graph);", apply_graph_body)
        self.assertIn("state.plan = recoveredPlan;", apply_graph_body)
        self.assertIn("renderPlan(recoveredPlan);", apply_graph_body)
        plan_body = html.split("function planFromGraphState(graph)", 1)[1].split("function generatedFor(dataset)", 1)[0]
        self.assertIn("graph.requested_datasets", plan_body)
        self.assertIn("graph.target_datasets", plan_body)
        self.assertIn("graph.runnable_datasets", plan_body)
        self.assertIn("graph.blocked_datasets", plan_body)
        self.assertIn("graph.dependency_decisions", plan_body)
        self.assertIn("dependency_review_status", plan_body)

    def test_index_keeps_planning_selection_separate_from_active_target_view(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        selected_targets_body = html.split("function selectedTargets()", 1)[1].split("function planSelectionSet()", 1)[0]
        self.assertNotIn("selected.unshift(state.selectedTarget)", selected_targets_body)
        self.assertNotIn("!selected.includes(state.selectedTarget)", selected_targets_body)
        apply_graph_body = html.split("function applyGraphState(graph)", 1)[1].split("function generatedFor(dataset)", 1)[0]
        self.assertIn("const requestedTargets = graph?.requested_datasets || [];", apply_graph_body)
        self.assertNotIn("selectedTargetsForPlan = Array.from(new Set([...selectedTargets(), ...graphTargets", apply_graph_body)
        self.assertNotIn("selectedTargetsForPlan = graphTargets", apply_graph_body)
        self.assertIn("function plannedTargetsForDisplay(plan, fallbackTargets = null)", html)
        planned_display_body = html.split("function plannedTargetsForDisplay(plan, fallbackTargets = null)", 1)[1].split("function dependencyPlanSummary(plan)", 1)[0]
        self.assertIn("plan?.requested_datasets", planned_display_body)
        self.assertNotIn("plan.target_datasets", planned_display_body)
        view_handler = html.split("for (const button of node.querySelectorAll('[data-target-view]'))", 1)[1].split("byId('generateCodeButton')", 1)[0]
        self.assertIn("state.selectedTarget = button.dataset.targetView;", view_handler)
        self.assertNotIn("selectedTargetsForPlan", view_handler)
        dataset_card_handler = html.split("for (const card of node.querySelectorAll('[data-card-target]'))", 1)[1].split("function hasReferenceAdamEvidence", 1)[0]
        self.assertIn("state.selectedTarget = card.dataset.cardTarget;", dataset_card_handler)
        self.assertNotIn("preparePlan();", dataset_card_handler)

    def test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        dependency_body = html.split("function dependencyStepHtml(dependency, index, runnable, targets)", 1)[1].split("function dependencyDecisionFor(target)", 1)[0]
        runtime_body = html.split("function dependencyRuntimeAvailable(dependency, runnable, targets)", 1)[1].split("function dependencyEvidenceText(dependency, runnable, targets)", 1)[0]
        self.assertIn("Reference ADaM is comparison/output-shape evidence only", dependency_body)
        self.assertIn("not derivation authority or a runtime dependency by itself", dependency_body)
        self.assertIn("runtime input available or planned", dependency_body)
        self.assertIn("needs user action", dependency_body)
        self.assertNotIn("hasReferenceAdamEvidence(dependency)", runtime_body)
        self.assertIn("reference ADaM uploaded for compare/output-shape evidence only", html)
        self.assertIn("reference evidence", html)
        self.assertNotIn("provided in Reference ADaM", html)
        status_body = html.split("function datasetStatus(target, runnable, blocked)", 1)[1].split("async function generateCode()", 1)[0]
        self.assertLess(status_body.index("(runnable || []).includes(target)"), status_body.index("hasReferenceAdamEvidence(target)"))

    def test_product_workspace_endpoint_creates_hidden_default_workspace(self) -> None:
        client = TestClient(create_app())

        response = client.post("/product-workspace")

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertTrue(payload["study_id"].startswith("study_"))
        self.assertTrue(payload["run_id"].startswith("run_"))
        self.assertTrue((Path(payload["study_dir"]) / "input_sdtm").is_dir())
        self.assertIn("input_summary", payload)

    def test_workspace_endpoint_creates_canonical_folders(self) -> None:
        study_dir = _workspace_dir("phase8_workspace_endpoint") / "MY_STUDY"
        client = TestClient(create_app())

        response = client.post(
            "/studies/workspace",
            json={"study_dir": str(study_dir), "study_id": "MY_STUDY"},
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["study_id"], "MY_STUDY")
        for folder in ["input_sdtm", "input_spec", "input_define", "reference_adam", "legacy_code", "runs"]:
            self.assertTrue((study_dir / folder).is_dir())

    def test_upload_endpoint_saves_files_by_role_and_rescans(self) -> None:
        study_dir = _workspace_dir("phase8_upload_endpoint") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})

        response = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "sdtm"},
            files=[("files", ("dm.csv", b"USUBJID\n01\n", "text/csv"))],
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["role"], "sdtm")
        self.assertTrue((study_dir / "input_sdtm" / "dm.csv").exists())
        self.assertEqual(payload["input_summary"]["sdtm"][0]["dataset"], "DM")
        self.assertIn("digest", payload["input_fingerprint"])
        self.assertTrue(payload["input_diff"]["changed"])
        self.assertIn("input_sdtm/dm.csv", payload["input_diff"]["added"])

        define_response = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "define"},
            files=[("files", ("define.xml", b"<ODM></ODM>\n", "application/xml"))],
        )
        self.assertEqual(define_response.status_code, 200, define_response.text)
        define_payload = define_response.json()
        self.assertTrue((study_dir / "input_define" / "define.xml").exists())
        self.assertEqual(define_payload["input_summary"]["define"][0]["file_name"], "define.xml")

        legacy_response = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "legacy"},
            files=[("files", ("build_adlb.sas", b"data adlb; run;\n", "text/plain"))],
        )
        self.assertEqual(legacy_response.status_code, 200, legacy_response.text)
        legacy_payload = legacy_response.json()
        self.assertTrue((study_dir / "legacy_code" / "build_adlb.sas").exists())
        self.assertEqual(legacy_payload["input_summary"]["legacy_code"][0]["file_name"], "build_adlb.sas")
        self.assertEqual(legacy_payload["input_summary"]["legacy_code"][0]["status"], "ok")
        self.assertEqual(legacy_payload["input_summary"]["legacy_code"][0]["preview_type"], "code")
        self.assertIn("data adlb", legacy_payload["input_summary"]["legacy_code"][0]["text_preview"].lower())

    def test_upload_marks_existing_workflow_state_stale(self) -> None:
        study_dir = _workspace_dir("phase8_upload_stales_workflow") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (study_dir / "input_spec" / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM"}]}),
            encoding="utf-8",
        )

        plan = client.post(
            "/runs/prepare",
            json={"study_dir": str(study_dir), "run_id": "run_stale", "target_datasets": ["ADAE"]},
        )
        self.assertEqual(plan.status_code, 200, plan.text)

        upload = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "legacy"},
            files=[("files", ("build_adae.sas", b"data adae; set ae; run;\n", "text/plain"))],
        )

        self.assertEqual(upload.status_code, 200, upload.text)
        upload_payload = upload.json()
        self.assertIn("run_stale", upload_payload["touched_runs"])
        self.assertIn("run_stale", upload_payload["touched_graph_runs"])
        graph_state = json.loads((study_dir / "runs" / "run_stale" / "graph_state.json").read_text(encoding="utf-8"))
        state = json.loads((study_dir / "runs" / "run_stale" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertTrue(graph_state["dependency_plan"]["plan_stale"])
        self.assertTrue(graph_state["dependency_plan"]["input_diff"]["changed"])
        self.assertEqual(graph_state["dependency_review_status"], "stale")
        self.assertEqual(graph_state["current_interrupt"]["name"], "dependency_review")
        self.assertEqual(state["projection_source"], "langgraph")
        self.assertTrue(state["dependency_plan"]["plan_stale"])
        self.assertTrue(state["dependency_plan"]["input_diff"]["changed"])
        _assert_run_projection(self, study_dir, "run_stale")

    def test_upload_marks_existing_graph_product_state_stale_and_blocks_generation(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_upload_stales_graph_product")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_upload_graph_stale/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)

        upload = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "sdtm"},
            files=[("files", ("ae.csv", b"USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", "text/csv"))],
        )

        self.assertEqual(upload.status_code, 200, upload.text)
        upload_payload = upload.json()
        self.assertIn("run_upload_graph_stale", upload_payload["touched_runs"])
        self.assertIn("run_upload_graph_stale", upload_payload["touched_graph_runs"])
        graph_state = client.get(
            "/runs/run_upload_graph_stale/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        adae_state = graph_state["datasets"]["ADAE"]
        self.assertEqual(graph_state["dependency_review_status"], "stale")
        self.assertTrue(graph_state["dependency_plan"]["plan_stale"])
        self.assertEqual(graph_state["current_interrupt"]["name"], "dependency_review")
        self.assertEqual(adae_state["status"], "needs_review")
        self.assertEqual(adae_state["current_interrupt"]["name"], "code_review")
        self.assertEqual(adae_state["code_state"]["status"], "stale")
        self.assertIn("input_sdtm/ae.csv", adae_state["code_state"]["input_diff"]["changed_files"])
        _assert_run_projection(self, study_dir, "run_upload_graph_stale")

        blocked = client.post(
            "/runs/run_upload_graph_stale/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(blocked.status_code, 400)
        self.assertIn("dependency plan is stale", blocked.json()["detail"])

    def test_upload_invalidates_graph_run_even_when_workflow_projection_is_missing(self) -> None:
        study_dir = _workspace_dir("phase8_upload_graph_state_only") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        plan = client.post(
            "/runs/prepare",
            json={"study_dir": str(study_dir), "run_id": "run_graph_state_only", "target_datasets": ["ADAE"]},
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        (study_dir / "runs" / "run_graph_state_only" / "workflow_state.json").unlink()

        upload = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "sdtm"},
            files=[("files", ("ae.csv", b"USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", "text/csv"))],
        )

        self.assertEqual(upload.status_code, 200, upload.text)
        payload = upload.json()
        self.assertIn("run_graph_state_only", payload["touched_graph_runs"])
        self.assertEqual(payload["skipped_graph_runs"], [])
        graph_state = client.get(
            "/runs/run_graph_state_only/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        self.assertEqual(graph_state["dependency_review_status"], "stale")
        self.assertTrue(graph_state["dependency_plan"]["plan_stale"])
        _assert_run_projection(self, study_dir, "run_graph_state_only")

    def test_upload_reports_corrupt_graph_state_as_skipped(self) -> None:
        study_dir = _workspace_dir("phase8_upload_corrupt_graph_state") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        run_dir = study_dir / "runs" / "run_corrupt"
        run_dir.mkdir(parents=True)
        (run_dir / "graph_state.json").write_text("{not-json", encoding="utf-8")

        upload = client.post(
            "/studies/files",
            params={"study_dir": str(study_dir), "role": "sdtm"},
            files=[("files", ("ae.csv", b"USUBJID,AETERM\n01,HEADACHE\n", "text/csv"))],
        )

        self.assertEqual(upload.status_code, 200, upload.text)
        payload = upload.json()
        self.assertEqual(payload["touched_graph_runs"], [])
        self.assertEqual(payload["skipped_graph_runs"], ["run_corrupt"])

    def test_demo_study_endpoint_prepares_shiny_demo_shape(self) -> None:
        source = _demo_source("phase8_api_demo_source")
        target = _workspace_dir("phase8_api_demo_target") / "demo_adam"
        client = TestClient(create_app())

        response = client.post(
            "/demo-study",
            params={"demo_source_dir": str(source), "study_dir": str(target)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["study_id"], "demo_adam")
        self.assertEqual(payload["target_datasets"], ["ADAE"])
        self.assertIn(payload["execution_mode"], {"llm_downstream_provider", "llm_downstream_r_sandbox"})
        self.assertTrue((target / "input_sdtm" / "ae.csv").exists())
        self.assertTrue((target / "input_sdtm" / "dm.csv").exists())
        self.assertTrue((target / "input_sdtm" / "ex.csv").exists())
        self.assertTrue((target / "input_spec" / "ads_adae_full.csv").exists())
        self.assertTrue((target / "input_spec" / "ads_adsl_full.csv").exists())
        self.assertFalse((target / "input_spec" / "adae.csv").exists())
        self.assertFalse((target / "input_spec" / "adsl.csv").exists())
        self.assertTrue((target / "reference_adam" / "adsl.csv").exists())
        self.assertTrue((target / "reference_adam" / "adae.csv").exists())
        self.assertFalse((target / "legacy_code" / "adae.sas").exists())
        self.assertTrue(any("PSY201 is a separate project" in note for note in payload["notes"]))

    def test_study_inputs_endpoint_returns_human_oriented_file_summary(self) -> None:
        source = _demo_source("phase8_api_input_summary_source")
        target = _workspace_dir("phase8_api_input_summary_target") / "demo_adam"
        client = TestClient(create_app())
        demo_response = client.post(
            "/demo-study",
            params={"demo_source_dir": str(source), "study_dir": str(target)},
        )
        self.assertEqual(demo_response.status_code, 200, demo_response.text)

        response = client.get("/study-inputs", params={"study_dir": str(target)})

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["study_id"], "demo_adam")
        self.assertEqual({item["dataset"] for item in payload["sdtm"]}, {"AE", "DM", "EX"})
        self.assertEqual({item["dataset"] for item in payload["reference_adam"]}, {"ADAE", "ADSL"})
        self.assertIn("ads_adae_full.csv", {item["file_name"] for item in payload["specs"]})

    def test_study_inputs_marks_sas7bdat_as_runtime_supported_when_preview_unavailable(self) -> None:
        study_dir = _workspace_dir("phase8_sas7bdat_summary") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "dm.sas7bdat").write_bytes(b"not a real sas table")

        response = client.get("/study-inputs", params={"study_dir": str(study_dir)})

        self.assertEqual(response.status_code, 200, response.text)
        item = response.json()["sdtm"][0]
        self.assertEqual(item["file_name"], "dm.sas7bdat")
        self.assertEqual(item["format"], "sas7bdat")
        self.assertIn(item["status"], {"not_previewed", "error"})
        self.assertIn("haven", item["note"])

    def test_demo_study_rejects_run_to_completion_llm_endpoint(self) -> None:
        source = _demo_source("phase8_api_demo_run_source")
        target = _workspace_dir("phase8_api_demo_run_target") / "demo_adam"
        client = TestClient(create_app())
        demo_response = client.post(
            "/demo-study",
            params={"demo_source_dir": str(source), "study_dir": str(target)},
        )
        self.assertEqual(demo_response.status_code, 200, demo_response.text)
        demo = demo_response.json()

        run_response = client.post(
            "/runs",
            json={
                "study_dir": demo["study_dir"],
                "run_id": "run_demo_from_endpoint",
                "target_datasets": demo["target_datasets"],
                "config_path": demo["config_path"],
                "execution_mode": "llm_downstream_provider",
            },
        )

        self.assertEqual(run_response.status_code, 400)
        self.assertIn("would bypass review gates", run_response.json()["detail"])
        state = json.loads((target / "runs" / "run_demo_from_endpoint" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(state["current_interrupt"], "split_flow_required")
        self.assertEqual(state["workflow_control"], "legacy_run_to_completion_compatibility_shim")
        self.assertEqual(state["legacy_endpoint"], "POST /runs")
        self.assertTrue(state["product_flow_required"])
        self.assertIsNone(state["graph_state_path"])
        self.assertTrue(state["workflow_state_path"].endswith("runs/run_demo_from_endpoint/workflow_state.json"))

    def test_create_run_stub_is_marked_legacy_compatibility_shim(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_api_legacy_run_stub")
        client = TestClient(create_app())

        response = client.post(
            "/runs",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_legacy_stub",
                "target_datasets": ["ADAE"],
                "execution_mode": "stub",
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["workflow_control"], "legacy_run_to_completion_compatibility_shim")
        self.assertIsNone(payload["graph_state_path"])
        self.assertTrue(payload["workflow_state_path"].endswith("runs/run_legacy_stub/workflow_state.json"))
        self.assertEqual(payload["status"], "completed")
        workflow_state = json.loads((study_dir / "runs" / "run_legacy_stub" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(workflow_state["workflow_control"], "legacy_run_to_completion_compatibility_shim")
        self.assertEqual(workflow_state["legacy_endpoint"], "POST /runs")
        self.assertFalse(workflow_state["product_flow_required"])
        self.assertIsNone(workflow_state["graph_state_path"])
        self.assertEqual(workflow_state["workflow_state_path"], payload["workflow_state_path"])

    def test_generate_review_execute_split_flow(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_split_flow")
        client = TestClient(create_app())

        plan = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_split_flow",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        self.assertEqual(plan.json()["runnable_datasets"], ["ADAE"])

        generated = client.post(
            "/runs/run_split_flow/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        generated_payload = generated.json()
        self.assertEqual(generated_payload["status"], "code_generated")
        _, workflow_state = _assert_compatibility_projection(self, generated_payload)
        self.assertTrue(generated_payload["static_check_path"].endswith("adae_static_check.json"))
        self.assertTrue(Path(generated_payload["static_check_path"]).exists())
        static_check = json.loads(Path(generated_payload["static_check_path"]).read_text(encoding="utf-8"))
        self.assertEqual(static_check["status"], "pass")
        self.assertTrue(static_check["implemented"])
        self.assertIn("do not prove full CDISC", static_check["non_compliance_disclaimer"])
        self.assertEqual(workflow_state["datasets"]["ADAE"]["status"], "needs_review")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["code_state"]["status"], "generated")
        self.assertEqual(workflow_state["current_interrupt"], "code_review")
        self.assertTrue((study_dir / "runs" / "run_split_flow" / "code" / "build_adae.R").exists())
        self.assertTrue(generated_payload["generated_code"])
        self.assertFalse((study_dir / "runs" / "run_split_flow" / "outputs" / "adae.csv").exists())

        blocked_execute = client.post(
            "/runs/run_split_flow/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir)},
        )
        self.assertEqual(blocked_execute.status_code, 400)
        self.assertIn("must be approved", blocked_execute.json()["detail"])

        review = client.post(
            "/runs/run_split_flow/datasets/ADAE/code-review",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Approved for sandbox execution.",
            },
        )
        self.assertEqual(review.status_code, 200, review.text)
        review_payload = review.json()
        self.assertTrue(review_payload["approved"])
        _assert_compatibility_projection(self, review_payload)
        self.assertTrue(review_payload["static_check_path"].endswith("adae_static_check.json"))
        graph_state = client.get(
            "/runs/run_split_flow/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        adae_graph_state = graph_state.json()["datasets"]["ADAE"]
        self.assertEqual(adae_graph_state["code_state"]["status"], "approved")
        self.assertEqual(adae_graph_state["human_commands"][-1]["interrupt"], "code_review")
        self.assertEqual(adae_graph_state["human_commands"][-1]["action"], "approve")

        executed = client.post(
            "/runs/run_split_flow/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        executed_payload = executed.json()
        self.assertEqual(executed_payload["status"], "completed")
        _assert_compatibility_projection(self, executed_payload)
        validation_payload = json.loads(Path(executed_payload["validation_report_path"]).read_text(encoding="utf-8"))
        self.assertEqual(validation_payload["sandbox"]["backend_name"], "local_rscript")
        self.assertFalse(validation_payload["sandbox"]["hardened"])
        self.assertFalse(validation_payload["sandbox"]["network_disabled"])
        output_path = study_dir / "runs" / "run_split_flow" / "outputs" / "adae.csv"
        self.assertTrue(output_path.exists())
        with output_path.open(newline="", encoding="utf-8") as handle:
            rows = list(csv.DictReader(handle))
        self.assertGreater(len(rows), 0)
        self.assertIn("AETERM", rows[0])
        self.assertEqual(rows[0]["AETERM"], "HEADACHE")

        table = client.get(
            "/runs/run_split_flow/datasets/ADAE/table",
            params={"study_dir": str(study_dir), "kind": "generated", "page": 1, "page_size": 1},
        )
        self.assertEqual(table.status_code, 200, table.text)
        self.assertEqual(table.json()["status"], "ok")
        self.assertEqual(len(table.json()["rows"]), 1)

        compare = client.get(
            "/runs/run_split_flow/datasets/ADAE/compare",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(compare.status_code, 200, compare.text)
        self.assertEqual(compare.json()["dataset"], "ADAE")
        self.assertIn(compare.json()["status"], {"match", "differences", "missing_reference"})
        graph_after_compare = client.get(
            "/runs/run_split_flow/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_after_compare.status_code, 200, graph_after_compare.text)
        adae_after_compare = graph_after_compare.json()["datasets"]["ADAE"]
        self.assertEqual(adae_after_compare["compare_summary"]["status"], compare.json()["status"])
        self.assertEqual(adae_after_compare["result_summary"]["compare_status"], compare.json()["status"])
        self.assertTrue((study_dir / "runs" / "run_split_flow" / "compare" / "adae_compare_report.json").exists())
        _assert_run_projection(self, study_dir, "run_split_flow")

        download = client.get(
            "/runs/run_split_flow/datasets/ADAE/download",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        self.assertEqual(download.status_code, 200, download.text)
        self.assertIn("USUBJID", download.text)

    def test_compare_does_not_create_graph_state_without_prepared_run(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_compare_no_graph_state")
        client = TestClient(create_app())
        run_dir = study_dir / "runs" / "run_compare_no_graph_state"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir()
        (output_dir / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (validation_dir / "adae_validation_report.json").write_text(
            json.dumps({"dataset": "ADAE", "status": "pass"}),
            encoding="utf-8",
        )

        compare = client.get(
            "/runs/run_compare_no_graph_state/datasets/ADAE/compare",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(compare.status_code, 200, compare.text)
        self.assertEqual(compare.json()["dataset"], "ADAE")
        self.assertFalse((run_dir / "graph_state.json").exists())
        self.assertFalse((run_dir / "compare" / "adae_compare_report.json").exists())

    def test_review_summary_reports_compare_without_mutating_graph_when_reference_disappears(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_compare_missing_refresh")
        client = TestClient(create_app())
        plan = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_compare_missing_refresh",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        run_dir = study_dir / "runs" / "run_compare_missing_refresh"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        audit_dir = run_dir / "audit"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir()
        audit_dir.mkdir(exist_ok=True)
        (output_dir / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (validation_dir / "adae_validation_report.json").write_text(
            json.dumps({"dataset": "ADAE", "status": "pass"}),
            encoding="utf-8",
        )
        (study_dir / "reference_adam" / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (audit_dir / "manifest.json").write_text(
            json.dumps({"study_id": "PSY201", "run_id": "run_compare_missing_refresh", "requested_datasets": ["ADAE"]}),
            encoding="utf-8",
        )
        first = client.get(
            "/runs/run_compare_missing_refresh/datasets/ADAE/compare",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(first.status_code, 200, first.text)
        self.assertEqual(first.json()["status"], "match")
        (study_dir / "reference_adam" / "adae.csv").unlink()

        summary = client.get(
            "/runs/run_compare_missing_refresh/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(summary.status_code, 200, summary.text)
        dataset_review = summary.json()["dataset_reviews"][0]
        self.assertEqual(dataset_review["dataset"], "ADAE")
        self.assertEqual(dataset_review["compare_summary"]["status"], "missing_reference")
        graph_state = client.get(
            "/runs/run_compare_missing_refresh/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        self.assertEqual(graph_state["datasets"]["ADAE"]["compare_summary"]["status"], "match")
        _assert_run_projection(self, study_dir, "run_compare_missing_refresh")

    def test_adsl_uses_same_split_flow_as_other_adam_targets(self) -> None:
        study_dir = _study_with_adsl_inputs("phase8_adsl_split_flow")
        client = TestClient(create_app())

        plan = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_adsl_split_flow",
                "target_datasets": ["ADSL"],
            },
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        self.assertEqual(plan.json()["runnable_datasets"], ["ADSL"])

        finalized = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        self.assertEqual(finalized.json()["status"], "input_spec_ready")

        generated = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/generate-code",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        generated_payload = generated.json()
        self.assertEqual(generated_payload["status"], "code_generated")
        self.assertTrue(generated_payload["code_path"].endswith("code/build_adsl.R"))
        self.assertTrue(generated_payload["static_check_path"].endswith("adsl_static_check.json"))
        self.assertIn("mock ADSL generation", generated_payload["generated_code"])
        self.assertFalse((study_dir / "runs" / "run_adsl_split_flow" / "outputs" / "adsl.csv").exists())
        self.assertFalse((study_dir / "runs" / "run_adsl_split_flow" / "audit" / "adsl_manifest.json").exists())

        blocked_execute = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/execute-approved-code",
            json={"study_dir": str(study_dir)},
        )
        self.assertEqual(blocked_execute.status_code, 400)
        self.assertIn("must be approved", blocked_execute.json()["detail"])

        review = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        self.assertTrue(review.json()["approved"])

        executed = client.post(
            "/runs/run_adsl_split_flow/datasets/ADSL/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertEqual(executed.json()["status"], "completed")
        output_path = study_dir / "runs" / "run_adsl_split_flow" / "outputs" / "adsl.csv"
        self.assertTrue(output_path.exists())
        with output_path.open(newline="", encoding="utf-8") as handle:
            rows = list(csv.DictReader(handle))
        self.assertEqual(rows[0]["USUBJID"], "01")
        self.assertTrue((study_dir / "runs" / "run_adsl_split_flow" / "llm" / "adsl_context.json").exists())
        self.assertTrue((study_dir / "runs" / "run_adsl_split_flow" / "llm" / "adsl_response.json").exists())
        self.assertFalse((study_dir / "runs" / "run_adsl_split_flow" / "audit" / "adsl_manifest.json").exists())

    def test_approved_draft_spec_is_invalidated_when_inputs_change(self) -> None:
        study_dir = _workspace_dir("phase8_stale_draft_spec") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")

        finalized = client.post(
            "/runs/run_stale_draft/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        self.assertEqual(finalized.json()["next_action"], "review_draft_spec")

        review = client.post(
            "/runs/run_stale_draft/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)

        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")
        generated = client.post(
            "/runs/run_stale_draft/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(generated.status_code, 400)
        self.assertIn("Approved draft spec is stale", generated.json()["detail"])

    def test_approved_draft_spec_is_invalidated_when_approved_file_changes(self) -> None:
        study_dir = _workspace_dir("phase8_tampered_approved_draft_spec") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")

        finalized = client.post(
            "/runs/run_tampered_draft/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        review = client.post(
            "/runs/run_tampered_draft/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        approved_path = Path(review.json()["approved_spec_path"])
        approved_payload = json.loads(approved_path.read_text(encoding="utf-8"))
        approved_payload["variables"].append({"variable": "TAMPERED", "source_domains": ["AE"]})
        approved_path.write_text(json.dumps(approved_payload), encoding="utf-8")

        generated = client.post(
            "/runs/run_tampered_draft/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(generated.status_code, 400)
        self.assertIn("Approved draft spec changed after approval", generated.json()["detail"])

    def test_finalize_inputs_blocks_review_required_dependency_evidence(self) -> None:
        study_dir = _study_with_blocked_adae_dependency("phase8_dependency_review_blocks_finalize")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_dependency_gate/datasets/ADAE/finalize-inputs",
            json={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("cannot continue until dependency issues are resolved", response.json()["detail"])
        workflow_state = json.loads((study_dir / "runs" / "run_dependency_gate" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertNotEqual(workflow_state["last_node"], "finalize_inputs_start")
        _assert_run_projection(self, study_dir, "run_dependency_gate")

    def test_generate_code_dependency_gate_does_not_write_service_start_projection(self) -> None:
        study_dir = _study_with_blocked_adae_dependency("phase8_dependency_review_blocks_generate")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_dependency_generate_gate/datasets/ADAE/generate-code",
            json={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("cannot continue until dependency issues are resolved", response.json()["detail"])
        workflow_state = json.loads(
            (study_dir / "runs" / "run_dependency_generate_gate" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertNotEqual(workflow_state["last_node"], "generate_code_start")
        _assert_run_projection(self, study_dir, "run_dependency_generate_gate")

    def test_draft_spec_dependency_gate_does_not_write_service_start_projection(self) -> None:
        study_dir = _study_with_blocked_adae_dependency("phase8_dependency_review_blocks_draft")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_dependency_draft_gate/datasets/ADAE/draft-spec",
            json={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("cannot continue until dependency issues are resolved", response.json()["detail"])
        workflow_state = json.loads(
            (study_dir / "runs" / "run_dependency_draft_gate" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        self.assertNotEqual(workflow_state["last_node"], "draft_spec_start")
        _assert_run_projection(self, study_dir, "run_dependency_draft_gate")

    def test_finalize_inputs_blocks_dependency_warning(self) -> None:
        study_dir = _workspace_dir("phase8_dependency_warning_blocks_finalize") / "MY_STUDY"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        legacy_dir = study_dir / "legacy_code"
        output_dir = study_dir / "runs" / "run_dependency_warning" / "outputs"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        legacy_dir.mkdir()
        output_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (output_dir / "adlb.csv").write_text("USUBJID,PARAMCD\n01,ALT\n", encoding="utf-8")
        from adam_agent.graph.gateway import GraphGateway
        from adam_agent.schemas.artifacts import ArtifactRef

        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_dependency_warning",
            target_datasets=["ADLB"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_dependency_warning").model_copy(deep=True)
        adlb_path = output_dir / "adlb.csv"
        state.datasets["ADLB"].status = "completed"
        state.datasets["ADLB"].execution_state.update(
            {
                "terminal_failure": False,
                "partial_output_usable": True,
                "output_path": str(adlb_path.as_posix()),
            }
        )
        state.datasets["ADLB"].artifacts.append(
            ArtifactRef(
                artifact_id="output_adam_my_study_run_dependency_warning_adlb",
                kind="output_adam",
                path=str(adlb_path.as_posix()),
                sha256=f"sha256:{sha256_file(adlb_path)}",
                dataset="ADLB",
                format="csv",
                role="output",
            )
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_completed_dependency_output")
        (spec_dir / "adtte.json").write_text(
            json.dumps(
                {
                    "dataset": "ADTTE",
                    "variables": [
                        {
                            "variable": "CNSR",
                            "source_domains": ["ADLB"],
                            "derivation": "Use ADLB threshold records.",
                        }
                    ],
                }
            ),
            encoding="utf-8",
        )
        (legacy_dir / "ADTTE.sas").write_text("data adtte; merge adlb adae; by usubjid; run;", encoding="utf-8")
        client = TestClient(create_app())

        prepared = client.post(
            "/runs/prepare",
            json={"study_dir": str(study_dir), "run_id": "run_dependency_warning", "target_datasets": ["ADTTE"]},
        )
        response = client.post(
            "/runs/run_dependency_warning/datasets/ADTTE/finalize-inputs",
            json={"study_dir": str(study_dir)},
        )

        self.assertEqual(prepared.status_code, 200, prepared.text)
        self.assertEqual(prepared.json()["dependency_review_status"], "warning")
        self.assertEqual(response.status_code, 400)
        self.assertIn("dependency plan has warnings", response.json()["detail"])

    def test_draft_spec_without_fingerprint_cannot_be_approved(self) -> None:
        study_dir = _workspace_dir("phase8_draft_without_fingerprint") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_no_fingerprint"
        spec_dir = run_dir / "specs"
        spec_dir.mkdir(parents=True)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(json.dumps({"dataset": "ADAE", "variables": []}), encoding="utf-8")
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_draft_spec_generation(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_no_fingerprint",
            dataset="ADAE",
            draft_spec_path=draft_path,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_no_fingerprint/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("no input fingerprint", response.json()["detail"])

    def test_approved_draft_spec_without_fingerprint_cannot_be_reused(self) -> None:
        study_dir = _workspace_dir("phase8_approved_draft_without_fingerprint") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_approved_no_fingerprint"
        approved_dir = run_dir / "approved_specs"
        review_dir = run_dir / "reviews"
        approved_dir.mkdir(parents=True)
        review_dir.mkdir(parents=True)
        (approved_dir / "adae_approved_spec.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": []}),
            encoding="utf-8",
        )
        (review_dir / "adae_draft_spec_review.json").write_text(
            json.dumps({"decision": "approve", "approved": True}),
            encoding="utf-8",
        )
        client = TestClient(create_app())

        generated = client.post(
            "/runs/run_approved_no_fingerprint/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(generated.status_code, 400)
        self.assertIn("recorded in graph state", generated.json()["detail"])

    def test_execute_approved_code_terminal_failure_is_explicit(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_failure")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        run_dir = study_dir / "runs" / "run_terminal"
        code_dir = run_dir / "code"
        (code_dir / "build_adae.R").write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        review = client.post(
            "/runs/run_terminal/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("Generated code changed after graph code generation", review.json()["detail"])
        generated = client.post(
            "/runs/run_terminal/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = code_dir / "build_adae.R"
        code_path.write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal", "ADAE", code_path)
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)

        executed = client.post(
            "/runs/run_terminal/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 200, executed.text)
        payload = executed.json()
        self.assertEqual(payload["status"], "terminal_failure")
        self.assertTrue(payload["terminal_failure"])
        report = json.loads(Path(payload["validation_report_path"]).read_text(encoding="utf-8"))
        self.assertTrue(report["terminal_failure"])
        self.assertFalse(report["partial_output_usable"])
        graph_state = client.get(
            "/runs/run_terminal/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        self.assertEqual(graph_state.json()["datasets"]["ADAE"]["current_interrupt"]["name"], "terminal_failure")

        reviewed = client.post(
            "/runs/run_terminal/datasets/ADAE/terminal-failure-review",
            json={
                "study_dir": str(study_dir),
                "decision": "repair_code",
                "reviewer": "tester",
                "notes": "Generated code needs repair.",
            },
        )

        self.assertEqual(reviewed.status_code, 200, reviewed.text)
        self.assertEqual(reviewed.json()["decision"], "repair_code")
        self.assertEqual(reviewed.json()["current_interrupt"], "terminal_failure")
        self.assertEqual(reviewed.json()["next_action"], "repair_generated_code")
        reviewed_state = client.get(
            "/runs/run_terminal/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        adae_state = reviewed_state["datasets"]["ADAE"]
        self.assertEqual(adae_state["status"], "needs_review")
        self.assertEqual(adae_state["execution_state"]["terminal_failure_review"]["action"], "repair_code")
        self.assertEqual(adae_state["execution_state"]["next_action"], "repair_generated_code")
        self.assertEqual(adae_state["human_commands"][-1]["interrupt"], "terminal_failure")
        _assert_run_projection(self, study_dir, "run_terminal")

    def test_execute_requires_terminal_failure_review_before_retry(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_retry_gate")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal_retry_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_retry_gate" / "code" / "build_adae.R"
        code_path.write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal_retry_gate", "ADAE", code_path)
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_retry_gate",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_retry_gate/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        first_execution = client.post(
            "/runs/run_terminal_retry_gate/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(first_execution.status_code, 200, first_execution.text)
        self.assertTrue(first_execution.json()["terminal_failure"])

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            second_execution = client.post(
                "/runs/run_terminal_retry_gate/datasets/ADAE/execute-approved-code",
                json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
            )

        self.assertEqual(second_execution.status_code, 400, second_execution.text)
        self.assertIn("Terminal failure must be reviewed before retrying execution", second_execution.json()["detail"])
        compile_graph.assert_not_called()

    def test_draft_spec_uses_gateway_terminal_failure_preflight(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_draft_spec_gate")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal_draft_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_draft_gate" / "code" / "build_adae.R"
        code_path.write_text(
            "dir.create('outputs', showWarnings = FALSE)\n"
            "write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal_draft_gate", "ADAE", code_path)
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_draft_gate",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_draft_gate/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        first_execution = client.post(
            "/runs/run_terminal_draft_gate/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(first_execution.status_code, 200, first_execution.text)
        self.assertTrue(first_execution.json()["terminal_failure"])

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            draft_spec = client.post(
                "/runs/run_terminal_draft_gate/datasets/ADAE/draft-spec",
                json={
                    "study_dir": str(study_dir),
                    "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                    "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
                },
            )

        self.assertEqual(draft_spec.status_code, 400, draft_spec.text)
        self.assertIn("Terminal failure must be reviewed", draft_spec.json()["detail"])
        compile_graph.assert_not_called()

    def test_retry_execution_review_allows_approved_code_execution_path(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_retry_allows_execute")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal_retry_allows_execute/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_retry_allows_execute" / "code" / "build_adae.R"
        code_path.write_text(
            "dir.create('outputs', showWarnings = FALSE)\n"
            "write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n",
            encoding="utf-8",
        )
        static_path, static_sha = _write_static_check_for_code(
            study_dir,
            "run_terminal_retry_allows_execute",
            "ADAE",
            code_path,
        )
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_retry_allows_execute",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_retry_allows_execute/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        first_execution = client.post(
            "/runs/run_terminal_retry_allows_execute/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(first_execution.status_code, 200, first_execution.text)
        self.assertTrue(first_execution.json()["terminal_failure"])
        triage = client.post(
            "/runs/run_terminal_retry_allows_execute/datasets/ADAE/terminal-failure-review",
            json={"study_dir": str(study_dir), "decision": "retry_execution", "reviewer": "tester"},
        )
        self.assertEqual(triage.status_code, 200, triage.text)

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass"},
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            second_execution = client.post(
                "/runs/run_terminal_retry_allows_execute/datasets/ADAE/execute-approved-code",
                json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
            )

        self.assertEqual(second_execution.status_code, 200, second_execution.text)
        self.assertEqual(second_execution.json()["status"], "completed")
        compile_graph.assert_called_once()
        invoked_state = compile_graph.return_value.invoke.call_args.args[0]
        self.assertEqual(invoked_state["execution_mode"], "graph_product_execute")
        graph_state, _ = _assert_run_projection(self, study_dir, "run_terminal_retry_allows_execute")
        adae_state = graph_state["datasets"]["ADAE"]
        self.assertEqual(adae_state["status"], "completed")
        self.assertEqual(adae_state["execution_state"]["terminal_failure_followup"]["action"], "retry_execution")
        self.assertEqual(adae_state["execution_state"]["terminal_failure_followup_consumed_by"], "execute")

    def test_terminal_failure_requires_repair_code_before_regenerating_code(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_repair_gate")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal_repair_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_repair_gate" / "code" / "build_adae.R"
        code_path.write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal_repair_gate", "ADAE", code_path)
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_repair_gate",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_repair_gate/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        executed = client.post(
            "/runs/run_terminal_repair_gate/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertTrue(executed.json()["terminal_failure"])

        blocked = client.post(
            "/runs/run_terminal_repair_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(blocked.status_code, 400, blocked.text)
        self.assertIn("Terminal failure must be reviewed", blocked.json()["detail"])

        triage = client.post(
            "/runs/run_terminal_repair_gate/datasets/ADAE/terminal-failure-review",
            json={
                "study_dir": str(study_dir),
                "decision": "repair_code",
                "reviewer": "tester",
                "notes": "Regenerate code from the approved spec.",
            },
        )
        self.assertEqual(triage.status_code, 200, triage.text)
        repaired = client.post(
            "/runs/run_terminal_repair_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(repaired.status_code, 200, repaired.text)
        graph_state = client.get(
            "/runs/run_terminal_repair_gate/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        adae_state = graph_state["datasets"]["ADAE"]
        self.assertEqual(adae_state["current_interrupt"]["name"], "code_review")
        self.assertEqual(adae_state["code_state"]["terminal_failure_followup"]["action"], "repair_code")

    def test_terminal_failure_retry_execution_does_not_unlock_code_regeneration(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_retry_no_regenerate")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal_retry_no_regenerate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_retry_no_regenerate" / "code" / "build_adae.R"
        code_path.write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal_retry_no_regenerate", "ADAE", code_path)
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_retry_no_regenerate",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_retry_no_regenerate/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        executed = client.post(
            "/runs/run_terminal_retry_no_regenerate/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertTrue(executed.json()["terminal_failure"])
        triage = client.post(
            "/runs/run_terminal_retry_no_regenerate/datasets/ADAE/terminal-failure-review",
            json={"study_dir": str(study_dir), "decision": "retry_execution", "reviewer": "tester"},
        )
        self.assertEqual(triage.status_code, 200, triage.text)

        blocked = client.post(
            "/runs/run_terminal_retry_no_regenerate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(blocked.status_code, 400, blocked.text)
        self.assertIn("generate_code requires repair_code", blocked.json()["detail"])
        workflow_state = json.loads(
            (study_dir / "runs" / "run_terminal_retry_no_regenerate" / "workflow_state.json").read_text(encoding="utf-8")
        )
        self.assertNotEqual(workflow_state["last_node"], "generate_code_start")
        self.assertEqual(workflow_state["projection_source"], "langgraph")
        _assert_run_projection(self, study_dir, "run_terminal_retry_no_regenerate")

    def test_terminal_failure_skip_dataset_does_not_unlock_code_regeneration(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_skip_no_regenerate")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal_skip_no_regenerate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_skip_no_regenerate" / "code" / "build_adae.R"
        code_path.write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal_skip_no_regenerate", "ADAE", code_path)
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_skip_no_regenerate",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_skip_no_regenerate/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        executed = client.post(
            "/runs/run_terminal_skip_no_regenerate/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertTrue(executed.json()["terminal_failure"])
        triage = client.post(
            "/runs/run_terminal_skip_no_regenerate/datasets/ADAE/terminal-failure-review",
            json={"study_dir": str(study_dir), "decision": "skip_dataset", "reviewer": "tester"},
        )
        self.assertEqual(triage.status_code, 200, triage.text)

        blocked = client.post(
            "/runs/run_terminal_skip_no_regenerate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(blocked.status_code, 400, blocked.text)
        self.assertIn("generate_code requires repair_code", blocked.json()["detail"])
        graph_state = client.get(
            "/runs/run_terminal_skip_no_regenerate/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        self.assertEqual(graph_state["datasets"]["ADAE"]["status"], "failed")
        _assert_run_projection(self, study_dir, "run_terminal_skip_no_regenerate")

    def test_terminal_failure_revise_spec_requires_finalize_before_regenerating_code(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_terminal_revise_spec_gate")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_revise_spec_gate" / "code" / "build_adae.R"
        code_path.write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal_revise_spec_gate", "ADAE", code_path)
        from adam_agent.graph.gateway import GraphGateway

        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_revise_spec_gate",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        executed = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertTrue(executed.json()["terminal_failure"])
        triage = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/terminal-failure-review",
            json={
                "study_dir": str(study_dir),
                "decision": "revise_spec",
                "reviewer": "tester",
                "notes": "Spec must be checked before code repair.",
            },
        )
        self.assertEqual(triage.status_code, 200, triage.text)

        blocked = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(blocked.status_code, 400, blocked.text)
        self.assertIn("generate_code requires repair_code", blocked.json()["detail"])

        finalized = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        self.assertEqual(finalized.json()["status"], "input_spec_ready")
        repaired = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(repaired.status_code, 200, repaired.text)
        graph_state = client.get(
            "/runs/run_terminal_revise_spec_gate/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        adae_state = graph_state["datasets"]["ADAE"]
        self.assertEqual(adae_state["current_interrupt"]["name"], "code_review")
        self.assertEqual(adae_state["spec_state"]["terminal_failure_followup"]["action"], "revise_spec")
        self.assertEqual(adae_state["execution_state"]["terminal_failure_followup_consumed_by"], "finalize_inputs")

    def test_terminal_failure_revise_spec_with_approved_draft_spec_generates_new_draft_and_clears_old_review(self) -> None:
        study_dir = _workspace_dir("phase8_terminal_revise_approved_draft") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        finalized = client.post(
            "/runs/run_terminal_revise_approved_draft/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        approved = client.post(
            "/runs/run_terminal_revise_approved_draft/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(approved.status_code, 200, approved.text)
        generated = client.post(
            "/runs/run_terminal_revise_approved_draft/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_terminal_revise_approved_draft" / "code" / "build_adae.R"
        code_path.write_text("dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)\n", encoding="utf-8")
        from adam_agent.graph.gateway import GraphGateway

        graph_state = client.get(
            "/runs/run_terminal_revise_approved_draft/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()
        code_state = graph_state["datasets"]["ADAE"]["code_state"]
        static_path, static_sha = _write_static_check_for_code(study_dir, "run_terminal_revise_approved_draft", "ADAE", code_path)
        GraphGateway().record_code_generation(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_terminal_revise_approved_draft",
            dataset="ADAE",
            code_path=code_path,
            code_sha256=f"sha256:{sha256_file(code_path)}",
            static_check_path=static_path,
            static_check_sha256=static_sha,
            spec_source=code_state["spec_source"],
            spec_path=code_state["spec_path"],
            spec_sha256=code_state["spec_sha256"],
            input_fingerprint_payload=input_fingerprint(study_dir),
        )
        review = client.post(
            "/runs/run_terminal_revise_approved_draft/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        executed = client.post(
            "/runs/run_terminal_revise_approved_draft/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/not/a/real/Rscript.exe"},
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertTrue(executed.json()["terminal_failure"])
        triage = client.post(
            "/runs/run_terminal_revise_approved_draft/datasets/ADAE/terminal-failure-review",
            json={"study_dir": str(study_dir), "decision": "revise_spec", "reviewer": "tester"},
        )
        self.assertEqual(triage.status_code, 200, triage.text)
        refinalized = client.post(
            "/runs/run_terminal_revise_approved_draft/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(refinalized.status_code, 200, refinalized.text)
        self.assertEqual(refinalized.json()["status"], "draft_spec_review_required")
        final_state = client.get(
            "/runs/run_terminal_revise_approved_draft/graph-state",
            params={"study_dir": str(study_dir)},
        ).json()["datasets"]["ADAE"]
        self.assertEqual(final_state["current_interrupt"]["name"], "draft_spec_review")
        self.assertEqual(final_state["status"], "needs_review")
        self.assertEqual(final_state["execution_state"]["terminal_failure_followup_consumed_by"], "draft_spec")
        self.assertNotIn("terminal_failure_review", final_state["execution_state"])

    def test_terminal_failure_output_is_not_previewed_or_downloadable(self) -> None:
        study_dir = _workspace_dir("phase8_terminal_failure_hidden_output") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_terminal_hidden"
        code_dir = run_dir / "code"
        review_dir = run_dir / "review"
        outputs_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        code_dir.mkdir(parents=True)
        review_dir.mkdir(parents=True)
        outputs_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (outputs_dir / "adae.csv").write_text("USUBJID,AETERM\n01,PARTIAL\n", encoding="utf-8")
        (validation_dir / "adae_validation_report.json").write_text(
            json.dumps({"dataset": "ADAE", "status": "fail", "terminal_failure": True, "partial_output_usable": False}),
            encoding="utf-8",
        )
        client = TestClient(create_app())

        table = client.get(
            "/runs/run_terminal_hidden/datasets/ADAE/table",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        download = client.get(
            "/runs/run_terminal_hidden/datasets/ADAE/download",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        review = client.get(
            "/runs/run_terminal_hidden/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(table.status_code, 200, table.text)
        self.assertEqual(table.json()["status"], "missing")
        self.assertEqual(download.status_code, 404)
        self.assertEqual(review.status_code, 200, review.text)
        self.assertIsNone(review.json()["dataset_reviews"][0]["output_preview"])
        self.assertIsNone(review.json()["dataset_reviews"][0]["output_path"])

    def test_code_approval_is_invalidated_when_code_or_inputs_change(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_stale_code_approval")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_stale_code/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review = client.post(
            "/runs/run_stale_code/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        code_path = study_dir / "runs" / "run_stale_code" / "code" / "build_adae.R"
        code_path.write_text(code_path.read_text(encoding="utf-8") + "\n# changed after approval\n", encoding="utf-8")

        changed_code = client.post(
            "/runs/run_stale_code/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(changed_code.status_code, 400)
        self.assertIn("Generated code changed after approval", changed_code.json()["detail"])

        review_again = client.post(
            "/runs/run_stale_code/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review_again.status_code, 400)
        self.assertIn("Generated code must be recorded in graph state", review_again.json()["detail"])
        regenerated = client.post(
            "/runs/run_stale_code/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(regenerated.status_code, 200, regenerated.text)
        review_again = client.post(
            "/runs/run_stale_code/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review_again.status_code, 200, review_again.text)
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        changed_inputs = client.post(
            "/runs/run_stale_code/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )
        self.assertEqual(changed_inputs.status_code, 400)
        self.assertIn("Code approval is stale", changed_inputs.json()["detail"])

    def test_code_review_does_not_write_approval_when_generated_code_changed(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_no_stale_code_review_file")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_no_stale_code_review_file/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        code_path = study_dir / "runs" / "run_no_stale_code_review_file" / "code" / "build_adae.R"
        review_path = study_dir / "runs" / "run_no_stale_code_review_file" / "review" / "adae_code_review.json"
        code_path.write_text(code_path.read_text(encoding="utf-8") + "\n# changed before review\n", encoding="utf-8")

        review = client.post(
            "/runs/run_no_stale_code_review_file/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("Generated code changed after graph code generation", review.json()["detail"])
        self.assertFalse(review_path.exists())

    def test_code_review_does_not_write_approval_when_inputs_changed_after_code_generation(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_no_stale_input_review_file")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_no_stale_input_review_file/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review_path = study_dir / "runs" / "run_no_stale_input_review_file" / "review" / "adae_code_review.json"
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        review = client.post(
            "/runs/run_no_stale_input_review_file/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("Study inputs changed after graph code generation", review.json()["detail"])
        self.assertFalse(review_path.exists())

    def test_product_steps_do_not_reprepare_and_overwrite_existing_graph_dataset_state(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_no_reprepare_overwrite")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_no_reprepare_overwrite/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        before = client.get(
            "/runs/run_no_reprepare_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(before.status_code, 200, before.text)
        before_code_state = before.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(before_code_state["status"], "generated")

        finalized = client.post(
            "/runs/run_no_reprepare_overwrite/datasets/ADAE/finalize-inputs",
            json={"study_dir": str(study_dir)},
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        after = client.get(
            "/runs/run_no_reprepare_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(after.status_code, 200, after.text)
        after_code_state = after.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(after_code_state["status"], "generated")
        self.assertEqual(after_code_state["code_sha256"], before_code_state["code_sha256"])

    def test_public_prepare_does_not_overwrite_existing_graph_dataset_state(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_prepare_no_overwrite")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_prepare_no_overwrite/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        before = client.get(
            "/runs/run_prepare_no_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(before.status_code, 200, before.text)
        before_code_state = before.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(before_code_state["status"], "generated")

        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_prepare_no_overwrite",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)
        after = client.get(
            "/runs/run_prepare_no_overwrite/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(after.status_code, 200, after.text)
        after_code_state = after.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(after_code_state["status"], "generated")
        self.assertEqual(after_code_state["code_sha256"], before_code_state["code_sha256"])

    def test_public_prepare_preserves_progress_for_other_target_in_same_run(self) -> None:
        study_dir = _study_with_adae_adcm_inputs("phase8_prepare_preserves_other_target")
        client = TestClient(create_app())
        adae_generated = client.post(
            "/runs/run_prepare_preserves_other_target/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(adae_generated.status_code, 200, adae_generated.text)
        before = client.get(
            "/runs/run_prepare_preserves_other_target/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(before.status_code, 200, before.text)
        before_code_state = before.json()["datasets"]["ADAE"]["code_state"]
        self.assertEqual(before_code_state["status"], "generated")

        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_prepare_preserves_other_target",
                "target_datasets": ["ADCM"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)
        after = client.get(
            "/runs/run_prepare_preserves_other_target/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(after.status_code, 200, after.text)
        state = after.json()
        self.assertIn("ADCM", state["datasets"])
        self.assertEqual(state["datasets"]["ADAE"]["code_state"]["status"], "generated")
        self.assertEqual(state["datasets"]["ADAE"]["code_state"]["code_sha256"], before_code_state["code_sha256"])
        self.assertIn("ADAE", state["target_datasets"])
        self.assertIn("ADCM", state["target_datasets"])

    def test_public_prepare_accepts_multi_target_plan_and_preserves_dataset_cards(self) -> None:
        study_dir = _study_with_adae_adcm_inputs("phase8_prepare_multi_target")
        client = TestClient(create_app())

        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_prepare_multi_target",
                "target_datasets": ["ADAE", "ADCM"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)
        payload = prepared.json()
        self.assertEqual(payload["target_datasets"], ["ADAE", "ADCM"])
        self.assertIn("ADAE", payload["runnable_datasets"])
        self.assertIn("ADCM", payload["runnable_datasets"])

        graph_state = client.get(
            "/runs/run_prepare_multi_target/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        state = graph_state.json()
        self.assertEqual(state["target_datasets"], ["ADAE", "ADCM"])
        self.assertIn("ADAE", state["datasets"])
        self.assertIn("ADCM", state["datasets"])

    def test_public_prepare_marks_existing_graph_dataset_state_stale_when_inputs_change(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_prepare_marks_stale")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_prepare_marks_stale/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n02,NAUSEA\n", encoding="utf-8")

        prepared = client.post(
            "/runs/prepare",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_prepare_marks_stale",
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)
        graph_state = client.get(
            "/runs/run_prepare_marks_stale/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        dataset_state = graph_state.json()["datasets"]["ADAE"]
        self.assertEqual(dataset_state["status"], "needs_review")
        self.assertEqual(dataset_state["current_interrupt"]["name"], "code_review")
        self.assertEqual(dataset_state["code_state"]["status"], "stale")
        self.assertIn("input_sdtm/ae.csv", dataset_state["code_state"]["input_diff"]["changed_files"])

    def test_code_review_requires_graph_generated_code_state(self) -> None:
        study_dir = _workspace_dir("phase8_code_review_requires_graph") / "MY_STUDY"
        code_dir = study_dir / "runs" / "run_no_graph_generation" / "code"
        code_dir.mkdir(parents=True)
        (code_dir / "build_adae.R").write_text("write.csv(data.frame(USUBJID='01'), 'outputs/adae.csv')\n", encoding="utf-8")
        client = TestClient(create_app())

        review = client.post(
            "/runs/run_no_graph_generation/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )

        self.assertEqual(review.status_code, 400)
        self.assertIn("Generated code must be recorded in graph state", review.json()["detail"])

    def test_execute_requires_graph_code_review_not_only_review_json(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_execute_requires_graph_review")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_execute_requires_graph_review/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        run_dir = study_dir / "runs" / "run_execute_requires_graph_review"
        code_path = run_dir / "code" / "build_adae.R"
        static_check_path = run_dir / "static_checks" / "adae_static_check.json"
        review_dir = run_dir / "review"
        review_dir.mkdir(exist_ok=True)
        graph_state = client.get(
            "/runs/run_execute_requires_graph_review/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        code_state = graph_state.json()["datasets"]["ADAE"]["code_state"]
        (review_dir / "adae_code_review.json").write_text(
            json.dumps(
                {
                    "study_id": study_dir.name,
                    "run_id": "run_execute_requires_graph_review",
                    "dataset": "ADAE",
                    "decision": "approve",
                    "approved": True,
                    "reviewer": "tester",
                    "input_fingerprint": input_fingerprint(study_dir),
                    "code_path": str(code_path.as_posix()),
                    "code_sha256": f"sha256:{sha256_file(code_path)}",
                    "static_check_path": str(static_check_path.as_posix()),
                    "static_check_sha256": f"sha256:{sha256_file(static_check_path)}",
                    "spec_source": code_state.get("spec_source"),
                    "spec_path": code_state.get("spec_path"),
                    "spec_sha256": code_state.get("spec_sha256"),
                }
            ),
            encoding="utf-8",
        )

        executed = client.post(
            "/runs/run_execute_requires_graph_review/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 400, executed.text)
        self.assertIn("Graph state does not contain an approved code-review decision", executed.json()["detail"])

    def test_execute_rejects_changed_graph_spec_artifact(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_execute_changed_graph_spec")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_changed_graph_spec/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review = client.post(
            "/runs/run_changed_graph_spec/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        spec_path = study_dir / "input_spec" / "adae.json"
        spec_payload = json.loads(spec_path.read_text(encoding="utf-8"))
        spec_payload["variables"].append({"variable": "TAMPERED", "source_domains": ["AE"]})
        spec_path.write_text(json.dumps(spec_payload), encoding="utf-8")

        executed = client.post(
            "/runs/run_changed_graph_spec/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 400, executed.text)
        self.assertIn("Code approval is stale", executed.json()["detail"])

    def test_execute_rejects_changed_runtime_dependency_artifact(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_runtime_dependency_hash")
        dependency_output_dir = study_dir / "runs" / "run_dependency_hash" / "outputs"
        dependency_output_dir.mkdir(parents=True)
        dependency_path = dependency_output_dir / "adsl.csv"
        dependency_path.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        from adam_agent.graph.gateway import GraphGateway
        from adam_agent.schemas.artifacts import ArtifactRef

        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id=study_dir.name,
            run_id="run_dependency_hash",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_dependency_hash").model_copy(deep=True)
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].execution_state.update(
            {
                "terminal_failure": False,
                "partial_output_usable": True,
                "output_path": str(dependency_path.as_posix()),
            }
        )
        state.datasets["ADSL"].artifacts.append(
            ArtifactRef(
                artifact_id="output_adam_psy201_run_dependency_hash_adsl",
                kind="output_adam",
                path=str(dependency_path.as_posix()),
                sha256=f"sha256:{sha256_file(dependency_path)}",
                dataset="ADSL",
                format="csv",
                role="output",
            )
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_completed_dependency_output")
        (study_dir / "input_spec" / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        client = TestClient(create_app())
        prepared = client.post(
            "/runs/prepare",
            json={"study_dir": str(study_dir), "run_id": "run_dependency_hash", "target_datasets": ["ADAE"]},
        )
        self.assertEqual(prepared.status_code, 200, prepared.text)

        generated = client.post(
            "/runs/run_dependency_hash/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        graph_state = client.get(
            "/runs/run_dependency_hash/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        dependency_artifacts = graph_state.json()["datasets"]["ADAE"]["code_state"]["dependency_artifacts"]
        self.assertEqual(dependency_artifacts[0]["required_dataset"], "ADSL")
        self.assertTrue(dependency_artifacts[0]["artifact_path"].endswith("outputs/adsl.csv"))
        review = client.post(
            "/runs/run_dependency_hash/datasets/ADAE/code-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(review.status_code, 200, review.text)
        dependency_path.write_text("USUBJID,TRTSDT\n01,2024-02-02\n", encoding="utf-8")

        executed = client.post(
            "/runs/run_dependency_hash/datasets/ADAE/execute-approved-code",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )

        self.assertEqual(executed.status_code, 400, executed.text)
        self.assertIn("Dependency artifact changed after graph approval", executed.json()["detail"])

    def test_code_review_cleans_approval_json_when_graph_recording_fails(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_review_cleanup_on_graph_failure")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_review_cleanup/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        review_path = study_dir / "runs" / "run_review_cleanup" / "review" / "adae_code_review.json"

        with patch("adam_agent.api.service.GraphGateway.record_code_review", side_effect=ValueError("forced graph failure")):
            review = client.post(
                "/runs/run_review_cleanup/datasets/ADAE/code-review",
                json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
            )

        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("forced graph failure", review.json()["detail"])
        self.assertFalse(review_path.exists())
        graph_state = client.get(
            "/runs/run_review_cleanup/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        self.assertEqual(graph_state.json()["datasets"]["ADAE"]["code_state"]["status"], "generated")

    def test_draft_spec_review_cleans_artifacts_when_graph_recording_fails(self) -> None:
        study_dir = _workspace_dir("phase8_draft_review_cleanup_on_graph_failure") / "MY_STUDY"
        client = TestClient(create_app())
        client.post("/studies/workspace", json={"study_dir": str(study_dir)})
        (study_dir / "input_sdtm" / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        finalized = client.post(
            "/runs/run_draft_review_cleanup/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(finalized.status_code, 200, finalized.text)
        review_path = study_dir / "runs" / "run_draft_review_cleanup" / "reviews" / "adae_draft_spec_review.json"
        approved_path = study_dir / "runs" / "run_draft_review_cleanup" / "approved_specs" / "adae_approved_spec.json"

        with patch("adam_agent.api.service.GraphGateway.record_draft_spec_review", side_effect=ValueError("forced graph failure")):
            review = client.post(
                "/runs/run_draft_review_cleanup/datasets/ADAE/draft-spec-review",
                json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
            )

        self.assertEqual(review.status_code, 400, review.text)
        self.assertIn("forced graph failure", review.json()["detail"])
        self.assertFalse(review_path.exists())
        self.assertFalse(approved_path.exists())
        graph_state = client.get(
            "/runs/run_draft_review_cleanup/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        self.assertEqual(graph_state.json()["datasets"]["ADAE"]["spec_state"]["status"], "draft_generated")

    def test_review_summary_recovers_multiple_outputs_from_same_run(self) -> None:
        study_dir = _workspace_dir("phase8_multi_output_review") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_multi_output"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (output_dir / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        for dataset in ["ADSL", "ADAE"]:
            (validation_dir / f"{dataset.lower()}_validation_report.json").write_text(
                json.dumps(
                    {
                        "dataset": dataset,
                        "status": "pass",
                        "terminal_failure": False,
                        "partial_output_usable": True,
                    }
                ),
                encoding="utf-8",
            )
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_multi_output/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        reviews = response.json()["dataset_reviews"]
        self.assertEqual({item["dataset"] for item in reviews}, {"ADSL", "ADAE"})
        for item in reviews:
            self.assertEqual(item["status"], "completed")
            self.assertTrue(item["output_preview"])
            self.assertTrue(item["output_path"].endswith(f"{item['dataset'].lower()}.csv"))

    def test_llm_connection_test_rejects_mock_provider(self) -> None:
        client = TestClient(create_app())

        response = client.post(
            "/llm/test-connection",
            json={
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "llm_exposure": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("real provider", response.json()["detail"])

    def test_llm_connection_test_uses_browser_scoped_provider_settings(self) -> None:
        client = TestClient(create_app())
        requests = []

        class FakeLLMClient:
            def generate(self, request):
                requests.append(request)
                return SimpleNamespace(
                    response_text="ADAM_AGENT_CONNECTION_OK",
                    call_record=SimpleNamespace(
                        provider_alias="openai-compatible",
                        transport="fake-transport",
                        provider_base_url="https://relay.example/v1",
                        external_relay=True,
                        risk_flags=["external_relay"],
                    ),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeLLMClient()) as builder:
            response = client.post(
                "/llm/test-connection",
                json={
                    "llm_provider": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "base_url": "https://relay.example/v1",
                        "api_key": "test-key",
                        "allow_custom_base_url": True,
                        "custom_base_url_approved_by": "tester",
                    },
                    "llm_exposure": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 3,
                        "include_reference_rows": True,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "ok")
        self.assertEqual(payload["model"], "gpt-5.5")
        self.assertTrue(payload["external_relay"])
        self.assertEqual(payload["risk_flags"], ["external_relay"])
        self.assertEqual(len(requests), 1)
        self.assertEqual(requests[0].model, "gpt-5.5")
        self.assertEqual(requests[0].exposure.mode, "demo_rich_context")
        builder.assert_called_once()
        provider_config = builder.call_args.args[0]
        self.assertEqual(provider_config.api_key, "test-key")

    def test_generate_code_uses_browser_scoped_real_provider_settings(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_real_provider_override")
        client = TestClient(create_app())
        requests = []

        class FakeLLMClient:
            def generate(self, request):
                requests.append(request)
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(USUBJID='01'), 'outputs/adae.csv', row.names = FALSE)",
                            "assumptions": ["Test response."],
                            "risk_points": [],
                            "used_inputs": ["AE"],
                            "expected_outputs": ["outputs/adae.csv"],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeLLMClient()) as builder:
            response = client.post(
                "/runs/run_real_provider_override/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "base_url": "https://relay.example/v1",
                        "api_key": "test-key",
                        "max_tokens": 1234,
                        "allow_custom_base_url": True,
                        "custom_base_url_approved_by": "tester",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 3,
                        "include_reference_rows": True,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        self.assertEqual(response.json()["status"], "code_generated")
        builder.assert_called_once()
        self.assertEqual(builder.call_args.args[0].provider, "openai-compatible")
        self.assertEqual(builder.call_args.args[0].model, "gpt-5.5")
        self.assertEqual(builder.call_args.args[0].api_key, "test-key")
        self.assertEqual(len(requests), 1)
        self.assertEqual(requests[0].model, "gpt-5.5")
        self.assertEqual(requests[0].max_tokens, 1234)
        self.assertEqual(requests[0].exposure.mode, "demo_rich_context")
        self.assertIn("## Target Spec", requests[0].prompt)
        self.assertNotIn('"target_spec"', requests[0].prompt)
        self.assertTrue(requests[0].prompt_artifact_id.startswith("llm_prompt_compact_"))
        self.assertTrue(
            (study_dir / "runs" / "run_real_provider_override" / "llm" / "adae_compact_prompt.txt").exists()
        )

    def test_missing_input_spec_requires_draft_spec_approval_before_code_generation(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_missing_spec_draft")
        client = TestClient(create_app())
        requests = []

        class FakeLLMClient:
            def generate(self, request):
                requests.append(request)
                if request.node == "draft_spec_from_evidence":
                    return SimpleNamespace(
                        response_text=json.dumps(
                            {
                                "dataset": "ADAE",
                                "variables": [
                                    {
                                        "variable": "AETERM",
                                        "label": "Reported Term for the Adverse Event",
                                        "type": "character",
                                        "source_domains": ["AE"],
                                        "source_variables": ["AETERM"],
                                        "derivation": "Copy from AE.AETERM based on legacy ADAE.sas evidence.",
                                        "confidence": 0.7,
                                        "review_required": True,
                                        "review_reasons": ["Generated from legacy SAS because approved spec is missing."],
                                        "risk_level": "high",
                                        "assumptions": ["Legacy SAS is treated as evidence, not directly executed."],
                                    }
                                ],
                            }
                        ),
                        call_record=SimpleNamespace(),
                    )
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "r_code": "dir.create('outputs', showWarnings = FALSE)\nwrite.csv(data.frame(AETERM='HEADACHE'), 'outputs/adae.csv', row.names = FALSE)",
                            "assumptions": ["Used generated draft spec."],
                            "risk_points": ["Draft spec requires human review."],
                            "used_inputs": ["AE"],
                            "expected_outputs": ["outputs/adae.csv"],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeLLMClient()):
            blocked = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )
            draft_response = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/draft-spec",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )
            review_response = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/draft-spec-review",
                json={
                    "study_dir": str(study_dir),
                    "reviewer": "tester",
                    "decision": "approve",
                    "notes": "Approved draft spec for this test run.",
                },
            )
            response = client.post(
                "/runs/run_missing_spec_draft/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )

        self.assertEqual(blocked.status_code, 400, blocked.text)
        self.assertIn("Generate and approve a draft spec", blocked.json()["detail"])
        self.assertEqual(draft_response.status_code, 200, draft_response.text)
        draft_payload = draft_response.json()
        self.assertEqual(draft_payload["status"], "draft_spec_generated")
        _assert_compatibility_projection(self, draft_payload)
        self.assertTrue(draft_payload["spec_path"].endswith("specs/adae_draft_spec.json"))
        self.assertEqual(review_response.status_code, 200, review_response.text)
        review_payload = review_response.json()
        self.assertTrue(review_payload["approved"])
        _assert_compatibility_projection(self, review_payload)
        graph_state = client.get(
            "/runs/run_missing_spec_draft/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        adae_state = graph_state.json()["datasets"]["ADAE"]
        self.assertEqual(adae_state["spec_state"]["status"], "approved")
        self.assertEqual(adae_state["human_commands"][-1]["interrupt"], "draft_spec_review")
        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "code_generated")
        self.assertTrue(payload["draft_spec_path"].endswith("approved_specs/adae_approved_spec.json"))
        self.assertTrue(any("user-approved draft spec" in warning for warning in payload["warnings"]))
        self.assertEqual([request.node for request in requests], ["draft_spec_from_evidence", "generate_downstream_code_for_review"])
        draft_spec = json.loads((study_dir / "runs" / "run_missing_spec_draft" / "specs" / "adae_draft_spec.json").read_text(encoding="utf-8"))
        self.assertEqual(draft_spec["dataset"], "ADAE")
        self.assertEqual(draft_spec["status"], "draft")
        self.assertEqual(draft_spec["variables"][0]["variable"], "AETERM")
        compact_prompt = (study_dir / "runs" / "run_missing_spec_draft" / "llm" / "adae_compact_prompt.txt").read_text(encoding="utf-8")
        self.assertIn("AETERM|character|AE.AETERM|Copy from AE.AETERM", compact_prompt)
        self.assertIn("user-approved draft spec", compact_prompt)

    def test_finalize_inputs_uses_existing_input_spec_without_draft_generation(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_finalize_existing_spec")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_finalize_existing_spec/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "input_spec_ready")
        _assert_compatibility_projection(self, payload)
        self.assertTrue(payload["input_spec_available"])
        self.assertFalse(payload["draft_spec_required"])
        self.assertEqual(payload["next_action"], "generate_code")
        self.assertTrue(payload["input_spec_path"].endswith("input_spec/adae.json"))
        self.assertIsNone(payload["draft_spec"])
        self.assertFalse((study_dir / "runs" / "run_finalize_existing_spec" / "specs" / "adae_draft_spec.json").exists())

    def test_finalize_inputs_generates_review_required_draft_spec_when_spec_missing(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_finalize_missing_spec")
        client = TestClient(create_app())
        requests = []

        class FakeDraftLLMClient:
            def generate(self, request):
                requests.append(request)
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "variables": [
                                {
                                    "variable": "AETERM",
                                    "label": "Reported Term",
                                    "type": "character",
                                    "source_domains": ["AE"],
                                    "source_variables": ["AETERM"],
                                    "derivation": "Copy from AE.AETERM based on uploaded evidence.",
                                    "confidence": 0.7,
                                    "review_required": True,
                                    "review_reasons": ["Generated because approved input_spec is missing."],
                                    "risk_level": "high",
                                }
                            ],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        with patch("adam_agent.api.service.build_llm_client", return_value=FakeDraftLLMClient()):
            response = client.post(
                "/runs/run_finalize_missing_spec/datasets/ADAE/finalize-inputs",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "draft_spec_review_required")
        _assert_compatibility_projection(self, payload)
        self.assertFalse(payload["input_spec_available"])
        self.assertTrue(payload["draft_spec_required"])
        self.assertTrue(payload["draft_spec_generated"])
        self.assertEqual(payload["next_action"], "review_draft_spec")
        self.assertIsNotNone(payload["draft_spec"])
        self.assertEqual(payload["draft_spec"]["workflow_control"], "graph_gateway_compatibility_shim")
        self.assertEqual(payload["draft_spec"]["variables"][0]["variable"], "AETERM")
        self.assertEqual([request.node for request in requests], ["draft_spec_from_evidence"])
        self.assertTrue((study_dir / "runs" / "run_finalize_missing_spec" / "specs" / "adae_draft_spec.json").exists())
        graph_state = client.get(
            "/runs/run_finalize_missing_spec/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        adae_state = graph_state.json()["datasets"]["ADAE"]
        self.assertEqual(adae_state["current_interrupt"]["name"], "draft_spec_review")
        self.assertEqual(adae_state["spec_state"]["status"], "draft_generated")

    def test_finalize_inputs_accepts_mock_model_alias_from_ui(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_finalize_mock_alias")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_finalize_mock_alias/datasets/ADAE/finalize-inputs",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "llm_provider_override": {"provider": "mock", "model": "mock"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "draft_spec_review_required")
        self.assertEqual(payload["draft_spec"]["dataset"], "ADAE")
        self.assertTrue(payload["draft_spec"]["variables"])

    def test_finalize_inputs_passes_rscript_path_to_context_builder(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_finalize_rscript_path")
        client = TestClient(create_app())
        rscript_paths = []

        class FakeDraftLLMClient:
            def generate(self, _request):
                return SimpleNamespace(
                    response_text=json.dumps(
                        {
                            "dataset": "ADAE",
                            "variables": [
                                {
                                    "variable": "AETERM",
                                    "label": "Reported Term",
                                    "type": "character",
                                    "source_domains": ["AE"],
                                    "source_variables": ["AETERM"],
                                    "derivation": "Copy from AE.AETERM.",
                                    "confidence": 0.7,
                                    "review_required": True,
                                    "risk_level": "high",
                                }
                            ],
                        }
                    ),
                    call_record=SimpleNamespace(),
                )

        from adam_agent.api import service as service_module

        original_builder = service_module.build_target_llm_context

        def tracking_builder(*args, **kwargs):
            rscript_paths.append(kwargs.get("rscript_path"))
            return original_builder(*args, **kwargs)

        with (
            patch("adam_agent.api.service.build_llm_client", return_value=FakeDraftLLMClient()),
            patch("adam_agent.api.service.build_target_llm_context", side_effect=tracking_builder),
        ):
            response = client.post(
                "/runs/run_finalize_rscript_path/datasets/ADAE/finalize-inputs",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe",
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                        "sample_rows_per_dataset": 1,
                    },
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        self.assertTrue(rscript_paths)
        self.assertTrue(all(path == "C:/Dev/R-4.5.2/bin/Rscript.exe" for path in rscript_paths))

    def test_generate_code_returns_readable_error_for_bad_llm_json(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_bad_llm_json")
        client = TestClient(create_app())

        class BadJsonLLMClient:
            def generate(self, _request):
                return SimpleNamespace(response_text="not json", call_record=SimpleNamespace())

        with patch("adam_agent.api.service.build_llm_client", return_value=BadJsonLLMClient()):
            response = client.post(
                "/runs/run_bad_llm_json/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {
                        "provider": "openai-compatible",
                        "model": "gpt-5.5",
                        "api_key": "test-key",
                    },
                    "llm_exposure_override": {
                        "mode": "demo_rich_context",
                        "data_classification": "processed_demo",
                        "external_api_allowed": True,
                        "approved_by": "tester",
                    },
                },
            )

        self.assertEqual(response.status_code, 400)
        self.assertIn("not valid JSON", response.json()["detail"])

    def test_create_run_rejects_llm_run_to_completion(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_api_create_run")
        client = TestClient(create_app())

        response = client.post(
            "/runs",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_phase8_api",
                "target_datasets": ["ADAE"],
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                "execution_mode": "llm_downstream_provider",
            },
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("Use /runs/prepare", response.json()["detail"])
        state = json.loads((study_dir / "runs" / "run_phase8_api" / "workflow_state.json").read_text(encoding="utf-8"))
        self.assertEqual(state["status"], "blocked")
        self.assertEqual(state["current_interrupt"], "split_flow_required")
        self.assertEqual(state["workflow_control"], "legacy_run_to_completion_compatibility_shim")
        self.assertEqual(state["legacy_endpoint"], "POST /runs")
        self.assertTrue(state["product_flow_required"])
        self.assertIsNone(state["graph_state_path"])

    def test_create_run_rejects_missing_study_dir(self) -> None:
        client = TestClient(create_app())

        response = client.post(
            "/runs",
            json={
                "study_dir": str(ROOT / ".tmp_tests" / "does_not_exist"),
                "run_id": "run_missing",
                "target_datasets": ["ADAE"],
            },
        )

        self.assertEqual(response.status_code, 400)
        self.assertIn("study_dir does not exist", response.json()["detail"])

    def test_artifact_endpoint_blocks_path_escape(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_api_path_escape")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_x/artifacts/read",
            params={"study_dir": str(study_dir)},
            json={"relative_path": "../outside.json"},
        )

        self.assertEqual(response.status_code, 404)


def _study_with_adae_inputs(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    input_spec = study_dir / "input_spec"
    reference_adam = study_dir / "reference_adam"
    input_sdtm.mkdir(parents=True)
    input_spec.mkdir()
    reference_adam.mkdir()
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (input_spec / "adae.json").write_text(
        json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
        encoding="utf-8",
    )
    (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
    return study_dir


def _study_with_blocked_adae_dependency(name: str) -> Path:
    study_dir = _workspace_dir(name) / "MY_STUDY"
    sdtm_dir = study_dir / "input_sdtm"
    spec_dir = study_dir / "input_spec"
    legacy_dir = study_dir / "legacy_code"
    output_dir = study_dir / "runs" / "run_prior_adlb_output" / "outputs"
    sdtm_dir.mkdir(parents=True)
    spec_dir.mkdir()
    legacy_dir.mkdir()
    output_dir.mkdir(parents=True)
    (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (output_dir / "adlb.csv").write_text("USUBJID,PARAMCD\n01,ALT\n", encoding="utf-8")
    (spec_dir / "adae.json").write_text(
        json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
        encoding="utf-8",
    )
    (legacy_dir / "ADAE.sas").write_text("data adae; merge ae adsl; by usubjid; run;", encoding="utf-8")
    return study_dir


def _study_with_adae_adcm_inputs(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    input_spec = study_dir / "input_spec"
    reference_adam = study_dir / "reference_adam"
    input_sdtm.mkdir(parents=True)
    input_spec.mkdir()
    reference_adam.mkdir()
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (input_sdtm / "cm.csv").write_text("USUBJID,CMTRT\n01,ASPIRIN\n", encoding="utf-8")
    (input_spec / "adae.json").write_text(
        json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
        encoding="utf-8",
    )
    (input_spec / "adcm.json").write_text(
        json.dumps({"dataset": "ADCM", "variables": [{"variable": "CMTRT", "source_domains": ["CM"]}]}),
        encoding="utf-8",
    )
    (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
    return study_dir


def _study_with_adsl_inputs(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    input_spec = study_dir / "input_spec"
    reference_adam = study_dir / "reference_adam"
    input_sdtm.mkdir(parents=True)
    input_spec.mkdir()
    reference_adam.mkdir()
    (input_sdtm / "dm.csv").write_text(
        "STUDYID,USUBJID,SUBJID,ARM,ACTARM\nS1,01,1001,Placebo,Placebo\n",
        encoding="utf-8",
    )
    (input_sdtm / "ex.csv").write_text("USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n", encoding="utf-8")
    (input_spec / "adsl.json").write_text(
        json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
        encoding="utf-8",
    )
    (reference_adam / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
    return study_dir


def _study_without_spec_with_auxiliary_evidence(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    reference_adam = study_dir / "reference_adam"
    legacy_code = study_dir / "legacy_code"
    input_sdtm.mkdir(parents=True)
    reference_adam.mkdir()
    legacy_code.mkdir()
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM,AESTDTC\n01,HEADACHE,2024-01-02\n", encoding="utf-8")
    (reference_adam / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (legacy_code / "ADAE.sas").write_text(
        "proc sql; create table adae as select usubjid, aeterm from sdtmdata.ae; quit;\n",
        encoding="utf-8",
    )
    return study_dir


def _demo_source(name: str) -> Path:
    source = _workspace_dir(name) / "demo-data"
    source.mkdir(parents=True)
    (source / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (source / "dm.csv").write_text("USUBJID,SUBJID\n01,1001\n", encoding="utf-8")
    (source / "ex.csv").write_text("USUBJID,EXSTDTC\n01,2024-01-01\n", encoding="utf-8")
    (source / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
    (source / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (source / "ads_adae_full.csv").write_text(
        "Dataset,Variable,Label,Type,Source,Derivation\n"
        "ADAE,USUBJID,Unique Subject Identifier,Copied,SDTM.AE.USUBJID,Copied from source\n"
        "ADAE,TRTSDT,Treatment Start Date,Copied,ADSL.TRTSDT,Copied from ADSL\n",
        encoding="utf-8",
    )
    (source / "ads_adsl_full.csv").write_text(
        "Dataset,Variable,Label,Type,Source,Derivation\n"
        "ADSL,USUBJID,Unique Subject Identifier,Copied,SDTM.DM.USUBJID,Copied from source\n",
        encoding="utf-8",
    )
    return source


if __name__ == "__main__":
    unittest.main()
