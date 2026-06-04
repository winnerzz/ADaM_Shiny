"""Tests for the Phase 8.1 FastAPI backend boundary."""

from __future__ import annotations

import ast
import csv
import inspect
import json
import os
import subprocess
import sys
import textwrap
import tomllib
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
    from adam_agent.graph.gateway import GraphGateway
    from adam_agent.graph.workflow_state import input_fingerprint, workflow_projection_consistency
    from adam_agent.tools.artifacts import sha256_file
    from adam_agent.tools.static_rules import run_generated_r_static_checks, write_static_rule_report
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app
    from adam_agent.graph.gateway import GraphGateway
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
    def setUp(self) -> None:
        self._old_backend = os.environ.get("ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND")
        os.environ["ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND"] = "memory"

    def tearDown(self) -> None:
        if self._old_backend is None:
            os.environ.pop("ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND", None)
        else:
            os.environ["ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND"] = self._old_backend

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

    def test_service_layer_no_longer_writes_workflow_state_directly(self) -> None:
        from adam_agent.api import service

        self.assertFalse(
            hasattr(service, "_graph_compatibility_metadata"),
            "Service must not synthesize graph/workflow paths after gateway-owned product actions.",
        )
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
            [],
            "Service helpers must delegate state changes to GraphGateway instead of writing workflow_state directly.",
        )

    def test_service_layer_constructs_graph_gateway_only_through_factory(self) -> None:
        from adam_agent.api import service

        direct_gateway_callers: list[str] = []
        for name, obj in vars(service).items():
            if name.startswith("__") or not inspect.isfunction(obj) or obj.__module__ != service.__name__:
                continue
            if name == "_new_graph_gateway":
                continue
            tree = ast.parse(textwrap.dedent(inspect.getsource(obj)))
            calls_gateway_directly = any(
                isinstance(node, ast.Call)
                and isinstance(node.func, ast.Name)
                and node.func.id == "GraphGateway"
                for node in ast.walk(tree)
            )
            if calls_gateway_directly:
                direct_gateway_callers.append(name)

        self.assertEqual(
            direct_gateway_callers,
            [],
            "Service helpers should construct GraphGateway only through _new_graph_gateway().",
        )
        factory_tree = ast.parse(textwrap.dedent(inspect.getsource(service._new_graph_gateway)))
        factory_calls = {
            node.func.id
            for node in ast.walk(factory_tree)
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
        }
        self.assertIn("GraphGateway", factory_calls)

    def test_service_layer_opens_graph_gateway_through_context_helper(self) -> None:
        from adam_agent.api import service

        direct_factory_callers: list[str] = []
        for name, obj in vars(service).items():
            if name.startswith("__") or not inspect.isfunction(obj) or obj.__module__ != service.__name__:
                continue
            if name in {"_new_graph_gateway", "_open_graph_gateway"}:
                continue
            tree = ast.parse(textwrap.dedent(inspect.getsource(obj)))
            calls_factory_directly = any(
                isinstance(node, ast.Call)
                and isinstance(node.func, ast.Name)
                and node.func.id == "_new_graph_gateway"
                for node in ast.walk(tree)
            )
            if calls_factory_directly:
                direct_factory_callers.append(name)

        self.assertEqual(
            direct_factory_callers,
            [],
            "Service helpers should open GraphGateway through _open_graph_gateway() so resources are closed.",
        )

    def test_open_graph_gateway_closes_gateway_after_use(self) -> None:
        from adam_agent.api import service

        with patch("adam_agent.api.service.GraphGateway") as gateway_cls:
            with service._open_graph_gateway(
                study_dir="D:/tmp/study",
                run_id="run_context_close",
            ) as gateway:
                self.assertIs(gateway, gateway_cls.return_value)
                gateway.close.assert_not_called()

            gateway.close.assert_called_once_with()

    def test_service_gateway_factory_defaults_to_run_scoped_sqlite_backend(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_service_gateway_default_sqlite") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        with patch.dict("os.environ", {}, clear=True), patch("adam_agent.api.service.GraphGateway") as gateway_cls:
            gateway = service._new_graph_gateway(
                study_dir=study_dir,
                run_id="run_factory_default",
            )

        self.assertIs(gateway, gateway_cls.return_value)
        gateway_cls.assert_called_once()
        _, kwargs = gateway_cls.call_args
        self.assertEqual(kwargs["checkpointer_backend"], "sqlite")
        sqlite_path = str(kwargs["sqlite_checkpointer_path"]).replace("\\", "/")
        self.assertTrue(sqlite_path.endswith("runs/run_factory_default/langgraph_checkpoints.sqlite"))

    def test_default_sqlite_checkpointer_dependency_is_base_dependency(self) -> None:
        project = tomllib.loads((ROOT / "pyproject.toml").read_text(encoding="utf-8"))["project"]
        dependencies = project["dependencies"]

        self.assertIn("langgraph-checkpoint-sqlite>=3.0.3,<3.1", dependencies)

    def test_service_gateway_factory_uses_run_scoped_sqlite_path_when_enabled(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_service_gateway_sqlite") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        env = {
            "ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "sqlite",
            "ADAM_AGENT_GRAPH_CHECKPOINTER_SQLITE_PATH": "D:/tmp/shared_should_not_be_used.sqlite",
        }
        with patch.dict("os.environ", env, clear=True), patch("adam_agent.api.service.GraphGateway") as gateway_cls:
            gateway = service._new_graph_gateway(
                study_dir=study_dir,
                run_id="run_factory_sqlite",
            )

        self.assertIs(gateway, gateway_cls.return_value)
        gateway_cls.assert_called_once()
        _, kwargs = gateway_cls.call_args
        self.assertEqual(kwargs["checkpointer_backend"], "sqlite")
        sqlite_path = str(kwargs["sqlite_checkpointer_path"]).replace("\\", "/")
        self.assertTrue(sqlite_path.endswith("runs/run_factory_sqlite/langgraph_checkpoints.sqlite"))
        self.assertNotIn("shared_should_not_be_used", sqlite_path)

    def test_service_gateway_factory_falls_back_to_memory_without_run_context(self) -> None:
        from adam_agent.api import service

        env = {
            "ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "sqlite",
            "ADAM_AGENT_GRAPH_CHECKPOINTER_SQLITE_PATH": "D:/tmp/shared_should_not_be_used.sqlite",
        }
        with patch.dict("os.environ", env, clear=True), patch("adam_agent.api.service.GraphGateway") as gateway_cls:
            gateway = service._new_graph_gateway()

        self.assertIs(gateway, gateway_cls.return_value)
        gateway_cls.assert_called_once_with()

    def test_service_gateway_factory_rejects_unknown_backend(self) -> None:
        from adam_agent.api import service

        with patch.dict("os.environ", {"ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "bogus"}, clear=True):
            with self.assertRaisesRegex(service.ApiServiceError, "Unsupported service GraphGateway checkpointer backend"):
                service._new_graph_gateway(study_dir="D:/tmp/study", run_id="run_unknown_backend")

    def test_prepare_endpoint_fails_closed_when_sqlite_checkpointer_unavailable(self) -> None:
        study_dir = _workspace_dir("phase8_service_gateway_sqlite_api_unavailable") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        client = TestClient(create_app())
        env = {"ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "sqlite"}
        original_find_spec = __import__("importlib").util.find_spec

        def find_spec_without_sqlite(name: str) -> Any:
            if name == "langgraph.checkpoint.sqlite":
                return None
            return original_find_spec(name)

        with (
            patch.dict("os.environ", env, clear=True),
            patch("adam_agent.graph.checkpointing.importlib.util.find_spec", side_effect=find_spec_without_sqlite),
        ):
            response = client.post(
                "/runs/prepare",
                json={
                    "study_dir": str(study_dir),
                    "study_id": "MY_STUDY",
                    "run_id": "run_sqlite_unavailable",
                    "target_datasets": ["ADAE"],
                },
            )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("SQLite LangGraph checkpointer is not installed", response.json()["detail"])
        self.assertFalse((study_dir / "runs" / "run_sqlite_unavailable" / "graph_state.json").exists())
        self.assertFalse((study_dir / "runs" / "run_sqlite_unavailable" / "workflow_state.json").exists())
        self.assertFalse((study_dir / "runs" / "run_sqlite_unavailable" / "langgraph_checkpoints.sqlite").exists())

    def test_split_flow_generate_code_uses_default_sqlite_and_fails_closed_when_unavailable(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_split_flow_sqlite_unavailable")
        client = TestClient(create_app())
        original_find_spec = __import__("importlib").util.find_spec

        def find_spec_without_sqlite(name: str) -> Any:
            if name == "langgraph.checkpoint.sqlite":
                return None
            return original_find_spec(name)

        with (
            patch.dict("os.environ", {}, clear=True),
            patch("adam_agent.graph.checkpointing.importlib.util.find_spec", side_effect=find_spec_without_sqlite),
        ):
            response = client.post(
                "/runs/run_split_flow_sqlite_unavailable/datasets/ADAE/generate-code",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                },
            )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("SQLite LangGraph checkpointer is not installed", response.json()["detail"])
        self.assertFalse((study_dir / "runs" / "run_split_flow_sqlite_unavailable" / "graph_state.json").exists())
        self.assertFalse((study_dir / "runs" / "run_split_flow_sqlite_unavailable" / "workflow_state.json").exists())
        self.assertFalse((study_dir / "runs" / "run_split_flow_sqlite_unavailable" / "langgraph_checkpoints.sqlite").exists())

    def test_prepare_endpoint_fails_closed_when_postgres_checkpointer_unavailable(self) -> None:
        study_dir = _workspace_dir("phase8_service_gateway_postgres_api_unavailable") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        client = TestClient(create_app())
        env = {"ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "postgres"}
        original_find_spec = __import__("importlib").util.find_spec

        def find_spec_without_postgres(name: str) -> Any:
            if name == "langgraph.checkpoint.postgres":
                return None
            return original_find_spec(name)

        with (
            patch.dict("os.environ", env, clear=True),
            patch("adam_agent.graph.checkpointing.importlib.util.find_spec", side_effect=find_spec_without_postgres),
        ):
            response = client.post(
                "/runs/prepare",
                json={
                    "study_dir": str(study_dir),
                    "study_id": "MY_STUDY",
                    "run_id": "run_postgres_unavailable",
                    "target_datasets": ["ADAE"],
                },
            )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("Postgres LangGraph checkpointer is not installed", response.json()["detail"])
        self.assertFalse((study_dir / "runs" / "run_postgres_unavailable" / "graph_state.json").exists())
        self.assertFalse((study_dir / "runs" / "run_postgres_unavailable" / "workflow_state.json").exists())
        self.assertFalse((study_dir / "runs" / "run_postgres_unavailable" / "langgraph_checkpoints.sqlite").exists())

    def test_service_layer_reads_graph_state_only_for_explicit_read_models(self) -> None:
        from adam_agent.api import service

        graph_state_readers: list[str] = []
        for name, obj in vars(service).items():
            if name.startswith("__") or not inspect.isfunction(obj) or obj.__module__ != service.__name__:
                continue
            tree = ast.parse(textwrap.dedent(inspect.getsource(obj)))
            calls_load_graph_state = any(
                isinstance(node, ast.Call)
                and isinstance(node.func, ast.Attribute)
                and node.func.attr == "load_graph_state"
                for node in ast.walk(tree)
            )
            if calls_load_graph_state:
                graph_state_readers.append(name)

        self.assertEqual(
            sorted(graph_state_readers),
            ["_load_read_model_graph_state", "read_run_graph_state"],
            "Service helpers should not inspect graph internals except explicit graph-state/read-model endpoints.",
        )
        review_summary_tree = ast.parse(textwrap.dedent(inspect.getsource(service.build_run_review_summary)))
        review_summary_calls = {
            node.func.id
            for node in ast.walk(review_summary_tree)
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
        }
        self.assertIn("_load_read_model_graph_state", review_summary_calls)

    def test_progress_endpoint_uses_graph_gateway_progress_read_model(self) -> None:
        from adam_agent.api import service

        tree = ast.parse(textwrap.dedent(inspect.getsource(service.read_run_progress)))
        called_names: set[str] = set()
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call):
                continue
            if isinstance(node.func, ast.Attribute):
                called_names.add(node.func.attr)
            elif isinstance(node.func, ast.Name):
                called_names.add(node.func.id)

        self.assertIn("progress_summary", called_names)
        self.assertNotIn("load_graph_state", called_names)
        self.assertNotIn("project_graph_state_to_workflow", called_names)

    def test_dependency_review_response_uses_gateway_projection_paths(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_dependency_projection_paths") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        gateway_result = SimpleNamespace(
            graph_state=SimpleNamespace(study_id="MY_STUDY"),
            workflow_projection={
                "workflow_control": "graph_gateway_compatibility_shim",
                "graph_state_path": "sentinel/graph_state.json",
                "workflow_state_path": "sentinel/workflow_state.json",
            },
            decision="approve",
            approved=True,
            current_interrupt=None,
        )

        with patch("adam_agent.api.service.GraphGateway") as gateway_cls:
            gateway_cls.return_value.review_dependency.return_value = gateway_result
            response = service.persist_dependency_review(
                "run_projection_paths",
                SimpleNamespace(
                    study_dir=str(study_dir),
                    reviewer="tester",
                    decision="approve",
                    notes="projection path test",
                    approved_dependency_datasets=[],
                ),
            )

        self.assertEqual(response.graph_state_path, "sentinel/graph_state.json")
        self.assertEqual(response.workflow_state_path, "sentinel/workflow_state.json")

    def test_terminal_failure_review_response_uses_gateway_projection_paths(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_terminal_projection_paths") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        gateway_result = SimpleNamespace(
            graph_state=SimpleNamespace(study_id="MY_STUDY"),
            workflow_projection={
                "workflow_control": "graph_gateway_compatibility_shim",
                "graph_state_path": "sentinel/terminal_graph_state.json",
                "workflow_state_path": "sentinel/terminal_workflow_state.json",
            },
            decision="skip_dataset",
            current_interrupt=None,
            next_action="review_summary",
        )

        with patch("adam_agent.api.service.GraphGateway") as gateway_cls:
            gateway_cls.return_value.review_terminal_failure.return_value = gateway_result
            response = service.persist_terminal_failure_review(
                "run_terminal_projection_paths",
                "ADAE",
                SimpleNamespace(
                    study_dir=str(study_dir),
                    reviewer="tester",
                    decision="skip_dataset",
                    notes="projection path test",
                ),
            )

        self.assertEqual(response.graph_state_path, "sentinel/terminal_graph_state.json")
        self.assertEqual(response.workflow_state_path, "sentinel/terminal_workflow_state.json")

    def test_run_study_from_request_delegates_legacy_run_state_to_gateway(self) -> None:
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
        self.assertNotIn("compile_study_graph", called_names)
        self.assertIn("block_legacy_run_to_completion", called_names)
        self.assertIn("run_legacy_to_completion", called_names)

    def test_legacy_run_response_uses_gateway_projection_metadata(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_legacy_projection_metadata") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        legacy_result = SimpleNamespace(
            graph_result={
                "study_id": "MY_STUDY",
                "run_id": "run_legacy_projection_metadata",
                "status": "completed",
                "requested_datasets": ["ADAE"],
                "target_datasets": ["ADAE"],
                "runnable_datasets": ["ADAE"],
                "blocked_datasets": [],
                "dependency_review_status": "accepted",
                "dataset_results": [],
                "audit_manifest": None,
            },
            workflow_projection={
                "workflow_control": "legacy_run_to_completion_compatibility_shim",
                "graph_state_path": None,
                "workflow_state_path": "sentinel/legacy_workflow_state.json",
            },
        )

        with patch("adam_agent.api.service.GraphGateway") as gateway_cls:
            gateway_cls.return_value.run_legacy_to_completion.return_value = legacy_result
            response = service.run_study_from_request(
                service.RunStudyRequest(
                    study_dir=str(study_dir),
                    run_id="run_legacy_projection_metadata",
                    target_datasets=["ADAE"],
                    execution_mode="stub",
                )
            )

        self.assertEqual(response.workflow_control, "legacy_run_to_completion_compatibility_shim")
        self.assertIsNone(response.graph_state_path)
        self.assertEqual(response.workflow_state_path, "sentinel/legacy_workflow_state.json")

    def test_legacy_run_workflow_helpers_are_removed_from_service_layer(self) -> None:
        from adam_agent.api import service

        self.assertFalse(hasattr(service, "_write_legacy_run_blocked_workflow_state"))
        self.assertFalse(hasattr(service, "_write_legacy_run_completion_workflow_state"))

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
        self.assertIn("headerStatusGrid", response.text)
        self.assertIn("headerOperation", response.text)
        self.assertIn("headerStudy", response.text)
        self.assertIn("headerTarget", response.text)
        self.assertIn("headerNextAction", response.text)
        self.assertIn("headerOperationProgress", response.text)
        self.assertIn("updateHeaderStatusOverview", response.text)
        self.assertIn("recognizedInputCount", response.text)
        self.assertIn("headerProgress.classList.add('running')", response.text)
        self.assertIn("headerProgress.classList.add('done')", response.text)
        self.assertIn("headerProgress.classList.add('failed')", response.text)
        self.assertIn("studyProgressPanel", response.text)
        self.assertIn("humanReviewQueuePanel", response.text)
        self.assertIn("Human Review Queue", response.text)
        self.assertIn("renderHumanReviewQueue", response.text)
        self.assertIn("humanReviewQueueItems", response.text)
        self.assertIn("reviewQueueActionText", response.text)
        self.assertIn("studyLoopResultPanel", response.text)
        self.assertIn("Study Loop Result", response.text)
        self.assertIn("lastStudyLoopResult", response.text)
        self.assertIn("renderStudyLoopResult", response.text)
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
        self.assertIn("Approve Code", response.text)
        self.assertIn("Run Approved Code", response.text)
        self.assertIn("runApprovedButton", response.text)
        self.assertIn("Start Runnable Datasets", response.text)
        self.assertIn("startStudyLoopButton", response.text)
        self.assertIn("/runs/native-study-loop", response.text)
        self.assertIn("startNativeStudyLoop", response.text)
        self.assertIn("This does not approve draft specs, approve code, or run R.", response.text)
        self.assertIn("submitNativeResumeReview", response.text)
        self.assertIn("nativeResumeQueueItem", response.text)
        self.assertIn("Saved graph resume", response.text)
        self.assertIn("finalize-inputs", response.text)
        self.assertIn("finalizedInputsByDataset", response.text)
        self.assertIn("Audit Timeline", response.text)
        self.assertIn("Advanced setup and audit files (usually not needed)", response.text)
        self.assertIn("Use this panel only when changing the LLM provider, troubleshooting local R, or inspecting audit file locations.", response.text)
        self.assertIn("Technical run id", response.text)
        self.assertIn("Pipeline config file", response.text)
        self.assertIn("Local Rscript executable", response.text)
        self.assertIn("Usually leave unchanged.", response.text)
        self.assertIn("not saved as a study artifact", response.text)
        self.assertIn("Upload Define", response.text)
        self.assertIn("Upload Legacy Code", response.text)
        self.assertIn("Add another ADaM target", response.text)
        self.assertIn("addTargetButton", response.text)
        self.assertIn("Select one or more ADaM datasets to plan together", response.text)
        self.assertIn("targetSelectionSummary", response.text)
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
        self.assertIn("refreshRunProgress", response.text)
        self.assertIn("/graph-command", response.text)
        self.assertIn("/progress?study_dir=", response.text)
        self.assertIn("runProgress", response.text)
        self.assertIn("canApproveGeneratedCode", response.text)
        self.assertIn("Generated-code state exists", response.text)
        self.assertIn("data-card-target", response.text)
        self.assertIn("resetActiveDatasetView", response.text)
        self.assertNotIn("resetGeneratedState", response.text)
        self.assertNotIn("/dependency-review", response.text)
        self.assertNotIn("Create / Open Study", response.text)
        self.assertNotIn("Approve And Run Locally", response.text)
        self.assertNotIn("Run Approved Code In Sandbox", response.text)

    def test_index_exposes_graph_owned_progress_panel(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        render_progress_body = html.split("function renderStudyProgress(targets, runnable, blocked)", 1)[1].split("function studyProgressSummary", 1)[0]
        self.assertIn("updateHeaderStatusOverview()", render_progress_body)
        progress_body = html.split("function studyProgressSummary(targets, runnable, blocked)", 1)[1].split("function renderHumanReviewQueue()", 1)[0]
        self.assertIn("state.runProgress", progress_body)
        self.assertIn("datasetProgressFor(active)", progress_body)
        self.assertIn("progressStepsFromReadModel(progress, activeProgress, inputCount)", progress_body)
        self.assertIn("progress?.next_action", progress_body)
        self.assertIn("studyStatusPill(progress, blocked, targets)", progress_body)
        self.assertIn("nativeResumeProgressNote()", progress_body)
        self.assertIn("studyQualityText(progress.output_quality_rollup)", progress_body)
        self.assertIn("activeProgress?.blocked_reason", progress_body)
        self.assertIn("state.graphState?.status", progress_body)
        self.assertIn("graphInterruptLabel()", progress_body)
        self.assertIn("function studyStatusPill(progress, blocked, targets)", html)
        self.assertIn("function nativeResumeProgressNote()", html)
        self.assertIn("function nativeResumeUnavailableText(resume)", html)
        self.assertIn("function nativeResumeActionHtml(dataset, interruptName)", html)
        self.assertIn("function nativeResumeActionAllowed(interruptName, actionName)", html)
        self.assertIn("function attachNativeResumeHandlers()", html)
        self.assertIn("This run uses saved graph state recovery, not a durable LangGraph checkpoint.", html)
        self.assertIn("current service is bound to a different checkpoint", html)
        self.assertNotIn("explicit_resume_endpoint", progress_body)
        self.assertNotIn("native-resume", progress_body)
        self.assertIn("function studyQualityText(rollup)", html)
        self.assertIn("review-only/demo output(s)", html)
        self.assertIn("targetSpecGateSatisfied(active)", html)
        self.assertIn("generatedFor(active)?.status === 'stale'", html)
        self.assertIn("executionFor(active)?.status === 'terminal_failure'", html)
        apply_progress_body = html.split("function applyRunProgress(progress)", 1)[1].split("function applyGraphState(graph)", 1)[0]
        self.assertIn("Object.prototype.hasOwnProperty.call(progress, 'study_loop_result')", apply_progress_body)
        self.assertIn("state.runProgress = progress || null;", apply_progress_body)
        self.assertIn("const requestedTargets = (progress?.requested_datasets || [])", apply_progress_body)
        self.assertIn("state.selectedTargetsForPlan = Array.from(new Set(requestedTargets)).sort();", apply_progress_body)
        self.assertIn("const activeTarget = String(state.selectedTarget || '').toUpperCase();", apply_progress_body)
        self.assertIn("const requestedTargetSet = new Set(requestedTargets);", apply_progress_body)
        self.assertIn("state.lastStudyLoopResult = progress.study_loop_result && Object.keys(progress.study_loop_result).length", apply_progress_body)
        refresh_body = html.split("async function refreshRunProgress()", 1)[1].split("async function refreshGraphReadModels()", 1)[0]
        self.assertIn("applyRunProgress(progress);", refresh_body)
        self.assertNotIn("state.runProgress = progress;", refresh_body)

    def test_index_exposes_human_review_queue_from_graph_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        queue_body = html.split("function renderHumanReviewQueue()", 1)[1].split("function graphInterruptLabel()", 1)[0]
        self.assertIn("state.runProgress", queue_body)
        self.assertIn("Array.isArray(progress.review_queue)", queue_body)
        self.assertIn("return progress.review_queue.map", queue_body)
        self.assertLess(queue_body.index("Array.isArray(progress.review_queue)"), queue_body.index("const graph = state.graphState || {};"))
        self.assertIn("source: item.source || 'graph_progress'", queue_body)
        self.assertIn("availableActions: Array.isArray(item.available_actions) ? item.available_actions : []", queue_body)
        self.assertIn("progress.current_interrupt", queue_body)
        self.assertIn("datasetProgress.current_interrupt", queue_body)
        self.assertIn("availableActions: datasetProgress.available_actions || []", queue_body)
        self.assertIn("progressInterruptName(datasetProgress.next_action)", queue_body)
        self.assertIn("state.graphState", queue_body)
        self.assertIn("graph.current_interrupt", queue_body)
        self.assertIn("const safeDatasetState = datasetState || {};", queue_body)
        self.assertIn("safeDatasetState.current_interrupt", queue_body)
        self.assertIn("safeDatasetState.status", queue_body)
        self.assertIn("Review dependency plan before product steps continue.", queue_body)
        self.assertIn("Review generated R code before local execution.", queue_body)
        self.assertIn("Review diagnostics and choose repair, retry, or skip.", queue_body)
        self.assertIn("function reviewQueueActionHints(item)", queue_body)
        self.assertIn("function reviewQueueGraphCommandActionHtml(item)", queue_body)
        self.assertIn("function graphCommandActionsForReviewItem(item)", queue_body)
        self.assertIn("Approve Dependency Plan", queue_body)
        self.assertIn("data-review-command-action", queue_body)
        self.assertIn("attachReviewQueueGraphCommandHandlers()", queue_body)
        self.assertIn("Available graph actions:", queue_body)
        self.assertNotIn("JSON.stringify", queue_body)

    def test_index_review_queue_renders_dependency_graph_command_actions(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
applyRunProgress({
  dependency_review_status: 'warning',
  current_interrupt: {name: 'dependency_review', status: 'open', reason: 'Review dependency warnings.'},
  review_queue: [{
    scope: 'study',
    dataset: '',
    name: 'dependency_review',
    reason: 'Review dependency warnings.',
    available_actions: [
      {action: 'approve', label: 'Approve Dependency Plan'},
      {action: 'reject', label: 'Reject Dependency Plan'},
      {action: 'execute_after_approval', label: 'Run Anyway'}
    ]
  }]
});
renderHumanReviewQueue();
console.log(JSON.stringify({
  title: nodes.get('humanReviewQueueTitle').textContent,
  detail: nodes.get('humanReviewQueueDetail').textContent,
  status: nodes.get('humanReviewQueueStatus').textContent,
  html: nodes.get('humanReviewQueueList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_dependency_review_graph_command_render.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIn("1 review gate", result["title"])
        self.assertEqual(result["status"], "review")
        self.assertIn("Review gates are read from graph state", result["detail"])
        self.assertIn("Study", result["html"])
        self.assertIn("Dependency review", result["html"])
        self.assertIn("Approve Dependency Plan", result["html"])
        self.assertIn("Reject Dependency Plan", result["html"])
        self.assertIn('data-review-command-interrupt="dependency_review"', result["html"])
        self.assertIn('data-review-command-dataset=""', result["html"])
        self.assertNotIn("Run Anyway", result["html"])
        self.assertNotIn("execute_after_approval", result["html"])
        self.assertNotIn("native-resume", result["html"])

    def test_index_dependency_review_queue_requires_advertised_actions(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
applyRunProgress({
  dependency_review_status: 'warning',
  current_interrupt: {name: 'dependency_review', status: 'open', reason: 'Review dependency warnings.'},
  review_queue: [{
    scope: 'study',
    dataset: '',
    name: 'dependency_review',
    reason: 'Review dependency warnings.',
    available_actions: []
  }]
});
renderHumanReviewQueue();
console.log(JSON.stringify({
  html: nodes.get('humanReviewQueueList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_dependency_review_requires_advertised_actions.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIn("Dependency review", result["html"])
        self.assertIn("Review dependency warnings.", result["html"])
        self.assertNotIn("Approve Dependency Plan", result["html"])
        self.assertNotIn("Reject Dependency Plan", result["html"])
        self.assertNotIn("data-review-command-action", result["html"])

    def test_index_dependency_review_queue_action_posts_graph_command_only(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    const values = {
      studyDir: 'D:/tmp/study',
      runId: 'run_ui_dependency_review_graph_command',
      reviewer: 'alice',
      reviewNotes: 'dependency plan looks acceptable',
      configPath: 'studies/_template/configs/mock_downstream.json',
      rscriptPath: 'C:/Dev/R-4.5.2/bin/Rscript.exe',
      modelMode: 'real',
      llmProvider: 'openai-compatible',
      llmModel: 'gpt-5.5',
      llmBaseUrl: 'http://localhost:8080/v1',
      llmApiKey: 'sk-test',
      llmAllowExternal: ''
    };
    nodes.set(id, {
      value: Object.prototype.hasOwnProperty.call(values, id) ? values[id] : '',
      checked: id === 'llmAllowExternal',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
const calls = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push({url, body: options.body ? JSON.parse(options.body) : null});
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_dependency_review_graph_command',
      scope: 'study',
      interrupt: 'dependency_review',
      action: 'approve',
      status: 'approved',
      dependency_review_status: 'approved',
      current_interrupt: null,
      next_action: 'start_runnable_datasets',
      graph_state_path: 'runs/run_ui_dependency_review_graph_command/graph_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_dependency_review_graph_command',
      status: 'planned',
      dependency_review_status: 'approved',
      current_interrupt: null,
      datasets: {}
    })};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({
      dependency_review_status: 'approved',
      review_queue: [],
      target_datasets: ['ADAE'],
      datasets: []
    })};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_dependency_review_graph_command', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
nodes.get('runId').value = 'run_ui_dependency_review_graph_command';
state.studyId = 'PSY201';
state.plan = {requested_datasets: ['ADAE'], runnable_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'warning'};
applyRunProgress({
  dependency_review_status: 'warning',
  current_interrupt: {name: 'dependency_review', status: 'open', reason: 'Review dependency warnings.'},
  review_queue: [{
    scope: 'study',
    dataset: '',
    name: 'dependency_review',
    reason: 'Review dependency warnings.',
    available_actions: [
      {action: 'approve', label: 'Approve Dependency Plan'},
      {action: 'reject', label: 'Reject Dependency Plan'}
    ]
  }]
});
await submitReviewQueueGraphCommand({dataset: '', interrupt: 'dependency_review', action: 'approve'});
const graphCommandCalls = calls.filter((item) => item.url.includes('/graph-command'));
console.log(JSON.stringify({
  graphCommandCalls,
  nativeResumeCalls: calls.filter((item) => item.url.includes('/native-resume')),
  dependencyReviewEndpointCalls: calls.filter((item) => item.url.includes('/dependency-review')),
  operation: nodes.get('operationTitle').textContent,
  planStatus: state.plan.dependency_review_status,
  queueLength: state.runProgress.review_queue.length
}));
"""
        script_path = TMP_ROOT / "ui_dependency_review_graph_command_post.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(len(result["graphCommandCalls"]), 1)
        call = result["graphCommandCalls"][0]
        self.assertIn("/runs/run_ui_dependency_review_graph_command/graph-command", call["url"])
        body = call["body"]
        self.assertEqual(body["study_dir"], "D:/tmp/study")
        self.assertEqual(body["reviewer"], "alice")
        self.assertEqual(body["interrupt"], "dependency_review")
        self.assertEqual(body["action"], "approve")
        self.assertEqual(body["notes"], "dependency plan looks acceptable")
        self.assertEqual(body["payload"], {"approved_dependency_datasets": []})
        self.assertNotIn("dataset", body)
        self.assertNotIn("execute_after_approval", body)
        self.assertNotIn("config_path", body)
        self.assertNotIn("rscript_path", body)
        self.assertNotIn("llm_provider", body)
        self.assertEqual(result["nativeResumeCalls"], [])
        self.assertEqual(result["dependencyReviewEndpointCalls"], [])
        self.assertEqual(result["operation"], "Graph review decision recorded")
        self.assertEqual(result["planStatus"], "approved")
        self.assertEqual(result["queueLength"], 0)

    def test_phase8_api_contract_marks_dependency_review_as_compatibility(self) -> None:
        contract = (ROOT / "docs" / "phase8_1_api_contract.md").read_text(encoding="utf-8")

        self.assertIn("decision through `POST /runs/{run_id}/graph-command`", contract)
        self.assertIn("Product UI code should prefer:", contract)
        self.assertIn("`POST /runs/native-study-loop` for study-level dispatch", contract)
        self.assertIn("`POST /runs/{run_id}/datasets/{dataset}/native-full-run`", contract)
        self.assertIn("`POST /runs/{run_id}/datasets/{dataset}/native-full-run/execute`", contract)
        self.assertIn("## Dataset Native Flow", contract)
        self.assertIn("## Dataset Split Flow Compatibility", contract)
        self.assertIn("compatibility/manual transition endpoints", contract)
        self.assertIn("Compatibility endpoint for older clients", contract)
        self.assertIn("Product review actions should use `/graph-command`.", contract)
        self.assertIn("New UI/product code should not call this endpoint.", contract)
        self.assertIn("Normal browser review actions should use `/graph-command`.", contract)
        self.assertNotIn(
            "If the dependency plan needs user input, use `POST /runs/{run_id}/dependency-review`.",
            contract,
        )

    def test_index_exposes_native_study_loop_result_summary(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        self.assertIn("Study Loop Result", html)
        self.assertIn("studyLoopResultPanel", html)
        self.assertIn("studyLoopResultList", html)
        start_body = html.split("async function startNativeStudyLoop()", 1)[1].split("async function approveDraftSpec()", 1)[0]
        self.assertIn("state.lastStudyLoopResult = {", start_body)
        self.assertIn("requested_targets: targets", start_body)
        self.assertIn("recorded_at:", start_body)
        dashboard_body = html.split("function renderGraphAwareDashboard()", 1)[1].split("function renderStudyProgress", 1)[0]
        self.assertIn("renderStudyLoopResult()", dashboard_body)
        loop_body = html.split("function renderStudyLoopResult()", 1)[1].split("function graphInterruptLabel()", 1)[0]
        self.assertIn("This does not approve draft specs, approve code, or run R.", loop_body)
        self.assertIn("Recovered from graph progress.", loop_body)
        self.assertIn("Recorded from the latest Start Runnable Datasets command.", loop_body)
        self.assertIn("durable LangGraph checkpoint resume is not enabled", loop_body)
        self.assertIn("Durable native resume is available", loop_body)
        self.assertIn("function studyLoopNativeResumeQueueText(result)", loop_body)
        self.assertIn("visible in native resume queue", loop_body)
        self.assertIn("nativeResumeUnavailableText(result)", loop_body)
        self.assertIn("this panel is status-only", loop_body)
        self.assertIn("function humanNativeResumeScope(scope)", loop_body)
        self.assertIn("pilot graph interrupts only", loop_body)
        self.assertNotIn("for ${result.native_resume_scope", loop_body)
        self.assertIn("studyLoopStartedRows(result)", loop_body)
        self.assertIn("studyLoopSkippedRows(skipped)", loop_body)
        self.assertIn("studyLoopBlockedRows(blocked)", loop_body)
        self.assertIn("studyLoopReviewQueueRows(reviewQueue, started, skipped)", loop_body)
        self.assertIn("humanDependencyReason(item.reason)", loop_body)
        self.assertNotIn("JSON.stringify", loop_body)

    def test_index_renders_native_study_loop_result_summary(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_study_loop_result' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.lastStudyLoopResult = {
  source: 'command_response',
  message: 'Started ADAE and stopped at human review gates. 2 review item(s) are now queued.',
  recorded_at: '10:30:00',
  native_resume_has_queue_items: true,
  native_resume_queue_item_count: 2,
  native_resume_available: false,
  runtime_binding_status: 'run_not_durable',
  resume_unavailable_reason: 'run_not_durable',
  started_datasets: ['ADAE'],
  dataset_results: [
    {dataset: 'ADAE', next_action: 'review_code', warnings: ['Static warning needs review.']}
  ],
          blocked_datasets: [
            {dataset: 'ADLB', reason: 'dependency_user_action_required', blocked_by: 'ADSL'}
          ],
          skipped_datasets: [
            {dataset: 'ADCM', reason: 'existing_graph_progress', status: 'needs_review', next_action: 'review_code', interrupt: 'code_review'}
          ],
          review_queue: [
            {dataset: 'ADCM', name: 'code_review', reason: 'Existing code review remains open.'},
            {dataset: 'ADLB', name: 'draft_spec_review', reason: 'No approved input spec was supplied.'}
          ]
        };
renderStudyLoopResult();
console.log(JSON.stringify({
  title: nodes.get('studyLoopResultTitle').textContent,
  detail: nodes.get('studyLoopResultDetail').textContent,
  status: nodes.get('studyLoopResultStatus').textContent,
  html: nodes.get('studyLoopResultList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_study_loop_result.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIn("1 dataset(s) moved to review gates", result["title"])
        self.assertIn("This does not approve draft specs, approve code, or run R.", result["detail"])
        self.assertIn("Recorded from the latest Start Runnable Datasets command.", result["detail"])
        self.assertNotIn("durable LangGraph checkpoint resume is not enabled", result["detail"])
        self.assertIn("2 review gates visible in native resume queue.", result["detail"])
        self.assertIn("This run uses saved graph state recovery, not a durable LangGraph checkpoint.", result["detail"])
        self.assertEqual(result["status"], "review")
        self.assertIn("ADAE", result["html"])
        self.assertIn("Review Code", result["html"])
        self.assertIn("Static warning needs review.", result["html"])
        self.assertIn("ADLB", result["html"])
        self.assertIn("Blocked", result["html"])
        self.assertIn("missing upstream ADaM", result["html"])
        self.assertIn("ADCM", result["html"])
        self.assertEqual(result["html"].count('<div class="study-loop-target">ADCM</div>'), 1)
        self.assertNotIn("Existing code review remains open.", result["html"])
        self.assertIn("Preserved", result["html"])
        self.assertIn("already has graph progress", result["html"])
        self.assertIn("ADLB", result["html"])
        self.assertIn("Draft spec review", result["html"])

    def test_index_recovers_study_loop_result_from_progress(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_study_loop_progress' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
applyRunProgress({
  target_datasets: ['ADAE'],
  study_loop_result: {
    source: 'graph_progress',
    message: 'Started ADAE and stopped at human review gates. 1 review item(s) are now queued.',
    started_datasets: ['ADAE'],
    blocked_datasets: [],
    review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'}]
  }
});
renderStudyLoopResult();
console.log(JSON.stringify({
  source: state.lastStudyLoopResult.source,
  detail: nodes.get('studyLoopResultDetail').textContent,
  html: nodes.get('studyLoopResultList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_study_loop_progress_result.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["source"], "graph_progress")
        self.assertIn("Recovered from graph progress.", result["detail"])
        self.assertIn("This does not approve draft specs, approve code, or run R.", result["detail"])
        self.assertIn("ADAE", result["html"])
        self.assertIn("Code review", result["html"])

    def test_index_recovers_planned_targets_from_progress_when_graph_state_unavailable(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_progress_requested_targets' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.selectedTargetsForPlan = [];
state.targetCandidates = ['ADSL'];
state.selectedTarget = 'ADSL';
applyRunProgress({
  requested_datasets: ['ADAE', 'ADCM'],
  target_datasets: ['ADSL', 'ADAE', 'ADCM'],
  runnable_datasets: ['ADAE', 'ADCM'],
  blocked_datasets: [],
  datasets: [
    {dataset: 'ADAE', status: 'needs_review', next_action: 'review_code', code_status: 'generated'},
    {dataset: 'ADCM', status: 'needs_review', next_action: 'review_draft_spec', spec_status: 'draft_generated'}
  ],
  study_loop_result: {
    source: 'graph_progress',
    started_datasets: ['ADAE', 'ADCM'],
    blocked_datasets: [],
    review_queue: []
  }
});
state.plan = {
  requested_datasets: ['ADAE', 'ADCM'],
  runnable_datasets: ['ADAE', 'ADCM'],
  blocked_datasets: []
};
renderDatasetBoard(state.runProgress.target_datasets, state.runProgress.runnable_datasets, state.runProgress.blocked_datasets);
console.log(JSON.stringify({
  selectedTargets: state.selectedTargetsForPlan,
  selectedTarget: state.selectedTarget,
  html: nodes.get('datasetBoard').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_progress_requested_targets.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["selectedTargets"], ["ADAE", "ADCM"])
        self.assertEqual(result["selectedTarget"], "ADAE")
        self.assertIn("ADAE: planned in this run", result["html"])
        self.assertIn("ADCM: planned in this run", result["html"])
        self.assertIn("ADSL: view-only history/candidate", result["html"])

    def test_index_renders_native_resume_available_as_status_not_action(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_native_resume_status' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
const progress = {
  target_datasets: ['ADAE'],
  native_resume: {
    available: true,
    scope: 'native_pilot_interrupts_only',
    boundary: 'durable_native_interrupt_resume',
    explicit_resume_endpoint: 'POST /runs/{run_id}/datasets/{dataset}/native-resume'
  },
  study_loop_result: {
    source: 'graph_progress',
    native_resume_available: true,
    native_resume_scope: 'native_pilot_interrupts_only',
    resume_boundary: 'durable_native_interrupt_resume',
    native_resume_has_queue_items: true,
    native_resume_queue_item_count: 1,
    message: 'Started ADAE and stopped at human review gates.',
    started_datasets: ['ADAE'],
    blocked_datasets: [],
    review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'}]
  }
};
applyRunProgress(progress);
renderStudyProgress(state.targetCandidates, [], []);
renderStudyLoopResult();
console.log(JSON.stringify({
  appliedResume: state.runProgress.native_resume.available,
  progressHtml: nodes.get('studyProgressSteps').innerHTML,
  detail: nodes.get('studyLoopResultDetail').textContent,
  loopHtml: nodes.get('studyLoopResultList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_native_resume_available_status.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        rendered = " ".join([result["progressHtml"], result["detail"], result["loopHtml"]])
        self.assertTrue(result["appliedResume"])
        self.assertIn("Native resume: available for pilot graph interrupts only.", rendered)
        self.assertIn("Durable native resume is available for pilot graph interrupts only.", rendered)
        self.assertIn("1 review gate visible in native resume queue.", rendered)
        self.assertIn("this panel is status-only.", rendered)
        self.assertNotIn("native-resume", rendered)
        self.assertNotIn("explicit_resume_endpoint", rendered)
        self.assertNotIn("<button", rendered)

    def test_index_renders_native_resume_actions_only_for_callable_queue_item(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_native_resume_action' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
applyRunProgress({
  target_datasets: ['ADAE'],
  native_resume: {
    available: true,
    scope: 'native_pilot_interrupts_only',
    boundary: 'durable_native_interrupt_resume',
    interrupt_queue: [
      {
        dataset: 'ADAE',
        interrupt: 'code_review',
        can_resume: true,
        resume_endpoint: 'POST /runs/{run_id}/datasets/{dataset}/native-resume',
        available_actions: [
          {action: 'approve', label: 'Approve Code'},
          {action: 'reject', label: 'Reject Code'},
          {action: 'run_arbitrary_tool', label: 'Run Arbitrary Tool'}
        ],
        reason: 'Review generated R code.'
      },
      {
        dataset: 'ADCM',
        interrupt: 'code_review',
        can_resume: false,
        resume_endpoint: null,
        available_actions: [{action: 'approve', label: 'Approve Code'}],
        reason: 'Memory-mode visible gate.'
      }
    ]
  },
  review_queue: [
    {dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'},
    {dataset: 'ADCM', name: 'code_review', reason: 'Memory-mode visible gate.'}
  ],
  study_loop_result: {
    source: 'graph_progress',
    native_resume_available: true,
    native_resume_scope: 'native_pilot_interrupts_only',
    native_resume_has_queue_items: true,
    native_resume_queue_item_count: 2,
    native_resume_interrupts: [
      {
        dataset: 'ADAE',
        interrupt: 'code_review',
        can_resume: true,
        available_actions: [{action: 'approve', label: 'Approve Code'}]
      },
      {
        dataset: 'ADCM',
        interrupt: 'code_review',
        can_resume: false,
        available_actions: [{action: 'approve', label: 'Approve Code'}]
      }
    ],
    started_datasets: [],
    blocked_datasets: [],
    review_queue: [
      {dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'},
      {dataset: 'ADCM', name: 'code_review', reason: 'Memory-mode visible gate.'}
    ]
  }
});
renderHumanReviewQueue();
renderStudyLoopResult();
console.log(JSON.stringify({
  queueHtml: nodes.get('humanReviewQueueList').innerHTML,
  loopHtml: nodes.get('studyLoopResultList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_native_resume_action_render.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        rendered = " ".join([result["queueHtml"], result["loopHtml"]])
        self.assertIn("Saved graph resume", rendered)
        self.assertIn('data-saved-graph-dataset="ADAE"', rendered)
        self.assertIn('data-saved-graph-action="approve"', rendered)
        self.assertIn("Approve Code", rendered)
        self.assertIn("Reject Code", rendered)
        self.assertNotIn("Run Arbitrary Tool", rendered)
        self.assertNotIn("run_arbitrary_tool", rendered)
        self.assertNotIn('data-saved-graph-dataset="ADCM"', rendered)
        self.assertNotIn("native-resume", rendered)
        self.assertNotIn("explicit_resume_endpoint", rendered)

    def test_index_native_resume_action_posts_existing_endpoint_when_callable(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
const calls = [];
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      let value = '';
      if (id === 'studyDir') value = 'D:/tmp/study';
      if (id === 'runId') value = 'run_ui_native_resume_click';
      if (id === 'reviewer') value = 'alice';
      nodes.set(id, {
        value,
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async (url, options = {}) => {
  calls.push({url, body: options.body ? JSON.parse(options.body) : null});
  if (url.includes('/native-resume')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_native_resume_click',
      dataset: 'ADAE',
      interrupt: 'code_review',
      decision: 'approve',
      status: 'needs_review',
      current_interrupt: null,
      executed: false,
      next_action: 'execute_approved_code',
      graph_state_path: 'runs/run_ui_native_resume_click/graph_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_native_resume_click', datasets: {}})};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({target_datasets: ['ADAE'], datasets: []})};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_native_resume_click', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
nodes.get('runId').value = 'run_ui_native_resume_click';
state.selectedTarget = 'ADAE';
applyRunProgress({
  target_datasets: ['ADAE'],
  native_resume: {
    available: true,
    scope: 'native_pilot_interrupts_only',
    boundary: 'durable_native_interrupt_resume',
    interrupt_queue: [{
      dataset: 'ADAE',
      interrupt: 'code_review',
      can_resume: true,
      available_actions: [{action: 'approve', label: 'Approve Code'}]
    }]
  }
});
await submitNativeResumeReview('ADAE', 'approve', 'code_review');
console.log(JSON.stringify({
  nativeResumeCalls: calls.filter((item) => item.url.includes('/native-resume')),
  review: state.reviewByDataset.ADAE,
  operation: nodes.get('operationTitle').textContent
}));
"""
        script_path = TMP_ROOT / "ui_native_resume_action_post.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(len(result["nativeResumeCalls"]), 1)
        call = result["nativeResumeCalls"][0]
        self.assertIn("/runs/run_ui_native_resume_click/datasets/ADAE/native-resume", call["url"])
        self.assertEqual(call["body"]["study_dir"], "D:/tmp/study")
        self.assertEqual(call["body"]["reviewer"], "alice")
        self.assertEqual(call["body"]["decision"], "approve")
        self.assertFalse(call["body"]["execute_after_approval"])
        self.assertTrue(result["review"]["native_resume"])
        self.assertEqual(result["operation"], "Saved graph gate resumed")

    def test_index_native_resume_action_revalidates_available_action_before_post(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
const calls = [];
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      let value = '';
      if (id === 'studyDir') value = 'D:/tmp/study';
      if (id === 'runId') value = 'run_ui_native_resume_click_revalidate';
      if (id === 'reviewer') value = 'alice';
      nodes.set(id, {
        value,
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async (url, options = {}) => {
  calls.push({url, body: options.body ? JSON.parse(options.body) : null});
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
nodes.get('runId').value = 'run_ui_native_resume_click_revalidate';
state.selectedTarget = 'ADAE';
applyRunProgress({
  target_datasets: ['ADAE'],
  native_resume: {
    available: true,
    scope: 'native_pilot_interrupts_only',
    boundary: 'durable_native_interrupt_resume',
    interrupt_queue: [{
      dataset: 'ADAE',
      interrupt: 'code_review',
      can_resume: true,
      available_actions: [{action: 'approve', label: 'Approve Code'}]
    }]
  },
  review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'}]
});
renderHumanReviewQueue();
await submitNativeResumeReview('ADAE', 'reject', 'code_review');
console.log(JSON.stringify({
  rendered: nodes.get('humanReviewQueueList').innerHTML,
  nativeResumeCalls: calls.filter((item) => item.url.includes('/native-resume')),
  operation: nodes.get('operationTitle')?.textContent || ''
}));
"""
        script_path = TMP_ROOT / "ui_native_resume_action_revalidates_available_action.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIn("Approve Code", result["rendered"])
        self.assertNotIn("Reject Code", result["rendered"])
        self.assertEqual(result["nativeResumeCalls"], [])
        self.assertNotEqual(result["operation"], "Saved graph gate resumed")

    def test_index_native_resume_actions_ignore_stale_study_loop_queue(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
const calls = [];
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_native_resume_stale_loop' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async (url, options = {}) => {
  calls.push({url, body: options.body ? JSON.parse(options.body) : null});
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
nodes.get('runId').value = 'run_ui_native_resume_stale_loop';
state.selectedTarget = 'ADAE';
applyRunProgress({
  target_datasets: ['ADAE'],
  native_resume: {
    available: false,
    scope: 'none',
    boundary: 'graph_state_projection_only',
    interrupt_queue: []
  },
  review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Current progress has no callable queue item.'}]
});
state.lastStudyLoopResult = {
  source: 'command_response',
  native_resume_available: true,
  native_resume_interrupts: [{
    dataset: 'ADAE',
    interrupt: 'code_review',
    can_resume: true,
    available_actions: [{action: 'approve', label: 'Approve Code'}]
  }],
  started_datasets: [],
  blocked_datasets: [],
  review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Stale command-response queue item.'}]
};
renderHumanReviewQueue();
renderStudyLoopResult();
await submitNativeResumeReview('ADAE', 'approve', 'code_review');
console.log(JSON.stringify({
  queueHtml: nodes.get('humanReviewQueueList').innerHTML,
  loopHtml: nodes.get('studyLoopResultList').innerHTML,
  nativeResumeCalls: calls.filter((item) => item.url.includes('/native-resume'))
}));
"""
        script_path = TMP_ROOT / "ui_native_resume_stale_loop_ignored.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        rendered = " ".join([result["queueHtml"], result["loopHtml"]])
        self.assertNotIn("Saved graph resume", rendered)
        self.assertNotIn("data-saved-graph-action", rendered)
        self.assertEqual(result["nativeResumeCalls"], [])

    def test_index_renders_native_resume_unavailable_reason_without_action(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_native_resume_reason' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
const progress = {
  target_datasets: ['ADAE'],
  native_resume: {
    available: false,
    scope: 'native_pilot_interrupts_only',
    boundary: 'graph_state_projection_only',
    runtime_binding_status: 'checkpoint_path_mismatch',
    resume_unavailable_reason: 'checkpoint_path_mismatch',
    explicit_resume_endpoint: 'POST /runs/{run_id}/datasets/{dataset}/native-resume'
  },
  study_loop_result: {
    source: 'graph_progress',
    native_resume_available: false,
    native_resume_scope: 'native_pilot_interrupts_only',
    runtime_binding_status: 'checkpoint_path_mismatch',
    resume_unavailable_reason: 'checkpoint_path_mismatch',
    native_resume_has_queue_items: true,
    native_resume_queue_item_count: 1,
    message: 'Existing progress recovered.',
    started_datasets: [],
    skipped_datasets: [{dataset: 'ADAE', reason: 'existing_graph_progress', next_action: 'review_code'}],
    blocked_datasets: [],
    review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'}]
  }
};
applyRunProgress(progress);
renderStudyProgress(state.targetCandidates, [], []);
renderStudyLoopResult();
console.log(JSON.stringify({
  progressHtml: nodes.get('studyProgressSteps').innerHTML,
  detail: nodes.get('studyLoopResultDetail').textContent,
  loopHtml: nodes.get('studyLoopResultList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_native_resume_unavailable_reason.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        rendered = " ".join([result["progressHtml"], result["detail"], result["loopHtml"]])
        self.assertIn("current service is bound to a different checkpoint", rendered)
        self.assertIn("1 review gate visible in native resume queue.", rendered)
        self.assertNotIn("native-resume", rendered)
        self.assertNotIn("explicit_resume_endpoint", rendered)
        self.assertNotIn("<button", rendered)

    def test_index_clears_stale_study_loop_result_when_progress_has_none(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.lastStudyLoopResult = {
  source: 'graph_progress',
  message: 'Old run result',
  started_datasets: ['ADAE'],
  blocked_datasets: [],
  review_queue: []
};
applyRunProgress({ target_datasets: ['ADLB'], study_loop_result: {} });
renderStudyLoopResult();
console.log(JSON.stringify({
  result: state.lastStudyLoopResult,
  title: nodes.get('studyLoopResultTitle').textContent,
  html: nodes.get('studyLoopResultList').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_study_loop_clear_empty_progress.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIsNone(result["result"])
        self.assertEqual(result["title"], "No batch start yet")
        self.assertIn("No study-level dataset dispatch has been started", result["html"])

    def test_index_apply_run_progress_null_clears_progress_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
global.window = { location: { href: '' } };
global.document = {
  getElementById() {
    return {
      value: '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    };
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.runProgress = { target_datasets: ['ADAE'], native_resume: { available: true } };
applyRunProgress(null);
console.log(JSON.stringify({ progress: state.runProgress }));
"""
        script_path = TMP_ROOT / "ui_apply_run_progress_null.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIsNone(result["progress"])

    def test_index_start_study_loop_keeps_graph_progress_result_over_command_response(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: '',
      checked: id === 'llmAllowExternal',
      disabled: false,
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('studyDir').value = 'D:/tmp/study';
node('runId').value = 'run_ui_study_loop_priority';
node('configPath').value = '';
node('rscriptPath').value = '';
node('modelMode').value = 'mock';
node('reviewer').value = 'local_user';
let progressCalls = 0;
global.fetch = async (path, options = {}) => {
  const url = String(path);
  if (url === '/runs/native-study-loop') {
    return {
      ok: true,
      json: async () => ({
        status: 'needs_review',
        message: 'Command response should be fallback only.',
        started_datasets: ['ADAE'],
        blocked_datasets: [],
        review_queue: [],
        dataset_results: [{dataset: 'ADAE', next_action: 'review_code'}]
      })
    };
  }
  if (url.includes('/graph-state')) {
    return {
      ok: true,
      json: async () => ({
        requested_datasets: ['ADAE'],
        target_datasets: ['ADAE'],
        runnable_datasets: ['ADAE'],
        blocked_datasets: [],
        dependency_decisions: [],
        dependency_resolution: [],
        datasets: {}
      })
    };
  }
  if (url.includes('/progress')) {
    progressCalls += 1;
    return {
      ok: true,
      json: async () => ({
        target_datasets: ['ADAE'],
        runnable_datasets: ['ADAE'],
        blocked_datasets: [],
        review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'}],
        datasets: [{dataset: 'ADAE', next_action: 'review_code', action_label: 'Review generated R code.', blocked: false}],
        study_loop_result: {
          source: 'graph_progress',
          message: 'Graph progress owns the study loop result.',
          started_datasets: ['ADAE'],
          blocked_datasets: [],
          review_queue: [{dataset: 'ADAE', name: 'code_review', reason: 'Review generated R code.'}]
        }
      })
    };
  }
  if (url.includes('/review-summary')) {
    return { ok: true, json: async () => ({}) };
  }
  return { ok: true, json: async () => ({}) };
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.targetCandidates = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], runnable_datasets: ['ADAE'], blocked_datasets: []};
state.runProgress = {
  target_datasets: ['ADAE'],
  runnable_datasets: ['ADAE'],
  blocked_datasets: [],
  datasets: [{dataset: 'ADAE', next_action: 'generate_code', action_label: 'Generate R code.', blocked: false}]
};
await startNativeStudyLoop();
renderStudyLoopResult();
console.log(JSON.stringify({
  source: state.lastStudyLoopResult.source,
  message: state.lastStudyLoopResult.message,
  progressCalls,
  detail: nodes.get('studyLoopResultDetail').textContent
}));
"""
        script_path = TMP_ROOT / "ui_study_loop_priority.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertGreaterEqual(result["progressCalls"], 1)
        self.assertEqual(result["source"], "graph_progress")
        self.assertEqual(result["message"], "Graph progress owns the study loop result.")
        self.assertIn("Recovered from graph progress.", result["detail"])

    def test_index_exposes_terminal_failure_triage_actions(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        self.assertIn("terminalFailureReviewByDataset", html)
        self.assertIn("function terminalFailurePanel(dataset)", html)
        self.assertIn("const datasetProgress = datasetProgressFor(target);", html)
        self.assertIn("const graphActions = Array.isArray(datasetProgress?.available_actions) ? datasetProgress.available_actions : [];", html)
        self.assertIn("const graphGateOpen = Boolean(datasetProgress", html)
        self.assertIn("const actionControls = graphActions.length", html)
        self.assertIn("Waiting for graph-owned terminal-failure actions to load.", html)
        self.assertIn("data-terminal-action=\"${escapeHtml(item.action)}\"", html)
        self.assertIn("function submitTerminalFailureReview(dataset, action)", html)
        terminal_body = html.split("async function submitTerminalFailureReview(dataset, action)", 1)[1].split("async function handleTableAction", 1)[0]
        self.assertIn("/graph-command", terminal_body)
        self.assertIn("interrupt: 'terminal_failure'", terminal_body)
        self.assertNotIn("/terminal-failure-review", terminal_body)
        self.assertIn("await refreshGraphReadModels()", html)
        self.assertIn("Choose one controlled next step; the graph will record the decision", html)

    def test_index_terminal_failure_panel_requires_graph_owned_actions(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_terminal_failure_ui' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.selectedTarget = 'ADAE';
state.executionByDataset = {ADAE: {dataset: 'ADAE', status: 'terminal_failure'}};
state.runProgress = null;
const localOnly = terminalFailurePanel('ADAE');
state.runProgress = {datasets: [{
  dataset: 'ADAE',
  status: 'terminal_failure',
  execution_status: 'terminal_failure',
  next_action: 'review_terminal_failure',
  available_actions: [
    {action: 'repair_code', label: 'Repair Code'},
    {action: 'revise_spec', label: 'Revise Spec'}
  ]
}]};
const graphOwned = terminalFailurePanel('ADAE');
state.runProgress = {datasets: [{
  dataset: 'ADAE',
  status: 'terminal_failure',
  execution_status: 'terminal_failure',
  next_action: 'review_terminal_failure',
  available_actions: []
}]};
const graphGateWithoutActions = terminalFailurePanel('ADAE');
console.log(JSON.stringify({
  localOnlyHasAction: localOnly.includes('data-terminal-action='),
  localOnlyVisible: localOnly.includes('Terminal Failure Triage'),
  graphOwnedHasRepair: graphOwned.includes('data-terminal-action="repair_code"'),
  graphOwnedHasRevise: graphOwned.includes('data-terminal-action="revise_spec"'),
  graphGateWithoutActionsHasAction: graphGateWithoutActions.includes('data-terminal-action='),
  graphGateWithoutActionsMessage: graphGateWithoutActions.includes('Waiting for graph-owned terminal-failure actions to load.')
}));
"""
        script_path = TMP_ROOT / "ui_terminal_failure_graph_actions.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertFalse(result["localOnlyVisible"])
        self.assertFalse(result["localOnlyHasAction"])
        self.assertTrue(result["graphOwnedHasRepair"])
        self.assertTrue(result["graphOwnedHasRevise"])
        self.assertFalse(result["graphGateWithoutActionsHasAction"])
        self.assertTrue(result["graphGateWithoutActionsMessage"])

    def test_index_graph_command_body_does_not_leak_continuation_settings(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    const values = {
      studyDir: 'D:/tmp/study',
      runId: 'run_ui_graph_command_body',
      reviewer: 'qa_user',
      reviewNotes: 'human decision',
      configPath: 'studies/_template/configs/mock_downstream.json',
      rscriptPath: 'C:/Dev/R-4.5.2/bin/Rscript.exe',
      modelMode: 'real',
      llmProvider: 'openai-compatible',
      llmModel: 'gpt-5.5',
      llmBaseUrl: 'http://localhost:8080/v1',
      llmApiKey: 'sk-test',
      llmAllowExternal: ''
    };
    nodes.set(id, {
      value: Object.prototype.hasOwnProperty.call(values, id) ? values[id] : '',
      checked: id === 'llmAllowExternal',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
const calls = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push({url, body: options.body ? JSON.parse(options.body) : null});
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_graph_command_body',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'terminal_failure',
      action: 'repair_code',
      status: 'needs_review',
      current_interrupt: null,
      approved: null,
      executed: false,
      terminal_failure: true,
      next_action: 'repair_generated_code',
      graph_state_path: 'runs/run_ui_graph_command_body/graph_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_graph_command_body',
      target_datasets: ['ADAE'],
      datasets: {
        ADAE: {
          status: 'needs_review',
          execution_state: {
            status: 'terminal_failure',
            terminal_failure_review: {action: 'repair_code', next_action: 'repair_generated_code'}
          }
        }
      }
    })};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({target_datasets: ['ADAE'], datasets: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
const helperBody = JSON.parse(graphCommandRequestBody({
  dataset: 'adae',
  interrupt: 'code_review',
  action: 'approve',
  payload: {reviewed_artifact: 'code'}
}));
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.executionByDataset = {ADAE: {dataset: 'ADAE', status: 'terminal_failure'}};
state.runProgress = {datasets: [{
  dataset: 'ADAE',
  status: 'terminal_failure',
  execution_status: 'terminal_failure',
  next_action: 'review_terminal_failure',
  available_actions: [{action: 'repair_code', label: 'Repair Code'}]
}]};
await submitTerminalFailureReview('ADAE', 'repair_code');
const commandBody = calls.find((item) => item.url.includes('/graph-command'))?.body || {};
console.log(JSON.stringify({
  helperKeys: Object.keys(helperBody).sort(),
  helperBody,
  commandKeys: Object.keys(commandBody).sort(),
  commandBody,
  terminalGraphCommand: state.terminalFailureReviewByDataset.ADAE?.graph_command === true
}));
"""
        script_path = TMP_ROOT / "ui_graph_command_body_keys.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        allowed_keys = ["action", "dataset", "interrupt", "notes", "payload", "reviewer", "study_dir"]
        self.assertEqual(result["helperKeys"], allowed_keys)
        self.assertEqual(result["commandKeys"], allowed_keys)
        self.assertEqual(result["helperBody"]["dataset"], "ADAE")
        self.assertEqual(result["commandBody"]["interrupt"], "terminal_failure")
        for forbidden in [
            "execute_after_approval",
            "config_path",
            "rscript_path",
            "llm_provider_override",
            "llm_exposure_override",
        ]:
            self.assertNotIn(forbidden, result["helperBody"])
            self.assertNotIn(forbidden, result["commandBody"])
        self.assertTrue(result["terminalGraphCommand"])

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
        self.assertIn("Technical path is available under Advanced setup and audit files", html)
        self.assertIn("<th>Path</th>", advanced_body)
        self.assertIn("review_summary_source", advanced_body)
        self.assertIn("review.graph_state_path", advanced_body)
        self.assertIn("review.workflow_state_path", advanced_body)
        self.assertNotIn("escapeHtml(finalized.input_spec_path)", draft_pane_body)
        self.assertNotIn("escapeHtml(finalized.approved_spec_path", draft_pane_body)
        self.assertNotIn("escapeHtml(draft.spec_path)", draft_pane_body)
        self.assertNotIn("escapeHtml(generated.draft_spec_path)", draft_notice_body)

    def test_index_hides_invalid_file_paths_from_input_warnings(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        summary_body = html.split("function renderInputSummary(summary)", 1)[1].split("function renderFiles(containerId, files)", 1)[0]
        self.assertIn("inputWarningText", summary_body)
        self.assertIn("Skipped ${fileName}", summary_body)
        self.assertIn("split(/[\\\\/]/)", summary_body)
        self.assertNotIn("`${item.path}: ${item.reason}`", summary_body)

    def test_index_explains_sas7bdat_not_previewed_as_runtime_input(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        render_files_body = html.split("function renderFiles(containerId, files)", 1)[1].split("function fileStatusLabel(file)", 1)[0]
        status_body = html.split("function fileStatusLabel(file)", 1)[1].split("function fileSummary(file)", 1)[0]
        summary_body = html.split("function fileSummary(file)", 1)[1].split("function inferTargets(summary)", 1)[0]
        self.assertIn("fileStatusPillClass(file)", render_files_body)
        self.assertIn("fileStatusLabel(file)", render_files_body)
        self.assertIn("file?.status === 'not_previewed' && file?.format === 'sas7bdat'", status_body)
        self.assertIn("return 'runtime input';", status_body)
        self.assertIn("file.status === 'not_previewed' && file.format === 'sas7bdat'", summary_body)
        self.assertIn("SAS dataset recognized as a runtime input", summary_body)

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
        self.assertIn("activeAgentNodeTrace()", audit_body)
        self.assertIn("datasetState?.agent_node_inputs", audit_body)
        self.assertIn("datasetState?.agent_node_outputs", audit_body)
        self.assertIn("graph.agent_node_inputs", audit_body)
        self.assertIn("graph.agent_node_outputs", audit_body)
        self.assertIn("agentNodeTraceCard(trace)", audit_body)
        self.assertIn("Agent node handoffs will appear here", audit_body)
        self.assertIn("validation_agent: 'Validation'", audit_body)
        self.assertIn("diagnosis_repair_agent: 'Diagnosis / repair'", audit_body)
        self.assertIn("readableDecisionName(decision.decision)", audit_body)
        self.assertIn("readableDecisionName(output.decision", audit_body)
        self.assertIn("readableRiskFlag", audit_body)
        self.assertIn("Reference compare limited scope", audit_body)
        self.assertNotIn("JSON.stringify", audit_body)

    def test_index_scan_inputs_refreshes_graph_read_models(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        scan_body = html.split("async function scanInputs()", 1)[1].split("function renderInputSummary(summary)", 1)[0]
        self.assertIn("await refreshGraphReadModels()", scan_body)
        self.assertNotIn("await refreshRunProgress()", scan_body)

    def test_index_demo_load_does_not_refresh_progress_directly(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        demo_body = html.split("async function createDemoStudy()", 1)[1].split("function applyWorkspacePayload(payload)", 1)[0]
        self.assertIn("await scanInputs()", demo_body)
        self.assertNotIn("await refreshRunProgress()", demo_body)

    def test_index_does_not_default_target_selection_to_adae(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        infer_body = html.split("function inferTargets(summary)", 1)[1].split("function inferAdTokens(text)", 1)[0]
        auto_body = html.split("function autoSelectFirstTarget(targets)", 1)[1].split("function renderTargetButtons(targets)", 1)[0]
        render_body = html.split("function renderTargetButtons(targets)", 1)[1].split("function renderTargetSelectionSummary()", 1)[0]
        progress_body = html.split("function applyRunProgress(progress)", 1)[1].split("function applyGraphState(graph)", 1)[0]
        graph_body = html.split("function applyGraphState(graph)", 1)[1].split("function planFromGraphState(graph)", 1)[0]
        self.assertNotIn("merged.add('ADAE')", infer_body)
        self.assertIn("const autoPlanned = available.filter(targetCanAutoPlan);", auto_body)
        self.assertIn("const preferred = preferredInitialTarget(autoPlanned.length ? autoPlanned : available);", auto_body)
        self.assertIn("if (normalized.includes('ADSL')) return 'ADSL';", auto_body)
        self.assertIn("if (state.selectedTargetsForPlan.length) preparePlan();", auto_body)
        self.assertIn("state.selectedTarget = targets[0];", render_body)
        self.assertIn("targetCanAutoPlan(state.selectedTarget)", render_body)
        self.assertIn("state.selectedTarget = requestedTargets[0];", progress_body)
        self.assertIn("state.graphState = graph || null;", graph_body)
        self.assertIn("const requestedGraphTargetSet = new Set(requestedTargets);", graph_body)
        self.assertIn("state.selectedTarget = requestedTargets[0];", graph_body)
        self.assertNotIn("includes('ADAE') ? 'ADAE'", auto_body + render_body + progress_body + graph_body)

    def test_index_refresh_graph_state_uses_apply_graph_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        refresh_body = html.split("async function refreshGraphState()", 1)[1].split("async function refreshRunProgress()", 1)[0]
        graph_body = html.split("function applyGraphState(graph)", 1)[1].split("function planFromGraphState(graph)", 1)[0]
        self.assertIn("applyGraphState(graph);", refresh_body)
        self.assertNotIn("state.graphState = graph;", refresh_body)
        self.assertIn("state.graphState = graph || null;", graph_body)

    def test_index_apply_graph_state_null_clears_graph_state(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
global.window = { location: { href: '' } };
global.document = {
  getElementById() {
    return {
      value: '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    };
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.graphState = { study_id: 'PSY201', run_id: 'run_ui_graph_null', target_datasets: ['ADAE'] };
state.plan = { dependency_review_status: 'approved', runnable_datasets: ['ADAE'] };
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
let renderPlanCalls = 0;
const originalRenderPlan = renderPlan;
renderPlan = (plan) => { renderPlanCalls += 1; originalRenderPlan(plan); };
applyGraphState(null);
console.log(JSON.stringify({
  graphState: state.graphState,
  plan: state.plan,
  selectedTarget: state.selectedTarget,
  selectedTargetsForPlan: state.selectedTargetsForPlan,
  renderPlanCalls
}));
"""
        script_path = TMP_ROOT / "ui_apply_graph_state_null.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIsNone(result["graphState"])
        self.assertEqual(result["plan"]["dependency_review_status"], "approved")
        self.assertEqual(result["selectedTarget"], "ADAE")
        self.assertEqual(result["selectedTargetsForPlan"], ["ADAE"])
        self.assertEqual(result["renderPlanCalls"], 0)

    def test_index_keeps_reference_only_targets_unplanned_until_explicitly_selected(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        helper_body = html.split("function recordTargetSource(target, source)", 1)[1].split("function llmProviderOverride()", 1)[0]
        infer_body = html.split("function inferTargets(summary)", 1)[1].split("function inferAdTokens(text)", 1)[0]
        auto_body = html.split("function autoSelectFirstTarget(targets)", 1)[1].split("function renderTargetButtons(targets)", 1)[0]
        render_body = html.split("function renderTargetButtons(targets)", 1)[1].split("function renderTargetSelectionSummary()", 1)[0]
        summary_body = html.split("function renderTargetSelectionSummary()", 1)[1].split("function addManualTarget()", 1)[0]
        action_body = html.split("function actionAvailability()", 1)[1].split("function reviewFor(dataset)", 1)[0]
        self.assertIn("targetEvidenceSources", html)
        self.assertIn("function isReferenceOnlyTarget(target)", helper_body)
        self.assertIn("return sources.includes('reference_adam') && !sources.some((source) => source !== 'reference_adam');", helper_body)
        self.assertIn("function targetCanAutoPlan(target)", helper_body)
        self.assertIn("return sources.some((source) => source !== 'reference_adam');", helper_body)
        self.assertIn("if (isReferenceOnlyTarget(target)) return 'reference only';", helper_body)
        self.assertIn("recordTargetSource(dataset, 'reference_adam')", infer_body)
        self.assertIn("recordTargetSource(dataset, 'input_spec')", infer_body)
        self.assertIn("recordTargetSource(token, 'legacy_code')", infer_body)
        self.assertIn("const autoPlanned = available.filter(targetCanAutoPlan);", auto_body)
        self.assertIn("const preferred = preferredInitialTarget(autoPlanned.length ? autoPlanned : available);", auto_body)
        self.assertIn("state.selectedTargetsForPlan = state.selectedTarget && targetCanAutoPlan(state.selectedTarget) ? [state.selectedTarget] : [];", auto_body)
        self.assertIn("targetSourceHint(target)", render_body)
        self.assertIn("Reference-only candidates stay unplanned until you explicitly select them.", summary_body)
        self.assertIn("const targetIsPlanned = Boolean(target && selectedTargets().includes(target));", action_body)
        self.assertIn("is currently reference-only evidence. Select its checkbox to request generation before finalizing inputs.", action_body)

    def test_index_input_change_clears_stale_planned_targets(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        invalidation_body = html.split("function invalidateUiStateAfterInputChange(payload)", 1)[1].split("function uploadDiffMessage(payload)", 1)[0]
        self.assertIn("state.selectedTargetsForPlan = [];", invalidation_body)
        self.assertIn("Re-check the output selection, then refresh the dependency plan", invalidation_body)
        self.assertIn("state.plan = null;", invalidation_body)
        self.assertIn("state.graphState = null;", invalidation_body)
        self.assertIn("state.generatedByDataset = {};", invalidation_body)

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
        self.assertIn("graphActionGate(progress, 'finalize')", action_body)
        self.assertIn("graphActionGate(progress, 'approveDraft')", action_body)
        self.assertIn("graphActionGate(progress, 'generate')", action_body)
        self.assertIn("graphActionGate(progress, 'approveCode')", action_body)
        self.assertIn("graphActionGate(progress, 'runApproved')", action_body)
        self.assertIn("const graphProgressMissingTarget = Boolean(state.runProgress && target && targetIsPlanned && !progress);", action_body)
        self.assertIn("Graph progress has no dataset step for ${target}", action_body)
        self.assertIn("waitingRuntimeDependenciesFor(target)", action_body)
        self.assertIn("runtimeDependencyWaitText(target, waitingRuntimeDependencies)", action_body)
        self.assertIn("Boolean(target && !blocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && approveCodeGate.ready && codeApprovalReady)", action_body)
        self.assertIn("const nativeExecutionContract = hasNativeFullRunExecutionContract(target);", action_body)
        self.assertIn("Boolean(target && !blocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && runApprovedGate.ready && generated && nativeExecutionContract)", action_body)
        self.assertIn("Product UI can execute only graph-owned native full-run code", action_body)
        self.assertIn("Reference ADaM files do not satisfy this runtime dependency", html)
        self.assertIn("local execution is paused until dependency review is resolved", action_body)
        self.assertIn("Ready for human code approval for ${target}. This will not run R.", action_body)
        self.assertIn("Approve the generated code before running local R.", action_body)
        self.assertIn("Clicking will prepare the dependency plan first", action_body)
        self.assertIn("Generated-code metadata exists", action_body)
        self.assertIn("function setButtonAvailability(id, item)", html)
        self.assertIn("button.setAttribute('aria-disabled-reason', item.reason)", html)
        self.assertIn("button.dataset.actionReady = String(Boolean(item.ready))", html)
        self.assertIn("button.disabled = !item.ready", html)
        self.assertIn("const availability = actionAvailability().finalize;", html)
        self.assertIn("const availability = actionAvailability().generate;", html)
        self.assertIn("const availability = actionAvailability().approveCode;", html)
        self.assertIn("const availability = actionAvailability().runApproved;", html)
        dashboard_body = html.split("function renderGraphAwareDashboard()", 1)[1].split("function renderStudyProgress", 1)[0]
        self.assertIn("renderActionAvailability()", dashboard_body)

    def test_index_primary_action_buttons_use_single_availability_writer(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        script = html.split("<script>", 1)[1].split("</script>", 1)[0]
        primary_buttons = [
            "finalizeInputsButton",
            "startStudyLoopButton",
            "approveDraftSpecButton",
            "generateCodeButton",
            "approveButton",
            "runApprovedButton",
        ]
        for button_id in primary_buttons:
            self.assertNotIn(f"byId('{button_id}').disabled =", script)
        self.assertIn("function setButtonAvailability(id, item)", script)
        self.assertIn("button.disabled = !item.ready", script)
        availability_body = html.split("function renderActionAvailability()", 1)[1].split("function setButtonAvailability", 1)[0]
        for button_id in primary_buttons:
            self.assertIn(f"setButtonAvailability('{button_id}'", availability_body)

    def test_index_primary_actions_follow_graph_progress_next_action(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        gate_body = html.split("function graphActionGate(progress, actionGroup)", 1)[1].split("function actionAvailability()", 1)[0]
        approve_body = html.split("async function approveCode()", 1)[1].split("async function runApprovedCode()", 1)[0]
        run_body = html.split("async function runApprovedCode()", 1)[1].split("async function loadReviewSummary", 1)[0]
        self.assertIn("finalize: ['finalize_inputs', 'reconfirm_inputs']", gate_body)
        self.assertIn("approveDraft: ['review_draft_spec']", gate_body)
        self.assertIn("generate: ['generate_code', 'repair_generated_code', 'revise_approved_spec']", gate_body)
        self.assertIn("approveCode: ['review_code']", gate_body)
        self.assertIn("runApproved: ['execute_approved_code', 'retry_approved_execution']", gate_body)
        self.assertIn("Graph next action:", gate_body)
        self.assertIn("Graph next action is", gate_body)
        self.assertIn("function graphAllowsCodeGeneration(progress)", html)
        self.assertIn("/native-full-run", html)
        self.assertNotIn("const endpoint = revisingSpec ? 'draft-spec' : 'native-full-run';", html)
        self.assertIn("async function applyNativeFullRunStart(payload)", html)
        self.assertIn("function hasNativeFullRunContract(target)", html)
        self.assertIn("function hasNativeFullRunExecutionContract(target)", html)
        self.assertIn("native-full-run", html)
        self.assertIn("function graphCommandRequestBody", html)
        self.assertIn("R will not run in this step.", approve_body)
        self.assertIn("/graph-command", approve_body)
        self.assertIn("interrupt: 'code_review'", approve_body)
        self.assertNotIn("native-full-run/resume", approve_body)
        self.assertNotIn("/code-review", approve_body)
        self.assertNotIn("/execute-approved-code", approve_body)
        self.assertIn("Executing the graph-approved ${generated.dataset} R code with local Rscript.", run_body)
        self.assertIn("hasNativeFullRunExecutionContract(generated.dataset)", run_body)
        self.assertIn("/native-full-run/execute", run_body)
        self.assertNotIn("/execute-approved-code", run_body)
        self.assertNotIn("/code-review", run_body)

    def test_index_primary_next_action_prefers_native_study_loop_over_compatibility_finalize(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_native_primary_action' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, toggle() {}, remove() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
        scrollIntoView() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.inputSummary = {sdtm: [{dataset: 'AE', file_name: 'ae.csv'}], specs: []};
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], runnable_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.runProgress = {
  dependency_review_status: 'accepted',
  target_datasets: ['ADAE'],
  runnable_datasets: ['ADAE'],
  blocked_datasets: [],
  datasets: [{
    dataset: 'ADAE',
    next_action: 'finalize_inputs',
    action_label: 'Confirm input spec or draft spec.',
    blocked: false
  }]
};
const availability = actionAvailability();
const view = primaryNextActionView();
console.log(JSON.stringify({
  finalizeReady: availability.finalize.ready,
  startStudyReady: availability.startStudy.ready,
  title: view.title,
  detail: view.detail,
  actions: view.buttons.map((item) => item.action),
  labels: view.buttons.map((item) => item.label)
}));
"""
        script_path = TMP_ROOT / "ui_primary_native_before_finalize.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["finalizeReady"])
        self.assertTrue(result["startStudyReady"])
        self.assertEqual(result["actions"], ["startStudy"])
        self.assertEqual(result["labels"], ["Start Runnable Datasets"])
        self.assertIn("review gates", result["title"])
        self.assertNotIn("finalizeInputs", result["actions"])

    def test_index_explains_waiting_runtime_dependencies_from_progress_read_model(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_waiting_runtime_dependency' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        disabled: false,
        classList: { add() {}, toggle() {}, remove() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute(name, value) { this[name] = value; },
        scrollIntoView() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.inputSummary = {
  sdtm: [{dataset: 'LB', file_name: 'lb.csv'}],
  specs: [],
  reference_adam: [{dataset: 'ADSL', file_name: 'adsl.csv'}],
  define: [],
  legacy_code: []
};
state.studyId = 'WAITSTUDY';
state.selectedTarget = 'ADLB';
state.selectedTargetsForPlan = ['ADSL', 'ADLB'];
state.targetCandidates = ['ADSL', 'ADLB'];
state.plan = {
  requested_datasets: ['ADSL', 'ADLB'],
  target_datasets: ['ADSL', 'ADLB'],
  runnable_datasets: ['ADSL'],
  blocked_datasets: [],
  dependency_review_status: 'accepted',
  dependency_decisions: [{dataset: 'ADLB', dependencies: ['ADSL'], source: 'legacy_code', review_required: false}]
};
state.runProgress = {
  status: 'running',
  next_action: 'generate_code',
  dependency_review_status: 'accepted',
  target_datasets: ['ADSL', 'ADLB'],
  runnable_datasets: ['ADSL'],
  blocked_datasets: [],
  datasets: [
    {dataset: 'ADSL', status: 'needs_review', next_action: 'review_code', code_status: 'generated', blocked: false},
    {dataset: 'ADLB', status: 'pending', next_action: 'complete_dependency_output', action_label: 'Complete upstream runtime output first: ADSL.', blocked: false, waiting_for_runtime_dependencies: ['ADSL']}
  ]
};
renderGraphAwareDashboard();
const availability = actionAvailability();
const view = primaryNextActionView();
console.log(JSON.stringify({
  graphStatus: nodes.get('graphStatus').textContent,
  title: view.title,
  detail: view.detail,
  finalizeReady: availability.finalize.ready,
  generateReady: availability.generate.ready,
  approveReady: availability.approveCode.ready,
  runReady: availability.runApproved.ready,
  generateReason: availability.generate.reason,
  progressDetail: nodes.get('studyProgressDetail').textContent,
  stepsHtml: nodes.get('studyProgressSteps').innerHTML,
  dependencyHtml: nodes.get('dependencyGraph').innerHTML,
  boardHtml: nodes.get('datasetBoard').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_waiting_runtime_dependency.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["graphStatus"], "waiting upstream")
        self.assertFalse(result["finalizeReady"])
        self.assertFalse(result["generateReady"])
        self.assertFalse(result["approveReady"])
        self.assertFalse(result["runReady"])
        self.assertIn("ADLB is waiting for ADSL", result["title"])
        self.assertIn("ADLB is waiting for real local runtime output from ADSL", result["detail"])
        self.assertIn("ADLB is waiting for real local runtime output from ADSL", result["generateReason"])
        self.assertIn("Reference ADaM files do not satisfy this runtime dependency", result["generateReason"])
        self.assertIn("waiting for ADSL", result["stepsHtml"])
        self.assertIn("waiting", result["stepsHtml"])
        self.assertIn("waiting upstream", result["boardHtml"])
        self.assertIn("ADLB is paused until ADSL has a real local runtime output in this run.", result["dependencyHtml"])
        self.assertIn("waiting for real upstream output: ADSL", result["boardHtml"])
        self.assertIn("dataset-card active  waiting", result["boardHtml"])

    def test_index_code_approval_uses_graph_command_when_lg3_contract_exists(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_lg3_code_resume' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('reviewer').value = 'tester';
node('modelMode').value = 'mock';
const calls = [];
const bodies = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (options.body) bodies.push(JSON.parse(options.body));
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_code_resume',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'code_review',
      action: 'approve',
      status: 'ready_to_execute',
      current_interrupt: null,
      approved: true,
      executed: false,
      terminal_failure: false,
      next_action: 'execute_approved_code',
      graph_state_path: 'runs/run_ui_lg3_code_resume/graph_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_code_resume',
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract'
        }
      },
      datasets: {
        ADAE: {
          status: 'ready_to_execute',
          code_state: {status: 'approved', code_path: 'runs/run_ui_lg3_code_resume/code/build_adae.R'}
        }
      }
    })};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({
      target_datasets: ['ADAE'],
      datasets: [{
        dataset: 'ADAE',
        status: 'ready_to_execute',
        next_action: 'execute_approved_code',
        action_label: 'Run the approved R code locally.',
        code_status: 'approved',
        blocked: false
      }]
    })};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_lg3_code_resume', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.graphState = {
  runtime_persistence: {
    native_dataset_full_run: {
      dataset: 'ADAE',
      contract: 'single_dataset_spec_code_review_execute',
      boundary: 'lg3_backend_contract'
    }
  }
};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'review_code', action_label: 'Review generated R code before execution.', code_status: 'generated', blocked: false}]};
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_lg3_code_resume',
    status: 'generated',
    code_path: 'runs/run_ui_lg3_code_resume/code/build_adae.R',
    generated_code: 'adae <- ae'
  }
};
await approveCode();
console.log(JSON.stringify({
  graphCommandCalled: calls.some((item) => item.includes('/graph-command')),
  nativeResumeCalled: calls.some((item) => item.includes('/native-full-run/resume')),
  legacyCodeReviewCalled: calls.some((item) => item.includes('/code-review')),
  executeCalled: calls.some((item) => item.includes('/execute-approved-code')),
  approveBody: bodies.find((item) => item.action === 'approve') || {},
  executeAfterApproval: bodies.find((item) => item.action === 'approve')?.execute_after_approval,
  reviewApproved: state.reviewByDataset.ADAE?.approved,
  reviewGraphCommand: state.reviewByDataset.ADAE?.graph_command === true,
  nativeResumeStillAvailable: nativeFullRunResumeAvailable('ADAE')
}));
"""
        script_path = TMP_ROOT / "ui_lg3_code_resume.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["graphCommandCalled"])
        self.assertFalse(result["nativeResumeCalled"])
        self.assertFalse(result["legacyCodeReviewCalled"])
        self.assertFalse(result["executeCalled"])
        self.assertNotIn("execute_after_approval", result["approveBody"])
        self.assertNotIn("config_path", result["approveBody"])
        self.assertNotIn("rscript_path", result["approveBody"])
        self.assertNotIn("executeAfterApproval", result)
        self.assertTrue(result["reviewApproved"])
        self.assertTrue(result["reviewGraphCommand"])
        self.assertFalse(result["nativeResumeStillAvailable"])

    def test_index_code_approval_uses_graph_command_from_progress_when_graph_state_missing(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_lg3_progress_contract' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('reviewer').value = 'tester';
node('modelMode').value = 'mock';
const calls = [];
const bodies = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (options.body) bodies.push(JSON.parse(options.body));
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_progress_contract',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'code_review',
      action: 'approve',
      status: 'ready_to_execute',
      current_interrupt: null,
      approved: true,
      executed: false,
      terminal_failure: false,
      next_action: 'execute_approved_code',
      graph_state_path: 'runs/run_ui_lg3_progress_contract/graph_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: false, json: async () => ({detail: 'graph_state unavailable in this browser test'})};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({
      requested_datasets: ['ADAE'],
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract'
        }
      },
      datasets: [{
        dataset: 'ADAE',
        status: 'ready_to_execute',
        next_action: 'execute_approved_code',
        action_label: 'Run the approved R code locally.',
        code_status: 'approved',
        blocked: false
      }]
    })};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_lg3_progress_contract', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.graphState = null;
state.runProgress = {
  requested_datasets: ['ADAE'],
  target_datasets: ['ADAE'],
  runtime_persistence: {
    native_dataset_full_run: {
      dataset: 'ADAE',
      contract: 'single_dataset_spec_code_review_execute',
      boundary: 'lg3_backend_contract'
    }
  },
  datasets: [{
    dataset: 'ADAE',
    next_action: 'review_code',
    action_label: 'Review generated R code before execution.',
    code_status: 'generated',
    blocked: false
  }]
};
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_lg3_progress_contract',
    status: 'generated',
    code_path: 'runs/run_ui_lg3_progress_contract/code/build_adae.R',
    generated_code: 'adae <- ae'
  }
};
await approveCode();
console.log(JSON.stringify({
  graphCommandCalled: calls.some((item) => item.includes('/graph-command')),
  nativeResumeCalled: calls.some((item) => item.includes('/native-full-run/resume')),
  legacyCodeReviewCalled: calls.some((item) => item.includes('/code-review')),
  graphStateLoaded: state.graphState !== null,
  nativeResumeStillAvailable: nativeFullRunResumeAvailable('ADAE'),
  reviewNativeFullRun: state.reviewByDataset.ADAE?.native_full_run === true,
  reviewGraphCommand: state.reviewByDataset.ADAE?.graph_command === true,
  approveBody: bodies.find((item) => item.action === 'approve') || {}
}));
"""
        script_path = TMP_ROOT / "ui_lg3_progress_contract_resume.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["graphCommandCalled"])
        self.assertFalse(result["nativeResumeCalled"])
        self.assertFalse(result["legacyCodeReviewCalled"])
        self.assertFalse(result["graphStateLoaded"])
        self.assertFalse(result["nativeResumeStillAvailable"])
        self.assertFalse(result["reviewNativeFullRun"])
        self.assertTrue(result["reviewGraphCommand"])
        self.assertNotIn("execute_after_approval", result["approveBody"])
        self.assertNotIn("config_path", result["approveBody"])
        self.assertNotIn("rscript_path", result["approveBody"])

    def test_index_does_not_use_lg3_resume_when_contract_marks_resume_unavailable(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_lg3_resume_unavailable' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('reviewer').value = 'tester';
node('modelMode').value = 'mock';
const calls = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_resume_unavailable',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'code_review',
      action: 'approve',
      status: 'ready_to_execute',
      current_interrupt: null,
      approved: true,
      executed: false,
      terminal_failure: false,
      next_action: 'execute_approved_code',
      graph_state_path: 'runs/run_ui_lg3_resume_unavailable/graph_state.json',
      workflow_state_path: 'runs/run_ui_lg3_resume_unavailable/workflow_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_resume_unavailable',
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract',
          compatibility_resume_currently_available: false,
          compatibility_resume_boundary: 'historical_contract_only'
        }
      },
      datasets: {
        ADAE: {
          status: 'needs_review',
          code_state: {status: 'generated', code_path: 'runs/run_ui_lg3_resume_unavailable/code/build_adae.R'},
          current_interrupt: {name: 'code_review', status: 'open', dataset: 'ADAE'}
        }
      }
    })};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({
      requested_datasets: ['ADAE'],
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract',
          compatibility_resume_currently_available: false,
          compatibility_resume_boundary: 'historical_contract_only'
        }
      },
      datasets: [{
        dataset: 'ADAE',
        next_action: 'review_code',
        action_label: 'Review generated R code before execution.',
        code_status: 'generated',
        blocked: false
      }]
    })};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_lg3_resume_unavailable', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.graphState = {
  runtime_persistence: {
    native_dataset_full_run: {
      dataset: 'ADAE',
      contract: 'single_dataset_spec_code_review_execute',
      boundary: 'lg3_backend_contract',
      compatibility_resume_currently_available: false,
      compatibility_resume_boundary: 'historical_contract_only'
    }
  }
};
state.runProgress = {
  requested_datasets: ['ADAE'],
  target_datasets: ['ADAE'],
  datasets: [{
    dataset: 'ADAE',
    next_action: 'review_code',
    action_label: 'Review generated R code before execution.',
    code_status: 'generated',
    blocked: false
  }]
};
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_lg3_resume_unavailable',
    status: 'generated',
    code_path: 'runs/run_ui_lg3_resume_unavailable/code/build_adae.R',
    generated_code: 'adae <- ae'
  }
};
await approveCode();
console.log(JSON.stringify({
  graphCommandCalled: calls.some((item) => item.includes('/graph-command')),
  nativeResumeCalled: calls.some((item) => item.includes('/native-full-run/resume')),
  legacyCodeReviewCalled: calls.some((item) => item.includes('/code-review')),
  nativeResumeAvailable: nativeFullRunResumeAvailable('ADAE'),
  reviewNativeFullRun: state.reviewByDataset.ADAE?.native_full_run === true,
  reviewGraphCommand: state.reviewByDataset.ADAE?.graph_command === true
}));
"""
        script_path = TMP_ROOT / "ui_lg3_resume_unavailable.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["graphCommandCalled"])
        self.assertFalse(result["nativeResumeCalled"])
        self.assertFalse(result["legacyCodeReviewCalled"])
        self.assertFalse(result["nativeResumeAvailable"])
        self.assertFalse(result["reviewNativeFullRun"])
        self.assertTrue(result["reviewGraphCommand"])

    def test_index_graph_state_contract_absence_overrides_stale_lg3_progress_contract(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_stale_lg3_progress' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('reviewer').value = 'tester';
node('modelMode').value = 'mock';
const calls = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_stale_lg3_progress',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'code_review',
      action: 'approve',
      status: 'ready_to_execute',
      current_interrupt: null,
      approved: true,
      executed: false,
      terminal_failure: false,
      next_action: 'execute_approved_code',
      graph_state_path: 'runs/run_ui_stale_lg3_progress/graph_state.json',
      workflow_state_path: 'runs/run_ui_stale_lg3_progress/workflow_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_stale_lg3_progress',
      target_datasets: ['ADAE'],
      runtime_persistence: {},
      datasets: {
        ADAE: {
          status: 'pending',
          code_state: {status: 'approved', code_path: 'runs/run_ui_stale_lg3_progress/code/build_adae.R'}
        }
      }
    })};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({
      requested_datasets: ['ADAE'],
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract'
        }
      },
      datasets: [{
        dataset: 'ADAE',
        status: 'ready_to_execute',
        next_action: 'execute_approved_code',
        action_label: 'Run the approved R code locally.',
        code_status: 'approved',
        blocked: false
      }]
    })};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_stale_lg3_progress', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.graphState = {runtime_persistence: {}};
state.runProgress = {
  requested_datasets: ['ADAE'],
  target_datasets: ['ADAE'],
  runtime_persistence: {
    native_dataset_full_run: {
      dataset: 'ADAE',
      contract: 'single_dataset_spec_code_review_execute',
      boundary: 'lg3_backend_contract'
    }
  },
  datasets: [{
    dataset: 'ADAE',
    next_action: 'review_code',
    action_label: 'Review generated R code before execution.',
    code_status: 'generated',
    blocked: false
  }]
};
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_stale_lg3_progress',
    status: 'generated',
    code_path: 'runs/run_ui_stale_lg3_progress/code/build_adae.R',
    generated_code: 'adae <- ae'
  }
};
await approveCode();
console.log(JSON.stringify({
  graphCommandCalled: calls.some((item) => item.includes('/graph-command')),
  nativeResumeCalled: calls.some((item) => item.includes('/native-full-run/resume')),
  legacyCodeReviewCalled: calls.some((item) => item.includes('/code-review')),
  graphStillLoaded: state.graphState !== null,
  hasContractAfterRefresh: hasNativeFullRunContract('ADAE'),
  reviewNativeFullRun: state.reviewByDataset.ADAE?.native_full_run === true,
  reviewGraphCommand: state.reviewByDataset.ADAE?.graph_command === true
}));
"""
        script_path = TMP_ROOT / "ui_stale_lg3_progress_contract.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["graphCommandCalled"])
        self.assertFalse(result["nativeResumeCalled"])
        self.assertFalse(result["legacyCodeReviewCalled"])
        self.assertTrue(result["graphStillLoaded"])
        self.assertFalse(result["hasContractAfterRefresh"])
        self.assertFalse(result["reviewNativeFullRun"])
        self.assertTrue(result["reviewGraphCommand"])

    def test_index_draft_approval_uses_graph_command_when_lg3_contract_exists(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_lg3_draft_resume' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('reviewer').value = 'tester';
node('modelMode').value = 'mock';
const calls = [];
const bodies = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (options.body) bodies.push(JSON.parse(options.body));
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_draft_resume',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'draft_spec_review',
      action: 'approve',
      status: 'pending',
      current_interrupt: null,
      approved: true,
      executed: false,
      terminal_failure: false,
      next_action: 'generate_code',
      graph_state_path: 'runs/run_ui_lg3_draft_resume/graph_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_draft_resume',
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract'
        }
      },
      datasets: {
        ADAE: {
          status: 'pending',
          spec_state: {status: 'approved', approved_spec_path: 'runs/run_ui_lg3_draft_resume/approved_specs/adae_approved_spec.json'},
          current_interrupt: null
        }
      }
    })};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({
      target_datasets: ['ADAE'],
      datasets: [{
        dataset: 'ADAE',
        status: 'pending',
        next_action: 'generate_code',
        action_label: 'Generate R code.',
        spec_status: 'approved',
        blocked: false
      }]
    })};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_draft_resume',
      dataset_reviews: []
    })};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.graphState = {
  runtime_persistence: {
    native_study_product_loop: {
      full_run_datasets: {
        ADAE: {
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract'
        }
      }
    }
  }
};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'review_draft_spec', action_label: 'Review generated draft spec before code generation.', spec_status: 'draft_generated', blocked: false}]};
state.draftSpecByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_lg3_draft_resume',
    variables: [{variable: 'AETERM', type: 'text', source_domains: ['AE'], derivation: 'copy AE.AETERM', risk_level: 'low'}],
    warnings: []
  }
};
await approveDraftSpec();
console.log(JSON.stringify({
  graphCommandCalled: calls.some((item) => item.includes('/graph-command')),
  nativeResumeCalled: calls.some((item) => item.includes('/native-full-run/resume')),
  legacyDraftReviewCalled: calls.some((item) => item.includes('/draft-spec-review')),
  bodyHasConfig: Object.prototype.hasOwnProperty.call(bodies.find((item) => item.action === 'approve') || {}, 'config_path'),
  bodyHasRscript: Object.prototype.hasOwnProperty.call(bodies.find((item) => item.action === 'approve') || {}, 'rscript_path'),
  generatedCode: state.generatedByDataset.ADAE?.generated_code || '',
  codeStatus: nodes.get('codeStatus').textContent,
  graphStillHasContract: hasNativeFullRunContract('ADAE'),
  draftGraphCommand: state.draftSpecReviewByDataset.ADAE?.graph_command === true
}));
"""
        script_path = TMP_ROOT / "ui_lg3_draft_resume.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["graphCommandCalled"])
        self.assertFalse(result["nativeResumeCalled"])
        self.assertFalse(result["legacyDraftReviewCalled"])
        self.assertFalse(result["bodyHasConfig"])
        self.assertFalse(result["bodyHasRscript"])
        self.assertEqual(result["generatedCode"], "")
        self.assertEqual(result["codeStatus"], "not generated")
        self.assertFalse(result["graphStillHasContract"])
        self.assertTrue(result["draftGraphCommand"])

    def test_index_generate_button_starts_native_full_run(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_native_full_run' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('configPath').value = 'studies/_template/configs/mock_downstream.json';
node('modelMode').value = 'mock';
const calls = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (url === '/runs/prepare') {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run',
        requested_datasets: ['ADAE'],
        target_datasets: ['ADAE'],
        runnable_datasets: ['ADAE'],
        blocked_datasets: [],
        dependency_review_status: 'accepted',
        dependency_decisions: []
      })
    };
  }
  if (url.includes('/native-full-run')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run',
        dataset: 'ADAE',
        phase: 'waiting_for_human_gate',
        status: 'needs_review',
        current_interrupt: {name: 'code_review', status: 'open', dataset: 'ADAE'},
        next_action: 'code_review',
        code_path: 'runs/run_ui_native_full_run/code/build_adae.R',
        static_check_path: 'runs/run_ui_native_full_run/static_checks/adae_static_check.json',
        graph_state_path: 'runs/run_ui_native_full_run/graph_state.json'
      })
    };
  }
  if (url.includes('/graph-state')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run',
        requested_datasets: ['ADAE'],
        target_datasets: ['ADAE'],
        runnable_datasets: ['ADAE'],
        blocked_datasets: [],
        dependency_review_status: 'accepted',
        datasets: {
          ADAE: {
            status: 'needs_review',
            spec_state: {status: 'input_spec_ready', input_spec_path: 'inputs/input_spec/ads_adae_full.csv'},
            code_state: {
              status: 'generated',
              code_path: 'runs/run_ui_native_full_run/code/build_adae.R',
              static_check_path: 'runs/run_ui_native_full_run/static_checks/adae_static_check.json'
            },
            current_interrupt: {name: 'code_review', status: 'open', dataset: 'ADAE'}
          }
        }
      })
    };
  }
  if (url.includes('/progress')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run',
        status: 'needs_review',
        next_action: 'review_code',
        action_label: 'Review generated R code before execution.',
        dependency_review_status: 'accepted',
        target_datasets: ['ADAE'],
        runnable_datasets: ['ADAE'],
        blocked_datasets: [],
        datasets: [{
          dataset: 'ADAE',
          status: 'needs_review',
          next_action: 'review_code',
          action_label: 'Review generated R code before execution.',
          code_status: 'generated',
          blocked: false
        }]
      })
    };
  }
  if (url.includes('/review-summary')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run',
        dataset_reviews: [{
          dataset: 'ADAE',
          status: 'needs_review',
          generated_code_path: 'runs/run_ui_native_full_run/code/build_adae.R',
          generated_code: 'adae <- ae',
          assumptions: ['native full-run code review gate'],
          expected_outputs: ['ADAE']
        }]
      })
    };
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_native_full_run',
    status: 'generated',
    code_path: 'runs/run_ui_native_full_run/code/old_build_adae.R',
    generated_code: 'old code must be cleared'
  }
};
state.plan = {
  requested_datasets: ['ADAE'],
  target_datasets: ['ADAE'],
  runnable_datasets: ['ADAE'],
  blocked_datasets: [],
  dependency_review_status: 'accepted'
};
state.runProgress = {
  datasets: [{
    dataset: 'ADAE',
    next_action: 'generate_code',
    action_label: 'Generate R code from the approved spec evidence.',
    blocked: false
  }]
};
state.inputSummary = {specs: [{dataset: 'ADAE', file_name: 'ads_adae_full.csv'}]};
await generateCode();
console.log(JSON.stringify({
  nativeCalled: calls.some((item) => item.includes('/native-full-run')),
  legacyGenerateCalled: calls.some((item) => item.includes('/generate-code')),
  generatedCode: state.generatedByDataset.ADAE?.generated_code || '',
  generatedCodePath: state.generatedByDataset.ADAE?.code_path || '',
  selectedView: state.selectedView,
  codeStatus: nodes.get('codeStatus').textContent,
  reviewPane: nodes.get('reviewPane').innerHTML,
  operationTitle: nodes.get('operationTitle').textContent
}));
"""
        script_path = TMP_ROOT / "ui_native_full_run_generate.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["nativeCalled"])
        self.assertFalse(result["legacyGenerateCalled"])
        self.assertEqual(result["generatedCode"], "adae <- ae")
        self.assertTrue(result["generatedCodePath"].endswith("code/build_adae.R"))
        self.assertEqual(result["selectedView"], "summary")
        self.assertEqual(result["codeStatus"], "review")
        self.assertIn("R code is ready for ADAE", result["reviewPane"])
        self.assertEqual(result["operationTitle"], "R code generated")

    def test_index_native_full_run_clears_stale_code_when_new_artifact_text_missing(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_native_full_run_stale_code' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('modelMode').value = 'mock';
const calls = [];
global.fetch = async (path) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/native-full-run')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_stale_code',
        dataset: 'ADAE',
        phase: 'waiting_for_human_gate',
        status: 'needs_review',
        current_interrupt: {name: 'code_review', status: 'open', dataset: 'ADAE'},
        next_action: 'code_review',
        code_path: 'runs/run_ui_native_full_run_stale_code/code/build_adae_new.R',
        static_check_path: 'runs/run_ui_native_full_run_stale_code/static_checks/adae_static_check.json'
      })
    };
  }
  if (url.includes('/graph-state')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_stale_code',
        requested_datasets: ['ADAE'],
        target_datasets: ['ADAE'],
        datasets: {
          ADAE: {
            status: 'needs_review',
            spec_state: {status: 'input_spec_ready', input_spec_path: 'inputs/input_spec/ads_adae_full.csv'},
            code_state: {status: 'generated', code_path: 'runs/run_ui_native_full_run_stale_code/code/build_adae_new.R'},
            current_interrupt: {name: 'code_review', status: 'open', dataset: 'ADAE'}
          }
        }
      })
    };
  }
  if (url.includes('/progress')) {
    return {
      ok: true,
      json: async () => ({
        datasets: [{
          dataset: 'ADAE',
          next_action: 'review_code',
          action_label: 'Review generated R code before execution.',
          code_status: 'generated',
          blocked: false
        }]
      })
    };
  }
  if (url.includes('/review-summary')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_stale_code',
        dataset_reviews: [{
          dataset: 'ADAE',
          status: 'needs_review',
          generated_code_path: 'runs/run_ui_native_full_run_stale_code/code/build_adae_new.R'
        }]
      })
    };
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'generate_code', action_label: 'Generate R code.', blocked: false}]};
state.inputSummary = {specs: [{dataset: 'ADAE', file_name: 'ads_adae_full.csv'}]};
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_native_full_run_stale_code',
    status: 'generated',
    code_path: 'runs/run_ui_native_full_run_stale_code/code/old_build_adae.R',
    generated_code: 'old code must not survive'
  }
};
await generateCode();
const availability = actionAvailability();
await approveCode();
console.log(JSON.stringify({
  generatedCode: state.generatedByDataset.ADAE?.generated_code || '',
  codePath: state.generatedByDataset.ADAE?.code_path || '',
  approveReady: availability.approveCode.ready,
  codeReviewPosted: calls.some((item) => item.includes('/code-review')),
  reviewPane: nodes.get('reviewPane').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_native_full_run_clears_stale_code.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["generatedCode"], "")
        self.assertTrue(result["codePath"].endswith("code/build_adae_new.R"))
        self.assertFalse(result["approveReady"])
        self.assertFalse(result["codeReviewPosted"])
        self.assertIn("Generated-code metadata exists", result["reviewPane"])

    def test_index_generate_button_handles_native_full_run_draft_gate(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_native_full_run_draft' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('modelMode').value = 'mock';
const stepCalls = [];
const calls = [];
const draftVariables = [{variable: 'AETERM', type: 'text', source_domains: ['AE'], derivation: 'copy AE.AETERM', risk_level: 'low'}];
global.fetch = async (path) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/native-full-run')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_draft',
        dataset: 'ADAE',
        phase: 'waiting_for_human_gate',
        status: 'needs_review',
        current_interrupt: {name: 'draft_spec_review', status: 'open', dataset: 'ADAE'},
        next_action: 'draft_spec_review',
        draft_spec_path: 'runs/run_ui_native_full_run_draft/specs/adae_draft_spec.json',
        graph_state_path: 'runs/run_ui_native_full_run_draft/graph_state.json'
      })
    };
  }
  if (url.includes('/graph-state')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_draft',
        requested_datasets: ['ADAE'],
        target_datasets: ['ADAE'],
        datasets: {
          ADAE: {
            status: 'needs_review',
            spec_state: {
              status: 'draft_generated',
              draft_spec_path: 'runs/run_ui_native_full_run_draft/specs/adae_draft_spec.json',
              variables: draftVariables,
              warnings: ['Review draft before code generation.']
            },
            current_interrupt: {name: 'draft_spec_review', status: 'open', dataset: 'ADAE'}
          }
        }
      })
    };
  }
  if (url.includes('/progress')) {
    return {
      ok: true,
      json: async () => ({
        datasets: [{
          dataset: 'ADAE',
          next_action: 'review_draft_spec',
          action_label: 'Review generated draft spec before code generation.',
          spec_status: 'draft_generated',
          blocked: false
        }]
      })
    };
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_native_full_run_draft', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
const originalSetStep = setStep;
setStep = (index) => { stepCalls.push(index); originalSetStep(index); };
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'generate_code', action_label: 'Generate R code.', blocked: false}]};
state.inputSummary = {specs: []};
await generateCode();
console.log(JSON.stringify({
  nativeCalled: calls.some((item) => item.includes('/native-full-run')),
  legacyGenerateCalled: calls.some((item) => item.includes('/generate-code')),
  draftVariable: state.draftSpecByDataset.ADAE?.variables?.[0]?.variable || '',
  generatedExists: Boolean(state.generatedByDataset.ADAE),
  codeStatus: nodes.get('codeStatus').textContent,
  operationTitle: nodes.get('operationTitle').textContent,
  draftPane: nodes.get('draftSpecPane').innerHTML,
  reviewPane: nodes.get('reviewPane').innerHTML,
  lastStep: stepCalls.at(-1)
}));
"""
        script_path = TMP_ROOT / "ui_native_full_run_generate_draft_gate.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["nativeCalled"])
        self.assertFalse(result["legacyGenerateCalled"])
        self.assertEqual(result["draftVariable"], "AETERM")
        self.assertFalse(result["generatedExists"])
        self.assertEqual(result["codeStatus"], "draft review")
        self.assertEqual(result["operationTitle"], "Draft spec ready")
        self.assertIn("Draft spec for ADAE", result["draftPane"])
        self.assertIn("Review the generated draft spec above before R code can be generated", result["reviewPane"])
        self.assertEqual(result["lastStep"], 4)

    def test_index_generate_revise_spec_uses_native_full_run_not_draft_spec_endpoint(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_lg3_revise_spec' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('modelMode').value = 'mock';
const calls = [];
const draftVariables = [{variable: 'AETERM', type: 'text', source_domains: ['AE'], derivation: 'review revised spec', risk_level: 'medium'}];
global.fetch = async (path) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/draft-spec')) throw new Error('Product UI must not call split-flow draft-spec for revise_approved_spec.');
  if (url.includes('/native-full-run')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_lg3_revise_spec',
        dataset: 'ADAE',
        phase: 'waiting_for_human_gate',
        status: 'needs_review',
        current_interrupt: {name: 'draft_spec_review', status: 'open', dataset: 'ADAE'},
        next_action: 'draft_spec_review',
        draft_spec_path: 'runs/run_ui_lg3_revise_spec/specs/adae_revised_draft_spec.json',
        graph_state_path: 'runs/run_ui_lg3_revise_spec/graph_state.json'
      })
    };
  }
  if (url.includes('/graph-state')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_lg3_revise_spec',
        requested_datasets: ['ADAE'],
        target_datasets: ['ADAE'],
        runtime_persistence: {
          native_dataset_full_run: {
            dataset: 'ADAE',
            contract: 'single_dataset_spec_code_review_execute',
            boundary: 'lg3_backend_contract',
            phase: 'waiting_for_human_gate',
            terminal_failure_followup: {action: 'revise_spec', next_action: 'revise_approved_spec'}
          }
        },
        datasets: {
          ADAE: {
            status: 'needs_review',
            spec_state: {
              status: 'draft_generated',
              draft_spec_path: 'runs/run_ui_lg3_revise_spec/specs/adae_revised_draft_spec.json',
              variables: draftVariables,
              warnings: ['Review revised spec before code generation.']
            },
            current_interrupt: {name: 'draft_spec_review', status: 'open', dataset: 'ADAE'}
          }
        }
      })
    };
  }
  if (url.includes('/progress')) {
    return {
      ok: true,
      json: async () => ({
        target_datasets: ['ADAE'],
        datasets: [{
          dataset: 'ADAE',
          next_action: 'review_draft_spec',
          action_label: 'Review generated draft spec before code generation.',
          blocked: false
        }]
      })
    };
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_lg3_revise_spec', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'revise_approved_spec', action_label: 'Revise approved spec before regenerating code.', blocked: false}]};
state.draftSpecReviewByDataset = {ADAE: {dataset: 'ADAE', approved: true}};
state.generatedByDataset = {ADAE: {dataset: 'ADAE', run_id: 'run_ui_lg3_revise_spec', status: 'generated', generated_code: 'old code must clear'}};
await generateCode();
console.log(JSON.stringify({
  nativeCalled: calls.some((item) => item.includes('/native-full-run')),
  splitDraftSpecCalled: calls.some((item) => item.includes('/draft-spec')),
  draftVariable: state.draftSpecByDataset.ADAE?.variables?.[0]?.variable || '',
  generatedExists: Boolean(state.generatedByDataset.ADAE),
  codeStatus: nodes.get('codeStatus').textContent,
  operationTitle: nodes.get('operationTitle').textContent,
  draftPane: nodes.get('draftSpecPane').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_lg3_revise_spec_native_full_run.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["nativeCalled"])
        self.assertFalse(result["splitDraftSpecCalled"])
        self.assertEqual(result["draftVariable"], "AETERM")
        self.assertFalse(result["generatedExists"])
        self.assertEqual(result["codeStatus"], "draft review")
        self.assertEqual(result["operationTitle"], "Draft spec ready")
        self.assertIn("Draft spec for ADAE", result["draftPane"])

    def test_index_native_full_run_draft_gate_clears_stale_generated_code(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_native_full_run_draft_stale' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('modelMode').value = 'mock';
const calls = [];
const draftVariables = [{variable: 'AETERM', type: 'text', source_domains: ['AE'], derivation: 'copy AE.AETERM', risk_level: 'low'}];
global.fetch = async (path) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/native-full-run')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_draft_stale',
        dataset: 'ADAE',
        phase: 'waiting_for_human_gate',
        status: 'needs_review',
        current_interrupt: {name: 'draft_spec_review', status: 'open', dataset: 'ADAE'},
        next_action: 'draft_spec_review',
        draft_spec_path: 'runs/run_ui_native_full_run_draft_stale/specs/adae_draft_spec.json'
      })
    };
  }
  if (url.includes('/graph-state')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_draft_stale',
        requested_datasets: ['ADAE'],
        target_datasets: ['ADAE'],
        datasets: {
          ADAE: {
            status: 'needs_review',
            spec_state: {
              status: 'draft_generated',
              draft_spec_path: 'runs/run_ui_native_full_run_draft_stale/specs/adae_draft_spec.json',
              variables: draftVariables,
              warnings: []
            },
            code_state: {status: 'generated', code_path: 'runs/run_ui_native_full_run_draft_stale/code/old_build_adae.R'},
            current_interrupt: {name: 'draft_spec_review', status: 'open', dataset: 'ADAE'}
          }
        }
      })
    };
  }
  if (url.includes('/progress')) {
    return {
      ok: true,
      json: async () => ({
        datasets: [{
          dataset: 'ADAE',
          next_action: 'review_draft_spec',
          action_label: 'Review generated draft spec before code generation.',
          spec_status: 'draft_generated',
          code_status: 'generated',
          blocked: false
        }]
      })
    };
  }
  if (url.includes('/review-summary')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_native_full_run_draft_stale',
        dataset_reviews: [{
          dataset: 'ADAE',
          status: 'needs_review',
          generated_code_path: 'runs/run_ui_native_full_run_draft_stale/code/old_build_adae.R',
          generated_code: 'old code must not be shown while draft spec is under review'
        }]
      })
    };
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'generate_code', action_label: 'Generate R code.', blocked: false}]};
state.inputSummary = {specs: []};
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_native_full_run_draft_stale',
    status: 'generated',
    code_path: 'runs/run_ui_native_full_run_draft_stale/code/old_build_adae.R',
    generated_code: 'old code must not survive draft gate'
  }
};
await generateCode();
const availability = actionAvailability();
await approveCode();
console.log(JSON.stringify({
  generatedExists: Boolean(state.generatedByDataset.ADAE),
  approveReady: availability.approveCode.ready,
  codeStatus: nodes.get('codeStatus').textContent,
  codeReviewPosted: calls.some((item) => item.includes('/code-review')),
  reviewPane: nodes.get('reviewPane').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_native_full_run_draft_gate_clears_stale_code.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertFalse(result["generatedExists"])
        self.assertFalse(result["approveReady"])
        self.assertEqual(result["codeStatus"], "draft review")
        self.assertFalse(result["codeReviewPosted"])
        self.assertIn("Review the generated draft spec above before R code can be generated", result["reviewPane"])
        self.assertNotIn("R code is ready for ADAE", result["reviewPane"])

    def test_index_native_full_run_draft_gate_points_to_draft_review(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ok: true, json: async () => ({})});
""" + script + r"""
state.selectedTarget = 'ADAE';
state.runProgress = {
  datasets: [{
    dataset: 'ADAE',
    next_action: 'review_draft_spec',
    action_label: 'Review generated draft spec before code generation.',
    spec_status: 'draft_generated',
    blocked: false
  }]
};
state.draftSpecByDataset = {
  ADAE: {
    dataset: 'ADAE',
    variables: [{variable: 'AETERM', type: 'text', source_domains: ['AE'], derivation: 'copy AE.AETERM', risk_level: 'low'}],
    warnings: []
  }
};
state.generatedByDataset = {};
state.selectedView = 'summary';
renderDraftSpecPane();
renderPane();
console.log(JSON.stringify({
  draftPane: nodes.get('draftSpecPane').innerHTML,
  reviewPane: nodes.get('reviewPane').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_native_full_run_draft_gate.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIn("Draft spec for ADAE", result["draftPane"])
        self.assertIn("Review the generated draft spec above before R code can be generated", result["reviewPane"])
        self.assertNotIn("Generate code after choosing a target", result["reviewPane"])

    def test_index_code_approval_does_not_execute_split_flow_code_from_product_ui(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: '',
      disabled: false,
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('studyDir').value = 'D:/tmp/study';
node('runId').value = 'run_ui_split_review_execute';
node('reviewer').value = 'local_user';
node('reviewNotes').value = 'reviewed';
node('rscriptPath').value = '';
const calls = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_split_review_execute',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'code_review',
      action: 'approve',
      status: 'ready_to_execute',
      current_interrupt: null,
      approved: true,
      executed: false,
      next_action: 'execute_approved_code',
      graph_state_path: 'runs/run_ui_split_review_execute/graph_state.json'
    })};
  }
  if (url.includes('/execute-approved-code')) throw new Error('Product UI must not call split-flow execute.');
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({target_datasets: ['ADAE'], datasets: {}})};
  }
  if (url.includes('/progress')) {
    const approved = calls.some((item) => item.includes('/graph-command'));
    return {
      ok: true,
      json: async () => ({
        target_datasets: ['ADAE'],
        blocked_datasets: [],
        datasets: [{
          dataset: 'ADAE',
          next_action: approved ? 'execute_approved_code' : 'review_code',
          action_label: approved ? 'Run the approved R code locally.' : 'Review generated R code.',
          blocked: false
        }]
      })
    };
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], blocked_datasets: []};
state.generatedByDataset = {ADAE: {dataset: 'ADAE', run_id: 'run_ui_split_review_execute', status: 'generated', generated_code: 'x <- 1'}};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'review_code', action_label: 'Review generated R code.', blocked: false}]};
await approveCode();
const afterApprove = [...calls];
state.reviewByDataset = {ADAE: {dataset: 'ADAE', approved: true}};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'execute_approved_code', action_label: 'Run approved code.', blocked: false}]};
await runApprovedCode();
console.log(JSON.stringify({
  afterApprove,
  afterRun: calls,
  approved: state.reviewByDataset.ADAE.approved,
  executionExists: Boolean(state.executionByDataset.ADAE),
  reviewPane: nodes.get('reviewPane').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_split_review_execute.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(any("/graph-command" in item for item in result["afterApprove"]))
        self.assertFalse(any("/code-review" in item for item in result["afterApprove"]))
        self.assertFalse(any("/execute-approved-code" in item for item in result["afterApprove"]))
        self.assertFalse(any("/execute-approved-code" in item for item in result["afterRun"]))
        self.assertFalse(any("/native-full-run/execute" in item for item in result["afterRun"]))
        self.assertTrue(result["approved"])
        self.assertFalse(result["executionExists"])
        self.assertIn("graph-owned native full-run code", result["reviewPane"])

    def test_index_run_approved_code_uses_native_full_run_execute_when_contract_exists(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'studyDir' ? 'D:/tmp/study' : id === 'runId' ? 'run_ui_lg3_execute' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
      scrollIntoView() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
node('rscriptPath').value = 'C:/Dev/R-4.5.2/bin/Rscript.exe';
const calls = [];
global.fetch = async (path, options = {}) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/native-full-run/execute')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_execute',
      dataset: 'ADAE',
      status: 'completed',
      validation_status: 'pass',
      terminal_failure: false,
      graph_state_path: 'runs/run_ui_lg3_execute/graph_state.json'
    })};
  }
  if (url.includes('/graph-state')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_lg3_execute',
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract',
          phase: 'executed'
        }
      },
      datasets: {
        ADAE: {
          status: 'completed',
          code_state: {status: 'approved', code_path: 'runs/run_ui_lg3_execute/code/build_adae.R'},
          execution_state: {status: 'completed'}
        }
      }
    })};
  }
  if (url.includes('/progress')) {
    return {ok: true, json: async () => ({
      target_datasets: ['ADAE'],
      runtime_persistence: {
        native_dataset_full_run: {
          dataset: 'ADAE',
          contract: 'single_dataset_spec_code_review_execute',
          boundary: 'lg3_backend_contract',
          phase: 'executed'
        }
      },
      datasets: [{
        dataset: 'ADAE',
        status: 'completed',
        next_action: 'complete',
        action_label: 'Dataset completed.',
        code_status: 'approved',
        execution_status: 'completed',
        blocked: false
      }]
    })};
  }
  if (url.includes('/review-summary')) {
    return {ok: true, json: async () => ({study_id: 'PSY201', run_id: 'run_ui_lg3_execute', dataset_reviews: []})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: [], dependency_review_status: 'accepted'};
state.graphState = {
  runtime_persistence: {
    native_dataset_full_run: {
      dataset: 'ADAE',
      contract: 'single_dataset_spec_code_review_execute',
      boundary: 'lg3_backend_contract',
      phase: 'reviewed'
    }
  },
  datasets: {
    ADAE: {
      code_state: {status: 'approved', code_path: 'runs/run_ui_lg3_execute/code/build_adae.R'}
    }
  }
};
state.runProgress = {
  datasets: [{dataset: 'ADAE', next_action: 'execute_approved_code', action_label: 'Run approved code.', code_status: 'approved', blocked: false}]
};
state.generatedByDataset = {
  ADAE: {
    dataset: 'ADAE',
    run_id: 'run_ui_lg3_execute',
    status: 'generated',
    code_path: 'runs/run_ui_lg3_execute/code/build_adae.R',
    generated_code: 'adae <- ae'
  }
};
state.reviewByDataset = {ADAE: {dataset: 'ADAE', approved: true}};
await runApprovedCode();
console.log(JSON.stringify({
  nativeExecuteCalled: calls.some((item) => item.includes('/native-full-run/execute')),
  compatibilityExecuteCalled: calls.some((item) => item.includes('/execute-approved-code')),
  executionStatus: state.executionByDataset.ADAE.status
}));
"""
        script_path = TMP_ROOT / "ui_lg3_native_full_run_execute.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["nativeExecuteCalled"])
        self.assertFalse(result["compatibilityExecuteCalled"])
        self.assertEqual(result["executionStatus"], "completed")

    def test_index_load_review_summary_recovers_generated_code_for_graph_review_gate(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'runId' ? 'run_ui_review_recovery' : id === 'studyDir' ? 'D:/tmp/study' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    return node(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async (path) => {
  const url = String(path);
  if (url.includes('/review-summary')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_review_recovery',
        dataset_reviews: [{
          dataset: 'ADAE',
          status: 'needs_review',
          generated_code_path: 'runs/run_ui_review_recovery/code/build_adae.R',
          generated_code: 'adae <- ae',
          assumptions: ['AE is available.'],
          risk_points: ['Human review required.'],
          warnings: ['Static check limited scope.']
        }]
      })
    };
  }
  if (url.includes('/graph-state')) {
    const graphApproved = node('graphApproved').value === 'yes';
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_review_recovery',
        target_datasets: ['ADAE'],
        requested_datasets: ['ADAE'],
        datasets: {
          ADAE: {
            status: graphApproved ? 'pending' : 'needs_review',
            code_state: {status: graphApproved ? 'approved' : 'generated', code_path: 'runs/run_ui_review_recovery/code/build_adae.R'},
            current_interrupt: graphApproved ? null : {name: 'code_review', status: 'open', dataset: 'ADAE'}
          }
        }
      })
    };
  }
  if (url.includes('/progress')) {
    const graphApproved = node('graphApproved').value === 'yes';
    return {
      ok: true,
      json: async () => ({
        target_datasets: ['ADAE'],
        blocked_datasets: [],
        datasets: [{
          dataset: 'ADAE',
          status: graphApproved ? 'pending' : 'needs_review',
          next_action: graphApproved ? 'execute_approved_code' : 'review_code',
          action_label: graphApproved ? 'Run approved R code locally.' : 'Review generated R code before execution.',
          blocked: false
        }]
      })
    };
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: []};
state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'review_code', action_label: 'Review generated R code before execution.', blocked: false}]};
state.generatedByDataset = {};
await loadReviewSummary('run_ui_review_recovery');
const availability = actionAvailability();
const firstApproveButtonDisabled = nodes.get('approveButton').disabled;
const firstRunButtonDisabled = nodes.get('runApprovedButton').disabled;
state.generatedByDataset = {};
state.graphState = null;
state.runProgress = null;
applyReviewSummaryDataset({
  dataset: 'ADAE',
  status: 'needs_review',
  generated_code_path: 'runs/run_ui_review_recovery/code/build_adae.R',
  generated_code: 'artifact only'
});
const artifactOnlyRecovered = Boolean(state.generatedByDataset.ADAE);
node('graphApproved').value = 'yes';
state.reviewByDataset = {ADAE: {dataset: 'ADAE', approved: true}};
await loadReviewSummary('run_ui_review_recovery');
console.log(JSON.stringify({
  recoveredCode: state.generatedByDataset.ADAE.generated_code,
  recoveredPath: state.generatedByDataset.ADAE.code_path,
  assumptions: state.generatedByDataset.ADAE.assumptions,
  approveReady: availability.approveCode.ready,
  runReady: availability.runApproved.ready,
  approveButtonDisabled: firstApproveButtonDisabled,
  runButtonDisabled: firstRunButtonDisabled,
  artifactOnlyRecovered,
  approvalStillPresent: Boolean(state.reviewByDataset.ADAE?.approved)
}));
"""
        script_path = TMP_ROOT / "ui_review_summary_recovery.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["recoveredCode"], "adae <- ae")
        self.assertIn("build_adae.R", result["recoveredPath"])
        self.assertEqual(result["assumptions"], ["AE is available."])
        self.assertTrue(result["approveReady"])
        self.assertFalse(result["runReady"])
        self.assertFalse(result["approveButtonDisabled"])
        self.assertTrue(result["runButtonDisabled"])
        self.assertFalse(result["artifactOnlyRecovered"])
        self.assertTrue(result["approvalStillPresent"])

    def test_index_review_summary_without_graph_gate_cannot_enable_code_approval(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'runId' ? 'run_ui_review_recovery_no_gate' : id === 'studyDir' ? 'D:/tmp/study' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
const calls = [];
global.fetch = async (path) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/review-summary')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_review_recovery_no_gate',
        dataset_reviews: [{
          dataset: 'ADAE',
          status: 'needs_review',
          generated_code_path: 'runs/run_ui_review_recovery_no_gate/code/build_adae.R',
          generated_code: 'adae <- ae'
        }]
      })
    };
  }
  if (url.includes('/graph-state') || url.includes('/progress')) {
    return {ok: false, json: async () => ({detail: 'graph read model unavailable'})};
  }
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({dataset: 'ADAE', interrupt: 'code_review', action: 'approve', approved: true, graph_state_path: 'runs/run_ui_review_recovery_no_gate/graph_state.json'})};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: []};
await loadReviewSummary('run_ui_review_recovery_no_gate');
const availability = actionAvailability();
await approveCode();
console.log(JSON.stringify({
  generatedRecovered: Boolean(state.generatedByDataset.ADAE?.generated_code),
  approveReady: availability.approveCode.ready,
  approveButtonDisabled: nodes.get('approveButton').disabled,
  graphCommandPosted: calls.some((item) => item.includes('/graph-command')),
  codeReviewPosted: calls.some((item) => item.includes('/code-review')),
  reviewPane: nodes.get('reviewPane').innerHTML
}));
"""
        script_path = TMP_ROOT / "ui_review_summary_no_gate.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertFalse(result["generatedRecovered"])
        self.assertFalse(result["approveReady"])
        self.assertTrue(result["approveButtonDisabled"])
        self.assertFalse(result["graphCommandPosted"])
        self.assertFalse(result["codeReviewPosted"])
        self.assertIn("Generate code after choosing a target", result["reviewPane"])

    def test_index_review_summary_can_recover_code_when_graph_state_succeeds_without_progress(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
function node(id) {
  if (!nodes.has(id)) {
    nodes.set(id, {
      value: id === 'runId' ? 'run_ui_review_recovery_graph_only' : id === 'studyDir' ? 'D:/tmp/study' : '',
      textContent: '',
      innerHTML: '',
      className: '',
      dataset: {},
      disabled: false,
      classList: { add() {}, remove() {}, toggle() {} },
      addEventListener() {},
      querySelectorAll() { return []; },
      setAttribute() {},
    });
  }
  return nodes.get(id);
}
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) { return node(id); },
  querySelectorAll() { return []; },
};
const calls = [];
global.fetch = async (path) => {
  const url = String(path);
  calls.push(url);
  if (url.includes('/review-summary')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_review_recovery_graph_only',
        dataset_reviews: [{
          dataset: 'ADAE',
          status: 'needs_review',
          generated_code_path: 'runs/run_ui_review_recovery_graph_only/code/build_adae.R',
          generated_code: 'adae <- ae',
          assumptions: ['Recovered from graph state.']
        }]
      })
    };
  }
  if (url.includes('/graph-state')) {
    return {
      ok: true,
      json: async () => ({
        study_id: 'PSY201',
        run_id: 'run_ui_review_recovery_graph_only',
        target_datasets: ['ADAE'],
        requested_datasets: ['ADAE'],
        datasets: {
          ADAE: {
            status: 'needs_review',
            code_state: {status: 'generated', code_path: 'runs/run_ui_review_recovery_graph_only/code/build_adae.R'},
            current_interrupt: {name: 'code_review', status: 'open', dataset: 'ADAE'}
          }
        }
      })
    };
  }
  if (url.includes('/progress')) {
    return {ok: false, json: async () => ({detail: 'progress unavailable'})};
  }
  if (url.includes('/graph-command')) {
    return {ok: true, json: async () => ({
      study_id: 'PSY201',
      run_id: 'run_ui_review_recovery_graph_only',
      scope: 'dataset',
      dataset: 'ADAE',
      interrupt: 'code_review',
      action: 'approve',
      status: 'ready_to_execute',
      current_interrupt: null,
      approved: true,
      next_action: 'execute_approved_code',
      graph_state_path: 'runs/run_ui_review_recovery_graph_only/graph_state.json'
    })};
  }
  return {ok: true, json: async () => ({})};
};
""" + script + r"""
state.studyId = 'PSY201';
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], target_datasets: ['ADAE'], blocked_datasets: []};
await loadReviewSummary('run_ui_review_recovery_graph_only');
const availability = actionAvailability();
await approveCode();
console.log(JSON.stringify({
  graphStateLoaded: Boolean(state.graphState?.datasets?.ADAE),
  progressLoaded: Boolean(state.runProgress),
  recoveredCode: state.generatedByDataset.ADAE?.generated_code || '',
  approveReady: availability.approveCode.ready,
  approveButtonDisabled: nodes.get('approveButton').disabled,
  graphCommandPosted: calls.some((item) => item.includes('/graph-command')),
  codeReviewPosted: calls.some((item) => item.includes('/code-review')),
  approved: Boolean(state.reviewByDataset.ADAE?.approved)
}));
"""
        script_path = TMP_ROOT / "ui_review_summary_graph_only.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertTrue(result["graphStateLoaded"])
        self.assertFalse(result["progressLoaded"])
        self.assertEqual(result["recoveredCode"], "adae <- ae")
        self.assertTrue(result["approveReady"])
        self.assertFalse(result["approveButtonDisabled"])
        self.assertTrue(result["graphCommandPosted"])
        self.assertFalse(result["codeReviewPosted"])
        self.assertTrue(result["approved"])

    def test_index_action_availability_next_action_matrix(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_matrix' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
function check(nextAction) {
  state.selectedTarget = 'ADAE';
  state.selectedTargetsForPlan = ['ADAE'];
  state.plan = {requested_datasets: ['ADAE'], blocked_datasets: []};
  state.finalizedInputsByDataset = nextAction === 'finalize_inputs' ? {} : {ADAE: {input_spec_available: true}};
  state.draftSpecByDataset = nextAction === 'review_draft_spec'
    ? {ADAE: {dataset: 'ADAE', variables: [{variable: 'AETERM'}]}}
    : {};
  state.draftSpecReviewByDataset = {};
  state.generatedByDataset = {ADAE: {dataset: 'ADAE', run_id: 'run_ui_matrix', status: 'generated', generated_code: 'x <- 1'}};
  state.runProgress = {datasets: [{dataset: 'ADAE', next_action: nextAction, action_label: nextAction.replaceAll('_', ' '), blocked: false}]};
  const availability = actionAvailability();
  return {
    nextAction,
    finalize: availability.finalize.ready,
    approveDraft: availability.approveDraft.ready,
    generate: availability.generate.ready,
    generateLabel: availability.generate.label,
    approveCode: availability.approveCode.ready,
    runApproved: availability.runApproved.ready,
  };
}
const results = ['finalize_inputs', 'review_draft_spec', 'generate_code', 'revise_approved_spec', 'review_code', 'execute_approved_code'].map(check);
function graphGenerateWithoutLocalSpecGate() {
  state.selectedTarget = 'ADAE';
  state.selectedTargetsForPlan = ['ADAE'];
  state.plan = {requested_datasets: ['ADAE'], blocked_datasets: []};
  state.finalizedInputsByDataset = {};
  state.draftSpecByDataset = {};
  state.draftSpecReviewByDataset = {};
  state.inputSummary = {specs: []};
  state.generatedByDataset = {};
  state.runProgress = {datasets: [{dataset: 'ADAE', next_action: 'generate_code', action_label: 'Generate R code.', blocked: false}]};
  return actionAvailability().generate.ready;
}
results.push({nextAction: 'graph_generate_without_local_spec_gate', generate: graphGenerateWithoutLocalSpecGate()});
console.log(JSON.stringify(results));
"""
        script_path = TMP_ROOT / "ui_action_matrix.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        results = {item["nextAction"]: item for item in json.loads(completed.stdout.strip())}
        self.assertTrue(results["finalize_inputs"]["finalize"])
        self.assertFalse(results["finalize_inputs"]["generate"])
        self.assertTrue(results["review_draft_spec"]["approveDraft"])
        self.assertFalse(results["review_draft_spec"]["generate"])
        self.assertTrue(results["generate_code"]["generate"])
        self.assertFalse(results["generate_code"]["approveCode"])
        self.assertFalse(results["generate_code"]["runApproved"])
        self.assertTrue(results["revise_approved_spec"]["generate"])
        self.assertEqual(results["revise_approved_spec"]["generateLabel"], "Generate Revised Draft Spec")
        self.assertFalse(results["revise_approved_spec"]["finalize"])
        self.assertTrue(results["graph_generate_without_local_spec_gate"]["generate"])
        self.assertTrue(results["review_code"]["approveCode"])
        self.assertFalse(results["review_code"]["runApproved"])
        self.assertFalse(results["execute_approved_code"]["approveCode"])
        self.assertFalse(results["execute_approved_code"]["runApproved"])

    def test_index_primary_actions_fail_closed_when_progress_lacks_active_target(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_missing_progress_target' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.selectedTarget = 'ADAE';
state.selectedTargetsForPlan = ['ADAE'];
state.plan = {requested_datasets: ['ADAE'], blocked_datasets: []};
state.finalizedInputsByDataset = {ADAE: {input_spec_available: true}};
state.draftSpecByDataset = {ADAE: {dataset: 'ADAE', variables: [{variable: 'AETERM'}]}};
state.generatedByDataset = {ADAE: {dataset: 'ADAE', run_id: 'run_ui_missing_progress_target', status: 'generated', generated_code: 'x <- 1'}};
state.reviewByDataset = {ADAE: {approved: true}};
state.executionByDataset = {ADAE: {status: 'completed'}};
state.runProgress = {datasets: [{dataset: 'ADSL', next_action: 'execute_approved_code', blocked: false}]};
const availability = actionAvailability();
state.runProgress = null;
const legacyFallback = actionAvailability();
console.log(JSON.stringify({
  finalize: availability.finalize,
  approveDraft: availability.approveDraft,
  generate: availability.generate,
  approveCode: availability.approveCode,
  runApproved: availability.runApproved,
  legacyGenerateReady: legacyFallback.generate.ready,
  legacyApproveReady: legacyFallback.approveCode.ready,
  legacyRunReady: legacyFallback.runApproved.ready,
}));
"""
        script_path = TMP_ROOT / "ui_action_progress_missing_target.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        for key in ["finalize", "approveDraft", "generate", "approveCode", "runApproved"]:
            self.assertFalse(result[key]["ready"], key)
            self.assertIn("Graph progress has no dataset step for ADAE", result[key]["reason"])
        self.assertTrue(result["legacyGenerateReady"])
        self.assertTrue(result["legacyApproveReady"])
        self.assertFalse(result["legacyRunReady"])

    def test_index_draft_review_gate_overrides_local_input_spec_shortcuts(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        draft_body = html.split("function renderDraftSpecPane()", 1)[1].split("function renderDraftSpecReviewTable", 1)[0]
        action_body = html.split("function actionAvailability()", 1)[1].split("function renderActionAvailability", 1)[0]
        self.assertIn("const graphRequiresDraftReview = progress?.next_action === 'review_draft_spec';", draft_body)
        self.assertIn("if (graphRequiresDraftReview && draft)", draft_body)
        self.assertLess(draft_body.index("if (graphRequiresDraftReview && draft)"), draft_body.index("if (finalized?.input_spec_available || targetHasInputSpec"))
        self.assertIn("Boolean(target && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && draftGate.ready && draft && !draftReview?.approved)", action_body)
        self.assertNotIn("draftGate.ready && draft && !draftReview?.approved && !finalized?.input_spec_available", action_body)

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

    def test_index_apply_graph_state_prefers_requested_target_over_dependency_target(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_ui_graph_requested_target' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, remove() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
function setPill() {}
function renderPlan() {}
""" + script + r"""
state.targetCandidates = ['ADSL'];
state.selectedTarget = 'ADSL';
applyGraphState({
  study_id: 'PSY201',
  run_id: 'run_ui_graph_requested_target',
  requested_datasets: ['ADAE', 'ADCM'],
  target_datasets: ['ADSL', 'ADAE', 'ADCM'],
  runnable_datasets: ['ADAE', 'ADCM'],
  blocked_datasets: [],
  dependency_review_status: 'accepted',
  datasets: {
    ADAE: {status: 'needs_review', spec_state: {status: 'input_spec_ready'}, code_state: {status: 'generated', code_path: 'runs/x/code/build_adae.R'}},
    ADCM: {status: 'needs_review', spec_state: {status: 'draft_generated', draft_spec_path: 'runs/x/spec/adcm.json'}, code_state: {}}
  }
});
console.log(JSON.stringify({
  selectedTarget: state.selectedTarget,
  selectedTargets: state.selectedTargetsForPlan,
  candidates: state.targetCandidates
}));
"""
        script_path = TMP_ROOT / "ui_graph_requested_target.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["selectedTarget"], "ADAE")
        self.assertEqual(result["selectedTargets"], ["ADAE", "ADCM"])
        self.assertEqual(result["candidates"], ["ADAE", "ADCM", "ADSL"])

    def test_index_keeps_planning_selection_separate_from_active_target_view(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        selected_targets_body = html.split("function selectedTargets()", 1)[1].split("function planSelectionSet()", 1)[0]
        self.assertNotIn("selected.unshift(state.selectedTarget)", selected_targets_body)
        self.assertNotIn("!selected.includes(state.selectedTarget)", selected_targets_body)
        apply_graph_body = html.split("function applyGraphState(graph)", 1)[1].split("function generatedFor(dataset)", 1)[0]
        self.assertIn("const requestedTargets = (graph?.requested_datasets || []).map", apply_graph_body)
        self.assertNotIn("selectedTargetsForPlan = Array.from(new Set([...selectedTargets(), ...graphTargets", apply_graph_body)
        self.assertNotIn("selectedTargetsForPlan = graphTargets", apply_graph_body)
        self.assertIn("function plannedTargetsForDisplay(plan, fallbackTargets = null)", html)
        self.assertIn("function renderTargetSelectionSummary()", html)
        self.assertIn(
            "Start Runnable Datasets moves all runnable targets to review gates; R execution stays per dataset after human approval.",
            html,
        )
        self.assertIn(
            "This is not dependency proof. R will not run.",
            html,
        )
        self.assertIn(
            "Dispatching ${targets.join(', ')} to graph-owned draft/code review gates. This is not dependency proof. R will not run.",
            html,
        )
        self.assertIn("function datasetPlanningContext(target, isPlanned, isActive, status)", html)
        self.assertIn("planned in this run", html)
        self.assertIn("view-only history/candidate", html)
        planned_display_body = html.split("function plannedTargetsForDisplay(plan, fallbackTargets = null)", 1)[1].split("function dependencyPlanSummary(plan)", 1)[0]
        self.assertIn("plan?.requested_datasets", planned_display_body)
        self.assertNotIn("plan.target_datasets", planned_display_body)
        view_handler = html.split("for (const button of node.querySelectorAll('[data-target-view]'))", 1)[1].split("renderDraftSpecPane();", 1)[0]
        self.assertIn("state.selectedTarget = button.dataset.targetView;", view_handler)
        self.assertNotIn("selectedTargetsForPlan", view_handler)
        dataset_card_handler = html.split("for (const card of node.querySelectorAll('[data-card-target]'))", 1)[1].split("function hasReferenceAdamEvidence", 1)[0]
        self.assertIn("state.selectedTarget = card.dataset.cardTarget;", dataset_card_handler)
        self.assertNotIn("preparePlan();", dataset_card_handler)

    def test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        board_body = html.split("function renderDatasetBoard(targets, runnable, blocked)", 1)[1].split("function datasetPlanningContext", 1)[0]
        context_body = html.split("function datasetPlanningContext(target, isPlanned, isActive, status)", 1)[1].split("function renderAgentAuditPanel", 1)[0]
        self.assertIn("const isReferenceOnly = status === 'reference evidence' && !isPlanned;", board_body)
        self.assertIn("codeStageClassFor(target, progress, isActive, isPlanned, isBlocked, isReferenceOnly)", board_body)
        self.assertIn("function codeStageClassFor(target, progress, isActive, isPlanned, isBlocked, isReferenceOnly)", html)
        self.assertIn("!isReferenceOnly ? 'active' : ''", html)
        self.assertIn("datasetPlanningContext(target, isPlanned, isActive, status)", board_body)
        self.assertIn("reference ADaM only: compare/output-shape evidence, not generation input", context_body)
        self.assertNotIn("target === state.selectedTarget && !progress?.blocked ? 'active' : ''", board_body)

    def test_index_dataset_card_stages_prefer_graph_progress_read_model(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_stage_matrix' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
function stages(progress) {
  state.generatedByDataset = {};
  state.reviewByDataset = {};
  state.executionByDataset = {};
  state.reviewSummaryByDataset = {};
  return {
    code: codeStageClassFor('ADAE', progress, true, true, false, false),
    review: reviewStageClassFor('ADAE', progress),
    run: runStageClassFor('ADAE', progress, false),
  };
}
console.log(JSON.stringify({
  reviewCode: stages({next_action: 'review_code', code_status: 'generated'}),
  executeApproved: stages({next_action: 'execute_approved_code', code_status: 'approved'}),
  terminalFailure: stages({next_action: 'review_terminal_failure', execution_status: 'terminal_failure'}),
  completed: stages({next_action: 'complete', execution_status: 'completed'}),
  localCacheWithoutProgress: (() => {
    state.generatedByDataset = {ADAE: {dataset: 'ADAE', generated_code: 'x <- 1'}};
    state.reviewByDataset = {ADAE: {approved: true}};
    state.executionByDataset = {ADAE: {status: 'completed'}};
    state.reviewSummaryByDataset = {ADAE: {output_preview: [{USUBJID: '01'}]}};
    return {
      code: codeStageClassFor('ADAE', {}, true, true, false, false),
      review: reviewStageClassFor('ADAE', {}),
      run: runStageClassFor('ADAE', {}, false),
    };
  })(),
  referenceOnly: codeStageClassFor('ADAE', null, true, true, false, true)
}));
"""
        script_path = TMP_ROOT / "ui_dataset_stage_matrix.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["reviewCode"], {"code": "done", "review": "active", "run": ""})
        self.assertEqual(result["executeApproved"], {"code": "done", "review": "done", "run": "active"})
        self.assertEqual(result["terminalFailure"], {"code": "done", "review": "done", "run": "blocked"})
        self.assertEqual(result["completed"], {"code": "done", "review": "done", "run": "done"})
        self.assertEqual(result["localCacheWithoutProgress"], {"code": "active", "review": "", "run": ""})
        self.assertEqual(result["referenceOnly"], "")

    def test_index_dataset_status_ignores_local_completion_cache_when_progress_loaded(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_status_progress' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.plan = {runnable_datasets: ['ADAE'], blocked_datasets: []};
state.generatedByDataset = {ADAE: {dataset: 'ADAE', generated_code: 'x <- 1'}};
state.executionByDataset = {ADAE: {status: 'completed'}};
state.reviewSummaryByDataset = {ADAE: {status: 'completed', output_preview: [{USUBJID: '01'}]}};
state.runReview = {
  dataset_reviews: [{
    dataset: 'ADAE',
    status: 'completed',
    output_quality: {quality_status: 'not_real_derivation'},
    output_preview: [{USUBJID: '01'}]
  }]
};
state.runProgress = {datasets: [{dataset: 'ADSL', status: 'completed'}]};
const withProgress = datasetStatus('ADAE', ['ADAE'], []);
const qualityWithProgress = datasetOutputQualityStatus('ADAE');
state.runProgress = null;
const legacyFallback = datasetStatus('ADAE', ['ADAE'], []);
const qualityLegacyFallback = datasetOutputQualityStatus('ADAE');
console.log(JSON.stringify({withProgress, qualityWithProgress, legacyFallback, qualityLegacyFallback}));
"""
        script_path = TMP_ROOT / "ui_dataset_status_progress.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertEqual(result["withProgress"], "ready")
        self.assertEqual(result["qualityWithProgress"], "")
        self.assertEqual(result["legacyFallback"], "review only")
        self.assertEqual(result["qualityLegacyFallback"], "not_real_derivation")

    def test_index_next_action_text_ignores_local_cache_when_progress_loaded(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        script = response.text.split("<script>", 1)[1].split("</script>", 1)[0]
        harness = r"""
const nodes = new Map();
global.window = { location: { href: '' } };
global.document = {
  getElementById(id) {
    if (!nodes.has(id)) {
      nodes.set(id, {
        value: id === 'runId' ? 'run_next_action_progress' : '',
        textContent: '',
        innerHTML: '',
        className: '',
        dataset: {},
        classList: { add() {}, toggle() {} },
        addEventListener() {},
        querySelectorAll() { return []; },
        setAttribute() {},
      });
    }
    return nodes.get(id);
  },
  querySelectorAll() { return []; },
};
global.fetch = async () => ({ ok: true, json: async () => ({}) });
""" + script + r"""
state.plan = {runnable_datasets: ['ADAE'], blocked_datasets: []};
state.generatedByDataset = {ADAE: {dataset: 'ADAE', generated_code: 'x <- 1'}};
state.reviewByDataset = {ADAE: {approved: true}};
state.executionByDataset = {ADAE: {status: 'completed'}};
state.finalizedInputsByDataset = {ADAE: {input_spec_available: true}};
state.runProgress = {datasets: [{dataset: 'ADSL', status: 'completed'}]};
const withProgress = nextActionText('ADAE', 'ready', false);
state.runProgress = null;
const legacyFallback = nextActionText('ADAE', 'completed', false);
console.log(JSON.stringify({withProgress, legacyFallback}));
"""
        script_path = TMP_ROOT / "ui_next_action_progress.js"
        TMP_ROOT.mkdir(exist_ok=True)
        script_path.write_text(harness, encoding="utf-8")
        completed = subprocess.run(
            ["node", str(script_path)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        result = json.loads(completed.stdout.strip())
        self.assertIn("graph progress has no dataset step", result["withProgress"].lower())
        self.assertIn("inspect the generated ADaM table", result["legacyFallback"])

    def test_index_marks_review_only_outputs_without_runtime_language(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        runtime_body = html.split("function dependencyRuntimeSummary(target, status, isBlocked)", 1)[1].split("function dependencyFlowRowHtml", 1)[0]
        action_body = html.split("function nextActionText(target, status, isBlocked)", 1)[1].split("function renderDatasetBoard", 1)[0]
        board_body = html.split("function renderDatasetBoard(targets, runnable, blocked)", 1)[1].split("function datasetPlanningContext", 1)[0]
        status_body = html.split("function datasetStatus(target, runnable, blocked)", 1)[1].split("async function generateCode", 1)[0]
        self.assertIn("function datasetOutputQualityStatus(target)", html)
        self.assertIn("const quality = datasetOutputQualityStatus(target);", runtime_body)
        self.assertIn("has a structural demo output for review only. It cannot satisfy downstream runtime dependencies.", runtime_body)
        self.assertIn("has a mock/offline output for review only. It cannot satisfy downstream runtime dependencies.", runtime_body)
        self.assertIn("has a completed local R runtime output for review.", runtime_body)
        self.assertIn(
            "review must still confirm that the dependency assumption is correct",
            runtime_body,
        )
        self.assertIn("const completedExecution = executionFor(target)?.status === 'completed' || datasetProgressFor(target)?.execution_status === 'completed';", action_body)
        self.assertIn("inspect this review-only/demo output. It cannot be used as runtime input for another dataset.", action_body)
        self.assertIn("const reviewOnlyOutput = ['structural_stub', 'not_real_derivation'].includes(qualityStatus);", board_body)
        self.assertIn("runStageClassFor(target, progress, reviewOnlyOutput)", board_body)
        self.assertIn("function runStageClassFor(target, progress, reviewOnlyOutput)", html)
        self.assertIn("if (reviewOnlyOutput) return 'review-only';", html)
        self.assertIn(".stage.review-only", html)
        self.assertIn("datasetOutputQualityStatus(target)", status_body)

    def test_index_allows_initial_compare_when_reference_preview_exists(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        compare_body = html.split("function comparePane(review)", 1)[1].split("function downloadsPane(review)", 1)[0]
        self.assertIn("const canRunCompare = Boolean(review?.output_preview && review?.reference_preview);", compare_body)
        self.assertIn("const compareButtonLabel = compare ? 'Run Compare Again' : 'Run Compare';", compare_body)
        self.assertIn("Compare is not available yet. A generated table and a reference ADaM table are both required.", compare_body)
        self.assertIn("Compare has not been run yet. Reference ADaM is used only as comparison evidence.", compare_body)
        self.assertIn('<button class="secondary" id="refreshCompareButton">Run Compare</button>', compare_body)
        self.assertIn("${escapeHtml(compareButtonLabel)}", compare_body)
        self.assertIn("const compareButton = byId('refreshCompareButton');", html)

    def test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency(self) -> None:
        client = TestClient(create_app())

        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        html = response.text
        dependency_body = html.split("function dependencyFlowRowHtml(dependency, runnable, targets)", 1)[1].split("function dependencyDecisionFor(target)", 1)[0]
        runtime_body = html.split("function dependencyRuntimeAvailable(dependency, runnable, targets)", 1)[1].split("function dependencyEvidenceText(dependency, runnable, targets)", 1)[0]
        render_body = html.split("function renderDependencyGraph(targets, runnable, blocked)", 1)[1].split("function dependencySourceEvidenceText", 1)[0]
        self.assertIn("dependency-plain", render_body)
        self.assertIn("dependency-plain-card", render_body)
        self.assertIn("What it means", render_body)
        self.assertIn("Why", render_body)
        self.assertIn("Evidence", render_body)
        self.assertIn("Next action", render_body)
        self.assertIn("trust-boundary", render_body)
        self.assertIn("function dependencySourceEvidenceText(sdtm)", html)
        self.assertIn("function dependencyDecisionSummary(target, decision, dependencies)", html)
        self.assertIn("function dependencyRuntimeSummary(target, status, isBlocked)", html)
        self.assertIn("has no upstream ADaM dependency evidence in the current uploaded materials; this must be confirmed in spec/code review.", html)
        self.assertIn("No dependency evidence is not the same as clinical proof.", html)
        self.assertIn("this must be confirmed in spec/code review", html)
        self.assertIn("Reference ADaM is comparison/output-shape evidence only", dependency_body)
        self.assertIn("not derivation authority or a runtime dependency by itself", dependency_body)
        self.assertIn("Runtime input is available or planned", dependency_body)
        self.assertIn("User action is needed before this target can generate", dependency_body)
        self.assertNotIn("hasReferenceAdamEvidence(dependency)", runtime_body)
        self.assertIn("Reference ADaM is used only to preview shape and compare final output.", html)
        self.assertIn("Reference ADaM supports comparison/output-shape review only", html)
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

    def test_progress_endpoint_reports_graph_owned_next_actions(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_progress_endpoint")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_progress_endpoint/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)

        progress = client.get(
            "/runs/run_progress_endpoint/progress",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(progress.status_code, 200, progress.text)
        payload = progress.json()
        self.assertEqual(payload["status"], "needs_review")
        self.assertEqual(payload["current_interrupt"]["name"], "code_review")
        self.assertEqual(payload["next_action"], "review_code")
        self.assertEqual(payload["requested_datasets"], ["ADAE"])
        self.assertEqual(payload["target_datasets"], ["ADAE"])
        self.assertNotIn("ADSL", payload["target_datasets"])
        self.assertTrue(payload["graph_state_path"].endswith("graph_state.json"))
        self.assertEqual(payload["native_resume"]["available"], False)
        self.assertEqual(payload["native_resume"]["scope"], "none")
        self.assertEqual(payload["native_resume"]["boundary"], "graph_state_projection_only")
        self.assertEqual(payload["native_resume"]["runtime_binding_status"], "run_not_durable")
        self.assertEqual(payload["native_resume"]["resume_unavailable_reason"], "run_not_durable")
        self.assertEqual(
            payload["native_resume"]["explicit_resume_endpoint"],
            "POST /runs/{run_id}/datasets/{dataset}/native-resume",
        )
        self.assertNotIn("endpoint", payload["native_resume"])
        self.assertEqual(payload["native_resume"]["default_review_path"], "split_flow_review_endpoints")
        self.assertIn("not enabled", payload["native_resume"]["message"])
        by_dataset = {item["dataset"]: item for item in payload["datasets"]}
        self.assertEqual(by_dataset["ADAE"]["next_action"], "review_code")
        self.assertEqual(by_dataset["ADAE"]["action_label"], "Review generated R code.")
        self.assertFalse(by_dataset["ADAE"]["blocked"])
        self.assertEqual(by_dataset["ADAE"]["code_status"], "generated")

    def test_progress_endpoint_reports_missing_workflow_projection_as_null(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_progress_graph_state_only")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_progress_graph_state_only/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        workflow_path = study_dir / "runs" / "run_progress_graph_state_only" / "workflow_state.json"
        self.assertTrue(workflow_path.exists())
        workflow_path.unlink()

        progress = client.get(
            "/runs/run_progress_graph_state_only/progress",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(progress.status_code, 200, progress.text)
        payload = progress.json()
        self.assertTrue(payload["graph_state_path"].endswith("graph_state.json"))
        self.assertIsNone(payload["workflow_state_path"])

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
        self.assertEqual(payload["target_datasets"], ["ADSL", "ADAE"])
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
        detail = run_response.json()["detail"]
        self.assertIn("would bypass review gates", detail)
        self.assertIn("/runs/native-study-loop", detail)
        self.assertIn("/graph-command", detail)
        self.assertIn("native-full-run/execute", detail)
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

    def test_create_run_requires_explicit_execution_mode_instead_of_implicit_stub(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_api_no_implicit_stub")
        client = TestClient(create_app())

        response = client.post(
            "/runs",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_no_implicit_stub",
                "target_datasets": ["ADAE"],
            },
        )

        self.assertEqual(response.status_code, 400)
        detail = response.json()["detail"]
        self.assertIn("requires an explicit execution_mode", detail)
        self.assertIn("execution_mode='stub'", detail)
        self.assertIn("/runs/prepare", detail)
        self.assertIn("/runs/native-study-loop", detail)
        self.assertFalse((study_dir / "runs" / "run_no_implicit_stub" / "workflow_state.json").exists())

    def test_create_run_rejects_unknown_execution_mode_before_legacy_graph(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_api_unknown_execution_mode")
        client = TestClient(create_app())

        response = client.post(
            "/runs",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_unknown_execution_mode",
                "target_datasets": ["ADAE"],
                "execution_mode": "legacy_auto_magic",
            },
        )

        self.assertEqual(response.status_code, 400)
        detail = response.json()["detail"]
        self.assertIn("Unsupported execution_mode for POST /runs", detail)
        self.assertIn("legacy_auto_magic", detail)
        self.assertIn("Allowed legacy endpoint modes", detail)
        self.assertIn("/runs/prepare", detail)
        self.assertIn("/runs/native-study-loop", detail)
        self.assertFalse((study_dir / "runs" / "run_unknown_execution_mode" / "workflow_state.json").exists())

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

    def test_compare_fails_closed_when_existing_graph_state_is_corrupt(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_compare_corrupt_graph_state")
        client = TestClient(create_app())
        run_dir = study_dir / "runs" / "run_compare_corrupt_graph_state"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir()
        (output_dir / "adae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (validation_dir / "adae_validation_report.json").write_text(
            json.dumps({"dataset": "ADAE", "status": "pass"}),
            encoding="utf-8",
        )
        (run_dir / "workflow_state.json").write_text(
            json.dumps(
                {
                    "study_id": "MY_STUDY",
                    "run_id": "run_compare_corrupt_graph_state",
                    "status": "completed",
                    "datasets": {"ADAE": {"dataset": "ADAE", "status": "completed"}},
                }
            ),
            encoding="utf-8",
        )
        (run_dir / "graph_state.json").write_text("{not-json", encoding="utf-8")

        compare = client.get(
            "/runs/run_compare_corrupt_graph_state/datasets/ADAE/compare",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(compare.status_code, 400, compare.text)
        detail = compare.json()["detail"]
        self.assertIn("Canonical graph state", detail)
        self.assertIn("Compare will not fall back", detail)
        self.assertFalse((run_dir / "compare" / "adae_compare_report.json").exists())

    def test_compare_endpoint_supports_sas7bdat_reference_when_graph_state_exists(self) -> None:
        study_dir = _workspace_dir("phase8_compare_sas7bdat_reference") / "PSY201"
        run_id = "run_compare_sas7bdat_reference"
        output_dir = study_dir / "runs" / run_id / "outputs"
        reference_dir = study_dir / "reference_adam"
        output_dir.mkdir(parents=True)
        reference_dir.mkdir(parents=True)
        (output_dir / "addm.csv").write_text("USUBJID,SITEID\n01,01\n02,01\n", encoding="utf-8")
        (reference_dir / "addm.sas7bdat").write_text("binary placeholder", encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id=run_id,
            target_datasets=["ADDM"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id).model_copy(deep=True)
        state.datasets["ADDM"].status = "completed"
        state.datasets["ADDM"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": str((output_dir / "addm.csv").as_posix()),
        }
        gateway._persist_graph_state(study_dir, state, node="test_seed_api_compare_sas7bdat_reference")
        client = TestClient(create_app())

        def fake_reader(path: Path) -> dict:
            self.assertTrue(path.name.lower().endswith(".sas7bdat"))
            return {
                "status": "ok",
                "columns": ["usubjid", "siteid"],
                "rows": [{"usubjid": "01", "siteid": "01"}],
            }

        with patch("adam_agent.api.service._sas7bdat_table_reader", return_value=fake_reader):
            compare = client.get(
                f"/runs/{run_id}/datasets/ADDM/compare",
                params={"study_dir": str(study_dir)},
            )

        self.assertEqual(compare.status_code, 200, compare.text)
        payload = compare.json()
        self.assertEqual(payload["status"], "differences")
        self.assertEqual(payload["reference_file"], "addm.sas7bdat")
        self.assertEqual(payload["row_count_generated"], 2)
        self.assertEqual(payload["row_count_reference"], 1)
        self.assertEqual(payload["generated_only_keys"], ["02"])

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
        graph_state = GraphGateway().load_graph_state(study_dir=study_dir, run_id="run_compare_missing_refresh").model_copy(
            deep=True
        )
        graph_state.datasets["ADAE"].status = "completed"
        graph_state.datasets["ADAE"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": str((output_dir / "adae.csv").as_posix()),
        }
        GraphGateway()._persist_graph_state(study_dir, graph_state, node="test_seed_compare_output_path")
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

        revised_draft = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/draft-spec",
            json={
                "study_dir": str(study_dir),
                "llm_provider_override": {"provider": "mock", "model": "mock-model"},
                "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
            },
        )
        self.assertEqual(revised_draft.status_code, 200, revised_draft.text)
        self.assertEqual(revised_draft.json()["status"], "draft_spec_generated")
        draft_review = client.post(
            "/runs/run_terminal_revise_spec_gate/datasets/ADAE/draft-spec-review",
            json={"study_dir": str(study_dir), "decision": "approve", "reviewer": "tester"},
        )
        self.assertEqual(draft_review.status_code, 200, draft_review.text)
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
        self.assertEqual(adae_state["execution_state"]["terminal_failure_followup_consumed_by"], "draft_spec")

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
        dataset_review = review.json()["dataset_reviews"][0]
        self.assertIn("0 dataset(s) have real runtime output", review.json()["plain_summary"])
        self.assertIn("1 failed", review.json()["plain_summary"])
        self.assertIn("1 terminal failure output(s) hidden", review.json()["plain_summary"])
        self.assertIsNone(dataset_review["output_preview"])
        self.assertIsNone(dataset_review["output_path"])
        self.assertEqual(dataset_review["output_quality"]["quality_status"], "terminal_failure")
        self.assertFalse(dataset_review["output_quality"]["runtime_dependency_eligible"])

    def test_graph_state_blocks_generated_table_download_without_output_artifact(self) -> None:
        study_dir = _workspace_dir("phase8_graph_blocks_unbacked_generated_output") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_unbacked_output"
        outputs_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        outputs_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (outputs_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_unbacked_output",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_unbacked_output").model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "terminal_failure"
        state.datasets["ADSL"].status = "terminal_failure"
        state.datasets["ADSL"].current_interrupt = None
        state.datasets["ADSL"].execution_state = {
            "status": "terminal_failure",
            "terminal_failure": True,
            "partial_output_usable": False,
        }
        state.datasets["ADSL"].validation_summary = {"status": "fail", "terminal_failure": True}
        gateway._persist_graph_state(study_dir, state, node="test_seed_unbacked_generated_output")
        client = TestClient(create_app())

        table = client.get(
            "/runs/run_graph_unbacked_output/datasets/ADSL/table",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        download = client.get(
            "/runs/run_graph_unbacked_output/datasets/ADSL/download",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        review = client.get(
            "/runs/run_graph_unbacked_output/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(table.status_code, 200, table.text)
        self.assertEqual(table.json()["status"], "missing")
        self.assertEqual(download.status_code, 404)
        self.assertEqual(review.status_code, 200, review.text)
        dataset_review = review.json()["dataset_reviews"][0]
        self.assertIsNone(dataset_review["output_path"])
        self.assertIsNone(dataset_review["output_preview"])
        generated_download = next(item for item in dataset_review["downloads"] if item["kind"] == "generated")
        self.assertFalse(generated_download["available"])
        self.assertIn("terminal failure output", review.json()["plain_summary"])

    def test_graph_state_generated_artifact_controls_table_download_path(self) -> None:
        study_dir = _workspace_dir("phase8_graph_generated_artifact_path") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_generated_artifact"
        outputs_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        outputs_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (outputs_dir / "adsl.csv").write_text("USUBJID,TRTSDT\nSTALE,1999-01-01\n", encoding="utf-8")
        graph_output = outputs_dir / "adsl_graph.csv"
        graph_output.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_generated_artifact",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_generated_artifact").model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "completed"
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].current_interrupt = None
        state.datasets["ADSL"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": str(graph_output.as_posix()),
        }
        state.datasets["ADSL"].validation_summary = {"status": "pass"}
        gateway._persist_graph_state(study_dir, state, node="test_seed_generated_artifact_path")
        client = TestClient(create_app())

        table = client.get(
            "/runs/run_graph_generated_artifact/datasets/ADSL/table",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        download = client.get(
            "/runs/run_graph_generated_artifact/datasets/ADSL/download",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )

        self.assertEqual(table.status_code, 200, table.text)
        self.assertEqual(table.json()["status"], "ok")
        self.assertEqual(table.json()["file_name"], "adsl_graph.csv")
        self.assertEqual(table.json()["rows"][0]["USUBJID"], "01")
        self.assertEqual(download.status_code, 200, download.text)
        self.assertIn("2024-01-01", download.text)
        self.assertNotIn("1999-01-01", download.text)

    def test_graph_state_blocks_generated_reads_for_dataset_absent_from_canonical_state(self) -> None:
        study_dir = _workspace_dir("phase8_graph_absent_dataset_blocks_stale_output") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_absent_dataset"
        code_dir = run_dir / "code"
        outputs_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        compare_dir = run_dir / "compare"
        code_dir.mkdir(parents=True)
        outputs_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        compare_dir.mkdir(parents=True)
        (code_dir / "build_adlb.R").write_text("adlb <- data.frame(USUBJID = '01')\n", encoding="utf-8")
        (outputs_dir / "adlb.csv").write_text("USUBJID,PARAMCD\n01,ALT\n", encoding="utf-8")
        (validation_dir / "adlb_validation_report.json").write_text(
            json.dumps({"dataset": "ADLB", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        (compare_dir / "adlb_compare_report.json").write_text(
            json.dumps({"dataset": "ADLB", "status": "match"}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_absent_dataset",
            target_datasets=["ADSL"],
        )
        client = TestClient(create_app())

        table = client.get(
            "/runs/run_graph_absent_dataset/datasets/ADLB/table",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        download = client.get(
            "/runs/run_graph_absent_dataset/datasets/ADLB/download",
            params={"study_dir": str(study_dir), "kind": "generated"},
        )
        code_download = client.get(
            "/runs/run_graph_absent_dataset/datasets/ADLB/download",
            params={"study_dir": str(study_dir), "kind": "code"},
        )
        validation_download = client.get(
            "/runs/run_graph_absent_dataset/datasets/ADLB/download",
            params={"study_dir": str(study_dir), "kind": "validation_report"},
        )
        compare_download = client.get(
            "/runs/run_graph_absent_dataset/datasets/ADLB/download",
            params={"study_dir": str(study_dir), "kind": "compare_report"},
        )
        validation_artifact = client.get(
            "/runs/run_graph_absent_dataset/datasets/ADLB/validation",
            params={"study_dir": str(study_dir)},
        )
        artifact_read = client.post(
            "/runs/run_graph_absent_dataset/artifacts/read",
            params={"study_dir": str(study_dir)},
            json={"relative_path": "validation/adlb_validation_report.json"},
        )
        artifact_read_with_dotdot = client.post(
            "/runs/run_graph_absent_dataset/artifacts/read",
            params={"study_dir": str(study_dir)},
            json={"relative_path": "validation/../validation/adlb_validation_report.json"},
        )
        artifact_read_with_backslashes = client.post(
            "/runs/run_graph_absent_dataset/artifacts/read",
            params={"study_dir": str(study_dir)},
            json={"relative_path": r"validation\..\validation\adlb_validation_report.json"},
        )
        review = client.get(
            "/runs/run_graph_absent_dataset/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(table.status_code, 200, table.text)
        self.assertEqual(table.json()["status"], "missing")
        self.assertEqual(download.status_code, 404)
        self.assertEqual(code_download.status_code, 404)
        self.assertEqual(validation_download.status_code, 404)
        self.assertEqual(compare_download.status_code, 404)
        self.assertEqual(validation_artifact.status_code, 404)
        self.assertEqual(artifact_read.status_code, 404)
        self.assertEqual(artifact_read_with_dotdot.status_code, 404)
        self.assertEqual(artifact_read_with_backslashes.status_code, 404)
        self.assertEqual(review.status_code, 200, review.text)
        self.assertNotIn("ADLB", [item["dataset"] for item in review.json()["dataset_reviews"]])
        self.assertNotIn("validation_adlb", review.json()["advanced_artifacts"])

    def test_graph_state_allows_recorded_validation_artifact_read(self) -> None:
        study_dir = _workspace_dir("phase8_graph_recorded_validation_read") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_recorded_validation"
        validation_dir = run_dir / "validation"
        validation_dir.mkdir(parents=True)
        validation_path = validation_dir / "adsl_validation_report.json"
        validation_path.write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "warnings": ["canonical validation"]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_recorded_validation",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_recorded_validation").model_copy(deep=True)
        state.datasets["ADSL"].validation_summary = {"status": "pass", "warnings": ["canonical validation"]}
        state.datasets["ADSL"].execution_state["validation_report_path"] = str(validation_path.as_posix())
        gateway._persist_graph_state(study_dir, state, node="test_seed_recorded_validation_read")
        client = TestClient(create_app())

        validation_artifact = client.get(
            "/runs/run_graph_recorded_validation/datasets/ADSL/validation",
            params={"study_dir": str(study_dir)},
        )
        download = client.get(
            "/runs/run_graph_recorded_validation/datasets/ADSL/download",
            params={"study_dir": str(study_dir), "kind": "validation_report"},
        )

        self.assertEqual(validation_artifact.status_code, 200, validation_artifact.text)
        self.assertEqual(validation_artifact.json()["status"], "pass")
        self.assertEqual(download.status_code, 200, download.text)
        self.assertIn("canonical validation", download.text)

    def test_graph_state_advanced_artifacts_ignore_unrecorded_run_files(self) -> None:
        study_dir = _workspace_dir("phase8_graph_advanced_artifact_guard") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_advanced_artifact_guard"
        planning_dir = run_dir / "planning"
        planning_dir.mkdir(parents=True)
        stale_plan = planning_dir / "dependency_plan.json"
        stale_plan.write_text(json.dumps({"stale": True}), encoding="utf-8")
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_advanced_artifact_guard",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_advanced_artifact_guard").model_copy(deep=True)
        state.artifacts = [
            artifact
            for artifact in state.artifacts
            if not bool(artifact.metadata.get("planning_artifact"))
        ]
        (run_dir / "graph_state.json").write_text(state.model_dump_json(indent=2), encoding="utf-8")
        client = TestClient(create_app())

        review = client.get(
            "/runs/run_graph_advanced_artifact_guard/review-summary",
            params={"study_dir": str(study_dir)},
        )
        artifact_read = client.post(
            "/runs/run_graph_advanced_artifact_guard/artifacts/read",
            params={"study_dir": str(study_dir)},
            json={"relative_path": "planning/dependency_plan.json"},
        )

        self.assertEqual(review.status_code, 200, review.text)
        self.assertFalse(
            any("dependency_plan" in key for key in review.json()["advanced_artifacts"]),
            review.json()["advanced_artifacts"],
        )
        self.assertEqual(artifact_read.status_code, 404)

    def test_graph_state_records_dependency_plan_as_run_artifact(self) -> None:
        study_dir = _workspace_dir("phase8_graph_recorded_plan_artifact") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_recorded_plan_artifact",
            target_datasets=["ADSL"],
        )
        client = TestClient(create_app())

        review = client.get(
            "/runs/run_graph_recorded_plan_artifact/review-summary",
            params={"study_dir": str(study_dir)},
        )
        artifact_read = client.post(
            "/runs/run_graph_recorded_plan_artifact/artifacts/read",
            params={"study_dir": str(study_dir)},
            json={"relative_path": "planning/dependency_plan.json"},
        )

        self.assertEqual(review.status_code, 200, review.text)
        advanced = review.json()["advanced_artifacts"]
        self.assertTrue(any("dependency_plan" in key for key in advanced), advanced)
        self.assertEqual(artifact_read.status_code, 200, artifact_read.text)
        self.assertEqual(artifact_read.json()["study_id"], "MY_STUDY")
        self.assertEqual(artifact_read.json()["run_id"], "run_graph_recorded_plan_artifact")

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

    def test_native_study_loop_endpoint_starts_multiple_runnable_datasets(self) -> None:
        study_dir = _study_with_adae_adcm_inputs("phase8_native_study_loop_endpoint")
        client = TestClient(create_app())

        response = client.post(
            "/runs/native-study-loop",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_native_study_loop_endpoint",
                "target_datasets": ["ADAE", "ADCM"],
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["started_datasets"], ["ADAE", "ADCM"])
        self.assertEqual({item["dataset"] for item in payload["dataset_results"]}, {"ADAE", "ADCM"})
        self.assertTrue(all(item["next_action"] == "review_code" for item in payload["dataset_results"]))
        self.assertFalse(payload["native_resume_available"])
        self.assertEqual(payload["native_resume_scope"], "none")
        self.assertEqual(payload["resume_boundary"], "graph_state_projection_only")
        self.assertTrue(payload["native_resume_has_queue_items"])
        self.assertEqual(payload["native_resume_queue_item_count"], 2)
        self.assertEqual(
            [(item["dataset"], item["interrupt"], item["can_resume"]) for item in payload["native_resume_interrupts"]],
            [("ADAE", "code_review", False), ("ADCM", "code_review", False)],
        )
        self.assertTrue((study_dir / "runs" / "run_native_study_loop_endpoint" / "graph_state.json").exists())
        progress = client.get(
            "/runs/run_native_study_loop_endpoint/progress",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(progress.status_code, 200, progress.text)
        progress_payload = progress.json()
        review_queue = {(item["dataset"], item["name"]) for item in progress_payload["review_queue"]}
        self.assertIn(("ADAE", "code_review"), review_queue)
        self.assertIn(("ADCM", "code_review"), review_queue)
        self.assertEqual(progress_payload["study_loop_result"]["source"], "graph_progress")
        self.assertEqual(progress_payload["study_loop_result"]["started_datasets"], ["ADAE", "ADCM"])
        self.assertFalse(progress_payload["study_loop_result"]["native_resume_available"])
        self.assertEqual(progress_payload["study_loop_result"]["native_resume_scope"], "none")
        self.assertEqual(progress_payload["study_loop_result"]["resume_boundary"], "graph_state_projection_only")
        self.assertEqual(payload["native_resume_available"], progress_payload["study_loop_result"]["native_resume_available"])
        self.assertEqual(payload["native_resume_scope"], progress_payload["study_loop_result"]["native_resume_scope"])
        self.assertEqual(payload["resume_boundary"], progress_payload["study_loop_result"]["resume_boundary"])
        self.assertEqual(payload["native_resume_has_queue_items"], progress_payload["study_loop_result"]["native_resume_has_queue_items"])
        self.assertEqual(payload["native_resume_queue_item_count"], progress_payload["study_loop_result"]["native_resume_queue_item_count"])
        self.assertTrue(progress_payload["native_resume"]["has_queue_items"])
        self.assertEqual(progress_payload["native_resume"]["queue_item_count"], 2)
        self.assertTrue(progress_payload["study_loop_result"]["native_resume_has_queue_items"])
        self.assertEqual(progress_payload["study_loop_result"]["native_resume_queue_item_count"], 2)
        self.assertEqual(
            [(item["dataset"], item["interrupt"], item["can_resume"]) for item in progress_payload["native_resume"]["interrupt_queue"]],
            [("ADAE", "code_review", False), ("ADCM", "code_review", False)],
        )
        self.assertEqual(
            [(item["dataset"], item["interrupt"], item["can_resume"]) for item in progress_payload["study_loop_result"]["native_resume_interrupts"]],
            [("ADAE", "code_review", False), ("ADCM", "code_review", False)],
        )
        loop_review_queue = {(item["dataset"], item["name"]) for item in progress_payload["study_loop_result"]["review_queue"]}
        self.assertIn(("ADAE", "code_review"), loop_review_queue)
        self.assertIn(("ADCM", "code_review"), loop_review_queue)

    def test_native_study_loop_endpoint_preserves_full_run_draft_warnings(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_native_study_loop_draft_warnings")
        client = TestClient(create_app())

        response = client.post(
            "/runs/native-study-loop",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_native_study_loop_draft_warnings",
                "target_datasets": ["ADAE"],
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["started_datasets"], ["ADAE"])
        self.assertEqual(len(payload["dataset_results"]), 1)
        result = payload["dataset_results"][0]
        self.assertEqual(result["dataset"], "ADAE")
        self.assertEqual(result["next_action"], "review_draft_spec")
        self.assertEqual(result["result_type"], "draft_spec_review")
        self.assertTrue(result["draft_spec_path"].endswith("runs/run_native_study_loop_draft_warnings/specs/adae_draft_spec.json"))
        self.assertTrue(
            any("No approved input_spec" in warning for warning in result["warnings"]),
            result["warnings"],
        )

    def test_progress_endpoint_reports_waiting_runtime_dependencies_after_dependency_approval(self) -> None:
        study_dir = _study_with_legacy_adam_dependency("phase8_progress_waiting_runtime_dependency")
        client = TestClient(create_app())
        request = {
            "study_dir": str(study_dir),
            "run_id": "run_progress_waiting_runtime_dependency",
            "target_datasets": ["ADDM", "ADAE"],
            "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
        }

        first = client.post("/runs/native-study-loop", json=request)
        self.assertEqual(first.status_code, 200, first.text)
        self.assertEqual(first.json()["review_queue"][0]["name"], "dependency_review")

        approved = client.post(
            "/runs/run_progress_waiting_runtime_dependency/graph-command",
            json={
                "study_dir": str(study_dir),
                "interrupt": "dependency_review",
                "action": "approve",
                "reviewer": "tester",
                "notes": "Approve the dependency evidence for this run.",
            },
        )
        self.assertEqual(approved.status_code, 200, approved.text)
        self.assertIsNone(approved.json()["current_interrupt"])

        second = client.post("/runs/native-study-loop", json=request)
        self.assertEqual(second.status_code, 200, second.text)
        second_payload = second.json()
        self.assertEqual(second_payload["started_datasets"], ["ADDM"])
        self.assertEqual(second_payload["skipped_datasets"][0]["dataset"], "ADAE")
        self.assertEqual(second_payload["skipped_datasets"][0]["reason"], "waiting_for_runtime_dependency_output")

        progress = client.get(
            "/runs/run_progress_waiting_runtime_dependency/progress",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(progress.status_code, 200, progress.text)
        payload = progress.json()
        self.assertEqual(payload["dependency_review_status"], "approved")
        by_dataset = {item["dataset"]: item for item in payload["datasets"]}
        self.assertEqual(by_dataset["ADDM"]["next_action"], "review_draft_spec")
        self.assertEqual(by_dataset["ADAE"]["next_action"], "complete_dependency_output")
        self.assertEqual(by_dataset["ADAE"]["waiting_for_runtime_dependencies"], ["ADDM"])
        self.assertNotIn(
            ("", "dependency_review"),
            {(item["dataset"], item["name"]) for item in payload["review_queue"]},
        )

    def test_native_full_run_endpoint_starts_single_dataset_at_code_review(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_native_full_run_endpoint")
        client = TestClient(create_app())

        response = client.post(
            "/runs/run_native_full_run_endpoint/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["dataset"], "ADAE")
        self.assertEqual(payload["phase"], "waiting_for_human_gate")
        self.assertEqual(payload["status"], "needs_review")
        self.assertEqual(payload["current_interrupt"]["name"], "code_review")
        self.assertEqual(payload["next_action"], "code_review")
        self.assertTrue(payload["code_path"].endswith("runs/run_native_full_run_endpoint/code/build_adae.R"))
        self.assertTrue(payload["static_check_path"].endswith("runs/run_native_full_run_endpoint/static_checks/adae_static_check.json"))
        self.assertIsNone(payload["draft_spec_path"])
        graph_state, workflow_state = _assert_compatibility_projection(self, payload)
        contract = graph_state["runtime_persistence"]["native_dataset_full_run"]
        self.assertEqual(contract["contract"], "single_dataset_spec_code_review_execute")
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["dataset"], "ADAE")
        self.assertEqual(contract["phase"], "waiting_for_human_gate")
        self.assertEqual(contract["current_interrupt"], "code_review")
        self.assertEqual(contract["next_action"], "code_review")
        self.assertEqual(contract["resume_mode"], "graph_state_full_run_compatibility")
        self.assertTrue(contract["compatibility_resume_available"])
        self.assertTrue(contract["compatibility_resume_currently_available"])
        self.assertEqual(contract["compatibility_resume_boundary"], "current_graph_state_review_gate")
        self.assertEqual(contract["compatibility_resume_supported_interrupts"], ["draft_spec_review", "code_review"])
        self.assertTrue(contract["graph_state_resume_available"])
        self.assertEqual(contract["resume_endpoint"], "POST /runs/{run_id}/datasets/{dataset}/native-full-run/resume")
        self.assertEqual(contract["native_resume_endpoint"], "POST /runs/{run_id}/datasets/{dataset}/native-resume")
        self.assertFalse(contract["durable_resume_available"])
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertEqual(contract["durable_full_run_resume_boundary"], "not_implemented")
        self.assertFalse(contract["durable_native_interrupt_resume_available"])
        self.assertFalse(contract["durable_native_interrupt_checkpointer_bound"])
        self.assertEqual(contract["durable_native_resume_scope"], "none")
        self.assertEqual(contract["durable_native_interrupt_resume_boundary"], "graph_state_projection_only")
        self.assertNotIn("api_key", contract["llm_provider"])
        self.assertEqual(graph_state["datasets"]["ADAE"]["current_interrupt"]["name"], "code_review")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["code_state"]["status"], "generated")
        self.assertFalse((study_dir / "runs" / "run_native_full_run_endpoint" / "outputs" / "adae.csv").exists())

    def test_graph_command_approves_current_code_review_gate(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_graph_command_code_review")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_graph_command_code_review/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)

        response = client.post(
            "/runs/run_graph_command_code_review/graph-command",
            json={
                "study_dir": str(study_dir),
                "dataset": "ADAE",
                "interrupt": "code_review",
                "action": "approve",
                "reviewer": "qa_user",
                "notes": "Approved through unified graph command.",
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["scope"], "dataset")
        self.assertEqual(payload["dataset"], "ADAE")
        self.assertEqual(payload["interrupt"], "code_review")
        self.assertEqual(payload["action"], "approve")
        self.assertTrue(payload["approved"])
        self.assertEqual(payload["next_action"], "execute_approved_code")
        self.assertTrue(payload["review_artifact_path"].endswith("runs/run_graph_command_code_review/review/adae_code_review.json"))
        graph_state = client.get(
            "/runs/run_graph_command_code_review/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        state = graph_state.json()
        self.assertEqual(state["datasets"]["ADAE"]["code_state"]["status"], "approved")
        self.assertEqual(state["datasets"]["ADAE"]["human_commands"][-1]["reviewer"], "qa_user")

    def test_graph_command_rejects_dataset_command_while_study_gate_is_open(self) -> None:
        from adam_agent.schemas.graph_state import DatasetRunState, InterruptState, StudyRunState

        study_dir = _workspace_dir("phase8_graph_command_study_gate") / "PSY201"
        run_id = "run_graph_command_study_gate"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            current_interrupt=InterruptState(name="dependency_review", reason="Review dependency plan first."),
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(
                        name="code_review",
                        dataset="ADAE",
                        reason="Review ADAE code.",
                    ),
                )
            },
        )
        GraphGateway()._persist_graph_state(study_dir, state, node="test_seed_api_graph_command_study_gate")
        client = TestClient(create_app())

        response = client.post(
            f"/runs/{run_id}/graph-command",
            json={
                "study_dir": str(study_dir),
                "dataset": "ADAE",
                "interrupt": "code_review",
                "action": "approve",
                "reviewer": "qa_user",
            },
        )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("Resolve study-level interrupt dependency_review", response.json()["detail"])
        graph_state = client.get(f"/runs/{run_id}/graph-state", params={"study_dir": str(study_dir)})
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        persisted = graph_state.json()
        self.assertEqual(persisted["current_interrupt"]["name"], "dependency_review")
        self.assertEqual(persisted["datasets"]["ADAE"]["current_interrupt"]["name"], "code_review")
        self.assertFalse((study_dir / "runs" / run_id / "review" / "adae_code_review.json").exists())

    def test_graph_command_rejects_dependency_review_status_without_open_study_interrupt(self) -> None:
        from adam_agent.schemas.graph_state import DatasetRunState, InterruptState, StudyRunState

        study_dir = _workspace_dir("phase8_graph_command_rejected_dependency") / "PSY201"
        run_id = "run_graph_command_rejected_dependency"
        study_dir.mkdir(parents=True)
        state = StudyRunState(
            study_id="PSY201",
            run_id=run_id,
            status="needs_review",
            target_datasets=["ADAE"],
            runnable_datasets=["ADAE"],
            dependency_review_status="rejected",
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id=run_id,
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(
                        name="code_review",
                        dataset="ADAE",
                        reason="Review ADAE code.",
                    ),
                )
            },
        )
        GraphGateway()._persist_graph_state(study_dir, state, node="test_seed_api_graph_command_rejected_dependency")
        client = TestClient(create_app())

        response = client.post(
            f"/runs/{run_id}/graph-command",
            json={
                "study_dir": str(study_dir),
                "dataset": "ADAE",
                "interrupt": "code_review",
                "action": "approve",
                "reviewer": "qa_user",
            },
        )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("Resolve dependency_review", response.json()["detail"])
        graph_state = client.get(f"/runs/{run_id}/graph-state", params={"study_dir": str(study_dir)})
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        persisted = graph_state.json()
        self.assertEqual(persisted["dependency_review_status"], "rejected")
        self.assertEqual(persisted["datasets"]["ADAE"]["current_interrupt"]["name"], "code_review")
        self.assertFalse((study_dir / "runs" / run_id / "review" / "adae_code_review.json").exists())

    def test_graph_command_rejects_reserved_execute_after_approval(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_graph_command_reserved_execute")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_graph_command_reserved_execute/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)

        response = client.post(
            "/runs/run_graph_command_reserved_execute/graph-command",
            json={
                "study_dir": str(study_dir),
                "dataset": "ADAE",
                "interrupt": "code_review",
                "action": "approve",
                "reviewer": "qa_user",
                "execute_after_approval": True,
            },
        )

        self.assertEqual(response.status_code, 422, response.text)
        self.assertIn("execute_after_approval", response.text)
        graph_state = client.get(
            "/runs/run_graph_command_reserved_execute/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        self.assertEqual(graph_state.json()["datasets"]["ADAE"]["code_state"]["status"], "generated")
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_graph_command_reserved_execute"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_graph_command_rejects_continuation_settings_at_request_boundary(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_graph_command_reserved_config")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_graph_command_reserved_config/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)

        base_request = {
            "study_dir": str(study_dir),
            "dataset": "ADAE",
            "interrupt": "code_review",
            "action": "approve",
            "reviewer": "qa_user",
        }
        forbidden_fields = [
            {"config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json")},
            {"rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
            {"llm_provider_override": {"provider": "mock", "model": "mock-model"}},
            {"llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"}},
        ]
        for forbidden in forbidden_fields:
            with self.subTest(forbidden=sorted(forbidden)):
                response = client.post(
                    "/runs/run_graph_command_reserved_config/graph-command",
                    json={**base_request, **forbidden},
                )

                self.assertEqual(response.status_code, 422, response.text)
                for field in forbidden:
                    self.assertIn(field, response.text)

        graph_state = client.get(
            "/runs/run_graph_command_reserved_config/graph-state",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(graph_state.status_code, 200, graph_state.text)
        self.assertEqual(graph_state.json()["datasets"]["ADAE"]["code_state"]["status"], "generated")
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_graph_command_reserved_config"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_graph_command_rejects_action_that_does_not_match_current_gate(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_graph_command_wrong_gate")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_graph_command_wrong_gate/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)

        response = client.post(
            "/runs/run_graph_command_wrong_gate/graph-command",
            json={
                "study_dir": str(study_dir),
                "dataset": "ADAE",
                "interrupt": "draft_spec_review",
                "action": "approve",
                "reviewer": "qa_user",
            },
        )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("does not match current interrupt code_review", response.json()["detail"])

    def test_native_resume_endpoint_fails_closed_without_durable_checkpointer(self) -> None:
        study_dir = _study_with_adae_adcm_inputs("phase8_native_resume_memory_block")
        client = TestClient(create_app())
        started = client.post(
            "/runs/native-study-loop",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_native_resume_memory_block",
                "target_datasets": ["ADAE"],
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)

        response = client.post(
            "/runs/run_native_resume_memory_block/datasets/ADAE/native-resume",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Should fail closed with default memory checkpointer.",
            },
        )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("Native LangGraph interrupt resume is not enabled", response.json()["detail"])
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_native_resume_memory_block"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_native_full_run_resume_endpoint_approves_code_without_durable_resume(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_native_full_run_resume_code")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_native_full_run_resume_code/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)

        response = client.post(
            "/runs/run_native_full_run_resume_code/datasets/ADAE/native-full-run/resume",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Approve code but do not execute R.",
                "execute_after_approval": False,
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["dataset"], "ADAE")
        self.assertEqual(payload["last_interrupt"], "code_review")
        self.assertEqual(payload["phase"], "reviewed")
        self.assertTrue(payload["approved"])
        self.assertFalse(payload["executed"])
        self.assertFalse(payload["terminal_failure"])
        self.assertIsNone(payload["current_interrupt"])
        graph_state, _ = _assert_compatibility_projection(self, payload)
        contract = graph_state["runtime_persistence"]["native_dataset_full_run"]
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["last_interrupt"], "code_review")
        self.assertFalse(contract["executed_after_approval"])
        self.assertEqual(contract["resume_mode"], "graph_state_full_run_compatibility")
        self.assertTrue(contract["compatibility_resume_available"])
        self.assertFalse(contract["compatibility_resume_currently_available"])
        self.assertEqual(contract["compatibility_resume_boundary"], "historical_contract_only")
        self.assertTrue(contract["graph_state_resume_available"])
        self.assertFalse(contract["durable_resume_available"])
        self.assertFalse(contract["durable_full_run_resume_available"])
        self.assertEqual(contract["durable_full_run_resume_boundary"], "not_implemented")
        self.assertFalse(contract["durable_native_interrupt_resume_available"])
        self.assertEqual(
            graph_state["runtime_persistence"]["native_code_review_resume"]["resume_source"],
            "graph_state_compatibility_fallback",
        )
        self.assertFalse((study_dir / "runs" / "run_native_full_run_resume_code" / "outputs" / "adae.csv").exists())

    def test_native_full_run_paused_code_approval_then_explicit_execution_updates_contract(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_native_full_run_pause_then_execute")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_native_full_run_pause_then_execute/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)
        approved = client.post(
            "/runs/run_native_full_run_pause_then_execute/datasets/ADAE/native-full-run/resume",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Approve code but leave execution to the explicit UI action.",
                "execute_after_approval": False,
            },
        )
        self.assertEqual(approved.status_code, 200, approved.text)
        approved_state, _ = _assert_compatibility_projection(self, approved.json())
        self.assertEqual(approved_state["runtime_persistence"]["native_dataset_full_run"]["phase"], "reviewed")

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass", "errors": [], "warnings": []},
                "output_path": "runs/run_native_full_run_pause_then_execute/outputs/adae.csv",
                "validation_report_path": "runs/run_native_full_run_pause_then_execute/validation/adae_validation_report.json",
                "diagnostics_path": "",
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [],
                "agent_node_outputs": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            executed = client.post(
                "/runs/run_native_full_run_pause_then_execute/datasets/ADAE/execute-approved-code",
                json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
            )

        self.assertEqual(executed.status_code, 200, executed.text)
        payload = executed.json()
        self.assertEqual(payload["status"], "completed")
        graph_state, _ = _assert_compatibility_projection(self, payload)
        contract = graph_state["runtime_persistence"]["native_dataset_full_run"]
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "executed")
        self.assertEqual(contract["last_interrupt"], "code_review")
        self.assertTrue(contract["approved"])
        self.assertTrue(contract["executed_after_approval"])
        self.assertFalse(contract["terminal_failure"])
        self.assertNotIn("next_action", contract)
        self.assertEqual(
            graph_state["runtime_persistence"]["native_code_review_resume"]["resume_source"],
            "graph_state_compatibility_fallback",
        )

    def test_native_full_run_execute_endpoint_requires_lg3_contract(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_native_full_run_execute_requires_contract")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_native_full_run_execute_requires_contract/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)

        response = client.post(
            "/runs/run_native_full_run_execute_requires_contract/datasets/ADAE/native-full-run/execute",
            json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
        )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("No LG3 native full-run contract exists", response.json()["detail"])

    def test_native_full_run_execute_endpoint_runs_approved_lg3_contract(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_native_full_run_execute_endpoint")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_native_full_run_execute_endpoint/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)
        approved = client.post(
            "/runs/run_native_full_run_execute_endpoint/datasets/ADAE/native-full-run/resume",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Approve code before native full-run execute.",
                "execute_after_approval": False,
            },
        )
        self.assertEqual(approved.status_code, 200, approved.text)

        with patch("adam_agent.graph.gateway.compile_dataset_graph") as compile_graph:
            compile_graph.return_value.invoke.return_value = {
                "status": "completed",
                "response_status": "completed",
                "real_validation_status": "pass",
                "terminal_failure": False,
                "validation_report": {"status": "pass", "errors": [], "warnings": []},
                "output_path": "runs/run_native_full_run_execute_endpoint/outputs/adae.csv",
                "validation_report_path": "runs/run_native_full_run_execute_endpoint/validation/adae_validation_report.json",
                "diagnostics_path": "",
                "real_run_artifacts": {},
                "failure_records": [],
                "agent_decisions": [],
                "agent_node_inputs": [],
                "agent_node_outputs": [],
                "risk_flags": [],
                "execution_errors": [],
                "execution_warnings": [],
            }
            executed = client.post(
                "/runs/run_native_full_run_execute_endpoint/datasets/ADAE/native-full-run/execute",
                json={"study_dir": str(study_dir), "rscript_path": "C:/Dev/R-4.5.2/bin/Rscript.exe"},
            )

        self.assertEqual(executed.status_code, 200, executed.text)
        payload = executed.json()
        self.assertEqual(payload["status"], "completed")
        self.assertEqual(payload["validation_status"], "pass")
        graph_state, _ = _assert_compatibility_projection(self, payload)
        contract = graph_state["runtime_persistence"]["native_dataset_full_run"]
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["phase"], "executed")
        self.assertTrue(contract["executed_after_approval"])
        self.assertNotIn("next_action", contract)

    def test_native_full_run_resume_accepts_study_loop_full_run_dataset_contract(self) -> None:
        study_dir = _study_with_adae_adcm_inputs("phase8_native_full_run_resume_study_contract")
        client = TestClient(create_app())
        started = client.post(
            "/runs/native-study-loop",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_native_full_run_resume_study_contract",
                "target_datasets": ["ADAE", "ADCM"],
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)
        graph_path = study_dir / "runs" / "run_native_full_run_resume_study_contract" / "graph_state.json"
        before = json.loads(graph_path.read_text(encoding="utf-8"))
        self.assertEqual(before["runtime_persistence"]["native_dataset_full_run"]["dataset"], "ADCM")
        self.assertIn("ADAE", before["runtime_persistence"]["native_study_product_loop"]["full_run_datasets"])

        response = client.post(
            "/runs/run_native_full_run_resume_study_contract/datasets/ADAE/native-full-run/resume",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Approve ADAE even though the top-level single-dataset contract points to ADCM.",
                "execute_after_approval": False,
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["dataset"], "ADAE")
        self.assertEqual(payload["last_interrupt"], "code_review")
        self.assertTrue(payload["approved"])
        self.assertFalse(payload["executed"])
        graph_state, _ = _assert_compatibility_projection(self, payload)
        self.assertEqual(graph_state["datasets"]["ADAE"]["code_state"]["status"], "approved")
        self.assertEqual(graph_state["datasets"]["ADCM"]["current_interrupt"]["name"], "code_review")
        self.assertIn("ADAE", graph_state["runtime_persistence"]["native_study_product_loop"]["full_run_datasets"])

    def test_native_full_run_resume_endpoint_approves_draft_and_continues_to_code_review(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_native_full_run_resume_draft")
        client = TestClient(create_app())
        started = client.post(
            "/runs/run_native_full_run_resume_draft/datasets/ADAE/native-full-run",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(started.status_code, 200, started.text)
        self.assertEqual(started.json()["current_interrupt"]["name"], "draft_spec_review")

        response = client.post(
            "/runs/run_native_full_run_resume_draft/datasets/ADAE/native-full-run/resume",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
                "notes": "Approve draft and continue to code review.",
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["last_interrupt"], "draft_spec_review")
        self.assertEqual(payload["phase"], "waiting_for_human_gate")
        self.assertTrue(payload["approved"])
        self.assertFalse(payload["executed"])
        self.assertEqual(payload["current_interrupt"]["name"], "code_review")
        graph_state, _ = _assert_compatibility_projection(self, payload)
        dataset_state = graph_state["datasets"]["ADAE"]
        self.assertEqual(dataset_state["spec_state"]["status"], "approved")
        self.assertEqual(dataset_state["code_state"]["status"], "generated")
        self.assertEqual(dataset_state["current_interrupt"]["name"], "code_review")
        contract = graph_state["runtime_persistence"]["native_dataset_full_run"]
        self.assertEqual(contract["boundary"], "lg3_backend_contract")
        self.assertEqual(contract["last_interrupt"], "draft_spec_review")
        self.assertTrue(contract["code_generation_continued"])
        self.assertEqual(
            graph_state["runtime_persistence"]["native_draft_spec_review_resume"]["resume_source"],
            "graph_state_compatibility_fallback",
        )
        self.assertIn("native_dataset_product_loop_draft_resume", graph_state["runtime_persistence"])

    def test_native_full_run_resume_endpoint_rejects_non_lg3_split_flow(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_native_full_run_resume_requires_contract")
        client = TestClient(create_app())
        generated = client.post(
            "/runs/run_native_full_run_resume_requires_contract/datasets/ADAE/generate-code",
            json={
                "study_dir": str(study_dir),
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)

        response = client.post(
            "/runs/run_native_full_run_resume_requires_contract/datasets/ADAE/native-full-run/resume",
            json={
                "study_dir": str(study_dir),
                "decision": "approve",
                "reviewer": "tester",
            },
        )

        self.assertEqual(response.status_code, 400, response.text)
        self.assertIn("No LG3 native full-run contract exists", response.json()["detail"])
        self.assertFalse(
            (
                study_dir
                / "runs"
                / "run_native_full_run_resume_requires_contract"
                / "review"
                / "adae_code_review.json"
            ).exists()
        )

    def test_native_resume_endpoint_passes_llm_config_for_draft_continuation(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_native_resume_llm_config") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        dataset_state = SimpleNamespace(status="needs_review", current_interrupt=None)
        gateway_result = SimpleNamespace(
            graph_state=SimpleNamespace(
                study_id="MY_STUDY",
                datasets={"ADAE": dataset_state},
            ),
            workflow_projection={
                "workflow_control": "graph_gateway_compatibility_shim",
                "graph_state_path": str((study_dir / "runs" / "run_native_resume_llm_config" / "graph_state.json").as_posix()),
                "workflow_state_path": str((study_dir / "runs" / "run_native_resume_llm_config" / "workflow_state.json").as_posix()),
            },
            interrupt="draft_spec_review",
            decision="approve",
            execution=None,
        )
        captured: dict[str, Any] = {}

        class FakeGateway:
            def close(self) -> None:
                return None

            def native_interrupt_resume_available(self) -> bool:
                return True

            def resume_native_dataset_interrupt(self, **kwargs: Any) -> Any:
                captured.update(kwargs)
                return gateway_result

        request = SimpleNamespace(
            study_dir=str(study_dir),
            reviewer="tester",
            decision="approve",
            notes="Continue from draft spec to code review.",
            execute_after_approval=False,
            rscript_path="",
            config_path=None,
            llm_provider_override=SimpleNamespace(
                model_dump=lambda exclude_none=True: {
                    "provider": "mock",
                    "model": "mock-model",
                    "api_key": "test-key",
                    "timeout_seconds": 123.0,
                }
            ),
            llm_exposure_override=SimpleNamespace(
                model_dump=lambda exclude_none=True: {
                    "mode": "metadata_only",
                    "data_classification": "unknown",
                    "external_api_allowed": False,
                }
            ),
        )

        with patch("adam_agent.api.service._new_graph_gateway", return_value=FakeGateway()):
            response = service.resume_native_dataset_interrupt("run_native_resume_llm_config", "ADAE", request)

        self.assertEqual(response.interrupt, "draft_spec_review")
        self.assertEqual(captured["dataset"], "ADAE")
        self.assertEqual(captured["decision"], "approve")
        self.assertEqual(captured["llm_provider"]["provider"], "mock")
        self.assertEqual(captured["llm_provider"]["model"], "mock-model")
        self.assertEqual(captured["llm_provider"]["api_key"], "test-key")
        self.assertEqual(captured["llm_provider"]["timeout_seconds"], 123.0)
        self.assertEqual(captured["llm_exposure"]["mode"], "metadata_only")
        self.assertIs(captured["llm_client_builder"], service.build_llm_client)
        self.assertIs(captured["target_context_builder"], service.build_target_llm_context)

    def test_native_full_run_service_delegates_to_gateway_with_llm_config(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_native_full_run_service_config") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        dataset_state = SimpleNamespace(
            status="needs_review",
            current_interrupt=SimpleNamespace(model_dump=lambda mode="json": {"name": "code_review", "status": "open"}),
            code_state={
                "code_path": "runs/run_native_full_run_service/code/adae_generated.R",
                "static_check_path": "runs/run_native_full_run_service/static_checks/adae_static_check.json",
            },
            spec_state={},
        )
        gateway_result = SimpleNamespace(
            graph_state=SimpleNamespace(
                study_id="MY_STUDY",
                status="needs_review",
                datasets={"ADAE": dataset_state},
            ),
            workflow_projection={
                "workflow_control": "graph_gateway_compatibility_shim",
                "graph_state_path": str((study_dir / "runs" / "run_native_full_run_service" / "graph_state.json").as_posix()),
                "workflow_state_path": str((study_dir / "runs" / "run_native_full_run_service" / "workflow_state.json").as_posix()),
            },
            phase="waiting_for_human_gate",
        )
        captured: dict[str, Any] = {}

        class FakeGateway:
            def close(self) -> None:
                return None

            def start_native_dataset_full_run(self, **kwargs: Any) -> Any:
                captured.update(kwargs)
                return gateway_result

        request = SimpleNamespace(
            study_dir=str(study_dir),
            study_id="MY_STUDY",
            config_path=None,
            rscript_path="C:/Dev/R-4.5.2/bin/Rscript.exe",
            llm_provider_override=SimpleNamespace(
                model_dump=lambda exclude_none=True: {
                    "provider": "mock",
                    "model": "mock-model",
                    "api_key": "test-key",
                    "timeout_seconds": 123.0,
                }
            ),
            llm_exposure_override=SimpleNamespace(
                model_dump=lambda exclude_none=True: {
                    "mode": "metadata_only",
                    "data_classification": "unknown",
                    "external_api_allowed": False,
                }
            ),
        )

        with patch("adam_agent.api.service._new_graph_gateway", return_value=FakeGateway()):
            response = service.start_native_dataset_full_run("run_native_full_run_service", "adae", request)

        self.assertEqual(response.dataset, "ADAE")
        self.assertEqual(response.current_interrupt["name"], "code_review")
        self.assertEqual(response.next_action, "code_review")
        self.assertTrue(response.static_check_path.endswith("static_checks/adae_static_check.json"))
        self.assertEqual(captured["dataset"], "ADAE")
        self.assertEqual(captured["run_id"], "run_native_full_run_service")
        self.assertEqual(captured["llm_provider"]["api_key"], "test-key")
        self.assertEqual(captured["llm_exposure"]["mode"], "metadata_only")
        self.assertIs(captured["llm_client_builder"], service.build_llm_client)
        self.assertIs(captured["target_context_builder"], service.build_target_llm_context)
        self.assertEqual(captured["rscript_path"], "C:/Dev/R-4.5.2/bin/Rscript.exe")

    def test_native_resume_endpoint_fails_before_llm_config_when_memory_checkpointer(self) -> None:
        from adam_agent.api import service

        study_dir = _workspace_dir("phase8_native_resume_memory_before_llm") / "MY_STUDY"
        study_dir.mkdir(parents=True)
        resume_called = False

        class FakeGateway:
            def close(self) -> None:
                return None

            def native_interrupt_resume_available(self) -> bool:
                return False

            def resume_native_dataset_interrupt(self, **kwargs: Any) -> Any:
                nonlocal resume_called
                resume_called = True
                raise AssertionError("Service must not call gateway native resume in memory mode.")

        request = SimpleNamespace(
            study_dir=str(study_dir),
            reviewer="tester",
            decision="approve",
            notes="Should fail closed before LLM config resolution.",
            execute_after_approval=False,
            rscript_path="",
            config_path="D:/does/not/exist.json",
            llm_provider_override=None,
            llm_exposure_override=None,
        )

        with patch("adam_agent.api.service._new_graph_gateway", return_value=FakeGateway()):
            with self.assertRaisesRegex(service.ApiServiceError, "Native LangGraph interrupt resume is not enabled"):
                service.resume_native_dataset_interrupt("run_memory_before_llm", "ADAE", request)
        self.assertFalse(resume_called)

    def test_native_study_loop_endpoint_reports_preserved_progress_on_restart(self) -> None:
        study_dir = _study_with_adae_adcm_inputs("phase8_native_study_loop_restart_skip")
        client = TestClient(create_app())
        request = {
            "study_dir": str(study_dir),
            "run_id": "run_native_study_loop_restart_skip",
            "target_datasets": ["ADAE", "ADCM"],
            "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
        }
        first = client.post("/runs/native-study-loop", json=request)
        self.assertEqual(first.status_code, 200, first.text)

        second = client.post("/runs/native-study-loop", json=request)

        self.assertEqual(second.status_code, 200, second.text)
        payload = second.json()
        self.assertEqual(payload["started_datasets"], [])
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in payload["skipped_datasets"]],
            [
                ("ADAE", "existing_graph_progress", "review_code"),
                ("ADCM", "existing_graph_progress", "review_code"),
            ],
        )
        self.assertIn("existing graph progress was preserved for ADAE, ADCM", payload["message"])
        progress = client.get(
            "/runs/run_native_study_loop_restart_skip/progress",
            params={"study_dir": str(study_dir)},
        )
        self.assertEqual(progress.status_code, 200, progress.text)
        progress_payload = progress.json()
        self.assertEqual(
            [(item["dataset"], item["reason"], item["next_action"]) for item in progress_payload["study_loop_result"]["skipped_datasets"]],
            [
                ("ADAE", "existing_graph_progress", "review_code"),
                ("ADCM", "existing_graph_progress", "review_code"),
            ],
        )

    def test_native_study_loop_endpoint_does_not_start_dependency_blocked_targets(self) -> None:
        study_dir = _workspace_dir("phase8_native_study_loop_dependency_block") / "MY_STUDY"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "lb.csv").write_text("USUBJID,LBTEST\n01,ALT\n", encoding="utf-8")
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
        client = TestClient(create_app())

        response = client.post(
            "/runs/native-study-loop",
            json={
                "study_dir": str(study_dir),
                "run_id": "run_native_study_loop_dependency_block",
                "target_datasets": ["ADTTE"],
                "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
            },
        )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["started_datasets"], [])
        self.assertEqual(payload["dataset_results"], [])
        self.assertTrue(any(block["dataset"] == "ADTTE" for block in payload["blocked_datasets"]))
        self.assertFalse((study_dir / "runs" / "run_native_study_loop_dependency_block" / "code").exists())

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
        self.assertEqual(response.json()["read_model_source"], "artifact_fallback")
        self.assertIsNone(response.json()["graph_state_path"])
        self.assertIsNone(response.json()["workflow_state_path"])
        reviews = response.json()["dataset_reviews"]
        self.assertIn("2 dataset(s) have real runtime output", response.json()["plain_summary"])
        self.assertEqual({item["dataset"] for item in reviews}, {"ADSL", "ADAE"})
        for item in reviews:
            self.assertEqual(item["status"], "completed")
            self.assertTrue(item["output_preview"])
            self.assertTrue(item["output_path"].endswith(f"{item['dataset'].lower()}.csv"))

    def test_review_summary_surfaces_not_real_quality_from_workflow_projection(self) -> None:
        study_dir = _workspace_dir("phase8_review_not_real_quality") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_not_real_quality"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        (run_dir / "workflow_state.json").write_text(
            json.dumps(
                {
                    "study_id": "MY_STUDY",
                    "run_id": "run_not_real_quality",
                    "status": "completed",
                    "datasets": {
                        "ADSL": {
                            "dataset": "ADSL",
                            "status": "completed",
                            "code_state": {
                                "generation_quality": {
                                    "llm_provider": "mock",
                                    "llm_model": "mock-model",
                                    "not_real_derivation": True,
                                }
                            },
                            "execution_state": {
                                "status": "completed",
                                "terminal_failure": False,
                                "partial_output_usable": True,
                                "not_real_derivation": True,
                                "generation_quality": {
                                    "llm_provider": "mock",
                                    "llm_model": "mock-model",
                                    "not_real_derivation": True,
                                },
                            },
                            "validation_summary": {"status": "pass"},
                        }
                    },
                }
            ),
            encoding="utf-8",
        )
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_not_real_quality/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        self.assertEqual(response.json()["read_model_source"], "workflow_state_fallback")
        self.assertIsNone(response.json()["graph_state_path"])
        self.assertTrue(response.json()["workflow_state_path"].endswith("runs/run_not_real_quality/workflow_state.json"))
        review = response.json()["dataset_reviews"][0]
        self.assertIn("0 dataset(s) have real runtime output", response.json()["plain_summary"])
        self.assertIn("1 review-only/demo output(s)", response.json()["plain_summary"])
        self.assertEqual(review["status"], "completed")
        self.assertEqual(review["output_quality"]["quality_status"], "not_real_derivation")
        self.assertFalse(review["output_quality"]["runtime_dependency_eligible"])
        self.assertIn("mock", " ".join(review["warnings"]).lower())

    def test_review_summary_fails_closed_when_existing_graph_state_is_corrupt(self) -> None:
        study_dir = _workspace_dir("phase8_review_corrupt_graph_state") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_corrupt_graph_review"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        (run_dir / "workflow_state.json").write_text(
            json.dumps(
                {
                    "study_id": "MY_STUDY",
                    "run_id": "run_corrupt_graph_review",
                    "status": "completed",
                    "datasets": {
                        "ADSL": {
                            "dataset": "ADSL",
                            "status": "completed",
                            "validation_summary": {"status": "pass"},
                        }
                    },
                }
            ),
            encoding="utf-8",
        )
        (run_dir / "graph_state.json").write_text("{not-json", encoding="utf-8")
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_corrupt_graph_review/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 404, response.text)
        detail = response.json()["detail"]
        self.assertIn("Canonical graph state", detail)
        self.assertIn("will not fall back to workflow_state.json", detail)

    def test_review_summary_prefers_graph_state_without_workflow_projection(self) -> None:
        study_dir = _workspace_dir("phase8_review_graph_state_authority") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_review_authority"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        output_path = output_dir / "adsl.csv"
        output_path.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_review_authority",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_review_authority").model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "completed"
        state.datasets["ADSL"].current_interrupt = None
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].code_state = {
            "status": "approved",
            "generation_quality": {
                "llm_provider": "mock",
                "llm_model": "mock-model",
                "not_real_derivation": True,
            },
        }
        state.datasets["ADSL"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": str(output_path.as_posix()),
            "generation_quality": {
                "llm_provider": "mock",
                "llm_model": "mock-model",
                "not_real_derivation": True,
            },
            "not_real_derivation": True,
        }
        state.datasets["ADSL"].validation_summary = {"status": "pass"}
        gateway._persist_graph_state(study_dir, state, node="test_seed_review_graph_state_authority")
        workflow_projection = run_dir / "workflow_state.json"
        self.assertTrue(workflow_projection.exists())
        workflow_projection.unlink()
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_graph_review_authority/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        self.assertEqual(response.json()["study_id"], "MY_STUDY")
        self.assertEqual(response.json()["status"], "completed")
        self.assertEqual(response.json()["read_model_source"], "graph_state")
        self.assertTrue(response.json()["graph_state_path"].endswith("runs/run_graph_review_authority/graph_state.json"))
        self.assertIsNone(response.json()["workflow_state_path"])
        review = response.json()["dataset_reviews"][0]
        self.assertIn("0 dataset(s) have real runtime output", response.json()["plain_summary"])
        self.assertIn("1 review-only/demo output(s)", response.json()["plain_summary"])
        self.assertEqual(review["dataset"], "ADSL")
        self.assertEqual(review["status"], "completed")
        self.assertEqual(review["output_quality"]["quality_status"], "not_real_derivation")
        self.assertEqual(review["output_quality"]["provider"], "mock")
        self.assertFalse(review["output_quality"]["runtime_dependency_eligible"])
        self.assertTrue(review["output_preview"])

    def test_review_summary_prefers_graph_validation_over_stale_validation_file(self) -> None:
        study_dir = _workspace_dir("phase8_review_graph_validation_authority") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_validation_authority"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        output_path = output_dir / "adsl.csv"
        output_path.write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps(
                {
                    "dataset": "ADSL",
                    "status": "pass",
                    "terminal_failure": False,
                    "partial_output_usable": True,
                    "warnings": ["stale validation file warning"],
                }
            ),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_validation_authority",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_validation_authority").model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "completed"
        state.datasets["ADSL"].current_interrupt = None
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": str(output_path.as_posix()),
        }
        state.datasets["ADSL"].validation_summary = {
            "status": "structural_stub_pass",
            "warnings": ["canonical graph warning"],
        }
        gateway._persist_graph_state(study_dir, state, node="test_seed_review_graph_validation_authority")
        (run_dir / "workflow_state.json").unlink()
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_graph_validation_authority/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        review = response.json()["dataset_reviews"][0]
        self.assertEqual(review["validation_status"], "structural_stub_pass")
        self.assertEqual(review["validation_report"]["status"], "structural_stub_pass")
        self.assertEqual(review["output_quality"]["quality_status"], "structural_stub")
        self.assertFalse(review["output_quality"]["runtime_dependency_eligible"])
        self.assertIn("canonical graph warning", " ".join(review["warnings"]))
        self.assertNotIn("stale validation file warning", " ".join(review["warnings"]))

    def test_review_summary_resolves_graph_state_run_relative_output_path(self) -> None:
        study_dir = _workspace_dir("phase8_review_graph_state_run_relative_output") / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_review_run_relative"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_review_run_relative",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_review_run_relative").model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "completed"
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": "runs/run_graph_review_run_relative/outputs/adsl.csv",
        }
        state.datasets["ADSL"].validation_summary = {"status": "pass"}
        gateway._persist_graph_state(study_dir, state, node="test_seed_review_graph_state_run_relative")
        (run_dir / "workflow_state.json").unlink()
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_graph_review_run_relative/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        review = response.json()["dataset_reviews"][0]
        self.assertTrue(review["output_path"].endswith("runs/run_graph_review_run_relative/outputs/adsl.csv"))
        self.assertTrue(review["output_preview"])

    def test_review_summary_ignores_graph_state_output_path_outside_run_dir(self) -> None:
        workspace = _workspace_dir("phase8_review_graph_state_output_escape")
        study_dir = workspace / "MY_STUDY"
        run_dir = study_dir / "runs" / "run_graph_review_escape"
        output_dir = run_dir / "outputs"
        validation_dir = run_dir / "validation"
        output_dir.mkdir(parents=True)
        validation_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        escaped_output = workspace / "escaped_adsl.csv"
        escaped_output.write_text("USUBJID,TRTSDT\n99,2099-01-01\n", encoding="utf-8")
        (validation_dir / "adsl_validation_report.json").write_text(
            json.dumps({"dataset": "ADSL", "status": "pass", "terminal_failure": False, "partial_output_usable": True}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="MY_STUDY",
            run_id="run_graph_review_escape",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(study_dir=study_dir, run_id="run_graph_review_escape").model_copy(deep=True)
        state.dependency_review_status = "accepted"
        state.current_interrupt = None
        state.status = "completed"
        state.datasets["ADSL"].status = "completed"
        state.datasets["ADSL"].execution_state = {
            "status": "completed",
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": "../../escaped_adsl.csv",
        }
        state.datasets["ADSL"].validation_summary = {"status": "pass"}
        gateway._persist_graph_state(study_dir, state, node="test_seed_review_graph_state_escape")
        (run_dir / "workflow_state.json").unlink()
        client = TestClient(create_app())

        response = client.get(
            "/runs/run_graph_review_escape/review-summary",
            params={"study_dir": str(study_dir)},
        )

        self.assertEqual(response.status_code, 200, response.text)
        review = response.json()["dataset_reviews"][0]
        self.assertIsNone(review["output_path"])
        self.assertIsNone(review["output_preview"])
        self.assertEqual(review["compare_summary"]["status"], "missing_generated")

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

    def test_native_resume_approves_generated_draft_spec_without_500_when_sqlite_enabled(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_native_resume_draft_spec_sqlite")
        client = TestClient(create_app())
        run_id = "run_native_resume_draft_spec_sqlite"
        with patch.dict("os.environ", {"ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "sqlite"}):
            finalized = client.post(
                f"/runs/{run_id}/datasets/ADAE/finalize-inputs",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {"provider": "mock", "model": "mock"},
                    "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
                },
            )
            self.assertEqual(finalized.status_code, 200, finalized.text)
            self.assertEqual(finalized.json()["status"], "draft_spec_review_required")

            response = client.post(
                f"/runs/{run_id}/datasets/ADAE/native-resume",
                json={
                    "study_dir": str(study_dir),
                    "reviewer": "api_tester",
                    "decision": "approve",
                    "notes": "Approve generated draft spec through UI native-resume path.",
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {"provider": "mock", "model": "mock"},
                    "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["interrupt"], "draft_spec_review")
        self.assertEqual(payload["decision"], "approve")
        self.assertNotEqual(payload["status"], "failed")
        self.assertIsNone(payload["current_interrupt"])
        graph_state, workflow_state = _assert_run_projection(self, study_dir, run_id)
        self.assertEqual(graph_state["datasets"]["ADAE"]["spec_state"]["status"], "approved")
        self.assertIsNone(graph_state["datasets"]["ADAE"]["current_interrupt"])
        self.assertEqual(workflow_state["datasets"]["ADAE"]["spec_state"]["status"], "approved")

    def test_graph_command_approval_then_native_full_run_continues_to_code_review(self) -> None:
        study_dir = _study_without_spec_with_auxiliary_evidence("phase8_graph_command_draft_to_full_run_sqlite")
        client = TestClient(create_app())
        run_id = "run_graph_command_draft_to_full_run_sqlite"
        with patch.dict("os.environ", {"ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "sqlite"}):
            finalized = client.post(
                f"/runs/{run_id}/datasets/ADAE/finalize-inputs",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {"provider": "mock", "model": "mock"},
                    "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
                },
            )
            self.assertEqual(finalized.status_code, 200, finalized.text)
            self.assertEqual(finalized.json()["status"], "draft_spec_review_required")

            approved = client.post(
                f"/runs/{run_id}/graph-command",
                json={
                    "study_dir": str(study_dir),
                    "dataset": "ADAE",
                    "interrupt": "draft_spec_review",
                    "action": "approve",
                    "reviewer": "api_tester",
                    "notes": "Approve generated draft spec through unified graph command.",
                },
            )
            self.assertEqual(approved.status_code, 200, approved.text)
            self.assertEqual(approved.json()["status"], "pending")

            response = client.post(
                f"/runs/{run_id}/datasets/ADAE/native-full-run",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                    "llm_provider_override": {"provider": "mock", "model": "mock"},
                    "llm_exposure_override": {"mode": "metadata_only", "data_classification": "unknown"},
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["phase"], "waiting_for_human_gate")
        self.assertEqual(payload["current_interrupt"]["name"], "code_review")
        self.assertEqual(payload["next_action"], "code_review")
        graph_state, workflow_state = _assert_run_projection(self, study_dir, run_id)
        self.assertEqual(graph_state["datasets"]["ADAE"]["spec_state"]["status"], "approved")
        self.assertEqual(graph_state["datasets"]["ADAE"]["code_state"]["status"], "generated")
        self.assertEqual(graph_state["datasets"]["ADAE"]["code_state"]["spec_source"], "approved_draft_spec")
        self.assertEqual(workflow_state["datasets"]["ADAE"]["current_interrupt"], "code_review")

    def test_finalize_inputs_with_input_spec_does_not_start_native_draft_spec_when_sqlite_enabled(self) -> None:
        study_dir = _study_with_adae_inputs("phase8_finalize_existing_spec_sqlite")
        client = TestClient(create_app())
        run_id = "run_finalize_existing_spec_sqlite"
        with patch.dict("os.environ", {"ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND": "sqlite"}):
            response = client.post(
                f"/runs/{run_id}/datasets/ADAE/finalize-inputs",
                json={
                    "study_dir": str(study_dir),
                    "config_path": str(ROOT / "studies" / "_template" / "configs" / "mock_downstream.json"),
                },
            )

        self.assertEqual(response.status_code, 200, response.text)
        payload = response.json()
        self.assertEqual(payload["status"], "input_spec_ready")
        self.assertFalse(payload["draft_spec_required"])
        self.assertIsNone(payload["draft_spec"])
        graph_state, _workflow_state = _assert_compatibility_projection(self, payload)
        self.assertEqual(graph_state["datasets"]["ADAE"]["spec_state"]["status"], "input_spec_ready")
        self.assertNotIn("native_draft_spec_review_interrupt", graph_state["runtime_persistence"])

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


def _study_with_legacy_adam_dependency(name: str) -> Path:
    study_dir = _workspace_dir(name) / "PSY201"
    input_sdtm = study_dir / "input_sdtm"
    legacy_code = study_dir / "legacy_code"
    input_sdtm.mkdir(parents=True)
    legacy_code.mkdir()
    (input_sdtm / "dm.csv").write_text("USUBJID,STUDYID\n01,PSY201\n", encoding="utf-8")
    (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
    (legacy_code / "ADDM.sas").write_text("data addm; set dm; run;\n", encoding="utf-8")
    (legacy_code / "ADAE.sas").write_text("data adae; set addm ae; run;\n", encoding="utf-8")
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
