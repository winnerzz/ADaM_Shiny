"""Smoke tests for the Phase 3 LangGraph skeleton."""

from __future__ import annotations

import sys
import unittest
import uuid
from pathlib import Path

from langgraph.checkpoint.memory import InMemorySaver

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"
LOCAL_RSCRIPT = Path(r"C:\Dev\R-4.5.2\bin\Rscript.exe")

try:
    from adam_agent.graph.dataset_graph import compile_dataset_graph
    from adam_agent.graph.routing import route_after_sandbox
    from adam_agent.graph.study_graph import compile_study_graph
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.graph.dataset_graph import compile_dataset_graph
    from adam_agent.graph.routing import route_after_sandbox
    from adam_agent.graph.study_graph import compile_study_graph


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class GraphSmokeTests(unittest.TestCase):
    def test_study_graph_runs_foundation_then_downstream_stub_datasets(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_smoke",
                "target_datasets": ["ADSL", "ADAE"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(set(summaries), {"ADSL", "ADAE"})
        self.assertEqual(summaries["ADSL"].status, "completed")
        self.assertEqual(summaries["ADAE"].status, "completed")
        self.assertEqual(result["status"], "completed")
        self.assertEqual(result["blocked_datasets"], [])
        self.assertEqual(result["audit_manifest"].kind, "audit_manifest")
        self.assertIn("ADSL", result["audit_manifest"].metadata["datasets"])
        self.assertIn("ADAE", result["audit_manifest"].metadata["datasets"])

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_study_graph_can_run_real_adsl_minimal_foundation(self) -> None:
        study_dir = _workspace_dir("graph_real_adsl") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        for folder in ["reference_adam", "input_spec", "input_define", "legacy_code", "runs"]:
            (study_dir / folder).mkdir()
        (input_dir / "dm.csv").write_text(
            "STUDYID,USUBJID,AGE,SEX,ARM\nS1,01,34,F,Test Drug\nS1,02,41,M,Placebo\n",
            encoding="utf-8",
        )
        (input_dir / "ex.csv").write_text(
            "USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n",
            encoding="utf-8",
        )
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_graph_real_adsl",
                "target_datasets": ["ADSL"],
                "execution_mode": "real_adsl_minimal",
                "study_dir": str(study_dir),
                "rscript_path": str(LOCAL_RSCRIPT),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        adsl_summary = summaries["ADSL"]
        self.assertEqual(result["status"], "completed")
        self.assertEqual(adsl_summary.status, "completed")
        self.assertEqual(adsl_summary.validation_status, "pass")
        self.assertEqual(adsl_summary.compare_status, "skipped")
        self.assertEqual(adsl_summary.output_artifact_ids, ["adsl_output_csv"])
        self.assertTrue((study_dir / "runs" / "run_graph_real_adsl" / "outputs" / "adsl.csv").exists())
        self.assertTrue((study_dir / "runs" / "run_graph_real_adsl" / "audit" / "manifest.json").exists())

    def test_dataset_state_isolation_across_stub_runs(self) -> None:
        dataset_graph = compile_dataset_graph()

        adsl = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_isolation",
                "dataset": "ADSL",
                "stub_scenario": "success",
                "audit_artifacts": [],
            }
        )
        adae = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_isolation",
                "dataset": "ADAE",
                "stub_scenario": "code_error_then_success",
                "audit_artifacts": [],
            }
        )

        self.assertEqual(adsl["summary"].status, "completed")
        self.assertEqual(adae["summary"].status, "completed")
        self.assertEqual(adsl["repair_attempts"], 0)
        self.assertEqual(adae["repair_attempts"], 1)
        self.assertEqual(adsl["summary"].dataset, "ADSL")
        self.assertEqual(adae["summary"].dataset, "ADAE")

    def test_real_adsl_dataset_graph_returns_structured_failure_for_missing_inputs(self) -> None:
        study_dir = _workspace_dir("graph_real_adsl_missing") / "PSY201"
        study_dir.mkdir(parents=True)
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_graph_real_adsl_missing",
                "dataset": "ADSL",
                "execution_mode": "real_adsl_minimal",
                "study_dir": str(study_dir),
                "rscript_path": str(LOCAL_RSCRIPT),
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["summary"].status, "failed")
        self.assertEqual(result["summary"].validation_status, "not_run")
        self.assertEqual(result["summary"].failure_ids, ["failure_adsl_real_minimal"])
        self.assertIn("requires input_sdtm/dm.csv or .sas7bdat", result["real_run_error"])

    def test_adsl_failure_blocks_downstream_without_running_it(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_blocked",
                "target_datasets": ["ADSL", "ADAE"],
                "stub_scenarios": {"ADSL": "fail_adsl"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADSL"].status, "failed")
        self.assertEqual(summaries["ADAE"].status, "failed")
        self.assertEqual(summaries["ADAE"].validation_status, "blocked_by_adsl")
        self.assertEqual(result["blocked_datasets"], [{"dataset": "ADAE", "reason": "blocked_by_adsl", "blocked_by": "ADSL"}])
        self.assertEqual(result["status"], "failed")

    def test_downstream_only_request_still_runs_adsl_foundation_first(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_downstream_only",
                "target_datasets": ["ADAE"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(set(summaries), {"ADSL", "ADAE"})
        self.assertEqual(result["foundation_datasets"], ["ADSL"])
        self.assertEqual(result["downstream_datasets"], ["ADAE"])

    def test_checkpoint_history_exists_and_matches_final_state(self) -> None:
        checkpointer = InMemorySaver()
        graph = compile_study_graph(checkpointer=checkpointer)
        config = {"configurable": {"thread_id": "PSY201:run_phase3_checkpoint"}}

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_checkpoint",
                "target_datasets": ["ADSL", "ADAE"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            },
            config=config,
        )
        history = list(graph.get_state_history(config))

        self.assertGreater(len(history), 0)
        self.assertEqual(result["status"], "completed")
        self.assertTrue(any(snapshot.values.get("status") == "completed" for snapshot in history))
        final_snapshot = graph.get_state(config)
        self.assertEqual(final_snapshot.values["status"], result["status"])

    def test_dataset_result_reducer_keeps_more_than_two_results(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_reducer",
                "target_datasets": ["ADSL", "ADAE", "ADCM"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        datasets = [summary.dataset for summary in result["dataset_results"]]
        self.assertEqual(set(datasets), {"ADSL", "ADAE", "ADCM"})
        self.assertEqual(len(datasets), 3)

    def test_routing_result_selects_one_path_per_scenario(self) -> None:
        self.assertEqual(route_after_sandbox({"failure_type": None}), "success")
        self.assertEqual(route_after_sandbox({"failure_type": "code_error"}), "repair_code")
        self.assertEqual(route_after_sandbox({"failure_type": "spec_error"}), "revise_spec")

    def test_routing_respects_max_repair_attempts(self) -> None:
        self.assertEqual(
            route_after_sandbox(
                {
                    "failure_type": "code_error",
                    "repair_attempts": 3,
                    "max_repair_attempts": 3,
                }
            ),
            "fail",
        )
        self.assertEqual(
            route_after_sandbox(
                {
                    "failure_type": "spec_error",
                    "repair_attempts": 1,
                    "max_repair_attempts": 3,
                }
            ),
            "revise_spec",
        )


if __name__ == "__main__":
    unittest.main()
