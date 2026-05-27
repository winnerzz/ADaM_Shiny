"""Smoke tests for the Phase 3 LangGraph skeleton."""

from __future__ import annotations

import json
import subprocess
import sys
import unittest
import uuid
from pathlib import Path

from langgraph.checkpoint.memory import InMemorySaver

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"
LOCAL_RSCRIPT = Path(r"C:\Dev\R-4.5.2\bin\Rscript.exe")

try:
    from adam_agent.graph.dependencies import plan_dataset_dependencies
    from adam_agent.graph.dataset_graph import compile_dataset_graph
    from adam_agent.graph.routing import route_after_sandbox
    from adam_agent.graph.study_graph import compile_study_graph
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.graph.dependencies import plan_dataset_dependencies
    from adam_agent.graph.dataset_graph import compile_dataset_graph
    from adam_agent.graph.routing import route_after_sandbox
    from adam_agent.graph.study_graph import compile_study_graph


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class GraphSmokeTests(unittest.TestCase):
    def test_dependency_plan_auto_adds_adsl_once_for_downstream_requests(self) -> None:
        plan = plan_dataset_dependencies(["ADAE", "ADCM"])

        self.assertEqual(plan.requested_datasets, ["ADAE", "ADCM"])
        self.assertEqual(plan.target_datasets, ["ADSL", "ADAE", "ADCM"])
        self.assertEqual(plan.auto_added_datasets, ["ADSL"])
        self.assertEqual(plan.foundation_datasets, ["ADSL"])
        self.assertEqual(plan.downstream_datasets, ["ADAE", "ADCM"])
        self.assertEqual(plan.unsupported_datasets, [])
        self.assertEqual(plan.dependencies["ADAE"], ["ADSL"])
        self.assertEqual(plan.dependencies["ADCM"], ["ADSL"])
        self.assertEqual(plan.dependency_graph, {"ADSL": ["ADAE", "ADCM"]})
        self.assertEqual(plan.evidence, "phase7_mvp_fallback_adsl_foundation")
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADAE"].source, "mvp_common_adam_fallback")
        self.assertTrue(decisions["ADAE"].review_required)
        self.assertGreater(decisions["ADAE"].confidence, 0.5)

    def test_dependency_plan_marks_unknown_adam_dependencies_low_confidence_review_required(self) -> None:
        plan = plan_dataset_dependencies(["ADTTE"])

        self.assertEqual(plan.target_datasets, ["ADSL", "ADTTE"])
        self.assertEqual(plan.dependencies["ADTTE"], ["ADSL"])
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADTTE"].source, "mvp_unknown_adam_fallback")
        self.assertEqual(decisions["ADTTE"].confidence, 0.4)
        self.assertTrue(decisions["ADTTE"].review_required)
        self.assertIn("requires review", decisions["ADTTE"].reason)

    def test_dependency_plan_marks_non_ad_targets_unsupported(self) -> None:
        plan = plan_dataset_dependencies(["LB"])

        self.assertEqual(plan.requested_datasets, ["LB"])
        self.assertEqual(plan.target_datasets, ["LB"])
        self.assertEqual(plan.foundation_datasets, [])
        self.assertEqual(plan.downstream_datasets, [])
        self.assertEqual(plan.unsupported_datasets, ["LB"])
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["LB"].source, "mvp_no_default_dependency")
        self.assertTrue(decisions["LB"].review_required)

    def test_dependency_plan_uses_input_spec_dependency_before_fallback(self) -> None:
        study_dir = _workspace_dir("phase7_spec_dependency") / "PSY201"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "adtte.json").write_text(
            json.dumps(
                {
                    "dataset": "ADTTE",
                    "variables": [
                        {
                            "variable": "PARAMCD",
                            "source_domains": ["ADSL", "ADLB"],
                            "derivation": "Derive time to threshold from ADLB and subject-level covariates from ADSL.",
                        }
                    ],
                }
            ),
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADTTE"], study_dir=study_dir)

        self.assertEqual(plan.requested_datasets, ["ADTTE"])
        self.assertEqual(plan.target_datasets, ["ADSL", "ADLB", "ADTTE"])
        self.assertEqual(plan.dependencies["ADTTE"], ["ADSL", "ADLB"])
        self.assertEqual(plan.dependencies["ADLB"], ["ADSL"])
        self.assertEqual(plan.execution_batches, [["ADSL"], ["ADLB"], ["ADTTE"]])
        self.assertEqual(plan.evidence, "user_evidence_plus_mvp_fallback")
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADTTE"].source, "input_spec_dependency")
        self.assertGreater(decisions["ADTTE"].confidence, 0.8)
        self.assertFalse(decisions["ADTTE"].review_required)
        self.assertTrue(decisions["ADTTE"].evidence_ids)

    def test_dependency_plan_recognizes_ads_full_spec_filename(self) -> None:
        study_dir = _workspace_dir("phase7_ads_full_spec_filename") / "demo_adam"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "ads_adae_full.csv").write_text(
            "Dataset,Variable,Label,Type,Source,Derivation\n"
            "ADAE,USUBJID,Unique Subject Identifier,Copied,SDTM.AE.USUBJID,Copied from source\n"
            "ADAE,TRTSDT,Treatment Start Date,Copied,ADSL.TRTSDT,Copied from ADSL\n",
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADAE"], study_dir=study_dir)

        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(plan.dependencies["ADAE"], ["ADSL"])
        self.assertEqual(decisions["ADAE"].source, "input_spec_dependency")
        self.assertFalse(
            any("no dependency evidence was extracted for ADAE" in warning for warning in plan.planning_warnings)
        )

    def test_target_input_spec_without_adam_dependency_does_not_force_adsl(self) -> None:
        study_dir = _workspace_dir("phase7_addm_spec_without_adsl") / "demo_adam"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "addm_spec.csv").write_text(
            "Dataset,Variable,Label,Type,Source,Derivation\n"
            "ADDM,USUBJID,Unique Subject Identifier,Copied,SDTM.DM.USUBJID,Copied from DM\n"
            "ADDM,DMDTC,Disposition Date,Copied,SDTM.DS.DSSTDTC,Copied from DS\n",
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADDM"], study_dir=study_dir)

        self.assertEqual(plan.target_datasets, ["ADDM"])
        self.assertEqual(plan.dependencies["ADDM"], [])
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADDM"].source, "input_spec_no_adam_dependency")
        self.assertFalse(decisions["ADDM"].review_required)
        self.assertFalse(plan.blocked_datasets if hasattr(plan, "blocked_datasets") else False)

    def test_dependency_plan_ignores_ad_words_in_spec_labels(self) -> None:
        study_dir = _workspace_dir("phase7_spec_label_ad_words") / "demo_adam"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "ads_adae_full.csv").write_text(
            "Dataset,Variable,Label,Type,Source,Derivation\n"
            "ADAE,AETERM,Adverse Event Term,Copied,SDTM.AE.AETERM,Copied from source\n"
            "ADAE,TRTSDT,Treatment Start Date,Copied,ADSL.TRTSDT,Copied from ADSL\n",
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADAE"], study_dir=study_dir)

        self.assertEqual(plan.target_datasets, ["ADSL", "ADAE"])
        self.assertEqual(plan.dependencies["ADAE"], ["ADSL"])
        self.assertNotIn("ADVERSE", plan.target_datasets)

    def test_input_spec_is_authoritative_and_secondary_conflict_becomes_warning(self) -> None:
        study_dir = _workspace_dir("phase7_spec_conflict_dependency") / "PSY201"
        spec_dir = study_dir / "input_spec"
        sas_dir = study_dir / "legacy_code"
        spec_dir.mkdir(parents=True)
        sas_dir.mkdir(parents=True)
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
        (sas_dir / "ADTTE.sas").write_text(
            "data adtte;\n  merge adlb adae;\nrun;\n",
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADTTE"], study_dir=study_dir)

        self.assertEqual(plan.dependencies["ADTTE"], ["ADLB"])
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADTTE"].source, "input_spec_dependency")
        self.assertFalse(decisions["ADTTE"].review_required)
        self.assertNotIn("ADAE", plan.target_datasets)
        self.assertTrue(any("Dependency conflict" in warning and "ADAE" in warning for warning in plan.planning_warnings))

    def test_input_spec_consistent_secondary_evidence_stays_quiet(self) -> None:
        study_dir = _workspace_dir("phase7_spec_consistent_dependency") / "PSY201"
        spec_dir = study_dir / "input_spec"
        sas_dir = study_dir / "legacy_code"
        spec_dir.mkdir(parents=True)
        sas_dir.mkdir(parents=True)
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
        (sas_dir / "ADTTE.sas").write_text(
            "data adtte;\n  merge adlb;\nrun;\n",
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADTTE"], study_dir=study_dir)

        self.assertEqual(plan.dependencies["ADTTE"], ["ADLB"])
        self.assertFalse(any("Dependency conflict" in warning for warning in plan.planning_warnings))

    def test_input_spec_present_but_missing_target_spec_warns_before_fallback(self) -> None:
        study_dir = _workspace_dir("phase7_spec_gap_dependency") / "PSY201"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID"}]}),
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADAE"], study_dir=study_dir)

        self.assertEqual(plan.dependencies["ADAE"], ["ADSL"])
        self.assertTrue(
            any(
                "Input spec is present" in warning
                and "ADAE" in warning
                and "MVP fallback" in warning
                for warning in plan.planning_warnings
            )
        )

    def test_dependency_plan_uses_legacy_sas_dependency_evidence(self) -> None:
        study_dir = _workspace_dir("phase7_sas_dependency") / "PSY201"
        sas_dir = study_dir / "legacy_code"
        sas_dir.mkdir(parents=True)
        (sas_dir / "ADTTE.sas").write_text(
            "data adtte;\n  merge adsl adlb;\n  by usubjid;\nrun;\n",
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADTTE"], study_dir=study_dir)

        self.assertEqual(plan.dependencies["ADTTE"], ["ADSL", "ADLB"])
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADTTE"].source, "legacy_sas_dependency")
        self.assertEqual(plan.execution_batches, [["ADSL"], ["ADLB"], ["ADTTE"]])

    def test_dependency_plan_uses_define_xml_dependency_evidence(self) -> None:
        study_dir = _workspace_dir("phase7_define_dependency") / "PSY201"
        define_dir = study_dir / "input_define"
        define_dir.mkdir(parents=True)
        (define_dir / "define.xml").write_text(
            """
            <ItemGroupDef Name="ADTTE">
              <Description>ADTTE uses ADLB threshold records and ADSL population flags.</Description>
            </ItemGroupDef>
            """,
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADTTE"], study_dir=study_dir)

        self.assertEqual(plan.dependencies["ADTTE"], ["ADSL", "ADLB"])
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADTTE"].source, "define_xml_dependency")
        self.assertEqual(plan.execution_batches, [["ADSL"], ["ADLB"], ["ADTTE"]])

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
        self.assertEqual(result["audit_manifest"].metadata["requested_datasets"], ["ADSL", "ADAE"])
        self.assertEqual(result["audit_manifest"].metadata["dependency_evidence"], "phase7_mvp_fallback_adsl_foundation")
        self.assertTrue(result["audit_manifest"].metadata["dependency_decisions"][1]["review_required"])

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
        self.assertTrue((study_dir / "runs" / "run_graph_real_adsl" / "audit" / "adsl_manifest.json").exists())
        study_manifest = json.loads((study_dir / "runs" / "run_graph_real_adsl" / "audit" / "manifest.json").read_text(encoding="utf-8"))
        self.assertEqual(study_manifest["manifest_scope"], "study")
        self.assertTrue(any(artifact["path"].endswith("adsl_manifest.json") for artifact in study_manifest["artifacts"]))

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

    def test_non_ad_target_is_blocked_as_unsupported_not_completed_stub(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase7_unsupported_lb",
                "target_datasets": ["LB"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["LB"].status, "failed")
        self.assertEqual(summaries["LB"].validation_status, "unsupported_dataset")
        self.assertEqual(result["blocked_datasets"], [{"dataset": "LB", "reason": "unsupported_dataset", "blocked_by": "study_planner"}])
        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["audit_manifest"].metadata["unsupported_datasets"], ["LB"])

    def test_real_adsl_spec_error_records_route_without_stub_revision(self) -> None:
        study_dir = _workspace_dir("graph_real_adsl_spec_error") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        input_dir.mkdir(parents=True)
        (input_dir / "dm.csv").write_text("STUDYID,AGE\nS1,34\n", encoding="utf-8")
        (input_dir / "ex.csv").write_text("USUBJID,EXSTDTC\n01,2024-01-01\n", encoding="utf-8")
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_graph_real_adsl_spec_error",
                "dataset": "ADSL",
                "execution_mode": "real_adsl_minimal",
                "study_dir": str(study_dir),
                "rscript_path": str(LOCAL_RSCRIPT),
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["summary"].status, "failed")
        self.assertEqual(result["failure_records"][0].failure_type, "spec_error")
        self.assertEqual(result["failure_records"][0].recommended_route, "revise_spec")
        self.assertEqual(result.get("repair_attempts", 0), 0)
        self.assertNotEqual(result.get("draft_spec_ready"), True)

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
        self.assertEqual(summaries["ADAE"].validation_status, "blocked_by_dependency")
        self.assertEqual(result["blocked_datasets"], [{"dataset": "ADAE", "reason": "blocked_by_dependency", "blocked_by": "ADSL"}])
        self.assertEqual(result["status"], "failed")

    def test_downstream_only_request_requires_dependency_decision_when_missing(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_downstream_missing_dependency",
                "target_datasets": ["ADAE"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(set(summaries), {"ADAE"})
        self.assertEqual(summaries["ADAE"].status, "failed")
        self.assertEqual(summaries["ADAE"].validation_status, "dependency_user_action_required")
        self.assertEqual(result["blocked_datasets"], [{"dataset": "ADAE", "reason": "dependency_user_action_required", "blocked_by": "ADSL"}])
        self.assertEqual(result["foundation_datasets"], ["ADSL"])
        self.assertEqual(result["downstream_datasets"], ["ADAE"])
        self.assertEqual(result["requested_datasets"], ["ADAE"])
        self.assertEqual(result["auto_added_datasets"], ["ADSL"])
        self.assertEqual(result["runnable_datasets"], [])
        self.assertEqual(result["dataset_dependencies"]["ADAE"], ["ADSL"])
        self.assertTrue(result["dependency_action_required"])
        self.assertEqual(result["dependency_resolution"][0]["target_dataset"], "ADAE")
        self.assertEqual(result["dependency_resolution"][0]["required_dataset"], "ADSL")
        self.assertEqual(result["dependency_resolution"][0]["resolution_status"], "user_action_required")
        self.assertEqual(result["status"], "failed")

    def test_downstream_request_uses_available_dependency_artifact_without_running_it(self) -> None:
        study_dir = _workspace_dir("phase74_available_dependency") / "PSY201"
        reference_dir = study_dir / "reference_adam"
        reference_dir.mkdir(parents=True)
        (reference_dir / "adsl.csv").write_text("USUBJID,SAFFL\n01,Y\n", encoding="utf-8")
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_available_dependency",
                "target_datasets": ["ADAE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(set(summaries), {"ADAE"})
        self.assertEqual(result["runnable_datasets"], ["ADAE"])
        self.assertFalse(result["dependency_action_required"])
        self.assertEqual(result["dependency_resolution"][0]["resolution_status"], "available")
        self.assertEqual(result["dependency_resolution"][0]["artifact_source"], "reference_adam")
        self.assertEqual(result["satisfied_dependency_datasets"], ["ADSL"])
        self.assertEqual(result["status"], "completed")

    def test_study_graph_runs_llm_downstream_stubbed_when_dependency_available(self) -> None:
        study_dir = _workspace_dir("phase74_graph_llm_downstream") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_graph_llm_downstream",
                "target_datasets": ["ADAE"],
                "execution_mode": "llm_downstream_stubbed",
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(set(summaries), {"ADAE"})
        self.assertEqual(result["status"], "completed")
        self.assertEqual(summaries["ADAE"].status, "completed_stub")
        self.assertEqual(summaries["ADAE"].validation_status, "structural_stub_pass")
        self.assertTrue(summaries["ADAE"].metadata["stubbed_r_execution"])
        self.assertTrue(summaries["ADAE"].metadata["not_real_derivation"])
        self.assertEqual(summaries["ADAE"].metadata["llm_provider"], "mock")
        self.assertEqual(summaries["ADAE"].metadata["summary_status_note"], "completed_stub")
        self.assertEqual(summaries["ADAE"].output_artifact_ids, ["output_adam_psy201_run_phase74_graph_llm_downstream_adae"])
        self.assertTrue((study_dir / "runs" / "run_phase74_graph_llm_downstream" / "llm" / "adae_context.json").exists())
        self.assertTrue((study_dir / "runs" / "run_phase74_graph_llm_downstream" / "code" / "build_adae.R").exists())
        self.assertTrue((study_dir / "runs" / "run_phase74_graph_llm_downstream" / "outputs" / "adae.csv").exists())
        manifest_payload = json.loads((study_dir / "runs" / "run_phase74_graph_llm_downstream" / "audit" / "manifest.json").read_text(encoding="utf-8"))
        artifact_ids = {artifact["artifact_id"] for artifact in manifest_payload["artifacts"]}
        self.assertIn("llm_context_psy201_run_phase74_graph_llm_downstream_adae", artifact_ids)
        self.assertIn("llm_response_psy201_run_phase74_graph_llm_downstream_adae", artifact_ids)

    def test_study_graph_runs_llm_downstream_provider_mode_with_mock_config(self) -> None:
        study_dir = _workspace_dir("phase76_graph_llm_provider_mock") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase76_graph_llm_provider_mock",
                "target_datasets": ["ADAE"],
                "execution_mode": "llm_downstream_provider",
                "study_dir": str(study_dir),
                "llm_exposure": {},
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(result["status"], "completed")
        self.assertEqual(summaries["ADAE"].status, "completed_stub")
        self.assertEqual(summaries["ADAE"].metadata["llm_provider"], "mock")
        self.assertEqual(summaries["ADAE"].metadata["provider_alias"], "mock")
        self.assertTrue(summaries["ADAE"].metadata["stubbed_r_execution"])
        self.assertTrue((study_dir / "runs" / "run_phase76_graph_llm_provider_mock" / "code" / "build_adae.R").exists())

    def test_study_graph_provider_mode_fails_closed_without_external_approval(self) -> None:
        study_dir = _workspace_dir("phase76_graph_provider_fail_closed") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase76_graph_provider_fail_closed",
                "target_datasets": ["ADAE"],
                "execution_mode": "llm_downstream_provider",
                "study_dir": str(study_dir),
                "llm_exposure": {},
                "llm_provider": {
                    "provider": "deepseek",
                    "model": "deepseek-chat",
                    "api_key": "test-key",
                },
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(result["status"], "failed")
        self.assertEqual(summaries["ADAE"].status, "failed")
        self.assertEqual(summaries["ADAE"].validation_status, "not_run")
        self.assertIn("failure_adae_llm_downstream", summaries["ADAE"].failure_ids)
        self.assertEqual(summaries["ADAE"].metadata["llm_provider"], "deepseek")
        self.assertEqual(summaries["ADAE"].metadata["provider_base_url"], "https://api.deepseek.com/v1")
        self.assertIn("provider_config_failed", summaries["ADAE"].metadata["risk_flags"])

    def test_study_graph_downstream_failure_summary_carries_diagnosis(self) -> None:
        study_dir = _workspace_dir("phase78_graph_failure_diagnosis") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase78_graph_failure",
                "target_datasets": ["ADAE"],
                "execution_mode": "llm_downstream_r_sandbox",
                "study_dir": str(study_dir),
                "rscript_path": "C:/not/a/real/Rscript.exe",
                "llm_exposure": {},
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        adae = summaries["ADAE"]
        self.assertEqual(result["status"], "failed")
        self.assertEqual(adae.status, "failed")
        self.assertEqual(adae.metadata["failure_root_cause"], "r_environment_error")
        self.assertEqual(adae.metadata["recommended_route"], "human_review")
        self.assertTrue(adae.failure_ids)
        self.assertTrue((study_dir / "runs" / "run_phase78_graph_failure" / "diagnostics" / "adae_failure_report.json").exists())

    def test_cli_run_study_uses_configured_provider_boundary_with_mock(self) -> None:
        study_dir = _workspace_dir("phase76_cli_run_study") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        config_dir = study_dir / "configs"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_dir.mkdir()
        config_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        config_path = config_dir / "provider_mock.json"
        config_path.write_text(
            json.dumps(
                {
                    "llm_exposure": {},
                    "llm_provider": {
                        "provider": "mock",
                        "model": "mock-model",
                    },
                }
            ),
            encoding="utf-8",
        )

        completed = subprocess.run(
            [
                sys.executable,
                "-m",
                "adam_agent.cli",
                "run-study",
                "--study-dir",
                str(study_dir),
                "--run-id",
                "run_phase76_cli",
                "--target",
                "ADAE",
                "--config",
                str(config_path),
                "--execution-mode",
                "llm_downstream_provider",
            ],
            cwd=str(ROOT),
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(completed.returncode, 0, completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["status"], "completed")
        self.assertEqual(payload["execution_mode"], "llm_downstream_provider")
        self.assertEqual(payload["dataset_results"][0]["status"], "completed_stub")
        self.assertTrue((study_dir / "runs" / "run_phase76_cli" / "llm" / "adae_response.json").exists())

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_study_graph_runs_llm_downstream_r_sandbox_with_mock_config(self) -> None:
        study_dir = _workspace_dir("phase77_graph_llm_r_sandbox") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase77_graph_llm_r_sandbox",
                "target_datasets": ["ADAE"],
                "execution_mode": "llm_downstream_r_sandbox",
                "study_dir": str(study_dir),
                "rscript_path": str(LOCAL_RSCRIPT),
                "llm_exposure": {},
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(result["status"], "completed")
        self.assertEqual(summaries["ADAE"].status, "completed")
        self.assertEqual(summaries["ADAE"].validation_status, "pass")
        self.assertFalse(summaries["ADAE"].metadata["stubbed_r_execution"])
        self.assertTrue(summaries["ADAE"].metadata["not_real_derivation"])
        self.assertTrue((study_dir / "runs" / "run_phase77_graph_llm_r_sandbox" / "outputs" / "adae.csv").exists())

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_cli_run_study_can_execute_llm_downstream_r_sandbox(self) -> None:
        study_dir = _workspace_dir("phase77_cli_r_sandbox") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        reference_dir = study_dir / "reference_adam"
        config_dir = study_dir / "configs"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        reference_dir.mkdir()
        config_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        config_path = config_dir / "mock.json"
        config_path.write_text(
            json.dumps({"llm_exposure": {}, "llm_provider": {"provider": "mock", "model": "mock-model"}}),
            encoding="utf-8",
        )

        completed = subprocess.run(
            [
                sys.executable,
                "-m",
                "adam_agent.cli",
                "run-study",
                "--study-dir",
                str(study_dir),
                "--run-id",
                "run_phase77_cli_r",
                "--target",
                "ADAE",
                "--config",
                str(config_path),
                "--execution-mode",
                "llm_downstream_r_sandbox",
                "--rscript-path",
                str(LOCAL_RSCRIPT),
            ],
            cwd=str(ROOT),
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(completed.returncode, 0, completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["status"], "completed")
        self.assertEqual(payload["dataset_results"][0]["status"], "completed")
        self.assertEqual(payload["dataset_results"][0]["validation_status"], "pass")
        self.assertTrue((study_dir / "runs" / "run_phase77_cli_r" / "outputs" / "adae.csv").exists())

    def test_sas7bdat_dependency_artifact_is_found_but_not_usable_for_downstream_availability(self) -> None:
        study_dir = _workspace_dir("phase74_unusable_sas7bdat_dependency") / "PSY201"
        reference_dir = study_dir / "reference_adam"
        reference_dir.mkdir(parents=True)
        (reference_dir / "adsl.sas7bdat").write_text("not a real sas7bdat", encoding="utf-8")
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_unusable_sas7bdat_dependency",
                "target_datasets": ["ADAE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        resolution = result["dependency_resolution"][0]
        self.assertEqual(result["runnable_datasets"], [])
        self.assertTrue(result["dependency_action_required"])
        self.assertEqual(resolution["resolution_status"], "found_but_unusable")
        self.assertFalse(resolution["available"])
        self.assertTrue(resolution["artifact_path"].endswith("adsl.sas7bdat"))
        self.assertEqual(summaries["ADAE"].validation_status, "dependency_user_action_required")
        self.assertEqual(result["blocked_datasets"], [{"dataset": "ADAE", "reason": "dependency_user_action_required", "blocked_by": "ADSL"}])
        self.assertEqual(result["status"], "failed")

    def test_approved_dependency_generation_allows_running_dependency_once(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_approved_dependency_generation",
                "target_datasets": ["ADAE", "ADCM"],
                "approved_dependency_datasets": ["ADSL"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        datasets = [summary.dataset for summary in result["dataset_results"]]
        self.assertEqual(datasets.count("ADSL"), 1)
        self.assertEqual(set(datasets), {"ADSL", "ADAE", "ADCM"})
        self.assertEqual(result["target_datasets"], ["ADSL", "ADAE", "ADCM"])
        self.assertEqual(result["auto_added_datasets"], ["ADSL"])
        self.assertEqual(result["runnable_datasets"], ["ADSL", "ADAE", "ADCM"])
        self.assertEqual(result["dataset_dependencies"]["ADAE"], ["ADSL"])
        self.assertEqual(result["dataset_dependencies"]["ADCM"], ["ADSL"])
        self.assertFalse(result["dependency_action_required"])
        self.assertTrue(
            all(record["resolution_status"] == "approved_for_system_generation" for record in result["dependency_resolution"])
        )
        self.assertEqual(result["audit_manifest"].metadata["auto_added_datasets"], ["ADSL"])

    def test_approved_midstream_dependency_still_requires_its_missing_parent(self) -> None:
        study_dir = _workspace_dir("phase74_midstream_dependency_requires_parent") / "PSY201"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "ADTTE.json").write_text(
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
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_midstream_dependency_requires_parent",
                "target_datasets": ["ADTTE"],
                "study_dir": str(study_dir),
                "approved_dependency_datasets": ["ADLB"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADLB"].validation_status, "dependency_user_action_required")
        self.assertEqual(summaries["ADTTE"].validation_status, "dependency_user_action_required")
        self.assertEqual(result["runnable_datasets"], [])
        self.assertTrue(result["dependency_action_required"])
        self.assertIn(
            {"dataset": "ADLB", "reason": "dependency_user_action_required", "blocked_by": "ADSL"},
            result["blocked_datasets"],
        )
        self.assertIn(
            {"dataset": "ADTTE", "reason": "dependency_user_action_required", "blocked_by": "ADLB"},
            result["blocked_datasets"],
        )
        resolutions = {
            (record["target_dataset"], record["required_dataset"]): record["resolution_status"]
            for record in result["dependency_resolution"]
        }
        self.assertEqual(resolutions[("ADTTE", "ADLB")], "approved_for_system_generation")
        self.assertEqual(resolutions[("ADLB", "ADSL")], "user_action_required")

    def test_downstream_stub_failure_does_not_change_completed_adsl_status(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase7_downstream_fail",
                "target_datasets": ["ADSL", "ADAE"],
                "stub_scenarios": {"ADAE": "fail_adsl"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADSL"].status, "completed")
        self.assertEqual(summaries["ADAE"].status, "failed")
        self.assertEqual(result["status"], "failed")

    def test_midstream_dependency_failure_blocks_only_dependent_datasets(self) -> None:
        study_dir = _workspace_dir("phase7_midstream_block") / "PSY201"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "ADTTE.json").write_text(
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
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase7_midstream_block",
                "target_datasets": ["ADAE", "ADTTE"],
                "study_dir": str(study_dir),
                "approved_dependency_datasets": ["ADSL", "ADLB"],
                "stub_scenarios": {"ADLB": "fail_adsl"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADSL"].status, "completed")
        self.assertEqual(summaries["ADAE"].status, "completed")
        self.assertEqual(summaries["ADLB"].status, "failed")
        self.assertEqual(summaries["ADTTE"].status, "failed")
        self.assertEqual(summaries["ADTTE"].validation_status, "blocked_by_dependency")
        self.assertIn({"dataset": "ADTTE", "reason": "blocked_by_dependency", "blocked_by": "ADLB"}, result["blocked_datasets"])
        self.assertEqual(result["audit_manifest"].metadata["execution_batches"], [["ADSL"], ["ADAE", "ADLB"], ["ADTTE"]])
        self.assertEqual(result["status"], "failed")

    def test_study_graph_writes_dependency_plan_review_artifacts(self) -> None:
        study_dir = _workspace_dir("phase73_dependency_review") / "PSY201"
        study_dir.mkdir(parents=True)
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase73_dependency_review",
                "target_datasets": ["ADAE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        plan_path = study_dir / "runs" / "run_phase73_dependency_review" / "planning" / "dependency_plan.json"
        review_path = study_dir / "runs" / "run_phase73_dependency_review" / "planning" / "dependency_review.md"
        self.assertTrue(plan_path.exists())
        self.assertTrue(review_path.exists())
        payload = json.loads(plan_path.read_text(encoding="utf-8"))
        review_text = review_path.read_text(encoding="utf-8")

        self.assertEqual(payload["requested_datasets"], ["ADAE"])
        self.assertEqual(payload["target_datasets"], ["ADSL", "ADAE"])
        self.assertEqual(payload["auto_added_datasets"], ["ADSL"])
        self.assertEqual(payload["dataset_dependencies"]["ADAE"], ["ADSL"])
        self.assertEqual(payload["execution_batches"], [])
        self.assertEqual(payload["review_status"], "blocked")
        self.assertTrue(payload["dependency_action_required"])
        self.assertEqual(payload["dependency_resolution"][0]["resolution_status"], "user_action_required")
        self.assertIn("dependency_decisions", payload)
        self.assertIn("dependency_evidence_records", payload)
        self.assertIn("Review status: blocked", review_text)
        self.assertIn("## Execution Batches\n- None", review_text)
        self.assertIn("ADAE: dependencies=ADSL", review_text)
        self.assertIn("ADAE requires ADSL", review_text)
        self.assertIsNotNone(result["dependency_plan_artifact"].sha256)
        self.assertIsNotNone(result["dependency_review_artifact"].sha256)
        self.assertEqual(result["audit_manifest"].path, str((study_dir / "runs" / "run_phase73_dependency_review" / "audit" / "manifest.json").as_posix()))
        self.assertIsNotNone(result["audit_manifest"].sha256)
        self.assertEqual(
            result["audit_manifest"].metadata["dependency_plan_artifact_id"],
            result["dependency_plan_artifact"].artifact_id,
        )
        manifest_payload = json.loads((study_dir / "runs" / "run_phase73_dependency_review" / "audit" / "manifest.json").read_text(encoding="utf-8"))
        self.assertEqual(manifest_payload["manifest_scope"], "study")
        self.assertEqual(manifest_payload["dependency_plan_artifact_id"], result["dependency_plan_artifact"].artifact_id)
        self.assertEqual(manifest_payload["dependency_review_artifact_id"], result["dependency_review_artifact"].artifact_id)
        self.assertTrue(any(artifact["artifact_id"] == result["dependency_plan_artifact"].artifact_id for artifact in manifest_payload["artifacts"]))

    def test_dependency_review_artifacts_include_conflict_warning(self) -> None:
        study_dir = _workspace_dir("phase73_dependency_conflict_review") / "PSY201"
        spec_dir = study_dir / "input_spec"
        sas_dir = study_dir / "legacy_code"
        spec_dir.mkdir(parents=True)
        sas_dir.mkdir(parents=True)
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
        (sas_dir / "ADTTE.sas").write_text(
            "data adtte;\n  merge adlb adae;\nrun;\n",
            encoding="utf-8",
        )
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase73_dependency_conflict_review",
                "target_datasets": ["ADTTE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        plan_path = study_dir / "runs" / "run_phase73_dependency_conflict_review" / "planning" / "dependency_plan.json"
        review_path = study_dir / "runs" / "run_phase73_dependency_conflict_review" / "planning" / "dependency_review.md"
        payload = json.loads(plan_path.read_text(encoding="utf-8"))
        review_text = review_path.read_text(encoding="utf-8")

        self.assertEqual(result["dependency_review_status"], "blocked")
        self.assertEqual(payload["review_status"], "blocked")
        self.assertEqual(payload["dataset_dependencies"]["ADTTE"], ["ADLB"])
        self.assertNotIn("ADAE", payload["target_datasets"])
        self.assertTrue(any("Dependency conflict" in warning and "ADAE" in warning for warning in payload["dependency_planning_warnings"]))
        self.assertIn("Dependency conflict", review_text)
        self.assertIn("ADAE", review_text)

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
