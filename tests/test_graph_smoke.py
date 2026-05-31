"""Smoke tests for the Phase 3 LangGraph skeleton."""

from __future__ import annotations

import json
import subprocess
import sys
import unittest
import uuid
from pathlib import Path
from unittest.mock import patch

from langgraph.checkpoint.memory import InMemorySaver

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"
LOCAL_RSCRIPT = Path(r"C:\Dev\R-4.5.2\bin\Rscript.exe")

try:
    from adam_agent.graph.dependencies import plan_dataset_dependencies
    from adam_agent.graph.dataset_graph import compile_dataset_graph
    from adam_agent.graph.dataset_graph import compile_legacy_stub_dataset_graph
    from adam_agent.graph.dataset_graph import prepare_dataset
    from adam_agent.graph.dataset_graph import route_after_product_context
    from adam_agent.graph.gateway import GraphGateway
    from adam_agent.graph.routing import route_after_sandbox
    from adam_agent.graph.study_graph import compile_study_graph
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.graph_state import HumanCommand
    from adam_agent.tools.artifacts import sha256_file
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.graph.dependencies import plan_dataset_dependencies
    from adam_agent.graph.dataset_graph import compile_dataset_graph
    from adam_agent.graph.dataset_graph import compile_legacy_stub_dataset_graph
    from adam_agent.graph.dataset_graph import prepare_dataset
    from adam_agent.graph.dataset_graph import route_after_product_context
    from adam_agent.graph.gateway import GraphGateway
    from adam_agent.graph.routing import route_after_sandbox
    from adam_agent.graph.study_graph import compile_study_graph
    from adam_agent.schemas.artifacts import ArtifactRef
    from adam_agent.schemas.graph_state import HumanCommand
    from adam_agent.tools.artifacts import sha256_file


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class GraphSmokeTests(unittest.TestCase):
    def test_dependency_plan_does_not_invent_adsl_without_evidence(self) -> None:
        plan = plan_dataset_dependencies(["ADAE", "ADCM"])

        self.assertEqual(plan.requested_datasets, ["ADAE", "ADCM"])
        self.assertEqual(plan.target_datasets, ["ADAE", "ADCM"])
        self.assertEqual(plan.auto_added_datasets, [])
        self.assertEqual(plan.foundation_datasets, ["ADAE", "ADCM"])
        self.assertEqual(plan.downstream_datasets, [])
        self.assertEqual(plan.unsupported_datasets, [])
        self.assertEqual(plan.dependencies["ADAE"], [])
        self.assertEqual(plan.dependencies["ADCM"], [])
        self.assertEqual(plan.dependency_graph, {})
        self.assertEqual(plan.evidence, "no_dependency_evidence")
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADAE"].source, "no_dependency_evidence")
        self.assertTrue(decisions["ADAE"].review_required)
        self.assertLess(decisions["ADAE"].confidence, 0.5)

    def test_dependency_plan_marks_unknown_adam_dependencies_low_confidence_review_required(self) -> None:
        plan = plan_dataset_dependencies(["ADTTE"])

        self.assertEqual(plan.target_datasets, ["ADTTE"])
        self.assertEqual(plan.dependencies["ADTTE"], [])
        decisions = {decision.dataset: decision for decision in plan.decisions}
        self.assertEqual(decisions["ADTTE"].source, "no_dependency_evidence")
        self.assertEqual(decisions["ADTTE"].confidence, 0.35)
        self.assertTrue(decisions["ADTTE"].review_required)
        self.assertIn("will not invent an ADSL dependency", decisions["ADTTE"].reason)

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
        self.assertEqual(plan.dependencies["ADLB"], [])
        self.assertEqual(plan.execution_batches, [["ADSL", "ADLB"], ["ADTTE"]])
        self.assertEqual(plan.evidence, "user_evidence")
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

    def test_input_spec_present_but_missing_target_spec_warns_without_adsl_fallback(self) -> None:
        study_dir = _workspace_dir("phase7_spec_gap_dependency") / "PSY201"
        spec_dir = study_dir / "input_spec"
        spec_dir.mkdir(parents=True)
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID"}]}),
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADAE"], study_dir=study_dir)

        self.assertEqual(plan.target_datasets, ["ADAE"])
        self.assertEqual(plan.dependencies["ADAE"], [])
        self.assertTrue(
            any(
                "Input spec is present" in warning
                and "ADAE" in warning
                and "not imposing a default ADSL dependency" in warning
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
        self.assertEqual(plan.execution_batches, [["ADSL", "ADLB"], ["ADTTE"]])

    def test_dependency_plan_ignores_sas_libref_that_starts_with_ad(self) -> None:
        study_dir = _workspace_dir("phase7_ignore_ad_libref") / "PSY201"
        sas_dir = study_dir / "legacy_code"
        sas_dir.mkdir(parents=True)
        (sas_dir / "ADAE.sas").write_text(
            """
            libname adout_hw "Primary/HW/AD/Output";
            proc sql;
              create table dm as
              select usubjid from addata.addm;
            quit;
            data adout_hw.adae;
              set ae_t3;
            run;
            data addata.adae;
              set adout_hw.adae;
            run;
            """,
            encoding="utf-8",
        )

        plan = plan_dataset_dependencies(["ADAE"], study_dir=study_dir)

        self.assertEqual(plan.dependencies["ADAE"], ["ADDM"])
        self.assertFalse(any(record.dependency == "ADOUT_HW" for record in plan.evidence_records))

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
        self.assertEqual(plan.execution_batches, [["ADSL", "ADLB"], ["ADTTE"]])

    def test_study_graph_runs_foundation_then_downstream_stub_datasets(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_smoke",
                "target_datasets": ["ADSL", "ADAE"],
                "execution_mode": "stub",
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
        self.assertEqual(result["audit_manifest"].metadata["dependency_evidence"], "no_dependency_evidence")
        self.assertTrue(result["audit_manifest"].metadata["dependency_decisions"][1]["review_required"])

    def test_study_graph_runs_adsl_through_unified_llm_adam_flow(self) -> None:
        study_dir = _workspace_dir("graph_unified_adsl") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        input_dir.mkdir(parents=True)
        spec_dir.mkdir()
        for folder in ["reference_adam", "input_define", "legacy_code", "runs"]:
            (study_dir / folder).mkdir()
        (input_dir / "dm.csv").write_text(
            "STUDYID,USUBJID,AGE,SEX,ARM\nS1,01,34,F,Test Drug\nS1,02,41,M,Placebo\n",
            encoding="utf-8",
        )
        (input_dir / "ex.csv").write_text(
            "USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n",
            encoding="utf-8",
        )
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_graph_unified_adsl",
                "target_datasets": ["ADSL"],
                "execution_mode": "llm_downstream_stubbed",
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        adsl_summary = summaries["ADSL"]
        self.assertEqual(result["status"], "completed")
        self.assertEqual(adsl_summary.status, "completed_stub")
        self.assertEqual(adsl_summary.validation_status, "structural_stub_pass")
        self.assertTrue(adsl_summary.metadata["stubbed_r_execution"])
        self.assertEqual(adsl_summary.metadata["llm_provider"], "mock")
        self.assertEqual(adsl_summary.output_artifact_ids, ["output_adam_psy201_run_graph_unified_adsl_adsl"])
        self.assertTrue((study_dir / "runs" / "run_graph_unified_adsl" / "llm" / "adsl_context.json").exists())
        self.assertTrue((study_dir / "runs" / "run_graph_unified_adsl" / "llm" / "adsl_response.json").exists())
        self.assertTrue((study_dir / "runs" / "run_graph_unified_adsl" / "code" / "build_adsl.R").exists())
        self.assertTrue((study_dir / "runs" / "run_graph_unified_adsl" / "outputs" / "adsl.csv").exists())
        self.assertFalse((study_dir / "runs" / "run_graph_unified_adsl" / "audit" / "adsl_manifest.json").exists())

    def test_dataset_state_isolation_across_stub_runs(self) -> None:
        dataset_graph = compile_legacy_stub_dataset_graph()

        adsl = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_isolation",
                "dataset": "ADSL",
                "execution_mode": "stub",
                "stub_scenario": "success",
                "audit_artifacts": [],
            }
        )
        adae = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_isolation",
                "dataset": "ADAE",
                "execution_mode": "stub",
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

    def test_dataset_graph_missing_execution_mode_fails_closed_not_completed_stub(self) -> None:
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_missing_execution_mode",
                "dataset": "ADAE",
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["failure_type"], "input_error")
        self.assertIn("requires an explicit execution_mode", result["real_run_error"])
        self.assertEqual(result["summary"].status, "failed")

    def test_dataset_graph_unknown_execution_mode_fails_closed_not_completed_stub(self) -> None:
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_unknown_execution_mode",
                "dataset": "ADAE",
                "execution_mode": "legacy_auto_magic",
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["failure_type"], "input_error")
        self.assertIn("got legacy_auto_magic", result["real_run_error"])
        self.assertNotEqual(result["summary"].validation_status, "passed_stub")

    def test_product_dataset_graph_rejects_stub_mode_without_legacy_compiler(self) -> None:
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_product_graph_rejects_stub_mode",
                "dataset": "ADAE",
                "execution_mode": "stub",
                "legacy_stub_graph_enabled": True,
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["failure_type"], "input_error")
        self.assertIn("legacy/test dataset graph compiler", result["real_run_error"])
        self.assertNotEqual(result["summary"].status, "completed")

    def test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes(self) -> None:
        graph = compile_dataset_graph().get_graph()
        edges = {(edge.source, edge.target, edge.data) for edge in graph.edges}
        node_names = {node.id for node in graph.nodes.values()}

        for product_node in {"draft_spec_agent", "generate_r_code_agent", "execute_approved_code"}:
            self.assertIn((product_node, "summarize_dataset", None), edges)
            self.assertNotIn((product_node, "draft_lineage_stub", None), edges)

        for legacy_node in {
            "draft_lineage_stub",
            "draft_spec_stub",
            "generate_code_stub",
            "run_sandbox_stub",
            "repair_code_stub",
            "revise_spec_stub",
        }:
            self.assertNotIn(legacy_node, node_names)
        self.assertNotIn(("prepare_dataset", "draft_lineage_stub", "stub_chain"), edges)

    def test_legacy_stub_dataset_graph_contains_only_explicit_stub_chain(self) -> None:
        graph = compile_legacy_stub_dataset_graph().get_graph()
        edges = {(edge.source, edge.target, edge.data) for edge in graph.edges}

        self.assertIn(("prepare_dataset", "draft_lineage_stub", "stub_chain"), edges)

    def test_dataset_graph_non_legacy_modes_never_route_to_stub_chain(self) -> None:
        mode_cases = [
            ("graph_product_prepare", {"spec_source": "input_spec"}, "summarize"),
            ("graph_product_prepare", {"spec_source": "missing_input_spec"}, "draft_spec_agent"),
            ("graph_product_generate_code", {}, "generate_r_code_agent"),
            ("graph_product_execute", {}, "execute_approved_code"),
            ("llm_downstream_stubbed", {}, "summarize"),
            ("llm_downstream_provider", {}, "summarize"),
            ("llm_downstream_r_sandbox", {}, "summarize"),
            ("real_adsl_minimal", {}, "summarize"),
        ]

        for execution_mode, extra, expected_route in mode_cases:
            with self.subTest(execution_mode=execution_mode, extra=extra):
                state = {
                    "study_id": "PSY201",
                    "run_id": "run_non_legacy_route_guard",
                    "dataset": "ADAE",
                    "execution_mode": execution_mode,
                    "status": "needs_review",
                    "audit_artifacts": [],
                    **extra,
                }

                route = route_after_product_context(state)

                self.assertEqual(route, expected_route)
                self.assertNotEqual(route, "stub_chain")

    def test_graph_product_execute_prepare_does_not_run_r_before_execute_node(self) -> None:
        with patch("adam_agent.graph.dataset_graph.execute_approved_r_code") as execute:
            result = prepare_dataset(
                {
                    "study_id": "PSY201",
                    "run_id": "run_graph_product_execute_prepare",
                    "dataset": "ADAE",
                    "execution_mode": "graph_product_execute",
                    "audit_artifacts": [],
                }
            )

        execute.assert_not_called()
        self.assertEqual(result["status"], "running")
        self.assertEqual(result["sandbox_runs"], 0)

    def test_dataset_graph_product_prepare_uses_input_spec_without_stub_code(self) -> None:
        study_dir = _workspace_dir("lg2_dataset_product_prepare_spec") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "ads_adae_full.csv").write_text(
            "Dataset,Variable,Label,Type,Source,Derivation\n"
            "ADAE,USUBJID,Unique Subject Identifier,Copied,SDTM.AE.USUBJID,Copied from source\n",
            encoding="utf-8",
        )
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_lg2_product_prepare_spec",
                "dataset": "ADAE",
                "execution_mode": "graph_product_prepare",
                "study_dir": str(study_dir),
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "needs_review")
        self.assertEqual(result["current_interrupt"], "code_generation_ready")
        self.assertEqual(result["spec_source"], "input_spec")
        self.assertFalse(result["draft_spec_required"])
        self.assertNotIn("generated_code", result)
        self.assertTrue((study_dir / "runs" / "run_lg2_product_prepare_spec" / "llm" / "adae_context.json").exists())
        summary = result["summary"]
        self.assertEqual(summary.status, "needs_review")
        self.assertEqual(summary.metadata["next_action"], "generate_code")
        self.assertEqual(summary.metadata["spec_source"], "input_spec")

    def test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review(self) -> None:
        study_dir = _workspace_dir("lg2_dataset_product_prepare_missing_spec") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        legacy_dir = study_dir / "legacy_code"
        sdtm_dir.mkdir(parents=True)
        legacy_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (legacy_dir / "adae.sas").write_text("data adae; set ae; run;\n", encoding="utf-8")
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_lg2_product_prepare_missing_spec",
                "dataset": "ADAE",
                "execution_mode": "graph_product_prepare",
                "study_dir": str(study_dir),
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "needs_review")
        self.assertEqual(result["current_interrupt"], "draft_spec_review")
        self.assertEqual(result["spec_source"], "draft_spec")
        self.assertTrue(result["draft_spec_required"])
        self.assertTrue(result["draft_spec_path"].endswith("specs/adae_draft_spec.json"))
        self.assertTrue(result["draft_spec_prompt_path"].endswith("llm/adae_draft_spec_prompt.txt"))
        self.assertTrue(result["draft_spec_response_path"].endswith("llm/adae_draft_spec_response.json"))
        self.assertTrue(result["draft_spec_variables"])
        self.assertNotIn("generated_code", result)
        draft_spec = json.loads(Path(result["draft_spec_path"]).read_text(encoding="utf-8"))
        self.assertEqual(draft_spec["dataset"], "ADAE")
        self.assertEqual(draft_spec["status"], "draft")
        self.assertIn("input_fingerprint", draft_spec)
        self.assertEqual(
            draft_spec["reference_adam_policy"],
            "Reference ADaM is compare/output-shape evidence only, not derivation authority.",
        )
        summary = result["summary"]
        self.assertEqual(summary.metadata["next_action"], "review_draft_spec")
        self.assertEqual(summary.metadata["spec_source"], "draft_spec")

    def test_dataset_graph_product_generate_code_uses_input_spec_and_stops_for_review(self) -> None:
        study_dir = _workspace_dir("lg2_dataset_product_generate_code_spec") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_lg2_product_generate_code_spec",
                "dataset": "ADAE",
                "execution_mode": "graph_product_generate_code",
                "study_dir": str(study_dir),
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "llm_exposure": {},
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "needs_review")
        self.assertEqual(result["current_interrupt"], "code_review")
        self.assertEqual(result["spec_source"], "input_spec")
        self.assertIn("generated_code", result)
        self.assertEqual(result["sandbox_runs"], 0)
        self.assertTrue((study_dir / "runs" / "run_lg2_product_generate_code_spec" / "code" / "build_adae.R").exists())
        self.assertTrue(
            (study_dir / "runs" / "run_lg2_product_generate_code_spec" / "llm" / "adae_parsed_response.json").exists()
        )
        self.assertTrue(
            (study_dir / "runs" / "run_lg2_product_generate_code_spec" / "static_checks" / "adae_static_check.json").exists()
        )
        summary = result["summary"]
        self.assertEqual(summary.status, "needs_review")
        self.assertEqual(summary.metadata["next_action"], "review_code")
        self.assertEqual(summary.metadata["spec_source"], "input_spec")
        self.assertTrue(summary.metadata["code_path"].endswith("code/build_adae.R"))
        self.assertEqual(summary.validation_status, "not_run")

    def test_study_graph_batch_path_preserves_agent_decisions(self) -> None:
        study_dir = _workspace_dir("lg2_study_graph_agent_decisions") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_lg2_study_graph_agent_decisions",
                "target_datasets": ["ADAE"],
                "execution_mode": "graph_product_generate_code",
                "study_dir": str(study_dir),
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "llm_exposure": {},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        agents = [item["agent"] for item in result["agent_decisions"]]
        self.assertIn("evidence_agent", agents)
        self.assertIn("code_agent", agents)
        self.assertIn("static_review_agent", agents)
        self.assertIn("static_check_limited_scope", result["risk_flags"])
        self.assertEqual(result["audit_manifest"].metadata["agent_decisions"][0]["agent"], "evidence_agent")
        self.assertEqual(result["agent_audit_summary"]["summary_writer"]["agent"], "audit_agent")
        self.assertIn("ADAE", result["agent_audit_summary"]["datasets"])
        self.assertIn(
            "llm_context_psy201_run_lg2_study_graph_agent_decisions_adae",
            result["agent_audit_summary"]["datasets"]["ADAE"]["artifact_ids"],
        )
        self.assertEqual(result["audit_manifest"].metadata["agent_audit_summary"]["summary_type"], "agent_audit_summary")
        self.assertTrue((study_dir / "runs" / "run_lg2_study_graph_agent_decisions" / "audit" / "agent_summary.json").exists())

    def test_dataset_graph_product_generate_code_requires_approved_spec(self) -> None:
        study_dir = _workspace_dir("lg2_dataset_product_generate_code_missing_spec") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        sdtm_dir.mkdir(parents=True)
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_lg2_product_generate_code_missing_spec",
                "dataset": "ADAE",
                "execution_mode": "graph_product_generate_code",
                "study_dir": str(study_dir),
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "llm_exposure": {},
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["failure_type"], "spec_error")
        self.assertEqual(result["current_interrupt"], "draft_spec_review")
        self.assertEqual(result["next_action"], "review_draft_spec")
        self.assertNotIn("generated_code", result)
        self.assertFalse((study_dir / "runs" / "run_lg2_product_generate_code_missing_spec" / "code").exists())
        summary = result["summary"]
        self.assertEqual(summary.status, "failed")
        self.assertEqual(summary.metadata["next_action"], "review_draft_spec")

    def test_dataset_graph_product_generate_code_uses_approved_draft_spec(self) -> None:
        study_dir = _workspace_dir("lg2_dataset_product_generate_code_approved_draft") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "runs" / "run_lg2_product_generate_code_approved_draft" / "specs"
        approved_dir = study_dir / "runs" / "run_lg2_product_generate_code_approved_draft" / "approved_specs"
        reviews_dir = study_dir / "runs" / "run_lg2_product_generate_code_approved_draft" / "reviews"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        approved_dir.mkdir(parents=True)
        reviews_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        from adam_agent.graph.workflow_state import input_fingerprint

        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps(
                {
                    "dataset": "ADAE",
                    "status": "draft",
                    "input_fingerprint": fingerprint,
                    "variables": [{"variable": "AETERM", "source_domains": ["AE"]}],
                }
            ),
            encoding="utf-8",
        )
        approved_path = approved_dir / "adae_approved_spec.json"
        approved_path.write_text(
            json.dumps(
                {
                    "dataset": "ADAE",
                    "status": "approved_draft",
                    "input_fingerprint": fingerprint,
                    "variables": [{"variable": "AETERM", "source_domains": ["AE"]}],
                }
            ),
            encoding="utf-8",
        )
        review_path = reviews_dir / "adae_draft_spec_review.json"
        review_path.write_text(
            json.dumps(
                {
                    "decision": "approve",
                    "approved": True,
                    "input_fingerprint": fingerprint,
                    "approved_spec_sha256": f"sha256:{sha256_file(approved_path)}",
                }
            ),
            encoding="utf-8",
        )
        approved_sha = f"sha256:{sha256_file(approved_path)}"
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_product_generate_code_approved_draft",
            dataset="ADAE",
            draft_spec_path=draft_path,
            input_fingerprint_payload=fingerprint,
        )
        gateway.record_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_product_generate_code_approved_draft",
            dataset="ADAE",
            command=HumanCommand(
                interrupt="draft_spec_review",
                action="approve",
                dataset="ADAE",
                reviewer="tester",
            ),
            review_path=review_path,
            draft_spec_path=draft_path,
            approved_spec_path=approved_path,
            approved_spec_sha256=approved_sha,
            input_fingerprint_payload=fingerprint,
        )
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_lg2_product_generate_code_approved_draft",
                "dataset": "ADAE",
                "execution_mode": "graph_product_generate_code",
                "study_dir": str(study_dir),
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "llm_exposure": {},
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "needs_review")
        self.assertEqual(result["current_interrupt"], "code_review")
        self.assertEqual(result["spec_source"], "approved_draft_spec")
        self.assertEqual(result["approved_spec_path"], str(approved_path.as_posix()))
        self.assertIn("generated_code", result)
        summary = result["summary"]
        self.assertEqual(summary.metadata["spec_source"], "approved_draft_spec")
        self.assertEqual(summary.metadata["next_action"], "review_code")
        self.assertEqual(summary.validation_status, "not_run")

    def test_dataset_graph_product_generate_code_rejects_stale_approved_draft_spec(self) -> None:
        study_dir = _workspace_dir("lg2_dataset_product_generate_code_stale_draft") / "PSY201"
        sdtm_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "runs" / "run_lg2_product_generate_code_stale_draft" / "specs"
        approved_dir = study_dir / "runs" / "run_lg2_product_generate_code_stale_draft" / "approved_specs"
        reviews_dir = study_dir / "runs" / "run_lg2_product_generate_code_stale_draft" / "reviews"
        sdtm_dir.mkdir(parents=True)
        spec_dir.mkdir(parents=True)
        approved_dir.mkdir(parents=True)
        reviews_dir.mkdir()
        (sdtm_dir / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        from adam_agent.graph.workflow_state import input_fingerprint

        fingerprint = input_fingerprint(study_dir)
        draft_path = spec_dir / "adae_draft_spec.json"
        draft_path.write_text(
            json.dumps({"dataset": "ADAE", "status": "draft", "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        approved_path = approved_dir / "adae_approved_spec.json"
        approved_path.write_text(
            json.dumps({"dataset": "ADAE", "status": "approved_draft", "input_fingerprint": fingerprint}),
            encoding="utf-8",
        )
        review_path = reviews_dir / "adae_draft_spec_review.json"
        review_path.write_text(
            json.dumps(
                {
                    "decision": "approve",
                    "approved": True,
                    "input_fingerprint": fingerprint,
                    "approved_spec_sha256": f"sha256:{sha256_file(approved_path)}",
                }
            ),
            encoding="utf-8",
        )
        approved_sha = f"sha256:{sha256_file(approved_path)}"
        gateway = GraphGateway()
        gateway.record_draft_spec_generation(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_product_generate_code_stale_draft",
            dataset="ADAE",
            draft_spec_path=draft_path,
            input_fingerprint_payload=fingerprint,
        )
        gateway.record_draft_spec_review(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_lg2_product_generate_code_stale_draft",
            dataset="ADAE",
            command=HumanCommand(
                interrupt="draft_spec_review",
                action="approve",
                dataset="ADAE",
                reviewer="tester",
            ),
            review_path=review_path,
            draft_spec_path=draft_path,
            approved_spec_path=approved_path,
            approved_spec_sha256=approved_sha,
            input_fingerprint_payload=fingerprint,
        )
        (sdtm_dir / "cm.csv").write_text("USUBJID,CMTRT\n01,MED\n", encoding="utf-8")
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_lg2_product_generate_code_stale_draft",
                "dataset": "ADAE",
                "execution_mode": "graph_product_generate_code",
                "study_dir": str(study_dir),
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "llm_exposure": {},
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["failure_type"], "spec_error")
        self.assertEqual(result["next_action"], "regenerate_draft_spec")
        self.assertIn("stale", result["real_run_error"])
        self.assertFalse((study_dir / "runs" / "run_lg2_product_generate_code_stale_draft" / "code").exists())

    def test_adsl_unified_llm_flow_returns_structured_failure_for_missing_study_dir(self) -> None:
        study_dir = _workspace_dir("graph_unified_adsl_missing") / "PSY201"
        study_dir.mkdir(parents=True)
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_graph_unified_adsl_missing",
                "dataset": "ADSL",
                "execution_mode": "llm_downstream_stubbed",
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["real_run_error"], "execution_mode=llm_downstream_stubbed requires study_dir")

    def test_real_adsl_minimal_mode_is_retired_from_dataset_graph(self) -> None:
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_graph_retired_adsl_template",
                "dataset": "ADSL",
                "execution_mode": "real_adsl_minimal",
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["summary"].status, "failed")
        self.assertEqual(result["summary"].validation_status, "not_run_stub")
        self.assertIn("retired from DatasetGraph", result["real_run_error"])
        self.assertFalse(result.get("real_run_completed"))

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

    def test_adsl_unified_llm_flow_failure_records_route_without_stub_revision(self) -> None:
        study_dir = _workspace_dir("graph_unified_adsl_failure") / "PSY201"
        input_dir = study_dir / "input_sdtm"
        spec_dir = study_dir / "input_spec"
        input_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (input_dir / "dm.csv").write_text("STUDYID,USUBJID\nS1,01\n", encoding="utf-8")
        (spec_dir / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        dataset_graph = compile_dataset_graph()

        result = dataset_graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_graph_unified_adsl_failure",
                "dataset": "ADSL",
                "execution_mode": "llm_downstream_r_sandbox",
                "study_dir": str(study_dir),
                "rscript_path": "C:/not/a/real/Rscript.exe",
                "llm_exposure": {},
                "llm_provider": {"provider": "mock", "model": "mock-model"},
                "audit_artifacts": [],
            }
        )

        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["summary"].status, "failed")
        self.assertEqual(result["summary"].validation_status, "fail")
        self.assertEqual(result["failure_records"][0].root_cause, "r_environment_error")
        self.assertEqual(result["failure_records"][0].recommended_route, "human_review")
        self.assertEqual(result.get("repair_attempts", 0), 0)
        self.assertNotEqual(result.get("draft_spec_ready"), True)

    def test_adsl_failure_blocks_downstream_without_running_it(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase3_blocked",
                "target_datasets": ["ADSL", "ADAE"],
                "execution_mode": "stub",
                "stub_scenarios": {"ADSL": "fail_adsl"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADSL"].status, "failed")
        self.assertEqual(summaries["ADAE"].status, "completed")
        self.assertEqual(result["blocked_datasets"], [])
        self.assertEqual(result["status"], "failed")

    def test_downstream_only_request_does_not_invent_dependency_when_missing(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_downstream_missing_dependency",
                "target_datasets": ["ADAE"],
                "execution_mode": "stub",
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(set(summaries), {"ADAE"})
        self.assertEqual(summaries["ADAE"].status, "completed")
        self.assertEqual(result["blocked_datasets"], [])
        self.assertEqual(result["foundation_datasets"], ["ADAE"])
        self.assertEqual(result["downstream_datasets"], [])
        self.assertEqual(result["requested_datasets"], ["ADAE"])
        self.assertEqual(result["auto_added_datasets"], [])
        self.assertEqual(result["runnable_datasets"], ["ADAE"])
        self.assertEqual(result["dataset_dependencies"]["ADAE"], [])
        self.assertFalse(result["dependency_action_required"])
        self.assertEqual(result["dependency_resolution"], [])
        self.assertEqual(result["status"], "completed")

    def test_study_graph_missing_execution_mode_fails_closed_not_completed_stub(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_study_missing_execution_mode",
                "target_datasets": ["ADAE"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADAE"].status, "failed")
        self.assertEqual(result["status"], "failed")
        self.assertNotEqual(summaries["ADAE"].validation_status, "passed_stub")

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
                "execution_mode": "stub",
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
        self.assertEqual(result["dependency_resolution"], [])
        self.assertEqual(result["satisfied_dependency_datasets"], [])
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
        self.assertIn("static_check_psy201_run_phase74_graph_llm_downstream_adae", artifact_ids)

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

    @unittest.skipUnless(LOCAL_RSCRIPT.exists(), "local Rscript is not available")
    def test_study_graph_runs_adsl_llm_r_sandbox_with_mock_config(self) -> None:
        study_dir = _workspace_dir("phase8_adsl_unified_r_sandbox") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        (input_sdtm / "dm.csv").write_text(
            "STUDYID,USUBJID,SUBJID,ARM,ACTARM\nS1,01,1001,Placebo,Placebo\n",
            encoding="utf-8",
        )
        (input_sdtm / "ex.csv").write_text("USUBJID,EXSTDTC,EXENDTC\n01,2024-01-01,2024-01-05\n", encoding="utf-8")
        (input_spec / "adsl.json").write_text(
            json.dumps({"dataset": "ADSL", "variables": [{"variable": "USUBJID", "source_domains": ["DM"]}]}),
            encoding="utf-8",
        )
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase8_adsl_unified_r",
                "target_datasets": ["ADSL"],
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
        self.assertEqual(summaries["ADSL"].status, "completed")
        self.assertEqual(summaries["ADSL"].validation_status, "pass")
        self.assertFalse(summaries["ADSL"].metadata["stubbed_r_execution"])
        self.assertTrue(summaries["ADSL"].metadata["not_real_derivation"])
        self.assertTrue((study_dir / "runs" / "run_phase8_adsl_unified_r" / "code" / "build_adsl.R").exists())
        self.assertTrue((study_dir / "runs" / "run_phase8_adsl_unified_r" / "outputs" / "adsl.csv").exists())
        self.assertFalse((study_dir / "runs" / "run_phase8_adsl_unified_r" / "audit" / "adsl_manifest.json").exists())

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

    def test_cli_run_study_requires_explicit_execution_mode_with_mock_config(self) -> None:
        study_dir = _workspace_dir("phase8_cli_no_implicit_stub") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        config_dir = study_dir / "configs"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        config_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
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
                "run_cli_no_implicit_stub",
                "--target",
                "ADAE",
                "--config",
                str(config_path),
            ],
            cwd=str(ROOT),
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(completed.returncode, 1)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["status"], "failed")
        self.assertIn("requires an explicit --execution-mode", payload["error"])
        self.assertIn("--execution-mode stub", payload["error"])
        self.assertFalse((study_dir / "runs" / "run_cli_no_implicit_stub").exists())

    def test_cli_run_study_rejects_unknown_execution_mode_before_graph(self) -> None:
        study_dir = _workspace_dir("phase8_cli_unknown_execution_mode") / "PSY201"
        input_sdtm = study_dir / "input_sdtm"
        input_spec = study_dir / "input_spec"
        config_dir = study_dir / "configs"
        input_sdtm.mkdir(parents=True)
        input_spec.mkdir()
        config_dir.mkdir()
        (input_sdtm / "ae.csv").write_text("USUBJID,AETERM\n01,HEADACHE\n", encoding="utf-8")
        (input_spec / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "AETERM", "source_domains": ["AE"]}]}),
            encoding="utf-8",
        )
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
                "run_cli_unknown_execution_mode",
                "--target",
                "ADAE",
                "--config",
                str(config_path),
                "--execution-mode",
                "legacy_auto_magic",
            ],
            cwd=str(ROOT),
            capture_output=True,
            text=True,
            check=False,
        )

        self.assertEqual(completed.returncode, 1)
        payload = json.loads(completed.stdout)
        self.assertEqual(payload["status"], "failed")
        self.assertIn("Unsupported --execution-mode", payload["error"])
        self.assertIn("legacy_auto_magic", payload["error"])
        self.assertIn("Allowed run-study modes", payload["error"])
        self.assertFalse((study_dir / "runs" / "run_cli_unknown_execution_mode").exists())

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

    def test_reference_sas7bdat_dependency_artifact_does_not_satisfy_runtime_dependency(self) -> None:
        study_dir = _workspace_dir("phase74_reference_sas7bdat_not_runtime_dependency") / "PSY201"
        reference_dir = study_dir / "reference_adam"
        spec_dir = study_dir / "input_spec"
        reference_dir.mkdir(parents=True)
        spec_dir.mkdir()
        (reference_dir / "adsl.sas7bdat").write_text("not a real sas7bdat", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_reference_sas7bdat_not_runtime_dependency",
                "target_datasets": ["ADAE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        resolution = result["dependency_resolution"][0]
        self.assertEqual(result["runnable_datasets"], [])
        self.assertTrue(result["dependency_action_required"])
        self.assertEqual(resolution["resolution_status"], "available")
        self.assertTrue(resolution["available"])
        self.assertTrue(resolution["artifact_path"].endswith("adsl.sas7bdat"))
        self.assertEqual(resolution["artifact_source"], "reference_adam")
        self.assertEqual(result["blocked_datasets"][0]["dataset"], "ADAE")
        self.assertEqual(result["blocked_datasets"][0]["blocked_by"], "ADSL")
        self.assertEqual(result["status"], "failed")

    def test_run_output_dependency_artifact_wins_over_reference_adam(self) -> None:
        study_dir = _workspace_dir("phase74_run_output_dependency_priority") / "PSY201"
        reference_dir = study_dir / "reference_adam"
        spec_dir = study_dir / "input_spec"
        output_dir = study_dir / "runs" / "run_phase74_run_output_dependency_priority" / "outputs"
        reference_dir.mkdir(parents=True)
        spec_dir.mkdir()
        output_dir.mkdir(parents=True)
        (reference_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n99,2099-01-01\n", encoding="utf-8")
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        _seed_completed_run_output(
            study_dir=study_dir,
            run_id="run_phase74_run_output_dependency_priority",
            dataset="ADSL",
            output_path=output_dir / "adsl.csv",
        )
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_run_output_dependency_priority",
                "target_datasets": ["ADAE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        resolution = result["dependency_resolution"][0]
        self.assertEqual(resolution["artifact_source"], "run_output")
        self.assertTrue(resolution["artifact_path"].endswith("runs/run_phase74_run_output_dependency_priority/outputs/adsl.csv"))
        self.assertEqual(result["runnable_datasets"], ["ADAE"])
        self.assertFalse(result["dependency_action_required"])

    def test_unbacked_run_output_dependency_is_not_usable(self) -> None:
        study_dir = _workspace_dir("phase74_unbacked_run_output_dependency") / "PSY201"
        spec_dir = study_dir / "input_spec"
        output_dir = study_dir / "runs" / "run_phase74_unbacked_run_output_dependency" / "outputs"
        spec_dir.mkdir(parents=True)
        output_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_phase74_unbacked_run_output_dependency",
            target_datasets=["ADSL"],
        )

        result = compile_study_graph().invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_unbacked_run_output_dependency",
                "target_datasets": ["ADAE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        resolution = result["dependency_resolution"][0]
        self.assertEqual(resolution["resolution_status"], "found_but_unusable")
        self.assertEqual(resolution["artifact_source"], "run_output")
        self.assertEqual(result["runnable_datasets"], [])

    def test_terminal_failure_run_output_dependency_does_not_satisfy_downstream(self) -> None:
        study_dir = _workspace_dir("phase74_failed_run_output_dependency") / "PSY201"
        spec_dir = study_dir / "input_spec"
        output_dir = study_dir / "runs" / "run_phase74_failed_run_output_dependency" / "outputs"
        spec_dir.mkdir(parents=True)
        output_dir.mkdir(parents=True)
        (output_dir / "adsl.csv").write_text("USUBJID,TRTSDT\n01,2024-01-01\n", encoding="utf-8")
        (spec_dir / "adae.json").write_text(
            json.dumps({"dataset": "ADAE", "variables": [{"variable": "TRTSDT", "source_domains": ["ADSL"]}]}),
            encoding="utf-8",
        )
        gateway = GraphGateway()
        gateway.start_dependency_plan(
            study_dir=study_dir,
            study_id="PSY201",
            run_id="run_phase74_failed_run_output_dependency",
            target_datasets=["ADSL"],
        )
        state = gateway.load_graph_state(
            study_dir=study_dir,
            run_id="run_phase74_failed_run_output_dependency",
        ).model_copy(deep=True)
        state.datasets["ADSL"].status = "terminal_failure"
        state.datasets["ADSL"].execution_state.update(
            {
                "terminal_failure": True,
                "partial_output_usable": False,
                "output_path": str((output_dir / "adsl.csv").as_posix()),
            }
        )
        gateway._persist_graph_state(study_dir, state, node="test_seed_terminal_failure_output")

        result = compile_study_graph().invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_failed_run_output_dependency",
                "target_datasets": ["ADAE"],
                "study_dir": str(study_dir),
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        resolution = result["dependency_resolution"][0]
        self.assertEqual(resolution["resolution_status"], "found_but_unusable")
        self.assertEqual(resolution["artifact_source"], "run_output")
        self.assertIn("terminal_failure or failed", resolution["reason"])
        self.assertEqual(result["blocked_datasets"][0]["dataset"], "ADAE")
        self.assertEqual(result["runnable_datasets"], [])

    def test_approved_dependency_generation_allows_running_dependency_once(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase74_approved_dependency_generation",
                "target_datasets": ["ADAE", "ADCM"],
                "execution_mode": "stub",
                "approved_dependency_datasets": ["ADSL"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        datasets = [summary.dataset for summary in result["dataset_results"]]
        self.assertEqual(datasets.count("ADSL"), 0)
        self.assertEqual(set(datasets), {"ADAE", "ADCM"})
        self.assertEqual(result["target_datasets"], ["ADAE", "ADCM"])
        self.assertEqual(result["auto_added_datasets"], [])
        self.assertEqual(result["runnable_datasets"], ["ADAE", "ADCM"])
        self.assertEqual(result["dataset_dependencies"]["ADAE"], [])
        self.assertEqual(result["dataset_dependencies"]["ADCM"], [])
        self.assertFalse(result["dependency_action_required"])
        self.assertEqual(result["dependency_resolution"], [])
        self.assertEqual(result["audit_manifest"].metadata["auto_added_datasets"], [])

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
                "execution_mode": "stub",
                "study_dir": str(study_dir),
                "approved_dependency_datasets": ["ADLB"],
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADLB"].status, "completed")
        self.assertEqual(summaries["ADTTE"].status, "completed")
        self.assertEqual(result["runnable_datasets"], ["ADLB", "ADTTE"])
        self.assertFalse(result["dependency_action_required"])
        self.assertEqual(result["blocked_datasets"], [])
        resolutions = {
            (record["target_dataset"], record["required_dataset"]): record["resolution_status"]
            for record in result["dependency_resolution"]
        }
        self.assertEqual(resolutions[("ADTTE", "ADLB")], "approved_for_system_generation")

    def test_downstream_stub_failure_does_not_change_completed_adsl_status(self) -> None:
        graph = compile_study_graph()

        result = graph.invoke(
            {
                "study_id": "PSY201",
                "run_id": "run_phase7_downstream_fail",
                "target_datasets": ["ADSL", "ADAE"],
                "execution_mode": "stub",
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
                "execution_mode": "stub",
                "study_dir": str(study_dir),
                "approved_dependency_datasets": ["ADSL", "ADLB"],
                "stub_scenarios": {"ADLB": "fail_adsl"},
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )

        summaries = {summary.dataset: summary for summary in result["dataset_results"]}
        self.assertEqual(summaries["ADAE"].status, "completed")
        self.assertEqual(summaries["ADLB"].status, "failed")
        self.assertEqual(summaries["ADTTE"].status, "failed")
        self.assertEqual(summaries["ADTTE"].validation_status, "blocked_by_dependency")
        self.assertIn({"dataset": "ADTTE", "reason": "blocked_by_dependency", "blocked_by": "ADLB"}, result["blocked_datasets"])
        self.assertEqual(result["audit_manifest"].metadata["execution_batches"], [["ADAE", "ADLB"], ["ADTTE"]])
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
        self.assertEqual(payload["target_datasets"], ["ADAE"])
        self.assertEqual(payload["auto_added_datasets"], [])
        self.assertEqual(payload["dataset_dependencies"]["ADAE"], [])
        self.assertEqual(payload["execution_batches"], [["ADAE"]])
        self.assertEqual(payload["review_status"], "review_required")
        self.assertFalse(payload["dependency_action_required"])
        self.assertEqual(payload["dependency_resolution"], [])
        self.assertIn("dependency_decisions", payload)
        self.assertIn("dependency_evidence_records", payload)
        self.assertIn("Review status: review_required", review_text)
        self.assertIn("ADAE: None", review_text)
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
                "execution_mode": "stub",
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
                "execution_mode": "stub",
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


def _seed_completed_run_output(*, study_dir: Path, run_id: str, dataset: str, output_path: Path) -> None:
    target = dataset.strip().upper()
    gateway = GraphGateway()
    gateway.start_dependency_plan(
        study_dir=study_dir,
        study_id="PSY201",
        run_id=run_id,
        target_datasets=[target],
    )
    state = gateway.load_graph_state(study_dir=study_dir, run_id=run_id).model_copy(deep=True)
    state.datasets[target].status = "completed"
    state.datasets[target].execution_state.update(
        {
            "terminal_failure": False,
            "partial_output_usable": True,
            "output_path": str(output_path.as_posix()),
        }
    )
    state.datasets[target].artifacts.append(
        ArtifactRef(
            artifact_id=f"output_adam_psy201_{run_id}_{target.lower()}",
            kind="output_adam",
            path=str(output_path.as_posix()),
            sha256=f"sha256:{sha256_file(output_path)}",
            dataset=target,
            format=output_path.suffix.lower().lstrip(".") or "csv",
            role="output",
        )
    )
    gateway._persist_graph_state(study_dir, state, node="test_seed_completed_run_output")


if __name__ == "__main__":
    unittest.main()
