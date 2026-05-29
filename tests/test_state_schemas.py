"""Tests for Phase 2 state schemas."""

from __future__ import annotations

import json
import sys
import unittest
from pathlib import Path

from pydantic import ValidationError

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.schemas import (
        ArtifactRef,
        DatasetState,
        EvidenceRecord,
        LLMCallRecord,
        LLMExposureConfig,
        SpecVariable,
        DatasetRunState,
        InterruptState,
        StudyRunState,
        StudyState,
    )
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.schemas import (
        ArtifactRef,
        DatasetState,
        EvidenceRecord,
        LLMCallRecord,
        LLMExposureConfig,
        SpecVariable,
        DatasetRunState,
        InterruptState,
        StudyRunState,
        StudyState,
    )


FIXTURES = ROOT / "tests" / "fixtures"


def load_fixture(name: str) -> dict:
    return json.loads((FIXTURES / name).read_text(encoding="utf-8"))


class StateSchemaTests(unittest.TestCase):
    def test_study_state_fixture_round_trips_json(self) -> None:
        state = StudyState.model_validate(load_fixture("study_state_adsl_minimal.json"))

        dumped = state.model_dump_json()
        restored = StudyState.model_validate_json(dumped)

        self.assertEqual(restored, state)
        self.assertEqual(restored.study_id, "PSY201")
        self.assertEqual(restored.target_datasets, ["ADSL"])
        self.assertEqual(restored.llm_exposure.mode, "demo_rich_context")

    def test_dataset_state_fixture_round_trips_json(self) -> None:
        state = DatasetState.model_validate(load_fixture("dataset_state_adsl_minimal.json"))

        dumped = state.model_dump_json()
        restored = DatasetState.model_validate_json(dumped)

        self.assertEqual(restored, state)
        self.assertEqual(restored.dataset, "ADSL")
        self.assertEqual(restored.input_domains, ["DM", "EX"])
        self.assertEqual(restored.repair_attempts, 0)
        self.assertEqual(restored.route_decisions[0].decision, "human_review")
        self.assertIsNotNone(restored.draft_spec)
        self.assertEqual(restored.draft_spec.variables[0].evidence_ids, ["ev_adsl_usubjid_001"])

    def test_dataset_repair_attempts_are_isolated(self) -> None:
        adsl = DatasetState(
            study_id="PSY201",
            run_id="run_001",
            dataset="ADSL",
            dataset_role="subject_level",
            repair_attempts=2,
        )
        adae = DatasetState(
            study_id="PSY201",
            run_id="run_001",
            dataset="ADAE",
            dataset_role="event_level",
            repair_attempts=0,
        )

        self.assertEqual(adsl.repair_attempts, 2)
        self.assertEqual(adae.repair_attempts, 0)

    def test_extra_fields_are_forbidden(self) -> None:
        payload = load_fixture("study_state_adsl_minimal.json")
        payload["unexpected"] = "not allowed"

        with self.assertRaises(ValidationError):
            StudyState.model_validate(payload)

    def test_reference_adam_cannot_directly_assert_derivation_logic(self) -> None:
        with self.assertRaises(ValidationError):
            EvidenceRecord(
                evidence_id="ev_bad_001",
                dataset="ADSL",
                variable="TRTSDT",
                source_type="reference_adam",
                source_ref="reference_adam.ADSL.TRTSDT",
                summary="Reference ADSL contains TRTSDT values",
                supports=["derivation_logic"],
            )

    def test_spec_variable_requires_evidence_and_approval_for_approved_status(self) -> None:
        with self.assertRaises(ValidationError):
            SpecVariable(variable="TRTSDT", derivation="Minimum EXSTDTC", evidence_ids=[])

        with self.assertRaises(ValidationError):
            SpecVariable(
                variable="USUBJID",
                derivation="Direct mapping from DM.USUBJID",
                evidence_ids=["ev_001"],
                approval_status="approved",
                approval_ids=[],
            )

    def test_demo_rich_context_requires_explicit_processed_demo_approval(self) -> None:
        with self.assertRaises(ValidationError):
            LLMExposureConfig(
                mode="demo_rich_context",
                data_classification="unknown",
                external_api_allowed=False,
            )

    def test_full_data_allowed_requires_approver(self) -> None:
        with self.assertRaises(ValidationError):
            LLMExposureConfig(mode="full_data_allowed", data_classification="processed_demo")

    def test_llm_call_policy_rejects_metadata_only_rows_and_full_data(self) -> None:
        with self.assertRaises(ValidationError):
            LLMCallRecord(
                call_id="llm_bad_001",
                node="draft_spec",
                provider="openai_compatible",
                model="test-model",
                exposure_mode="metadata_only",
                sample_row_counts={"DM": 1},
            )

        with self.assertRaises(ValidationError):
            LLMCallRecord(
                call_id="llm_bad_002",
                node="draft_spec",
                provider="openai_compatible",
                model="test-model",
                exposure_mode="demo_rich_context",
                full_data_included=True,
            )

        with self.assertRaises(ValidationError):
            LLMCallRecord(
                call_id="llm_bad_004",
                node="draft_spec",
                provider="openai-compatible",
                model="test-model",
                exposure_mode="metadata_only",
                subject_level_data_included=True,
            )

    def test_llm_call_policy_rejects_negative_sample_counts(self) -> None:
        with self.assertRaises(ValidationError):
            LLMCallRecord(
                call_id="llm_bad_003",
                node="draft_spec",
                provider="openai_compatible",
                model="test-model",
                exposure_mode="demo_rich_context",
                sample_row_counts={"DM": -1},
                prompt_artifact_id="art_prompt_001",
                response_artifact_id="art_response_001",
                redaction_policy="none_for_processed_demo",
            )

    def test_artifact_rejects_sas_program_as_input_sdtm(self) -> None:
        with self.assertRaises(ValidationError):
            ArtifactRef(
                artifact_id="art_bad_sas_001",
                kind="input_sdtm",
                path="studies/PSY201/input_sdtm/dm.sas",
                role="source",
            )

    def test_artifact_rejects_input_sdtm_output_role(self) -> None:
        with self.assertRaises(ValidationError):
            ArtifactRef(
                artifact_id="art_bad_role_001",
                kind="input_sdtm",
                path="studies/PSY201/input_sdtm/dm.csv",
                role="output",
            )

    def test_graph_native_study_run_state_round_trips_two_dataset_interrupts(self) -> None:
        state = StudyRunState(
            study_id="PSY201",
            run_id="run_lg2_state",
            status="needs_review",
            requested_datasets=["ADAE", "ADCM"],
            target_datasets=["ADAE", "ADCM"],
            current_interrupt=InterruptState(name="dependency_review", reason="Review low-confidence plan."),
            datasets={
                "ADAE": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_state",
                    dataset="ADAE",
                    status="needs_review",
                    current_interrupt=InterruptState(
                        name="draft_spec_review",
                        dataset="ADAE",
                        reason="No input spec was supplied.",
                    ),
                    risk_flags=["missing_input_spec"],
                ),
                "ADCM": DatasetRunState(
                    study_id="PSY201",
                    run_id="run_lg2_state",
                    dataset="ADCM",
                    status="needs_review",
                    current_interrupt=InterruptState(
                        name="code_review",
                        dataset="ADCM",
                        reason="Generated R code requires human approval.",
                    ),
                    risk_flags=["generated_code_pending_review"],
                ),
            },
            risk_flags=["dependency_review_required"],
            evidence_bundle_id="evb_run_lg2_state",
            reference_queries=[{"tool": "search_cdisc_reference", "query": "ADAE"}],
            agent_decisions=[{"agent": "dependency_agent", "decision": "review_required"}],
        )

        restored = StudyRunState.model_validate_json(state.model_dump_json())

        self.assertEqual(restored, state)
        self.assertEqual(restored.current_interrupt.name, "dependency_review")
        self.assertEqual(restored.datasets["ADAE"].current_interrupt.name, "draft_spec_review")
        self.assertEqual(restored.datasets["ADCM"].current_interrupt.name, "code_review")

    def test_graph_native_dataset_key_must_match_dataset_name(self) -> None:
        with self.assertRaises(ValidationError):
            StudyRunState(
                study_id="PSY201",
                run_id="run_lg2_bad_key",
                datasets={
                    "ADAE": DatasetRunState(study_id="PSY201", run_id="run_lg2_bad_key", dataset="ADSL")
                },
            )


if __name__ == "__main__":
    unittest.main()
