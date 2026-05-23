"""Build a starter ADSL spec for the Phase 5 minimal loop."""

from __future__ import annotations

from dataclasses import dataclass

from adam_agent.schemas.approval import ApprovalRecord
from adam_agent.schemas.evidence import EvidenceRecord
from adam_agent.schemas.specs import SpecDocument, SpecVariable
from adam_agent.tools.artifacts import sha256_text
from adam_agent.tools.sdtm_reader import DatasetProfile


@dataclass
class StarterAdslSpecResult:
    """Draft spec plus the evidence records used to create it."""

    draft_spec: SpecDocument
    evidence_records: list[EvidenceRecord]


DIRECT_DM_COLUMNS = ["STUDYID", "USUBJID", "AGE", "SEX", "RACE", "ARM", "ACTARM"]


def build_starter_adsl_spec(dm_profile: DatasetProfile, ex_profile: DatasetProfile) -> StarterAdslSpecResult:
    """Create a reviewable ADSL starter spec from DM/EX profiles.

    The output is intentionally a draft. Treatment dates and SAFFL are MVP
    candidates, not production rules.
    """

    dm_columns = set(dm_profile.columns)
    ex_columns = set(ex_profile.columns)
    if "USUBJID" not in dm_columns:
        raise ValueError("ADSL starter spec requires DM.USUBJID")

    evidence_records: list[EvidenceRecord] = []
    variables: list[SpecVariable] = []

    for column in DIRECT_DM_COLUMNS:
        if column not in dm_columns:
            continue
        evidence = _evidence(
            variable=column,
            source_ref=f"DM.{column}",
            summary=f"DM.{column} exists and can be carried into the ADSL starter spec.",
            supports=["target_variable", "source_variable", "derivation_candidate"],
            confidence_delta=0.4,
        )
        evidence_records.append(evidence)
        variables.append(
            SpecVariable(
                variable=column,
                label=_label_for_direct_dm(column),
                type=_type_for_direct_dm(column),
                source_domains=["DM"],
                source_variables=[column],
                derivation=f"Direct mapping from DM.{column}",
                evidence_ids=[evidence.evidence_id],
                confidence=0.95 if column in {"STUDYID", "USUBJID"} else 0.86,
                review_required=False if column in {"STUDYID", "USUBJID"} else True,
                review_reasons=[] if column in {"STUDYID", "USUBJID"} else ["Direct demographic/treatment carry-forward should be checked against study metadata."],
                risk_level="low" if column in {"STUDYID", "USUBJID"} else "medium",
            )
        )

    if {"USUBJID", "EXSTDTC"}.issubset(ex_columns):
        evidence = _evidence(
            variable="TRTSDT",
            source_ref="EX.EXSTDTC",
            summary="EX.EXSTDTC exists and can support a first-exposure-date candidate.",
            supports=["source_variable", "derivation_candidate", "review_requirement"],
            confidence_delta=0.25,
        )
        evidence_records.append(evidence)
        variables.append(
            SpecVariable(
                variable="TRTSDT",
                label="Date of First Exposure to Treatment",
                type="date",
                source_domains=["EX"],
                source_variables=["EXSTDTC"],
                derivation="MVP candidate: minimum non-missing EX.EXSTDTC per USUBJID.",
                evidence_ids=[evidence.evidence_id],
                confidence=0.72,
                review_required=True,
                review_reasons=["Treatment date derivation is study-specific and requires spec/SAP/legacy/human support."],
                risk_level="high",
                assumptions=["This is a Phase 5 starter candidate, not a production treatment-date rule."],
            )
        )

    if "USUBJID" in ex_columns and ("EXENDTC" in ex_columns or "EXSTDTC" in ex_columns):
        source_var = "EXENDTC" if "EXENDTC" in ex_columns else "EXSTDTC"
        evidence = _evidence(
            variable="TRTEDT",
            source_ref=f"EX.{source_var}",
            summary=f"EX.{source_var} exists and can support a last-exposure-date candidate.",
            supports=["source_variable", "derivation_candidate", "review_requirement"],
            confidence_delta=0.2,
        )
        evidence_records.append(evidence)
        variables.append(
            SpecVariable(
                variable="TRTEDT",
                label="Date of Last Exposure to Treatment",
                type="date",
                source_domains=["EX"],
                source_variables=[source_var],
                derivation=f"MVP candidate: maximum non-missing EX.{source_var} per USUBJID.",
                evidence_ids=[evidence.evidence_id],
                confidence=0.68,
                review_required=True,
                review_reasons=["Treatment end date derivation is study-specific and requires spec/SAP/legacy/human support."],
                risk_level="high",
                assumptions=["This is a Phase 5 starter candidate, not a production treatment-date rule."],
            )
        )

    if "USUBJID" in ex_columns:
        evidence = _evidence(
            variable="SAFFL",
            source_ref="EX.USUBJID",
            summary="EX records exist and can support a demo/MVP exposure-presence safety flag candidate.",
            supports=["derivation_candidate", "review_requirement"],
            confidence_delta=0.1,
        )
        evidence_records.append(evidence)
        variables.append(
            SpecVariable(
                variable="SAFFL",
                label="Safety Population Flag",
                type="character",
                source_domains=["EX"],
                source_variables=["USUBJID"],
                derivation="Demo/MVP candidate only: Y if the subject has any EX record, otherwise N.",
                evidence_ids=[evidence.evidence_id],
                confidence=0.55,
                review_required=True,
                review_reasons=["Safety population logic is study-specific and must not default to exposure presence in production."],
                risk_level="high",
                assumptions=["This rule is for demo/MVP execution only and is not production-approved."],
            )
        )

    return StarterAdslSpecResult(
        draft_spec=SpecDocument(dataset="ADSL", status="draft", variables=variables),
        evidence_records=evidence_records,
    )


def create_demo_approved_spec(
    draft_spec: SpecDocument,
    *,
    approved_by: str = "demo_config",
    approval_id: str = "appr_adsl_demo_only_no_review",
) -> tuple[SpecDocument, ApprovalRecord]:
    """Create a demo-only approved spec while preserving audit caveats."""

    draft_hash = f"sha256:{sha256_text(draft_spec.model_dump_json())}"
    approval = ApprovalRecord(
        approval_id=approval_id,
        dataset=draft_spec.dataset,
        reviewed_variables=[variable.variable for variable in draft_spec.variables],
        approval_mode="demo_only_no_review",
        approved_by=approved_by,
        source_draft_spec_hash=draft_hash,
        unresolved_assumptions=[
            assumption
            for variable in draft_spec.variables
            for assumption in variable.assumptions
        ],
        notes="Demo-only approval for Phase 5 execution. Starter rules are not production-approved.",
    )
    approved_variables = [
        variable.model_copy(
            update={
                "approval_ids": [approval.approval_id],
                "approval_status": "approved",
            }
        )
        for variable in draft_spec.variables
    ]
    return (
        SpecDocument(dataset=draft_spec.dataset, status="approved", variables=approved_variables),
        approval,
    )


def _evidence(
    *,
    variable: str,
    source_ref: str,
    summary: str,
    supports: list[str],
    confidence_delta: float,
) -> EvidenceRecord:
    return EvidenceRecord(
        evidence_id=f"ev_adsl_{variable.lower()}_profile",
        dataset="ADSL",
        variable=variable,
        source_type="sdtm_profile",
        source_ref=source_ref,
        summary=summary,
        supports=supports,
        confidence_delta=confidence_delta,
    )


def _label_for_direct_dm(column: str) -> str:
    labels = {
        "STUDYID": "Study Identifier",
        "USUBJID": "Unique Subject Identifier",
        "AGE": "Age",
        "SEX": "Sex",
        "RACE": "Race",
        "ARM": "Planned Arm",
        "ACTARM": "Actual Arm",
    }
    return labels.get(column, column)


def _type_for_direct_dm(column: str) -> str:
    if column == "AGE":
        return "numeric"
    return "character"
