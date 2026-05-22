"""Evidence schemas for lineage and spec drafting."""

from __future__ import annotations

from datetime import datetime
from typing import Literal

from pydantic import Field, model_validator

from adam_agent.schemas.base import NonEmptyStr, StrictBaseModel, utc_now


EvidenceSourceType = Literal[
    "existing_spec",
    "legacy_sas",
    "sap_protocol_tfl",
    "define_xml",
    "reference_adam",
    "sdtm_profile",
    "cdisc_standard",
    "company_standard",
    "built_in_template",
    "human_note",
]

EvidenceSupport = Literal[
    "target_variable",
    "label",
    "type",
    "source_domain",
    "source_variable",
    "derivation_logic",
    "derivation_candidate",
    "output_shape",
    "validation",
    "review_requirement",
]


class EvidenceRecord(StrictBaseModel):
    """Evidence supporting a variable-level lineage or draft-spec decision."""

    evidence_id: NonEmptyStr
    dataset: NonEmptyStr
    variable: str | None = None
    source_type: EvidenceSourceType
    source_ref: NonEmptyStr
    artifact_id: str | None = None
    summary: NonEmptyStr
    supports: list[EvidenceSupport] = Field(default_factory=list)
    confidence_delta: float = Field(default=0.0, ge=-1.0, le=1.0)
    created_at: datetime = Field(default_factory=utc_now)

    @model_validator(mode="after")
    def reference_adam_cannot_assert_derivation_logic_alone(self) -> "EvidenceRecord":
        if self.source_type == "reference_adam" and "derivation_logic" in self.supports:
            raise ValueError(
                "reference_adam evidence cannot directly support derivation_logic; "
                "use derivation_candidate/output_shape plus review instead"
            )
        return self
