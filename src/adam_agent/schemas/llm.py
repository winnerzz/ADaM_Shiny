"""LLM exposure and call audit schemas."""

from __future__ import annotations

from datetime import datetime
from typing import Literal

from pydantic import Field, model_validator

from adam_agent.schemas.base import NonEmptyStr, Sha256, StrictBaseModel, utc_now


LLMExposureMode = Literal["metadata_only", "demo_rich_context", "full_data_allowed"]
DataClassification = Literal["unknown", "real_clinical", "processed_demo", "synthetic", "public"]
ProviderLocality = Literal["external_api", "local_model", "unknown"]


class LLMExposureConfig(StrictBaseModel):
    """Run-level policy for what may be sent to an LLM provider."""

    mode: LLMExposureMode = "metadata_only"
    data_classification: DataClassification = "unknown"
    external_api_allowed: bool = False
    approved_by: str | None = None
    approval_note: str = ""
    sample_rows_per_dataset: int = Field(default=0, ge=0)
    max_unique_values_per_variable: int = Field(default=30, ge=0)
    max_subject_summaries: int = Field(default=0, ge=0)
    include_reference_rows: bool = False
    created_at: datetime = Field(default_factory=utc_now)

    @model_validator(mode="after")
    def validate_exposure_mode(self) -> "LLMExposureConfig":
        if self.mode == "demo_rich_context":
            if self.data_classification != "processed_demo" or not self.external_api_allowed:
                raise ValueError(
                    "demo_rich_context requires data_classification='processed_demo' "
                    "and external_api_allowed=True"
                )
        if self.mode == "full_data_allowed" and not self.approved_by:
            raise ValueError("full_data_allowed requires explicit approved_by")
        return self


class LLMCallRecord(StrictBaseModel):
    """Per-call audit record for an LLM request/response."""

    call_id: NonEmptyStr
    node: NonEmptyStr
    provider: NonEmptyStr
    model: NonEmptyStr
    exposure_mode: LLMExposureMode
    datasets_included: list[str] = Field(default_factory=list)
    variables_included: list[str] = Field(default_factory=list)
    sample_row_counts: dict[str, int] = Field(default_factory=dict)
    full_data_included: bool = False
    prompt_artifact_id: str | None = None
    response_artifact_id: str | None = None
    prompt_hash: Sha256 | None = None
    response_hash: Sha256 | None = None
    redaction_policy: str | None = None
    provider_locality: ProviderLocality = "unknown"
    created_at: datetime = Field(default_factory=utc_now)

    @model_validator(mode="after")
    def validate_call_policy(self) -> "LLMCallRecord":
        if any(count < 0 for count in self.sample_row_counts.values()):
            raise ValueError("sample_row_counts cannot contain negative values")
        if self.exposure_mode == "metadata_only":
            if self.full_data_included:
                raise ValueError("metadata_only calls cannot include full data")
            if any(count > 0 for count in self.sample_row_counts.values()):
                raise ValueError("metadata_only calls cannot include sample rows")
        if self.full_data_included and self.exposure_mode != "full_data_allowed":
            raise ValueError("full_data_included requires full_data_allowed exposure mode")
        if self.exposure_mode in {"demo_rich_context", "full_data_allowed"}:
            if not self.redaction_policy:
                raise ValueError("rich/full-data LLM calls require redaction_policy")
            if not self.prompt_artifact_id or not self.response_artifact_id:
                raise ValueError("rich/full-data LLM calls require prompt and response artifact ids")
        return self
