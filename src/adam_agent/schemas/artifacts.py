"""Artifact reference schemas."""

from __future__ import annotations

from datetime import datetime
from typing import Any, Literal

from pydantic import Field, model_validator

from adam_agent.schemas.base import NonEmptyStr, Sha256, StrictBaseModel, utc_now


ArtifactKind = Literal[
    "input_sdtm",
    "input_define",
    "input_spec",
    "reference_adam",
    "legacy_code",
    "draft_spec",
    "approved_spec",
    "lineage",
    "generated_code",
    "output_adam",
    "validation_report",
    "compare_report",
    "audit_manifest",
    "llm_prompt",
    "llm_response",
    "tool_log",
]

ArtifactRole = Literal["source", "reference", "intermediate", "output", "audit"]


class ArtifactRef(StrictBaseModel):
    """Path/hash reference to an input, output, report, prompt, or log file."""

    artifact_id: NonEmptyStr
    kind: ArtifactKind
    path: NonEmptyStr
    sha256: Sha256 | None = None
    dataset: str | None = None
    format: str | None = None
    role: ArtifactRole
    created_at: datetime = Field(default_factory=utc_now)
    metadata: dict[str, Any] = Field(default_factory=dict)

    @model_validator(mode="after")
    def validate_kind_role_and_sas_boundary(self) -> "ArtifactRef":
        source_kinds = {"input_sdtm", "input_define", "input_spec", "legacy_code"}
        reference_kinds = {"reference_adam"}
        output_kinds = {
            "draft_spec",
            "approved_spec",
            "lineage",
            "generated_code",
            "output_adam",
            "validation_report",
            "compare_report",
        }
        audit_kinds = {"audit_manifest", "llm_prompt", "llm_response", "tool_log"}

        if self.kind in source_kinds and self.role not in {"source", "intermediate"}:
            raise ValueError(f"{self.kind} artifacts must use source/intermediate role")
        if self.kind in reference_kinds and self.role != "reference":
            raise ValueError(f"{self.kind} artifacts must use reference role")
        if self.kind in output_kinds and self.role not in {"output", "intermediate"}:
            raise ValueError(f"{self.kind} artifacts must use output/intermediate role")
        if self.kind in audit_kinds and self.role != "audit":
            raise ValueError(f"{self.kind} artifacts must use audit role")

        path_lower = self.path.lower()
        if path_lower.endswith(".sas") and self.kind != "legacy_code":
            raise ValueError(".sas programs must be legacy_code artifacts in the MVP")
        if self.kind == "legacy_code" and not path_lower.endswith((".sas", ".r", ".txt", ".md")):
            raise ValueError("legacy_code artifacts should point to code/text evidence")
        if self.kind in {"input_sdtm", "reference_adam"} and path_lower.endswith(".sas"):
            raise ValueError(".sas programs cannot be data artifacts")
        return self
