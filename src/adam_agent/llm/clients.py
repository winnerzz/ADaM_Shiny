"""LLM client interfaces and mock implementation."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Protocol

from adam_agent.llm.model_registry import ModelRegistry
from adam_agent.schemas.llm import LLMCallRecord, LLMExposureConfig
from adam_agent.tools.artifacts import sha256_text


@dataclass
class LLMRequest:
    """Input contract for an LLM generation call."""

    prompt: str
    provider: str
    model: str
    exposure: LLMExposureConfig
    node: str
    call_id: str
    datasets_included: list[str] = field(default_factory=list)
    variables_included: list[str] = field(default_factory=list)
    sample_row_counts: dict[str, int] = field(default_factory=dict)
    full_data_included: bool = False
    prompt_artifact_id: str | None = None
    response_artifact_id: str | None = None
    redaction_policy: str = "phase4_mock_no_external_call"


@dataclass
class LLMResponse:
    """Output contract for an LLM generation call."""

    response_text: str
    call_record: LLMCallRecord


class LLMClient(Protocol):
    """Protocol implemented by mock and future provider clients."""

    def generate(self, request: LLMRequest) -> LLMResponse:
        """Generate text and return an auditable response."""


class MockLLMClient:
    """Deterministic mock client that requires no API key or network."""

    def __init__(self, registry: ModelRegistry | None = None, *, fixed_response_text: str | None = None) -> None:
        self.registry = registry or ModelRegistry()
        self.fixed_response_text = fixed_response_text

    def generate(self, request: LLMRequest) -> LLMResponse:
        model_info = self.registry.lookup(request.provider, request.model)
        response_text = self.fixed_response_text or f"[mock:{model_info.model}] deterministic response for {request.node}"
        prompt_artifact_id = request.prompt_artifact_id or f"prompt_{request.call_id}"
        response_artifact_id = request.response_artifact_id or f"response_{request.call_id}"

        call_record = LLMCallRecord(
            call_id=request.call_id,
            node=request.node,
            provider=model_info.provider,
            model=model_info.model,
            exposure_mode=request.exposure.mode,
            datasets_included=request.datasets_included,
            variables_included=request.variables_included,
            sample_row_counts=request.sample_row_counts,
            full_data_included=request.full_data_included,
            prompt_artifact_id=prompt_artifact_id,
            response_artifact_id=response_artifact_id,
            prompt_hash=f"sha256:{sha256_text(request.prompt)}",
            response_hash=f"sha256:{sha256_text(response_text)}",
            redaction_policy=request.redaction_policy,
            provider_locality=model_info.provider_locality,
        )
        return LLMResponse(response_text=response_text, call_record=call_record)
