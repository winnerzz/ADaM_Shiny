"""LLM client interfaces, mock implementation, and provider adapters."""

from __future__ import annotations

import json
import os
from dataclasses import dataclass, field
from typing import Any, Callable, Protocol
from urllib import error, request

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


class LLMClientError(RuntimeError):
    """Base error for LLM client failures."""


class LLMClientConfigError(LLMClientError):
    """Raised when provider configuration is incomplete or unsafe."""


class LLMProviderResponseError(LLMClientError):
    """Raised when the provider response cannot be used."""


@dataclass(frozen=True)
class OpenAICompatibleConfig:
    """Configuration for OpenAI-compatible chat-completions providers."""

    base_url: str
    api_key: str | None = None
    api_key_env: str = "OPENAI_API_KEY"
    timeout_seconds: float = 60.0
    provider_locality: str = "external_api"
    requires_api_key: bool = True
    organization: str | None = None
    extra_headers: dict[str, str] = field(default_factory=dict)

    @classmethod
    def from_env(
        cls,
        *,
        base_url_env: str = "OPENAI_BASE_URL",
        api_key_env: str = "OPENAI_API_KEY",
        default_base_url: str = "https://api.openai.com/v1",
        provider_locality: str = "external_api",
        requires_api_key: bool = True,
    ) -> "OpenAICompatibleConfig":
        """Create config from environment variables without hard-coding models."""

        return cls(
            base_url=os.environ.get(base_url_env, default_base_url),
            api_key=os.environ.get(api_key_env),
            api_key_env=api_key_env,
            provider_locality=provider_locality,
            requires_api_key=requires_api_key,
        )


JsonTransport = Callable[[str, dict[str, str], dict[str, Any], float], dict[str, Any]]


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


class OpenAICompatibleLLMClient:
    """Client for OpenAI-compatible `/chat/completions` APIs.

    This adapter intentionally accepts any model id string supplied by the run
    configuration. Provider/model allowlists live outside the graph so model
    upgrades do not require business-logic edits.
    """

    def __init__(
        self,
        config: OpenAICompatibleConfig,
        *,
        transport: JsonTransport | None = None,
    ) -> None:
        self.config = config
        self.transport = transport or _post_json

    def generate(self, request: LLMRequest) -> LLMResponse:
        self._validate_request(request)
        url = _chat_completions_url(self.config.base_url)
        headers = self._headers()
        payload = {
            "model": request.model,
            "messages": [{"role": "user", "content": request.prompt}],
        }
        response_payload = self.transport(url, headers, payload, self.config.timeout_seconds)
        response_text = _extract_chat_completion_text(response_payload)

        prompt_artifact_id = request.prompt_artifact_id or f"prompt_{request.call_id}"
        response_artifact_id = request.response_artifact_id or f"response_{request.call_id}"
        call_record = LLMCallRecord(
            call_id=request.call_id,
            node=request.node,
            provider=request.provider,
            model=request.model,
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
            provider_locality=self.config.provider_locality,
        )
        return LLMResponse(response_text=response_text, call_record=call_record)

    def _validate_request(self, request: LLMRequest) -> None:
        if self.config.provider_locality == "external_api" and not request.exposure.external_api_allowed:
            raise LLMClientConfigError("External LLM calls require external_api_allowed=True in LLMExposureConfig.")
        if self.config.requires_api_key and not self._api_key():
            raise LLMClientConfigError(f"Missing API key for provider; set {self.config.api_key_env} or pass api_key.")

    def _headers(self) -> dict[str, str]:
        headers = {
            "Content-Type": "application/json",
            **self.config.extra_headers,
        }
        api_key = self._api_key()
        if api_key:
            headers["Authorization"] = f"Bearer {api_key}"
        if self.config.organization:
            headers["OpenAI-Organization"] = self.config.organization
        return headers

    def _api_key(self) -> str | None:
        return self.config.api_key or os.environ.get(self.config.api_key_env)


def _chat_completions_url(base_url: str) -> str:
    root = base_url.rstrip("/")
    if root.endswith("/chat/completions"):
        return root
    return f"{root}/chat/completions"


def _extract_chat_completion_text(payload: dict[str, Any]) -> str:
    try:
        choice = payload["choices"][0]
    except (KeyError, IndexError, TypeError) as exc:
        raise LLMProviderResponseError("Provider response does not contain choices[0].") from exc
    message = choice.get("message") if isinstance(choice, dict) else None
    if isinstance(message, dict) and isinstance(message.get("content"), str):
        return message["content"]
    if isinstance(choice, dict) and isinstance(choice.get("text"), str):
        return choice["text"]
    raise LLMProviderResponseError("Provider response does not contain message.content text.")


def _post_json(url: str, headers: dict[str, str], payload: dict[str, Any], timeout_seconds: float) -> dict[str, Any]:
    body = json.dumps(payload).encode("utf-8")
    http_request = request.Request(url, data=body, headers=headers, method="POST")
    try:
        with request.urlopen(http_request, timeout=timeout_seconds) as response:
            response_text = response.read().decode("utf-8")
    except error.HTTPError as exc:
        message = exc.read().decode("utf-8", errors="replace")
        raise LLMProviderResponseError(f"Provider HTTP error {exc.code}: {message}") from exc
    except error.URLError as exc:
        raise LLMProviderResponseError(f"Provider request failed: {exc.reason}") from exc
    try:
        return json.loads(response_text)
    except json.JSONDecodeError as exc:
        raise LLMProviderResponseError("Provider response was not valid JSON.") from exc
