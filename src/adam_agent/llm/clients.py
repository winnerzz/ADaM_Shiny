"""LLM client interfaces, mock implementation, and provider adapters."""

from __future__ import annotations

import json
import os
from dataclasses import dataclass, field
from http.client import RemoteDisconnected
from typing import Any, Callable, Protocol
from urllib import error, request

from adam_agent.llm.model_registry import ModelInfo, ModelRegistry
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
    system_prompt: str | None = None
    datasets_included: list[str] = field(default_factory=list)
    variables_included: list[str] = field(default_factory=list)
    sample_row_counts: dict[str, int] = field(default_factory=dict)
    full_data_included: bool = False
    subject_level_data_included: bool = False
    max_tokens: int | None = None
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
    provider_alias: str = "openai-compatible"
    transport: str = "openai-compatible"
    allow_custom_base_url: bool = False
    custom_base_url_approved_by: str | None = None
    known_base_urls: tuple[str, ...] = ("https://api.openai.com/v1",)
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
            provider_alias="openai",
            known_base_urls=(default_base_url,),
        )


JsonTransport = Callable[[str, dict[str, str], dict[str, Any], float], dict[str, Any]]


@dataclass(frozen=True)
class AnthropicMessagesConfig:
    """Configuration for Anthropic's official Messages API."""

    base_url: str = "https://api.anthropic.com"
    api_key: str | None = None
    api_key_env: str = "ANTHROPIC_API_KEY"
    timeout_seconds: float = 60.0
    provider_locality: str = "external_api"
    requires_api_key: bool = True
    provider_alias: str = "anthropic"
    transport: str = "anthropic-messages"
    anthropic_version: str = "2023-06-01"
    max_tokens: int = 4096
    allow_custom_base_url: bool = False
    custom_base_url_approved_by: str | None = None
    known_base_urls: tuple[str, ...] = ("https://api.anthropic.com",)
    extra_headers: dict[str, str] = field(default_factory=dict)


@dataclass(frozen=True)
class LLMProviderConfig:
    """Run-level provider selection used by the LLM client factory."""

    provider: str = "mock"
    model: str = "mock-model"
    base_url: str | None = None
    api_key: str | None = None
    api_key_env: str | None = None
    timeout_seconds: float = 60.0
    max_tokens: int = 4096
    allow_custom_base_url: bool = False
    custom_base_url_approved_by: str | None = None
    anthropic_version: str = "2023-06-01"


class MockLLMClient:
    """Deterministic mock client that requires no API key or network."""

    def __init__(self, registry: ModelRegistry | None = None, *, fixed_response_text: str | None = None) -> None:
        self.registry = registry or ModelRegistry()
        self.fixed_response_text = fixed_response_text

    def generate(self, request: LLMRequest) -> LLMResponse:
        model_info = self._model_info(request)
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
            subject_level_data_included=request.subject_level_data_included,
            prompt_artifact_id=prompt_artifact_id,
            response_artifact_id=response_artifact_id,
            prompt_hash=f"sha256:{sha256_text(request.prompt)}",
            response_hash=f"sha256:{sha256_text(response_text)}",
            redaction_policy=request.redaction_policy,
            provider_locality=model_info.provider_locality,
            provider_alias="mock",
            transport="mock",
        )
        return LLMResponse(response_text=response_text, call_record=call_record)

    def _model_info(self, request: LLMRequest) -> ModelInfo:
        try:
            return self.registry.lookup(request.provider, request.model)
        except KeyError:
            if request.provider.strip().lower() != "mock":
                raise
            return ModelInfo(
                provider="mock",
                model=request.model or "mock-model",
                provider_locality="local_model",
                requires_api_key=False,
                implemented=True,
            )


def build_llm_client(
    config: LLMProviderConfig,
    *,
    transport: JsonTransport | None = None,
) -> LLMClient:
    """Build an LLM client from run configuration without touching graph logic."""

    provider = config.provider.strip().lower()
    if provider == "mock":
        return MockLLMClient()
    if provider in {"openai", "openai-compatible", "deepseek", "qwen"}:
        return OpenAICompatibleLLMClient(
            _openai_compatible_config_from_provider(config, provider),
            transport=transport,
        )
    if provider in {"anthropic", "claude", "anthropic-messages"}:
        return AnthropicMessagesLLMClient(
            _anthropic_config_from_provider(config, provider),
            transport=transport,
        )
    raise LLMClientConfigError(f"Unsupported LLM provider: {config.provider}")


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
        messages = []
        if request.system_prompt:
            messages.append({"role": "system", "content": request.system_prompt})
        messages.append({"role": "user", "content": request.prompt})
        payload = {
            "model": request.model,
            "messages": messages,
        }
        if request.max_tokens is not None:
            payload["max_tokens"] = request.max_tokens
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
            subject_level_data_included=request.subject_level_data_included,
            prompt_artifact_id=prompt_artifact_id,
            response_artifact_id=response_artifact_id,
            prompt_hash=f"sha256:{sha256_text(request.prompt)}",
            response_hash=f"sha256:{sha256_text(response_text)}",
            redaction_policy=request.redaction_policy,
            provider_locality=self.config.provider_locality,
            provider_alias=self.config.provider_alias,
            transport=self.config.transport,
            provider_base_url=_redacted_base_url(self.config.base_url),
            external_relay=_is_external_relay(self.config.base_url, self.config.known_base_urls),
            risk_flags=_provider_risk_flags(
                base_url=self.config.base_url,
                known_base_urls=self.config.known_base_urls,
                allow_custom_base_url=self.config.allow_custom_base_url,
                subject_level_data_included=request.subject_level_data_included,
            ),
        )
        return LLMResponse(response_text=response_text, call_record=call_record)

    def _validate_request(self, request: LLMRequest) -> None:
        if self.config.provider_locality == "external_api" and not request.exposure.external_api_allowed:
            raise LLMClientConfigError("External LLM calls require external_api_allowed=True in LLMExposureConfig.")
        if self.config.requires_api_key and not self._api_key():
            raise LLMClientConfigError(f"Missing API key for provider; set {self.config.api_key_env} or pass api_key.")
        _validate_base_url_policy(
            base_url=self.config.base_url,
            known_base_urls=self.config.known_base_urls,
            allow_custom_base_url=self.config.allow_custom_base_url,
            approved_by=self.config.custom_base_url_approved_by,
        )

    def _headers(self) -> dict[str, str]:
        headers = {
            "Content-Type": "application/json",
            "Accept": "application/json",
            "User-Agent": "ADaM-Agent-Studio/0.1",
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


class AnthropicMessagesLLMClient:
    """Client for Anthropic's official Messages API."""

    def __init__(
        self,
        config: AnthropicMessagesConfig,
        *,
        transport: JsonTransport | None = None,
    ) -> None:
        self.config = config
        self.transport = transport or _post_json

    def generate(self, request: LLMRequest) -> LLMResponse:
        self._validate_request(request)
        url = _anthropic_messages_url(self.config.base_url)
        payload: dict[str, Any] = {
            "model": request.model,
            "max_tokens": request.max_tokens or self.config.max_tokens,
            "messages": [{"role": "user", "content": request.prompt}],
        }
        if request.system_prompt:
            payload["system"] = request.system_prompt
        response_payload = self.transport(url, self._headers(), payload, self.config.timeout_seconds)
        response_text = _extract_anthropic_text(response_payload)

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
            subject_level_data_included=request.subject_level_data_included,
            prompt_artifact_id=prompt_artifact_id,
            response_artifact_id=response_artifact_id,
            prompt_hash=f"sha256:{sha256_text(request.prompt)}",
            response_hash=f"sha256:{sha256_text(response_text)}",
            redaction_policy=request.redaction_policy,
            provider_locality=self.config.provider_locality,
            provider_alias=self.config.provider_alias,
            transport=self.config.transport,
            provider_base_url=_redacted_base_url(self.config.base_url),
            external_relay=_is_external_relay(self.config.base_url, self.config.known_base_urls),
            risk_flags=_provider_risk_flags(
                base_url=self.config.base_url,
                known_base_urls=self.config.known_base_urls,
                allow_custom_base_url=self.config.allow_custom_base_url,
                subject_level_data_included=request.subject_level_data_included,
            ),
        )
        return LLMResponse(response_text=response_text, call_record=call_record)

    def _validate_request(self, request: LLMRequest) -> None:
        if self.config.provider_locality == "external_api" and not request.exposure.external_api_allowed:
            raise LLMClientConfigError("External LLM calls require external_api_allowed=True in LLMExposureConfig.")
        if self.config.requires_api_key and not self._api_key():
            raise LLMClientConfigError(f"Missing API key for provider; set {self.config.api_key_env} or pass api_key.")
        _validate_base_url_policy(
            base_url=self.config.base_url,
            known_base_urls=self.config.known_base_urls,
            allow_custom_base_url=self.config.allow_custom_base_url,
            approved_by=self.config.custom_base_url_approved_by,
        )

    def _headers(self) -> dict[str, str]:
        headers = {
            "Content-Type": "application/json",
            "anthropic-version": self.config.anthropic_version,
            **self.config.extra_headers,
        }
        api_key = self._api_key()
        if api_key:
            headers["x-api-key"] = api_key
        return headers

    def _api_key(self) -> str | None:
        return self.config.api_key or os.environ.get(self.config.api_key_env)


def _chat_completions_url(base_url: str) -> str:
    root = base_url.rstrip("/")
    if root.endswith("/chat/completions"):
        return root
    return f"{root}/chat/completions"


def _anthropic_messages_url(base_url: str) -> str:
    root = base_url.rstrip("/")
    if root.endswith("/v1/messages"):
        return root
    return f"{root}/v1/messages"


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


def _extract_anthropic_text(payload: dict[str, Any]) -> str:
    content = payload.get("content")
    if not isinstance(content, list):
        raise LLMProviderResponseError("Anthropic response does not contain a content block list.")
    text_parts = [
        block.get("text", "")
        for block in content
        if isinstance(block, dict) and block.get("type") == "text" and isinstance(block.get("text"), str)
    ]
    response_text = "\n".join(part for part in text_parts if part)
    if not response_text:
        raise LLMProviderResponseError("Anthropic response does not contain text content blocks.")
    return response_text


def _openai_compatible_config_from_provider(config: LLMProviderConfig, provider: str) -> OpenAICompatibleConfig:
    defaults = {
        "openai": ("https://api.openai.com/v1", "OPENAI_API_KEY"),
        "openai-compatible": ("https://api.openai.com/v1", "OPENAI_API_KEY"),
        "deepseek": ("https://api.deepseek.com/v1", "DEEPSEEK_API_KEY"),
        "qwen": ("https://dashscope.aliyuncs.com/compatible-mode/v1", "DASHSCOPE_API_KEY"),
    }
    default_base_url, default_api_key_env = defaults[provider]
    base_url = config.base_url or default_base_url
    return OpenAICompatibleConfig(
        base_url=base_url,
        api_key=config.api_key,
        api_key_env=config.api_key_env or default_api_key_env,
        timeout_seconds=config.timeout_seconds,
        provider_alias=provider,
        known_base_urls=(default_base_url,),
        allow_custom_base_url=config.allow_custom_base_url,
        custom_base_url_approved_by=config.custom_base_url_approved_by,
    )


def _anthropic_config_from_provider(config: LLMProviderConfig, provider: str) -> AnthropicMessagesConfig:
    default_base_url = "https://api.anthropic.com"
    return AnthropicMessagesConfig(
        base_url=config.base_url or default_base_url,
        api_key=config.api_key,
        api_key_env=config.api_key_env or "ANTHROPIC_API_KEY",
        timeout_seconds=config.timeout_seconds,
        provider_alias="anthropic" if provider == "claude" else provider,
        anthropic_version=config.anthropic_version,
        max_tokens=config.max_tokens,
        known_base_urls=(default_base_url,),
        allow_custom_base_url=config.allow_custom_base_url,
        custom_base_url_approved_by=config.custom_base_url_approved_by,
    )


def _validate_base_url_policy(
    *,
    base_url: str,
    known_base_urls: tuple[str, ...],
    allow_custom_base_url: bool,
    approved_by: str | None,
) -> None:
    if _normalized_base_url(base_url) in {_normalized_base_url(url) for url in known_base_urls}:
        return
    if allow_custom_base_url and approved_by:
        return
    raise LLMClientConfigError("Custom LLM base_url requires allow_custom_base_url=True and custom_base_url_approved_by.")


def _provider_risk_flags(
    *,
    base_url: str,
    known_base_urls: tuple[str, ...],
    allow_custom_base_url: bool,
    subject_level_data_included: bool,
) -> list[str]:
    flags: list[str] = []
    if _is_external_relay(base_url, known_base_urls):
        flags.append("external_relay")
        if allow_custom_base_url:
            flags.append("custom_base_url_approved")
    if subject_level_data_included:
        flags.append("subject_level_data_sent")
    return flags


def _is_external_relay(base_url: str, known_base_urls: tuple[str, ...]) -> bool:
    return _normalized_base_url(base_url) not in {_normalized_base_url(url) for url in known_base_urls}


def _normalized_base_url(base_url: str) -> str:
    return base_url.rstrip("/").lower()


def _redacted_base_url(base_url: str) -> str:
    return base_url.rstrip("/")


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
    except RemoteDisconnected as exc:
        raise LLMProviderResponseError("Provider closed the connection without returning a response.") from exc
    except TimeoutError as exc:
        raise LLMProviderResponseError(f"Provider request timed out after {timeout_seconds} seconds.") from exc
    try:
        return json.loads(response_text)
    except json.JSONDecodeError as exc:
        raise LLMProviderResponseError("Provider response was not valid JSON.") from exc
