"""Model registry for the Phase 4 MVP."""

from __future__ import annotations

from dataclasses import dataclass


class ModelNotImplementedError(NotImplementedError):
    """Raised when a provider/model is intentionally not implemented."""


@dataclass(frozen=True)
class ModelInfo:
    """Minimal provider/model metadata."""

    provider: str
    model: str
    provider_locality: str
    requires_api_key: bool
    implemented: bool = True


class ModelRegistry:
    """Registry with mock-only support in Phase 4."""

    def __init__(self) -> None:
        self._models: dict[tuple[str, str], ModelInfo] = {
            ("mock", "mock-model"): ModelInfo(
                provider="mock",
                model="mock-model",
                provider_locality="local_model",
                requires_api_key=False,
                implemented=True,
            )
        }

    def lookup(self, provider: str, model: str) -> ModelInfo:
        provider_key = provider.lower()
        model_key = model
        if provider_key in {"openai", "anthropic", "openai-compatible"}:
            raise ModelNotImplementedError(
                f"{provider}/{model} is not implemented in Phase 4; use mock/mock-model"
            )

        try:
            return self._models[(provider_key, model_key)]
        except KeyError as exc:
            raise KeyError(f"Unknown model: {provider}/{model}") from exc
