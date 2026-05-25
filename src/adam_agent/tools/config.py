"""Run configuration loading for the Phase 4 MVP."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from adam_agent.llm.clients import LLMProviderConfig
from adam_agent.schemas.llm import LLMExposureConfig


@dataclass
class RunConfig:
    """Minimal run configuration used by tool-layer tests."""

    study_id: str
    run_id: str
    llm_exposure: LLMExposureConfig
    llm_provider: LLMProviderConfig


class ConfigLoader:
    """Load run configuration and normalize to existing schema objects."""

    def load(self, path: str | Path | None = None, *, study_id: str = "UNKNOWN", run_id: str = "run_001") -> RunConfig:
        if path is None:
            return RunConfig(
                study_id=study_id,
                run_id=run_id,
                llm_exposure=LLMExposureConfig(),
                llm_provider=LLMProviderConfig(),
            )

        payload = json.loads(Path(path).read_text(encoding="utf-8"))
        return self.from_dict(payload, study_id=study_id, run_id=run_id)

    def from_dict(self, payload: dict[str, Any], *, study_id: str = "UNKNOWN", run_id: str = "run_001") -> RunConfig:
        if "llm_exposure" in payload:
            llm_payload = dict(payload["llm_exposure"])
        else:
            llm_payload = {
                key: value
                for key, value in payload.items()
                if key not in {"study_id", "run_id"}
            }
        if "llm_exposure_mode" in llm_payload and "mode" not in llm_payload:
            llm_payload["mode"] = llm_payload.pop("llm_exposure_mode")
        llm_provider_payload = dict(payload.get("llm_provider", {}))
        if "llm_provider" not in payload:
            for old_key, new_key in {
                "provider": "provider",
                "model": "model",
                "base_url": "base_url",
                "api_key_env": "api_key_env",
                "timeout_seconds": "timeout_seconds",
            }.items():
                if old_key in payload:
                    llm_provider_payload[new_key] = payload[old_key]

        return RunConfig(
            study_id=str(payload.get("study_id", study_id)),
            run_id=str(payload.get("run_id", run_id)),
            llm_exposure=LLMExposureConfig.model_validate(llm_payload),
            llm_provider=LLMProviderConfig(**llm_provider_payload),
        )
