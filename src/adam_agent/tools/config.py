"""Run configuration loading for the Phase 4 MVP."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from adam_agent.schemas.llm import LLMExposureConfig


@dataclass
class RunConfig:
    """Minimal run configuration used by tool-layer tests."""

    study_id: str
    run_id: str
    llm_exposure: LLMExposureConfig


class ConfigLoader:
    """Load run configuration and normalize to existing schema objects."""

    def load(self, path: str | Path | None = None, *, study_id: str = "UNKNOWN", run_id: str = "run_001") -> RunConfig:
        if path is None:
            return RunConfig(
                study_id=study_id,
                run_id=run_id,
                llm_exposure=LLMExposureConfig(),
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

        return RunConfig(
            study_id=str(payload.get("study_id", study_id)),
            run_id=str(payload.get("run_id", run_id)),
            llm_exposure=LLMExposureConfig.model_validate(llm_payload),
        )
