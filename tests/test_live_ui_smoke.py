"""Opt-in live smoke test for the Phase 8 browser-equivalent workflow.

This test follows the same backend contract used by the local UI:

1. prepare demo study inputs
2. build the dependency plan
3. test the browser-scoped LLM provider config
4. generate reviewable R code through the real provider
5. optionally approve and execute the generated code with local R

It is skipped by default because it calls an external LLM provider and requires
private credentials.
"""

from __future__ import annotations

import os
import sys
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from fastapi.testclient import TestClient

    from adam_agent.api.app import create_app


def _live_ui_enabled() -> bool:
    return os.environ.get("ADAM_AGENT_RUN_LIVE_UI_SMOKE", "").strip() == "1"


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


def _demo_source(name: str) -> Path:
    source = _workspace_dir(name) / "demo-data"
    source.mkdir(parents=True)
    (source / "ae.csv").write_text(
        "USUBJID,AESEQ,AETERM,AESTDTC,AEENDTC,AEREL,AESEV,AESER\n"
        "01,1,HEADACHE,2024-01-02,2024-01-03,POSSIBLY RELATED,MILD,N\n",
        encoding="utf-8",
    )
    (source / "dm.csv").write_text(
        "USUBJID,SUBJID,ARM,RFSTDTC,RFENDTC,SEX,AGE\n"
        "01,1001,Test Drug,2024-01-01,2024-01-10,F,42\n",
        encoding="utf-8",
    )
    (source / "ex.csv").write_text(
        "USUBJID,EXTRT,EXSTDTC,EXENDTC\n"
        "01,Test Drug,2024-01-01,2024-01-10\n",
        encoding="utf-8",
    )
    (source / "adsl.csv").write_text("USUBJID,TRTSDT,TRTEDT\n01,2024-01-01,2024-01-10\n", encoding="utf-8")
    (source / "adae.csv").write_text("USUBJID,AESEQ,AETERM,TRTEMFL\n01,1,HEADACHE,Y\n", encoding="utf-8")
    (source / "ads_adae_full.csv").write_text(
        "Dataset,Variable,Label,Type,Source,Derivation\n"
        "ADAE,USUBJID,Unique Subject Identifier,char,AE.USUBJID,Copy from SDTM AE\n"
        "ADAE,AESEQ,Sequence Number,num,AE.AESEQ,Copy from SDTM AE\n"
        "ADAE,AETERM,Reported Term,char,AE.AETERM,Copy from SDTM AE\n"
        "ADAE,ASTDT,Analysis Start Date,date,AE.AESTDTC,Convert AESTDTC to Date\n"
        "ADAE,TRTEMFL,Treatment Emergent Flag,char,AE.AESTDTC and ADSL.TRTSDT,Y when ASTDT >= TRTSDT\n",
        encoding="utf-8",
    )
    (source / "ads_adsl_full.csv").write_text(
        "Dataset,Variable,Label,Type,Source,Derivation\n"
        "ADSL,USUBJID,Unique Subject Identifier,char,DM.USUBJID,Copy from SDTM DM\n"
        "ADSL,TRTSDT,Treatment Start Date,date,EX.EXSTDTC,First exposure start date\n",
        encoding="utf-8",
    )
    return source


@unittest.skipUnless(_live_ui_enabled(), "Set ADAM_AGENT_RUN_LIVE_UI_SMOKE=1 to call a real LLM via the API/UI workflow.")
class LiveUIWorkflowSmokeTests(unittest.TestCase):
    def test_ui_equivalent_real_llm_generation_workflow(self) -> None:
        provider = os.environ.get("ADAM_AGENT_LIVE_LLM_PROVIDER", "openai-compatible")
        model = os.environ.get("ADAM_AGENT_LIVE_LLM_MODEL", "gpt-5.5")
        base_url = os.environ.get("ADAM_AGENT_LIVE_LLM_BASE_URL")
        api_key_env = os.environ.get("ADAM_AGENT_LIVE_LLM_API_KEY_ENV", "ADAM_AGENT_LIVE_LLM_API_KEY")
        approved_by = os.environ.get("ADAM_AGENT_LIVE_LLM_APPROVED_BY", "local_live_ui_smoke")
        api_key = os.environ.get(api_key_env)
        if not api_key:
            self.skipTest(f"Set {api_key_env} before running the live UI smoke test.")

        execute_r = os.environ.get("ADAM_AGENT_LIVE_UI_EXECUTE_R", "").strip() == "1"
        rscript_path = os.environ.get("ADAM_AGENT_LIVE_UI_RSCRIPT_PATH") or r"C:\Dev\R-4.5.2\bin\Rscript.exe"

        client = TestClient(create_app())
        source = _demo_source("phase8_5_live_ui_source")
        target = _workspace_dir("phase8_5_live_ui_target") / "demo_adam"
        run_id = "run_phase8_5_live_ui"
        provider_payload = {
            "provider": provider,
            "model": model,
            "base_url": base_url,
            "api_key": api_key,
            "timeout_seconds": float(os.environ.get("ADAM_AGENT_LIVE_LLM_TIMEOUT", "120")),
            "max_tokens": int(os.environ.get("ADAM_AGENT_LIVE_LLM_MAX_TOKENS", "4096")),
            "allow_custom_base_url": bool(base_url),
            "custom_base_url_approved_by": approved_by if base_url else None,
        }
        exposure_payload = {
            "mode": "demo_rich_context",
            "data_classification": "processed_demo",
            "external_api_allowed": True,
            "approved_by": approved_by,
            "approval_note": "Opt-in Phase 8.5 live UI smoke with processed demo data.",
            "sample_rows_per_dataset": 3,
            "include_reference_rows": True,
        }

        demo = client.post(
            "/demo-study",
            params={"demo_source_dir": str(source), "study_dir": str(target)},
        )
        self.assertEqual(demo.status_code, 200, demo.text)
        demo_payload = demo.json()

        plan = client.post(
            "/runs/prepare",
            json={
                "study_dir": demo_payload["study_dir"],
                "study_id": demo_payload["study_id"],
                "run_id": run_id,
                "target_datasets": ["ADAE"],
            },
        )
        self.assertEqual(plan.status_code, 200, plan.text)
        self.assertIn("ADAE", plan.json()["target_datasets"])

        connection = client.post(
            "/llm/test-connection",
            json={"llm_provider": provider_payload, "llm_exposure": exposure_payload},
        )
        self.assertEqual(connection.status_code, 200, connection.text)
        self.assertEqual(connection.json()["status"], "ok")

        generated = client.post(
            f"/runs/{run_id}/datasets/ADAE/generate-code",
            json={
                "study_dir": demo_payload["study_dir"],
                "study_id": demo_payload["study_id"],
                "config_path": demo_payload["config_path"],
                "llm_provider_override": provider_payload,
                "llm_exposure_override": exposure_payload,
            },
        )
        self.assertEqual(generated.status_code, 200, generated.text)
        generated_payload = generated.json()
        self.assertEqual(generated_payload["status"], "code_generated")
        self.assertEqual(generated_payload["dataset"], "ADAE")
        normalized_code = generated_payload["generated_code"].lower().replace("\\", "/")
        self.assertIn("adae.csv", normalized_code)
        self.assertIn("output", normalized_code)
        self.assertNotIn(api_key, generated.text)

        if not execute_r:
            return

        review = client.post(
            f"/runs/{run_id}/datasets/ADAE/code-review",
            json={
                "study_dir": demo_payload["study_dir"],
                "decision": "approve",
                "reviewer": approved_by,
                "notes": "Approved for opt-in live UI smoke execution.",
            },
        )
        self.assertEqual(review.status_code, 200, review.text)

        executed = client.post(
            f"/runs/{run_id}/datasets/ADAE/execute-approved-code",
            json={
                "study_dir": demo_payload["study_dir"],
                "study_id": demo_payload["study_id"],
                "rscript_path": rscript_path,
            },
        )
        self.assertEqual(executed.status_code, 200, executed.text)
        self.assertEqual(executed.json()["status"], "completed")

        compare = client.get(
            f"/runs/{run_id}/datasets/ADAE/compare",
            params={"study_dir": demo_payload["study_dir"]},
        )
        self.assertEqual(compare.status_code, 200, compare.text)
        self.assertIn(compare.json()["status"], {"match", "differences", "missing_reference"})


if __name__ == "__main__":
    unittest.main()
