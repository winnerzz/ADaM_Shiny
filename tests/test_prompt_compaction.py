"""Tests for compact prompt rendering used by real LLM calls."""

from __future__ import annotations

import json
import sys
import unittest
import uuid
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = ROOT / ".tmp_tests"

try:
    from adam_agent.llm.prompt_compaction import compact_prompt_from_context, write_compact_prompt_artifact
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.llm.prompt_compaction import compact_prompt_from_context, write_compact_prompt_artifact


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class PromptCompactionTests(unittest.TestCase):
    def test_compact_prompt_renders_csv_spec_as_pipe_table(self) -> None:
        context = _context_with_csv_spec()

        prompt = compact_prompt_from_context(context)
        full_json = json.dumps(context, indent=2, sort_keys=True)

        self.assertLess(len(prompt), len(full_json))
        self.assertIn("variable|type|source|derivation", prompt)
        self.assertIn("COPIED: USUBJID<-AE.USUBJID", prompt)
        self.assertIn("TRTEMFL|char|AE.AESTDTC and ADSL.TRTSDT|Y when ASTDT >= TRTSDT", prompt)
        self.assertIn("### AE [csv; status=ok; rows=2; cols=3]", prompt)
        self.assertIn("Read path from R working directory: ../../input_sdtm/ae.csv", prompt)
        self.assertIn("Sample rows:", prompt)
        self.assertIn("### ADSL [csv; status=ok; rows=2; cols=2]", prompt)
        self.assertIn("Read path from R working directory: outputs/adsl.csv", prompt)
        self.assertIn("runtime_output_path: outputs/adae.csv", prompt)
        self.assertIn("csv_read_policy:", prompt)
        self.assertIn("colClasses", prompt)
        self.assertNotIn('"target_spec"', prompt)

    def test_compact_prompt_renders_json_spec_source_domains(self) -> None:
        context = _context_with_csv_spec()
        context["target_spec"] = {
            "format": "json",
            "text": "",
            "json": {
                "dataset": "ADAE",
                "variables": [
                    {
                        "variable": "AETERM",
                        "type": "char",
                        "source_domains": ["AE"],
                        "source_columns": ["AETERM"],
                        "derivation": "Copy reported term.",
                    }
                ],
            },
        }

        prompt = compact_prompt_from_context(context)

        self.assertIn("AETERM|char|AE.AETERM|Copy reported term.", prompt)

    def test_write_compact_prompt_artifact_records_exact_provider_prompt(self) -> None:
        study_dir = _workspace_dir("prompt_compaction_artifact") / "PSY201"
        prompt = compact_prompt_from_context(_context_with_csv_spec())

        artifact = write_compact_prompt_artifact(
            study_id="PSY201",
            run_id="run_prompt_compaction",
            target_dataset="ADAE",
            study_dir=study_dir,
            prompt=prompt,
            source_context_artifact_id="llm_context_psy201_run_prompt_compaction_adae",
        )

        prompt_path = Path(artifact.path)
        self.assertTrue(prompt_path.exists())
        self.assertEqual(prompt_path.read_text(encoding="utf-8"), prompt)
        self.assertEqual(artifact.kind, "llm_prompt")
        self.assertEqual(artifact.role, "audit")
        self.assertTrue(artifact.metadata["compact_prompt"])
        self.assertEqual(
            artifact.metadata["source_context_artifact_id"],
            "llm_context_psy201_run_prompt_compaction_adae",
        )


def _context_with_csv_spec() -> dict:
    return {
        "study_id": "PSY201",
        "run_id": "run_001",
        "target_dataset": "ADAE",
        "target_spec": {
            "format": "csv",
            "text": (
                "Dataset,Variable,Label,Type,Source,Derivation\n"
                "ADAE,USUBJID,Unique Subject Identifier,Copied,SDTM.AE.USUBJID,Copied from source\n"
                "ADAE,TRTEMFL,Treatment Emergent Flag,char,AE.AESTDTC and ADSL.TRTSDT,Y when ASTDT >= TRTSDT\n"
            ),
            "json": None,
        },
        "source_dataset_profiles": {
            "AE": {
                "format": "csv",
                "status": "ok",
                "columns": ["USUBJID", "AETERM", "AESTDTC"],
                "row_count": 2,
                "read_path": "../../input_sdtm/ae.csv",
                "sample_rows": [{"USUBJID": "01", "AETERM": "HEADACHE", "AESTDTC": "2024-01-02"}],
                "message": "",
            }
        },
        "resolved_dependencies": {
            "ADSL": {
                "format": "csv",
                "status": "ok",
                "columns": ["USUBJID", "TRTSDT"],
                "row_count": 2,
                "read_path": "outputs/adsl.csv",
                "sample_rows": [],
                "message": "",
            }
        },
        "runtime_contract": {
            "language": "R",
            "relative_output_path": "runs/run_001/outputs/adae.csv",
            "runtime_output_path": "outputs/adae.csv",
            "output_path": "runs/run_001/outputs/adae.csv",
            "code_path": "runs/run_001/code/build_adae.R",
            "write_only_to_run_dir": True,
            "no_network": True,
            "csv_read_policy": "Read CSV inputs with colClasses='character'.",
        },
        "exposure": {
            "mode": "demo_rich_context",
            "sample_rows_per_dataset": 1,
            "full_data_included": False,
        },
        "warnings": [],
    }


if __name__ == "__main__":
    unittest.main()
