"""Tests for local reference lookup tool contracts."""

from __future__ import annotations

import sys
import unittest
import uuid
from pathlib import Path

from tests.temp_workspace import test_session_root

ROOT = Path(__file__).resolve().parents[1]
TMP_ROOT = test_session_root()

try:
    from adam_agent.tools.reference_store import (
        LocalReferenceStore,
        ReferenceToolRequest,
        lookup_adam_rule,
        lookup_company_standard,
        lookup_p21_rule,
        run_reference_tool,
        search_cdisc_reference,
    )
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.tools.reference_store import (
        LocalReferenceStore,
        ReferenceToolRequest,
        lookup_adam_rule,
        lookup_company_standard,
        lookup_p21_rule,
        run_reference_tool,
        search_cdisc_reference,
    )


def _workspace_dir(name: str) -> Path:
    TMP_ROOT.mkdir(parents=True, exist_ok=True)
    path = TMP_ROOT / f"{name}_{uuid.uuid4().hex}"
    path.mkdir(parents=True, exist_ok=False)
    return path


class ReferenceStoreTests(unittest.TestCase):
    def test_local_reference_store_searches_only_local_text_roots(self) -> None:
        workspace = _workspace_dir("reference_store")
        references = workspace / "references"
        references.mkdir()
        (references / "guide.md").write_text("ADaM timing variables require traceable source evidence.\n", encoding="utf-8")

        hits = LocalReferenceStore([references]).search("timing", limit=2)

        self.assertEqual(len(hits), 1)
        self.assertEqual(hits[0].source, "references")
        self.assertIn("timing", hits[0].snippet.lower())

    def test_reference_tools_route_to_explicit_local_roots(self) -> None:
        workspace = _workspace_dir("reference_tools")
        reference_root = workspace / "references"
        (reference_root / "CDISC" / "ADaM_IG").mkdir(parents=True)
        (reference_root / "CDISC" / "P21_Rules").mkdir(parents=True)
        (reference_root / "company_standards").mkdir(parents=True)
        (reference_root / "CDISC" / "ADaM_IG" / "adam.md").write_text(
            "Traceability appears in the local ADaM guidance.\n",
            encoding="utf-8",
        )
        (reference_root / "CDISC" / "P21_Rules" / "p21.md").write_text(
            "Rule SD9999 appears only in the P21 fixture.\n",
            encoding="utf-8",
        )
        (reference_root / "company_standards" / "company.md").write_text(
            "Company naming policy mentions analysis traceability.\n",
            encoding="utf-8",
        )

        adam = lookup_adam_rule("Traceability", reference_root=reference_root)
        p21 = lookup_p21_rule("SD9999", reference_root=reference_root)
        company = lookup_company_standard("naming policy", reference_root=reference_root)
        cdisc = search_cdisc_reference("Traceability", reference_root=reference_root)

        self.assertEqual(adam.tool, "lookup_adam_rule")
        self.assertTrue(all("/ADaM_IG/" in hit.path for hit in adam.hits))
        self.assertEqual(p21.tool, "lookup_p21_rule")
        self.assertTrue(all("/P21_Rules/" in hit.path for hit in p21.hits))
        self.assertEqual(company.tool, "lookup_company_standard")
        self.assertTrue(all("/company_standards/" in hit.path for hit in company.hits))
        self.assertEqual(cdisc.tool, "search_cdisc_reference")
        self.assertIn("non_compliance_disclaimer", cdisc.as_dict())

    def test_reference_tool_records_query_context_and_missing_reference_warning(self) -> None:
        workspace = _workspace_dir("reference_tools_missing")
        reference_root = workspace / "references"
        (reference_root / "CDISC" / "ADaM_IG").mkdir(parents=True)
        request = ReferenceToolRequest(
            tool="lookup_adam_rule",
            query="traceability",
            dataset="CUSTOM",
            variable="SUBJECT_ID",
            limit=100,
        )

        result = run_reference_tool(request, reference_root=reference_root)

        self.assertEqual(result.query, "traceability CUSTOM SUBJECT_ID")
        self.assertEqual(result.hits, ())
        self.assertIn("No local reference hit", " ".join(result.warnings))
        self.assertEqual(result.as_dict()["scope"], "local_file_lookup_only")


if __name__ == "__main__":
    unittest.main()
