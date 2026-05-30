"""Tests for shared R execution-boundary helpers."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.tools.r_safety import find_forbidden_r_call_matches, find_forbidden_r_calls
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.tools.r_safety import find_forbidden_r_call_matches, find_forbidden_r_calls


class RSafetyTests(unittest.TestCase):
    def test_forbidden_call_detection_ignores_comments_and_strings(self) -> None:
        calls = find_forbidden_r_calls(
            "# system('not-a-call')\n"
            "message(\"shell('not-a-call')\")\n"
            "system2('whoami')\n"
        )

        self.assertEqual(calls, ("system2",))

    def test_forbidden_call_matches_can_use_prestripped_code(self) -> None:
        matches = find_forbidden_r_call_matches(
            "system('whoami')\n",
            ("system",),
            code_is_stripped=True,
        )

        self.assertEqual(matches, (("system", "system("),))


if __name__ == "__main__":
    unittest.main()
