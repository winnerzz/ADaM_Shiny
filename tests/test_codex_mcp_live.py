"""Optional live smoke test for Codex MCP integration.

This test is skipped by default. Enable it only on a developer machine with a
working Codex CLI, CODEX_HOME, and API-key environment.
"""

from __future__ import annotations

import asyncio
import os
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.agents.codex_mcp import CodexMCPClient, CodexMCPConfig, CodexMCPRequest
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.agents.codex_mcp import CodexMCPClient, CodexMCPConfig, CodexMCPRequest


@unittest.skipUnless(
    os.environ.get("ADAM_AGENT_RUN_LIVE_CODEX_MCP") == "1"
    and bool(os.environ.get("SUB2API_API_KEY"))
    and bool(os.environ.get("CODEX_HOME")),
    "live Codex MCP smoke requires ADAM_AGENT_RUN_LIVE_CODEX_MCP=1, SUB2API_API_KEY, and CODEX_HOME",
)
class LiveCodexMCPTests(unittest.TestCase):
    def test_live_codex_mcp_smoke(self) -> None:
        command = os.environ.get("ADAM_AGENT_CODEX_COMMAND")
        config = CodexMCPConfig(
            cwd=str(ROOT),
            command=command,
            codex_home=os.environ["CODEX_HOME"],
            model=os.environ.get("ADAM_AGENT_CODEX_MODEL", "gpt-5.5"),
            timeout_seconds=float(os.environ.get("ADAM_AGENT_CODEX_TIMEOUT_SECONDS", "300")),
            sandbox="read-only",
            approval_policy="never",
        )
        client = CodexMCPClient(config)

        result = asyncio.run(
            client.run(
                CodexMCPRequest(
                    prompt="Reply with exactly: CODEX_MCP_LIVE_OK",
                    developer_instructions="Do not inspect or modify files.",
                )
            )
        )

        self.assertEqual(result.status, "ok", result.error)
        self.assertIn("codex", result.tool_names)
        self.assertEqual(result.content.strip(), "CODEX_MCP_LIVE_OK")


if __name__ == "__main__":
    unittest.main()
