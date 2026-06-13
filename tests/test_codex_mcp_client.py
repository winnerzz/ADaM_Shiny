"""Tests for the optional Codex MCP adapter boundary."""

from __future__ import annotations

import asyncio
import os
import sys
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]

try:
    from adam_agent.agents.codex_mcp import CodexMCPClient, CodexMCPConfig, CodexMCPRequest
except ModuleNotFoundError:
    SRC = ROOT / "src"
    if str(SRC) not in sys.path:
        sys.path.insert(0, str(SRC))
    from adam_agent.agents.codex_mcp import CodexMCPClient, CodexMCPConfig, CodexMCPRequest


class FakeCodexTool:
    name = "codex"

    def __init__(self) -> None:
        self.payload: dict[str, object] | None = None

    async def ainvoke(self, args):  # type: ignore[no-untyped-def]
        self.payload = dict(args)
        return {
            "content": "CODEX_MCP_FAKE_OK",
            "thread_id": "thread_fake_001",
        }


class FakeBackend:
    def __init__(self, tools) -> None:  # type: ignore[no-untyped-def]
        self.tools = tools
        self.server_name: str | None = None

    async def get_tools(self, *, server_name: str | None = None):  # type: ignore[no-untyped-def]
        self.server_name = server_name
        return self.tools


class FakeOtherTool:
    name = "codex-reply"

    async def ainvoke(self, args):  # type: ignore[no-untyped-def]
        return "unused"


class CodexMCPClientTests(unittest.TestCase):
    def test_config_builds_stdio_connection_without_persisting_secret(self) -> None:
        with patch.dict(os.environ, {"SUB2API_API_KEY": "test-secret"}, clear=False):
            config = CodexMCPConfig(
                cwd=str(ROOT),
                command=r"C:\Users\winnerzz\AppData\Roaming\npm\codex.cmd",
                codex_home=r"C:\Users\winnerzz\.codex-6",
                extra_env={"EXTRA_SETTING": "1"},
            )
            connection = config.connection()

        self.assertEqual(connection["transport"], "stdio")
        self.assertEqual(connection["args"], ["mcp-server"])
        self.assertTrue(str(connection["command"]).endswith("codex.cmd"))
        self.assertEqual(connection["env"]["CODEX_HOME"], r"C:\Users\winnerzz\.codex-6")
        self.assertEqual(connection["env"]["SUB2API_API_KEY"], "test-secret")
        self.assertEqual(connection["env"]["EXTRA_SETTING"], "1")

    def test_run_invokes_codex_tool_with_bounded_payload(self) -> None:
        tool = FakeCodexTool()
        backend = FakeBackend([tool, FakeOtherTool()])
        client = CodexMCPClient(
            CodexMCPConfig(
                cwd=str(ROOT),
                command="codex.cmd",
                model="gpt-5.5",
                timeout_seconds=30,
                sandbox="read-only",
                approval_policy="never",
            ),
            backend=backend,
        )

        result = asyncio.run(
            client.run(
                CodexMCPRequest(
                    prompt="Reply with exactly CODEX_MCP_FAKE_OK",
                    developer_instructions="No file edits.",
                    config={"model_provider": "sub2api"},
                )
            )
        )

        self.assertEqual(result.status, "ok")
        self.assertEqual(result.content, "CODEX_MCP_FAKE_OK")
        self.assertEqual(result.thread_id, "thread_fake_001")
        self.assertEqual(result.tool_names, ["codex", "codex-reply"])
        self.assertEqual(backend.server_name, "codex")
        self.assertIsNotNone(tool.payload)
        assert tool.payload is not None
        self.assertEqual(tool.payload["prompt"], "Reply with exactly CODEX_MCP_FAKE_OK")
        self.assertEqual(tool.payload["model"], "gpt-5.5")
        self.assertEqual(tool.payload["sandbox"], "read-only")
        self.assertEqual(tool.payload["approval-policy"], "never")
        self.assertEqual(tool.payload["developer-instructions"], "No file edits.")
        self.assertEqual(tool.payload["config"], {"model_provider": "sub2api"})

    def test_run_reports_missing_codex_tool_as_error(self) -> None:
        client = CodexMCPClient(
            CodexMCPConfig(cwd=str(ROOT), command="codex.cmd"),
            backend=FakeBackend([FakeOtherTool()]),
        )

        result = asyncio.run(client.run(CodexMCPRequest(prompt="hello")))

        self.assertEqual(result.status, "error")
        self.assertIn("Codex MCP tool 'codex' was not found", str(result.error))


if __name__ == "__main__":
    unittest.main()
