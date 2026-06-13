"""Optional Codex MCP adapter for external authoring agents.

This module keeps Codex integration behind a small boundary. The Studio can
ask Codex to author a reviewable package, then validate and review the package
through the existing graph/workbench flow.
"""

from __future__ import annotations

import asyncio
import os
import shutil
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Protocol


class CodexMCPError(RuntimeError):
    """Raised when the optional Codex MCP call cannot complete cleanly."""


class CodexTool(Protocol):
    """Minimal LangChain tool surface used by the adapter."""

    name: str

    async def ainvoke(self, args: dict[str, Any]) -> Any:
        ...


class CodexMCPBackend(Protocol):
    """Backend protocol so tests can use a fake client."""

    async def get_tools(self, *, server_name: str | None = None) -> list[CodexTool]:
        ...


@dataclass(frozen=True)
class CodexMCPConfig:
    """Runtime settings for the Codex MCP server."""

    cwd: str
    command: str | None = None
    codex_home: str | None = None
    model: str = "gpt-5.5"
    timeout_seconds: float = 300.0
    sandbox: str = "read-only"
    approval_policy: str = "never"
    api_key_env_var: str = "SUB2API_API_KEY"
    extra_env: dict[str, str] = field(default_factory=dict)
    server_name: str = "codex"

    def resolved_command(self) -> str:
        if self.command:
            return str(Path(self.command).expanduser())
        found = shutil.which("codex.cmd") or shutil.which("codex")
        if found:
            return found
        appdata = os.environ.get("APPDATA")
        if appdata:
            candidate = Path(appdata) / "npm" / "codex.cmd"
            if candidate.exists():
                return str(candidate)
        return "codex.cmd" if os.name == "nt" else "codex"

    def environment(self) -> dict[str, str]:
        env: dict[str, str] = {}
        if self.codex_home:
            env["CODEX_HOME"] = str(Path(self.codex_home).expanduser())
        api_key = os.environ.get(self.api_key_env_var)
        if api_key:
            env[self.api_key_env_var] = api_key
        env.update(self.extra_env)
        return env

    def connection(self) -> dict[str, Any]:
        return {
            "transport": "stdio",
            "command": self.resolved_command(),
            "args": ["mcp-server"],
            "env": self.environment(),
        }


@dataclass(frozen=True)
class CodexMCPRequest:
    """One bounded Codex authoring request."""

    prompt: str
    cwd: str | None = None
    model: str | None = None
    sandbox: str | None = None
    approval_policy: str | None = None
    developer_instructions: str | None = None
    config: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class CodexMCPResult:
    """Structured result from a Codex MCP request."""

    status: str
    content: str
    tool_names: list[str]
    duration_ms: int
    thread_id: str | None = None
    raw_result: Any = None
    error: str | None = None


class CodexMCPClient:
    """Thin async wrapper around the optional langchain MCP adapter."""

    def __init__(self, config: CodexMCPConfig, *, backend: CodexMCPBackend | None = None) -> None:
        self.config = config
        self._backend = backend

    async def run(self, request: CodexMCPRequest) -> CodexMCPResult:
        started = time.perf_counter()
        backend = self._backend or self._build_backend()
        try:
            tools = await asyncio.wait_for(
                backend.get_tools(server_name=self.config.server_name),
                timeout=self.config.timeout_seconds,
            )
            tool_names = [tool.name for tool in tools]
            codex_tool = _find_tool(tools, "codex")
            if codex_tool is None:
                raise CodexMCPError(f"Codex MCP tool 'codex' was not found. Available tools: {tool_names}")
            payload = self._tool_payload(request)
            raw_result = await asyncio.wait_for(
                codex_tool.ainvoke(payload),
                timeout=self.config.timeout_seconds,
            )
            duration_ms = int((time.perf_counter() - started) * 1000)
            return CodexMCPResult(
                status="ok",
                content=_content_from_tool_result(raw_result),
                tool_names=tool_names,
                thread_id=_thread_id_from_tool_result(raw_result),
                raw_result=raw_result,
                duration_ms=duration_ms,
            )
        except Exception as exc:  # pragma: no cover - exact adapter errors vary by install.
            duration_ms = int((time.perf_counter() - started) * 1000)
            return CodexMCPResult(
                status="error",
                content="",
                tool_names=[],
                duration_ms=duration_ms,
                error=str(exc),
            )

    def _build_backend(self) -> CodexMCPBackend:
        try:
            from langchain_mcp_adapters.client import MultiServerMCPClient
        except ModuleNotFoundError as exc:
            raise CodexMCPError(
                "Codex MCP support requires optional dependency 'langchain-mcp-adapters'. "
                "Install the package extra 'codex-mcp' before enabling this adapter."
            ) from exc
        return MultiServerMCPClient({self.config.server_name: self.config.connection()})

    def _tool_payload(self, request: CodexMCPRequest) -> dict[str, Any]:
        payload: dict[str, Any] = {
            "prompt": request.prompt,
            "cwd": request.cwd or self.config.cwd,
            "model": request.model or self.config.model,
            "sandbox": request.sandbox or self.config.sandbox,
            "approval-policy": request.approval_policy or self.config.approval_policy,
        }
        if request.developer_instructions:
            payload["developer-instructions"] = request.developer_instructions
        if request.config:
            payload["config"] = request.config
        return payload


def _find_tool(tools: list[CodexTool], name: str) -> CodexTool | None:
    for tool in tools:
        if tool.name == name:
            return tool
    return None


def _content_from_tool_result(raw_result: Any) -> str:
    if isinstance(raw_result, str):
        return raw_result
    if isinstance(raw_result, dict):
        for key in ["content", "result", "output", "text"]:
            value = raw_result.get(key)
            if isinstance(value, str):
                return value
        if "messages" in raw_result:
            return str(raw_result["messages"])
    content = getattr(raw_result, "content", None)
    if isinstance(content, str):
        return content
    return str(raw_result)


def _thread_id_from_tool_result(raw_result: Any) -> str | None:
    if isinstance(raw_result, dict):
        for key in ["thread_id", "threadId", "conversationId", "conversation_id"]:
            value = raw_result.get(key)
            if value:
                return str(value)
    for key in ["thread_id", "threadId", "conversationId", "conversation_id"]:
        value = getattr(raw_result, key, None)
        if value:
            return str(value)
    return None
