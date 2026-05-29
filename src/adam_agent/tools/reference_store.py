"""Local reference lookup boundary for standards and study guidance.

This is a minimal file-backed interface. It gives future graph agents a stable
tool shape without pretending that the repo already contains a complete CDISC
or P21 knowledge base.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Literal


@dataclass(frozen=True)
class ReferenceHit:
    """One local reference search hit."""

    source: str
    path: str
    snippet: str
    confidence: str = "low"

    def as_dict(self) -> dict[str, str]:
        return {
            "source": self.source,
            "path": self.path,
            "snippet": self.snippet,
            "confidence": self.confidence,
        }


class LocalReferenceStore:
    """Search local text-like reference files under approved roots."""

    def __init__(self, roots: list[str | Path] | None = None) -> None:
        self.roots = [Path(root) for root in roots or []]

    def search(self, query: str, *, limit: int = 5) -> list[ReferenceHit]:
        term = query.strip().lower()
        if not term:
            return []
        hits: list[ReferenceHit] = []
        for root in self.roots:
            if not root.exists() or not root.is_dir():
                continue
            for path in _reference_files(root):
                text = _read_text(path)
                if not text:
                    continue
                index = text.lower().find(term)
                if index < 0:
                    continue
                hits.append(
                    ReferenceHit(
                        source=root.name,
                        path=str(path.as_posix()),
                        snippet=_snippet(text, index),
                        confidence="low",
                    )
                )
                if len(hits) >= limit:
                    return hits
        return hits


ReferenceToolName = Literal[
    "search_cdisc_reference",
    "lookup_adam_rule",
    "lookup_p21_rule",
    "lookup_company_standard",
]


REFERENCE_TOOL_ROOTS: dict[ReferenceToolName, tuple[str, ...]] = {
    "search_cdisc_reference": ("CDISC",),
    "lookup_adam_rule": ("CDISC/ADaM_IG",),
    "lookup_p21_rule": ("CDISC/P21_Rules",),
    "lookup_company_standard": ("company_standards",),
}


@dataclass(frozen=True)
class ReferenceToolRequest:
    """Input contract for one local reference lookup tool call."""

    tool: ReferenceToolName
    query: str
    dataset: str = ""
    variable: str = ""
    limit: int = 5

    def normalized_query(self) -> str:
        parts = [self.query, self.dataset, self.variable]
        return " ".join(part.strip() for part in parts if part.strip())


@dataclass(frozen=True)
class ReferenceToolResult:
    """Output contract for one local reference lookup tool call."""

    tool: ReferenceToolName
    query: str
    hits: tuple[ReferenceHit, ...]
    warnings: tuple[str, ...] = ()

    def as_dict(self) -> dict[str, object]:
        return {
            "tool": self.tool,
            "query": self.query,
            "hits": [hit.as_dict() for hit in self.hits],
            "warnings": list(self.warnings),
            "implemented": True,
            "scope": "local_file_lookup_only",
            "non_compliance_disclaimer": "Reference lookup does not prove CDISC, ADaM IG, P21, or company-standard compliance.",
        }


def search_cdisc_reference(query: str, *, reference_root: str | Path = "references", limit: int = 5) -> ReferenceToolResult:
    """Search locally supplied CDISC reference text."""

    return run_reference_tool(
        ReferenceToolRequest(tool="search_cdisc_reference", query=query, limit=limit),
        reference_root=reference_root,
    )


def lookup_adam_rule(query: str, *, reference_root: str | Path = "references", limit: int = 5) -> ReferenceToolResult:
    """Search locally supplied ADaM reference text."""

    return run_reference_tool(
        ReferenceToolRequest(tool="lookup_adam_rule", query=query, limit=limit),
        reference_root=reference_root,
    )


def lookup_p21_rule(query: str, *, reference_root: str | Path = "references", limit: int = 5) -> ReferenceToolResult:
    """Search locally supplied P21 rule text."""

    return run_reference_tool(
        ReferenceToolRequest(tool="lookup_p21_rule", query=query, limit=limit),
        reference_root=reference_root,
    )


def lookup_company_standard(query: str, *, reference_root: str | Path = "references", limit: int = 5) -> ReferenceToolResult:
    """Search locally supplied company-standard text."""

    return run_reference_tool(
        ReferenceToolRequest(tool="lookup_company_standard", query=query, limit=limit),
        reference_root=reference_root,
    )


def run_reference_tool(request: ReferenceToolRequest, *, reference_root: str | Path = "references") -> ReferenceToolResult:
    """Run one local reference lookup using approved tool-root mapping."""

    root = Path(reference_root)
    warnings: list[str] = []
    roots = [root / relative for relative in REFERENCE_TOOL_ROOTS[request.tool]]
    existing_roots = [item for item in roots if item.exists() and item.is_dir()]
    if not existing_roots:
        warnings.append(f"No local reference root is available for tool {request.tool}.")
    query = request.normalized_query()
    limit = max(1, min(int(request.limit), 20))
    hits = tuple(LocalReferenceStore(existing_roots).search(query, limit=limit))
    if not hits:
        warnings.append("No local reference hit was found. This is not evidence that no rule exists.")
    return ReferenceToolResult(tool=request.tool, query=query, hits=hits, warnings=tuple(warnings))


def _reference_files(root: Path) -> list[Path]:
    suffixes = {".md", ".txt", ".csv", ".json", ".yaml", ".yml"}
    return [
        path
        for path in sorted(root.rglob("*"))
        if path.is_file() and path.suffix.lower() in suffixes
    ]


def _read_text(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return ""


def _snippet(text: str, index: int, *, radius: int = 180) -> str:
    start = max(index - radius, 0)
    end = min(index + radius, len(text))
    return " ".join(text[start:end].split())
