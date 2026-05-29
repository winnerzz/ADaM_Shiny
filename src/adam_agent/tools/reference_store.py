"""Local reference lookup boundary for standards and study guidance.

This is a minimal file-backed interface. It gives future graph agents a stable
tool shape without pretending that the repo already contains a complete CDISC
or P21 knowledge base.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


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
