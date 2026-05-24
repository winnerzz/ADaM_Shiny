"""Artifact management tools for the Phase 4 MVP."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any

from adam_agent.schemas.artifacts import ArtifactKind, ArtifactRef, ArtifactRole


class ArtifactStore:
    """Register files and write a run-level artifact manifest."""

    def __init__(self, study_dir: str | Path, study_id: str, run_id: str, *, manifest_name: str = "manifest.json") -> None:
        self.study_dir = Path(study_dir)
        self.study_id = study_id
        self.run_id = run_id
        self.run_dir = self.study_dir / "runs" / run_id
        if Path(manifest_name).name != manifest_name:
            raise ValueError("manifest_name must be a file name, not a path")
        self.manifest_name = manifest_name
        self.artifacts: list[ArtifactRef] = []

    @property
    def manifest_path(self) -> Path:
        """Canonical Phase 4 manifest path."""

        return self.run_dir / "audit" / self.manifest_name

    def register_existing(
        self,
        path: str | Path,
        *,
        artifact_id: str,
        kind: ArtifactKind,
        role: ArtifactRole,
        dataset: str | None = None,
        format: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> ArtifactRef:
        """Register an existing file and compute its sha256 hash."""

        artifact_path = Path(path)
        if not artifact_path.exists():
            raise FileNotFoundError(f"Artifact file does not exist: {artifact_path}")
        if not artifact_path.is_file():
            raise ValueError(f"Artifact path is not a file: {artifact_path}")

        artifact = ArtifactRef(
            artifact_id=artifact_id,
            kind=kind,
            path=_path_string(artifact_path),
            sha256=f"sha256:{sha256_file(artifact_path)}",
            dataset=dataset,
            format=format or artifact_path.suffix.lower().lstrip("."),
            role=role,
            metadata=metadata or {},
        )
        self.artifacts.append(artifact)
        return artifact

    def add_ref(self, artifact: ArtifactRef) -> ArtifactRef:
        """Add an already-created artifact reference."""

        self.artifacts.append(artifact)
        return artifact

    def write_manifest(self, extra: dict[str, Any] | None = None) -> ArtifactRef:
        """Write the canonical run manifest and return its artifact ref."""

        self.manifest_path.parent.mkdir(parents=True, exist_ok=True)
        payload = {
            "study_id": self.study_id,
            "run_id": self.run_id,
            "artifacts": [artifact.model_dump(mode="json") for artifact in self.artifacts],
        }
        if extra:
            payload.update(extra)
        self.manifest_path.write_text(
            json.dumps(payload, indent=2, sort_keys=True),
            encoding="utf-8",
        )
        manifest = ArtifactRef(
            artifact_id=f"manifest_{self.study_id.lower()}_{self.run_id}",
            kind="audit_manifest",
            path=_path_string(self.manifest_path),
            sha256=f"sha256:{sha256_file(self.manifest_path)}",
            format="json",
            role="audit",
            metadata={"artifact_count": len(self.artifacts)},
        )
        self.artifacts.append(manifest)
        return manifest


def sha256_file(path: str | Path) -> str:
    """Return the sha256 digest for a file."""

    digest = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def sha256_text(text: str) -> str:
    """Return the sha256 digest for text."""

    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def _path_string(path: Path) -> str:
    return str(path.as_posix())
