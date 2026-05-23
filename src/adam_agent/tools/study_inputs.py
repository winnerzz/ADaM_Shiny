"""Study input scanning tools."""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path

from adam_agent.schemas.artifacts import ArtifactRef
from adam_agent.tools.artifacts import sha256_file


@dataclass
class InvalidInputFile:
    """A file found in a study input folder that the MVP will not consume."""

    path: str
    reason: str


@dataclass
class StudyInputIndex:
    """Structured index of study inputs."""

    study_id: str
    study_dir: str
    input_sdtm: dict[str, ArtifactRef] = field(default_factory=dict)
    reference_adam: dict[str, ArtifactRef] = field(default_factory=dict)
    input_spec: dict[str, ArtifactRef] = field(default_factory=dict)
    input_define: dict[str, ArtifactRef] = field(default_factory=dict)
    legacy_code: dict[str, ArtifactRef] = field(default_factory=dict)
    invalid_files: list[InvalidInputFile] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)


class StudyInputScanner:
    """Scan a local study folder into a structured input index."""

    def __init__(self, study_dir: str | Path, study_id: str | None = None) -> None:
        self.study_dir = Path(study_dir)
        self.study_id = study_id or self.study_dir.name

    def scan(self) -> StudyInputIndex:
        """Scan canonical Phase 1 input folders."""

        index = StudyInputIndex(
            study_id=self.study_id,
            study_dir=str(self.study_dir.as_posix()),
        )
        if not self.study_dir.exists():
            index.warnings.append(f"Study directory does not exist: {self.study_dir}")
            return index

        self._scan_input_sdtm(index)
        self._scan_reference_adam(index)
        self._scan_input_spec(index)
        self._scan_input_define(index)
        self._scan_legacy_code(index)
        return index

    def _scan_input_sdtm(self, index: StudyInputIndex) -> None:
        folder = self.study_dir / "input_sdtm"
        for path in _iter_files(folder, index):
            suffix = path.suffix.lower()
            domain = path.stem.upper()
            if suffix not in {".csv", ".sas7bdat"}:
                index.invalid_files.append(
                    InvalidInputFile(
                        path=str(path.as_posix()),
                        reason="input_sdtm supports only csv and sas7bdat in the MVP",
                    )
                )
                continue
            index.input_sdtm[domain] = _artifact(
                path,
                artifact_id=f"input_sdtm_{domain.lower()}",
                kind="input_sdtm",
                role="source",
                dataset=domain,
            )

    def _scan_reference_adam(self, index: StudyInputIndex) -> None:
        folder = self.study_dir / "reference_adam"
        for path in _iter_files(folder, index):
            suffix = path.suffix.lower()
            dataset = path.stem.upper()
            if suffix not in {".csv", ".sas7bdat"}:
                index.invalid_files.append(
                    InvalidInputFile(
                        path=str(path.as_posix()),
                        reason="reference_adam supports only csv and sas7bdat in the MVP",
                    )
                )
                continue
            index.reference_adam[dataset] = _artifact(
                path,
                artifact_id=f"reference_adam_{dataset.lower()}",
                kind="reference_adam",
                role="reference",
                dataset=dataset,
            )

    def _scan_input_spec(self, index: StudyInputIndex) -> None:
        folder = self.study_dir / "input_spec"
        for path in _iter_files(folder, index):
            key = path.stem.lower()
            index.input_spec[key] = _artifact(
                path,
                artifact_id=f"input_spec_{key}",
                kind="input_spec",
                role="source",
            )

    def _scan_input_define(self, index: StudyInputIndex) -> None:
        folder = self.study_dir / "input_define"
        for path in _iter_files(folder, index):
            key = path.stem.lower()
            index.input_define[key] = _artifact(
                path,
                artifact_id=f"input_define_{key}",
                kind="input_define",
                role="source",
            )

    def _scan_legacy_code(self, index: StudyInputIndex) -> None:
        folder = self.study_dir / "legacy_code"
        for path in _iter_files(folder, index):
            key = path.stem.lower()
            index.legacy_code[key] = _artifact(
                path,
                artifact_id=f"legacy_code_{key}",
                kind="legacy_code",
                role="source",
                dataset=path.stem.upper(),
            )


def _iter_files(folder: Path, index: StudyInputIndex):
    if not folder.exists():
        index.warnings.append(f"Missing optional input folder: {folder.name}")
        return []
    return sorted(path for path in folder.iterdir() if path.is_file())


def _artifact(path: Path, *, artifact_id: str, kind: str, role: str, dataset: str | None = None) -> ArtifactRef:
    return ArtifactRef(
        artifact_id=artifact_id,
        kind=kind,
        path=str(path.as_posix()),
        sha256=f"sha256:{sha256_file(path)}",
        dataset=dataset,
        format=path.suffix.lower().lstrip("."),
        role=role,
    )
