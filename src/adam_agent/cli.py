"""Small command-line entry points for local development."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from adam_agent.adsl.runner import run_adsl_minimal
from adam_agent.graph.study_graph import compile_study_graph
from adam_agent.tools.config import ConfigLoader


def main() -> int:
    parser = argparse.ArgumentParser(prog="adam-agent")
    subparsers = parser.add_subparsers(dest="command", required=True)

    adsl = subparsers.add_parser("run-adsl-minimal", help="Run the Phase 5 ADSL minimal loop.")
    adsl.add_argument("--study-dir", required=True, help="Path to a local study folder.")
    adsl.add_argument("--run-id", required=True, help="Run id to write under studies/{study_id}/runs/.")
    adsl.add_argument("--rscript-path", default=None, help="Optional full path to Rscript.exe.")
    adsl.add_argument("--study-id", default=None, help="Optional study id override.")

    study = subparsers.add_parser("run-study", help="Run the study-level LangGraph orchestration.")
    study.add_argument("--study-dir", required=True, help="Path to a local study folder.")
    study.add_argument("--run-id", required=True, help="Run id to write under studies/{study_id}/runs/.")
    study.add_argument("--target", action="append", required=True, help="Target ADaM dataset. Repeat for multiple targets.")
    study.add_argument("--config", default=None, help="Optional JSON run config.")
    study.add_argument("--execution-mode", default=None, help="Override execution mode, for example llm_downstream_provider.")
    study.add_argument("--approved-dependency", action="append", default=[], help="Dependency ADaM dataset approved for system generation.")
    study.add_argument("--rscript-path", default=None, help="Optional full path to Rscript.exe.")
    study.add_argument("--study-id", default=None, help="Optional study id override.")

    args = parser.parse_args()
    if args.command == "run-adsl-minimal":
        try:
            result = run_adsl_minimal(
                Path(args.study_dir),
                run_id=args.run_id,
                rscript_path=args.rscript_path,
                study_id=args.study_id,
            )
        except Exception as exc:
            print(
                json.dumps(
                    {
                        "status": "failed",
                        "error": str(exc),
                    },
                    indent=2,
                    sort_keys=True,
                )
            )
            return 1
        print(
            json.dumps(
                {
                    "study_id": result.study_id,
                    "run_id": result.run_id,
                    "status": result.status,
                    "run_dir": result.run_dir,
                    "validation_status": result.validation_report["status"],
                    "manifest": result.manifest.path,
                    "failure_id": result.failure_record.failure_id if result.failure_record else None,
                    "failure_type": result.failure_record.failure_type if result.failure_record else None,
                    "root_cause": result.failure_record.root_cause if result.failure_record else None,
                    "recommended_route": result.failure_record.recommended_route if result.failure_record else None,
                },
                indent=2,
                sort_keys=True,
            )
        )
        return 0 if result.status == "completed" else 1
    if args.command == "run-study":
        study_id = args.study_id or Path(args.study_dir).name
        config = ConfigLoader().load(args.config, study_id=study_id, run_id=args.run_id)
        execution_mode = args.execution_mode
        if execution_mode is None and config.llm_provider.provider != "mock":
            execution_mode = "llm_downstream_provider"
        if execution_mode is None:
            execution_mode = "stub"
        graph = compile_study_graph()
        result = graph.invoke(
            {
                "study_id": config.study_id,
                "run_id": config.run_id,
                "target_datasets": args.target,
                "execution_mode": execution_mode,
                "study_dir": str(Path(args.study_dir)),
                "rscript_path": args.rscript_path or "",
                "approved_dependency_datasets": args.approved_dependency,
                "llm_exposure": config.llm_exposure.model_dump(mode="json"),
                "llm_provider": {
                    key: value
                    for key, value in config.llm_provider.__dict__.items()
                    if value is not None
                },
                "dataset_results": [],
                "blocked_datasets": [],
                "audit_artifacts": [],
            }
        )
        print(
            json.dumps(
                {
                    "study_id": result["study_id"],
                    "run_id": result["run_id"],
                    "status": result["status"],
                    "execution_mode": execution_mode,
                    "requested_datasets": result.get("requested_datasets", []),
                    "target_datasets": result.get("target_datasets", []),
                    "runnable_datasets": result.get("runnable_datasets", []),
                    "blocked_datasets": result.get("blocked_datasets", []),
                    "dependency_review_status": result.get("dependency_review_status"),
                    "audit_manifest": result.get("audit_manifest").path if result.get("audit_manifest") else None,
                    "dataset_results": [
                        summary.model_dump(mode="json") for summary in result.get("dataset_results", [])
                    ],
                },
                indent=2,
                sort_keys=True,
            )
        )
        return 0 if result["status"] in {"completed", "completed_stub"} else 1
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
