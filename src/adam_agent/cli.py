"""Small command-line entry points for local development."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from adam_agent.adsl.runner import run_adsl_minimal


def main() -> int:
    parser = argparse.ArgumentParser(prog="adam-agent")
    subparsers = parser.add_subparsers(dest="command", required=True)

    adsl = subparsers.add_parser("run-adsl-minimal", help="Run the Phase 5 ADSL minimal loop.")
    adsl.add_argument("--study-dir", required=True, help="Path to a local study folder.")
    adsl.add_argument("--run-id", required=True, help="Run id to write under studies/{study_id}/runs/.")
    adsl.add_argument("--rscript-path", default=None, help="Optional full path to Rscript.exe.")
    adsl.add_argument("--study-id", default=None, help="Optional study id override.")

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
                },
                indent=2,
                sort_keys=True,
            )
        )
        return 0 if result.status == "completed" else 1
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
