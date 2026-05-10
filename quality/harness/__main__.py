"""CLI entry point for the validation harness.

Usage:
    python -m quality.harness derive <id>
    python -m quality.harness validate <id>

The repo root is auto-detected as the directory containing the `quality/`
folder (so the CLI works from anywhere inside the repo).
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path


def find_repo_root() -> Path:
    here = Path(__file__).resolve()
    for parent in (here, *here.parents):
        if (parent / "quality" / "golden").is_dir():
            return parent
    raise SystemExit(
        "Could not locate repo root (expected a quality/golden/ directory "
        "above the harness package)."
    )


def cmd_derive(args) -> int:
    from quality.harness import derive

    derive.main(args.id, find_repo_root())
    return 0


def cmd_validate(args) -> int:
    from quality.harness import validate

    return validate.main(args.id, find_repo_root())


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="python -m quality.harness")
    sub = p.add_subparsers(dest="cmd", required=True)

    pd = sub.add_parser("derive", help="Symbolic derivation; emit eval_request + symbolic_grid")
    pd.add_argument("id")
    pd.set_defaults(func=cmd_derive)

    pv = sub.add_parser("validate", help="Diff symbolic vs C++ eval; emit reports")
    pv.add_argument("id")
    pv.set_defaults(func=cmd_validate)

    return p


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
