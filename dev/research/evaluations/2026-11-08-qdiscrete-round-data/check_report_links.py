#!/usr/bin/env python3
"""Use the repository link parser for this job's changed Markdown files."""
import importlib.util
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit

sys.dont_write_bytecode = True

root = Path(__file__).resolve().parents[4]
spec = importlib.util.spec_from_file_location("check_links", root / "dev/tools/check_links.py")
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)
files = [root / name for name in (
    "bindings/js/README.md", "dev/roadmap/decisions.md",
    "dev/research/evaluations/2026-11-08-qdiscrete-round-report.md")]
checked = 0
errors = []
for file in files:
    for offset, url in checker.destinations(file.read_text()):
        if url.startswith(("#", "//")) or urlsplit(url).scheme:
            continue
        path = unquote(urlsplit(url).path)
        if not path:
            continue
        checked += 1
        target = root / path.lstrip("/") if path.startswith("/") else file.parent / path
        if not target.exists():
            errors.append(f"{file.relative_to(root)}: {url}")
print(f"{len(files)} changed Markdown files; {checked} local destinations; {len(errors)} broken")
for error in errors:
    print(error)
raise SystemExit(bool(errors))
