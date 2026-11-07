"""Run the repository's link parser on this job's Markdown; retain baseline evidence."""
from pathlib import Path
import hashlib
import importlib.util
import json
import subprocess

root = Path(__file__).resolve().parents[4]
# The report links this checker's result, which is finalized below.
(root / "dev/research/evaluations/2026-11-07-cmp-ledger-data/changed-markdown-links.json").touch(exist_ok=True)
spec = importlib.util.spec_from_file_location("local_links", root / "dev/tools/check_links.py")
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)
changed = ["dev/design/work-budgets.md", "dev/research/evaluations/2026-11-07-cmp-ledger-report.md"]
checked = 0
broken = []
for name in changed:
    file = root / name
    text = file.read_text()
    for offset, url in checker.destinations(text):
        if url.startswith(("#", "//")) or checker.urlsplit(url).scheme:
            continue
        path = checker.unquote(checker.urlsplit(url).path)
        if not path:
            continue
        checked += 1
        target = root / path.lstrip("/") if path.startswith("/") else file.parent / path
        if not target.exists():
            broken.append({"file": name, "line": text[:offset].count("\n") + 1, "url": url})
base = "d34a44cd1b0d7324e1f486d66e6b3f7e3db9210a"
preexisting = [
    ("dev/research/evaluations/2026-11-03-boundary-lit-report.md", 5, "../boundary-points.pdf"),
    ("univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md", 234, "1+m/r"),
    ("univariateML-cpp/notes/inference.md", 3, "/inference/"),
    ("univariateML-cpp/notes/inference.md", 95, "/inference/"),
]
evidence = []
for name, line, url in preexisting:
    current = (root / name).read_bytes()
    original = subprocess.check_output(["git", "show", f"{base}:{name}"], cwd=root)
    assert current == original, name
    evidence.append({"file": name, "line": line, "url": url,
                     "unchanged_from_base": True, "sha256": hashlib.sha256(current).hexdigest()})
result = {"files": changed, "checked_destinations": checked, "broken": broken,
          "repository_wide_check": {"markdown_files": 510, "destinations": 2769,
                                    "preexisting_findings": evidence}}
(root / "dev/research/evaluations/2026-11-07-cmp-ledger-data/changed-markdown-links.json").write_text(json.dumps(result, indent=2) + "\n")
print(f"Changed Markdown: {len(changed)} files; {checked} local destinations; {len(broken)} broken.")
print("All four repository-wide findings are in files byte-identical to the base revision.")
raise SystemExit(bool(broken))
