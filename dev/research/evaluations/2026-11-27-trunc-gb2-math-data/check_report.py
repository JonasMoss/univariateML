"""Validate all local links and Markdown fragments in the TASK-461 report."""
import pathlib
import sys
from urllib.parse import unquote, urlsplit

report = pathlib.Path(__file__).resolve().parent.with_name(
    "2026-11-27-trunc-gb2-math-report.md"
)
root = report.parents[3]
sys.path.insert(0, str(root / "dev/tools"))
from check_links import destinations
from existence_atlas import proof_anchors

checked = 0
for _, url in destinations(report.read_text()):
    link = urlsplit(url)
    if link.scheme or link.netloc:
        continue
    path = report.parent / unquote(link.path) if link.path else report
    assert path.is_file(), url
    if link.fragment and path.suffix == ".md":
        assert unquote(link.fragment) in proof_anchors(path), url
    checked += 1
print(f"{checked} local report links and their Markdown fragments validated.")
