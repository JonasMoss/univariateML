"""Verify the generated report outline, local links and atlas references."""

import re
import sys
from pathlib import Path


def main():
    report = Path(__file__).resolve().parent.parent / "2026-11-28-trunc-est3-math-report.md"
    sys.path.insert(0, str(report.parents[3] / "dev" / "tools"))
    from existence_atlas import proof_anchors

    source = report.read_text()
    assert not any(ord(char) < 32 and char not in "\n\t" for char in source)
    assert len(re.findall(r"^### .* truncate:\w+-estimated$", source, re.M)) == 447
    assert source.splitlines().count("$$") % 2 == 0
    anchors_by_path = {}
    for link in sorted(set(re.findall(r"\]\(([^ )]+)\)", source))):
        if link.startswith(("http:", "https:")):
            continue
        name, _, anchor = link.partition("#")
        path = report.parent / name if name else report
        assert path.is_file(), link
        if anchor and path.suffix == ".md":
            if path not in anchors_by_path:
                anchors_by_path[path] = proof_anchors(path)
            assert anchor in anchors_by_path[path], link
    print("447 regime headings and all local report links verified")


if __name__ == "__main__":
    main()
