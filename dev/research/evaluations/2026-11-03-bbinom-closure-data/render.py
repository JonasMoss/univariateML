#!/usr/bin/env python3
"""Render the requested Markdown report as a script-free MathML preview."""
from pathlib import Path
import re
import runpy
import subprocess
from urllib.parse import unquote, urlsplit

here = Path(__file__).resolve().parent
root = here.parents[3]
source = here.parent / "2026-11-03-bbinom-closure-report.md"
checker = runpy.run_path(str(root / "dev/tools/check_links.py"))
count = 0
for file in (source, here / "README.md"):
    for _, url in checker["destinations"](file.read_text()):
        if url.startswith(("#", "//")) or urlsplit(url).scheme:
            continue
        path = unquote(urlsplit(url).path)
        target = root / path.lstrip("/") if path.startswith("/") else file.parent / path
        assert target.exists(), (file, url)
        count += 1

result = subprocess.run(
    ["pandoc", "--standalone", "--mathml",
     "--from=markdown+tex_math_single_backslash",
     "--metadata=pagetitle:Beta-binomial closure and finite attainment", str(source)],
    check=True, capture_output=True, text=True,
)
assert not result.stderr.strip(), result.stderr
html = result.stdout
assert html.count("<math") >= 30
assert "<script" not in html.lower() and "mathjax" not in html.lower()

def link(match):
    url = match[1]
    if not url.startswith(("#", "/", "//")) and not urlsplit(url).scheme:
        url = "../" + url
    return 'href="' + url + '"'

html = re.sub(r'href="([^"]+)"', link, html)
(here / "preview.html").write_text(html)
print(f"PASS: {count} local Markdown links; {html.count('<math')} MathML elements; no scripts")
