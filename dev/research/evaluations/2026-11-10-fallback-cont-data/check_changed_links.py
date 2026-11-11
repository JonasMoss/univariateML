#!/usr/bin/env python3
"""Apply the repository link scanner to this job's Markdown files only."""
from pathlib import Path
import runpy
from urllib.parse import unquote, urlsplit

ROOT = Path(__file__).resolve().parents[4]
scanner = runpy.run_path(str(ROOT / 'dev/tools/check_links.py'))['destinations']
dist = ROOT / 'univariateML-cpp/include/cppunivariate/distributions'
files = [dist / family / 'README.md' for family in
         ['burr', 'invburr', 'pareto4', 'paralogis', 'betapr', 'bhatt', 'ged', 'gb2', 'sged', 'invparalogis', 'pareto3']]
files += [dist / 'README_SCHEMA.md', ROOT / 'dev/validation/diagnostics.md']
report = ROOT / 'dev/research/evaluations/2026-11-10-fallback-cont-report.md'
if report.exists():
    files.append(report)
errors = []
checked = 0
for file in files:
    text = file.read_text()
    for offset, url in scanner(text):
        if url.startswith(('#', '//')) or urlsplit(url).scheme:
            continue
        path = unquote(urlsplit(url).path)
        if not path:
            continue
        checked += 1
        target = ROOT / path.lstrip('/') if path.startswith('/') else file.parent / path
        if not target.exists():
            errors.append(f'{file.relative_to(ROOT)}:{text[:offset].count(chr(10))+1}: {url}')
print(f'{len(files)} job Markdown files; {checked} local destinations; {len(errors)} broken')
for error in errors:
    print(error)
raise SystemExit(bool(errors))
