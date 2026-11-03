#!/usr/bin/env python3
"""Validate report links and render using the existing Pandoc/MathML route."""
from pathlib import Path
import hashlib
import json
import re
import runpy
import subprocess
from urllib.parse import unquote, urlsplit

here = Path(__file__).resolve().parent
root = here.parents[3]
source = here.parent/'2026-11-03-burr-closure-report.md'
checker = runpy.run_path(str(root/'dev/tools/check_links.py'))
summary = json.loads((here/'summary.json').read_text())
for name, expected in summary['script_sha256'].items():
    assert hashlib.sha256((here/name).read_bytes()).hexdigest() == expected, name
count = 0
targets = []
for file in (source, here/'README.md'):
    for _, url in checker['destinations'](file.read_text()):
        if url.startswith(('#', '//')) or urlsplit(url).scheme:
            continue
        path = unquote(urlsplit(url).path)
        target = root/path.lstrip('/') if path.startswith('/') else file.parent/path
        assert target.exists() or target == here/'preview.html', (file, url)
        targets.append(target)
        count += 1
result = subprocess.run(['pandoc', '--standalone', '--mathml',
                         '--from=markdown+tex_math_single_backslash',
                         '--metadata=pagetitle:Burr XII closure and finite attainment', str(source)],
                        check=True, capture_output=True, text=True)
assert not result.stderr.strip(), result.stderr
html = result.stdout
assert html.count('<math') >= 40
assert '<script' not in html.lower() and 'mathjax' not in html.lower()

# Pandoc places a fenced block's explicit .literal-code on <pre>.
# Preserve that annotation on its <code> too for the note validator.
def literal_block(match):
    before, attrs = match[1], match[2]
    if 'class="' in attrs:
        attrs = attrs.replace('class="', 'class="literal-code ', 1)
    else:
        attrs += ' class="literal-code"'
    return before+'<code'+attrs+'>'

html = re.sub(r'(<pre\b[^>]*\bliteral-code\b[^>]*>\s*)<code([^>]*)>',
              literal_block, html)

def link(match):
    url = match[2]
    if not url.startswith(('#', '/', '//')) and not urlsplit(url).scheme:
        url = '../'+url
    return match[1]+'="'+url+'"'

html = re.sub(r'(href|src)="([^"]+)"', link, html)
(here/'preview.html').write_text(html)
assert all(target.exists() for target in targets)
print(f'PASS: research source hashes match; {count} local links; '
      f'{html.count("<math")} MathML elements; no scripts')
