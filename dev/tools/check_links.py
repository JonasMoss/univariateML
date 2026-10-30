#!/usr/bin/env python3
"""Check local Markdown link/image destinations, reference links and HTML assets.

Run from any directory. Ignores fenced code, URI schemes and fragment-only
links; validates destination files, not section fragments or remote resources.
Generated packages, build output, docs/ and .github/ are outside this check.
"""
from pathlib import Path
from urllib.parse import unquote, urlsplit
import re
import sys

ROOT = Path(__file__).resolve().parents[2]

def destinations(text):
    text = re.sub(r'^\s*(```|~~~).*?^\s*\1\s*$', '', text, flags=re.M | re.S)
    for match in re.finditer(r'!?\[[^\]\n]*\]\(\s*(<[^>]+>|[^\s)]+)(?:\s+[^)]*)?\)', text):
        yield match.start(), match[1].strip('<>')
    definitions = r'^\s{0,3}\[([^\]\n]+)\]:\s*(<[^>]+>|\S+)'
    body = re.sub(definitions, '', text, flags=re.M)
    for match in re.finditer(definitions, text, re.M):
        # A reference destination may carry an optional quoted title.
        if re.search(r'\[' + re.escape(match[1]) + r'\](?![:(])', body, re.I):
            yield match.start(), match[2].strip('<>')
    for match in re.finditer(r'(?:href|src)=["\']([^"\']+)["\']', text):
        yield match.start(), match[1]

def main():
    files = sorted(ROOT.glob('*.md'))
    for base in ('dev', 'bindings', 'univariateML-cpp'):
        files.extend(sorted((ROOT/base).rglob('*.md')))
        files.extend(sorted((ROOT/base).rglob('*.Rmd')))
    checked = 0
    errors = []
    for file in files:
        if 'build' in file.relative_to(ROOT).parts or '__pycache__' in file.parts:
            continue
        text = file.read_text()
        for offset, url in destinations(text):
            if url.startswith(('#', '//')) or urlsplit(url).scheme:
                continue
            path = unquote(urlsplit(url).path)
            if not path:
                continue
            checked += 1
            target = ROOT/path.lstrip('/') if path.startswith('/') else file.parent/path
            if not target.exists():
                errors.append(f'{file.relative_to(ROOT)}:{text[:offset].count(chr(10))+1}: {url}')
    print(f'{len(files)} Markdown files; {checked} local destinations; {len(errors)} broken')
    print('\n'.join(errors), end='\n' if errors else '')
    return bool(errors)

if __name__ == '__main__':
    sys.exit(main())
