#!/usr/bin/env python3
"""Check local Markdown link/image destinations, reference links and HTML assets.

Run from any directory. Ignores fenced code, URI schemes and fragment-only
links; validates destination files, not section fragments or remote resources.
Generated packages, build output, docs/ and .github/ are outside this check.
"""
from pathlib import Path
from urllib.parse import unquote, urlsplit
import re
import json
import subprocess
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
    retirement=json.loads((ROOT/'dev/validation/retired-files.json').read_text())
    historical=set(retirement['paths'])
    historical_checked=set()
    baseline_paths=set(subprocess.check_output(['git','ls-tree','-r','--name-only',retirement['baseline']],cwd=ROOT,text=True).splitlines())
    baseline_dirs={str(Path(path).parent) for path in baseline_paths}
    baseline_urls={}
    inherited=[]
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
                relative=str(target.resolve().relative_to(ROOT)) if target.resolve().is_relative_to(ROOT) else ''
                if str(file.relative_to(ROOT)).startswith(('dev/research/','dev/roadmap/')) and relative in historical:
                    if relative not in historical_checked:
                        subprocess.run(['git','cat-file','-e',retirement['baseline']+':'+relative],cwd=ROOT,check=True)
                        historical_checked.add(relative)
                    continue
                source_path=str(file.relative_to(ROOT))
                if source_path not in baseline_urls:
                    old=subprocess.run(['git','show',retirement['baseline']+':'+source_path],cwd=ROOT,text=True,capture_output=True)
                    baseline_urls[source_path]={u for _,u in destinations(old.stdout)} if old.returncode==0 else set()
                record=f'{file.relative_to(ROOT)}:{text[:offset].count(chr(10))+1}: {url}'
                # A destination already absent at the recorded baseline is an
                # inherited issue; a newly missing tracked target still fails.
                if url in baseline_urls[source_path] and relative not in baseline_paths and relative not in baseline_dirs:
                    inherited.append(record)
                else:errors.append(record)
    print(f'{len(historical_checked)} historical retirement citations checked at '+retirement['baseline'])
    print(f'{len(files)} Markdown files; {checked} local destinations; {len(inherited)} inherited broken; {len(errors)} new broken')
    print('\n'.join('inherited: '+entry for entry in inherited),end='\n' if inherited else '')
    print('\n'.join(errors), end='\n' if errors else '')
    return bool(errors)

if __name__ == '__main__':
    sys.exit(main())
