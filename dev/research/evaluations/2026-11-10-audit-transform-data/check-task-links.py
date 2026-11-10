#!/usr/bin/env python3
"""Check task Markdown and identify unchanged baseline link diagnostics."""
from pathlib import Path
from urllib.parse import unquote,urlsplit
import importlib.util,subprocess
ROOT=Path(__file__).resolve().parents[4]
spec=importlib.util.spec_from_file_location('links',ROOT/'dev/tools/check_links.py')
links=importlib.util.module_from_spec(spec);spec.loader.exec_module(links)
files=subprocess.check_output(['git','diff','--name-only','--','*.md'],cwd=ROOT,text=True).splitlines()
files.append('dev/research/evaluations/2026-11-10-audit-transform-report.md')
count=0;errors=[]
for name in files:
 file=ROOT/name;text=file.read_text()
 for offset,url in links.destinations(text):
  if url.startswith(('#','//')) or urlsplit(url).scheme:continue
  path=unquote(urlsplit(url).path)
  if not path:continue
  count+=1;target=ROOT/path.lstrip('/') if path.startswith('/') else file.parent/path
  if not target.exists():errors.append((name,url))
assert not errors,errors
print(f'PASS: {len(files)} task Markdown files; {count} local destinations; zero missing.')
lines=(ROOT/'build/scratch/links-final.log').read_text().splitlines()
old={s.split(':',1)[0] for s in lines[1:] if ':' in s}
for name in sorted(old):
 assert (ROOT/name).read_bytes()==subprocess.check_output(['git','show','HEAD:'+name],cwd=ROOT),name
 print('Unchanged baseline diagnostic file:',name)
print(f'All {len(lines)-1} global diagnostics concern {len(old)} files byte-identical to HEAD.')
