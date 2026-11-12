"""Check this job's Markdown links and identify unrelated global failures."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
from urllib.parse import unquote, urlsplit

H = Path(__file__).resolve().parent
ROOT = H.parents[3]
spec = importlib.util.spec_from_file_location('links', ROOT/'dev/tools/check_links.py')
links = importlib.util.module_from_spec(spec)
spec.loader.exec_module(links)
files = sorted(ROOT.glob('*.md'))
for base in ('dev', 'bindings', 'univariateML-cpp'):
    files += sorted((ROOT/base).rglob('*.md'))
    files += sorted((ROOT/base).rglob('*.Rmd'))
errors = []
for file in files:
    if 'build' in file.relative_to(ROOT).parts or '__pycache__' in file.parts:
        continue
    text = file.read_text()
    for offset, url in links.destinations(text):
        if url.startswith(('#', '//')) or urlsplit(url).scheme:
            continue
        path = unquote(urlsplit(url).path)
        if not path:
            continue
        target = ROOT/path.lstrip('/') if path.startswith('/') else file.parent/path
        if not target.exists():
            errors.append(dict(file=str(file.relative_to(ROOT)),
                               line=text[:offset].count('\n')+1, destination=url))
base = '217ab8b533cac25ccc8eba8d66eccde1b376f0b4'
env = dict(os.environ, GIT_OPTIONAL_LOCKS='0')
for name in {e['file'] for e in errors}:
    before = subprocess.check_output(['git', 'show', base+':'+name], cwd=ROOT, env=env)
    assert before == (ROOT/name).read_bytes(), name
owned = ['dev/research/evaluations/2026-11-12-dpq-skew-report.md']
owned += [f'univariateML-cpp/include/cppunivariate/distributions/{f}/README.md'
          for f in ['ged', 'sged', 'sstd', 'st']]
assert not any(e['file'] in owned for e in errors)
result = dict(baseline=base, owned_files=owned, new_failures=[],
              global_existing_failures=errors, failure_files_byte_identical_to_baseline=True)
(H/'report-links.json').write_text(json.dumps(result, indent=2)+'\n')
print('Job Markdown links green;', len(errors), 'unrelated global failures in baseline-identical files')
