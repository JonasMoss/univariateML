"""Check job documentation and identify unchanged repository-wide findings."""
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
from urllib.parse import unquote,urlsplit

root=Path.cwd()
data=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('repository_links',root/'dev/tools/check_links.py')
checker=importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)

def broken(file):
    text=file.read_text()
    rows=[]
    for offset,url in checker.destinations(text):
        if url.startswith(('#','//')) or urlsplit(url).scheme:continue
        path=unquote(urlsplit(url).path)
        if not path:continue
        target=root/path.lstrip('/') if path.startswith('/') else file.parent/path
        if not target.exists():
            rows.append({'file':str(file.relative_to(root)),'line':text[:offset].count('\n')+1,'destination':url})
    return rows

files=sorted(root.glob('*.md'))
for base in ('dev','bindings','univariateML-cpp'):
    files.extend(sorted((root/base).rglob('*.md')))
    files.extend(sorted((root/base).rglob('*.Rmd')))
findings=[]
for file in files:
    if 'build' in file.relative_to(root).parts or '__pycache__' in file.parts:continue
    findings.extend(broken(file))
unchanged={}
for name in sorted({row['file'] for row in findings}):
    original=subprocess.check_output(['git','show','HEAD:'+name])
    assert (root/name).read_bytes()==original,'link finding in a changed file: '+name
    unchanged[name]=hashlib.sha256(original).hexdigest()
job=[root/'univariateML-cpp/include/cppunivariate/distributions'/family/'README.md' for family in ('gev','frechet')]
job.append(data/'proof.md')
report=data.parent/'2026-11-10-gev-held2-report.md'
if report.exists():job.append(report)
job_findings=[row for file in job for row in broken(file)]
assert not job_findings,job_findings
record={'head':subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),
        'job_documents':[str(p.relative_to(root)) for p in job],
        'job_broken_links':job_findings,'repository_findings':findings,
        'unchanged_head_file_sha256':unchanged}
(data/'link-validation.json').write_text(json.dumps(record,indent=2)+'\n')
print(f'{len(job)} job documents: 0 broken links; repository scan: {len(findings)} findings, all in files identical to HEAD.')
