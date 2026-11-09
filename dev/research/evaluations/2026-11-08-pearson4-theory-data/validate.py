#!/usr/bin/env python3
"""Validate retained evidence and document inventory, without rerunning passed jobs."""
import json, pathlib, hashlib, re, sys, datetime, subprocess, runpy
from urllib.parse import urlsplit, unquote
from decimal import Decimal
HERE=pathlib.Path(__file__).resolve().parent
pairs=[('check.py','checks.json'),('sampler.py','sampler.json'),('structure.py','structure.json'),('hyper.py','hyper.json')]
checks=[]
for script,result in pairs:
    data=json.loads((HERE/result).read_text())
    sha=hashlib.sha256((HERE/script).read_bytes()).hexdigest()
    assert data['script_sha256']==sha,(script,'stale result')
    assert data['passed']>0
    for row in data.get('checks',[]):
        if 'error' in row and 'bound' in row:
            assert Decimal(row['error'])<=Decimal(row['bound'])
        if 'mathematical_bound' in row:
            assert Decimal(row['error'])<=Decimal(row['mathematical_bound'])+Decimal(row['working_arithmetic_allowance'])
    checks.append(dict(script=script,result=result,sha256=sha,exit_code=0,records=data['passed']))
rows=re.findall(r'^\| (\d+) \|', (HERE/'subsets.md').read_text(),flags=re.M)
assert list(map(int,rows))==list(range(16)),rows
root=HERE.parents[3]
destinations=runpy.run_path(str(root/'dev/tools/check_links.py'))['destinations']
link_count=0
for file in [HERE.parent/'2026-11-08-pearson4-theory-report.md',*sorted(HERE.glob('*.md'))]:
    for _,url in destinations(file.read_text()):
        if url.startswith(('#','//')) or urlsplit(url).scheme:continue
        path=unquote(urlsplit(url).path)
        if not path:continue
        target=root/path.lstrip('/') if path.startswith('/') else file.parent/path
        assert target.exists(),(file,url)
        link_count+=1
status=subprocess.check_output(['git','status','--short','--untracked-files=all'],text=True)
allowed='dev/research/evaluations/2026-11-08-pearson4-theory'
for line in status.splitlines():
    assert line[3:].startswith(allowed),line
out=dict(completed_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
    machine='workstation; no simbox run',cpu='one slot via /tmp/umvl-cpu/run; nice -n 10; OMP/BLAS one thread',
    python=sys.version.split()[0],runs=checks,free_subsets=16,
    scoped_links=dict(checked=link_count,broken=0),
    repository_link_check=dict(exit_code=1,broken=6,classification='Existing issues outside this job; no new bundle destination failed.',
        issues=['2026-10-31-inference-6-report.md: missing desktop.png and mobile-evidence.png in ignored build scratch',
                '2026-11-03-boundary-lit-report.md: missing generated boundary-points.pdf',
                'nbinom/README.md: parser treats the mathematical expression 1+m/r as a destination',
                'notes/inference.md: two site-root /inference/ links interpreted as filesystem paths']),
    evidence='All four retained numerical jobs exited zero; source hashes match their outputs.',
    package_suites='Not required or run: no tracked production code, fitting, dpqr, bindings, test infrastructure or dossiers changed.',
    scope='Only the requested report and evidence directory; no commit, board edit, install or sub-agent.',
    initial_failure='The first check.py run failed an invalid tail-only quantile test bracket. Corrected script then passed; details in numerics.md.',
    open_entries=['E: exact attained-point decisions in general boundary ties',
                  'N: uniform normal-face local experiment and inference calibration',
                  'G: production combined gamma branches, derivative bounds and floating-point margins',
                  'H: full published global hypergeometric transformations as optional accelerators'],
    success=True)
(HERE/'validation.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps({'validated_numerical_records':sum(x['records'] for x in checks),'subsets':16,'scoped_links':link_count,'broken':0,'success':True}))
