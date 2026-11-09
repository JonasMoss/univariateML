"""Read-only source/evidence audit; run after final validation and site cleanup.

Caller reserves one CPU. This writes only this job's evidence JSON.
"""
from pathlib import Path
import datetime
import hashlib
import json
import math
import os
import re
import subprocess

data = Path(__file__).resolve().parent
repo = data.parents[3]
env = dict(os.environ, GIT_OPTIONAL_LOCKS='0')
def git(*args):
    return subprocess.check_output(['git', *args], cwd=repo, env=env)

paths = [
    'bindings/common/distributions/st/fit.hpp',
    'bindings/js/stop-test.mjs',
    'bindings/r/distributions/st/test.R',
    'univariateML-cpp/include/cppunivariate/distributions/st/README.md',
    'univariateML-cpp/include/cppunivariate/distributions/st/certificate.hpp',
    'univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp',
    'univariateML-cpp/include/cppunivariate/distributions/st/test.cpp',
]
assert set(git('diff', '--name-only').decode().splitlines()) == set(paths)
git('diff', '--check')
append_only = {}
for name in (paths[1], paths[2], paths[-1]):
    append_only[name] = (repo/name).read_bytes().startswith(git('show', 'HEAD:'+name))
assert all(append_only.values())
base = git('rev-parse', 'HEAD').decode().strip()
assert base == 'ddb399172bd99091c8d1b04dbf7fe329c8adc43e'
frozen = {}
family = 'univariateML-cpp/include/cppunivariate/distributions/st/'
for name in ('golden.json', 'dpqr.hpp', 'truth.py', 'truth.json'):
    value = (repo/family/name).read_bytes()
    assert value == git('show', 'HEAD:'+family+name)
    frozen[name] = hashlib.sha256(value).hexdigest()

def timings(name):
    rows = {}
    faces = {}
    for line in (data/name).read_text().splitlines():
        row = line.split()
        if row[0] == 'one-side':
            faces[row[1]] = row
        else:
            rows[int(row[0]), int(row[1])] = row
    assert len(rows) == 32 and len(faces) == 2
    return rows, faces
before, faces_before = timings('timing-before.txt')
after, faces_after = timings('timing-after.txt')
assert before.keys() == after.keys()
def same_number(a, b, tolerance):
    a, b = float(a), float(b)
    if math.isnan(a) or math.isnan(b):
        return math.isnan(a) and math.isnan(b)
    if not math.isfinite(a) or not math.isfinite(b):
        return a == b
    return abs(a-b) <= tolerance*max(1, abs(a), abs(b))
defaults = {}
for n in (7, 500):
    a, b = before[n, 15], after[n, 15]
    assert a[2:4] == b[2:4]
    assert all(same_number(x,y,1e-12) for x,y in zip(a[4:8],b[4:8]))
    assert same_number(a[8],b[8],1e-10)
    defaults[n] = dict(before=a[:-1], after=b[:-1], identical_printed_values=a[:-1]==b[:-1])
summary = dict(
    rows=[dict(n=n,mask=mask,before_ms=float(before[n,mask][-1]),
               after_ms=float(after[n,mask][-1]),before=before[n,mask][2:-1],
               after=after[n,mask][2:-1]) for n,mask in sorted(before)],
    one_sign=[dict(alpha=key,before_ms=float(faces_before[key][-1]),
                   after_ms=float(faces_after[key][-1])) for key in faces_before],
    extras=[], default_replay=defaults)
extras_before = [line.split() for line in (data/'extra-timing-before.txt').read_text().splitlines()]
extras_after = [line.split() for line in (data/'extra-timing-after.txt').read_text().splitlines()]
assert len(extras_before) == len(extras_after) == 6
for a,b in zip(extras_before,extras_after):
    assert a[0] == b[0]
    summary['extras'].append(dict(route=int(a[0]),before_ms=float(a[-1]),after_ms=float(b[-1]),
                                  before=a[1:-1],after=b[1:-1]))
(data/'timing-summary.json').write_text(json.dumps(summary,indent=2)+'\n')

checks = json.loads((data/'checks.json').read_text())
assert len(checks['fits']) == 12 and len(checks['scales']) == 4
assert checks['range_points'] == 2000
assert max(r['error'] for r in checks['fits']) < 1e-10
assert max(r['relative_error'] for r in checks['scales']) < 1e-10
golden = json.loads((data/'golden-conditional-check.json').read_text())
assert float(golden['certified_xi_error']) < 1e-10
assert abs(float(golden['stored_xi_difference'])) < 1e-5
thin = json.loads((data/'thin-score-checks.json').read_text())
assert all(row['enclosed'] for row in thin if row['resolved'])
r_results = {}
for tier in ('dev','full'):
    counts = re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',
                        (data/f'r-{tier}.txt').read_text())
    assert counts, tier
    fail,warn,skip,passed = map(int,counts[-1])
    assert fail == warn == 0
    if tier == 'full':
        assert skip == 0
    r_results[tier] = dict(fail=fail,warn=warn,skip=skip,passed=passed)
js = (data/'js-test.txt').read_text()
js_results = {key:int(re.search(r'^# '+key+r' (\d+)\s*$',js,re.M)[1])
              for key in ('tests','pass','fail','cancelled','skipped')}
assert js_results['fail'] == js_results['cancelled'] == js_results['skipped'] == 0
assert js_results['tests'] == js_results['pass']
manifest = dict(base=base, timestamp_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                source_sha256={name:hashlib.sha256((repo/name).read_bytes()).hexdigest() for name in paths},
                append_only_tests=append_only, unchanged_family_files=frozen,
                default_replay=defaults, r=r_results, js=js_results,
                numerical_checks=dict(fits=12,scales=4,range_points=2000,
                                      max_xi_error=max(r['error'] for r in checks['fits']),
                                      max_scale_relative_error=max(r['relative_error'] for r in checks['scales']),
                                      max_unpadded_term_violation=checks['max_unpadded_violation'],
                                      thin_points=len(thin),thin_unresolved=sum(not r['resolved'] for r in thin),
                                      thin_incorrect_bounds=0))
(data/'source-manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
print(json.dumps(manifest,indent=2))
