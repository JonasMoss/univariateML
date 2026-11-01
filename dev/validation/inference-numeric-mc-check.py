#!/usr/bin/env python3
"""Validate a completed, source-matched Monte Carlo experiment and its records.

A successful protocol check does not turn recorded failed fits, inference
failures, skips or two-MCSE discrepancy flags into passes. They remain explicit
in the source summary and the returned counts. Unexplained failures and missing
families make this check fail.
"""
import hashlib
import json
import math
from pathlib import Path
import re
import shlex

ROOT=Path(__file__).resolve().parents[2]
INC=ROOT/'univariateML-cpp/include/cppunivariate';DIST=INC/'distributions'
COMMON=ROOT/'bindings/common';DRIVER=ROOT/'dev/validation/inference-numeric-mc.py'
registry=(INC/'core/inference_numeric_generated.hpp').read_text()
generated=re.findall(r'#include "\.\./distributions/([^/]+)/dpqr.hpp"',registry)
base='norm gamma beta pois exp geom lnorm logitnorm lgamma rayleigh binom'.split()
special='laplace gev gpd ged sged snorm sstd tnorm cmb cmp tbinom tpois zmbinom'.split()
families=sorted(set(base+generated+special))
h=hashlib.sha256(DRIVER.read_bytes())
for directory in (INC,COMMON):
    for path in sorted(directory.rglob('*.hpp')):
        h.update(str(path.relative_to(ROOT)).encode());h.update(path.read_bytes())
for family in families:
    path=DIST/family/'README.md';h.update(str(path.relative_to(ROOT)).encode());h.update(path.read_bytes())
identity=h.hexdigest();path=ROOT/'build/scratch/inference-numeric-mc'/identity[:16]/'summary.json'
if not path.exists():raise SystemExit(f'no source-matched Monte Carlo results: {path}')
summary=json.loads(path.read_text())
assert summary['source_hash']==identity
assert summary['sample_size']==500 and summary['replications']==200
assert summary['family_order']==families
records=summary['families']
assert set(records)==set(families),(len(records),len(families),sorted(set(families)-set(records)))
counts={'complete':0,'complete_with_recorded_failures':0,'skipped_slow':0,'skipped_budget':0,
        'failed_replications':0,'flagged_coordinates':0}
for family,item in records.items():
    status=item['status']
    if status in ('skipped_slow','skipped_budget'):
        assert item.get('reason') and 'probe_cpu_seconds' in item,(family,item)
        counts[status]+=1;continue
    assert status=='complete',(family,item)
    assert item['replications']==200 and item['successes']+len(item['failures'])==200,(family,item)
    assert item['successes']>=2,(family,'too few successful fits')
    assert set(item['coordinates'])==set(item['free']),(family,'missing coordinate results')
    for line in item['failures']:
        fields=shlex.split(line)
        assert len(fields)>=4 and fields[1] in ('fit_failure','inference_failure','restricted_fit') and fields[3],(family,line)
        if fields[1]=='inference_failure':
            assert re.search(r'inference=\d+, model=\d+, sandwich=\d+:',fields[3]),(family,line)
    counts['failed_replications']+=len(item['failures'])
    counts['complete_with_recorded_failures' if item['failures'] else 'complete']+=1
    for name,coord in item['coordinates'].items():
        assert coord['empirical_sd']>0 and math.isfinite(coord['empirical_sd_mcse']),(family,name)
        flagged=False
        for kind in ('model','sandwich'):
            v=coord[kind]
            assert all(math.isfinite(v[k]) for k in ('mean_se','mean_se_mcse','difference','difference_mcse_paired')),(family,name,kind)
            assert v['mean_se']>=0 and v['mean_se_mcse']>=0 and v['difference_mcse_paired']>=0
            assert v['flag_2mcse']==(abs(v['difference'])>2*v['difference_mcse_paired'])
            flagged=flagged or v['flag_2mcse']
        counts['flagged_coordinates']+=int(flagged)
output={'source_hash':identity,'source_summary':str(path.relative_to(ROOT)),
        'interpretation':'Protocol validation only; recorded failed replications, skips and flags are not passes.',
        'counts':counts}
path.with_name('protocol-check.json').write_text(json.dumps(output,indent=2)+'\n')
print(json.dumps(output,indent=2))
