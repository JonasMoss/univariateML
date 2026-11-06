"""Cross-check native accepted boxes against the retained independent roots."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
here=Path(__file__).resolve().parent
rows=json.loads((here/'replay.json').read_text())
retained=json.loads((here.parent/'2026-11-05-t-root-data/independent.json').read_text())
reference=retained['roots_checked']
research=json.loads((here.parent/'2026-11-05-t-root-data/global.json').read_text())
checks=[]
for row in rows:
    key=(row['name'],row['family'])
    old=next(r for r in research if (r['name'],r['family'])==key)
    if row['outcome'] in ('finite','cauchy','normal','t2','collapse','unbounded'):
        expected={'collapse_unbounded':'unbounded'}.get(old['outcome'],old['outcome'])
        assert row['outcome']==expected,(key,row['outcome'],expected)
    if row['outcome']=='finite':
        ref=next(r for r in reference if (r['name'],r['family'])==key)
        for k,name in enumerate(['mu','scale','nu']):
            l,h=map(mp.mpf,row['coefficient_box'][k]);v=mp.mpf(ref['coefficients'][name])
            assert l<v<h,(key,name,l,v,h)
        checks.append({'name':row['name'],'family':row['family'],'independent_root_inside':True})
    if row['outcome'] in ('cauchy','t2'):
        ref=next(r for r in retained['endpoint_roots_checked'] if (r['name'],r['family'])==key)
        for k,name in enumerate(['mu','ordinary_scale','nu']):
            l,h=map(mp.mpf,row['coefficient_box'][k]);v=mp.mpf(ref[name])
            assert l<v<h,(key,name,l,v,h)
        checks.append({'name':row['name'],'family':row['family'],'independent_endpoint_root_inside':True})
    if row['name']==row['family']+'-golden':
        root=here.parents[3]
        golden=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{row["family"]}/golden.json').read_text())['estimates']
        box=[list(map(mp.mpf,b)) for b in row['coefficient_box']]
        if row['family']=='std':
            sl,sh=box[1];vl,vh=box[2]
            box[1]=[sl*mp.sqrt(vh/(vh-2)),sh*mp.sqrt(vl/(vl-2))]
        names=['mu','sigma','df'] if row['family']=='lst' else ['mean','sd','nu']
        err=max(abs(v-mp.mpf(golden[n]))/abs(mp.mpf(golden[n])) for b,n in zip(box,names) for v in b)
        assert err<mp.mpf('3e-5'),(key,err)
        checks.append({'name':row['name'],'whole_box_golden_relative_error':str(err),'tolerance':3e-5})
(here/'independent.json').write_text(json.dumps(checks,indent=2)+'\n')
print('Independent native box and outcome checks passed:',len(checks))
