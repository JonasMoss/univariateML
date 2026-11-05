"""Root boxes and global exclusions on the prior multimodal examples."""
import importlib
import json
from pathlib import Path
import mpmath as mp
import numpy as np
import roots

HERE=Path(__file__).resolve().parent
engine=importlib.import_module('global')
source=HERE.parent/'2026-11-04-t-closure-data/multimodality.json'
cases=json.loads(source.read_text());rows=[]
for name,case in cases.items():
    x=np.repeat(case['x'],list(map(int,case['weights'])))
    certificates=[]
    for candidate in case['stationary_candidates']:
        q=roots.isolate(x,float(candidate['nu']),float(candidate['mu']),float(candidate['scale']))
        assert q.get('box'),(name,q)
        p=[(mp.mpf(candidate['mu'])-q['center'])/q['unit'],
           mp.log(mp.mpf(candidate['scale'])/q['unit']*mp.sqrt(mp.mpf(candidate['nu']))),
           mp.mpf(candidate['nu'])/mp.mpf(q['v0'])]
        assert all(mp.mpf(l)<v<mp.mpf(h) for l,v,h in zip(q['box']['lo'],p,q['box']['hi'])),name
        certificates.append(q)
    winner=max(zip(case['stationary_candidates'],certificates),key=lambda p:mp.mpf(p[0]['ll']))[1]
    outcomes={}
    for family,floor in [('lst',1),('std',2)]:
        outcomes[family]=engine.strings(engine.Engine(x.tolist(),floor).solve(winner))
    rows.append({'name':name,'roots':certificates,'global':outcomes})
assert sum(len(q['roots']) for q in rows)==5
(HERE/'adversarial.json').write_text(json.dumps(rows,indent=2,allow_nan=False)+'\n')
print('All five retained multimodal stationary points have root inclusion boxes.')
for row in rows:print(row['name'],{k:v['outcome'] for k,v in row['global'].items()})
