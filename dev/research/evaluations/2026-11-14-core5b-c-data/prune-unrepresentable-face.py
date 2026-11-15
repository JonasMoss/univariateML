#!/usr/bin/env python3
"""Prune the two fixed Lomax payload entries skipped by fit-density's item filter."""
import fractions,importlib.util,json,sys
from pathlib import Path
here=Path(__file__).resolve().parent;root=here.parents[3]
sys.path.insert(0,str(root/'dev/validation'))
sp=importlib.util.spec_from_file_location('fs',root/'dev/validation/fit-snapshot.py');fs=importlib.util.module_from_spec(sp);sp.loader.exec_module(fs)
path=root/'dev/validation/strict-known.json';known=json.loads(path.read_text())
original=json.loads((here/'known-before.json').read_text());proof=[]
for key,old in original.items():
 parts=key.split('|')
 if parts[0]!='density:limit' or parts[2]!='lomax' or old['ref']!='TASK-214.16':continue
 mode,f,source,case=parts[1:5];r=fs.load(root/f'build/snapshots/{mode}')[(f,source,case)]
 res=r['result'];assert res['outcome']['kind']=='failure' and res['outcome']['reason']=='evaluation',res
 assert not res['limit_estimates'] and res['limit_loglik']=='nan',res
 data=[fractions.Fraction(v) for v in r['data']];assert data and min(data)>0
 rate=1/(sum(data)/len(data));assert rate>fractions.Fraction(sys.float_info.max)
 if key in known['known']:del known['known'][key]
 proof.append(dict(key=key,reference=old['ref'],exact_exponential_rate=str(rate),exceeds_binary64_max=True,
   new_outcome=res['outcome'],reason='No density item remains: the core correctly rejects the unrepresentable face payload.'))
known['known']=dict(sorted(known['known'].items()));path.write_text(json.dumps(known,indent=1)+'\n')
(here/'pruned-unrepresentable-face.json').write_text(json.dumps(proof,indent=2)+'\n')
print('Removed %d baseline Lomax entries after exact-rational rate and outcome checks.'%len(proof))
