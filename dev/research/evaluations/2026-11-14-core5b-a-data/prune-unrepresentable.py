#!/usr/bin/env python3
"""Retire obsolete density checks whose former infinite-rate face is now rejected.

fit-density --prune only visits rows having a density payload, so cannot
retire these two entries itself. Prove the absence of a finite binary64
rate independently before removing precisely those original keys.
"""
from pathlib import Path
import sys,json,importlib.util,collections,gzip
import mpmath as mp
ROOT=Path(__file__).resolve().parents[4];sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('density',ROOT/'dev/validation/fit-density.py');fd=importlib.util.module_from_spec(spec);spec.loader.exec_module(fd)
archive=collections.defaultdict(list)
for row in json.loads(gzip.decompress(fd.fs.ARCHIVE.read_bytes())):archive[row['family']].append(row)
inputs=fd.inputs('pareto2',archive)[1]
p=ROOT/'dev/validation/strict-known.json';known=json.loads(p.read_text());records=[]
for mode in ['native','double']:
 key=f'density:limit|{mode}|pareto2|class|subnormal'
 row=fd.load(str(ROOT/'build/snapshots'/mode))['pareto2','class','subnormal'];r=row['result']
 assert r['outcome']['kind']=='failure' and r['outcome']['reason']=='evaluation',r
 data=inputs['class','subnormal'][0]['data'];minimum=row['held'].get('min',0)
 with mp.workdps(100):
  rate=len(data)/mp.fsum(mp.mpf(x)-mp.mpf(minimum) for x in data)
  assert rate>mp.mpf(sys.float_info.max)
  records.append(dict(key=key,rate=mp.nstr(rate,80),binary64_max=sys.float_info.max,reason=r['reason']))
 if key in known['known']:
  assert known['known'][key]['ref']=='TASK-214.16'
  del known['known'][key]
p.write_text(json.dumps(known,indent=1)+'\n')
(Path(__file__).parent/'unrepresentable-limit-pruning.json').write_text(json.dumps(records,indent=2)+'\n')
print('Verified and retired two obsolete infinite-rate density payload checks.')
