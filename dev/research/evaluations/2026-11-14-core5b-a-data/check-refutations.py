#!/usr/bin/env python3
"""Independently verify each feasible density witness refuting a lognormal face."""
from pathlib import Path
import sys,json,gzip,collections,importlib.util,math
ROOT=Path(__file__).resolve().parents[4];sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('density',ROOT/'dev/validation/fit-density.py');fd=importlib.util.module_from_spec(spec);spec.loader.exec_module(fd)
archive=collections.defaultdict(list)
for c in json.loads(gzip.decompress(fd.fs.ARCHIVE.read_bytes())):archive[c['family']].append(c)
inputs=fd.inputs('fpareto',archive)[1];records=[]
for mode in ['native','double']:
 for key,row in fd.load(str(ROOT/'build/snapshots'/mode)).items():
  family,source,case=key;r=row['result']
  if family!='fpareto' or not r.get('message','').startswith('A finite feasible Feller-Pareto point refutes'):continue
  data=inputs[source,case][0]['data'];point={k:fd.number(v) for k,v in r['candidates'][0]['estimates'].items()}
  ref,error=fd.attribute('fpareto',point,data)
  minimum=point['min'];face={k:fd.number(v) for k,v in r['limit_estimates'].items()}
  if 'meanlog' not in face:raise RuntimeError('refuted face coordinates missing: '+str(key))
  face_ref,face_error=fd.attribute('lnorm',face,[x-minimum for x in data])
  held_ok=all(point[n]==v for n,v in row['held'].items())
  passed=ref is not None and face_ref is not None and ref>face_ref and held_ok
  records.append(dict(mode=mode,source=source,case=case,point=point,reference=ref,face_reference=face_ref,gain=ref-face_ref if ref is not None and face_ref is not None else None,held_ok=held_ok,error=error,face_error=face_error,passed=passed))
(Path(__file__).parent/'refutation-references.json').write_text(json.dumps(records,indent=2)+'\n')
failed=[r for r in records if not r['passed']]
print(f'{len(records)} feasible-witness comparisons; {len(failed)} failures')
for r in failed:print(r)
raise SystemExit(bool(failed))
