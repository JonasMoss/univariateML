#!/usr/bin/env python3
"""Retry guarded rows with the ORIGINAL census executable, without rebuilding.

Run under /tmp/umvl-cpu/run. Wall guards are audit outcomes, never fit statuses.
"""
import argparse,hashlib,json,math,subprocess,sys,time
from pathlib import Path
root=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(root/'dev/validation'))
import importlib.util
spec=importlib.util.spec_from_file_location('seam',root/'dev/validation/seam-check.py');seam=importlib.util.module_from_spec(spec);spec.loader.exec_module(seam)
p=argparse.ArgumentParser();p.add_argument('input',type=Path);p.add_argument('output',type=Path);p.add_argument('--timeout',type=float,default=120);p.add_argument('--provenance',type=Path);p.add_argument('--case',type=int)
a=p.parse_args()
provenance=json.loads((a.provenance or a.input.with_suffix('.provenance.json')).read_text())
tag=hashlib.sha256(','.join(provenance['families']).encode()).hexdigest()[:12]+'-'+provenance['source_sha256'][:12]
exe=seam.WORK/('probe-'+tag+('-double' if provenance['no_long_double'] else ''))
assert exe.is_file(),exe
rows=[json.loads(l) for l in a.input.read_text().splitlines()]
records=[]
with a.output.open('w') as dest:
 for c in rows:
  if c['outcome']!='open fit' or (a.case is not None and c['index']!=a.case):continue
  meta,_=seam.read(seam.DISTS/c['family']);names=[p['name'] for p in meta['params']]
  holds=[float(c['held'].get(n,0)) for n in names];mask=sum(1<<i for i,n in enumerate(names) if n in c['held'])
  line=' '.join(map(str,[c['family'],mask,len(names),len(c['data']),*holds,*c['data']]))+'\n'
  r={k:c[k] for k in ('family','source','index','data','held','sample') if k in c}
  start=time.time()
  try:
   process=subprocess.run([str(exe)],input=line,text=True,capture_output=True,check=True,timeout=a.timeout)
   cells=process.stdout.strip().split('\t')
   if cells[0]!='ok':r.update(outcome='stop',reason='\t'.join(cells[1:]))
   else:
    values=list(map(float,cells[1:]));pars=values[1:1+len(names)];density=math.fsum(values[1+len(names):])
    outside=seam.support_violations(meta,pars,c['data'])
    r.update(outcome='pass',reported=values[0],parameters=pars,dpqr=density,error=seam.error(values[0],density),outside_support=outside)
    if r['error']>1e-10 or outside:
     r.update(outcome='mismatch');seam.mp.mp.dps=300
     value=seam.reference(seam.DISTS/c['family'],meta,dict(zip(names,pars)),c['data'])
     r.update(reference=value,fitter_error=seam.error(r['reported'],value),dpqr_error=seam.error(density,value))
     r['attribution']='support' if value==-math.inf else 'both' if r['fitter_error']>1e-10 and r['dpqr_error']>1e-10 else 'fitter' if r['fitter_error']>1e-10 else 'dpqr' if r['dpqr_error']>1e-10 else 'summation margin'
  except subprocess.TimeoutExpired:r.update(outcome='open fit',reason=f'no terminal result within {a.timeout}s')
  r.update(guard_seconds=a.timeout,seconds=time.time()-start,probe=str(exe.relative_to(root)))
  records.append(r);dest.write(json.dumps(r)+'\n');dest.flush()
  print(r['family'],r['index'],r['outcome'],r['seconds'],flush=True)
a.output.with_suffix('.provenance.json').write_text(json.dumps(dict(provenance,timeout=a.timeout,retry_input=str(a.input)),indent=2)+'\n')
