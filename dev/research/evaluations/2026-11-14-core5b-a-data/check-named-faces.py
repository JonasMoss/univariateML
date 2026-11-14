#!/usr/bin/env python3
"""Independent mpmath values for newly named faces lacking a shared density ID."""
from pathlib import Path
import sys,json,gzip,collections,math,importlib.util
import mpmath as mp
ROOT=Path(__file__).resolve().parents[4];sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('density',ROOT/'dev/validation/fit-density.py');fd=importlib.util.module_from_spec(spec);spec.loader.exec_module(fd)
archive=collections.defaultdict(list)
for c in json.loads(gzip.decompress(fd.fs.ARCHIVE.read_bytes())):archive[c['family']].append(c)
rebuild={};records=[]
def data_for(family,source,case):
 if family not in rebuild:rebuild[family]=fd.inputs(family,archive)[1]
 return rebuild[family][source,case][0]['data']
def reference(model,coef,data):
 p={k:mp.mpf(fd.number(v)) for k,v in coef.items()};terms=[]
 for x in map(mp.mpf,data):
  if model=='unavailable:shifted-double-pareto':
   y=x-p['min'];z=mp.log(y/p['kink']);a=p['left_rate'];b=p['right_rate']
   terms.append(-mp.log(y)-mp.log(1/a+1/b)+(a*z if z<0 else -b*z))
  elif model=='unavailable:shifted-lognormal':
   y=x-p['min'];z=(mp.log(y)-p['meanlog'])/p['sdlog']
   terms.append(-mp.log(y)-mp.log(p['sdlog'])-mp.log(2*mp.pi)/2-z*z/2)
  elif model=='unavailable:two-piece-t2':
   xi=p['xi'];base=p['scale'];side=base/xi if x<p['mode'] else base*xi;z=(x-p['mode'])/side
   terms.append(mp.log(2)-mp.log(base/xi+base*xi)+mp.loggamma(mp.mpf('1.5'))-mp.loggamma(1)-mp.log(2*mp.pi)/2-mp.mpf('1.5')*mp.log1p(z*z/2))
  elif model in ['unavailable:reflected-half-normal','unavailable:reflected-half-t']:
   if x>p['mu']:return -mp.inf
   z=(p['mu']-x)/p['sigma']
   if model.endswith('normal'):terms.append(mp.log(2)-mp.log(p['sigma'])-mp.log(2*mp.pi)/2-z*z/2)
   else:
    nu=p['nu'];terms.append(mp.log(2)-mp.log(p['sigma'])+mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(mp.pi*nu)/2-(nu+1)/2*mp.log1p(z*z/nu))
  else:raise ValueError(model)
 return mp.fsum(terms)
models={'unavailable:shifted-double-pareto','unavailable:shifted-lognormal','unavailable:two-piece-t2','unavailable:reflected-half-normal','unavailable:reflected-half-t'}
for mode in ['native','double']:
 for key,row in fd.load(str(ROOT/'build/snapshots'/mode)).items():
  family,source,case=key;r=row['result']
  if family not in ['sstd','fpareto'] or r.get('kind')!='limit' or r.get('model') not in models:continue
  data=data_for(*key);coef=r['limit_estimates'];model=r['model'];reported=fd.number(r['limit_loglik'])
  with mp.workdps(100):a=reference(model,coef,data)
  with mp.workdps(160):b=reference(model,coef,data);stable=abs(a-b)<=mp.mpf('1e-80')*max(1,abs(b));value=float(b)
  relative=fd.relative(reported,value);passed=stable and relative<1e-10
  records.append(dict(mode=mode,family=family,source=source,case=case,model=model,reported=reported,reference=value if math.isfinite(value) else str(value),relative=relative if math.isfinite(relative) else str(relative),stable=bool(stable),passed=bool(passed)))
(Path(__file__).parent/'named-face-references.json').write_text(json.dumps(records,indent=2)+'\n')
failed=[r for r in records if not r['passed']]
print(f'{len(records)} named face comparisons at 100 and 160 digits; {len(failed)} failures')
for r in failed:print(r)
raise SystemExit(bool(failed))
