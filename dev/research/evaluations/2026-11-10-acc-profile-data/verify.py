#!/usr/bin/env python3
"""Validate all four binding modes and optionally apply measured golden policy.

Captures are produced by capture.py/capture.R/capture.mjs. No golden or truth
fixture is rewritten. The 1,2,3,5,10 allowance lattice is the TASK-149.1 policy.
"""
import argparse,hashlib,json,math,re,subprocess
from pathlib import Path
import mpmath as mp
root=Path(__file__).resolve().parents[4];here=Path(__file__).resolve().parent
ids='sn skellam sgomp dgamma pareto3 zinb zmnbinom ztnbinom ht emg genbeta'.split()
mp.mp.dps=80
args=argparse.ArgumentParser();args.add_argument('--apply',action='store_true');args=args.parse_args()
def err(a,b):
 a,b=mp.mpf(float(a)),mp.mpf(b)
 return float(abs(a-b)/(abs(b) if b else 1))
def allowance(e):
 if e==0:return 0
 base=10**math.floor(math.log10(e))
 return next(base*k for k in (1,2,3,5,10) if base*k>=e)
captures={}
for mode in ['native','double','r','wasm']:
 p=here/f'after-{mode}.jsonl'
 rows=[json.loads(x) for x in p.read_text().splitlines()]
 assert len(rows)==11 and {r['id'] for r in rows}==set(ids),(mode,'capture incomplete')
 assert all(not r['error'] for r in rows),(mode,'failed fit')
 captures[mode]={r['id']:r for r in rows}
records=[]
for id in ids:
 p=root/f'univariateML-cpp/include/cppunivariate/distributions/{id}/golden.json'
 original=subprocess.check_output(['git','--no-optional-locks','show','HEAD:'+str(p.relative_to(root))],cwd=root)
 assert original==p.read_bytes(),(id,'frozen golden changed')
 g=json.loads(original);reference=g['reference'];held=g.get('fit_args',{})
 errors={};components={}
 for mode,c in captures.items():
  r=c[id]
  components[mode]={k:max(err(r['estimates'][k],v),err(r['estimates'][k],g['estimates'][k])) for k,v in reference['estimates'].items() if k not in held}
  components[mode]['logLik']=max(err(r['logLik'],reference['logLik']),err(r['logLik'],g['logLik']))
  errors[mode]=max(components[mode].values())
  assert errors[mode]<=1e-10,(id,mode,errors[mode])
 measured=max(errors.values());tol=allowance(1.5*measured)
 assert measured==0 or tol<=3*measured
 records.append(dict(id=id,golden_sha256=hashlib.sha256(original).hexdigest(),components=components,errors=errors,allowance=tol))
if args.apply:
 p=root/'univariateML-cpp/tests/test_support.hpp';s=p.read_text()
 for r in records:s=re.sub(r'(constexpr double kGolden_'+r['id']+r' = )[^;]+;',lambda m:m[1]+f'{r["allowance"]:.0e};',s)
 if p.read_text()!=s:p.write_text(s)
 p=root/'bindings/r/tests/testthat/test_golden_regression.R';s=p.read_text();i=s.index('GOLDEN_STOP_TOLERANCE');a,b=s[:i],s[i:]
 for r in records:a=re.sub(r'(^  '+r['id']+r' = )[^,\n]+',lambda m:m[1]+f'{r["allowance"]:.0e}',a,flags=re.M)
 if p.read_text()!=a+b:p.write_text(a+b)
(here/'checks.json').write_text(json.dumps(dict(modes=list(captures),goldens_unchanged=True,all_fits_within_1e_10=True,allowance_headroom_max=3,families=records),indent=2)+'\n')
print('Verified 44 default fits; all errors <=1e-10; all eleven golden files unchanged.')
for r in records:print(r['id'],f'{max(r["errors"].values()):.3g}',f'{r["allowance"]:.0e}')
