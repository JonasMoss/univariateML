"""Recheck every changed row at 200 decimal digits (run under slot lock)."""
import gzip,json
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
cases={c['index']:c for c in json.load(gzip.open(ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt')) if c['family']=='bnbinom'}
cases['golden']=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())
for mask in range(1,8):cases[f'golden-mask-{mask}']=cases['golden']
mp.mp.dps=200
out=[]
for mode,s in json.loads((HERE/'comparison.json').read_text()).items():
 rows={r['index']:r for r in json.loads((HERE/f'after-{mode}.json').read_text())}
 for c in s['changes']:
  r,a,b=map(lambda s:mp.mpf(float(s)),rows[c['index']]['coefficients'])
  # Independent beta-function pmf, evaluated with exact binary64 coefficients.
  ref=mp.fsum(mp.loggamma(r+x)-mp.loggamma(r)-mp.loggamma(x+1)+mp.log(mp.beta(r+a,x+b))-mp.log(mp.beta(a,b)) for x in cases[c['index']]['data'])
  err=abs(ref-mp.mpf(c['reference']))/max(1,abs(ref))
  assert err<mp.mpf('1e-88'),(mode,c['index'],err)
  out.append(dict(mode=mode,index=c['index'],reference_200=mp.nstr(ref,180),reference_relative_difference=float(err)))
(HERE/'reference-recheck.json').write_text(json.dumps(out,indent=2)+'\n')
print(len(out),'references agree at 88 significant decimal places')
