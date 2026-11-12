"""80-digit positive Poisson-convolution reference for changed fit logLik rows."""
import json,math
from pathlib import Path
import mpmath as mp
HERE=Path(__file__).resolve().parent
mp.mp.dps=80
before={r['index']:r for r in json.loads((HERE/'fits-before-double.json').read_text()) if r['family']=='skellam'}
after={r['index']:r for r in json.loads((HERE/'fits-after-double.json').read_text()) if r['family']=='skellam'}
cases={r['index']:r for r in map(json.loads,(HERE/'fits-after.jsonl').read_text().splitlines()) if r['family']=='skellam'}
out=[]
for index in [6337,6351]:
 row=after[index];a,b=map(mp.mpf,row['coefficients']);ref=mp.mpf(0)
 for k in cases[index]['data']:
  term=mp.mpf(1);total=term
  for j in range(10000):
   ratio=a*b/((k+j+1)*(j+1))
   if ratio<1 and term*ratio/(1-ratio)<total*mp.mpf('1e-70'):break
   term*=ratio;total+=term
  else:raise ArithmeticError('positive-series remainder did not close')
  ref+=-a-b+k*mp.log(a)-mp.loggamma(k+1)+mp.log(total)
 out.append(dict(index=index,reference=mp.nstr(ref,75),before=before[index]['logLik'],after=row['logLik'],absolute_error_before=float(abs(mp.mpf(before[index]['logLik'])-ref)),absolute_error_after=float(abs(mp.mpf(row['logLik'])-ref))))
(HERE/'fit-loglik-reference.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
