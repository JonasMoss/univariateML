"""Resolve R fitter cusp coefficients against the same 300-digit density."""
import csv,json,sys
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
mp.mp.dps=300;rows=[]
for r in json.loads((H/'sged-cusp-js-raw.json').read_text()):
 r={k:float(v) for k,v in r.items() if k!='status'};m,s,n,t=map(mp.mpf,(r['mean'],r['sd'],r['nu'],r['xi']));moment=mp.exp(mp.loggamma(2/n)-(mp.loggamma(1/n)+mp.loggamma(3/n))/2);shift=moment*(t-1/t);sigma=mp.sqrt((1-moment*moment)*(t*t+1/(t*t))+2*moment*moment-1);ls=(mp.loggamma(1/n)-mp.loggamma(3/n))/2;ref=mp.mpf(0)
 for x in (-3.,-1.,-.2,0.,.3,.4,2.):
  z=(mp.mpf(x)-m)/s*sigma+shift;v=z/(t if z>=0 else 1/t)
  ref+=mp.log(2/(t+1/t))-mp.log(2)-ls-mp.loggamma(1+1/n)-mp.exp(n*(mp.log(abs(v))-ls))-mp.log(s)+mp.log(sigma)
 r.update(reference=float(ref),fitter_error=audit.error(r['reported'],float(ref)),dpqr_error=audit.error(r['dpqr'],float(ref)));rows.append(r)
(H/'sged-cusp-js.json').write_text(json.dumps(rows,indent=2)+'\n');print('JS cusp: fitter',sum(r['fitter_error']>1e-10 for r in rows),'dpqr',sum(r['dpqr_error']>1e-10 for r in rows))
assert all(r['dpqr_error']<=1e-10 for r in rows)
