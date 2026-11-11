"""Independent exact-input MP density checks for unequal concentrating shapes."""
import json,math,sys,collections
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
mp.mp.dps=320;jobs=[];refs=[]
for p,q in [(1e8,2e8),(1e16,1.00000001e16),(1e23,2e23),(5.454590759462655e23,8.876547609650913e23),(3.0861599294133976e16,3.0861599286892316e16)]:
 for power in (1.,1e-8):
  aa,bb,cc=map(mp.mpf,(p,q,power));center=mp.exp(mp.log(aa/bb)/cc)
  if not mp.mpf('1e-250')<center<mp.mpf('1e250'):continue
  for scale in (1.,1000.):
   for z in (-2.,0.,2.):
    xx=center*mp.exp(mp.mpf(z)*mp.sqrt(1/aa+1/bb)/cc)*mp.mpf(scale);x=float(xx)
    if not 0<x<math.inf:continue
    for ident in ('betapr','gb2','fpareto'):
     if ident=='betapr' and power!=1:continue
     pars=[p,q,scale] if ident=='betapr' else [q,power,p,scale] if ident=='gb2' else [q,power,p,scale,0.]
     t=cc*mp.log(mp.mpf(x)/mp.mpf(scale));sp=lambda y:max(y,0)+mp.log1p(mp.exp(-abs(y)))
     want=mp.log(cc)-mp.log(mp.mpf(x))-mp.log(mp.beta(aa,bb))-aa*sp(-t)-bb*sp(t)
     jobs.append((ident,0,x,True,True,pars));refs.append(float(want))
rows=[];failures=[];named=[]
for job,got,want in zip(jobs,audit.evaluate(jobs),refs):
 error=audit.error(got,want);row=dict(job=job,actual=got,expected=want,error=error);rows.append(row)
 if math.isnan(got):named.append(row)
 elif error>1e-10:failures.append(row)
(H/'asymmetric-smoke.json').write_text(json.dumps(dict(checks=len(rows),failures=failures,named_precision_regions=named,worst_resolved=max(r['error'] for r in rows if not math.isnan(r['actual'])),rows=rows),indent=2)+'\n')
print('asymmetric density',len(rows),'checks;',len(failures),'finite failures;',len(named),'named NaNs; worst',max(r['error'] for r in rows if not math.isnan(r['actual'])),flush=True)
if failures:print(failures[:4]);raise SystemExit(1)
