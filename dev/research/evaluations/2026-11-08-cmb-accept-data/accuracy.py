"""Independent 60-digit moment equations at the selected golden-sample designs.
Run from repository root: /tmp/umvl-cpu/run python3 <this-file>.
This verifies coefficients at those designs, not global size selection.
"""
import json
from collections import Counter
from pathlib import Path
import mpmath as mp
mp.mp.dps=60
root=Path(__file__).parent
g=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/cmb/golden.json').read_text())
counts=Counter(g['data']);n=mp.mpf(len(g['data']))
rows=[]
for line in (root/'after.txt').read_text().splitlines()[:4]:
    mask,status,N,p,nu,ll,budget,seconds=line.split()
    mask=int(mask);N=int(N);p0=mp.mpf(p);v0=mp.mpf(nu)
    h=[mp.log(mp.binomial(N,k)) for k in range(N+1)]
    m=sum(k*c for k,c in counts.items())/n
    t=sum(h[k]*c for k,c in counts.items())/n
    def moments(e,v):
        w=[mp.exp(e*k+v*h[k]) for k in range(N+1)]
        z=sum(w)
        return sum(k*w[k] for k in range(N+1))/z,sum(h[k]*w[k] for k in range(N+1))/z
    e0=mp.log(p0/(1-p0))
    if mask==0:
        e,v=mp.findroot(lambda e,v:(moments(e,v)[0]-m,moments(e,v)[1]-t),(e0,v0),tol=mp.mpf('1e-50'))
    elif mask==1:
        e=mp.log(mp.mpf(str(g['estimates']['prob']))/(1-mp.mpf(str(g['estimates']['prob']))))
        v=mp.findroot(lambda v:moments(e,v)[1]-t,v0,tol=mp.mpf('1e-50'))
    elif mask==2:
        v=mp.mpf(str(g['estimates']['nu']))
        e=mp.findroot(lambda e:moments(e,v)[0]-m,e0,tol=mp.mpf('1e-50'))
    else:e,v=e0,v0
    pstar=1/(1+mp.exp(-e))
    pe=abs(pstar-p0)/(1+abs(pstar));ve=abs(v-v0)/(1+abs(v))
    assert pe<mp.mpf('1e-8') and ve<mp.mpf('1e-8')
    rows.append(dict(held_mask=mask,size=N,reference_prob=str(pstar),reference_nu=str(v),prob_scaled_error=float(pe),nu_scaled_error=float(ve)))
(root/'accuracy.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
