"""Recompute edge references from exact binary64 inputs (1000 decimal digits).
Run from repository root through /tmp/umvl-cpu/run python3 <this file>.
No production kernel or other distribution implementation is called.
"""
import json, math
from pathlib import Path
import mpmath as mp
mp.mp.dps=1000
cases=[(math.exp(10),1.,1e308,1e-308),
       (1e-200,1.,2.,math.ulp(0.)),
       (1e-308,1.,1.,1e308),
       (1e-160,1.,2.,1e308),
       (700.,1.,1e308,1e-308)]
rows=[]
for xx,aa,bb,cc in cases:
    x,a,b,c=map(mp.mpf,(xx,aa,bb,cc))
    z=b*mp.log(x/a)
    # For z>10000 the softplus correction < exp(-10000), negligible
    # at 1000 digits even after multiplication by the binary64 exponent.
    L=z if z>10000 else mp.log1p(mp.exp(z))
    v=c*L
    H=mp.expm1(v)
    logq=0 if z>10000 else z-L
    ld=mp.log(b)+mp.log(c)-mp.log(x)+logq+v-H
    rows.append(dict(x=xx,scale=aa,shape=bb,exponent=cc,
                     log_density=mp.nstr(ld,60),log_lower=mp.nstr(mp.log(-mp.expm1(-H)),60),
                     log_upper=mp.nstr(-H,60)))
c=math.ulp(0.)
lp=math.log(c)+math.log(1000.)
H=-mp.log1p(-mp.exp(mp.mpf(lp)))
x=mp.expm1(mp.log1p(H)/mp.mpf(c))**mp.mpf('.5')
h=mp.mpf(3.000000004)
guard_q=mp.mpf(1e-308)*mp.expm1(mp.log1p(h)/2)**(1/mp.mpf(1e-12))
center_lp=math.log(c)+math.log(math.log(2.))
center_h=-mp.log1p(-mp.exp(mp.mpf(center_lp)))
center_q=mp.expm1(mp.log1p(center_h)/mp.mpf(c))**(1/mp.mpf(1e-4))
result=dict(subnormal_center=dict(log_lower=center_lp,scale=1,shape=1e-4,exponent=c,quantile=mp.nstr(center_q,60)),inverse_cancellation=dict(log_upper=-3.000000004,scale=1e-308,shape=1e-12,exponent=2,exact_quantile=mp.nstr(guard_q,60)),dps=mp.mp.dps,values=rows,subnormal_inverse=dict(log_lower=lp,scale=1,shape=2,exponent=c,quantile=mp.nstr(x,60)))
Path(__file__).with_suffix('.json').write_text(json.dumps(result,indent=2)+'\n')
