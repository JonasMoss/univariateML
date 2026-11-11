import json
from pathlib import Path
import mpmath as m
m.mp.dps=110
nu,xi,X=m.mpf(1.01),m.mpf(100),m.mpf(1e100)
a=m.gamma(2/nu)/m.sqrt(m.gamma(1/nu)*m.gamma(3/nu))
shift=a*(xi-1/xi);su=m.sqrt((1-a*a)*(xi*xi+xi**-2)+2*a*a-1)
# Two observations at0,X: the strictly increasing convex-power score is zero
# at c/(X-c)=xi**(-2*nu/(nu-1)), independently of the held scale.
mode=X/(1+xi**(2*nu/(nu-1)));mean=mode+shift/su
r=dict(mean=str(mean),mode=str(mode),sd=1,nu=str(nu),xi=100,precision_digits=110)
p=Path(__file__).resolve().parent/'loose-reference.json';p.write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
