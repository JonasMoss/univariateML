"""Finite affine quotient despite an overflowing raw displacement, 240 digits."""
import json,sys
from pathlib import Path
import mpmath as mp
H=Path(__file__).resolve().parent;sys.path.insert(0,str(H));import audit
mp.mp.dps=240;m=mp.sqrt(2/mp.pi);xi=mp.mpf(2);sigma=mp.sqrt((1-m*m)*(xi*xi+1/(xi*xi))+2*m*m-1)
v=(2*sigma+m*(xi-1/xi))/xi
ld=mp.log(2/(xi+1/xi))-mp.log(2*mp.pi)/2-v*v/2-mp.log(mp.mpf(1e308))+mp.log(sigma)
p=1-2*xi/(xi+1/xi)*mp.erfc(v/mp.sqrt(2))/2
pars=[-1e308,1e308,float.fromhex('0x1.fffffffffffffp+1023'),2.]
jobs=[('sstd',0,1e308,True,True,pars),('sstd',1,1e308,True,False,pars)]
rows=[dict(job=j,actual=a,expected=float(b),error=audit.error(a,float(b),j[1]==1)) for j,a,b in zip(jobs,audit.evaluate(jobs),(ld,p))]
(H/'skew-overflow.json').write_text(json.dumps(rows,indent=2)+'\n');print(rows)
assert all(r['error']<=1e-10 for r in rows)
