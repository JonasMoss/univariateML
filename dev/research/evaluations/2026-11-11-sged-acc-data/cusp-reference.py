import json,sys
from pathlib import Path
import mpmath as m
D=Path(__file__).resolve().parent;sys.path.insert(0,str(D.parent/'2026-11-11-acc-fb-t-data'))
from oracle import objective
m.mp.dps=110
x=list(map(m.mpf,[-3.,-1.,-.2,0.,.3,.4,2.]));nu,xi,mean=m.mpf(.5),m.mpf(.3),m.mpf(.1)
a=m.gamma(2/nu)/m.sqrt(m.gamma(1/nu)*m.gamma(3/nu));shift=a*(xi-1/xi);su=m.sqrt((1-a*a)*(xi*xi+xi**-2)+2*a*a-1)
sd=-(m.mpf(2)-mean)*su/shift
r=dict(sd=str(sd),logLik=str(objective('sged',x,[mean,sd,nu,xi])),precision_digits=110,knot=2)
(D/'cusp-maximum-reference.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
