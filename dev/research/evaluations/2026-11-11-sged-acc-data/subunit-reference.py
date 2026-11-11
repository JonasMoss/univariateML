import sys,json
from pathlib import Path
import mpmath as m
D=Path(__file__).resolve().parent;sys.path.insert(0,str(D.parent/'2026-11-11-acc-fb-t-data'))
from oracle import objective
m.mp.dps=110
x=list(map(m.mpf,[-3.,-1.,-.2,0.,.3,.4,2.]));p=[m.mpf(.1),m.mpf(1),m.mpf(.9),m.mpf(1.3)]
fun=lambda ls:objective('sged',x,[p[0],m.exp(ls),p[2],p[3]])
z=m.findroot(lambda t:m.diff(fun,t),(.35,.45),tol=m.mpf('1e-95'))
r=dict(sd=str(m.exp(z)),logLik=str(fun(z)),score=str(m.diff(fun,z)),information=str(-m.diff(fun,z,2)),digits=110)
assert -m.diff(fun,z,2)>0
(D/'smooth-subunit-reference.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
