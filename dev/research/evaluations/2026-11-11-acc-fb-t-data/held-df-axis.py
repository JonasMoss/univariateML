"""A seeded held-subset skew-t root near the normal face (nu=1000)."""
from pathlib import Path
import random,json,runpy,mpmath as m
D=Path(__file__).resolve().parent
objective=runpy.run_path(str(D/'oracle.py'),run_name='oracle_lib')['objective'];m.mp.dps=90
rng=random.Random(33407);delta=2/m.sqrt(5)
x=[m.mpf(float(delta*abs(rng.gauss(0,1))+m.sqrt(1-delta*delta)*rng.gauss(0,1))) for j in range(64)]
j=max(range(len(x)),key=lambda j:x[j]);target=m.mpf(1000)
def data(t):return [t if k==j else v for k,v in enumerate(x)]
def score(t):return m.diff(lambda d:-objective('st',data(t),[m.mpf(0),m.mpf(1),m.mpf(2),d]),target)
t=m.findroot(score,(3,5),tol=m.mpf('1e-75'),maxsteps=100)
y=list(map(m.mpf,map(float,data(t))))
fun=lambda d:-objective('st',y,[m.mpf(0),m.mpf(1),m.mpf(2),d])
root=m.findroot(lambda d:m.diff(fun,d),target,tol=m.mpf('1e-75'),maxsteps=100)
assert m.diff(fun,root,2)>0
row=dict(index=33101,family='st',mask=7,held={'xi':0.,'omega':1.,'alpha':2.},data=list(map(float,y)),sample='held-df-axis-seed-33407')
(D/'held-df-axis-case.json').write_text(json.dumps(row,indent=2)+'\n')
(D/'held-df-axis-reference.json').write_text(json.dumps(dict(seed=33407,shape=str(root),information=str(m.diff(fun,root,2)),tuned_extreme=str(t)),indent=2)+'\n')
print(root,flush=True)
