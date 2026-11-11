"""Tune binary64 samples whose independent shape root is just above one."""
from pathlib import Path
import json,runpy,mpmath as m
D=Path(__file__).resolve().parent
objective=runpy.run_path(str(D/'oracle.py'),run_name='oracle_lib')['objective']
m.mp.dps=70;target=m.mpf('1.0001');rows=[]
n=30
base=[m.tan(m.pi*((m.mpf(j)+m.mpf('.5'))/n-m.mpf('.5'))) for j in range(n)]
# Freeze the untuned coordinates as binary64 observations first.
base=list(map(lambda v:m.mpf(float(v)),base))
def student_data(t):return [-t]+base[1:-1]+[t]
def score(t):return m.diff(lambda d:-objective('lst',student_data(t),[m.mpf(0),m.mpf(1),d]),target)
t=m.findroot(score,(20,40),tol=m.mpf('1e-60'));x=list(map(float,student_data(t)))
rows += [dict(index=33000,family='lst',mask=2,held={'sigma':1.},data=x,sample='near-restriction-1.0001'),dict(index=33001,family='st',mask=7,held={'xi':0.,'omega':1.,'alpha':0.},data=x,sample='near-restriction-1.0001')]
a=1/m.sqrt(17);b=4*a;c=a-b
base=[]
for j in range(n):
 u=(m.mpf(j)+m.mpf('.5'))/n
 base.append(float(c+a*m.log(u/m.mpf('.2')) if u<m.mpf('.2') else c-b*m.log((1-u)/m.mpf('.8'))))
base=list(map(m.mpf,base))
def ged_data(t):return base[:-1]+[t]
def score(t):return m.diff(lambda d:-objective('sged',ged_data(t),[m.mpf(0),m.mpf(1),d,m.mpf(2)]),target)
t2=m.findroot(score,(base[-1],base[-1]*2),tol=m.mpf('1e-60'))
rows.append(dict(index=33002,family='sged',mask=11,held={'mean':0.,'sd':1.,'xi':2.},data=list(map(float,ged_data(t2))),sample='near-restriction-1.0001'))
(D/'near-boundary-cases.json').write_text(json.dumps(rows,indent=2)+'\n')
print('student extreme',str(t),'ged extreme',str(t2),flush=True)
for r in rows:
 f=r['family'];x=list(map(m.mpf,r['data']))
 fun=(lambda d:-objective('lst',x,[m.mpf(0),m.mpf(1),d])) if f!='sged' else (lambda d:-objective('sged',x,[m.mpf(0),m.mpf(1),d,m.mpf(2)]))
 root=m.findroot(lambda d:m.diff(fun,d),target,tol=m.mpf('1e-60'))
 print(f,'shape',str(root),'information',str(m.diff(fun,root,2)),flush=True)
 assert abs(root-target)<m.mpf('1e-12') and m.diff(fun,root,2)>0
