"""Run after study.py after; check the unit-shape reachable-face lower bound."""
import subprocess,math,json
from pathlib import Path
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
lines=[];faces={};cases=[]
for delta in [-.001,-.0001,-.00005,-.00002,-.00001,-.000001,.0001,.00000001]:
 for tol in [1e-6,1e-8]:
  for maxit in [16,1000]:
   x=[1.,1.,1.,1.,6+delta];face=-5*(1+math.log(sum(x)/5))
   for family,held,data in [('burr','shape2',x),('betapr','shape1',x),('betapr','shape2',[1/v for v in x])]:
    key=f'{family}-{held}-{delta}-{tol}-{maxit}'
    faces[key]=face+(2*sum(map(math.log,x)) if held=='shape2' and family=='betapr' else 0)
    cases.append(dict(index=key,family=family,held={held:1},data=data,tol=tol,iterlim=maxit))
    lines.append(' '.join([family,key,'5','1',str(maxit),str(tol)]+list(map(repr,data))+[held,'1']))
r=subprocess.run([str(ROOT/'build/scratch/fallback-study/after-fit')],input='\n'.join(lines)+'\n',text=True,capture_output=True,check=True)
results=[]
for line in r.stdout.splitlines():
 row=line.split('\t');record=dict(index=row[0],outcome=row[2],row=line,face_loglik=faces[row[0]])
 if row[2]=='uncertified':
  ll=float(row[3]);margin=2048*math.ulp(1.)*(5+abs(ll));gap=ll-faces[row[0]]
  record.update(gain=gap,margin=margin,refuted=gap < -margin)
 results.append(record)
(HERE/'face-refutation-sweep.json').write_text(json.dumps(dict(cases=cases,results=results),indent=2)+'\n')
refuted=[r for r in results if r.get('refuted')]
print('cases',len(results),'uncertified',sum(r['outcome']=='uncertified' for r in results),'refuted',len(refuted))
for r in refuted:print(r)
assert not refuted
