"""Independent 80-digit joint root on the exact native quantile observations."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
here=Path(__file__).resolve().parent
x=[mp.mpf(float(s)) for s in Path('build/scratch/t-close-quantile.txt').read_text().split()]
n=len(x)
def score(m,t,v):
 s2=mp.exp(2*t);a=v*s2;rm=[];rr=[];logs=[]
 for y in x:
  r=y-m;r2=r*r;den=a+r2
  rm.append(r/den);rr.append(r2/den);logs.append(mp.log1p(r2/a))
 R=mp.fsum(rr)
 cp=(mp.digamma((v+1)/2)-mp.digamma(v/2))/2
 return -(v+1)*mp.fsum(rm),n-(v+1)*R,-n*(cp-1/(2*v))+(mp.fsum(logs)-(v+1)*R/v)/2
m,t,v=mp.findroot(score,(mp.mpf('0'),mp.log(mp.mpf('1.00131588021')),mp.mpf('539.46057298')),tol=mp.mpf('1e-60'),maxsteps=15)
s=mp.exp(t);res=max(map(abs,score(m,t,v)));assert res<mp.mpf('1e-55')
rows=json.loads((here/'replay.json').read_text())
for row in rows:
 if row['name']=='quantile-4000':
  for j,value in enumerate([m,s,v]):assert mp.mpf(row['box'][2*j])<value<mp.mpf(row['box'][2*j+1]),(row,j,value)
result=dict(precision_digits=80,n=n,coefficients=dict(mu=str(m),scale=str(s),nu=str(v),sd=str(s*mp.sqrt(v/(v-2)))),max_abs_score=str(res),both_native_boxes_enclose_root=True)
(here/'quantile-root.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
