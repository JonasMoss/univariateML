"""Independent fixed-k concave profiles; a finite grid is not global coverage."""
from pathlib import Path
import json,mpmath as m,signal
P=Path(__file__).resolve().parent;m.mp.dps=90
cases=[c for c in json.loads((P/'cases.json').read_text()) if c['index']=='route-burr-nearly-constant']
cases.append(dict(index='forced-zero-cover',data=[.2,.5,1.,2.,4.]))
def timeout(*args):raise TimeoutError('30 second conditional budget')
signal.signal(signal.SIGALRM,timeout)
results=[]
for case in cases:
 logs=[m.log(m.mpf(v)) for v in case['data']];n=len(logs);mean=m.fsum(logs)/n;sd=m.sqrt(m.fsum((v-mean)**2 for v in logs)/n);z=[(v-mean)/sd for v in logs]
 def soft(t):return max(t,0)+m.log1p(m.exp(-abs(t)))
 def conditional(u):
  k=m.exp(u)
  C0=1/(-min(z)*k) if k<.05 else m.mpf('1.2') if k>32 else m.mpf(2)
  d0=C0*min(z)+m.log(n*k) if k<.05 else m.log(k)+m.log(m.fsum(m.exp(C0*t) for t in z)/n) if k>32 else m.log(k)
  def g(v,d):
   C=m.exp(v);p=[1/(1+m.exp(-(C*t-d))) for t in z]
   return n+C*m.fsum(t*(1-(k+1)*r) for t,r in zip(z,p)),m.fsum((k+1)*r-1 for r in p)
  v,d=m.findroot(g,(m.log(C0),d0),tol=m.mpf('1e-65'),maxsteps=100)
  C=m.exp(v);score=n-k*m.fsum(soft(C*t-d) for t in z)
  return score,(v,d)
 rows=[];previous=None
 for exponent in range(-6,19):
  u=exponent*m.log(10);row=dict(log10_k=exponent)
  try:
   signal.alarm(30);score,solution=conditional(u);row['log_shape_score']=m.nstr(score,70)
   if previous and previous[1]>0 and score<0:
    root=m.findroot(lambda q:conditional(q)[0],(previous[0],u),tol=m.mpf('1e-55'),maxsteps=50)
    delta=m.mpf('1e-20');curvature=(conditional(root+delta)[0]-conditional(root-delta)[0])/(2*delta)
    assert previous[0]<root<u and curvature<0
    row['profile_second_derivative']=m.nstr(curvature,65);v,d=conditional(root)[1]
    row['local_maximum_reference']=dict(shape1=m.nstr(m.exp(root),70),shape2=m.nstr(m.exp(v)/sd,70),scale=m.nstr(m.exp(mean+d*sd/m.exp(v)),70))
   previous=(u,score)
  except Exception as e:row['open']=str(e)[:300];previous=None
  signal.alarm(0);rows.append(row);print(case['index'],row,flush=True)
 results.append(dict(id=case['index'],digits=90,profile=rows,claim='Strict conditional concavity; numerical finite k grid, no complete global exclusion.'))
 (P/'burr-profile-evidence.json').write_text(json.dumps(results,indent=2)+'\n')
