"""Independent standardized-log-data stationary search, mpmath 90 digits."""
from pathlib import Path
import json,mpmath as m,signal
P=Path(__file__).resolve().parent;m.mp.dps=90
cases=[c for c in json.loads((P/'cases.json').read_text()) if c['index']=='route-burr-nearly-constant']
cases.append(dict(index='forced-zero-cover',data=[.2,.5,1.,2.,4.]))
results=[]
def timeout(*args):raise TimeoutError('60-second root budget')
signal.signal(signal.SIGALRM,timeout)
for case in cases:
 x=list(map(m.mpf,case['data']));logs=list(map(m.log,x));mean=m.fsum(logs)/len(x);sd=m.sqrt(m.fsum((t-mean)**2 for t in logs)/len(x));t=[(v-mean)/sd for v in logs]
 def f(u,v,d):
  k=m.exp(u);c=m.exp(v)
  return m.fsum(u+v-z-m.log1p(m.exp(-(c*z-d)))-k*m.log1p(m.exp(c*z-d)) for z in t)
 def g(u,v,d):return tuple(m.diff(lambda h:f(*[h if j==i else [u,v,d][j] for j in range(3)]),[u,v,d][i]) for i in range(3))
 for start in ['.1','.5','1','2','10','100']:
  out=dict(id=case['index'],start=start)
  try:
   signal.alarm(60)
   z=m.findroot(g,(m.log(m.mpf(start)),m.log(2),m.log(m.mpf(start))),tol=m.mpf('1e-65'),maxsteps=70)
   H=m.matrix([[ -m.diff(lambda a:g(*[a if j==i else z[j] for j in range(3)])[k],z[i]) for i in range(3)] for k in range(3)])
   try:m.cholesky(H);out['positive_information']=True
   except ValueError:out['positive_information']=False
   out['reference']=dict(shape1=m.nstr(m.exp(z[0]),75),shape2=m.nstr(m.exp(z[1])/sd,75),scale=m.nstr(m.exp(mean+z[2]*sd/m.exp(z[1])),75))
   out['residual']=m.nstr(max(map(abs,g(*z))),8)
  except Exception as e:out['open']=str(e)
  signal.alarm(0);results.append(out);print(out,flush=True)
  (P/'burr-stationary-evidence.json').write_text(json.dumps(results,indent=2)+'\n')
  if out.get('positive_information'):break
