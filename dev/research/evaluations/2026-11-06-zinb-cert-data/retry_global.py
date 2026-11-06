"""Retry unresolved joint workloads at larger budgets; independently check successes."""
import collections,json,math,subprocess,sys,time
from pathlib import Path
import mpmath as m
m.mp.dps=70;base=Path(__file__).resolve().parent
source=json.loads((base/'global-oracle.json').read_text())
cases=[c for c in source['cases'] if c['status']==3 and not c['name'].startswith(('budget','two-peak'))]
trials=[]
for c in cases:
 for maxit in [2000,20000]:
  line=' '.join(map(str,[0,len(c['x']),c['mask'],c['r'],c['p'],c['pi'],maxit]+c['x']))+'\n'
  start=time.monotonic();a=subprocess.run([sys.argv[1]],input=line,text=True,capture_output=True,check=True);elapsed=time.monotonic()-start
  q=a.stdout.split();record=dict(name=c['name'],maxit=maxit,status=int(q[0]),failure=int(q[1]),work=int(q[2]),elapsed_seconds=elapsed,output=q)
  if int(q[0])==0:
   r,p,pi=map(m.mpf,q[3:6]);n=len(c['x']);z=m.mpf(c['x'].count(0))/n
   held=m.mpf(c['pi']) if c['mask']&4 else None
   def ll(a,b):
    rr=m.exp(a);t=m.exp(b);pp=m.exp(-t/rr);infl=held if held is not None else max(m.mpf(0),(z-m.exp(-t))/(1-m.exp(-t)))
    return sum(w*(m.log(infl+(1-infl)*pp**rr) if v==0 else m.log1p(-infl)+m.loggamma(rr+v)-m.loggamma(rr)-m.loggamma(v+1)+rr*m.log(pp)+v*m.log1p(-pp)) for v,w in collections.Counter(c['x']).items())
   aa,bb=m.findroot(lambda a,b:(m.diff(lambda t:ll(t,b),a),m.diff(lambda t:ll(a,t),b)),(m.log(r),m.log(-r*m.log(p))),tol=m.mpf('1e-45'),maxsteps=30)
   error=max(abs(r/m.exp(aa)-1),abs(p/m.exp(-m.exp(bb-aa))-1));assert error<m.mpf('2e-6')
   record['relative_error']=str(error)
  trials.append(record)
  print(json.dumps(record),flush=True)
  if int(q[0])!=3:break
(base/'global-retries.json').write_text(json.dumps(trials,indent=2)+'\n')
