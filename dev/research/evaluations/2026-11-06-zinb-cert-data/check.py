"""Independent 70-digit stationary-branch, face and tail checks. Seed 1167."""
import json,math,random,statistics,subprocess,sys
from pathlib import Path
import mpmath as m
m.mp.dps=70
rng=random.Random(1167)
base=Path(__file__).resolve().parent
root=base.parents[3]
M=lambda x:m.mpf(float(x))
def bisect(f,a,b):
 fa=f(a)
 for _ in range(240):
  c=(a+b)/2;fc=f(c)
  if (fc>0)==(fa>0):a=c;fa=fc
  else:b=c
 return (a+b)/2

def nbll(x,r,p,pi):
 return sum(m.log(pi+(1-pi)*p**r) if v==0 else m.log1p(-pi)+m.loggamma(r+v)-m.loggamma(r)-m.loggamma(v+1)+r*m.log(p)+v*m.log1p(-p) for v in x)
def nb_roots(x,r,pi):
 n=len(x);k=sum(v>0 for v in x);S=sum(x)
 if pi==0 or k==n:return [r/(r+m.mpf(S)/n)]
 u=k*r/(k*r+S);v=n*r/(n*r+S)
 B=r*(u+v)-(v-u);D=B*B-4*r*r*u*v
 cuts=[u,v]
 if D>0:cuts=sorted([u,v]+[p for p in [(B-m.sqrt(D))/(2*r),(B+m.sqrt(D))/(2*r)] if u<p<v])
 E=lambda p:pi/(1-pi)*(k*r-(k*r+S)*p)+p**r*(n*r-(n*r+S)*p)
 return [bisect(E,a,b) for a,b in zip(cuts,cuts[1:]) if E(a)>0 and E(b)<0]
def zipll(x,t,pi):
 return sum(m.log(pi+(1-pi)*m.exp(-t)) if v==0 else m.log1p(-pi)-t+v*m.log(t)-m.loggamma(v+1) for v in x)
def zip_roots(x,pi):
 n=len(x);k=sum(v>0 for v in x);S=sum(x)
 if pi==0 or k==n:return [m.mpf(S)/n]
 u=m.mpf(S)/n;v=m.mpf(S)/k
 B=-m.mpf(S)*(n+k);C=S*S+S*(n-k);D=B*B-4*n*k*C
 cuts=[u,v]
 if D>0:cuts=sorted([u,v]+[t for t in [(-B-m.sqrt(D))/(2*n*k),(-B+m.sqrt(D))/(2*n*k)] if u<t<v])
 # Multiplying the score by t*(a+exp(-t)) gives this continuous E.
 E=lambda t:pi/(1-pi)*(S-k*t)+m.exp(-t)*(S-n*t)
 return [bisect(E,a,b) for a,b in zip(cuts,cuts[1:]) if E(a)>0 and E(b)<0]

cases=[]
g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/zinb/golden.json').read_text())
# Inspect fixture schema rather than relying on estimates ordering.
data=g.get('data',g.get('x'))
est=g['estimates']
for held in range(4):cases.append(dict(name='golden-'+str(held),x=data,r=est['size'],p=est['prob'],pi=est['pi'],held=held))
for pi in [.7,.8,.76839477568758077746,.76839477568758077746-1e-8,.76839477568758077746+1e-8]:
 cases.append(dict(name='competing-'+repr(pi),x=[0]*99+[10],r=10.,p=.5,pi=pi,held=2))
for held in [1,3]:cases.append(dict(name='held-prob-golden-'+str(held),x=data,r=-1.,p=est['prob'],pi=est['pi'],held=held))
for i in range(16):
 x=[0]*rng.randrange(1,100)+[rng.randrange(1,25) for _ in range(rng.randrange(1,20))]
 cases.append(dict(name='held-prob-'+str(i),x=x,r=-1.,p=rng.uniform(.05,.95),pi=rng.uniform(.001,.99),held=3))
for i in range(64):
 r=10**rng.uniform(-1.3,1.5);pi=rng.uniform(.001,.97)
 x=[0]*rng.randrange(0,100)+[rng.randrange(1,25) for _ in range(rng.randrange(1,30))]
 cases.append(dict(name='seeded-'+str(i),x=x,r=r,p=.5,pi=pi,held=2))
lines=[' '.join(map(str,[len(c['x']),c['r'],c['p'],c['pi'],c['held']]+c['x'])) for c in cases]
run=subprocess.run([sys.argv[1]],input='\n'.join(lines)+'\n',text=True,capture_output=True,check=True)
(base/'probe.txt').write_text(run.stdout)
rows=run.stdout.splitlines();assert len(rows)==len(cases)
records=[];errors=[];pi_errors=[];ll_errors=[]
for c,line in zip(cases,rows):
 q=line.split();status,failure=int(q[0]),int(q[1]);
 if 'golden' in c['name']:assert status==0,(c['name'],q)
 r=M(c['r']);pi=M(c['pi']);p=M(c['p']);x=c['x']
 if c['r']<0:
  n=len(x);k=sum(v>0 for v in x);S=sum(x);t=-m.log(p);n0=n-k
  def size_score(r):
   H=sum(m.digamma(r+v)-m.digamma(r) for v in x if v>0)
   if c['held']&2:
    q=(1-pi)*p**r/(pi+(1-pi)*p**r)
    return H-t*(k+n0*q)
   if p**r>=m.mpf(n0)/n:return H-n*t
   return H-k*t/(1-p**r)
  lo=m.mpf(k)/(2*n*t);hi=2*m.mpf(S)/(k*t)
  grid=[lo*(hi/lo)**(m.mpf(j)/160) for j in range(161)]
  roots=[bisect(size_score,a,b) for a,b in zip(grid,grid[1:]) if size_score(a)>0 and size_score(b)<0]
  assert roots
 elif c['held']&1:roots=[p]
 elif c['held']&2:roots=nb_roots(x,r,pi)
 else:
  n=len(x);z=m.mpf(x.count(0))/n;S=sum(x);k=n-x.count(0)
  pnb=r/(r+m.mpf(S)/n)
  if pnb**r>=z:roots=[pnb]
  else:
   mean=lambda p:r*(1-p)/p/(1-p**r)-m.mpf(S)/k
   roots=[bisect(mean,m.mpf('1e-50'),1-m.mpf('1e-50'))]
 def getpi(p):return pi if c['held']&2 else max(m.mpf(0),(m.mpf(x.count(0))/len(x)-p**r)/(1-p**r))
 if c['r']<0:
  def pi_size(rr):return pi if c['held']&2 else max(m.mpf(0),(m.mpf(x.count(0))/len(x)-p**rr)/(1-p**rr))
  vals=[nbll(x,rr,p,pi_size(rr)) for rr in roots]
 else:vals=[nbll(x,r,p,getpi(p)) for p in roots]
 winner=max(range(len(vals)),key=vals.__getitem__)
 rec=dict(c,status=status,failure=failure,roots=list(map(str,roots)),root_loglik=list(map(str,vals)),new_us=float(q[12]),legacy_us=float(q[13]))
 if status==0:
  fitp=M(float(q[2]));fitpi=M(float(q[3]));ll=M(float(q[4]));lower=M(float(q[5]));upper=M(float(q[6]))
  fitr=M(float(q[14]))
  error=float(abs((fitr if c['r']<0 else fitp)-roots[winner])/roots[winner]);errors.append(error)
  assert error<2e-7,(c['name'],error)
  expectedpi=pi_size(roots[winner]) if c['r']<0 else getpi(roots[winner])
  pi_error=float(abs(fitpi-expectedpi));pi_errors.append(pi_error)
  ll_error=float(abs(ll-vals[winner])/len(x));ll_errors.append(ll_error)
  assert pi_error<2e-7,(c['name'],pi_error)
  assert ll_error<1e-9,(c['name'],ll_error)
  assert lower<=vals[winner]<=upper,(c['name'],lower,vals[winner],upper)
  assert abs(ll-nbll(x,fitr,fitp,fitpi))<m.mpf('1e-9')*len(x)
  rec.update(relative_probability_error=error,supremum_lower=str(lower),supremum_upper=str(upper))
 elif c['name'].startswith('competing-0.7683947756875808'):assert failure==3,(c['name'],q)
 records.append(rec)
# Independent same-inflation ZIP root enumeration and uniform tail inequality.
faces=[]
for i in range(24):
 x=[0]*rng.randrange(1,100)+[rng.randrange(1,20) for _ in range(rng.randrange(1,20))]
 pi=M(rng.uniform(.05,.95));roots=zip_roots(x,pi)
 assert roots
 A=sum(m.mpf(v)*(v-1)/2 for v in x)
 for rr in [m.mpf('.1'),m.mpf(1),m.mpf(10),m.mpf(1000)]:
  for t in [m.mpf('.1'),m.mpf(1),m.mpf(10)]:
   assert nbll(x,rr,m.exp(-t/rr),pi)<=zipll(x,t,pi)+A/rr+m.mpf('1e-60')
 faces.append(dict(x=x,pi=str(pi),roots=list(map(str,roots)),loglik=[str(zipll(x,t,pi)) for t in roots],tail_A=str(A)))
summary=dict(seed=1167,precision=70,calls=len(records),success=sum(c['status']==0 for c in records),failures={str(i):sum(c['failure']==i for c in records) for i in range(4)},max_relative_coefficient_error=max(errors),max_absolute_inflation_error=max(pi_errors),max_loglik_error_per_observation=max(ll_errors),median_new_us=statistics.median(c['new_us'] for c in records),median_legacy_us=statistics.median(c['legacy_us'] for c in records),tail_inequalities=24*4*3)
(base/'oracle.json').write_text(json.dumps(dict(summary=summary,cases=records,zip_faces=faces),indent=2)+'\n')
print(json.dumps(summary,indent=2))
