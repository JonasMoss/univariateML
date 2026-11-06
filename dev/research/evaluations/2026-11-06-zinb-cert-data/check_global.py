"""Independent 70-digit checks of the joint certificate and Hessian enclosures."""
import collections,json,math,random,subprocess,sys
from pathlib import Path
import mpmath as m
m.mp.dps=70
base=Path(__file__).resolve().parent;root=base.parents[3];rng=random.Random(11671)
M=lambda v:m.mpf(float(v))
def ll(x,a,b,pi):
 r=m.exp(a);t=m.exp(b);p=m.exp(-t/r);z=m.mpf(x.count(0))/len(x)
 if pi is None:pi=max(m.mpf(0),(z-m.exp(-t))/(1-m.exp(-t)))
 return sum(w*(m.log(pi+(1-pi)*p**r) if v==0 else m.log1p(-pi)+m.loggamma(r+v)-m.loggamma(r)-m.loggamma(v+1)+r*m.log(p)+v*m.log1p(-p)) for v,w in collections.Counter(x).items())
def pois(mu):
 L=math.exp(-mu);product=1.;k=0
 while product>L:k+=1;product*=rng.random()
 return k-1
def sample(n,r,p,pi):
 x=[0 if rng.random()<pi else pois(rng.gammavariate(r,(1-p)/p)) for _ in range(n)]
 if not sum(x):x[-1]=1
 return x

g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/zinb/golden.json').read_text());e=g['estimates']
cases=[]
for mask in range(8):cases.append(dict(name='golden-'+str(mask),x=g['data'],mask=mask,r=e['size'],p=e['prob'],pi=e['pi'],maxit=200))
for i in range(24):
 r=10**rng.uniform(-.6,1);p=rng.uniform(.12,.8);pi=rng.uniform(.05,.85)
 cases.append(dict(name='joint-'+str(i),x=sample(rng.randrange(60,180),r,p,pi),mask=4 if i%2 else 0,r=r,p=p,pi=pi,maxit=200))
for i in range(6):cases.append(dict(name='face-'+str(i),x=[0 if rng.random()<.4*(i%2) else pois(3) for _ in range(400)],mask=0,r=1,p=.5,pi=.4,maxit=200))
for mask in (0,4,5):cases.append(dict(name='budget-'+str(mask),x=g['data'],mask=mask,r=e['size'],p=e['prob'],pi=e['pi'],maxit=1))
for pi in [.7,.8,.76839477568758077746]:cases.append(dict(name='two-peak-'+str(pi),x=[0]*99+[10],mask=5,r=10,p=.5,pi=pi,maxit=200))
cases.append(dict(name='held-ZIP',x=[0]*50+[4]*50,mask=4,r=1,p=.5,pi=.5,maxit=200))
cases.append(dict(name='positive-tail',x=[0]*5000+[1]*3958+[2]*4063+[4]*1979,mask=0,r=1,p=.5,pi=.3,maxit=200,mode=3))
for i in range(3):cases.append(dict(name='generated-'+str(i),x=[],mask=0,r=1,p=i,pi=.4,maxit=200,mode=2))
lines=[' '.join(map(str,[c.get('mode',0),len(c['x']),c['mask'],c['r'],c['p'],c['pi'],c['maxit']]+c['x'])) for c in cases]
bounds=[]
for i in range(24):
 x=sample(50,1.3,.35,.4);a=rng.uniform(-2,3);b=rng.uniform(-2,3);wa=rng.uniform(.001,.7);wb=rng.uniform(.001,.7)
 c=dict(name='box-'+str(i),x=x,mask=4 if i%2 else 0,pi=rng.uniform(.05,.95),al=a-wa,ah=a+wa,bl=b-wb,bh=b+wb)
 bounds.append(c);lines.append(' '.join(map(str,[1,len(x),c['mask'],1,.5,c['pi'],200]+x+[c['al'],c['ah'],c['bl'],c['bh']])))
a=subprocess.run([sys.argv[1]],input='\n'.join(lines)+'\n',text=True,capture_output=True,check=True)
(base/'global-probe.txt').write_text(a.stdout);rows=a.stdout.splitlines();assert len(rows)==len(lines)
records=[];errors=[];pi_errors=[];ll_errors=[]
for c,line in zip(cases,rows):
 q=line.split();status=int(q[0]);failure=int(q[1]);
 if c.get('mode')==2:
  c['x']=list(map(int,q[13:]));assert len(c['x'])==int(q[12])
 rec=dict(c,status=status,failure=failure,work=int(q[2]))
 if c['name']=='generated-1':assert status==5,(c['name'],q[:12])
 if c['name']=='generated-2':assert status==0,(c['name'],q[:12])
 if c['name'] in ('generated-0','positive-tail'):
  x=c['x'];S=sum(x);k=sum(v>0 for v in x);n=len(x);z=m.mpf(n-k)/n
  rate=m.findroot(lambda t:t/(1-m.exp(-t))-m.mpf(S)/k,4)
  C=sum(m.mpf(v)*(v-1)/2 for v in x)-S*rate/2
  rec['Poisson_tail_first_order']=str(C);rec['ZIP_rate']=str(rate)
  if C>0:assert status!=4,(c['name'],'finite maximum exists',C,q[:12])
  if c.get('mode')==3:rec['legacy_status']=int(q[12]);rec['legacy_estimates']=list(map(float,q[13:17]))
 if c['name'].startswith('golden'):assert status==0,(c['name'],q)
 if c['name']=='held-ZIP':assert status==4,(c['name'],q)
 if c['name'].startswith('budget'):assert status==3 and failure==2,(c['name'],q)
 if c['name'].startswith('two-peak-0.7683947756875808'):assert status==3 and failure==3,(c['name'],q)
 if status==0:
  r,p,pi=map(lambda v:M(float(v)),q[3:6]);reported=M(float(q[6]));x=c['x'];aa=m.log(r);bb=m.log(-r*m.log(p))
  actual=ll(x,aa,bb,pi);assert abs(reported-actual)/len(x)<m.mpf('1e-8'),(c['name'],reported,actual)
  for bit,v,hold in [(1,r,c['r']),(2,p,c['p']),(4,pi,c['pi'])]:
   if c['mask']&bit:assert v==M(hold),(c['name'],bit,v,hold)
  if c['mask'] in (0,4):
   fixed=M(c['pi']) if c['mask']&4 else None
   f=lambda a,b:ll(x,a,b,fixed)
   ar,br=m.findroot(lambda a,b:(m.diff(lambda v:f(v,b),a),m.diff(lambda v:f(a,v),b)),(aa,bb),tol=m.mpf('1e-45'),maxsteps=20)
   rr=m.exp(ar);tt=m.exp(br);pp=m.exp(-tt/rr);zz=m.mpf(x.count(0))/len(x)
   pip=M(c['pi']) if c['mask']&4 else max(m.mpf(0),(zz-m.exp(-tt))/(1-m.exp(-tt)))
   error=max(float(abs(r/rr-1)),float(abs(p/pp-1)));errors.append(error);pi_errors.append(float(abs(pi-pip)))
   assert error<2e-6 and abs(pi-pip)<2e-6,(c['name'],error,pi,pip)
   true_ll=f(ar,br);ll_errors.append(float(abs(reported-true_ll)/len(x)))
   low,high=float(q[7]),float(q[8])
   if math.isfinite(low):assert M(low)<=true_ll,(c['name'],'lower',low,true_ll)
   if math.isfinite(high):assert true_ll<=M(high),(c['name'],'upper',high,true_ll)
   rec.update(relative_coefficient_error=error,true_loglik=str(true_ll),true_r=str(rr),true_p=str(pp),true_pi=str(pip))
  if c['name'].startswith('golden'):
   assert abs(float(reported)-g['logLik'])<1e-7
   if c['mask']==0:assert float(r)==e['size'] and float(p)==e['prob'] and float(pi)==e['pi']
  rec['estimates']=[float(r),float(p),float(pi)]
 records.append(rec)
checked=0;face_checks=0;tail_checks=0
for c,line in zip(bounds,rows[len(cases):]):
 v=list(map(float,line.split()));x=c['x'];pi=M(c['pi']) if c['mask']&4 else None
 al,ah,bl,bh=map(M,[c['al'],c['ah'],c['bl'],c['bh']]);ac=(al+ah)/2;bc=(bl+bh)/2
 f=lambda a,b:ll(x,a,b,pi)
 assert abs(M(v[0])-f(ac,bc))<=M(v[1])
 assert abs(M(v[2])-m.diff(lambda a:f(a,bc),ac))<=M(v[4])
 assert abs(M(v[3])-m.diff(lambda b:f(ac,b),bc))<=M(v[4])
 for a in [al,ac,ah]:
  for b in [bl,bc,bh]:
   aa=m.diff(lambda w:f(w,b),a,2);bb=m.diff(lambda w:f(a,w),b,2)
   ab=m.diff(lambda w:m.diff(lambda t:f(w,t),b),a)
   for value,lo,hi in [(aa,v[8],v[9]),(ab,v[10],v[11]),(bb,v[12],v[13])]:assert M(lo)<=value<=M(hi),(c['name'],value,lo,hi)
   wa=(ah-al)/2;wb=(bh-bl)/2
   upper=M(v[0])+M(v[1])+(abs(M(v[2]))+M(v[4]))*wa+(abs(M(v[3]))+M(v[4]))*wb+(max(abs(M(v[8])),abs(M(v[9])))*wa**2+2*M(v[11])*wa*wb+max(abs(M(v[12])),abs(M(v[13])))*wb**2)/2
   assert f(a,b)<=upper,(c['name'],'Taylor',f(a,b),upper)
   checked+=1
 # A dense independent scan of the compact stationary-rate bracket checks
 # the certified face upper bound; analytic branch enumeration is in code.
 if int(v[14]):
  n=len(x);k=sum(y>0 for y in x);S=sum(x);z=m.mpf(n-k)/n
  for j in range(81):
   t=m.mpf(S)/n+(m.mpf(S)/k-m.mpf(S)/n)*j/80
   pp=pi if pi is not None else max(m.mpf(0),(z-m.exp(-t))/(1-m.exp(-t)))
   value=sum(w*(m.log(pp+(1-pp)*m.exp(-t)) if y==0 else m.log1p(-pp)-t+y*m.log(t)-m.loggamma(y+1)) for y,w in collections.Counter(x).items())
   assert value<=M(v[18]),(c['name'],'ZIP',value,v[18])
   face_checks+=1
 # The signed first-order remainder bounds are independent of inflation.
 A=sum(m.mpf(y)*(y-1)/2 for y in x);B2=sum(m.mpf(y)*(y-1)*(2*y-1)/6 for y in x)
 for rr in [m.mpf('.1'),m.mpf(1),m.mpf(10),m.mpf(1000)]:
  for tt in [m.mpf('.1'),m.mpf(1),m.mpf(10)]:
   zz=m.mpf(x.count(0))/len(x);pp=pi if pi is not None else max(m.mpf(0),(zz-m.exp(-tt))/(1-m.exp(-tt)))
   zipll=sum(w*(m.log(pp+(1-pp)*m.exp(-tt)) if y==0 else m.log1p(-pp)-tt+y*m.log(tt)-m.loggamma(y+1)) for y,w in collections.Counter(x).items())
   difference=ll(x,m.log(rr),m.log(tt),pp)-zipll;C=A-sum(x)*tt/2
   assert C/rr-B2/(2*rr*rr)<=difference<=C/rr+sum(x)*tt*tt/(8*rr*rr)
   tail_checks+=1
summary=dict(seed=11671,precision=70,calls=len(cases),statuses=dict(collections.Counter(c['status'] for c in records)),failures=dict(collections.Counter(c['failure'] for c in records)),max_relative_coefficient_error=max(errors),max_absolute_inflation_error=max(pi_errors),max_loglik_error_per_observation=max(ll_errors),hessian_and_Taylor_points=checked,face_grid_checks=face_checks,first_order_tail_checks=tail_checks)
(base/'global-oracle.json').write_text(json.dumps(dict(summary=summary,cases=records,boxes=bounds),indent=2)+'\n');print(json.dumps(summary,indent=2))
