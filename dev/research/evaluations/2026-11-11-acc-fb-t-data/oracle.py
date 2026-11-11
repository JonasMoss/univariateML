"""90-digit stationary points from direct densities, mpmath numerical derivatives.
Inputs are exact binary64 values. No production derivatives or search are used.
Run through /tmp/umvl-cpu/run; checkpoints after each row make this resumable.
"""
import json,sys,mpmath as m
from pathlib import Path
D=Path(__file__).resolve().parent;m.mp.dps=90
names={'slash':['mu','sigma'],'sstd':['mean','sd','nu','xi'],'sged':['mean','sd','nu','xi'],'st':['xi','omega','alpha','nu'],'std':['mean','sd','nu'],'lst':['mu','sigma','df']}
def objective(f,x,p):
 mu,s=p[:2];n=len(x)
 if f=='slash':
  def term(v):
   z=(v-mu)/s;t=z*z/2
   return -m.log(2) if not t else m.log(-m.expm1(-t))-m.log(2*t)
  return n*(-m.log(s)-m.log(2*m.pi)/2)+m.fsum(term(v) for v in x)
 if f in ['lst','std','st']:
  d=p[3] if f=='st' else p[2];r=d-2 if f=='std' else d
  value=n*(m.loggamma((d+1)/2)-m.loggamma(d/2)-m.log(m.pi*r)/2-m.log(s))
  value-=(d+1)/2*m.fsum(m.log1p(((v-mu)/s)**2/r) for v in x)
  if f=='st':
   def prob(v):
    z=(v-mu)/s;h=p[2]*z*m.sqrt((d+1)/(d+z*z));df=d+1
    # Integrate the smaller tail rather than subtracting nearly equal numbers.
    tail=m.betainc(df/2,m.mpf('.5'),0,df/(df+h*h),regularized=True)/2
    return m.log(tail) if h<0 else m.log1p(-tail)
   value+=n*m.log(2)+m.fsum(prob(v) for v in x)
  return value
 d,xi=p[2:];student=f=='sstd'
 if student:
  a=m.gamma((d-1)/2)/m.gamma(d/2)*m.sqrt((d-2)/m.pi)
  constant=m.loggamma((d+1)/2)-m.loggamma(d/2)-m.log((d-2)*m.pi)/2
 else:
  lam=m.sqrt(m.gamma(1/d)/m.gamma(3/d));a=m.gamma(2/d)/m.sqrt(m.gamma(1/d)*m.gamma(3/d))
  constant=m.log(d)-m.log(2*lam)-m.loggamma(1/d)
 su=m.sqrt((1-a*a)*(xi*xi+1/(xi*xi))+2*a*a-1);shift=a*(xi-1/xi)
 def term(v):
  z=(v-mu)*su/s+shift;w=z/xi if z>0 else z*xi
  return -(d+1)*m.log1p(w*w/(d-2))/2 if student else -abs(w/lam)**d
 return n*(m.log(su)-m.log(s)+m.log(2/(xi+1/xi))+constant)+m.fsum(term(v) for v in x)
def corner_reference(c,p,x):
 """Independent constrained exponential-power density at a data knot, 90 digits."""
 f=c['family'];keys=names[f];held=c['held'];hm='mean' in held;hs='sd' in held;hx='xi' in held
 if hm and hs and hx:return None
 origin,unit=p[:2];y=[(v-origin)/unit for v in x]
 mean=(m.mpf(held['mean'])-origin)/unit if hm else None
 sd=m.mpf(held['sd'])/unit if hs else None
 xi=m.mpf(held['xi']) if hx else None
 d0=p[2];K=m.gamma(2/d0)/m.gamma(1/d0);G=m.gamma(3/d0)/m.gamma(1/d0)
 r=p[3]**2;a0=1/m.sqrt(G*(1+r*r-r)-K*K*(r-1)**2);b0=r*a0;mode=K*(a0-b0)
 knot=min(y,key=lambda v:abs(v-mode))
 if abs(knot-mode)>max(m.mpf('1e-5')*max(1,abs(mode)),m.mpf('2e-15')*abs(origin/unit)):return None
 two=not(hm or hs or hx)
 tangent=two or hm and not hs and not hx or not hm and hs and not hx or not hm and not hs and hx
 q=[m.log(a0),m.log(b0)] if two else [m.log(a0) if hm and not hs and not hx or not hm and not hs and hx else m.log(p[3])]
 def sides(q,mode,d):
  K=m.gamma(2/d)/m.gamma(1/d);G=m.gamma(3/d)/m.gamma(1/d)
  if two:return m.exp(q[0]),m.exp(q[1])
  if hm and hs:
   delta=(mean-mode)/K;total=m.sqrt((4*sd*sd+(4*K*K-3*G)*delta*delta)/G)
   return (total-delta)/2,(total+delta)/2
  if hm and not hx:
   a=m.exp(q[0]);return a,a+(mean-mode)/K
  ratio=xi*xi if hx else m.exp(2*q[0])
  if hm:a=(mean-mode)/(K*(ratio-1))
  elif hs:a=sd/m.sqrt(G*(1+ratio*ratio-ratio)-K*K*(ratio-1)**2)
  else:a=m.exp(q[0])
  return a,ratio*a
 def cost(q,mode,d):
  a,b=sides(q,mode,d)
  return len(y)*(m.log(a+b)+m.loggamma(1/d)-m.log(d))+m.fsum((abs(v-mode)/(a if v<mode else b))**d for v in y)
 magnitude=max(1,abs(cost(q,knot,d0)))
 def fun(*q):return cost(q,knot,d0)/magnitude
 if tangent:
  def scores(*q):return tuple(m.diff(fun,tuple(q),tuple(int(k==j) for k in range(len(q)))) for j in range(len(q)))
  q=list(m.findroot(scores,tuple(q),solver='mdnewton',tol=m.mpf('1e-75'),maxsteps=40))
  residual=max(map(abs,scores(*q)))
  h=m.matrix([[m.diff(fun,tuple(q),tuple(int(k==i)+int(k==j) for k in range(len(q)))) for j in range(len(q))] for i in range(len(q))]);m.cholesky(h)
  correction=m.lu_solve(h,m.matrix(scores(*q)))
  reference_error=max(map(abs,correction))
  assert reference_error<m.mpf('1e-60'),reference_error
 else:residual=m.mpf(0);reference_error=m.mpf(0)
 a,b=sides(q,knot,d0);answer=[origin+unit*(knot+K*(b-a)),unit*m.sqrt(G*(a*a+b*b-a*b)-K*K*(b-a)**2),d0,m.sqrt(b/a)]
 for j,k in enumerate(keys):
  if k in held:answer[j]=m.mpf(held[k])
 left=-m.inf if d0<1 else m.diff(lambda z:cost(q,z,d0),knot,direction=-1)
 right=m.inf if d0<1 else m.diff(lambda z:cost(q,z,d0),knot,direction=1)
 assert left<0 and right>0,(left,right)
 shape_score=None
 if 'nu' not in held:
  shape_score=m.diff(lambda d:cost(q,knot,d),m.mpf(1),direction=1)
  assert shape_score>0,shape_score
 return dict(index=c['index'],family=f,theta={k:str(v) for k,v in zip(keys,answer)},residual=str(residual),active_shape='nu' not in held,positive_information=True,reference_coordinate_error=str(reference_error),precision_digits=90,method='independent constrained exponential-power raw side scales at exact data knot',knot=str(origin+unit*knot),mode_left_score=str(left),mode_right_score=str(right),shape_right_score=None if shape_score is None else str(shape_score))

def run():
 cases=json.loads((D/'cases.json').read_text());before={r['index']:r for r in map(json.loads,(D/'before-native.jsonl').read_text().splitlines())}
 path=D/'stationary.jsonl';done={r['index'] for r in map(json.loads,path.read_text().splitlines())} if path.exists() else set()
 for c in cases:
  if c['index'] in done:continue
  b=before[c['index']];f=c['family'];keys=names[f]
  # Newly searched subunit cases have no baseline estimate; use the new search.
  if b['error']:
   after={r['index']:r for r in map(json.loads,(D/'after-native.jsonl').read_text().splitlines())};b=after.get(c['index'],b)
  if b['error']:
   row=dict(index=c['index'],family=f,stop=b['error'])
  else:
   try:
    p=[m.mpf(b['estimates'][k]) for k in keys];x=[m.mpf(v) for v in c['data']]
    if f=='sged' and p[2]<=1:
     contact=corner_reference(c,p,x)
     if contact is not None:
      with path.open('a') as out:out.write(json.dumps(contact)+'\n')
      print(c['index'],f,'corner done',flush=True)
      continue
    free=[j for j,k in enumerate(keys) if k not in c['held']]
    floor=1 if f in ['st','sged','lst'] else 2
    shape=3 if f=='st' else 2
    active=f!='slash' and shape in free and p[shape]==floor
    if active:
     def shape_cost(d):
      v=p[:];v[shape]=d;return -objective(f,x,v)
     if m.diff(shape_cost,m.mpf(floor),direction=1)<0:active=False
    if active:free.remove(shape)
    # Normalize location and scale before solving so tiny/huge units do not
    # affect numerical conditioning of the independent root solve.
    origin=p[0];unit=p[1];y=[(v-origin)/unit for v in x];p[0]=m.mpf(0);p[1]=m.mpf(1)
    q=[p[j] if j==0 or f=='st' and j==2 else m.log(p[j]) for j in free]
    def decode(q):
     out=p[:]
     for j,v in zip(free,q):out[j]=v if j==0 or f=='st' and j==2 else m.exp(v)
     return out
    magnitude=max(m.mpf(1),abs(objective(f,y,decode(q))))
    def fun(*q):return -objective(f,y,decode(q))/magnitude
    def scores(*q):
     return tuple(m.diff(fun,tuple(q),tuple(int(k==j) for k in range(len(q)))) for j in range(len(q)))
    if free:
     root=m.findroot(scores,tuple(q),solver='mdnewton',tol=m.mpf('1e-75'),maxsteps=40)
     q=list(root);g=scores(*q)
     h=m.matrix([[m.diff(fun,tuple(q),tuple(int(k==i)+int(k==j) for k in range(len(q)))) for j in range(len(q))] for i in range(len(q))]);m.cholesky(h)
     correction=m.lu_solve(h,m.matrix(g));reference_error=max(map(abs,correction))
     assert reference_error<m.mpf('1e-55'),reference_error
    else:g=[];reference_error=m.mpf(0)
    answer=decode(q);answer[0]=origin+unit*answer[0];answer[1]*=unit
    row=dict(index=c['index'],family=f,theta={k:str(v) for k,v in zip(keys,answer)},residual=str(max(map(abs,g),default=0)),active_shape=active,positive_information=True,reference_coordinate_error=str(reference_error),precision_digits=90)
   except Exception as e:row=dict(index=c['index'],family=f,open=repr(e))
  with path.open('a') as out:out.write(json.dumps(row)+'\n')
  print(c['index'],f,'open' if 'open' in row else 'done',flush=True)
if __name__=='__main__':run()
