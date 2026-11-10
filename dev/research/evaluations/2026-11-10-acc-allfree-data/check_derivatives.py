"""Recheck independent derivative fixtures at 110 digits; does not edit tests."""
import json,mpmath as m
from pathlib import Path
m.mp.dps=110
D=Path(__file__).resolve().parent
rows=[]
for name in ['st-derivatives','st-tail-derivatives','beta-derivatives']:
 r=json.loads((D/(name+'.json')).read_text());point=list(map(m.mpf,r.get('point',r.get('log_point'))))
 if name.startswith('st-'):
  x=list(map(m.mpf,r.get('data',r.get('x'))))
  def f(*p):
   mu,ls,alpha,ln=p;omega=m.exp(ls);nu=1+m.exp(ln)
   value=len(x)*(ls-m.loggamma((nu+1)/2)+m.loggamma(nu/2)+m.log(nu*m.pi)/2-m.log(2))
   for v in x:
    z=(v-mu)/omega;h=alpha*z*m.sqrt((nu+1)/(nu+z*z));df=nu+1
    c=m.betainc(m.mpf('.5'),df/2,0,h*h/(df+h*h),regularized=True)
    prob=(1+c)/2 if h>=0 else (1-c)/2
    value+=(nu+1)*m.log1p(z*z/nu)/2-m.log(prob)
   return value
 else:
  def f(*p):
   a,b=map(m.exp,p);return m.loggamma(a)+m.loggamma(b)-m.loggamma(a+b)
 n=len(point);g=[];h=[]
 for i in range(n):
  def one(t):
   q=point[:];q[i]=t;return f(*q)
  g.append(m.diff(one,point[i]));row=[]
  for j in range(n):
   if i==j:row.append(m.diff(one,point[i],2))
   else:
    def pair(a,b):
     q=point[:];q[i]=a;q[j]=b;return f(*q)
    row.append(m.diff(pair,(point[i],point[j]),(1,1)))
  h.append(row)
 observed=[f(*point)]+g+([h[0][0],h[0][1],h[1][1]] if n==2 else [v for row in h for v in row])
 expected=[m.mpf(r['value'])]+list(map(m.mpf,r['gradient']))+list(map(m.mpf,r['hessian'] if n==2 else [v for row in r['hessian'] for v in row]))
 error=max(abs(a-b)/(1+abs(b)) for a,b in zip(observed,expected))
 rows.append(dict(fixture=name,precision_digits=110,maximum_normalized_error=str(error),passed=bool(error<m.mpf('1e-45'))))
(D/'derivative-checks.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2));assert all(r['passed'] for r in rows)
