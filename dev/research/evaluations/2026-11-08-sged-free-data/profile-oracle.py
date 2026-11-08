#!/usr/bin/env python3
"""Independent 70-digit raw profile, curvature, moment-map and root checks."""
import json,math
from pathlib import Path
import mpmath as m
m.mp.dps=70
out=Path(__file__).resolve().parent
lines=out.joinpath('profile-boxes.txt').read_text().splitlines()
checks=0;results=[]
M=lambda x:m.mpf(float(x))
def enclosure(value,lo,hi):
 global checks
 checks+=1
 assert m.mpf(float(lo))<=value<=m.mpf(float(hi)),(str(value),lo,hi)
def mode(x,p,xi):
 lo=min(x);hi=max(x)
 for _ in range(180):
  c=(lo+hi)/2
  score=m.fsum(xi*(xi*(c-t))**(p-1) if t<c else -(1/xi)*((t-c)/xi)**(p-1) for t in x)
  if score>0:hi=c
  else:lo=c
 return (lo+hi)/2
def weighted(x,c,p,xi):
 r=[(xi if t<c else 1/xi)*abs(c-t) for t in x]
 T=m.fsum(t**p for t in r);U=m.fsum(t**p*m.log(t) if t else 0 for t in r)
 V=m.fsum(t**p*m.log(t)**2 if t else 0 for t in r)
 G=m.fsum(m.sign(c-t)*(xi if t<c else 1/xi)*rj**(p-1)*m.log(rj) if rj else 0 for t,rj in zip(x,r))
 H=m.fsum((xi if t<c else 1/xi)**2*rj**(p-2) if rj else (m.inf if p<2 else 0) for t,rj in zip(x,r))
 A=m.digamma(1+1/p)+m.log(p)-p*U/T+m.log(T/len(x))
 curvature=1/p-m.polygamma(1,1+1/p)/p**2-p*(V/T-(U/T)**2)+p*p*G*G/((p-1)*T*H)
 h=(p*T/len(x))**(1/p)
 k=m.gamma(2/p)/m.gamma(1/p);v=m.gamma(3/p)/m.gamma(1/p)
 mu=c+k*h*(xi-1/xi);sd=h*m.sqrt((v-k*k)*(xi-1/xi)**2+v)
 ll=len(x)*(-m.loggamma(1+1/p)-m.log(xi+1/xi)-(m.log(p*T/len(x))+1)/p)
 return A,curvature,mu,sd,ll
for line in lines:
 row=line.split();kind=row[0]
 if kind=='DATA':small=list(map(M,row[3:]))
 if kind=='W':
  xi,p,q=map(M,row[1:4]);valid=int(row[4]);assert valid
  for shape in [p,(p+q)/2,q]:
   c=mode(small,shape,xi);A,curv,mu,sd,ll=weighted(small,c,shape,xi)
   enclosure(c,row[5],row[6]);enclosure(A,row[7],row[8]);enclosure(curv,row[9],row[10])
   if int(row[11]):enclosure(mu,row[12],row[13]);enclosure(m.log(sd),row[14],row[15])
 if kind=='M':
  p,lo,hi=map(M,row[1:4]);valid=int(row[4]);r=1/(1+p);f=p/(1+p)
  for fraction in [m.mpf('.2'),m.mpf('.5'),m.mpf('.8')]:
   c=lo+(hi-lo)*fraction
   A=m.fsum((c-t)**p for t in small if t<c);B=m.fsum((t-c)**p for t in small if t>c)
   C=m.fsum((c-t)**(p-1) for t in small if t<c);E=m.fsum((t-c)**(p-1) for t in small if t>c)
   H=m.fsum((c-t)**(p-2) for t in small if t<c);J=m.fsum((t-c)**(p-2) for t in small if t>c)
   F=C/A**f-E/B**f
   derivative=(p-1)*(H/A**f+J/B**f)-f*p*(C*C/A**(f+1)+E*E/B**(f+1))
   S=A**r+B**r;ll=len(small)*(m.log(p)-m.loggamma(1/p)-(m.log(p/len(small))+1)/p-(1+1/p)*m.log(S))
   if valid:enclosure(F,row[5],row[6]);enclosure(derivative,row[7],row[8])
   enclosure(ll,'-inf',row[9])
original=json.load(open('univariateML-cpp/include/cppunivariate/distributions/sged/golden.json'))['data']
x=[m.mpf(t) for t in original]
for line in lines:
 row=line.split();kind=row[0]
 if kind not in ['FW','FM']:continue
 held=M(row[1]);status=int(row[2]);certified=int(row[3]);assert certified,(kind,held,status)
 if status!=0:
  assert status in [6,7,8],(kind,held,status)
  results.append(dict(kind=kind,held=float(held),status=status));continue
 mu0,sd0,p0,xi0=map(M,row[4:8]);ll0=M(row[8])
 k=m.gamma(2/p0)/m.gamma(1/p0);v=m.gamma(3/p0)/m.gamma(1/p0)
 h=sd0/m.sqrt((v-k*k)*(xi0-1/xi0)**2+v);c0=mu0-k*h*(xi0-1/xi0)
 if kind=='FW':
  xi=held
  if p0==1:
   results.append(dict(kind=kind,held=float(held),status=status,nu=1));continue
  def equations(c,p):
   g=m.fsum(xi*(xi*(c-t))**(p-1) if t<c else -(1/xi)*((t-c)/xi)**(p-1) for t in x)
   return g,weighted(x,c,p,xi)[0]
  c,p=m.findroot(equations,(c0,p0),tol=m.mpf('1e-50'))
  A,curv,mu,sd,ll=weighted(x,c,p,xi)
 else:
  p=held;r=1/(p+1);f=p/(p+1)
  def terms(c):
   A=m.fsum((c-t)**p for t in x if t<c);B=m.fsum((t-c)**p for t in x if t>c)
   C=m.fsum((c-t)**(p-1) for t in x if t<c);E=m.fsum((t-c)**(p-1) for t in x if t>c)
   return A,B,C/A**f-E/B**f
  c=m.findroot(lambda c:terms(c)[2],(c0,c0+m.mpf('1e-6')),tol=m.mpf('1e-50'))
  A,B,F=terms(c);S=A**r+B**r;t=(p*S/len(x))**(1/p);a=t*A**r;b=t*B**r
  k=m.gamma(2/p)/m.gamma(1/p);v=m.gamma(3/p)/m.gamma(1/p)
  mu=c+k*(b-a);sd=m.sqrt((v-k*k)*(b-a)**2+v*a*b);xi=m.sqrt(b/a)
  ll=len(x)*(m.log(p)-m.loggamma(1/p)-m.log(t*S)-1/p)
 error=max(abs(mu-mu0),abs(sd-sd0),abs(p-p0),abs(xi-xi0))
 assert error<m.mpf('2e-7'),(kind,held,error)
 results.append(dict(kind=kind,held=float(held),status=status,mean=str(mu),sd=str(sd),nu=str(p),xi=str(xi),loglik=str(ll),max_coefficient_error=float(error),likelihood_error=float(abs(ll-ll0))))
record=dict(precision=m.mp.dps,interval_checks=checks,violations=0,results=results)
out.joinpath('profile-oracle.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record,indent=2))
