#!/usr/bin/env python3
import os
for k in ('OPENBLAS_NUM_THREADS','OMP_NUM_THREADS','MKL_NUM_THREADS'): os.environ[k]='1'
import csv, math
import numpy as np
from scipy.special import gammaln
from scipy.optimize import minimize_scalar
from pathlib import Path
p=Path(__file__).resolve().parent
rows=list(csv.DictReader((p/'samples.csv').open())); result=[]
for i,r in enumerate(rows):
 x=np.array([int(t) for t in r['data'].split()]); h=len(x); m=x.mean(); M=max(x); v=np.mean((x-m)**2); j=np.arange(M); A=np.array([sum(x>k) for k in j]); const=-np.sum(gammaln(x+1)); lp=h*m*math.log(m)-h*m+const
 def nb(a): return np.dot(A,np.log1p(j/a))+h*m*math.log(m)-(h*a+h*m)*math.log1p(m/a)+const
 def bi(n):
  if n==m: return 0. if np.all(x==n) else -math.inf
  return np.dot(A,np.log1p(-j/n))+h*m*math.log(m)+np.sum(n-x)*math.log1p(-m/n)+const
 bll=-math.inf; nbest=''; aa=''; nll=lp; slope=''; tar=''; bound=False
 if len(set(x))==1: bll=0.;nbest=M
 elif v<m-1e-12:
  z=minimize_scalar(lambda z:-bi(math.exp(z)),bounds=(math.log(M+1e-8),math.log(1e8)),method='bounded',options={'xatol':1e-11}); n0=max(M,round(math.exp(z.x))); choices=[M]+[max(M,n0+k) for k in range(-3,4)]; bll,nbest=max((bi(n),n) for n in choices);bound=n0>0.99e8
  pr=m/nbest;tar=h*(v-nbest*pr*(1-pr))/(2*pr*(1-pr))
 elif v>m+1e-12:
  z=minimize_scalar(lambda z:-nb(math.exp(z)),bounds=(-12,18),method='bounded',options={'xatol':1e-10}); a=math.exp(z.x);nll=nb(a);aa=a;bound=abs(z.x-18)<1e-3 or abs(z.x+12)<1e-3
  slope=sum(-t*(t-1)/2+t*(t+1)*m/(2*(a+m))+m*m*(a-1)/(2*(a+m)) for t in x)
 result.append(dict(index=i,poisson=lp,binom=bll,binom_N=nbest,nb=nll,nb_a=aa,nb_slope=slope,binom_slope=tar,boundary=bound))
with (p/'faces.csv').open('w',newline='') as f:
 w=csv.DictWriter(f,fieldnames=result[0]);w.writeheader();w.writerows(result)
print('faces',len(result),'boundary',sum(r['boundary'] for r in result))
