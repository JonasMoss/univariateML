#!/usr/bin/env python3
import os
for k in ('OPENBLAS_NUM_THREADS','OMP_NUM_THREADS','MKL_NUM_THREADS','NUMEXPR_NUM_THREADS'): os.environ[k]='1'
import itertools, csv, sys, math
import numpy as np
from scipy.special import gammaln, betaln, digamma
from scipy.optimize import minimize, minimize_scalar
from scipy.stats import nbinom
from collections import Counter
from pathlib import Path
P=Path(__file__).resolve().parent
stats = {'failed': 0, 'boundary': 0}

def poisson(x):
 m=np.mean(x); return np.sum(x*np.log(m)-m-gammaln(x+1))
def binom(x,n):
 m=np.mean(x); p=m/n
 if p>=1: return 0. if np.all(x==n) else -np.inf
 return np.sum(gammaln(n+1)-gammaln(x+1)-gammaln(n-x+1)+x*np.log(p)+(n-x)*np.log1p(-p))
def negbin(x,a):
 m=np.mean(x)
 return np.sum(gammaln(x+a)-gammaln(a)-gammaln(x+1)+a*np.log(a/(a+m))+x*np.log(m/(a+m)))
def fixed(x,n,starts):
 m=np.mean(x); p=m/n
 if np.all((x==0)|(x==n)):
  q=np.mean(x==n); return (0. if q in (0,1) else len(x)*(q*np.log(q)+(1-q)*np.log1p(-q))),np.nan,np.nan,'endpoint'
 best=(binom(x,n),np.inf,np.inf,'binom')
 def fun(z):
  a,b=np.exp(z); t=a+b
  ll=np.sum(betaln(x+a,n-x+b)-betaln(a,b)+gammaln(n+1)-gammaln(x+1)-gammaln(n-x+1))
  ga=np.sum(digamma(x+a)-digamma(a)-digamma(n+t)+digamma(t))*a
  gb=np.sum(digamma(n-x+b)-digamma(b)-digamma(n+t)+digamma(t))*b
  return -ll,-np.array([ga,gb])
 for start in starts:
  z=minimize(fun,np.log(np.clip(start,1e-5,1e5)),jac=True,method='L-BFGS-B',bounds=[(-11,12),(-11,12)],options={'maxiter':100,'ftol':1e-11,'gtol':1e-6})
  ok = z.success or (np.isfinite(z.fun) and np.max(np.abs(z.jac)) < 1e-4)
  if not ok: stats['failed'] += 1
  if np.any(np.abs(z.x) >= 10.999): stats['boundary'] += 1
  if ok and np.isfinite(z.fun) and -z.fun>best[0]+1e-8: best=(-z.fun,*np.exp(z.x),'interior')
 return best

def analyse(x, cap):
 stats['failed']=stats['boundary']=0
 x=np.array(x,dtype=float); h=len(x); M=int(max(x)); m=x.mean(); v=np.mean((x-m)**2); d=v/m if m else 0
 if not m: return None
 lp=poisson(x); bn=(-np.inf,None)
 if np.all(x==x[0]): bn=(0.,M)
 elif v<m-1e-10:
  opt=minimize_scalar(lambda z:-binom(x,np.exp(z)),bounds=(np.log(M+1e-6),np.log(1e6)),method='bounded',options={'xatol':1e-10})
  n0=max(M,round(np.exp(opt.x))); ns=set([M]+[max(M,n0+j) for j in range(-3,4)])
  bn=max((binom(x,n),n) for n in ns)
 nb=(lp,np.inf)
 if v>m+1e-10 and not np.all(x==x[0]):
  opt=minimize_scalar(lambda z:-negbin(x,np.exp(z)),bounds=(-12,12),method='bounded',options={'xatol':1e-9})
  if -opt.fun>lp: nb=(-opt.fun,np.exp(opt.x))
 slope=np.nan
 if np.isfinite(nb[1]):
  a=nb[1]; c=a/m
  slope=np.sum(-x*(x-1)/2+x*(x+1)/(2*(c+1))+a*(a-1)/(2*c*(c+1)))
 tar=np.nan
 if bn[1] and bn[1]>m:
  n=bn[1]; p=m/n; tar=np.sum(((x-n*p)**2-n*p*(1-p))/(2*p*(1-p)))
 best=(-np.inf,None,None,None,None); tail=[]; prev=None
 maxn=max(M,cap)
 for n in range(M,maxn+1):
  if n==1: val=(binom(x,n),np.inf,np.inf,'binom')
  else:
   starts=[(max(.02,m*m/max(v-m,.05)), max(.02,(n-m)*m/max(v-m,.05))),(.5,.5)]
   if prev is not None and np.isfinite(prev[1]): starts.insert(0,(prev[1],prev[2]*(n-m)/max(n-1-m,.01)))
   val=fixed(x,n,starts); prev=val
  if val[0]>best[0]: best=(val[0],n,val[1],val[2],val[3])
  if n in (maxn,max(M,maxn//2)): tail.append((n,val[0]))
 candidates=[(lp,'poisson'),(bn[0],'binomial'),(nb[0],'nb'),(best[0],best[4])]
 win=max(candidates,key=lambda t:t[0]); gap=win[0]-max(t[0] for t in candidates if t[1]!=win[1]) if len(set(t[1] for t in candidates))>1 else 0
 return dict(optimizer_failed=stats['failed'],optimizer_boundary=stats['boundary'],h=h,M=M,mean=m,var=v,D=d,central3=np.mean((x-m)**3),factorial2=np.mean(x*(x-1)),factorial3=np.mean(x*(x-1)*(x-2)),poisson=lp,binom=bn[0],binom_N=bn[1],nb=nb[0],nb_a=nb[1],nb_slope=slope,binom_slope=tar,grid=best[0],grid_N=best[1],grid_a=best[2],grid_b=best[3],grid_type=best[4],winner=win[1],margin=gap,tail_half=tail[0][1],tail_end=tail[-1][1],Nmax=maxn,data=' '.join(map(str,x.astype(int))))

def main():
 seed=113; rng=np.random.default_rng(seed); samples=[]
 for h in range(3,7):
  for t in itertools.combinations_with_replacement(range(5),h):
   if max(t)>0: samples.append(('exhaustive',np.array(t)))
 for _ in range(1800):
  h=int(rng.choice([3,5,10,20,30,50])); typ=rng.choice(['binomial','poisson','nb','bb']); n=int(rng.choice([5,10,20,40])); mu=float(rng.choice([.5,1,2,4,8]));
  if typ=='binomial': x=rng.binomial(n,min(.9,mu/n),h)
  elif typ=='poisson': x=rng.poisson(mu,h)
  elif typ=='nb':
   a=float(rng.choice([.5,1,2,5,15])); x=rng.negative_binomial(a,a/(a+mu),h)
  else:
   a=float(rng.choice([.3,1,3,10])); b=float(rng.choice([.5,2,5,20])); x=rng.binomial(n,rng.beta(a,b,h))
  if x.max()>0 and x.max()<=65: samples.append((typ,x))
 samples += [('opz',np.array(t)) for t in ([16,18,22,25,27],[16,18,22,25,28],[0,1,2,3,4],[0,0,1,2,2])]
 out=[]
 for i,(typ,x) in enumerate(samples):
  r=analyse(x, max(60,int(x.max())+15));
  if r: r['source']=typ; out.append(r)
  if (i+1)%100==0: print(i+1,'/',len(samples),flush=True)
 with (P/'samples.csv').open('w',newline='') as f:
  w=csv.DictWriter(f,fieldnames=list(out[0])); w.writeheader(); w.writerows(out)
 print('done',len(out),flush=True)
if __name__=='__main__': main()
