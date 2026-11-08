#!/usr/bin/env python3
"""Independent 60-digit checks of the held-q interval derivative bounds."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=60
rows=[]
def pfit(h,q):
 return mp.exp(mp.findroot(lambda lp:mp.digamma(mp.exp(lp)+q)-mp.digamma(mp.exp(lp))-h,mp.log(q/h),tol=mp.mpf('1e-48'),maxsteps=100))
for raw in [['.01','.1','1','3'],['.2','.8','1','2']]:
 v=list(map(mp.mpf,raw));mean=sum(v)/len(v);v=[x/mean for x in v]
 def stats(h):
  D=sum(z/mp.expm1(h*z) for z in v)/len(v)
  K=sum(z*z*mp.exp(h*z)/mp.expm1(h*z)**2 for z in v)/len(v)
  return D,K
 for q in map(mp.mpf,['.01','.3','.7','2','10']):
  for L in map(mp.mpf,['.01','.3','1','10']):
   H=L*mp.mpf('1.2');pl,ph=pfit(L,q),pfit(H,q)
   dl,kl=stats(L);dh,kh=stats(H)
   jl=mp.polygamma(1,pl)-mp.polygamma(1,pl+q);jh=mp.polygamma(1,ph)-mp.polygamma(1,ph+q)
   sl=1/H-pl+(q-1)*(dh if q>=1 else dl);sh=1/L-ph+(q-1)*(dl if q>=1 else dh)
   cl=-1/L**2+1/jh-(q-1)*(kl if q>=1 else kh);ch=-1/H**2+1/jl-(q-1)*(kh if q>=1 else kl)
   for fraction in map(mp.mpf,['0','.25','.5','.75','1']):
    h=L+(H-L)*fraction;p=pfit(h,q);d,k=stats(h);j=mp.polygamma(1,p)-mp.polygamma(1,p+q)
    s=1/h-p+(q-1)*d;c=-1/h**2+1/j-(q-1)*k
    assert sl-mp.mpf('1e-45')<=s<=sh+mp.mpf('1e-45')
    assert cl-mp.mpf('1e-40')<=c<=ch+mp.mpf('1e-40')
   rows.append(dict(q=float(q),L=float(L),H=float(H)))
Path(__file__).with_name('cell-bounds.json').write_text(json.dumps(dict(evidence='60-digit consistency checks, proof is separate',cells=rows,interior_checks=5*len(rows)),indent=2)+'\n')
print('passed',len(rows),'cells;',5*len(rows),'derivative checks')
