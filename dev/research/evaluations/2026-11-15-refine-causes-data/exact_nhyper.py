"""nhyper nesting entries: exact integer/rational checks. P(T=x) = r/x * C(m,r) C(N-m,x-r) / C(N,x).
Adjacent ratios per observation (all exact):
  N -> N+1 :  (N+1-m)/(N+1-m-(x-r)) * (N+1-x)/(N+1)
  r -> r+1 :  (m-r)(x-r) / (r (N-m-x+r+1))
  m=r=1 -> m=r=3 at fixed N: product over x of 3(x-1)(x-2)/((N-1)(N-2))  (P = r/x prod_{i<r}(x-i)/(N-i) when m=r)."""
import json
from fractions import Fraction as F
import mpmath as mp
mp.mp.dps=40
x=[1000000,1000001,1000003,1000008,1000011]
def rN(N,m,r):
    t=F(1)
    for v in x: t*=F(N+1-m,N+1-m-(v-r))*F(N+1-v,N+1)
    return t
def rR(N,m,r):
    t=F(1)
    for v in x: t*=F((m-r)*(v-r),r*(N-m-v+r+1))
    return t
def lg(f): return mp.log(mp.mpf(f.numerator)/f.denominator)
out={}
# 5161: all free, returned (N,m,r)=(1000011,1,1)
N=1000011; f=F(1)
for v in x: f*=F(3*(v-1)*(v-2),(N-1)*(N-2))
out['5161_(1,1)->(3,3)_at_N=1000011']=dict(ratio_gt_1=f>1,log_ratio=str(lg(f)))
# 5175: m=20 held, returned (N,r)=(6000029,1)
N=6000029; m=20
a=rN(N,m,1); b=rR(N,m,1)
out['5175_N_plus_1']=dict(ratio_gt_1=a>1,log_ratio=str(lg(a)))
out['5175_r_1_to_2']=dict(ratio_gt_1=b>1,log_ratio=str(lg(b)))
# 5230: m=20,r=3 held, returned N=6000027
N=6000027; a=rN(N,20,3)
out['5230_N_plus_1']=dict(ratio_gt_1=a>1,log_ratio=str(lg(a)))
# location of the N maximum for fixed (m,r) by exact bisection on ratio>1 (monotone in N per observation factor? checked by scan)
def Nstar(m,r,lo,hi=10**14):
    # largest N with L(N+1)/L(N)>1 under the assumption that the sign changes once (scanned below)
    while hi-lo>1:
        mid=(lo+hi)//2
        if rN(mid,m,r)>1: lo=mid
        else: hi=mid
    return lo
def ll(N,m,r):
    t=mp.mpf(0)
    for v in x:
        t+=mp.log(mp.mpf(r)/v)+mp.log(mp.binomial(m,r))+mp.loggamma(N-m+1)-mp.loggamma(v-r+1)-mp.loggamma(N-m-v+r+1)-(mp.loggamma(N+1)-mp.loggamma(v+1)-mp.loggamma(N-v+1))
    return t
rows=[]
for r in range(1,21):
    lo=20+max(x)-r
    # sign scan on a log grid to check a single sign change
    grid=[int(lo*(1.5**k)) for k in range(0,60)]
    signs=[rN(n,20,r)>1 for n in grid]
    changes=sum(1 for i in range(1,len(signs)) if signs[i]!=signs[i-1])
    Ns=Nstar(20,r,lo)+1
    rows.append(dict(r=r,N_star=Ns,sign_changes_on_grid=changes,loglik=str(ll(Ns,20,r))))
out['5175_profile_max_by_r_(m=20)']=rows
out['5230_Nstar_r3']=Nstar(20,3,1000028)+1
print(json.dumps(out,indent=1))
json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/exact_nhyper.json','w'),indent=1)
