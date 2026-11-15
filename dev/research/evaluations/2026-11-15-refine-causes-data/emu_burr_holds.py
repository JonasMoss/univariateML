"""Emulate burr_mle_holds (burr/mle.hpp 359-430) for shape1 held, shape2 and scale free (cases 638, 639): nested bisection on log c in doubles.
Variant B replaces l_i = log x_i, mean by centred logs d_i = log1p((x_i - x_0)/x_0), w_i = d_i - mean(d)."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=80
def sigmoid(z): return 1/(1+math.exp(-z)) if z>=0 else math.exp(z)/(1+math.exp(z))
def root(score,maxit=160):
    lo,hi=-1.0,1.0
    for i in range(maxit):
        if not score(lo)<0: break
        lo*=2
    for i in range(maxit):
        if not score(hi)>0: break
        hi*=2
    if not (score(lo)>=0 and score(hi)<=0): return float('nan')
    for i in range(maxit):
        m=(lo+hi)/2
        if m==lo or m==hi: return m
        v=score(m)
        if v==0: return m
        if v>0: lo=m
        else: hi=m
    return (lo+hi)/2
def solve(w,a,n):
    # w_i = log x_i - mean(log x); location(power): root over c of -(n - sum (a+1) sigmoid(power w - c)) in c
    def location(power):
        return root(lambda c: -(n-sum((a+1)*sigmoid(power*v-c) for v in w)))
    def score(z):
        power=math.exp(z); c=location(power); g=n
        for v in w: g+=power*v*(1-(a+1)*sigmoid(power*v-c))
        return g
    zz=root(score); return math.exp(zz)
def star(x,a):
    X=[mp.mpf(v) for v in x]; L=[mp.log(v) for v in X]; n=len(L); m=sum(L)/n; W=[l-m for l in L]
    def ll_prof(logc,c):  # profile in c and location parameter; maximise numerically via 2D Newton on (power, c) by findroot of scores
        pass
    # score equations in (power b, location offset c): u_i = b*w_i - c ; d/dc: n-(a+1)sum sig(u)=0 ; d/db: n + sum b w_i (1-(a+1) sig(u_i)) = 0
    def eqs(b,c):
        return (n-(a+1)*sum(1/(1+mp.exp(-(b*w-c))) for w in W), n+sum(b*w*(1-(a+1)/(1+mp.exp(-(b*w-c)))) for w in W))
    return eqs,W
if __name__=='__main__':
    a_=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json')); out=[]
    for case in (638,639):
        e=[e for e in ex if e['family']=='burr' and e['mode']=='double' and e['case']=='%06d'%case][0]
        a=e['held']['shape1']; x=list(a_[case]); n=len(x); ret=float(e['est']['shape2']); star=mp.mpf(e['star']['shape2'])
        # A as in C++
        l=[math.log(v) for v in x]; mean=0.0
        for v in l: mean+=v/n
        wA=[v-mean for v in l]  # C++ uses (v-mean) with v=l[i]
        bA=solve(wA,a,n)
        # B: centred logs
        x0=sorted(x)[n//2]; d=[math.log1p((v-x0)/x0) for v in x]; md=sum(d)/n; wB=[v-md for v in d]
        bB=solve(wB,a,n)
        # exact-w check: w from 80-digit logs, rounded to double
        Lm=[mp.log(mp.mpf(v)) for v in x]; mm=sum(Lm)/n; wC=[float(v-mm) for v in Lm]; bC=solve(wC,a,n)
        rel=lambda v: float((mp.mpf(v)-star)/star)
        o=dict(case=case,returned=ret,A_cpp_logs=bA,A_rel_err=rel(bA),returned_rel_err=rel(ret),A_equals_returned=(bA==ret),
               B_centred_log1p=bB,B_rel_err=rel(bB),C_exact_w_rounded=bC,C_rel_err=rel(bC),
               spread_of_logs=max(wA)-min(wA),ulp_log=2.2e-16*abs(l[0]))
        print(json.dumps(o)); out.append(o)
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_burr_holds.json','w'),indent=1)
