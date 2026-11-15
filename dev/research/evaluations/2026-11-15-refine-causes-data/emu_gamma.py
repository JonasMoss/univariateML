"""Emulate gamma_mle (gamma/mle.hpp 175-200): double s, Minka start, newton_raphson_1d_abs. mpmath digamma rounded to double stands in for Boost."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=60
dg=lambda k: float(mp.digamma(k)); tg=lambda k: float(mp.polygamma(1,k))
def s_of(x):
    n=len(x); m=sum(x)/n; lx=sum(math.log(v) for v in x)/n   # double-mode sums (the native mode sums in long double)
    return math.log(m)-lx, m
def newton_abs(s,tol,rel=False,last=False,maxit=100):
    f=lambda k:(math.log(k)-dg(k)-s)/(1/k-tg(k))
    k=1/(12*s)*(3-s+math.sqrt((s-3)**2+24*s))*0.9989+0.0102; tr=[]
    for i in range(1,maxit+1):
        st=f(k); k-=st; tr.append((i,st,k))
        if (abs(st)/abs(k) if rel else abs(st))<tol: break
    return k,tr
def star_k(x):
    mp.mp.dps=60
    X=[mp.mpf(v) for v in x]; n=len(X)
    s=mp.log(sum(X)/n)-sum(mp.log(v) for v in X)/n
    return mp.findroot(lambda k: mp.log(k)-mp.digamma(k)-s, 1.8 if len(x)<5 else 1.9),s
if __name__=='__main__':
    a=archive_data(); ex={e['case']:e for e in json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json')) if e['family']=='gamma' and e['mode']=='double'}
    res=[]
    for c in (1977,1991):
        x=list(a[c]); s,m=s_of(x); ks,s50=star_k(x)
        e=ex['%06d'%c]; ret=float(e['est']['shape'])
        out=dict(case=c,s_double=s,s_50=str(s50),s_rel_err=float((s-s50)/s50),k_star=str(ks),returned=ret,returned_rel_err=float((ret-ks)/ks))
        for name,tol,rel in (('abs1.22e-4',1.220703125e-4,False),('abs1e-10',1e-10,False),('rel1e-10',1e-10,True)):
            k,tr=newton_abs(s,tol,rel)
            out[name]=dict(shape=k,iters=len(tr),rel_err_vs_maximizer_of_double_s=float((k-float(mp.findroot(lambda t: mp.log(t)-mp.digamma(t)-s,k)))/k),rel_err_vs_true=float((k-ks)/ks),last_step=tr[-1][1])
        # sensitivity: how much shape moves per rounding error in s
        out['dk_per_ds_rel']=float(1/(ks*(1/ks-mp.polygamma(1,ks))))
        k,tr=newton_abs(s,1.220703125e-4); out['trace']=[(i,st,kk) for i,st,kk in tr]
        res.append(out); print(json.dumps(out,default=str)); print()
    json.dump(res,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_gamma.json','w'),indent=1,default=str)
