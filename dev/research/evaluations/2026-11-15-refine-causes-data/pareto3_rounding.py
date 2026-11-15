"""pareto3 (min = 0): shape error equals the effect of rounding log(v/unit) to double (spread of the logs is 1e-8 .. 1e-12).
Solve in 80 digits the likelihood  sum[log a + t - 2 softplus(t)], t = a (l_i - z),  once with exact l_i = log(v_i/unit) and once with l_i
rounded as the code forms them, fl(log(fl(v_i/unit))), and compare the shapes."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=80
def solve(L,a0):
    n=len(L)
    def eqs(la,z):
        a=mp.exp(la); u=[a*(l-z) for l in L]; s=[1/(1+mp.exp(-t)) for t in u]
        # d/dz: -a*sum(1-2 s_i)  ; d/da: n/a + sum (l-z)(1-2 s)  -> use log a coordinate: n + a*sum((l-z)(1-2s))
        return (sum(1-2*x for x in s), n+a*sum((l-z)*(1-2*x) for l,x in zip(L,s)))
    z0=sum(L)/n
    return mp.findroot(eqs,(mp.log(a0),z0),tol=mp.mpf(10)**-60,maxsteps=100)
if __name__=='__main__':
    a_=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json')); out=[]
    for case in (5590,5591,5593):
        for mode in ('double','native'):
            es=[e for e in ex if e['family']=='pareto3' and e['mode']==mode and e['case']=='%06d'%case]
            if not es: continue
            e=es[0]; x=list(a_[case]); star=mp.mpf(e['star']['shape']); sc=float(e['star']['scale']); ret=float(e['est']['shape'])
            Lx=[mp.log(mp.mpf(v)/mp.mpf(sc)) for v in x]
            r=solve(Lx,float(star)); sh=mp.exp(r[0]); 
            Lr=[mp.mpf(math.log(v/sc)) for v in x]
            r2=solve(Lr,float(star)); sh2=mp.exp(r2[0])
            o=dict(case=case,mode=mode,returned_shape=ret,returned_rel_err=float((ret-star)/star),exact_logs_rel_err=float((sh-star)/star),
                   double_rounded_logs_rel_err=float((sh2-star)/star),log_spread=float(max(Lx)-min(Lx)))
            print(json.dumps(o)); out.append(o)
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/pareto3_rounding.json','w'),indent=1)
