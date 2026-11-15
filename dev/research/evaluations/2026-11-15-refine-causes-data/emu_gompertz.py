"""Emulate gompertz_mle_impl (gompertz/mle.hpp 59-157): standardise by the mean, Newton on b from 0.1, stop |step| < |b| tol + tol."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=60
def newton(z,tol,maxit=100):
    n=len(z); sx=sum(z); b=0.1; tr=[]
    for it in range(1,maxit+1):
        sb=sbx=sbx2=0.0
        for v in z:
            e=math.exp(b*v); sb+=e; sbx+=v*e; sbx2+=v*v*e
        s=sb-n; r=b*sbx-s; a=b*n/s; da=-n*r/(s*s)
        d2a=n/(s*s)*(b*(2*sbx*sbx/s-sbx2)-2*sbx)
        f=n*da/a+sx; df=n*(a*d2a-da*da)/(a*a); st=f/df
        b-=st; tr.append((it,st,b))
        if abs(st)<abs(b)*tol+tol: break
    sb=sum(math.exp(b*v) for v in z); a=b*n/(sb-n)
    return a,b,tr
if __name__=='__main__':
    a_=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json'))
    e=[e for e in ex if e['family']=='gompertz' and e['mode']=='double'][0]
    x=list(a_[2793]); n=len(x); unit=sum(x)/n; z=[v/unit for v in x]
    st={k:mp.mpf(v) for k,v in e['star'].items()}
    ret=(float(e['est']['a']),float(e['est']['b']))
    out=dict(returned=ret,star={k:str(v) for k,v in st.items()},unit=unit,z=z)
    for name,tol in (('1.22e-4',1.220703e-4),('1e-10',1e-10)):
        a,b,tr=newton(z,tol); A,B=a/unit,b/unit
        out[name]=dict(a=A,b=B,iters=len(tr),a_rel=float((A-st['a'])/st['a']),b_rel=float((B-st['b'])/st['b']),steps=[t[1] for t in tr],match=(A==ret[0] and B==ret[1]))
        print(name,out[name])
    # standardised problem's conditioning: Newton step relative sizes vs true error
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_gompertz.json','w'),indent=1,default=str)
