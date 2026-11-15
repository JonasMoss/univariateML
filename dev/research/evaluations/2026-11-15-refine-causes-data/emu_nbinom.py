"""Emulate nbinom_candidate_mle Newton (nbinom/mle.hpp 167-209), then certificate bracket (certificate.hpp 119-140)."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=60
dg=lambda z: float(mp.digamma(z)); tg=lambda z: float(mp.polygamma(1,z))
def run(x,tol,maxit=100):
    n=len(x); xb=sum(x)/n; var=sum(v*v for v in x)/n-xb*xb
    u={}; 
    for v in x: u[v]=u.get(v,0)+1
    def f_df(size):
        prob=size/(xb+size); dprob=xb/(size*size+xb*size); f=df=0.0
        for v,c in u.items(): f+=c*dg(size+v); df+=c*tg(size+v)
        f-=n*dg(size); df-=n*tg(size); f+=n*math.log(prob); df+=n*dprob
        return f/df
    size=xb*xb/(var-xb); tr=[]
    for it in range(1,maxit+1):
        st=f_df(size); size-=st; tr.append((it,st,size))
        if abs(st)<abs(size)*tol+tol: break
    return size,tr
def star(x):
    n=len(x); xb=mp.mpf(sum(x))/n
    sc=lambda r: sum(mp.digamma(r+v) for v in x)/n-mp.digamma(r)-mp.log1p(xb/r)
    return mp.findroot(sc,20.5)
if __name__=='__main__':
    a=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json'))
    out={}
    for c in (9668,10128):
        x=list(a[c]); r0=star(x); e=[e for e in ex if e['family']=='nbinom' and e['mode']=='double' and e['case']=='%06d'%c][0]
        print(c,'sorted data',sorted(x),'var-mean',sum(v*v for v in x)/len(x)-(sum(x)/len(x))**2-sum(x)/len(x))
        o=dict(returned=e['est']['size'],star=str(r0))
        for name,tol in (('1.22e-4',1.220703e-4),('1e-10',1e-10)):
            s,tr=run(x,tol); o[name]=dict(size=s,iters=len(tr),rel_err=float((s-r0)/r0),steps=[t[1] for t in tr],match_returned=(s==float(e['est']['size'])))
            print(' tol',name,o[name])
        out[c]=o
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_nbinom.json','w'),indent=1,default=str)
