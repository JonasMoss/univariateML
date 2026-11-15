"""Emulate dnorm_mle (dnorm/mle.hpp 100-150) in 40-digit arithmetic: same start, Newton direction, Armijo backtracking, stop -slope <= tol^2 n."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=40
Phi=lambda t: mp.erfc(-t/mp.sqrt(2))/2
phi=lambda t: mp.exp(-t*t/2)/mp.sqrt(2*mp.pi)
def cells(data):
    xs=sorted(set(data)); w=[data.count(v) for v in xs]; n=len(data)
    c=sum(wi*(x-xs[0]) for x,wi in zip(xs,w))/mp.mpf(n)+xs[0]+mp.mpf(1)/2
    var=sum(wi*(x+mp.mpf(1)/2-c)**2 for x,wi in zip(xs,w))/n
    return xs,w,n,c,var
def evaluate(P,A,B,hm,hs,held):
    xs,w,n,c,var=P; d=1 if (hm or hs) else 2
    g=[mp.mpf(0)]*d; H=[[mp.mpf(0)]*d for _ in range(d)]; f=mp.mpf(0)
    for x,wi in zip(xs,w):
        l=x-c; r=l+1; a=l*B-A; b=r*B-A
        pr=Phi(b)-Phi(a); lp=mp.log(pr)
        fa=phi(a)/pr; fb=phi(b)/pr
        va=[l-held] if hm else [-1] if hs else [-1,l]
        vb=[r-held] if hm else [-1] if hs else [-1,r]
        f-=wi*lp
        s=[fb*vb[k]-fa*va[k] for k in range(d)]
        for k in range(d):
            g[k]-=wi*s[k]
            for m in range(d): H[k][m]+=wi*(s[k]*s[m]+b*fb*vb[k]*vb[m]-a*fa*va[k]*va[m])
    return f,g,H
def run(data,mean=None,sd=None,tol=1e-9,maxit=200,trace=False,apply_last=False):
    P=cells(data); xs,w,n,c,var=P
    hm=mean is not None; hs=sd is not None
    mean=mp.mpf(mean) if hm else c
    if not hs: sd=mp.hypot(mp.sqrt(max(mp.mpf('0.1'),var)), (mean-c) if hm else 0)
    B=1/mp.mpf(sd); A=(mean-c)*B; held=mean-c
    f,g,H=evaluate(P,A,B,hm,hs,held); hist=[]
    for it in range(maxit):
        d=len(g)
        if d==1: step=[-g[0]/H[0][0]]
        else:
            det=H[0][0]*H[1][1]-H[0][1]*H[1][0]
            step=[(-H[1][1]*g[0]+H[0][1]*g[1])/det,(H[1][0]*g[0]-H[0][0]*g[1])/det]
        slope=sum(g[k]*step[k] for k in range(d))
        dec=-slope
        cur=(c+A/B if not hm else mean, 1/B)
        hist.append((it,float(dec/n),cur))
        if dec<=mp.mpf(tol)**2*n:
            if apply_last:
                nA=A; nB=B
                if hm: nB+=step[0]; nA=held*nB
                else: nA+=step[0]; nB+= 0 if hs else step[1]
                A,B=nA,nB
            break
        t=mp.mpf(1)
        while t>1e-12:
            nA=A;nB=B
            if hm: nB+=t*step[0]; nA=held*nB
            else:
                nA+=t*step[0]
                if not hs: nB+=t*step[1]
            if nB>0:
                nf,ng,nH=evaluate(P,nA,nB,hm,hs,held)
                if nf<=f+mp.mpf('1e-4')*t*slope+mp.mpf('1e-14')*n: A,B,f,g,H=nA,nB,nf,ng,nH; break
            t/=2
    return (mean if hm else c+A/B, sd if hs else 1/B), hist
if __name__=='__main__':
    ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json'))
    a=archive_data(); out=[]
    for e in ex:
        if e['family']!='dnorm' or e['mode']!='double': continue
        data=list(a[int(e['case'])]); h=e['held']
        mean=h.get('mean'); 
        res={'case':e['case'],'held':h,'returned':e['est'],'star':e['star']}
        star=(mp.mpf(e['star']['mean']),mp.mpf(e['star']['sd']))
        for tol in (1e-9,1e-10):
            for al in (False,True):
                (m,s),hist=run(data,mean,None,tol,apply_last=al)
                res['tol=%g%s'%(tol,' +last step' if al else '')]=dict(iters=len(hist),sd_rel_err=float(abs(s-star[1])/star[1]),mean_rel_err=float(abs(m-star[0])/(abs(star[0]) or 1)),final_dec_per_n=hist[-1][1])
        (m,s),hist=run(data,mean,None,1e-9)
        res['trace']=[(i,d_,[float(x) for x in cur]) for i,d_,cur in hist]
        out.append(res); print(json.dumps({k:v for k,v in res.items() if k not in('star',)},default=str)[:1500]); print()
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_dnorm.json','w'),indent=1,default=str)
