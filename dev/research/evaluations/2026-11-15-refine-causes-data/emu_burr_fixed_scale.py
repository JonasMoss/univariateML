"""Emulate burr_mle_fixed_scale (burr/mle.hpp 301-340, fixed_scale.hpp 196-401, scale_certificate.hpp 105-110) for scale-held cases.
Steps: x/scale; moment start burr_mle_start; Newton on log c with abs step tol (legacy); proposal c; scale_fit replaces the
bisected root t by proposal*unit when |proposal*unit - t| < 1e-8 t."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=60
dg=lambda k: float(mp.digamma(k)); tg=lambda k: float(mp.polygamma(1,k)); tt=lambda k: float(mp.polygamma(2,k))
BASEL=math.pi**2/6; NE=-0.5772156649015328606
def logit(p): return math.log(p/(1-p))
def logistic(x): return 1/(1+math.exp(-x))
def invert_lower_logit(cv,tol,maxit=1000):
    loc=-0.5495872; scl=0.6103216; eps=1e-15
    if cv<0.1: k0=1-math.sqrt(2*cv/BASEL)
    elif cv>0.95: k0=math.sqrt((1-cv)/(cv*BASEL))
    else:
        y=min(1-eps,max(eps,1-cv)); xl=loc+scl*logit(y); k0=logistic((xl-loc)/scl)
    k0=min(1-eps,max(eps,k0)); x=logit(k0)
    for it in range(maxit):
        k=logistic(x); di=dg(k); tri=tg(k); te=tt(k); pd=di-NE; bpt=BASEL+tri
        f=pd*pd/bpt-cv; fpk=(2*pd*tri*bpt-pd*pd*te)/(bpt*bpt); df=fpk*(logistic(x)*(1-logistic(x)))
        x1=x-f/df
        if abs(x1-x)<abs(x)*tol+1e-3*tol: x=x1; break
        x=x1
    return logistic(x)
def invert_upper(cv,tol,maxit=1000):
    thr=0.2; eps=1e-9
    k0=1+math.sqrt(2*cv/BASEL) if cv<thr else math.exp(math.sqrt(cv*BASEL))
    k0=max(1+eps,k0); x=math.log(k0-1)
    for it in range(maxit):
        kk=math.exp(x)+1; di=dg(kk); tri=tg(kk); te=tt(kk); pd=di-NE; bpt=BASEL+tri
        f=pd*pd/bpt-cv; fpk=(2*pd*tri*bpt-pd*pd*te)/(bpt*bpt); df=fpk*math.exp(x)
        x1=x-f/df
        if abs(x1-x)<abs(x)*tol+1e-3*tol: x=x1; break
        x=x1
    return math.exp(x)+1
def start(logs):
    n=len(logs); mu=sum(logs)/n; var=sum(l*l for l in logs)/n-mu*mu
    cv=mu*mu/var
    if var>0 and abs(mu)>=2.2e-16 and not (mu>0 and cv>=1):
        k=invert_lower_logit(cv,1.220703e-4) if mu>0 else invert_upper(cv,1.220703e-4)
        csq=(BASEL+tg(k))/var
        if csq>0: return math.sqrt(csq),k
    return fallback(logs),None
def sp(z): return z+math.log1p(math.exp(-z)) if z>350 else math.log1p(math.exp(z))
def fallback(logs):
    # burr_mle_start, fixed_scale.hpp 264-314: profile in log c on a unit grid, then golden section
    n=len(logs); sm=sum(logs); scale=max(abs(l) for l in logs)
    def profile(logc):
        c=math.exp(logc); g=0.0; rem=0.0
        for l in logs:
            z=c*l; g+=sp(z); rem-=sp(-z)
        return n*(math.log(n)-math.log(g)+logc)-n-sm+rem
    center=-math.log(scale); mid=profile(center); left=profile(center-1); right=profile(center+1)
    if left>mid or right>mid:
        d=-1.0 if left>right else 1.0; best=mid
        for i in range(1000):
            nxt=center+d; v=profile(nxt)
            if v<=best: break
            center=nxt; best=v
    a,b=center-1,center+1; r=0.6180339887498948482
    u=b-r*(b-a); v=a+r*(b-a); fu=profile(u); fv=profile(v)
    for i in range(1000):
        if not (b-a>1e-9): break
        if fu>fv: b=v; v=u; fv=fu; u=b-r*(b-a); fu=profile(u)
        else: a=u; u=v; fu=fv; v=a+r*(b-a); fv=profile(v)
    return math.exp((a+b)/2)
def core(logs,c0,tol):
    n=len(logs); sl=sum(logs); logc=math.log(c0); tr=[]
    for it in range(1000):
        c=math.exp(logc); g=dg_=d2g1=d2g2=0.0
        for lx in logs:
            z=c*lx; xc=math.exp(z); t=xc/(xc+1); g+=math.log1p(xc); dg_+=lx*t; d2g1+=lx*lx*t; d2g2+=(lx*t)**2
        d2g=d2g1-d2g2; fc=n/c-n*dg_/g+sl-dg_; dfc=-n/(c*c)-n*(g*d2g-dg_*dg_)/(g*g)-d2g
        fp=c*fc; d2p=c*(fc+c*dfc); st=fp/d2p; logc-=st; tr.append(st)
        if abs(st)<tol: break
    c=math.exp(logc); tot=sum(math.log1p(math.exp(c*l)) for l in logs)
    return n/tot,c,tr
def star(x,scale):
    X=[mp.mpf(v)/scale for v in x]; L=[mp.log(v) for v in X]; n=len(X)
    k_of=lambda c: n/sum(mp.log1p(mp.exp(c*l)) for l in L)
    # profile score in c: derivative of profile log lik
    def prof(c):
        k=k_of(c); return n*(mp.log(k)+mp.log(c))+(c-1)*sum(L)-(k+1)*sum(mp.log1p(mp.exp(c*l)) for l in L)
    c0=mp.findroot(lambda c: mp.diff(prof,c),2.0)
    return k_of(c0),c0
if __name__=='__main__':
    a_=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json')); out=[]
    for case in (688,689,9424,9884):
        e=[e for e in ex if e['family']=='burr' and e['mode']=='double' and e['case']=='%06d'%case][0]
        sc=e['held']['scale']; x=list(a_[case]); logs=[math.log(v/sc) for v in x]
        c0,k0=start(logs); k,c,tr=core(logs,c0,1.220703e-4)
        ks,cs=star(x,mp.mpf(sc)); ret=(float(e['est']['shape1']),float(e['est']['shape2']))
        rel_prop=float((c-cs)/cs)
        o=dict(case=case,returned=ret,legacy_proposal=(k,c),legacy_iters=len(tr),last_step=tr[-1],
               returned_equals_proposal=abs(ret[1]-c)<=4e-16*c, proposal_rel_err_shape2=rel_prop,
               proposal_rel_err_shape1=float((k-ks)/ks),override_condition_holds=abs(rel_prop)<1e-8,
               returned_rel_err=(float((ret[0]-ks)/ks),float((ret[1]-cs)/cs)))
        # what a tight-tol Newton (or the bisected root) gives:
        k2,c2,tr2=core(logs,c0,1e-10); o['legacy_tol1e-10']=dict(shape2_rel_err=float((c2-cs)/cs),shape1_rel_err=float((k2-ks)/ks),iters=len(tr2))
        print(json.dumps(o)); out.append(o)
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_burr_fixed_scale.json','w'),indent=1)
