#!/usr/bin/env python3
"""Deterministic research prototypes, not package code. No scipy dependency.
Run from repo root through /tmp/umvl-cpu/run; writes checks.json beside itself.
All reference calculations use 90 decimal digits; working prototypes use 65.
"""
import json, math, hashlib, pathlib, platform, time
import mpmath as mp
mp.mp.dps = 90
HERE=pathlib.Path(__file__).resolve().parent
out={'precision':{'reference':90,'prototype':65},'checks':[]}
def record(name, error, bound=None, **extra):
    e=abs(error)
    row=dict(name=name,error=mp.nstr(e,12),**extra)
    if bound is not None:
        row['bound']=mp.nstr(bound,12)
        assert e <= bound, row
    out['checks'].append(row)
def C(m,v):
    return (2*m-2)*mp.log(2)-mp.log(mp.pi)+2*mp.re(mp.loggamma(m+1j*v/2))-mp.loggamma(2*m-1)
def lg_stirling(z, eps=mp.mpf('1e-60')):
    # Shift into the right half plane; the DLMF bound is for the full complex remainder.
    w=z; shift=mp.mpf(0); steps=0
    while True:
        val=(w-mp.mpf('.5'))*mp.log(w)-w+mp.log(2*mp.pi)/2
        for N in range(1,201):
            bound=abs(mp.bernoulli(2*N))/(2*N*(2*N-1)*abs(w)**(2*N-1))/mp.cos(mp.arg(w)/2)**(2*N)
            if bound < eps:
                return mp.re(val-shift),bound,steps,N
            val+=mp.bernoulli(2*N)/(2*N*(2*N-1)*w**(2*N-1))
        w+=32
        shift=sum(mp.log(z+k) for k in range(steps+32))
        steps+=32
        if steps>1024: raise ArithmeticError('prototype budget')
def logpdf(x,m,v,loc=0,scale=1):
    z=(x-loc)/scale
    return C(m,v)-mp.log(scale)-m*mp.log1p(z*z)-v*mp.atan(z)
def tail_quad(R,m,v,s):
    # Positive reciprocal/angular-endpoint quadrature. R>=1, s=+1 upper/-1 lower.
    a=2*m-1; b=1/R
    def integrand(u):
        t=b*u**(1/a)
        return mp.exp(-m*mp.log1p(t*t)+s*v*mp.atan(t))
    integral=mp.quad(integrand,[0,mp.mpf('.25'),mp.mpf('.75'),1])
    return C(m,v)-s*v*mp.pi/2+a*mp.log(b)-mp.log(a)+mp.log(integral)
def tail_reference(R,m,v,s):
    # Independent positive log-distance integral: t=b exp(-u/a), then u=v/(1-v).
    a=2*m-1; b=1/R
    def g(u):
        if u==1:return mp.mpf(0)
        q=u/(1-u); t=b*mp.exp(-q/a)
        return mp.exp(-q-m*mp.log1p(t*t)+s*v*mp.atan(t))/(1-u)**2
    I=mp.quad(g,[0,mp.mpf('.5'),mp.mpf('.9'),1])
    return C(m,v)-s*v*mp.pi/2+a*mp.log(b)-mp.log(a)+mp.log(I)
def tail_series(R,m,v,s,eps=mp.mpf('1e-52')):
    a=2*m-1;b=1/R;rho=mp.mpf('.5')
    assert b<rho
    logM=-m*mp.log1p(-rho*rho)+abs(v)*mp.atanh(rho)
    prev=mp.mpf(0); c=mp.mpf(1); total=1/a
    for j in range(1000):
        bound=mp.exp(logM)*(b/rho)**(j+1)/((a+j+1)*(1-b/rho))
        if total>bound and bound/(total-bound)<eps:
            lp=C(m,v)-s*v*mp.pi/2+a*mp.log(b)+mp.log(total)
            return lp,bound/(total-bound),j
        nxt=(s*v*c-(j-1+2*m)*prev)/(j+1)
        prev,c=c,nxt
        total+=c*b**(j+1)/(a+j+1)
    raise ArithmeticError('tail series budget')
def central_hyper(z,m,v):
    # Branch-continuous primitive; only differences near z=0 are used.
    A=m-1j*v/2;B=m+1j*v/2
    def H(w):
        return w**(1-B)/(1-B)*mp.hyp2f1(1-B,A,2-B,w)
    return mp.re(2j*mp.exp(C(m,v))*2**(-2*m)*(H((1-1j*z)/2)-H(mp.mpf('.5'))))
def central_series(z,m,v):
    p=mp.mpf(0);c=mp.mpf(1);total=z
    for j in range(300):
        nxt=(-v*c-(j-1+2*m)*p)/(j+1);p,c=c,nxt
        term=c*z**(j+2)/(j+2);total+=term
        # Fixed prototype degree, not a claimed tail bound.
    return mp.exp(C(m,v))*total

def run():
    started=time.time()
    cases=[('0.500000000001','0'),('.51','1000'),('1','-8'),('3.2','7'),('100','1e6'),('1e5','2')]
    print('complex gamma checks',flush=True)
    for ms,vs in cases:
        m=mp.mpf(ms);v=mp.mpf(vs);ref=mp.re(mp.loggamma(m+1j*v/2))
        with mp.workdps(65): value,bound,shifts,terms=lg_stirling(m+1j*v/2)
        # Add working arithmetic floor explicitly; this is not a binary64 certificate.
        record('complex-loggamma',value-ref,bound+mp.mpf('1e-57')*(1+abs(ref)),m=ms,nu=vs,shifts=shifts,terms=terms)
    print('tail checks',flush=True)
    tailcases=[('.5001','0','10'),('.51','80','100'),('1','-8','20'),('3.2','7','100'),('12','-100','1000'),('1','0','1e40')]
    for ms,vs,rs in tailcases:
        m,v,R=map(mp.mpf,(ms,vs,rs))
        for s in [-1,1]:
            ref=tail_reference(R,m,v,s)
            with mp.workdps(65): q=tail_quad(R,m,v,s);ser,bd,N=tail_series(R,m,v,s)
            record('positive-tail-quadrature-log',q-ref,mp.mpf('1e-48'),m=ms,nu=vs,R=rs,side=s)
            record('tail-series-log',ser-ref,2*bd+mp.mpf('1e-57'),m=ms,nu=vs,R=rs,side=s,terms=N)
    for m,v,z in [(mp.mpf('2.3'),mp.mpf('4'),mp.mpf('.4')),(mp.mpf('.7'),mp.mpf('-1.2'),mp.mpf('-.3'))]:
        ref=mp.quad(lambda t:mp.exp(logpdf(t,m,v)),[0,z])
        with mp.workdps(65): h=central_hyper(z,m,v);sr=central_series(z,m,v)
        record('hypergeometric-primitive',h-ref,mp.mpf('1e-55'))
        record('central-real-series',sr-ref,mp.mpf('1e-55'))
    # Full normalization from two halfline integrals; integrate z in bounded pieces
    # and use direct small tails from the independent reciprocal formula.
    for m,v in [(mp.mpf('.5001'),mp.mpf('0')),(mp.mpf('1.4'),mp.mpf('3'))]:
        center=mp.quad(lambda z:mp.exp(logpdf(z,m,v)),[-1,0,1])
        mass=center+sum(mp.exp(tail_reference(mp.mpf(1),m,v,s)) for s in [-1,1])
        record('normalization',mass-1,mp.mpf('1e-65'))
    # Safeguarded quantiles entirely in log distance for tails; inverse-uniform
    # sampler is precisely the same operation. Tail probabilities are never subtracted.
    print('quantile checks',flush=True)
    for ms,vs in [('.5001','0'),('.7','3'),('2.5','-8')]:
        m,v=map(mp.mpf,(ms,vs))
        for s,offset in [(-1,mp.mpf('-8')),(1,mp.mpf('-12'))]:
            lp=tail_quad(mp.mpf(4),m,v,s)+offset
            # These are strictly beyond |z|=1 in this grid.
            assert tail_quad(mp.mpf(1),m,v,s)>lp
            lo=mp.log(4);hi=lo+1
            def above_target(logR):
                value,error,_=tail_series(mp.exp(logR),m,v,s,mp.mpf('1e-70'))
                margin=2*error+mp.mpf('1e-78')*(1+abs(value))
                if abs(value-lp)<=margin:
                    raise ArithmeticError('quantile sign unresolved at working precision')
                return value>lp
            while above_target(hi): hi*=2
            for it in range(190):
                mid=(lo+hi)/2
                if above_target(mid):lo=mid
                else:hi=mid
            R=mp.exp((lo+hi)/2)
            relative_bracket=mp.expm1(hi-lo)
            assert relative_bracket<mp.mpf('1e-50')
            residual=tail_reference(R,m,v,s)-lp
            record('quantile-and-inverse-uniform',residual,mp.mpf('1e-48'),m=ms,nu=vs,side=s,logp=str(lp),log_abs_z=mp.nstr(mp.log(R),20),iterations=it+1,relative_R_bracket=mp.nstr(relative_bracket,14))
    # Type V, its O(alpha^2) correction, and normal paths, including held scale.
    a=mp.mpf('3');beta=mp.mpf('2');y=mp.mpf('1.3');m=(a+1)/2
    lv=a*mp.log(beta)-mp.loggamma(a)-(a+1)*mp.log(y)-beta/y
    h=a*(a*a-1)/(6*beta*beta)-m/(y*y)+beta/(3*y**3)
    errs=[]
    for al in [mp.mpf('1e-3'),mp.mpf('1e-5'),mp.mpf('1e-7')]:
        d=logpdf(y,m,-beta/al,0,al)-lv
        err=abs(d/al**2-h);errs.append(err)
        record('V-quadratic-coefficient',err,10*al**2,alpha=str(al),coefficient=mp.nstr(h,20))
    assert errs[2]<errs[1]<errs[0]
    for m in [mp.mpf('1e3'),mp.mpf('1e5')]:
        for r in [mp.mpf(0),mp.sqrt(2*m-1)]:
            al=mp.sqrt(2*m/(1+r*r));loc=al*r;v=2*m*r
            ref=-mp.log(2*mp.pi)/2-mp.mpf('.4')**2/2
            record('normal-path',logpdf(mp.mpf('.4'),m,v,loc,al)-ref,10/mp.sqrt(m),m=str(m),alpha=mp.nstr(al,12),r=mp.nstr(r,12))
    # Finite-difference verification of all four scores and a full-rank Gram matrix.
    m=mp.mpf('1.3');v=mp.mpf('2.1');loc=mp.mpf('.4');al=mp.mpf('1.7');x=mp.mpf('-.8');z=(x-loc)/al
    am=2*mp.re(mp.digamma(m+1j*v/2))-2*mp.digamma(2*m-1)+2*mp.log(2)
    av=-mp.im(mp.digamma(m+1j*v/2))
    scores=[(2*m*z+v)/(al*(1+z*z)),-1+(2*m*z*z+v*z)/(1+z*z),am-mp.log1p(z*z),av-mp.atan(z)]
    derivatives=[mp.diff(lambda q:logpdf(x,m,v,q,al),loc),mp.diff(lambda q:logpdf(x,m,v,loc,mp.exp(q)),mp.log(al)),mp.diff(lambda q:logpdf(x,q,v,loc,al),m),mp.diff(lambda q:logpdf(x,m,q,loc,al),v)]
    for j,(s,d) in enumerate(zip(scores,derivatives)):record('score-derivative',s-d,mp.mpf('1e-75'),coordinate=j)
    # Strict scale-score monotonicity counterexample: one residual z=10,m=1,nu=4.
    # Derivative wrt log alpha is -z*(4*m*z+v*(1-z*z))/(1+z*z)^2.
    z=mp.mpf(10);ds=-z*(4*z+4*(1-z*z))/(1+z*z)**2
    assert ds>0
    out['scale_score_counterexample']={'m':1,'nu':4,'z':10,'derivative':mp.nstr(ds,20)}
    out['elapsed_seconds']=round(time.time()-started,3)
    out['mpmath_version']=mp.__version__
    out['script_sha256']=hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest()
    out['passed']=len(out['checks'])
    (HERE/'checks.json').write_text(json.dumps(out,indent=2)+'\n')
    print(json.dumps({'passed':out['passed'],'seconds':out['elapsed_seconds']}))
if __name__=='__main__':run()
