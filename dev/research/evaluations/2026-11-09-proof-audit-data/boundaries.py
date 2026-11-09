#!/usr/bin/env python3
"""Additional exact-boundary and root-bracket checks at 120 digits."""
import json
import hashlib
from pathlib import Path
from fractions import Fraction
import mpmath as mp
from check import M, HERE, ge, eq, counts, rng, ITERLIM, SOLVER_BUDGET, profile


def bisect(f,l,u):
    assert f(l)>0 and f(u)<0
    for _ in range(SOLVER_BUDGET):
        mid=(l+u)/2
        if f(mid)>0:l=mid
        else:u=mid
        if u-l<M('1e-60')*max(M(1),abs(l)):break
    else:raise AssertionError('root bracket budget exhausted')
    ge('roots.left_sign',f(l),0,True)
    ge('roots.right_sign',0,f(u),True)
    return [str(l),str(u)]


# Degree zero and one cases excluded from the Bessel-polynomial barrier.
for mu in [M('1e-20'),M(1),M('1e20')]:
    for w in [M('1e-12'),M('.1'),M('.5'),1-M('1e-12')]:
        logp0=lambda v:-mu*(1-v)
        logp1=lambda v:-mu*(1-v)+mp.log(mu*(1-v)/(1+v))
        eq('pig.zero_count_linear',mp.diff(logp0,w,2),M(0))
        scaled=(1-w*w)**2*mp.diff(logp1,w,2)
        eq('pig.one_count_curvature',scaled,-4*w)
        ge('pig.one_count_strict_concavity',0,scaled,True)

samples=[[0,2],[1],[0,1],[0,0,6],[1,1,10],[0,0,0,20]]
samples += [[rng.randrange(21) for _ in range(rng.randrange(2,10))] for _ in range(30)]
rows=[]
for xs in samples:
    if not sum(xs):continue
    n=len(xs);m=M(sum(xs))/n;v=sum((x-m)**2 for x in xs)/n
    mf=Fraction(sum(xs),n)
    vf=sum((Fraction(x)-mf)**2 for x in xs)/n
    for muf in [mf,mf/2,2*mf,2*mf+Fraction(1,10**12)]:
        mu=M(muf.numerator)/muf.denominator
        boundary=n*mu-2*sum(xs)+sum(x*(x-1) for x in xs)/mu
        exact_boundary=n*muf-2*sum(xs)+sum(x*(x-1) for x in xs)/muf
        eq('pig.held_boundary_identity',mu*boundary/n,v+(m-mu)**2-m)
        def score(w):
            z=mu*(1-w*w)/(2*w)
            es=[]
            for k in xs:
                if k<2:es.append(M(0));continue
                terms=[mp.factorial(k-1+j)/mp.factorial(j)/mp.factorial(k-1-j)/(2*z)**j for j in range(k)]
                es.append(sum(j*t for j,t in enumerate(terms))/sum(terms))
            return n*mu-2*sum(xs)/(1-w*w)+(1+w*w)*sum(es)/(w*(1-w*w))
        eps=M('1e-30')
        ge('pig.boundary_score_convergence',M('1e-23')*(1+abs(boundary)),abs(score(eps)-boundary))
        ge('pig.upper_boundary_score',0,score(1-eps),True)
        if exact_boundary>0:
            bracket=bisect(score,eps,1-eps)
            rows.append({'family':'pig','data':xs,'held_mean':str(mu),'w_bracket':bracket})
        else:
            for w in [eps,M('.001'),M('.5'),1-eps]:ge('pig.nonpositive_boundary_no_root',0,score(w),True)
    def nbscore(r):return sum(mp.digamma(r+x)-mp.digamma(r) for x in xs)-n*mp.log1p(m/r)
    if vf>mf:
        lo=M('1e-20');hi=M(1)
        for _ in range(SOLVER_BUDGET):
            if nbscore(hi)<0:break
            hi*=2
        else:raise AssertionError('NB bracket budget exhausted')
        rows.append({'family':'nb','data':xs,'r_bracket':bisect(nbscore,lo,hi)})
    else:
        for r in [M('1e-20'),M(1),M('1e20')]:ge('nb.boundary_no_root',nbscore(r),0,True)

# Weighted beta-binomial tables, including the report's numerator -2.
gains=[]
tables=[(3,[1000,3001,2999,1000]),(2,[1,2,1]),(2,[25001,50000,25001]),(3,[12501,37499,37499,12501])]
for N,freq in tables:
    h=sum(freq);S=sum(j*f for j,f in enumerate(freq));Q=sum(j*j*f for j,f in enumerate(freq));p=M(S)/(h*N)
    numerator=N*h*Q-(N-1)*S*S-N*h*S
    if freq==[1000,3001,2999,1000]:eq('bbinom.critical_numerator',M(numerator),M(-2))
    reflected=list(reversed(freq));Sr=sum(j*f for j,f in enumerate(reflected));Qr=sum(j*j*f for j,f in enumerate(reflected))
    eq('bbinom.reflected_numerator',M(N*h*Qr-(N-1)*Sr*Sr-N*h*Sr),M(numerator))
    A=[sum(freq[j+1:]) for j in range(N)];C=[sum(freq[:N-j]) for j in range(N)]
    F=S*mp.log(p)+(h*N-S)*mp.log1p(-p)
    g=M(numerator)/(2*h*N*p*(1-p));B=M(h*(N-1)*N*(2*N-1))/6
    for s in [M('1e-30'),M('1e-10'),M('.001')]:
        ell=lambda pp:sum(A[j]*mp.log(pp+j*s)+C[j]*mp.log(1-pp+j*s)-h*mp.log1p(j*s) for j in range(N))
        for pp in [p,M('.0000001'),M('.9999999')]:ge('bbinom.critical_uniform_bound',g*s+B*s*s/2,ell(pp)-F)
    if freq in [[25001,50000,25001],[12501,37499,37499,12501]]:
        alpha=M(25000) if N==2 else M('18749.5')
        dispersion=1/(2*alpha)
        finite=sum(A[j]*mp.log(M('.5')+j*dispersion)+C[j]*mp.log(M('.5')+j*dispersion)-h*mp.log1p(j*dispersion) for j in range(N))
        ge('bbinom.tiny_positive_gain',finite,F,True)
        for j in range(N+1):
            probability=mp.binomial(N,j)*mp.fprod(M('.5')+k*dispersion for k in range(j))*mp.fprod(M('.5')+k*dispersion for k in range(N-j))/mp.fprod(1+k*dispersion for k in range(N))
            eq('bbinom.saturated_probability',probability,M(freq[j])/h)
        gains.append({'size':N,'frequencies':freq,'alpha':str(alpha),'gain':str(finite-F)})
    interior=sum(freq[1:N]);H=sum(M(1)/j for j in range(1,N));threshold=h*H/interior
    for s in [threshold*(1+M('1e-20')),10*threshold]:
        for pp in [p,M('1e-15'),1-M('1e-15')]:
            ss=sum(j*(M(A[j])/(pp+j*s)+M(C[j])/(1-pp+j*s)-M(h)/(1+j*s)) for j in range(1,N))
            ge('bbinom.far_tail_bound',-interior+h*H/s,s*ss)
            ge('bbinom.far_tail_negative',0,ss,True)

# Vanishing and sign-changing leading gamma-tail coefficient.
x=[M('.5'),M('1.5')]; V=M('1.25'); Mmax=M('1.5')
for a in [4*(1-M('1e-30')),M(4),4*(1+M('1e-30'))]:
    A=a*(a*(V-1)-1)/2
    if a==4:eq('betapr.zero_leading_coefficient',A,M(0))
    C=a*a/4+2*a**3*Mmax*(1+2*V)+2*a**3*V
    gamma=a*mp.log(a)-mp.loggamma(a)-a+(a-1)*sum(mp.log(v) for v in x)/2
    for q in [M(13),M('1e10'),M('1e30')]:
        finite,_,_=profile(x,a,q)
        ge('betapr.critical_signed_tail',gamma+A/q+C/q**2,finite)
        ge('betapr.critical_unsigned_tail',gamma+a*a*(1+2*V)/q,finite)

# Simultaneously large shapes with nearly constant data, extreme shape ratios,
# and arbitrary scale translations. The constant case tests the separate
# unbounded-height conclusion rather than invoking the nonconstant J>0 claim.
for x in [[M(1),1+M('1e-12')],[M('1e-30'),M(1),M('1e30')],[M(1),M(1)]]:
    ys=list(map(mp.log,x));n=len(x);sy=sorted(ys)
    J=sum((sy[-i-1]-sy[i])**2/(4+2*(sy[-i-1]-sy[i])) for i in range(n//2))
    for p,q in [(M('1e-30'),M('1e30')),(M('1e30'),M('1e-30')),(M('1e30'),M('1e30')),(M('1e-30'),M('1e-30'))]:
        h=p*q/(p+q)
        for shift in [M(-10000),M(0),M(10000)]:
            t=sum(ys)/n-mp.log(p/q)+shift
            lb=mp.loggamma(p)+mp.loggamma(q)-mp.loggamma(p+q)
            ll=-n*lb-sum(p*mp.log1p(mp.exp(t-y))+q*mp.log1p(mp.exp(y-t))+y for y in ys)
            ge('betapr.extreme_small_h',n*mp.log(h)-sum(ys),ll)
            ge('betapr.extreme_large_h',n*mp.log(h/(2*mp.pi))/2+n/(48*h)-h*J-sum(ys),ll)
        ge('betapr.variance_concentration_bound',1/p+1/p**2+1/q+1/q**2,mp.polygamma(1,p)+mp.polygamma(1,q))

out={'result':'PASS','digits':mp.mp.dps,'seed':165,'iterlim':ITERLIM,'assertions':sum(counts.values()),'checks':dict(counts),'root_brackets':rows,'bbinom_gains':gains,'mpmath':mp.__version__,'script_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'parent_script_sha256':hashlib.sha256((HERE/'check.py').read_bytes()).hexdigest()}
(HERE/'boundaries.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps({k:v for k,v in out.items() if k!='root_brackets'}))
