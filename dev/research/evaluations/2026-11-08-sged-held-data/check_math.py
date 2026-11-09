"""Independent 70-digit density, derivatives and finite reference coefficients.
Sampled comparisons validate the implementation, not the global theorem.
Run from repository root after check.cpp has produced checks.tsv.
"""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
folder=Path('dev/research/evaluations/2026-11-08-sged-held-data')
M=mp.mpf
x=list(map(M,[-3.,-1.,-.2,0.,.3,.4,2.]))
mean,sd=map(M,[.1,1.4])
def objective(z,p,sample=x,mu=mean,sigma=sd):
    k=mp.gamma(2/p)/mp.gamma(1/p)
    v=mp.gamma(3/p)/mp.gamma(1/p)
    s=2*sigma/mp.sqrt(v+(3*v-4*k*k)*z*z)
    c=mu-k*z*s
    a,b=s*(1-z)/2,s*(1+z)/2
    return len(sample)*(mp.log(p)-mp.loggamma(1/p)-mp.log(s))-sum((abs(t-c)/(a if t<c else b))**p for t in sample)
fits=[];tested=0;ties=[];golden_reference=None;face_references=[]
for row in (folder/'checks.tsv').read_text().splitlines():
    f=row.split('\t')
    if f[0]=='box':
        p,lo,hi,upper,sl,sh=map(M,f[1:])
        for t in [lo,(lo+hi)/2,hi]:
            assert objective(t,p)<=upper,(p,t,'value')
            score=mp.diff(lambda z:objective(z,p),t)
            assert sl<=score<=sh,(p,t,'score',score,sl,sh)
            tested+=1
    elif f[0]=='face':
        side,c,b,p,ll=map(lambda value:M(float(value)),f[1:])
        k=mp.gamma(2/p)/mp.gamma(1/p);v=mp.gamma(3/p)/mp.gamma(1/p)
        refb=1/mp.sqrt(v-k*k);refc=-side*k*refb
        refll=2*(mp.log(p)-mp.loggamma(1/p)-mp.log(refb))-2*(side*(-side*M(.2)-refc)/refb)**p
        assert abs(b/refb-1)<M('1e-10')
        assert abs(c-refc)<M('1e-10')
        assert abs(ll-refll)<M('1e-10')
        face_references.append(dict(side=int(side),endpoint=mp.nstr(refc,65),scale=mp.nstr(refb,65),loglik=mp.nstr(refll,65)))
    elif f[0]=='goldenfit':
        assert int(f[4])==0,f
        p,mu,sigma,xi,ll=map(lambda value:M(float(value)),[f[1],f[2],f[3],f[6],f[7]])
        golden=json.loads(Path('univariateML-cpp/include/cppunivariate/distributions/sged/golden.json').read_text())
        sample=list(map(M,golden['data']))
        z0=mp.tanh(mp.log(xi))
        z=mp.findroot(lambda z:mp.diff(lambda u:objective(u,p,sample,mu,sigma),z),(z0-M('.00001'),z0+M('.00001')))
        ref=mp.exp(mp.atanh(z));value=objective(z,p,sample,mu,sigma)
        error=abs(xi/ref-1)
        assert error<M('2e-7'),error
        assert abs(ll-value)<M('1e-8')
        golden_reference=dict(xi=mp.nstr(ref,65),loglik=mp.nstr(value,65),relative_xi_error=float(error),
            stored_xi_relative_difference=float(abs(M(golden['estimates']['xi'])/ref-1)),
            stored_loglik_difference=float(M(golden['logLik'])-value),frozen_tolerance=3e-5)
    elif f[0]=='tie':
        xi,lower,upper=map(M,f[1:])
        d=M(1.1)
        t=mp.findroot(lambda t:(t*t+2*t+5)/((t+3)*mp.sqrt(2*(1+t)))-d,(M('.1'),M('.3')))
        ref=mp.exp(mp.atanh(mp.sqrt(t)))
        if xi<1:ref=1/ref
        ll=mp.log(2*(1+t))-2*mp.log(2)-2*(d*mp.sqrt(2*(1+t))-2*t)/(1-t)
        assert abs(xi/ref-1)<M('1e-9')
        assert lower<=ll<=upper
        ties.append(dict(xi=mp.nstr(ref,65),loglik=mp.nstr(ll,65),relative_xi_error=float(abs(xi/ref-1))))
    elif f[0]=='fit':
        p=M(float(f[1]));status=int(f[2]);xi=M(f[4])
        if status:continue
        if p==1:
            d=(M(.3)-mean)/sd
            z=-d*mp.sqrt(2/(4-2*d*d)) # exact Laplace join c = x_i
        else:
            z=mp.findroot(lambda t:mp.diff(lambda u:objective(u,p),t),(-M('.2'),M('0')))
        ref=mp.exp(mp.atanh(z));ll=objective(z,p)
        error=abs(xi/ref-1)
        assert error<M('1e-10'),(p,error)
        assert abs(ll-M(f[5]))<M('1e-10')
        fits.append(dict(shape=str(p),xi=mp.nstr(ref,65),loglik=mp.nstr(ll,65),relative_xi_error=float(error)))
# Recheck the noncommuting held-mean/sd uniform/half-GED corner independently.
# Exact mathematical endpoints, rather than binary-rounded sqrt(3).
corners=[]
for shape in [1000,10000,100000]:
    p=M(shape);k=mp.gamma(2/p)/mp.gamma(1/p);v=mp.gamma(3/p)/mp.gamma(1/p)
    for lam in [1,2,10]:
        z=1-M(lam)/p;s=2/mp.sqrt(v+(3*v-4*k*k)*z*z);c=-k*z*s
        a,b=s*(1-z)/2,s*(1+z)/2
        left=p*mp.log((c+mp.sqrt(3))/a)
        right=p*mp.log((mp.sqrt(3)-c)/b)
        corners.append(dict(shape=shape,lam=lam,left_logpenalty=float(left),
            right_logpenalty=float(right),left_limit=float(-mp.euler+2*mp.zeta(2)/lam),
            right_limit=float(-mp.euler)))
assert abs(M(corners[-1]['left_logpenalty'])-M(corners[-1]['left_limit']))<M('1e-4')
(folder/'corners.json').write_text(json.dumps(corners,indent=2)+'\n')
# A fixed-skew held-moment tail fact: strictly interior observations have
# exponentially vanishing penalties, but the height approaches uniform from above.
normalizer_tail=[]
for shape in [1000,10000,100000]:
    p=M(shape);k=mp.gamma(2/p)/mp.gamma(1/p);v=mp.gamma(3/p)/mp.gamma(1/p)
    for z in [M('.2'),M('.8')]:
        scaled=p*p*(mp.log(p)-mp.loggamma(1/p)+mp.log(3*(v+(3*v-4*k*k)*z*z))/2)
        limit=3*mp.zeta(2)*(1+z*z)/2
        normalizer_tail.append(dict(shape=shape,z=str(z),scaled_height_excess=float(scaled),limit=float(limit)))
        if shape==100000:assert abs(scaled-limit)<M('.001')
(folder/'normalizer-tail.json').write_text(json.dumps(normalizer_tail,indent=2)+'\n')
(folder/'math.json').write_text(json.dumps(dict(precision=mp.mp.dps,interval_point_checks=tested,finite_references=fits,tie_references=ties,golden_held_reference=golden_reference,face_references=face_references),indent=2)+'\n')
print(json.dumps(dict(interval_point_checks=tested,finite_references=fits,tie_references=ties,golden_held_reference=golden_reference,face_references=face_references),indent=2))
