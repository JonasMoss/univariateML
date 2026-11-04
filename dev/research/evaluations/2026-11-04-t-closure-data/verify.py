#!/usr/bin/env python3
"""High precision identities, feasible witnesses, rational root isolation, bounds."""
import json
from pathlib import Path
import numpy as np
import mpmath as mp
import sympy as sp
from scipy.special import gammaln
from research import conditional,moments,ll,mp_refine
OUT=Path(__file__).resolve().parent
mp.mp.dps=80

def mll(x,w,mu,s,nu):
    n=sum(w)
    return n*(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-mp.log(s))-(nu+1)/2*sum(wi*mp.log1p(((xi-mu)/s)**2/nu) for xi,wi in zip(x,w))

def symmetric_profile(x,w,nu):
    n=sum(w)
    def score(t):
        s=mp.exp(t)
        return sum(wi*(nu+1)*xi**2/(nu*s*s+xi*xi) for xi,wi in zip(x,w))-n
    lo=mp.mpf(-200);hi=mp.mpf(200)
    assert score(lo)>0 and score(hi)<0
    for _ in range(320):
        mid=(lo+hi)/2
        if score(mid)>0:lo=mid
        else:hi=mid
    s=mp.exp((lo+hi)/2)
    assert abs(score(mp.log(s)))<mp.mpf('1e-65')
    return mll(x,w,mp.mpf(0),s,nu)

def main():
    checks={};n=20;x=list(map(mp.mpf,[0,1,10]));w=[9,6,5]
    mean=sum(a*b for a,b in zip(x,w))/n;v=sum(b*(a-mean)**2 for a,b in zip(x,w))/n
    normal=-n/2*mp.log(2*mp.pi*mp.e*v)
    witness=mll(x,w,mp.mpf('.5'),mp.mpf('.5'),mp.mpf(1))
    assert witness-normal>8
    checks['negative_seam_exact_decimal_witness']={'mu':'0.5','scale':'0.5','nu':1,'normal':str(normal),'ll':str(witness),'gap':str(witness-normal),'m4':str(sum(b*((a-mean)/mp.sqrt(v))**4 for a,b in zip(x,w))/n)}
    # Second-order coefficients of the fully optimized profile.
    seam=[]
    for x,w in [([-1,0,1],[1,4,1]),([-2,-1,0,1,2],[1,2,6,2,1])]:
        xx=list(map(mp.mpf,x));n=sum(w);v=sum(wi*xi**2 for xi,wi in zip(xx,w))/n
        normal=-n/2*mp.log(2*mp.pi*mp.e*v)
        m4=sum(wi*xi**4 for xi,wi in zip(xx,w))/n/v**2;m6=sum(wi*xi**6 for xi,wi in zip(xx,w))/n/v**3
        A=n*(m4-3)/4;Q=n*(m4/4-m6/6+(1-m4)**2/4)
        assert A==0
        rows=[]
        for nu in [100,1000,10000,100000]:
            ratio=(symmetric_profile(xx,w,mp.mpf(nu))-normal)*nu**2
            rows.append({'nu':nu,'scaled_gap':mp.nstr(ratio,35)})
        assert abs(mp.mpf(rows[-1]['scaled_gap'])-Q)<mp.mpf('.001')
        seam.append({'x':x,'weights':w,'A':str(A),'Q':str(Q),'rows':rows})
    checks['zero_slope']=seam
    third=[]
    for xx,w in [([-mp.sqrt(15)/2,-mp.mpf(1),mp.mpf(0),mp.mpf(1),mp.sqrt(15)/2],[32,45,176,45,32]),([-mp.sqrt(6),-mp.sqrt(3),-mp.mpf(1),mp.mpf(0),mp.mpf(1),mp.sqrt(3),mp.sqrt(6)],[1,15,9,70,9,15,1])]:
        n=sum(w);v=sum(wi*xi**2 for xi,wi in zip(xx,w))/n
        m4=sum(wi*xi**4 for xi,wi in zip(xx,w))/n/v**2
        m6=sum(wi*xi**6 for xi,wi in zip(xx,w))/n/v**3
        m8=sum(wi*xi**8 for xi,wi in zip(xx,w))/n/v**4
        assert abs(m4-3)<mp.mpf('1e-70') and abs(m6-mp.mpf('10.5'))<mp.mpf('1e-70')
        R=n*(m8-39)/8;normal=-n/2*mp.log(2*mp.pi*mp.e*v);rows=[]
        for nu in [100,1000,10000,100000]:
            ratio=(symmetric_profile(xx,w,mp.mpf(nu))-normal)*nu**3
            rows.append({'nu':nu,'scaled_gap':mp.nstr(ratio,35)})
        assert abs(mp.mpf(rows[-1]['scaled_gap'])-R)<mp.mpf('.01')
        third.append({'x':[mp.nstr(xi,35) for xi in xx],'weights':w,'m8':mp.nstr(m8,35),'cubic_coefficient':mp.nstr(R,35),'rows':rows})
    checks['zero_first_and_second_coefficients']=third
    # Exact rational stationary polynomial for a held-scale location slice.
    mu=sp.Symbol('mu');b=sp.Rational(1,20);xs=list(range(-2,3));den=[(mu-a)**2+b*b for a in xs]
    poly=sp.Poly(sum((a-mu)*sp.prod(den[:j]+den[j+1:]) for j,a in enumerate(xs)),mu)
    boxes=sp.polys.polytools.intervals(poly,eps=sp.Rational(1,10**24))
    assert len(boxes)==9 and all(mult==1 for _,mult in boxes)
    roots=[]
    for j,((lo,hi),mult) in enumerate(boxes):
        roots.append({'lo':str(lo),'hi':str(hi),'approx':str(sp.N((lo+hi)/2,22)),'kind':'maximum' if j%2==0 else 'minimum'})
    checks['held_scale_exact_roots']={'x':xs,'b':'1/20','polynomial':str(poly.as_expr()),'roots':roots}
    # Check the convex-profile chord and conditional tangent bounds numerically.
    x=np.array([0.,1.,10.]);w=np.array([9.,6.,5.]);n=sum(w)
    logK=lambda nu:gammaln((nu+1)/2)-gammaln(nu/2)-.5*np.log(np.pi)
    max_excess=-np.inf;max_gap=0
    for a,b in [(1,2),(2,5),(5,20),(20,100)]:
        pa=conditional(x,w,a);pb=conditional(x,w,b)
        for nu in np.linspace(a,b,31):
            p=conditional(x,w,nu);alpha=(b-nu)/(b-a)
            upper=n*logK(nu)+alpha*(pa['ll']+pa['conditional_value_gap_bound']-n*logK(a))+(1-alpha)*(pb['ll']+pb['conditional_value_gap_bound']-n*logK(b))
            max_excess=max(max_excess,p['ll']-upper);max_gap=max(max_gap,p['conditional_value_gap_bound'])
    assert max_excess<1e-8 and max_gap<1e-6
    checks['chord']={'points':124,'largest_computed_excess':max_excess,'largest_conditional_value_gap_bound':max_gap,'comparison_tolerance':1e-8}
    # Explicit signed-tail constants from the proof (conservative).
    mom=moments(x,w);V=1.;R=max(x)-min(x);k=max(w);r=n-k;delta=min(np.diff(x));L=delta/2*np.sqrt((r-k/V)/n);U=R*np.sqrt(1+1/V);sn=np.sqrt(mom['variance']);Z=R/L
    c=n*sn**2/(2*U**2);M=n*np.hypot(sn/L*(Z+Z**3),Z**2+Z**4);C=n*(Z**4/4+1/(24*V))+M*M/(4*c)
    cutoff=-mom['A']/(2*C)
    checks['signed_tail_constants']={'V':V,'L':L,'U':U,'Z':Z,'c':c,'M':M,'C':C,'xi_cutoff':cutoff,'nu_cutoff':1/cutoff}
    # The global gamma-ratio remainder used by that bound.
    max_margin=mp.mpf(0)
    for nu in list(map(mp.mpf,['1','2','3','10','100','10000'])):
        z=mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2+mp.log(2*mp.pi)/2
        assert -1/(4*nu)<=z<=-1/(4*nu)+1/(24*nu**3)
        max_margin=max(max_margin,z+1/(4*nu))
    checks['gamma_bound']={'points':6,'passed':True}
    # An exact empirical path from a normal-only supremum to a finite winner.
    u=sp.Symbol('u');px=[0,1,u,1+9*u];pw=[9,5,1,5];pn=sum(pw)
    pm=sum(a*b for a,b in zip(px,pw))/pn
    pv=sp.expand(sum(b*(a-pm)**2 for a,b in zip(px,pw))/pn)
    p4=sp.expand(sum(b*(a-pm)**4 for a,b in zip(px,pw))/pn)
    diff=sp.Poly(3*pv**2-p4,u);degree=diff.degree()
    bern=[sum(diff.nth(j)*sp.binomial(i,j)/sp.binomial(degree,j) for j in range(i+1)) for i in range(degree+1)]
    assert all(b>0 for b in bern)
    checks['normal_tie_path']={'x':['0','1','u','1+9*u'],'weights':pw,'range':'0 <= u <= 1','variance':str(pv),'three_variance_squared_minus_central_fourth':str(diff.as_expr()),'positive_Bernstein_coefficients':[str(b) for b in bern]}
    # An exact finite-attainment example for std: both missing faces improve.
    t=sp.Symbol('t');tp=8*t*t+41*t-63
    assert tp.subs(t,sp.Rational(123,100))<0<tp.subs(t,sp.Rational(124,100))
    exp_checks=[(sp.Rational(3,5),sp.Rational(223,123)),(sp.Rational(53,25),sp.Rational(341,41)),(sp.Rational(347,500),sp.Integer(2))]
    for q,target in exp_checks:
        assert sum(q**j/sp.factorial(j) for j in range(21))>target
    checks['exact_std_finite_attainment']={'x':[-3,-1,0,1,3],'weights':[1,4,6,4,1],'kurtosis':'680/169','t2_b_squared_polynomial':str(tp),'b_squared_bracket':['1.23','1.24'],'t2_inward_derivative_lower_bound':'47/125','method':'rational polynomial signs and positive exp Taylor lower sums through degree 20'}
    cusp=[]
    for a,x,w in [(1,[0,1,100],[2,1,1]),(2,[-1,0,1],[1,4,1])]:
        a=mp.mpf(a);xx=list(map(mp.mpf,x));n=sum(w);k=w[x.index(0)];r=n-k
        S1=sum(wi/xi for xi,wi in zip(xx,w) if xi);S2=sum(wi/xi**2 for xi,wi in zip(xx,w) if xi)
        D=a*(a+1)/2*(S2-S1*S1/k);expected=r/(2*D)
        def B(nu):
            return n*(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2)+(nu+1)*r/2*mp.log(nu)-(nu+1)*sum(wi*mp.log(abs(xi)) for xi,wi in zip(xx,w) if xi)
        C=mp.diff(B,a)+r/2*mp.log(expected)-r/2;rows=[]
        for estr in ['.001','.000001','.000000001','.000000000001']:
            e=mp.mpf(estr);nu=a+e
            def eq(m,t):
                mu=e*m;s2=e*mp.exp(2*t)
                um=(nu+1)*sum(wi*(xi-mu)/(nu*s2+(xi-mu)**2) for xi,wi in zip(xx,w))
                ut=sum(wi*(nu+1)*(xi-mu)**2/(nu*s2+(xi-mu)**2) for xi,wi in zip(xx,w))-n
                return um,ut/e
            m,t=mp.findroot(eq,(a*expected*S1/k,mp.log(expected)/2),tol=mp.mpf('1e-60'))
            val=mll(xx,w,e*m,mp.sqrt(e)*mp.exp(t),nu);pred=B(a)+r/2*e*mp.log(e)+C*e
            rows.append({'epsilon':estr,'scale_squared_over_epsilon':mp.nstr(mp.exp(2*t),30),'value_remainder_over_epsilon':mp.nstr((val-pred)/e,20)})
        assert abs(mp.mpf(rows[-1]['scale_squared_over_epsilon'])-expected)<mp.mpf('1e-8')
        assert abs(mp.mpf(rows[-1]['value_remainder_over_epsilon']))<mp.mpf('1e-7')
        cusp.append({'lower_nu':str(a),'x':x,'weights':w,'D':str(D),'scale_ratio_limit':str(expected),'linear_coefficient':str(C),'rows':rows})
    checks['critical_tie_cusps']=cusp
    search=OUT/'fixtures.json'
    if search.exists():
        found=json.loads(search.read_text());kept={}
        for name,v in found.items():
            # Retain the actual input and high-precision stationary candidates.
            kept[name]={'trial':v['trial'],'x':v['x'],'weights':v['weights'],'moments':v['moments'],'lower':v['lower'],'two':v['two'],'stationary_candidates':[]}
            for p in v['roots']:
                q=mp_refine(v['x'],v['weights'],p);q['kind']=p['kind']
                assert mp.mpf(q['max_abs_score'])<mp.mpf('1e-55')
                negative=sum(mp.mpf(e)<0 for e in q['negative_hessian_eigenvalues'])
                assert negative==(0 if p['kind']=='max' else 1)
                kept[name]['stationary_candidates'].append(q)
        (OUT/'multimodality.json').write_text(json.dumps(kept,indent=2)+'\n')
    else:
        raise RuntimeError('Missing retained multimodality fixtures')
    (OUT/'verification.json').write_text(json.dumps(checks,indent=2)+'\n')
    print('All research assertions passed; rational location roots isolated; 80-digit checks passed.')
if __name__=='__main__':main()
