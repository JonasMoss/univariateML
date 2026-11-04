"""Independent 70-decimal evaluation of roots and asymptotic coefficients."""
import json
from fractions import Fraction
import mpmath as mp
import numpy as np
from study import HERE,ROOT,save,fixed,seam,value_bracket,inputs
mp.mp.dps=70

def fit(x,v,start):
    x=list(map(mp.mpf,x));v=mp.mpf(v);n=len(x)
    def scores(m,t):
        s=mp.exp(t);z=[(q-m)/s for q in x]
        return (mp.fsum((v+1)*q/(s*(v+q*q)) for q in z),
                mp.fsum((v+1)*q*q/(v+q*q)-1 for q in z))
    m,t=mp.findroot(scores,(mp.mpf(start[0]),mp.log(mp.mpf(start[1]))),tol=mp.mpf('1e-55'),maxsteps=100)
    z=[(q-m)/mp.exp(t) for q in x]
    c=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2
    return m,t,n*(c-t)-(v+1)/2*mp.fsum(mp.log1p(q*q/v) for q in z)

cases=json.loads((HERE/'inputs.json').read_text());results=json.loads((HERE/'results.json').read_text())
checks=[]
for case,row in zip(cases,results):
    x=case['x']
    for family in ['lst','std']:
        p=row[family].get('proposal')
        if not p:continue
        m,t,ll=fit(x,p['nu'],[p['mu'],p['scale']])
        assert mp.mpf(p['lower'])<=ll<=mp.mpf(p['upper']),(case['name'],family)
        checks.append({'name':case['name'],'family':family,'abs_value_error':float(abs(ll-p['value']))})
# Joint stationary roots, independently evaluated in ordinary scale.
goldens=[]
for family in ['lst','std']:
    g=json.loads((ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    x=list(map(mp.mpf,g['data']));n=len(x);e=g['estimates']
    m0=e['mu'] if family=='lst' else e['mean'];s0=e['sigma'] if family=='lst' else e['sd']*np.sqrt((e['nu']-2)/e['nu']);v0=e['df'] if family=='lst' else e['nu']
    def score(m,t,v):
        s=mp.exp(t);z=[(q-m)/s for q in x]
        return (mp.fsum((v+1)*q/(s*(v+q*q)) for q in z),
                mp.fsum((v+1)*q*q/(v+q*q)-1 for q in z),
                n*(mp.digamma((v+1)/2)-mp.digamma(v/2)-1/v)/2
                -mp.fsum(mp.log1p(q*q/v) for q in z)/2
                +(v+1)/2*mp.fsum(q*q/(v*(v+q*q)) for q in z))
    m,t,v=mp.findroot(score,(m0,mp.log(s0),v0),tol=mp.mpf('1e-50'))
    s=mp.exp(t) if family=='lst' else mp.exp(t)*mp.sqrt(v/(v-2))
    actual=[m,s,v];expected=[m0,e['sigma'] if family=='lst' else e['sd'],v0]
    err=max(float(abs(a-b)/abs(b)) for a,b in zip(actual,expected));assert err<3e-5
    goldens.append({'family':family,'coefficients':[str(q) for q in actual],'max_relative_golden_error':err,'golden_tolerance':3e-5,'score_norm':str(mp.sqrt(mp.fsum(q*q for q in score(m,t,v))))})
# Seam expansion with nuisance refitting, compared at two shrinking xi.
seams=[]
for name in ['cluster-0.01','normal-0','seam-A-0.0']:
    case=next(q for q in cases if q['name']==name);x=case['x'];xx=list(map(mp.mpf,x));n=len(xx)
    mean=mp.fsum(xx)/n;sd=mp.sqrt(mp.fsum((q-mean)**2 for q in xx)/n);z=[(q-mean)/sd for q in xx]
    m3,m4,m6=[mp.fsum(q**k for q in z)/n for k in [3,4,6]]
    A=n*(m4-3)/4;B=n*(m4/4-m6/6+m3*m3/2+(1-m4)**2/4)
    normal=-n*(mp.log(sd)+mp.log(2*mp.pi)/2+mp.mpf('.5'));errors=[]
    for xi in [mp.mpf('1e-5'),mp.mpf('1e-6')]:
        m,t,ll=fit(x,1/xi,[float(mean),float(sd)])
        error=abs((ll-normal-A*xi)/xi**2-B);errors.append(float(error))
    assert errors[1]<errors[0]/5 and errors[1]<.001
    seams.append({'name':name,'A':str(A),'B':str(B),'second_coefficient_errors':errors})
# Strict negative slope, yet an explicit finite nu=3 witness above the normal.
ints=[-4,-3,-2,-1,1,2,3,4,-101,-99,99,101]
x=[mp.mpf(q)/100 for q in ints];n=len(x);S2=sum(q*q for q in ints);S4=sum(q**4 for q in ints)
Ar=Fraction(n,4)*(Fraction(n*S4,S2*S2)-3);assert Ar<0
p=fixed(np.array(list(map(float,x))),3.);m,t,ll=fit(x,3,[p['mu'],p['scale']]);sd=mp.sqrt(mp.fsum(q*q for q in x)/n)
normal=-n*(mp.log(sd)+mp.log(2*mp.pi)/2+mp.mpf('.5'));assert ll-normal>mp.mpf('.49')
counter={'A_exact':str(Ar),'nu':3,'mu':str(m),'scale':str(mp.exp(t)),'finite_minus_normal':str(ll-normal)}
# Exercise a budget refusal, with no interpretation as a root certificate.
budgets={str(b):value_bracket(np.array(cases[0]['x']),1.,budget=b)['outcome'] for b in [0,40]};assert all(q=='budget' for q in budgets.values())
save('independent.json',{'precision_digits':70,'profile_enclosures_checked':checks,'golden_roots':goldens,'seam_checks':seams,'counterexample':counter,'budget_checks':budgets})
print('Independent likelihood bounds, golden roots, seam expansion, counterexample and budget checks passed.')
