"""50-digit independent differentiation and GEV golden MLE; no fixture writes."""
import json,pathlib,os,mpmath as mp
mp.mp.dps=50
root=pathlib.Path(os.environ.get("UMVL_JOINT_OUTPUT",pathlib.Path(__file__).resolve().parent))
bounds=json.loads((root/'joint-bounds.json').read_text())
x=list(map(mp.mpf,bounds['data']));n=len(x)
def mean(v):return mp.fsum(v)/len(v)
def pivot(k,v,data=x):
    L=[mp.log1p(k*d/v)/k if k else d/v for d in data]
    return -mp.log(v)-mp.log(mean([mp.exp(-l) for l in L]))-(1+k)*mean(L)-1
def gap(k,e):
    a=-1/k;w=[e+1-d for d in x]
    return mp.log(a)+(a-1)*mean(list(map(mp.log,w)))-mp.log(mean([v**a for v in w]))-1
checks=0
root_checks=0
coefficient_checks=0
def inner_score(k,v,is_gap):
    if is_gap:
        a=-1/k;w=[v+1-d for d in x];p=[t**a for t in w];s=mp.fsum(p)
        return (a-1)*mean([1/t for t in w])-a*mp.fsum(q/t for q,t in zip(p,w))/s
    L=[mp.log1p(k*d/v)/k if k else d/v for d in x]
    lv=[-d/(v*(v+k*d)) for d in x];p=[mp.exp(-l) for l in L];s=mp.fsum(p)
    return -1/v+mp.fsum(q*t for q,t in zip(p,lv))/s-(1+k)*mean(lv)
def bracket_root(score,lo,hi):
    assert score(lo)>=0 and score(hi)<=0, (lo,hi,score(lo),score(hi))
    for _ in range(180):
        mid=(lo+hi)/2
        if score(mid)>0:lo=mid
        else:hi=mid
        if hi-lo<mp.mpf('1e-44')*max(1,abs(mid)):break
    return (lo+hi)/2
for row in bounds['rows']:
    f=gap if row['gap'] else pivot
    X=list(map(mp.mpf,row['xi']));V=list(map(mp.mpf,row['nuisance']))
    for k,v in [(X[0],V[0]),(X[1],V[1]),(mean(X),mean(V))]:
        values=[f(k,v),mp.diff(f,(k,v),(1,0)),mp.diff(f,(k,v),(0,1)),
                mp.diff(f,(k,v),(2,0)),mp.diff(f,(k,v),(1,1)),mp.diff(f,(k,v),(0,2))]
        for value,(lo,hi) in zip(values,row['jet']):
            assert mp.mpf(lo)<=value<=mp.mpf(hi),(row,k,v,value,lo,hi)
            checks+=1
    for k in [X[0],mean(X),X[1]]:
        for v in [V[0],mean(V),V[1]]:
            if row['gap']:
                a=-1/k;s=mean([(v+1-d)**a for d in x])**(1/a)
                coefficients=[1+v-s,s/a]
            else:
                L=[mp.log1p(k*d/v)/k if k else d/v for d in x]
                shift=-mp.log(mean([mp.exp(-l) for l in L]))
                coefficients=[v*mp.expm1(k*shift)/k if k else v*shift,v*mp.exp(k*shift)]
            for value,(lo,hi) in zip(coefficients,row['coefficients']):
                assert mp.mpf(lo)<=value<=mp.mpf(hi),(row,k,v,value,lo,hi)
                coefficient_checks+=1
    for k in [X[0],X[1],mean(X)]:
        score=lambda v:inner_score(k,v,row['gap'])
        r=bracket_root(score,V[0],V[1])
        assert V[0]<=r<=V[1],(row,k,r)
        root_checks+=1
face=bounds['face'];cut=mp.mpf(face['cut'])
for fraction in [mp.mpf(1),mp.mpf('.5'),mp.mpf('.0625')]:
    k=-1+(cut+1)*fraction
    seed=mp.mpf(face['gap_upper'])*fraction
    r=bracket_root(lambda e:inner_score(k,e,True),seed*mp.mpf('1e-40'),mp.mpf(face['gap_upper']))
    assert r>0 and gap(k,r)<=mp.mpf(face['upper'])
# The golden is normalized independently at high precision, using its stored
# binary-double sample exactly, not the already rounded C++ normalized array.
g=json.loads(pathlib.Path('univariateML-cpp/include/cppunivariate/distributions/gev/golden.json').read_text())
y=list(map(mp.mpf,g['data']));m=min(y);d=max(y)-m;y=[(t-m)/d for t in y]
def scores(k,v):
    L=[mp.log1p(k*t/v)/k for t in y]
    z=[t/v for t in y];t=[1+k*s for s in z]
    lx=[(s/u-l)/k for s,u,l in zip(z,t,L)]
    lv=[-s/(v*u) for s,u in zip(z,t)]
    w=[mp.exp(-l) for l in L];sw=mp.fsum(w)
    E=lambda a:mp.fsum(p*q for p,q in zip(w,a))/sw
    return E(lx)-mean(L)-(1+k)*mean(lx),-1/v+E(lv)-(1+k)*mean(lv)
e=g['estimates'];k0=mp.mpf(e['xi']);v0=mp.mpf(e['sigma'])/d-k0*(mp.mpf(e['mu'])-m)/d
k,v=mp.findroot(scores,(k0,v0),tol=mp.mpf('1e-45'),maxsteps=30)
L=[mp.log1p(k*t/v)/k for t in y];shift=-mp.log(mean([mp.exp(-l) for l in L]))
mu=m+d*v*mp.expm1(k*shift)/k;sigma=d*v*mp.exp(k*shift)
ll=n*(pivot(k,v,y)-mp.log(d))
ref=dict(mu=mu,sigma=sigma,xi=k)
for name,value in ref.items():assert abs(value-mp.mpf(e[name]))<=mp.mpf('1e-10')*abs(value)
assert abs(ll-mp.mpf(g['logLik']))<=mp.mpf('1e-10')*abs(ll)
gev_reference={**{p:mp.nstr(v,50) for p,v in ref.items()},'logLik':mp.nstr(ll,50)}
g=json.loads(pathlib.Path('univariateML-cpp/include/cppunivariate/distributions/frechet/golden.json').read_text())
y=list(map(mp.mpf,g['data']));m=min(y);d=max(y)-m;y=[(t-m)/d for t in y]
e=g['estimates'];k0=1/mp.mpf(e['lambda']);v0=k0*(m-mp.mpf(e['mu']))/d
k,v=mp.findroot(scores,(k0,v0),tol=mp.mpf('1e-45'),maxsteps=30)
L=[mp.log1p(k*t/v)/k for t in y];shift=-mp.log(mean([mp.exp(-l) for l in L]))
ref=dict(lambda_=1/k,mu=m-d*v/k,sigma=d*v*mp.exp(k*shift)/k)
ref['lambda']=ref.pop('lambda_')
ll=len(y)*(pivot(k,v,y)-mp.log(d))
for name,value in ref.items():assert abs(value-mp.mpf(e[name]))<=mp.mpf('1e-10')*abs(value)
assert abs(ll-mp.mpf(g['logLik']))<=mp.mpf('1e-10')*abs(ll)
frechet_reference={**{p:mp.nstr(v,50) for p,v in ref.items()},'logLik':mp.nstr(ll,50)}
y=list(map(mp.mpf,bounds['seeded_sample']))
k,v=mp.findroot(scores,(mp.mpf('-.31554486140157'),mp.mpf('.4508')),tol=mp.mpf('1e-45'),maxsteps=30)
L=[mp.log1p(k*t/v)/k for t in y];shift=-mp.log(mean([mp.exp(-l) for l in L]))
mu=v*mp.expm1(k*shift)/k;sigma=v*mp.exp(k*shift);ll=len(y)*pivot(k,v,y)
for actual,expected,scale in zip(bounds['seeded_fit'],[mu,sigma,k,ll],[sigma,sigma,1,abs(ll)]):
    assert abs(mp.mpf(actual)-expected)<=mp.mpf('1e-10')*max(abs(expected),scale)
seeded_reference={p:mp.nstr(t,50) for p,t in zip(['mu','sigma','xi','logLik'],[mu,sigma,k,ll])}
print(json.dumps(dict(precision=mp.mp.dps,interval_derivative_comparisons=checks,
    independent_inner_root_inclusions=root_checks,public_coefficient_comparisons=coefficient_checks,
    seeded_reference=seeded_reference,one_sided_face_comparisons=3,
    golden_reference=gev_reference,frechet_reference=frechet_reference,
    evidence='Independent high-precision checks of computed bounds and coefficients; the accompanying proof supplies global coverage.'),indent=2))
