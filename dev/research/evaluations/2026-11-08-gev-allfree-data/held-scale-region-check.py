"""Independent numerical checks of the supporting F1 compactness inequalities."""
import json,pathlib,mpmath as mp
mp.mp.dps=50
p=pathlib.Path('univariateML-cpp/include/cppunivariate/distributions/frechet/golden.json')
g=json.loads(p.read_text());x=list(map(mp.mpf,g['data']));n=len(x);m=min(x);R=max(x)-m;k=x.count(m)
sigma=mp.mpf(g['estimates']['sigma']);mu=mp.mpf(g['estimates']['mu']);shape=mp.mpf(g['estimates']['lambda'])
d=[(t-m)/sigma for t in x]
def ll(a,e):
    z=[mp.log(e+t) for t in d]
    return n*mp.log(a/sigma)-(a+1)*mp.fsum(z)-mp.fsum(mp.exp(-a*t) for t in z)
inc=ll(shape,(m-mu)/sigma);tau=R/sigma;delta=min(tau/2,mp.mpf('.5'));c=1-delta;A=-mp.log(c);C=mp.log1p(tau/2)
u1=lambda a:n*mp.log(a/sigma)+k*((a+1)*A-mp.exp(A*a))
u2=lambda a:n*mp.log(a/sigma)-(a+1)*C
v1=lambda a:n/a+k*A*(1-mp.exp(A*a))
v2=lambda a:n/a-C
bound=max(mp.mpf(1),mp.log(2)/A)
for _ in range(200):
    if u1(bound)<inc and u2(bound)<inc and v1(bound)<0 and v2(bound)<0:break
    bound*=2
else:raise AssertionError('tail bound not found')
epsilon=mp.mpf('.25')
face=lambda e:n*mp.log(bound/sigma)+k*(2*mp.log(1/e)-1/e)
for _ in range(200):
    if face(epsilon)<inc:break
    epsilon/=2
else:raise AssertionError('gap bound not found')
checks=0
for a in [bound,2*bound,4*bound]:
    for e in [c/2,c,1-delta/2,mp.mpf(1)]:
        assert ll(a,e)<= (u1(a) if e<=c else u2(a));checks+=1
for a in [mp.mpf(1),mp.sqrt(bound),bound]:
    for e in [epsilon,epsilon/2,epsilon/10]:
        assert ll(a,e)<=face(epsilon);checks+=1
root_checks=[]
for a in map(mp.mpf,[1,2,16,256,2048]):
    floor=(a*k/((a+1)*n))**(1/a)
    def score(e):
        return mp.fsum((a*mp.exp(-a*mp.log(e+t))-(a+1))/(e+t) for t in d)
    lo=floor;hi=mp.mpf(1)
    assert score(lo)>0 and score(hi)<0
    for _ in range(110):
        mid=(lo+hi)/2
        if score(mid)>0:lo=mid
        else:hi=mid
    e=(lo+hi)/2
    assert floor<=e<1 and e**(-a)<=2*n/k
    root_checks.append(dict(shape=str(a),floor=mp.nstr(floor,30),gap=mp.nstr(e,30),max_power=mp.nstr(e**(-a),30)))
print(json.dumps(dict(precision=mp.mp.dps,checks=checks,n=n,k=k,inner_root_checks=root_checks,
    held_sigma=mp.nstr(sigma,50),incumbent=mp.nstr(inc,50),lambda_upper=mp.nstr(bound,50),
    gap_lower=mp.nstr(epsilon,50),tail_upper=mp.nstr(max(u1(bound),u2(bound)),50),
    gap_face_upper=mp.nstr(face(epsilon),50),
    evidence='Numerical checks of proved supporting bounds for Open F1; no held-scale fitter certificate is claimed.'),indent=2))
