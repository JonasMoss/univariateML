"""Independent 90-digit log-gamma checks; no production evaluators imported."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=90
D=lambda x:mp.mpf(str(x))
H=lambda u:u**3/6+u**2/4+u/12
F=lambda z,u:mp.loggamma(z+u)-mp.loggamma(z)-u*mp.log(z)
worst_gamma=mp.mpf(0);gamma_cases=0
for z in map(D,['.001','.1','1','10','100','1e4','1e10']):
    for u in map(D,['.001','.2','.99','1','1.01','5','50','1000']):
        error=abs(F(z,u)-u*(u-1)/(2*z));bound=H(u)/z**2
        assert error<=bound
        worst_gamma=max(worst_gamma,error/bound);gamma_cases+=1

def bnb(x,r,a,b):
    return (mp.loggamma(r+x)-mp.loggamma(r)-mp.loggamma(x+1)+
            mp.loggamma(a+r)+mp.loggamma(b+x)-mp.loggamma(a+r+b+x)-
            mp.loggamma(a)-mp.loggamma(b)+mp.loggamma(a+b))
def nb(x,r,p):
    return mp.loggamma(r+x)-mp.loggamma(r)-mp.loggamma(x+1)+r*mp.log(p)+x*mp.log1p(-p)
points=[];worst_nb=mp.mpf(0);nb_cases=0
sample=[(0,2),(1,1),(4,2),(15,1)]
for r in map(D,['.2','1','5','100']):
    for p in map(D,['.01','.1','.5','.9','.99']):
        q=1-p
        C=sum(w*(r*(r-1)*q/p+x*(x-1)*p/q-2*r*x)/2 for x,w in sample)
        R=sum(w*(H(r)/p**2+H(D(x))/q**2+H(r+x)) for x,w in sample)
        for s in map(D,['.01','1','10','1000','1e8']):
            actual=sum(w*(bnb(x,r,s*p,s*q)-nb(x,r,p)) for x,w in sample)
            error=abs(actual-C/s)
            assert error<=R/s**2
            worst_nb=max(worst_nb,error/(R/s**2));nb_cases+=1
            # Exact reflection, independently from the beta/gamma pmf.
            reflected=sum(w*bnb(x,s*q,s*p,r) for x,w in sample)
            assert abs(reflected-sum(w*bnb(x,r,s*p,s*q) for x,w in sample))<D('1e-75')
worst_poisson=mp.mpf(0);poisson_cases=0
for r in map(D,['.2','2','20','200']):
    for b in map(D,['.3','3','30','300']):
        for lam in map(D,['.1','2','10']):
            h=1/r+1/b
            po=sum(w*(-lam+x*mp.log(lam)-mp.loggamma(x+1)) for x,w in sample)
            B=sum(w*((x-lam)**2-x)/2 for x,w in sample)
            R=lam**2/2*sum(w*x*(1+max(0,x-1)*h/4)**2 for x,w in sample)
            actual=sum(w*bnb(x,r,r*b/lam,b) for x,w in sample)
            assert actual<=po+B*h+R*h*h
            worst_poisson=max(worst_poisson,(actual-po-B*h)/(R*h*h));poisson_cases+=1
# Independently solve NB and compare two expressions for the signed coefficient.
x=[0]*20+[5]*20;n=len(x);mean=mp.fsum(x)/n
score=lambda r:mp.fsum(mp.digamma(r+y)-mp.digamma(r) for y in x)+n*mp.log(r/(r+mean))
r=mp.findroot(score,(D('.45'),D('.55')));p=r/(r+mean);q=1-p
variance=mp.fsum((y-mean)**2 for y in x)/n
c_direct=p*p*q/2*mp.fsum(-r/(p*p)-y/(q*q)+(r/p-y/q)**2 for y in x)
c_moments=(n*p*p*(variance-mean)/q-n*r*q)/2
assert abs(c_direct-c_moments)<D('1e-80')
assert c_direct<0
llnb=mp.fsum(nb(y,r,p) for y in x)
for a in map(D,['.05','1','10','100','1000','1e5']):
    b=a*q/p
    gap=mp.fsum(bnb(y,r,a,b) for y in x)-llnb
    # This is the fixed-(r,p) path, not a claimed optimized alpha profile.
    R=sum(H(r)/p**2+H(D(y))/q**2+H(r+y) for y in x)
    bound=R*p*p/(a*a)
    assert abs(gap-c_direct/a)<=bound
    points.append(dict(alpha=str(a),gap=mp.nstr(gap,35),alpha_times_gap=mp.nstr(a*gap,35),remainder_bound=mp.nstr(bound,35)))
poisson_asymptotic=[]
for scale in map(D,['100','1000','1e5','1e8']):
    lam=D('1.5');h=2/scale;xx=[0,2,2,2]
    po=mp.fsum(-lam+y*mp.log(lam)-mp.loggamma(y+1) for y in xx)
    gap=mp.fsum(bnb(y,scale,scale*scale/lam,scale) for y in xx)-po
    R=lam**2/2*sum(y*(1+max(0,y-1)*h/4)**2 for y in xx)
    B=D('-1.5')
    assert gap<=B*h+R*h*h
    assert abs(gap/h-B)<100/scale
    poisson_asymptotic.append(dict(size=str(scale),h=str(h),gap_over_h=mp.nstr(gap/h,35),coefficient=str(B)))
result=dict(poisson_asymptotic=poisson_asymptotic,digits=mp.mp.dps,gamma_cases=gamma_cases,gamma_max_error_to_bound=str(worst_gamma),
    nb_cases=nb_cases,nb_max_error_to_bound=str(worst_nb),poisson_cases=poisson_cases,
    poisson_max_signed_residual_to_bound=str(worst_poisson),nb_size=mp.nstr(r,70),nb_prob=mp.nstr(p,70),
    c1_direct=mp.nstr(c_direct,70),c1_moments=mp.nstr(c_moments,70),fixed_nb_path=points)
out=Path(__file__).with_name('high-precision.json');out.write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
