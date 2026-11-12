"""Independent density and original-angle integrals for large skew at df=5."""
import json, sys
import mpmath as mp
import study, check
mp.mp.dps=110
job=['st',1,1e-12,1,0,[0.,1.,1e12,5.]]
x=mp.mpf(job[2]); alpha=mp.mpf(job[5][2]); nu=mp.mpf(5)
theta=mp.atan(1/alpha)
constant=mp.gamma((nu+1)/2)/(mp.sqrt(nu*mp.pi)*mp.gamma(nu/2))
def density(v):
    t=x*v
    a=alpha*t*mp.sqrt((nu+1)/(nu+t*t))
    cdf=mp.mpf('.5') if a==0 else 1-mp.betainc((nu+1)/2,mp.mpf('.5'),0,(nu+1)/(nu+1+a*a),regularized=True)/2
    return 2*constant*mp.power(1+t*t/nu,-(nu+1)/2)*cdf
reference=theta/mp.pi+x*mp.quad(density,[0,1])
central=mp.betainc(mp.mpf('.5'),nu/2,0,x*x/(nu+x*x),regularized=True)
angular=central+theta/mp.pi*mp.quad(lambda u:mp.power(1+x*x/(nu*mp.sin(theta*u)**2),-nu/2) if u else 0,[0,1])
assert abs(reference-angular)<mp.mpf('1e-100')
result=dict(job=job,reference=float(reference),density_integral=mp.nstr(reference,105),angular_integral=mp.nstr(angular,105),modes={})
for mode in ['native','double']:
    actual=float(study.evaluate([job],mode)[0])
    error=check.error(actual,float(reference),True)
    result['modes'][mode]=dict(actual=actual,error=error)
    if sys.argv[1]=='after':assert error<1e-10
(study.H/('st-small-cdf-'+sys.argv[1]+'.json')).write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
