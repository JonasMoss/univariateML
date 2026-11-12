"""Normal-face ST reference with an explicit finite-df coupling bound."""
import json, sys
import mpmath as mp
import study, check
mp.mp.dps=110
nu=sys.float_info.max; alpha=1e8
theta=mp.atan(1/mp.mpf(alpha)); records=[]
for x in [-2e-16,2e-16]:
    t=mp.mpf(x)
    rho=abs(t)/mp.sin(theta)
    assert rho<2
    normal=theta/mp.pi+t*mp.quad(lambda u:mp.exp(-(t*u)**2/2)/mp.sqrt(2*mp.pi)*mp.erfc(-mp.mpf(alpha)*t*u/mp.sqrt(2)),[0,1])
    # K_nu=E exp(-W r^2/2), E W=1, Var W=2/nu. The normalized
    # angular Gaussian integral has derivative <=1.3 and is >=.02
    # for rho<=2. A constant 512 also covers the positive central mass.
    bound=512/mp.sqrt(mp.mpf(nu))
    job=['st',1,x,1,0,[0.,1.,alpha,nu]]
    r=dict(job=job,reference=float(normal),normal_reference=mp.nstr(normal,105),
           finite_df_relative_bound=mp.nstr(bound,20),modes={})
    for mode in ['native','double']:
        actual=float(study.evaluate([job],mode)[0]); error=check.error(actual,float(normal),True)
        r['modes'][mode]=dict(actual=actual,error=error)
        if sys.argv[1]=='after':assert error<1e-10
    records.append(r)
(study.H/('st-normal-face-'+sys.argv[1]+'.json')).write_text(json.dumps(records,indent=2)+'\n')
print(json.dumps(records,indent=2))
