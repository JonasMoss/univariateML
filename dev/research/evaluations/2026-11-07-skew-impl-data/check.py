"""Independent finite numerical checks, not a global-optimality proof.
Compile probe.cpp as recorded in validation.json; run this script under run.
Uses mpmath only for independent t-CDF/density and scalar score refinement.
"""
import json, math, pathlib, random, subprocess, sys
import mpmath as mp
mp.mp.dps=50
root=pathlib.Path(__file__).resolve().parent
rng=random.Random(11616)
cases=[]
for family in range(3):
    for j in range(6):
        x=[rng.uniform(-3,-.05) for _ in range(4)]+[rng.uniform(.05,5) for _ in range(5)]
        cases.append(dict(family=family,x=x,mean=0.,sd=1.3,nu=[.5,3,8,30,3,5][j] if family==2 else [2.5,3,5,12,3,8][j],xi=[.3,1,2,10,.7,1.5][j]))
cases.append(dict(family=1,x=[-35.321072334324725,11.459074428000962,-.015351125824354262,-.2513099360462538,-297.3459307209368,.5881721220894813],mean=0.,sd=1.,nu=3.,xi=10.))
inputs=''.join(' '.join(map(str,[c['family'],len(c['x']),c['mean'],c['sd'],c['nu'],c['xi'],*c['x']]))+'\n' for c in cases)
output=subprocess.run([sys.argv[1]],input=inputs,text=True,capture_output=True,check=True).stdout.splitlines()
assert len(output)==len(cases)
def tcdf(z,nu):
    if z==0:return mp.mpf('.5')
    p=mp.betainc(nu/2,mp.mpf('.5'),0,nu/(nu+z*z),regularized=True)/2
    return p if z<0 else 1-p
def tpdf(z,nu):return mp.gamma((nu+1)/2)/(mp.sqrt(nu*mp.pi)*mp.gamma(nu/2))*(1+z*z/nu)**(-(nu+1)/2)
checks=[]
for c,line in zip(cases,output):
    status,cert,p,ll=map(float,line.split());assert status==0 and cert==1,(c,line)
    xs=list(map(mp.mpf,c['x']));nu=mp.mpf(c['nu']);xi=mp.mpf(c['xi']);sd=mp.mpf(c['sd']);M=mp.mpf(c['mean'])
    if c['family']<2:
        m1=mp.gamma((nu-1)/2)/mp.gamma(nu/2)*mp.sqrt((nu-2)/mp.pi)
        shift=m1*(xi-1/xi);su=mp.sqrt(1+(1-m1*m1)*(xi-1/xi)**2)
        def score(t):
            ans=0 if c['family']==0 else len(xs)
            for x in xs:
                z=(x-t)*su/sd+shift if c['family']==0 else (x-M)*su*mp.exp(t)+shift
                d=(nu-2)*(xi if z>0 else 1/xi)**2+z*z
                ans+=(nu+1)*su/sd*z/d if c['family']==0 else -(nu+1)*(z-shift)*z/d
            return ans
        start=mp.mpf(p) if c['family']==0 else -mp.log(p)
        ref=mp.findroot(score,(start-mp.mpf('1e-5'),start+mp.mpf('1e-5')),tol=mp.mpf('1e-40'))
        refp=ref if c['family']==0 else mp.exp(-ref)
        def density(x):
            mean=refp if c['family']==0 else M;s=sd if c['family']==0 else refp
            z=(x-mean)*su/s+shift
            return su/s*2/(xi+1/xi)*mp.gamma((nu+1)/2)/(mp.sqrt((nu-2)*mp.pi)*mp.gamma(nu/2))*(1+(z/(xi if z>0 else 1/xi))**2/(nu-2))**(-(nu+1)/2)
        refll=sum(mp.log(density(x)) for x in xs)
    else:
        qs=[(x-M)/sd*mp.sqrt((nu+1)/(nu+((x-M)/sd)**2)) for x in xs]
        def score(a):return sum(q*tpdf(a*q,nu+1)/tcdf(a*q,nu+1) for q in qs)
        refp=mp.findroot(score,(mp.mpf(p)-mp.mpf('1e-5'),mp.mpf(p)+mp.mpf('1e-5')),tol=mp.mpf('1e-40'))
        refll=sum(mp.log(2*tpdf((x-M)/sd,nu)/sd*tcdf(refp*q,nu+1)) for x,q in zip(xs,qs))
    error=abs(float(refp)-p)/(1+abs(float(refp)));llerror=abs(float(refll)-ll)
    assert error<2e-8 and llerror<2e-10,(c,line,error,llerror)
    checks.append(dict(family=c['family'],parameter=p,reference=float(refp),relative_error=error,loglik_error=llerror))
# The appended C++ rational-range case checks extrema and seam crossings.
result=dict(seed=11616,mpmath_dps=50,cases=checks,max_parameter_error=max(z['relative_error'] for z in checks),max_loglik_error=max(z['loglik_error'] for z in checks))
(root/'checks.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps({k:v for k,v in result.items() if k!='cases'}))
