"""Independent 80-digit profile-difference, analytic score and Hessian checks."""
import json,subprocess,random
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
here=Path(__file__).resolve().parent
rng=random.Random(14020261106)
base=[-3.,-.3,0.,.2,2.,9.]
cases=[]
for v in [1.,1.001,2.,3.,32.,128.,1e3,1e8,1e12,1e14]:
    for m,t in [(0.,0.),(.2,-.3),(-.1,.4)]:cases.append((base,v,m,t))
for _ in range(30):cases.append(([rng.gauss(0,1) for _ in range(12)],rng.uniform(1,20),rng.uniform(-.5,.5),rng.uniform(-1,1)))
inputs=''.join(f'{len(x)} {v:.17g} {m:.17g} {t:.17g} '+' '.join(format(z,'.17g') for z in x)+'\n' for x,v,m,t in cases)
lines=subprocess.check_output(['build/scratch/t-port-arithmetic'],input=inputs,text=True).splitlines()
checks=[]
for case,line in zip(cases,lines):
    xx,v,m,t=case;v,m,t=map(mp.mpf,(v,m,t));f=list(map(lambda s:mp.mpf(float(s)),line.split()));center,unit,ta=f[:3];x=[(mp.mpf(z)-center)/unit for z in xx];n=len(x)
    bm=mp.fsum(x)/n;var=mp.fsum((z-bm)**2 for z in x)/n
    normal=-n*(mp.log(2*mp.pi*var)+1)/2
    c=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2
    delta=n*(c-t)-(v+1)/2*mp.fsum(mp.log1p(((z-m)*mp.exp(-t))**2/v) for z in x)-normal
    rs=[z-m for z in x];a2=mp.exp(2*ta);D=[r*r+a2 for r in rs]
    cp=(mp.digamma((v+1)/2)-mp.digamma(v/2))/2
    cm=(mp.polygamma(1,v/2)-mp.polygamma(1,(v+1)/2))/4
    g=[-(v+1)*mp.fsum(r/d for r,d in zip(rs,D)),n-(v+1)*mp.fsum(r*r/d for r,d in zip(rs,D)),v*(-n*cp-n*ta+mp.fsum(mp.log(d) for d in D)/2)]
    h00=(v+1)*mp.fsum((a2-r*r)/d**2 for r,d in zip(rs,D));h01=2*(v+1)*mp.fsum(r*a2/d**2 for r,d in zip(rs,D));h02=-v*mp.fsum(r/d for r,d in zip(rs,D));h11=2*(v+1)*mp.fsum(a2*r*r/d**2 for r,d in zip(rs,D));h12=-v*mp.fsum(r*r/d for r,d in zip(rs,D));h22=n*v*v*cm
    ordinary=-n*(cp-1/(2*v))+mp.fsum(mp.log1p(r*r/a2)-(1+1/v)*(r*r/(a2+r*r)) for r in rs)/2
    values=[delta,*g,h00,h01,h02,h01,h11,h12,h02,h12,h22,ordinary]
    for j,value in enumerate(values):assert f[3+2*j]<=value<=f[4+2*j],(case,j,str(value),str(f[3+2*j]),str(f[4+2*j]))
    checks.append(dict(n=n,nu=str(v),mu=str(m),t=str(t),enclosed_quantities=len(values),passed=True))
(here/'arithmetic.json').write_text(json.dumps(checks,indent=2)+'\n')
print('Independent arithmetic enclosures passed:',len(checks),'points x 14 quantities')
