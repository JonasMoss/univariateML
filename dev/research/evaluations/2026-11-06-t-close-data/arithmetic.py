"""Independent 80-digit derivatives, including sampled nonpoint boxes."""
import json,subprocess,random
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
here=Path(__file__).resolve().parent
rng=random.Random(140120261106)
cases=[]
for v in [.501,1.,1.001,2.,3.,32.,128.,218.,539.,1e3,1e8,1e12,1e14]:
 for width in [0.,1e-5,.001]:
  cases.append(([-3.,-.3,0.,.2,2.,9.],v,.2,-.3,width))
inputs=''.join(f'{len(x)} {v:.17g} {m:.17g} {t:.17g} {w:.17g} '+' '.join(format(z,'.17g') for z in x)+'\n' for x,v,m,t,w in cases)
lines=subprocess.check_output(['build/scratch/t-close-arithmetic'],input=inputs,text=True).splitlines()
checks=[]
for case,line in zip(cases,lines):
 xx,v0,m0,t0,width=case;f=[mp.mpf(float(s)) for s in line.split()];center,unit=f[:2];x=[(mp.mpf(z)-center)/unit for z in xx];n=len(x);v0=mp.mpf(v0)
 exact=(mp.polygamma(1,v0/2)-mp.polygamma(1,(v0+1)/2))/4-1/(2*v0*v0)
 assert f[2]<=exact<=f[3],('gamma4',case,exact,f[2:4])
 # Numeric differentiation of the independent log-gamma objective, not the
 # implemented derivative formulas. Points include box centres and corners.
 def objective(m,t,w):
  v=v0*w
  return n*(mp.loggamma(v/2)-mp.loggamma((v+1)/2)+mp.log(v*mp.pi)/2+t)+(v+1)/2*mp.fsum(mp.log1p((z-m)**2*mp.exp(-2*t)/v) for z in x)
 offsets=[(0,0,0)] if width==0 else [(a,b,c) for a in [-1,1] for b in [-1,1] for c in [-1,1]]
 for offset in offsets:
  # Exact binary64 interval endpoints as passed to the engine.
  p=tuple(mp.mpf(float(base+width*sgn)) for base,sgn in zip((m0,t0,1.),offset))
  values=[mp.diff(objective,p,tuple(int(k==j) for k in range(3))) for j in range(3)]
  values += [mp.diff(objective,p,tuple(int(k==i)+int(k==j) for k in range(3))) for i in range(3) for j in range(3)]
  for j,value in enumerate(values):assert f[4+2*j]<=value<=f[5+2*j],(case,offset,j,str(value),str(f[4+2*j]),str(f[5+2*j]))
 checks.append(dict(nu=str(v0),width=width,points=len(offsets),quantities_per_point=12,gamma4=True,passed=True))
(here/'arithmetic.json').write_text(json.dumps(checks,indent=2)+'\n')
print('Independent ordinary derivative and gamma4 checks passed:',len(checks),'boxes')
