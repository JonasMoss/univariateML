#!/usr/bin/env python3
import json,subprocess,sys,re
from pathlib import Path
import mpmath as mp
mp.mp.dps=70
root=Path(__file__).resolve().parents[4]
g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())
x=list(map(mp.mpf,g['data']));n=len(x)
text='\n'.join(map(repr,g['data']))+'\n'
lines=subprocess.run([sys.argv[1]],input=text,text=True,capture_output=True,check=True).stdout.splitlines()
assert len(lines)==4
records=[]
for mask,line in enumerate(lines):
    heldmean=mp.mpf(g['estimates']['mean']) if mask&1 else None
    heldsd=mp.mpf(g['estimates']['sd']) if mask&2 else None
    def equations(m,p):
        G=mp.fsum(mp.sign(m-v)*abs(m-v)**(p-1) for v in x)
        if heldsd is None:
            T=mp.fsum(abs(v-m)**p for v in x)
            U=mp.fsum(abs(v-m)**p*mp.log(abs(v-m)) if v!=m else 0 for v in x)
            A=mp.digamma(1+1/p)+mp.log(p)-p*U/T+mp.log(T/n)
        else:
            b=heldsd*mp.sqrt(mp.gamma(1/p)/mp.gamma(3/p))
            T=mp.fsum((abs(v-m)/b)**p for v in x)
            U=mp.fsum((abs(v-m)/b)**p*mp.log(abs(v-m)/b) if v!=m else 0 for v in x)
            B=(3*mp.digamma(1+3/p)-mp.digamma(1+1/p))/2
            A=mp.digamma(1+1/p)-B+(p*B*T-p*p*U)/n
        return G,A
    if heldmean is None:
        m,p=mp.findroot(equations,(0,3),tol=mp.mpf('1e-60'))
    else:
        m=heldmean;p=mp.findroot(lambda p:equations(m,p)[1],(2.5,3.5),tol=mp.mpf('1e-60'))
    T=mp.fsum(abs(v-m)**p for v in x)/n
    s=heldsd if heldsd is not None else (p*T)**(1/p)/mp.sqrt(mp.gamma(1/p)/mp.gamma(3/p))
    a=line.split();assert a[2]=='0' and a[4]=='1',line
    values=list(map(mp.mpf,map(float,a[6:9])))
    errors=[abs(v-ref) for v,ref in zip(values,[m,s,p])]
    assert max(errors)<mp.mpf('2e-8'),(mask,errors)
    records.append({'mask':mask,'mean':str(m),'sd':str(s),'nu':str(p),'absolute_errors':list(map(str,errors))})
print(json.dumps({'precision':mp.mp.dps,'rows':records,'violations':0},indent=2))
