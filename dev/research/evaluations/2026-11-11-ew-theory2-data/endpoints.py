#!/usr/bin/env python3
"""High-precision checks of free and held scale endpoint envelopes."""
import json
import mpmath as m
m.mp.dps=100
ys=list(map(m.mpf, ['0','.3','1','1','1']))
n=len(ys); s=m.mpf(1); d=sum(s-y for y in ys); k=n/d
r=1/m.expm1(1)
def ll(b, mu):
    c=k/b
    ans=m.mpf(0)
    for y in ys:
        t=m.exp(b*(y-mu)); h=m.log(-m.expm1(-t))
        ans+=m.log(b*c)-y+m.log(t)-t+(c-1)*h
    return ans
power=n*m.log(k)-sum(ys)-k*d
held=power+3*m.log(r)
out=[]
for b in map(m.mpf,[20,80,320,1280]):
    out.append({'b':int(b),'held_gap':float(held-ll(b,s)),
                'free_gap':float(power-ll(b,s+1/m.sqrt(b)))})
print(json.dumps({'digits':100,'log_x':[str(y) for y in ys],
                  'endpoint_multiplicity':3,'power_envelope':float(power),
                  'held_scale_envelope':float(held),'rows':out},indent=2))
