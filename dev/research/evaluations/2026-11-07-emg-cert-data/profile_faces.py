"""A rate profile with a finite mode and a separate local normal-face maximum."""
import json
from pathlib import Path
import mpmath as m
m.mp.dps=70
x=[m.mpf(-10),m.mpf(9)]
normal=sum(-v*v/2-m.log(2*m.pi)/2 for v in x)
def ll(rate):
    return sum(m.log(rate)+rate*rate/2-rate*v+m.log(m.erfc((rate-v)/m.sqrt(2))/2) for v in x)
values={str(rate):str(ll(m.mpf(rate))) for rate in [1,100,1000,10000]}
assert ll(1)>normal and ll(100)<ll(1000)<ll(10000)<normal
out=dict(data=[-10,9],mu=0,sigma=1,normal_logLik=str(normal),rate_logLik=values,
    conclusion='A finite-rate maximum exists; the distinct normal face is also a local maximum in the compactified rate profile. This does not prove two global maximizers.')
Path(__file__).with_suffix('.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
def likelihood(rate,b):
    return sum(m.log(rate)+rate*rate/2-rate*v+m.log(m.erfc((rate-v)/m.sqrt(2))/2) for v in [-10,b])
def normal_for(b):return -(100+b*b)/2-m.log(2*m.pi)
rate,b=m.findroot(lambda rate,b:(m.diff(lambda v:likelihood(v,b),rate),likelihood(rate,b)-normal_for(b)),(m.mpf('1.5'),m.mpf('2.7')))
curvature=m.diff(lambda v:likelihood(v,b),rate,2)
assert rate>0 and b<10 and curvature<0
out=dict(data=['-10',str(b)],mu=0,sigma=1,rate=str(rate),logLik=str(likelihood(rate,b)),
    curvature=str(curvature),interpretation='Numerical finite stationary maximum tied with the normal face; not an exhaustive global-maximizer proof for these rounded coordinates.')
Path(__file__).with_name('face-tie.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
