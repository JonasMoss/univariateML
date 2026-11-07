"""Resolve the two maxima found by multiple.py; probe their height crossing."""
import json
import numpy as np
from scipy.optimize import brentq, minimize_scalar
from scipy.special import betaln,digamma,expit
from pathlib import Path
root=Path(__file__).parent
case=json.loads((root/'multiple.json').read_text())['multiple_maxima'][0]
a=case['a'];base=np.array(case['x'])
def profile(x,b):
    lx=np.log(x)
    ls=brentq(lambda t:(a+b)*expit(lx-t).mean()-a,
        np.log(b)+lx.min()-np.log(a)-1,np.log(b)+lx.max()-np.log(a)+1,xtol=1e-13)
    logs=np.logaddexp(0,lx-ls).mean()
    return ((a-1)*lx.mean()-a*ls-(a+b)*logs-betaln(a,b),
        digamma(a+b)-digamma(b)-logs,np.exp(ls))
def maxima(t):
    x=base.copy();x[-1]*=np.exp(t)
    out=[]
    for lo,hi in [(.005,.14),(.14,.8)]:
        r=minimize_scalar(lambda b:-profile(x,b)[0],bounds=(lo,hi),method='bounded',options={'xatol':1e-14})
        ll,score,s=profile(x,r.x)
        b=float(r.x)
        if profile(x,b*(1-1e-4))[1]>0 and profile(x,b*(1+1e-4))[1]<0:
            b=brentq(lambda b:profile(x,b)[1],b*(1-1e-4),b*(1+1e-4),xtol=1e-14)
            ll,score,s=profile(x,b)
        out.append(dict(b=b,scale=s,logLik=len(x)*ll,score=score))
    return out
original=maxima(0)
probes=[];crossing=None
for t in np.linspace(-8,12,101):
    m=maxima(float(t));delta=m[0]['logLik']-m[1]['logLik']
    if all(abs(z['score'])<1e-6 for z in m):probes.append((float(t),delta))
for (l,dl),(h,dh) in zip(probes,probes[1:]):
    if dl*dh<0:
        t=brentq(lambda t:maxima(t)[0]['logLik']-maxima(t)[1]['logLik'],l,h,xtol=1e-12)
        crossing=dict(t=t,last_x=float(base[-1]*np.exp(t)),maxima=maxima(t),sign_bracket=[[l,dl],[h,dh]])
        break
proof=None
if crossing:
    t=crossing['t'];lo,hi=5.6463,5.6466
    x=base.copy();x[-1]*=np.exp(t)
    b0=.12
    valley=len(x)*profile(x,b0)[0]
    face=len(x)*(a*np.log(a/x.mean())-float(__import__('scipy').special.gammaln(a))+(a-1)*np.log(x).mean()-a)
    witness=crossing['maxima'][0]['logLik'];bw=crossing['maxima'][0]['b']
    radius=max(t-lo,hi-t)
    proof=dict(t_bracket=[lo,hi],endpoints=[maxima(lo),maxima(hi)],valley_b=b0,
        valley_logLik=valley,face_logLik=face,
        valley_uniform_gap=witness-valley-(1+bw+1+b0)*radius,
        face_uniform_gap=witness-face-(1+bw+1+a*len(x))*radius)
print(json.dumps(dict(a=a,x=base.tolist(),original=original,crossing=crossing,proof=proof),indent=2))
