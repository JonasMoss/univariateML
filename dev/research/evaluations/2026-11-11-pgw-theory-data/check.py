#!/usr/bin/env python3
"""100-digit pointwise boundary and derivative checks; no global optimization."""
import json
from pathlib import Path
import mpmath as m
m.mp.dps = 100

def logf(x,a,b,c):
    t=(x/a)**b
    return m.log(b*c/x)+(b*m.log(x/a))+(c-1)*m.log1p(t)-m.expm1(c*m.log1p(t))

def target_gompertz_power(x,s,k):
    u=(x/s)**k
    return m.log(k/x)+m.log(u)+u-m.expm1(u)

def target_threshold(x,s,k):
    if x<=s: return m.ninf
    u=(x/s)**k
    return m.log(k/x)+m.log(u)+1-u

x=m.mpf('1.7'); a=m.mpf('1.2'); b=m.mpf('1.4'); c=m.mpf('0.8')
t=(x/a)**b; L=m.log1p(t)
curv=m.diff(lambda v: logf(x,a,b,v),c,2)
rows={"precision_digits":100,"concavity_residual":str(abs(curv-(-1/c**2-L**2*m.exp(c*L)))),"power_gompertz_face":[],"threshold_face":[]}
for j in [10,100,1000,10000]:
    z=m.mpf(j); k=m.mpf('1.3'); s=m.mpf('1.2')
    rows['power_gompertz_face'].append({'c':j,'log_density_error':str(abs(logf(x,s*z**(1/k),k,z)-target_gompertz_power(x,s,k)))})
    rows['threshold_face'].append({'b':j,'log_density_error':str(abs(logf(x,s,z,k/z)-target_threshold(x,s,k)))})
rows['constant_held_scale_below']=[]
for j in [10,100,1000]:
    z=m.mpf(j); s=m.mpf('2'); x0=m.mpf('1')
    rows['constant_held_scale_below'].append({'b':j,'logf_minus_logb':str(logf(x0,s,z,(s/x0)**z)-m.log(z))})
d=m.log(2); count=5; upper=3
khat=m.findroot(lambda k: count/k-upper*d*m.expm1(k*d), (1,3))
rows['two_point_threshold']={'power':str(khat),'score_residual':str(abs(count/khat-upper*d*m.expm1(khat*d))),'lower_value':'0.8','upper_value':'1.6','multiplicities':[2,3]}
Path(__file__).with_name('check.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
