#!/usr/bin/env python3
"""Independent 80-digit checks; give the compiled check_bounds executable.
No fitted library estimate is used as an oracle. All nuisance roots are solved
independently with mpmath after rescaling the signed score.
"""
import itertools
import json
from pathlib import Path
import subprocess
import sys
import mpmath as mp
mp.mp.dps = 80
root = Path(__file__).resolve().parents[4]
golden = json.loads((root / 'univariateML-cpp/include/cppunivariate/distributions/ged/golden.json').read_text())
samples = [[-3,-1,0,1,4], [-1,1,1,1,1,1,1,1,1,1],
           [-.02,-.005,0,.007,.03], [-2,0,0,0,1], golden['data']]
rows = []
for sample, mean, sd, p in itertools.product(samples, [None, .2], [None, 1.3], [1.,1.2,2.,10.,100.]):
    rows.append((sample,mean,sd,p,p+.2))
for scale in [1e-150, 1e150]:
    for mean, sd in itertools.product([None, 0.], [None, 1.3*scale]):
        rows.append(([v*scale for v in samples[0]],mean,sd,2.,2.2))

def token(v):
    return 'nan' if v is None else repr(v)
text = ''.join(' '.join([str(len(x)),token(m),token(s),str(p),str(q),'200']+list(map(repr,x)))+'\n'
               for x,m,s,p,q in rows)
lines = subprocess.run([sys.argv[1]],input=text,text=True,check=True,capture_output=True).stdout.splitlines()
assert len(lines) == len(rows)

cache = {}
def profile(x, p, held_mean, held_sd):
    x = list(map(mp.mpf,x));p=mp.mpf(p)
    key = (tuple(x),p,held_mean)
    if key not in cache:
        if held_mean is not None:
            mean = mp.mpf(held_mean)
        elif p == 1:
            mean = sorted(x)[len(x)//2]
        else:
            lo,hi = min(x),max(x)
            for _ in range(180):
                mid = (lo+hi)/2
                radius = max(abs(v-mid) for v in x)
                score = mp.fsum(mp.sign(mid-v)*(abs(mid-v)/radius)**(p-1) for v in x)
                if score > 0: hi=mid
                elif score < 0: lo=mid
                else: lo=hi=mid;break
            mean = (lo+hi)/2
        moment = mp.fsum(abs(v-mean)**p for v in x)/len(x)
        cache[key] = (mean,moment)
    mean,moment = cache[key]
    if held_sd is None:
        logb = mp.log(p*moment)/p
    else:
        logb = mp.log(held_sd)+(mp.loggamma(1/p)-mp.loggamma(3/p))/2
    ll = len(x)*(-mp.log(2)-logb-mp.loggamma(1+1/p))-len(x)*moment*mp.exp(-p*logb)
    return mp.log(moment)/p,ll

checks = 0
for row,line in zip(rows,lines):
    x,m,s,p,q = row
    ml,mu,ib,tb,mv,iv,tv,uncertain = map(float,line.split())
    assert mv and iv and tv,(row,line)
    moment,_ = profile(x,p,m,s)
    assert mp.mpf(ml)<=moment<=mp.mpf(mu),(row,'moment',line,str(moment))
    checks += 1
    for v in [p,(p+q)/2,q]:
        _,ll = profile(x,v,m,s)
        assert ll<=mp.mpf(ib),(row,'interval',line,str(ll))
        checks += 1
    for v in [p,2*p,10*p]:
        _,ll = profile(x,v,m,s)
        assert ll<=mp.mpf(tb),(row,'tail',line,str(ll))
        checks += 1
print(json.dumps({'precision':mp.mp.dps,'rows':len(rows),'enclosure_checks':checks,'violations':0},indent=2))

analytic = 0
K = mp.mpf(9)/4-mp.euler-mp.log(2)/2
for t in [mp.mpf(1)/10000,mp.mpf('.01'),mp.mpf('.1'),mp.mpf('.5'),mp.mpf(1)]:
    B = (mp.loggamma(1+t)-mp.loggamma(1+3*t))/2
    Bp = (mp.digamma(1+t)-3*mp.digamma(1+3*t))/2
    Bpp = (mp.polygamma(1,1+t)-9*mp.polygamma(1,1+3*t))/2
    C = -3*mp.loggamma(1+t)/2+mp.loggamma(1+3*t)/2
    assert Bpp<0 and B<=mp.euler*t and C<=mp.mpf(9)/4*mp.zeta(2)*t*t
    assert mp.log(3)/2+B-t*Bp<=K+mp.mpf('1e-75')
    analytic += 4
for shape in [mp.mpf('1.001'),mp.mpf('1.1'),mp.mpf(2),mp.mpf(10),mp.mpf(10000)]:
    _,ll=profile([-1,1],shape,None,mp.mpf('.1'))
    _,laplace=profile([-1,1],mp.mpf(1),None,mp.mpf('.1'))
    assert ll<laplace
    analytic += 1
# An optimized O(1/nu) location path with unequal endpoint counts.
nu = mp.mpf(10)**7
r = mp.sqrt(3)
mean = r*mp.log(9)/(2*nu)
_,ll = profile([-r]+[r]*9,nu,mean,mp.mpf(1))
limit = -10*mp.log(2*r)-6*mp.exp(-mp.euler)
assert abs(ll-limit)<mp.mpf('2e-6')
analytic += 1
print(json.dumps({'analytic_inequalities':analytic,'violations':0},indent=2))

import math
rounded = math.sqrt(3)
_,finite = profile([-rounded,rounded],mp.mpf('1e19'),0.,mp.mpf(1))
old_face = -2*mp.log(2*mp.sqrt(3))-2*mp.exp(-mp.euler)
assert finite>old_face+1
print(json.dumps({'rounded_endpoint_counterexample':{
    'observation':repr(rounded),'held_mean':0,'held_sd':1,'finite_shape':'1e19',
    'finite_loglik':str(finite),'old_endpoint_face_loglik':str(old_face),
    'improvement':str(finite-old_face)}},indent=2))
