# Check beta free fits against mpmath MLEs (TASK-148).
# Usage: g++ -std=c++17 -O2 -IunivariateML-cpp/include -I<boost> dev/validation/beta-free-fit-check.cpp -o /tmp/bc
#        /tmp/bc > /tmp/beta.json && python3 dev/validation/beta-free-fit-check.py /tmp/beta.json
# Datasets: ramps near 0 and 1, nearly constant, U-shaped, tiny n, gamma-ratio samples.
import json, mpmath as mp, math, sys
worst=0
for k,c in enumerate(json.load(open(sys.argv[1]))):
    a0,b0=float.fromhex(c['alpha']),float.fromhex(c['beta'])
    if c['status']!=0: print(k,'status',c['status']); continue
    mp.mp.dps=60+int(max(0,math.log10(max(a0,b0,1/min(a0,b0)))))*2
    x=[mp.mpf(float.fromhex(v)) for v in c['x']]; n=len(x)
    s=sum(mp.log(v) for v in x)/n; r=sum(mp.log1p(-v) for v in x)/n
    f=lambda la,lb:[s-mp.digamma(mp.e**la)+mp.digamma(mp.e**la+mp.e**lb), r-mp.digamma(mp.e**lb)+mp.digamma(mp.e**la+mp.e**lb)]
    la,lb=mp.findroot(f,(mp.log(a0),mp.log(b0)),tol=mp.mpf(10)**(-mp.mp.dps+10),maxsteps=200,verify=False)
    a,b=mp.e**la,mp.e**lb
    ll=(a-1)*s*n+(b-1)*r*n-n*(mp.loggamma(a)+mp.loggamma(b)-mp.loggamma(a+b))
    rel=lambda u,v: float(abs((mp.mpf(u)-v)/v))
    worst=max(worst,rel(a0,a),rel(b0,b))
    print(f"{k:2d} n={n:3d} a {float(a):.4g} b {float(b):.4g} relerr a {rel(a0,a):.1e} b {rel(b0,b):.1e} ll abs {float(abs(float.fromhex(c['ll'])-ll)):.1e} it {c['it']}")
print('worst',worst)
