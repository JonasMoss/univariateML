"""Explore adaptive integration in standardized Prentice coordinates.

This experiment is not a production algorithm or a certified error bound.
Requires scipy; compare against reference.json generated independently with mpmath.
"""
import json
import math
import pathlib
import scipy.integrate
import scipy.special

ROOT = pathlib.Path(__file__).parent

def log_density(w, q):
    z = q*w
    if abs(z) < 0.1:
        term = 0.5
        total = term
        for k in range(3, 25):
            term *= z/k
            total += term
        penalty = w*w*total
    else:
        penalty = (math.expm1(z)-z)/(q*q)
    if abs(q) <= 0.25:
        q2 = q*q
        remainder = q2*(1/12-q2*q2/360+q2**4/1260-q2**6/1680)
    else:
        a = 1/(q*q)
        remainder = math.lgamma(a)-(a-0.5)*math.log(a)+a-0.5*math.log(2*math.pi)
    return -0.5*math.log(2*math.pi)-remainder-penalty

def smaller_tail(w, q):
    ld = log_density(w,q)
    slope = math.expm1(q*w)/q if q else w
    scale = max(1.0, abs(slope))
    direction = -1 if w < 0 else 1
    def f(t):
        if t == 1:
            return 0.0
        u = 1-t
        v = w+direction*t/(u*scale)
        try:
            logf = log_density(v,q)-ld-2*math.log(u)-math.log(scale)
        except OverflowError:
            return 0.0
        return math.exp(logf) if logf > -745 else 0.0
    integral, err = scipy.integrate.quad(f,0,1,epsabs=1e-13,epsrel=1e-13,limit=300)
    return ld+math.log(integral), err

rows=json.loads((ROOT/'reference.json').read_text())
worst=(0,None)
for row in rows:
    q=float(row['Q']); w=float(row['w'])
    got,err=smaller_tail(w,q)
    target=float(row['log_lower'] if w < 0 else row['log_upper'])
    diff=abs(got-target)
    if diff>worst[0]: worst=(diff,(q,w,got,target,err))
print('worst absolute log-tail error:',worst)
