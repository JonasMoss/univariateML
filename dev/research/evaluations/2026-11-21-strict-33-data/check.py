#!/usr/bin/env python3
"""Check the analytic bounds and stationary fits on the frozen dyadic inputs."""
from pathlib import Path
import sys

import mpmath as mp

# Locate the repository from the evaluation data directory.
sys.path.insert(0, str(Path(__file__).resolve().parents[4] / "dev/validation"))
from strict_common import snapshot_module

mp.mp.dps = 90
fs = snapshot_module()
float_max = (2 - mp.mpf(2)**-52) * mp.mpf(2)**1023
float_min = mp.mpf(2)**-1074


def sample(family, label):
    data = fs.classes(*fs.read(family))[label]
    print(f"{family} {label} {[float(v).hex() for v in data]}")
    return [mp.mpf(float(v)) for v in data]


def show(**values):
    for key, value in values.items():
        print(f"  {key}: {mp.nstr(value, 26)}")


x = sample("gompertz", "near-constant")
m, d = min(x), max(x) - min(x)
b0 = 2048/m
lower = 1 - b0*d - b0*max(x)/mp.expm1(b0*m)
upper = b0/mp.expm1(b0*m)
assert lower > mp.mpf(".5") and upper < float_min

# Center at the sample minimum before differentiating. The log integral
# contains a common exp(b*m) that must cancel from the rate profile.
def gompertz_profile(b):
    return (mp.log(b) + b*mp.fsum(v-m for v in x)/len(x)
            - mp.log(mp.fsum(mp.exp(b*(v-m)) for v in x)
                     - len(x)*mp.exp(-b*m)))


bd = mp.findroot(lambda t: mp.diff(lambda z: gompertz_profile(z/d), t), (1, 3))
b = bd/d
show(score_lower=lower, log_a_upper=mp.log(upper), b=b,
     log_a=mp.log(len(x)*b) - b*m -
     mp.log(mp.fsum(mp.exp(b*(v-m)) for v in x)-len(x)*mp.exp(-b*m)))

x = sample("kumar", "near-constant")
logs = [-mp.log(v) for v in x]
tmin, delta = min(logs), max(logs)-min(logs)
a0 = 1024/tmin
lower = 1-a0*delta-1024/mp.expm1(1024)
assert lower > mp.mpf(".5") and mp.expm1(1024) > float_max


def g(a):
    return mp.fsum(-mp.log1p(-mp.exp(-a*t)) for t in logs)


def kumar_profile(a):
    return mp.log(a)-mp.log(g(a))-a*mp.fsum(logs)/len(x)+g(a)/len(x)


ad = mp.findroot(lambda t: mp.diff(lambda z: kumar_profile(z/delta), t), (1, 3))
a = ad/delta
show(score_lower=lower, log_b_lower=mp.log(mp.expm1(1024)),
     a=a, log_b=mp.log(len(x)/g(a)))

x = sample("sgomp", "near-constant")
m, d = min(x), max(x)-min(x)
b = 1/d
delta = [b*(v-m) for v in x]
witness = mp.fsum(mp.log(b)-z-mp.exp(-z) +
                  mp.log1p(mp.exp(-b*m)*(-mp.expm1(-z))) for z in delta)
upper = len(x)*mp.log((mp.log(1+float_max)+1)/m)
assert witness > upper + len(x)
show(witness_loglik=witness, representable_loglik_upper=upper, gap=witness-upper)

x = sample("gengamma", "two")
d = mp.log(max(x)/min(x))
jacobian = mp.fsum(mp.log(v) for v in x)
for k in map(mp.mpf, [".001", ".1", ".5", "1", "10", "1000"]):
    A = 1/k
    # A/tanh(s) = s has its root above A. A+1 is a strict upper bound.
    lo, hi = A, A+1
    for _ in range(320):
        mid = (lo+hi)/2
        if mid*mp.tanh(mid) < A:
            lo = mid
        else:
            hi = mid
    s = (lo+hi)/2
    value = 2*(mp.log(2*s/d)-mp.loggamma(k)+k*(mp.log(k)-1-mp.log(mp.cosh(s))))-jacobian
    derivative = 2*(mp.log(k)-mp.digamma(k)-mp.log(mp.cosh(s)))
    assert derivative < 0
    show(k=k, loglik=value, derivative=derivative)
show(face_loglik=2*(mp.log(2/d)-1)-jacobian, exponent=2/d)

x = sample("sstd", "two")
d = max(x)-min(x)
for nu in map(mp.mpf, [2, 3, 10, 100, 1000]):
    log_c = mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(mp.pi*nu)/2
    z2 = 2*nu/(nu-1)
    value = mp.log(4*z2/d**2) + 2*log_c - (nu+1)*mp.log1p(z2/nu)/2
    derivative = (mp.digamma((nu+1)/2)-mp.digamma(nu/2)
                  + mp.log((nu-1)/(nu+1))/2)
    assert derivative > 0
    show(nu=nu, loglik=value, derivative=derivative)
show(face_loglik=mp.log(4/mp.pi)-1-2*mp.log(d), halfnormal_scale=d/mp.sqrt(2))
