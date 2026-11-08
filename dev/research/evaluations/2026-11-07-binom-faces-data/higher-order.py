#!/usr/bin/env python3
"""Local asymptotic diagnostics, supplemented by second-order-face.py.

The truncated-binomial fixture now has a uniform remainder and complete prefix
certificate in second-order-face.py. ZIP binary neighbors remain unresolved.
"""
import json
import math
from fractions import Fraction as F
import mpmath as mp

P = [50, 41, 33, 3, 3]
Q = [48, 48, 24, 8, 2]
n = sum(P)
def expectation(count, function):
    return sum(F(c, n) * function(k) for k, c in enumerate(count))
mean = expectation(Q, lambda k: k)
a = lambda k: -F(k * (k - 1), 2)
b = lambda k: -F(k * (k - 1) * (2 * k - 1), 12)
var_x = expectation(Q, lambda k: k * k) - mean * mean
mean_a = expectation(Q, a)
var_a = expectation(Q, lambda k: a(k) ** 2) - mean_a ** 2
cov = expectation(Q, lambda k: k * a(k)) - mean * mean_a
assert expectation(P, a) == mean_a
# Per-observation coefficient of 1/N^2 after profiling the natural parameter.
second = expectation(P, b) - expectation(Q, b) - (var_a - cov * cov / var_x) / 2
assert second < 0

mp.mp.dps = 70
u = mp.mpf('1.6')
pi = 3 * mp.exp(-u) / (1 + 3 * mp.exp(-u))
w = mp.mpf(1) / 4
S, k, n0, F2, T = 10, 5, 5, 16, 96
first = -F2 / 2 + S * u - (k + n0 * w) * u * u / 2
curvature = -S / u ** 2 + n0 * w * (1 - w)
derivative = S - u * (k + n0 * w) + n0 * u ** 2 * w * (1 - w) / 2
fixed_second = -mp.mpf(T) / 12 + S * u ** 2 / 2 - (k + n0 * w) * u ** 3 / 3 + n0 * w * (1 - w) * u ** 4 / 8
profiled_second = fixed_second - derivative ** 2 / (2 * curvature)
assert abs(first) < mp.mpf('1e-60') and curvature < 0 and profiled_second < 0
# Adjacent exact binary64 inflations straddle the real zero-coefficient value.
# At a stationary face rate, A=(S*u-F2)/2. The turning discriminant
# is exactly -7500, so each of these held-inflation faces has a unique rate.
assert (10*(10+5))**2-4*(10*5)*(10*10+10*5)==-7500
binary_diagnostics=[]
low=float('.3772146069463776')
for value in [low,math.nextafter(low,math.inf)]:
    held=mp.mpf(value)
    def weight(rate):
        q=mp.exp(-rate)
        return (1-held)*q/(held+(1-held)*q)
    def face_score(rate):return 10/rate-5-5*weight(rate)
    left=mp.mpf(1);right=mp.mpf(2)
    for _ in range(250):
        mid=(left+right)/2
        if face_score(mid)>0:left=mid
        else:right=mid
    rate=(left+right)/2;ww=weight(rate)
    assert abs(face_score(rate))<mp.mpf('1e-60')
    first_actual=(10*rate-16)/2
    curv=-10/rate**2+5*ww*(1-ww)
    derivative_actual=10-rate*(5+5*ww)+5*rate**2*ww*(1-ww)/2
    fixed_b=-mp.mpf(96)/12+10*rate**2/2-(5+5*ww)*rate**3/3+5*ww*(1-ww)*rate**4/8
    b=fixed_b-derivative_actual**2/(2*curv)
    binary_diagnostics.append(dict(binary64=repr(value),exact_input=str(held),rate=str(rate),first_coefficient=str(first_actual),second_coefficient=str(b),formal_two_term_size_scale=str(-2*b/first_actual) if first_actual>0 else None))
assert mp.mpf(binary_diagnostics[0]['first_coefficient'])<0
assert mp.mpf(binary_diagnostics[1]['first_coefficient'])>0

print(json.dumps(dict(
    interpretation='Truncated binomial is certified by second-order-face.py and the implemented uniform bound plus prefix. ZIP still lacks decisive signed comparison at these binary inputs. The formal two-term ZIP size scale is not an MLE estimate.',
    binary64_neighbours=binary_diagnostics,
    tbinom=dict(second_per_observation_exact=str(second), second_per_observation=float(second)),
    held_zib=dict(pi_real=str(pi), first=str(first), face_curvature=str(curvature), second_profiled=str(profiled_second)),
), indent=2))
