#!/usr/bin/env python3
"""Where is the MLE of kumar, gompertz and sgomp on the snapshot's near-constant class?

The class is [c, c, c+] with c the golden sample's median and c+ the next
binary64 value (fit-snapshot.py). The question (strict-rule memo, Q1) is
whether a public coefficient of the maximizer lies outside binary64, which
makes failure(evaluation) ("an unrepresentable result") the honest outcome.

kumar     the dossier proves a unique finite MLE with b = -n/S(a),
          S(a) = sum log(1 - x_i^a); the profile in a is strictly concave.
          Here: Newton on the profile score at 80 digits.
gompertz  the dossier proves a unique finite MLE when mean(x^2) < 2 mean(x)^2,
          with a = n/I(b), I(b) = sum (e^{b x_i} - 1)/b and a strictly concave
          profile b sum x - n log I(b). Here: Newton on the profile score.
sgomp     the dossier proves existence (uniqueness unknown); for held b the
          shape eta is unique (strictly decreasing score). Here: the profile
          in log b is scanned on a grid over 40 decades and refined; this is
          numerical evidence of where the maximum lies, not a uniqueness proof.

Every computation uses the exact binary64 inputs (mpmath at 80 digits).
Usage: python3 near_constant_maximizers.py > near_constant_maximizers.txt
"""
import json, math, sys
from pathlib import Path
import mpmath as mp
ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT/'dev/validation'))
from strict_common import snapshot_module
fs = snapshot_module()
mp.mp.dps = 80
DBL_MAX = mp.mpf(2)**1024*(1-mp.mpf(2)**-53)
DBL_TRUE_MIN = mp.mpf(2)**-1074

def sample(ident):
    meta, golden = fs.read(ident)
    return fs.classes(meta, golden)['near-constant']

def representable(v):
    return DBL_TRUE_MIN <= abs(v) <= DBL_MAX

def newton(score, dscore, t):
    for _ in range(200):
        step = score(t)/dscore(t); t -= step
        if abs(step) < mp.mpf(10)**-60*max(1, abs(t)): return t
    raise RuntimeError('no convergence')

def kumar(x):
    x = [mp.mpf(v) for v in x]; n = len(x); L = [mp.log(v) for v in x]
    # profile in s = log a: l(a) = n log a + n log n - n log(-S) + (a-1) sum L - n - S
    def S(a): return mp.fsum(mp.log1p(-mp.exp(a*l)) for l in L)
    def prof(s):
        a = mp.exp(s); return n*s - n*mp.log(-S(a)) + (a-1)*mp.fsum(L) - S(a)
    d = lambda s: mp.diff(prof, s); dd = lambda s: mp.diff(prof, s, 2)
    s = newton(d, dd, mp.log(2/((x[2]-x[0])/x[0])))
    a = mp.exp(s); b = -n/S(a)
    return dict(a=a, b=b, loglik=prof(s)+n*mp.log(n)-n, curvature=dd(s))

def gompertz(x):
    x = [mp.mpf(v) for v in x]; n = len(x)
    def logI(b): return mp.log(mp.fsum(mp.expm1(b*v) for v in x)/b)
    prof = lambda s: mp.exp(s)*mp.fsum(x) - n*logI(mp.exp(s))      # s = log b
    d = lambda s: mp.diff(prof, s); dd = lambda s: mp.diff(prof, s, 2)
    s = newton(d, dd, mp.log(2/(x[2]-x[0])))
    b = mp.exp(s); a = n/mp.exp(logI(b))
    m1 = mp.fsum(x)/n; m2 = mp.fsum(v*v for v in x)/n
    return dict(a=a, b=b, condition_mean_x2_lt_2mean2=m2 < 2*m1*m1, curvature=dd(s))

def sgomp(x):
    x = [mp.mpf(v) for v in x]; n = len(x)
    def loglik(b, eta):
        return mp.fsum(mp.log(b) - b*v - eta*mp.exp(-b*v) + mp.log(1+eta*(-mp.expm1(-b*v))) for v in x)
    def eta_hat(b):
        v = [mp.exp(-b*t) for t in x]
        score = lambda e: mp.fsum(-vi+(1-vi)/(1+e*(1-vi)) for vi in v)
        if score(0) <= 0: return mp.mpf(0)
        # bracket u = log eta: the score decreases strictly in eta (dossier)
        lo = mp.mpf(-200)
        while score(mp.exp(lo)) <= 0: lo *= 2
        hi = mp.mpf(1)
        while score(mp.exp(hi)) > 0: hi *= 2
        for _ in range(600):
            mid = (lo+hi)/2
            if score(mp.exp(mid)) > 0: lo = mid
            else: hi = mid
            if hi-lo <= mp.mpf(10)**-60*max(1, abs(hi)): break
        return mp.exp((lo+hi)/2)
    prof = lambda s: loglik(mp.exp(s), eta_hat(mp.exp(s)))
    grid = [mp.mpf(k)/4 for k in range(-40, 4*45)]          # log10 b from -10 to 45
    values = [(prof(g*mp.log(10)), g) for g in grid]
    best = max(values)
    g = best[1]
    lo, hi = (g-mp.mpf(1)/4)*mp.log(10), (g+mp.mpf(1)/4)*mp.log(10)
    s = mp.findroot(lambda t: mp.diff(prof, t), (lo+hi)/2, tol=mp.mpf(10)**-40)
    b = mp.exp(s)
    return dict(b=b, eta=eta_hat(b), loglik=prof(s), grid_best_log10_b=g,
                second_best_away_from_peak=max(v for v, gg in values if abs(gg-g) > 2))

def show(v):
    return mp.nstr(v, 12) if isinstance(v, mp.mpf) else v

out = {}
for ident, fn in (('kumar', kumar), ('gompertz', gompertz), ('sgomp', sgomp)):
    x = sample(ident)
    r = fn(x)
    coeffs = {k: v for k, v in r.items() if k in ('a', 'b', 'eta')}
    out[ident] = dict(data=[float(v).hex() for v in x],
                      **{k: show(v) for k, v in r.items()},
                      unrepresentable=[k for k, v in coeffs.items() if not representable(v)])
    print(ident, json.dumps(out[ident], indent=1))
