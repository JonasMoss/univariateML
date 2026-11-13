#!/usr/bin/env python3
"""50-digit references for the TASK-214.3 defect cases (fix-nonfinite job).

Run from the repository root through /tmp/umvl-cpu/run:
    python3 dev/research/evaluations/2026-11-13-fix-nonfinite-data/references.py
Writes references.json next to this script. Every value is computed from the
README log density or its closed-form MLE equations, independently of the C++
fitters; each entry says which.
"""
import json
from pathlib import Path
import mpmath as mp

mp.mp.dps = 50
HERE = Path(__file__).resolve().parent
D = mp.mpf(2) ** -1074          # binary64 denorm_min
DBL_MAX = (2 - mp.mpf(2) ** -52) * mp.mpf(2) ** 1023
out = {}

def f(v):
    return mp.nstr(v, 25)

def hexdata(words):
    return [mp.mpf(float.fromhex(w)) for w in words]

# 1. gamma, data (1, 2, 3) * denorm_min, all free.
#    Shape: log k - digamma(k) = log(mean) - mean(log x) (scale-free);
#    scale = mean / k; logLik from the README density.
x = [D, 2 * D, 3 * D]
s = mp.log(sum(x) / 3) - sum(mp.log(v) for v in x) / 3
k = mp.findroot(lambda k: mp.log(k) - mp.digamma(k) - s, 5)
theta = (sum(x) / 3) / k
ll = sum(-k * mp.log(theta) - mp.loggamma(k) + (k - 1) * mp.log(v) - v / theta for v in x)
out['gamma-subnormal'] = dict(evidence='closed-form score equations, 50 digits', shape=f(k), scale=f(theta),
    scale_over_denorm_min=f(theta / D), logLik=f(ll),
    note='the scale lies below half of denorm_min: it rounds to zero in binary64')

# 2. norm on (1, 1, 1 + 2^-52): closed form.
x = [mp.mpf(1), mp.mpf(1), 1 + mp.mpf(2) ** -52]
m = sum(x) / 3
sd = mp.sqrt(sum((v - m) ** 2 for v in x) / 3)
sd_rounded_mean = mp.sqrt(sum((v - 1) ** 2 for v in x) / 3)
out['norm-near-constant'] = dict(evidence='closed form, 50 digits', mean=f(m), sd=f(sd),
    logLik=f(-mp.mpf(3) / 2 * (mp.log(2 * mp.pi) + 1) - 3 * mp.log(sd)),
    sd_profiled_at_mean_1=f(sd_rounded_mean))

# 3. logis on (1e-8, 1, 1e8): both score equations at 50 digits from the
#    default-tol fit; the likelihood is jointly concave in (mu/s, 1/s).
x = [mp.mpf(1e-8), mp.mpf(1), mp.mpf(1e8)]
def logis_scores(mu, s):
    z = [(v - mu) / s for v in x]
    return [sum(mp.tanh(t / 2) for t in z), sum(t * mp.tanh(t / 2) for t in z) - len(x)]
mu, s = mp.findroot(logis_scores, (mp.mpf('26275335.368361998'), mp.mpf('28619993.763420407')))
ll = sum(-(v - mu) / s - mp.log(s) - 2 * mp.log1p(mp.exp(-(v - mu) / s)) for v in x)
out['logis-wide'] = dict(evidence='score equations, 50 digits', location=f(mu), scale=f(s), logLik=f(ll))

# 3b. logis held-scale archive case 004719 (offset-1e8, scale held).
x = hexdata(['0x1.7d783f609d127p+26', '0x1.7d7840110d012p+26', '0x1.7d7840000db82p+26',
             '0x1.7d783fc2b056ap+26', '0x1.7d7840351a593p+26', '0x1.7d783f61e5240p+26',
             '0x1.7d7840acdca3bp+26', '0x1.7d78407da7befp+26', '0x1.7d783fce71df4p+26',
             '0x1.7d783fdce8cc5p+26', '0x1.7d78402f7065cp+26', '0x1.7d78407729152p+26'])
s = mp.mpf(1.0081430589945095)
mu = mp.findroot(lambda m: sum(mp.tanh((v - m) / (2 * s)) for v in x), mp.mpf('100000000.116'))
ll = sum(-(v - mu) / s - mp.log(s) - 2 * mp.log1p(mp.exp(-(v - mu) / s)) for v in x)
out['logis-archive-004719'] = dict(evidence='location score equation, 50 digits', location=f(mu), logLik=f(ll))

# 4. weibull, shape held at denorm_min, data (0.5, 1, 2, 4): scale = mean(x^k)^(1/k).
#    k*log(x) ~ 1e-324, so this block works at 400 digits.
with mp.workdps(400):
    x = [mp.mpf(0.5), mp.mpf(1), mp.mpf(2), mp.mpf(4)]
    k = D
    lam = mp.exp(mp.log(sum(mp.exp(k * mp.log(v)) for v in x) / 4) / k)
    ll = sum(mp.log(k) - mp.log(lam) + (k - 1) * mp.log(v / lam) - (v / lam) ** k for v in x)
    out['weibull-shape-denorm'] = dict(evidence='closed-form scale profile, 400 digits', scale=f(lam),
        scale_over_sqrt2_minus_1=mp.nstr(lam / mp.sqrt(2) - 1, 5), logLik=f(ll))

# 5. weibull archive 008443: both held; the log likelihood at the held values.
x = hexdata(['0x1.4d91d33b60d64p+500', '0x1.56ed2f1ffbd0dp+500', '0x1.7fae0540168e1p+499',
             '0x1.15ada6d2f5b03p+499', '0x1.716897c08000fp+499', '0x1.43932c103ad40p+499',
             '0x1.c41da698e91b3p+499', '0x1.2b09ec555a47ep+500', '0x1.755a8917d40afp+500',
             '0x1.bf79f040d7eacp+498', '0x1.7e81ff53c34b1p+498', '0x1.8c351a591e363p+500'])
k, lam = mp.mpf(2.9155677520153174), mp.mpf(3.961112786792183)
ll = sum(mp.log(k) - mp.log(lam) + (k - 1) * mp.log(v / lam) - (v / lam) ** k for v in x)
out['weibull-archive-008443'] = dict(evidence='README log density, 50 digits', logLik=f(ll),
    below_minus_dbl_max=bool(ll < -DBL_MAX))

# 6. gumbel archive 003008: both held; the log likelihood at the held values.
x = hexdata(['0x1.6b51ab798eca6p+499', '-0x1.92c1ac345ade0p+494', '-0x1.5b83e24bb8ee8p+498',
             '0x1.0a95335440d9fp+500', '0x1.2dccd5b1619dap+500', '0x1.88bb91b93e758p+500',
             '0x1.b3aa95091712ep+499', '-0x1.8dfeed445558ep+499', '0x1.81af676c8cfdap+499',
             '0x1.d783fe789fa5dp+497', '0x1.62e0db47c3f33p+500', '0x1.7c1201c5ee206p+501'])
mu, sig = mp.mpf(2.964923020698754), mp.mpf(3.021078660513882)
ll = sum(-mp.log(sig) - (v - mu) / sig - mp.exp(-(v - mu) / sig) for v in x)
out['gumbel-archive-003008'] = dict(evidence='README log density, 50 digits', logLik=mp.nstr(ll, 10),
    below_minus_dbl_max=bool(ll < -DBL_MAX))

# 7. gompertz, data (1, 2, 3) * denorm_min, all free. Gompertz is scale
#    equivariant: fit z = x / u with u = 2 * denorm_min, then a = a_z / u,
#    b = b_z / u. Profile: a(b) = n b / sum(expm1(b z)); score in b.
z = [mp.mpf(0.5), mp.mpf(1), mp.mpf(1.5)]
n = len(z)
def a_of(b): return n * b / sum(mp.expm1(b * v) for v in z)
def prof(b):
    a = a_of(b)
    return n * mp.log(a) + b * sum(z) - a / b * sum(mp.expm1(b * v) for v in z)
bz = mp.findroot(lambda b: mp.diff(prof, b), 1)
az = a_of(bz)
u = 2 * D
out['gompertz-subnormal'] = dict(evidence='profile score, 50 digits, scale equivariance', a=f(az / u), b=f(bz / u),
    a_exceeds_dbl_max=bool(az / u > DBL_MAX), b_exceeds_dbl_max=bool(bz / u > DBL_MAX),
    logLik=f(prof(bz) - n * mp.log(u)))

# 9. gamma class 'huge' (snapshot input), all free.
x = [mp.mpf(v) for v in (2.1929206633396517e+307, 4.316903372290887e+307, 7.156392554778722e+307,
                         8.806638121904167e+307, 1.1769861760077501e+308, 1.7000000000000001e+308)]
s = mp.log(sum(x) / len(x)) - sum(mp.log(v) for v in x) / len(x)
k = mp.findroot(lambda k: mp.log(k) - mp.digamma(k) - s, 2.7)
theta = (sum(x) / len(x)) / k
ll = sum(-k * mp.log(theta) - mp.loggamma(k) + (k - 1) * mp.log(v) - v / theta for v in x)
out['gamma-huge'] = dict(evidence='closed-form score equations, 50 digits', shape=f(k), scale=f(theta), logLik=f(ll))

(HERE / 'references.json').write_text(json.dumps(out, indent=1) + '\n')
print(json.dumps(out, indent=1))
