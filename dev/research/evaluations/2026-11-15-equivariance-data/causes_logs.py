"""Log-based family group: burr, invburr, pareto4, fpareto, trgamma, gengamma.

Emulates, in binary64, the first quantities the C++ computes from std::log of
the near-constant sample, and compares them with the exact (mpmath) values.
"""
import sys, math, json
sys.path.insert(0, 'dev/validation')
import strict_common as sc
from mpmath import mp, mpf, log as mlog
mp.dps = 60
D = 'dev/research/evaluations/2026-11-15-fit-snapshot-data/'
EPS = 2.220446049250313e-16
rows = sc.load(D + 'native.jsonl.gz')
def data(f): return rows[(f, 'class', 'near-constant')]['data']
def data3(f): return rows[(f, 'class', 'tie3')]['data']
def ulp(v): return math.nextafter(abs(v), math.inf) - abs(v)

# ---- burr/certificate.hpp interval arithmetic (pad = 64 eps) ----
PAD = 64*EPS
class I:
    def __init__(s, lo, hi=None): s.lo = lo; s.hi = lo if hi is None else hi
    def valid(s): return math.isfinite(s.lo) and math.isfinite(s.hi) and s.lo <= s.hi
    def mag(s): return max(abs(s.lo), abs(s.hi))
def invalid(): return I(math.nan, math.nan)
def padded(a, b):
    if not (math.isfinite(a) and math.isfinite(b)) or a > b: return invalid()
    e = PAD*(1+max(abs(a), abs(b))); return I(a-e, b+e)
def add(a, b): return padded(a.lo+b.lo, a.hi+b.hi)
def neg(a): return I(-a.hi, -a.lo)
def sub(a, b): return add(a, neg(b))
def mul(a, b):
    v = [a.lo*b.lo, a.lo*b.hi, a.hi*b.lo, a.hi*b.hi]
    return padded(min(v), max(v)) if all(math.isfinite(t) for t in v) else invalid()
def div(a, b):
    if not b.valid() or not (b.lo > 0 or b.hi < 0): return invalid()
    return mul(a, padded(1/b.hi, 1/b.lo))
def ilog(a): return padded(math.log(a.lo), math.log(a.hi)) if a.valid() and a.lo > 0 else invalid()
class Sum:
    def __init__(s): s.lo = s.hi = s.mag = 0.0; s.n = 0
    def add(s, a): s.lo += a.lo; s.hi += a.hi; s.mag += a.mag(); s.n += 1
    def get(s): e = PAD*(s.n+1)*(1+s.mag); return I(s.lo-e, s.hi+e)

def burr_data(x):
    """burr/certificate.hpp data(); returns (standardised y, boxes, center, unit) or None."""
    y = sorted(math.log(v) for v in x); n = len(x)
    total = 0.0
    for t in y: total += t
    center = total/n
    var = 0.0
    for t in y: var += (t-center)*(t-center)/n
    unit = math.sqrt(var)
    if not (unit > 0): return None, dict(center=center, variance=var, unit=unit)
    box = [div(sub(padded(t, t), I(center)), I(unit)) for t in y]
    ys = [(t-center)/unit for t in y]
    return (ys, box), dict(center=center, variance=var, unit=unit, y=y)

def burr_P(x):
    got, info = burr_data(x)
    if got is None: return dict(data_ok=False, **info)
    ys, box = got; n = len(x)
    s = Sum()
    for b in box: s.add(sub(b, box[0]))
    dist = s.get()
    nn = I(float(n))
    P = sub(mul(nn, ilog(div(nn, dist))), nn)
    return dict(data_ok=True, standardised=ys, box_halfwidth=[(b.hi-b.lo)/2 for b in box],
                distances=(dist.lo, dist.hi), P_valid=P.valid(), unit=info['unit'], log_spacing=[info['y'][i]-info['y'][0] for i in range(n)])

def exact_logs(x):
    L = [mlog(mpf(v)) for v in x]; return [float(l-L[0]) for l in L]

out = {}
for f in ['burr', 'pareto4', 'invburr', 'trgamma', 'gengamma', 'fpareto']:
    x = data(f); x3 = data3(f); n = 3
    inp = x
    if f == 'invburr': inp = [1.0/v for v in x]           # invburr_mle reciprocals
    ex = exact_logs(x)                                    # exact log(x_i/x_0)
    fl = [math.log(v) for v in x]
    ll = [t-fl[0] for t in fl]
    rec = dict(c=x[0], ulp_c=ulp(x[0]), ulp_of_log_c=ulp(math.log(x[0])), exact_log_spread=ex[2],
               float_log_spread=ll[2], float_log_equal=(fl[0] == fl[2]),
               float_log_spread_in_ulps_of_log=(ll[2]/ulp(math.log(x[0]))),
               rel_error_of_float_spread=(abs(ll[2]-ex[2])/ex[2]))
    if f == 'invburr':
        rec['reciprocals_equal'] = inp[0] == inp[2]
        el = exact_logs(inp)  # exact log of reciprocals ratio
        rec['exact_log_spread_of_reciprocals_binary64'] = el[2]
        rec['spread_of_float_reciprocals_vs_exact_1_over_x'] = (math.log(inp[2])-math.log(inp[0]), -ex[2])
    if f in ('burr', 'pareto4', 'invburr'):
        rec['burr_cert'] = burr_P(inp)
    if f in ('trgamma', 'gengamma'):
        rec['lost_log_distinctions'] = (x[0] != x[2]) and (fl[0] == fl[2])
    if f == 'gengamma':
        mean = sum(fl)/3
        sd = math.sqrt(sum((t-mean)**2 for t in fl)/3)
        log_error = max(8*EPS*abs(t) for t in fl if t != 0)
        umax = (max(fl)-mean)/sd
        data_scale = min(sd, sd*umax)
        rec['gengamma'] = dict(sd_of_logs=sd, log_error=log_error, data_scale=data_scale,
                               arith_extra_16err_over_scale=16*log_error/data_scale)
    out[f] = rec
# ---- fpareto: gb2 standardize + concave certificate gates (gb2/concave_certificate.hpp) ----
x = data('fpareto'); fl = [math.log(v) for v in x]
mean = sum(fl)/3; u = [v-mean for v in fl]; shift = sum(u)/3; u = [v-shift for v in u]; mean += shift
sd = math.sqrt(sum(v*v for v in u)/3); u = [v/sd for v in u]
p, q = 0.4, 0.3                                   # held shape3, shape1 (golden holds)
r3 = rows[('fpareto', 'class', 'tie3')]['result']
a_pub = sc.number(r3['estimates']['shape2']); b_pub = sc.number(r3['estimates']['scale'])
A = a_pub*sd      # shape2 of the near-constant fit is shape2_tie3 / B, so A = shape2_nc * sd_nc = shape2_tie3 * sd_tie3
x3 = data3('fpareto'); f3 = [math.log(v) for v in x3]; m3 = sum(f3)/3; sd3 = math.sqrt(sum((t-m3)**2 for t in f3)/3)
A = a_pub*sd3
c = A*(math.log(b_pub)-m3)/sd3
logb = math.log(b_pub)
eps = EPS; target = min(1e-10, 1e-9)
radius = target/(4*(1+1/A+sd/A+sd*abs(c)/(A*A)))
gradient_error = 0.0; input_error = 0.0
for z in u:
    dv = 8*eps*(1+abs(mean)+sd*abs(z)+abs(logb))/sd
    dt = A*dv+16*eps*(1+abs(A*z)+abs(c)); du = min(1., dt/4)
    gradient_error += 0 + (p+q)*du                    # shape2 free: (p+q)*(dv+(|v|+dv)*du) is added as well
    gradient_error += (p+q)*(dv+(abs(z)+dv)*du) - (p+q)*du
    input_error += (p+q)*dt+dv*sd
out['fpareto_gb2_concave'] = dict(sd_of_logs=sd, A=A, c=c, dv_first=8*eps*(1+abs(mean)+sd*abs(u[0])+abs(logb))/sd,
    gradient_error_sum=gradient_error, radius=radius, curvature_scale_m_radius_over_2_upper_bound=(3/A**2+(p+q))*radius/2,
    note='certificate needs g < m*radius/2 with g >= gradient_error')
# same quantities for the exact-relative logs (log1p) for comparison
exl = exact_logs(x)
mean_e = sum(exl)/3; ue = [v-mean_e for v in exl]; sde = math.sqrt(sum(v*v for v in ue)/3)
out['fpareto_gb2_concave']['sd_of_exact_log_deltas'] = sde
print(json.dumps(out, indent=1, default=str))
json.dump(out, open('dev/research/evaluations/2026-11-15-equivariance-data/causes_logs.json', 'w'), indent=1, default=str)
