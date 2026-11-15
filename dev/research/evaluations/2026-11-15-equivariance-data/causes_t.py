"""lst, std: emulate t_cert_detail::data (lst/certificate.hpp lines 126-148) in binary64
on the near-constant and tie3 samples, with the same interval allowances."""
import sys, math, json
sys.path.insert(0, 'dev/validation')
import strict_common as sc
D = 'dev/research/evaluations/2026-11-15-fit-snapshot-data/'
EPS = 64*2.220446049250313e-16; DM = 5e-324
rows = sc.load(D + 'native.jsonl.gz')
class I:
    def __init__(s, lo, hi=None): s.lo = lo; s.hi = lo if hi is None else hi
    def mag(s): return max(abs(s.lo), abs(s.hi))
def pad(l, h):
    if not (math.isfinite(l) and math.isfinite(h)) or l > h: return I(math.nan, math.nan)
    e = EPS*max(abs(l), abs(h))+DM; return I(l-e, h+e)
def add(a, b): return pad(a.lo+b.lo, a.hi+b.hi)
def neg(a): return I(-a.hi, -a.lo)
def sub(a, b): return add(a, neg(b))
def sq(a): return pad(0 if a.lo <= 0 <= a.hi else min(a.lo*a.lo, a.hi*a.hi), max(a.lo*a.lo, a.hi*a.hi))
class Sum:
    def __init__(s): s.l = [0.0]*64; s.u = [0.0]*64; s.mag = 0.0; s.n = 0
    def add(s, v):
        lo, hi, count, level = v.lo, v.hi, s.n, 0
        while count & 1: lo += s.l[level]; hi += s.u[level]; count >>= 1; level += 1
        s.l[level] = lo; s.u[level] = hi; s.mag += v.mag(); s.n += 1
    def get(s):
        lo = hi = 0.0; count, level = s.n, 0
        while count:
            if count & 1: lo += s.l[level]; hi += s.u[level]
            count >>= 1; level += 1
        e = EPS*(2*level+2)*(1+s.mag*(1+2*EPS*s.n)); return I(lo-e, hi+e)
def data(x):
    o = sorted(x); n = len(x)
    base = o[n//2]; rng = max(abs(o[-1]/2-base/2), abs(o[0]/2-base/2))*2
    m = 0.0
    for v in o: m += ((v-base)/rng)/n
    center = base+rng*m; ss = 0.0
    for v in o: z = (v-center)/rng; ss += z*z/n
    unit = rng*math.sqrt(ss)
    xs = []; allowances = []; vals = []
    s = Sum()
    for v in o:
        value = (v-center)/unit
        err = EPS*(1+abs(v/unit)+abs(center/unit)+abs(value))
        xs.append(I(value-err, value+err)); allowances.append(err); vals.append(value); s.add(xs[-1])
    mean = s.get(); mean = I(mean.lo/n, mean.hi/n); mean = I(mean.lo, mean.hi)
    v = Sum()
    for t in xs: v.add(sq(sub(t, mean)))
    var = v.get(); var = I(var.lo/n, var.hi/n)
    return dict(range=rng, center=center, unit=unit, values=vals, allowance_per_value=allowances[0],
                allowance_if_exact_subtraction=EPS*(1+2*abs(vals[0])), var_lo=var.lo, var_hi=var.hi, data_ok=var.lo > 0)
out = {}
for f in ('lst', 'std'):
    for c in ('near-constant', 'tie3'):
        out[f+':'+c] = data(rows[(f, 'class', c)]['data'])
        print(f, c, json.dumps(out[f+':'+c]))
json.dump(out, open('dev/research/evaluations/2026-11-15-equivariance-data/causes_t.json', 'w'), indent=1)
