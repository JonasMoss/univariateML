"""emg (certificate coordinates) and sn (public-coordinate check) on the near-constant sample."""
import sys, math, json
sys.path.insert(0, 'dev/validation')
import strict_common as sc
import numpy as np
D = 'dev/research/evaluations/2026-11-15-fit-snapshot-data/'
EPS = 2.220446049250313e-16
rows = sc.load(D + 'native.jsonl.gz')
out = {}
# ---- emg: CertModel::initial(k=1) and cert_evaluate's z = b*v - u (emg/certificate_math.hpp 117-123, 187-205) ----
x = rows[('emg', 'class', 'near-constant')]['data']; n = 3
mean = sum(x)/n
sd = math.sqrt(sum((v-mean)**2 for v in x)/n)
k = 1.0
b = math.sqrt(1+1/(k*k))/sd
u = b*mean-1/k
z = [b*v-u for v in x]
zerr = 64*EPS*(abs(b*x[0])+abs(u))
out['emg'] = dict(c=x[0], sd=sd, b=b, u=u, ulp_of_u=math.nextafter(u, math.inf)-u,
                  b_times_spread=b*(x[2]-x[0]), z_values=z, z_error_allowance=zerr,
                  z_exact_spread=b*(x[2]-x[0]))
# ---- sn: lattice loss of finite candidates, standardised sample s = [0, 0, 1] (units of ulp(c)) ----
from math import erfc, log, sqrt, pi
def lphi(z): return -0.5*z*z-0.5*log(2*pi)
def lPhi(t):
    return log(0.5*erfc(-t/sqrt(2))) if t > -30 else lphi(t)-log(-t)+log1p_series(t)
def log1p_series(t): return math.log1p(-1/(t*t)+3/t**4)
def ll(s, xi, om, al):
    return sum(log(2)-log(om)+lphi((v-xi)/om)+lPhi(al*(v-xi)/om) for v in s)
s = [0.0, 0.0, 1.0]
def opt(al, xis=None):
    best = (-1e300, None)
    for xi in (np.linspace(-1.5, 1.2, 541) if xis is None else xis):
        for om in np.exp(np.linspace(-4, 1.5, 441)):
            v = ll(s, xi, om, al)
            if v > best[0]: best = (v, (xi, om))
    # refine
    xi0, om0 = best[1]
    step_x, step_o = 0.005, 0.01*om0
    for _ in range(60):
        improved = False
        for dx, do in ((step_x, 0), (-step_x, 0), (0, step_o), (0, -step_o)):
            xi1, om1 = (xi0+dx if xis is None else xi0), om0+do
            if om1 <= 0: continue
            v = ll(s, xi1, om1, al)
            if v > best[0]+1e-15: best = (v, (xi1, om1)); xi0, om0 = xi1, om1; improved = True
        if not improved: step_x /= 2; step_o /= 2
    return best
rows_sn = []
for al in (1, 3, 10, 30, 100):
    v, (xi, om) = opt(al)
    lat = min((opt(al, [float(f)]) for f in (math.floor(xi), math.ceil(xi))), key=lambda r: -r[0])
    rows_sn.append(dict(alpha=al, ll_continuous=v, xi_in_ulps=xi, omega_in_ulps=om, ll_best_lattice_xi=lat[0],
                        loss=v-lat[0]))
face = sum(0.5*log(2/pi)-log(om)-(v-0.0)**2/(2*om*om) for v in s for om in [math.sqrt(1/3)])  # hnorm at mu = 0 : sigma^2 = mean((x-mu)^2)
out['sn'] = dict(candidates=rows_sn, hnorm_face_ll=face)
for r in rows_sn: print(r)
print('hnorm face', face)
print(json.dumps(out['emg']))
json.dump(out, open('dev/research/evaluations/2026-11-15-equivariance-data/causes_loc.json', 'w'), indent=1)
