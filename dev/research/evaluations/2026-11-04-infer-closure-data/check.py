"""Independent closure-experiment checks; no production fits or optimizer estimates.

Run from the repository root through /tmp/umvl-cpu/run. Outputs are small JSON.
Finite grids give process diagnostics, never maximizers. PPP truncation error
is recorded separately; the finite-sample witness uses every observation.
"""
import json
import math
import platform
from pathlib import Path

import mpmath as mp
import numpy as np
import scipy
from scipy import integrate, special, stats
import sympy as sy

HERE = Path(__file__).resolve().parent
SEED = 1021220261104
rng = np.random.default_rng(SEED)
out = {"seed": SEED, "versions": {"python": platform.python_version(),
       "numpy": np.__version__, "scipy": scipy.__version__,
       "sympy": sy.__version__, "mpmath": mp.__version__}}


def bb_log_ratio(mu, u, N, xmax):
    k = 1 / N
    j = np.arange(N, dtype=float)
    zero = np.log1p(-mu * k / (1 + mu * u * k * j)).sum() + mu
    x = np.arange(xmax, dtype=float)
    increments = (np.log1p(-x*k) + np.log1p(x*u)
                  - np.log1p(mu*(u-k)-mu*(x+1)*u*k))
    return zero + np.r_[0., np.cumsum(increments)]


def chars(x, mu):
    c2 = x*(x-1)-2*mu*x+mu**2
    c3 = x*(x-1)*(x-2)-3*mu*x*(x-1)+3*mu**2*x-mu**3
    return c2, c3


# Exact symbolic Poisson expectations, using factorial moments.
x, m = sy.symbols("x m", positive=True)
c2, c3 = chars(x, m)
def poisson_expect(poly):
    poly = sy.Poly(sy.expand(poly), x)
    return sy.simplify(sum(coef * sum(sy.functions.combinatorial.numbers.stirling(
        power[0], j, kind=2)*m**j for j in range(power[0]+1))
        for power, coef in poly.terms()))
assert poisson_expect(c2) == poisson_expect(c3) == 0
assert poisson_expect(c2*c3) == 0
assert poisson_expect(c2**2) == 2*m**2
assert poisson_expect(c3**2) == 6*m**3
out["symbolic"] = {"C2_norm2": str(poisson_expect(c2**2)),
                   "C3_norm2": str(poisson_expect(c3**2)), "cross": "0"}

# Burr/Weibull efficient information, c=lambda=1; independent quadrature.
def ws(z):
    return np.array([z*z/2-z, z-1, 1+(1-z)*math.log(z)])
W = np.array([[integrate.quad(lambda z: ws(z)[i]*ws(z)[j]*math.exp(-z),
               0, np.inf, epsabs=2e-10)[0] for j in range(3)] for i in range(3)])
ieff = W[0,0]-W[0,1:] @ np.linalg.solve(W[1:,1:], W[1:,0])
assert ieff > 0
out["weibull_information"] = {"matrix": W.tolist(), "efficient": ieff}

# Fixed-N BB/binomial score and nuisance orthogonality, including N=1 exception.
out["binomial_seam"] = []
for N in (1, 2, 8):
    p = .3
    xx = np.arange(N+1)
    pmf = stats.binom.pmf(xx, N, p)
    d = xx-N*p
    sr = (d*d-(1-2*p)*d-N*p*(1-p))/(2*p*(1-p))
    sp = d/(p*(1-p))
    err = max(abs(pmf@sr), abs(pmf@(sr*sp)), abs(pmf@(sr*sr)-N*(N-1)/2))
    assert err < 2e-12
    out["binomial_seam"].append({"N": N, "information": float(pmf@(sr*sr)), "error": err})

# Fixed-NB transverse score: direct exact BB probabilities vs analytic derivative.
out["nb_seam"] = []
mu, a = 3., 2.
xx = np.arange(81)
pnb = stats.nbinom.pmf(xx, a, a/(a+mu))
sk = (-a*xx*(xx-1)+2*mu*(a+1)*xx-(a+1)*mu**2)/(2*(a+mu))
sm = a*(xx-mu)/(mu*(a+mu))
sa = special.digamma(a+xx)-special.digamma(a)+math.log(a/(a+mu))+(mu-xx)/(a+mu)
S = np.array([sk, sm, sa])
I = (S*pnb) @ S.T
out["nb_information"] = {"matrix": I.tolist(), "efficient": float(
    I[0,0]-I[0,1:] @ np.linalg.solve(I[1:,1:], I[1:,0]))}
assert out["nb_information"]["efficient"] > .01
for N in (1000, 10000, 100000):
    logbb = bb_log_ratio(mu, 1/a, N, 80)+stats.poisson.logpmf(xx, mu)
    slope = N*(logbb-stats.nbinom.logpmf(xx, a, a/(a+mu)))
    err = float(np.sqrt(pnb @ (slope-sk)**2))
    out["nb_seam"].append({"N": N, "weighted_score_error": err})
assert out["nb_seam"][-1]["weighted_score_error"] < .003

# Poisson-corner LAN at fixed local A,B and calibration of its score surrogate.
mu, reps = 3., 10000
xx = np.arange(61)
prob = stats.poisson.pmf(xx, mu)
assert stats.poisson.sf(60, mu) < 1e-50
prob /= prob.sum()
C2, C3 = chars(xx, mu)
from scipy.optimize import brentq
q95 = brentq(lambda t: .5*stats.chi2.cdf(t, 1)+.5*stats.chi2.cdf(t, 2)-.95, 1., 10.)
out["corner_critical_95"] = q95
out["corner"] = []
for n in (1000, 16000, 256000, 4096000):
    hist = rng.multinomial(n, prob, size=reps)
    A, B = .4, .6
    kideal = math.sqrt(B/(math.sqrt(n)*math.sqrt(2*mu**3/3)))
    N = round(1/kideal)
    k = 1/N
    u = k+(mu+1)*k*k+A*math.sqrt(2)/(mu*math.sqrt(n))
    Bactual = math.sqrt(n)*math.sqrt(2*mu**3/3)*k*k
    Z2 = hist@C2/math.sqrt(2*n*mu**2)
    Z3 = -hist@C3/math.sqrt(6*n*mu**3)
    ll = hist @ bb_log_ratio(mu, u, N, 60) if N > 60 else None
    # For small N the null sample exceeds support with negligible but nonzero
    # probability. Compute only bins through N, recording any impossible sample.
    if ll is None:
        lograt = bb_log_ratio(mu, u, N, N)
        good = hist[:, N+1:].sum(axis=1) == 0
        ll = hist[:, :N+1]@lograt
    else:
        good = np.ones(reps, dtype=bool)
    approximation = A*Z2+Bactual*Z3-.5*(A*A+Bactual**2)
    err = ll[good]-approximation[good]
    mh = (hist@xx)/n
    f2 = hist@(xx*(xx-1))/n
    f3 = hist@(xx*(xx-1)*(xx-2))/n
    z2fit = math.sqrt(n)*(f2-mh**2)/(math.sqrt(2)*mh)
    z3fit = -math.sqrt(n)*(f3-3*mh*f2+2*mh**3)/np.sqrt(6*mh**3)
    T = z2fit**2+np.maximum(z3fit, 0)**2
    out["corner"].append({"n": n, "N": N, "u": u, "A": A, "B_actual": Bactual,
       "support_failures": int((~good).sum()), "LAN_error_rms": float(np.sqrt(np.mean(err**2))),
       "LAN_error_mean": float(err.mean()), "score_reject_95": float(np.mean(T>q95)),
       "chi1_wrong_reject": float(np.mean(T>stats.chi2.ppf(.95, 1))),
       "z3_positive": float(np.mean(z3fit>0))})
assert out["corner"][-1]["LAN_error_rms"] < out["corner"][0]["LAN_error_rms"]
assert abs(out["corner"][-1]["score_reject_95"]-.05) < .012

# Uniformity illustration in the exact Gaussian limit experiment, separate
# from finite-sample likelihood calibration and with no optimized estimates.
out["gaussian_confidence"] = []
normal_error = np.random.default_rng(SEED+1).normal(size=200000)
for h in (0., .5, 2., 8.):
    Y = h+normal_error
    W = normal_error**2-np.minimum(Y,0)**2
    out["gaussian_confidence"].append({"h": h, "reps": len(Y),
        "coverage_boundary_cutoff": float(np.mean(W<=stats.chi2.ppf(.9,1))),
        "coverage_chi1_cutoff": float(np.mean(W<=stats.chi2.ppf(.95,1)))})

# Exponential/Lomax efficient-score calibration; no optimization.
out["lomax"] = []
for n in (500, 8000):
    values = []
    for start in range(0, 6000, 100):
        sample = rng.exponential(size=(100, n))
        z = sample/sample.mean(axis=1)[:,None]
        score = (z*z/2-2*z+1).sum(axis=1)/math.sqrt(n)
        values.extend(score.tolist())
    values = np.array(values)
    out["lomax"].append({"n": n, "reps": len(values), "positive": float(np.mean(values>0)),
                          "reject_95": float(np.mean(values>stats.norm.ppf(.95)))})

# Burr boundary layer: exact identities, Hellinger constant and PPP diagnostics.
# For w=k/alpha, b=0, H^2/k -> J=integral(sqrt(logistic(z))-1[z>=0])^2 dz.
Jleft = integrate.quad(lambda z: special.expit(z), -np.inf, 0)[0]
Jright = integrate.quad(lambda z: (math.sqrt(special.expit(z))-1)**2, 0, np.inf)[0]
out["pareto_hellinger_constant"] = Jleft+Jright
out["pareto_hellinger_minimum"] = {"a": math.log(3), "J": math.log(27/16)}
amin = math.log(3)
jmin_quad = (integrate.quad(lambda z: special.expit(z-amin), -np.inf, 0)[0]
             +integrate.quad(lambda z: (math.sqrt(special.expit(z-amin))-1)**2, 0, np.inf)[0])
assert abs(jmin_quad-math.log(27/16)) < 1e-11
out["pareto_hellinger_minimum"]["quadrature"] = jmin_quad
assert abs(Jleft-math.log(2)) < 1e-12
mp.mp.dps = 55
hchecks = []
for kval in ('0.01', '0.001', '0.0001'):
    k = mp.mpf(kval)
    # Integrate the square-root affinity on y>=0, in z=y/k;
    # exact leakage supplies the negative half-line contribution.
    fun = lambda z: k*(mp.sqrt(mp.exp(-k*z)*(1+mp.exp(-z))**(-k-1))
                        -mp.exp(-k*z/2))**2
    h2 = 1-mp.power(2,-k)+mp.quad(fun, [0,1,5,20,100,mp.inf])
    hchecks.append({"k": float(k), "H2_over_k": float(h2/k)})
out["pareto_hellinger"] = hchecks
assert abs(hchecks[-1]["H2_over_k"]-out["pareto_hellinger_constant"]) < .001

out["pareto_finite_process"] = []
for n in (500, 8000, 128000):
    # Generate only first 80 order statistics of n exponentials via spacings.
    # Every uncomputed term has a deterministic bound using the 80th point.
    Z = np.cumsum(rng.exponential(size=(8000,80)) *
                  (n/(n-np.arange(80)))[None,:], axis=1)
    t, v = 1., 2.
    F = np.logaddexp(0, (t-Z)/v).sum(axis=1)
    tailbound = (n-80)*np.exp((t-Z[:,-1])/v)
    exact = t-(1+v/n)*F
    limit_form = t-F
    out["pareto_finite_process"].append({"n": n,
       "mean_ll": float(exact.mean()), "mean_process_correction": float(np.mean(limit_form-exact)),
       "max_uncomputed_loglik_bound": float((1+v/n)*tailbound.max())})

# A shared PPP draw for expectations and finite-grid LR diagnostics. Points
# after cutoff R have an analytically bounded mean omitted contribution.
reps, R = 20000, 200.
process_ll, lr_lower, grid_tail = [], [], []
ts = np.arange(-4., 17., 1.)
vs = np.array([.5, 1., 2., 4., 8.])
for unused in range(reps):
    z = np.sort(rng.uniform(0, R, size=rng.poisson(R)))
    D = z-z[0]
    process_ll.append(1.-np.logaddexp(0, (1.-z)/2.).sum())
    vals = ts[:,None]-np.logaddexp(0, (ts[:,None,None]-D[None,None,:])/
                                  vs[None,:,None]).sum(axis=2)
    # grid value minus expected-tail padding is NOT a pathwise bound.
    # Instead append 1e-8 error control in probability via Markov: E tail <=
    # vmax exp((tmax-(R-zmin))/vmax); store its mean for explicit reporting.
    lr_lower.append(2*max(0., float(vals.max())))
    grid_tail.append(8.*math.exp((16.-(R-z[0]))/8.))
pll = np.array(process_ll)
glr = np.array(lr_lower)
out["pareto_ppp"] = {"reps": reps, "cutoff": R, "fixed_t": 1., "fixed_v": 2.,
    "mean_ll": float(pll.mean()), "mean_L": float(np.exp(pll).mean()),
    "expected_L": math.exp(-2*math.log1p(math.exp(-.5))),
    "grid_t": ts.tolist(), "grid_v": vs.tolist(),
    "grid_positive_fraction_at_1e_6": float(np.mean(glr>1e-6)),
    "grid_reject_at_chibar_95": float(np.mean(glr>stats.chi2.ppf(.9,1))),
    "grid_LR_quantiles": np.quantile(glr,[.5,.9,.95,.99]).tolist(),
    "mean_expected_tail_bound": float(np.mean(grid_tail)),
    "interpretation": "finite-grid diagnostic with truncation error in probability; not a fitted or certified global LR"}
assert abs(out["pareto_ppp"]["mean_L"]-out["pareto_ppp"]["expected_L"]) < .015

# Corrected finite-grid lower bound with deterministic tail for finite samples:
# verify actual finite Burr beats its best Pareto on a retained seeded sample.
# Only feasible-point comparisons, no maximizer returned.
found = None
for attempt in range(1000):
    n = 1000
    y = rng.exponential(size=n)
    b0 = y.min()
    ahat = n/(y-b0).sum()
    z = n*ahat*(y-b0)
    for t in (2.,4.,8.,12.):
        for v in (2.,4.,8.):
            gap = t-(1+v/n)*np.logaddexp(0, (t-z)/v).sum()
            if gap > .02:
                found = {"attempt": attempt, "n": n, "t": t, "v": v,
                         "alpha": ahat, "b": b0+t/(n*ahat), "k": v/n,
                         "loglik_gap_over_best_pareto": float(gap)}
                np.savetxt(HERE/'pareto-witness.csv', y, delimiter=',', header='log_x', comments='')
                break
        if found:
            break
    if found:
        break
assert found is not None
# Independent high precision original-density evaluation of the witness gap.
y = [mp.mpf(s) for s in (HERE/'pareto-witness.csv').read_text().splitlines()[1:]]
n = len(y)
b0 = min(y)
alpha = mp.mpf(n)/sum(q-b0 for q in y)
t, v = mp.mpf(found['t']), mp.mpf(found['v'])
b = b0+t/(n*alpha)
k = v/n
gap = sum(alpha*(b-b0)-(1+k)*mp.log1p(mp.exp(-alpha*(q-b)/k)) for q in y)
assert gap > mp.mpf('.02')
found['high_precision_gap'] = str(gap)
out['pareto_witness'] = found

# First local experiments can agree while exact face selection differs:
# evaluate the analytically proposed sub-detectability point, without search.
out['leakage_counterexample'] = []
for n, zn in ((100,10), (10000,100), (1000000,1000)):
    t = (3*zn/(4*n))**2
    ll = n*math.log1p(-t*t)+(n+zn)/2*math.log1p(t**1.5)+(n-zn)/2*math.log1p(-t**1.5)
    predicted = 27*zn**4/(256*n**3)
    assert ll > 0
    out['leakage_counterexample'].append({'n': n, 'Zn': zn, 't': t,
            'feasible_gain': ll, 'leading_gain': predicted, 'ratio': ll/predicted})

(HERE/'results.json').write_text(json.dumps(out, indent=2)+'\n')
print(json.dumps({"checks": "passed", "results": str(HERE/'results.json'),
                  "witness_gap": str(gap)}, indent=2))
