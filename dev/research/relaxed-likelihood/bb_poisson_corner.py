"""Beta-binomial (size N, shapes a, b all free) fitted to Poisson data.

Outcome per sample: finite BB MLE, NB face, free-size binomial face, or Poisson.
Prediction (relaxed-likelihood.tex, Poisson corner): finite 1/2, NB 1/4,
binomial 1/4 asymptotically; finite fits have N of order n^(1/4).
Uses the counts table, so cost does not grow with n.
Log pmf, cancellation-free:
  log P(x) = log C(N,x) + sum_{j<x} log(a+j) - sum_{N-x<=j<N} log(b+j)
             - sum_{j<N} log1p(a/(b+j)).
Run: OPENBLAS_NUM_THREADS=1 nice -n 10 python3 bb_poisson_corner.py
"""
import numpy as np
from scipy.optimize import minimize, minimize_scalar
from scipy.special import gammaln

rng = np.random.default_rng(20261110)
NCAP = 20000


def bb_ll(N, a, b, xs, cs):
    a = np.atleast_1d(a)[:, None]
    b = np.atleast_1d(b)[:, None]
    j = np.arange(N)[None, :]
    base = -np.log1p(a / (b + j)).sum(1)  # log P(0) part, per (a,b)
    out = np.zeros(a.shape[0])
    for x, c in zip(xs, cs):
        lc = gammaln(N + 1) - gammaln(x + 1) - gammaln(N - x + 1)
        up = np.log(a + np.arange(x)[None, :]).sum(1) if x > 0 else 0.0
        dn = np.log(b + np.arange(N - x, N)[None, :]).sum(1) if x > 0 else 0.0
        out += c * (lc + up - dn)
    return out + cs.sum() * base


def ab_from(N, p, logs):
    s = np.exp(logs)
    return p * s, (1 - p) * s


def bb_profile_N(N, xs, cs, mu):
    """max over s with mean fixed at mu (p = mu/N); returns (value, logs)."""
    p = mu / N
    grid = np.linspace(-4, 25, 120)
    a, b = ab_from(N, p, grid)
    v = bb_ll(N, a, b, xs, cs)
    i = int(np.argmax(v))
    lo, hi = grid[max(i - 1, 0)], grid[min(i + 1, len(grid) - 1)]
    r = minimize_scalar(lambda t: -bb_ll(N, *ab_from(N, p, t), xs, cs)[0], bounds=(lo, hi), method="bounded",
                        options={"xatol": 1e-10})
    return (-r.fun, r.x) if -r.fun > v[i] else (v[i], grid[i])


def bb_polish(N, xs, cs, mu, logs0):
    def f(t):
        p = 1 / (1 + np.exp(-t[0]))
        if t[1] > 25:
            return np.inf
        a, b = ab_from(N, p, t[1])
        return -bb_ll(N, a, b, xs, cs)[0]
    p0 = mu / N
    r = minimize(f, [np.log(p0 / (1 - p0)), logs0], method="Nelder-Mead", options={"xatol": 1e-10, "fatol": 1e-12})
    return -r.fun, r.x[1]


def faces(xs, cs, n, mu, v):
    lfact = gammaln(xs + 1)
    pois = (cs * (xs * np.log(mu) - mu - lfact)).sum()

    def nb(lr):
        r = np.exp(lr)
        up = np.array([np.log(r + np.arange(x)).sum() if x > 0 else 0.0 for x in xs])
        return (cs * (up - lfact + r * np.log1p(-mu / (r + mu)) + xs * np.log(mu / (r + mu)))).sum()
    nbv = pois
    if v > mu:
        rr = minimize_scalar(lambda t: -nb(t), bounds=(-6, 30), method="bounded", options={"xatol": 1e-10})
        nbv = max(pois, -rr.fun)

    def binom(Ns):
        Ns = np.asarray(Ns, dtype=float)
        p = mu / Ns
        tot = np.zeros(Ns.shape)
        for x, c in zip(xs, cs):
            lc = np.log(Ns[:, None] - np.arange(x)[None, :]).sum(1) if x > 0 else 0.0
            tot += c * (lc - gammaln(x + 1) + x * np.log(p) + (Ns - x) * np.log1p(-p))
        return tot
    bnv = pois  # free-size binomial beats Poisson only when v < mu (binomial seam)
    if v < mu:
        M = int(xs.max())
        Ns = np.unique(np.concatenate([np.arange(M, 200), np.geomspace(200, 1e7, 300).astype(np.int64)]))
        vals = binom(Ns)
        i = int(np.argmax(vals))
        lo, hi = int(Ns[max(i - 1, 0)]), int(Ns[min(i + 1, len(Ns) - 1)])
        bnv = max(pois, vals[i], binom(np.arange(max(lo, M), hi + 1)).max())
    return pois, nbv, bnv


def classify(n, mu=3.0, tol=1e-7):
    x = globals()['rng'].poisson(mu, n)
    xs, cs = np.unique(x, return_counts=True)
    cs = cs.astype(float)
    m = x.mean()
    v = x.var()
    pois, nbv, bnv = faces(xs, cs, n, m, v)
    face = max(pois, nbv, bnv)
    M = xs.max()
    Ns = np.unique(np.concatenate([np.arange(max(M, 1), 120), np.geomspace(120, NCAP, 80).astype(int)]))
    prof = [(bb_profile_N(N, xs, cs, m), N) for N in Ns]
    prof.sort(key=lambda z: -z[0][0])
    best, bestN = -np.inf, None
    for (val, logs), N in prof[:3]:
        if logs >= 24.9:
            continue  # binomial face, not a finite BB
        pv, pl = bb_polish(N, xs, cs, m, logs)
        if pl < 24.9 and pv > best:
            best, bestN = pv, N
    if best > face + tol:
        return "finite", bestN
    if face == pois or max(nbv, bnv) - pois <= tol:
        return "poisson", None
    return ("nb" if nbv >= bnv else "binom"), None


if __name__ == "__main__":
    import sys
    n = int(float(sys.argv[1]))
    R = int(sys.argv[2]) if len(sys.argv) > 2 else 300
    rng = np.random.default_rng([20261110, n])
    out = [classify(n) for _ in range(R)]
    kinds = [o[0] for o in out]
    Nf = np.array([o[1] for o in out if o[0] == "finite"])
    frac = {k: kinds.count(k) / R for k in ["finite", "nb", "binom", "poisson"]}
    med = np.median(Nf) if Nf.size else float("nan")
    print("n=%8d R=%d  finite %.3f  nb %.3f  binom %.3f  poisson %.3f   median N of finite fits %.0f  (n^(1/4)=%.1f), max N %s"
          % (n, R, frac["finite"], frac["nb"], frac["binom"], frac["poisson"], med, n ** 0.25,
             Nf.max() if Nf.size else "-"), flush=True)
