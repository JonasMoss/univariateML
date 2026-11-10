"""Escape of all-free Burr XII to its Pareto face when the truth is Pareto.

Log scale y = log x. Burr log density, exactly:
  log g = log a_B - k z - (1+k) sp(-z),  z = c (y - b),  a_B = k c,  sp = log1p(exp).
Best Pareto (endpoint at the sample minimum, upper trace): P = n log(n/D1) - n.
Local limit (Pareto truth, eta = n*alpha*y, kappa = c/(n*alpha), beta = n*alpha*b
relative to the minimum): with xi_j a unit-rate Poisson process on (0, inf),
  F(kappa, beta) = beta - sp(kappa*beta) - sum_j sp(-kappa*(xi_j - beta)),
the face value is 0, and a finite Burr MLE exists asymptotically iff sup F >= 0.
F < 0 for kappa >= 1 (equivalently k <= 1/n), matching the closure report's
k > m/(n-m) bound.

Part 1 simulates sup F; part 2 fits exact Burr likelihoods near the face for
finite n and compares with P (and checks P > W, the Weibull face).
Run: OPENBLAS_NUM_THREADS=1 nice -n 10 python3 -I burr_pareto_face.py
"""
import numpy as np
from scipy.optimize import minimize, minimize_scalar

rng = np.random.default_rng(20261110)
sp = np.logaddexp  # sp(u) = logaddexp(0, u)


def F_limit(kap, beta, xi):
    return beta - sp(0, kap * beta) - sp(0, -kap * (xi - beta)).sum()


def sup_F(xi):
    kaps = np.geomspace(0.1, 1.0, 16)
    betas = np.linspace(0.0, xi[0] + 6.0, 48)
    K, B = np.meshgrid(kaps, betas, indexing="ij")
    vals = (B - sp(0, K * B)) - sp(0, -K[..., None] * (xi - B[..., None])).sum(-1)
    i, j = np.unravel_index(np.argmax(vals), vals.shape)
    res = minimize(lambda t: -F_limit(np.exp(t[0]), t[1], xi), [np.log(K[i, j]), B[i, j]],
                   method="Nelder-Mead", options={"xatol": 1e-6, "fatol": 1e-9})
    return max(vals[i, j], -res.fun)


def part1(R=10000):
    sups = np.empty(R)
    gap1 = np.empty(R)
    for r in range(R):
        xi = np.cumsum(rng.exponential(size=500))  # points up to ~500; kappa >= 0.1
        sups[r] = sup_F(xi)
        gap1[r] = xi[0]
    p = (sups >= 0).mean()
    se = np.sqrt(p * (1 - p) / R)
    print("limit: P(finite Burr MLE) = %.4f (se %.4f); escape to Pareto = %.4f" % (p, se, 1 - p))
    for lo, hi in [(0, 1), (1, 2), (2, 3), (3, 4), (4, 6), (6, np.inf)]:
        sel = (gap1 >= lo) & (gap1 < hi)
        if sel.any():
            print("  first spacing in [%g,%g): share %.4f, P(finite MLE | spacing) = %.3f"
                  % (lo, hi, sel.mean(), (sups[sel] >= 0).mean()))
    return sups


def burr_profile(y, n, D1, ymin, alpha_hat):
    """max over (c, b) near the Pareto face of the exact Burr log-lik minus P."""
    P = n * np.log(n / D1) - n

    def ll(t):
        kap, beta = np.exp(t[0]), t[1]
        c = n * alpha_hat * kap
        b = ymin + beta / (n * alpha_hat)
        z = c * (y - b)
        s = sp(0, -z)
        aB = n / ((y - b).sum() + s.sum() / c)  # exact profile in a_B
        if not np.isfinite(aB) or aB <= 0:
            return -np.inf
        k = aB / c
        return n * np.log(aB) - aB * (y - b).sum() - (1 + k) * s.sum()

    best = (-np.inf, None)
    for kap in np.geomspace(0.1, 1.0, 12):
        for beta in np.linspace(0.0, 15.0, 31):
            v = ll([np.log(kap), beta])
            if v > best[0]:
                best = (v, [np.log(kap), beta])
    res = minimize(lambda t: -ll(t), best[1], method="Nelder-Mead", options={"xatol": 1e-7, "fatol": 1e-10})
    return max(best[0], -res.fun) - P, P


def weibull_face(y, n):
    D = (y - y.min()).sum()
    yy = y - y.min()

    def negW(lc):
        c = np.exp(lc)
        return -(n * np.log(c) + c * D - n * np.log(np.mean(np.exp(c * yy))) - n)

    r = minimize_scalar(negW, bounds=(-8, 4), method="bounded")
    return -r.fun


def part2(ns=(50, 200, 1000), R=2000, alpha=2.0):
    for n in ns:
        wins = 0
        p_below_w = 0
        for r in range(R):
            y = rng.exponential(size=n) / alpha  # log of Pareto(1, alpha)
            ymin = y.min()
            D1 = (y - ymin).sum()
            ahat = n / D1
            d, P = burr_profile(y, n, D1, ymin, ahat)
            W = weibull_face(y, n)  # same constant: log-scale, minimum-shifted
            Pc = n * np.log(n / D1) - n
            if W > Pc:
                p_below_w += 1
            wins += d >= 0
        print("n=%5d: P(finite Burr beats Pareto near the face) = %.4f; P(W > P) = %.4f" % (n, wins / R, p_below_w / R))


if __name__ == "__main__":
    part1()
    part2()
