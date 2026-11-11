"""Margins of finite beta-binomial fits over the best face at the Poisson corner.

Diagnostic for bb_poisson_corner.py: records gap = (best finite BB log-lik)
- (best face log-lik) for every sample. Small positive gaps near the 1e-7
classification tolerance would indicate numerical false positives.
Run: python3 bb_corner_margins.py <n> <R>
"""
import sys
import numpy as np
import bb_poisson_corner as m


def gap_sample(n, mu=3.0):
    x = m.rng.poisson(mu, n)
    xs, cs = np.unique(x, return_counts=True)
    cs = cs.astype(float)
    mean, v = x.mean(), x.var()
    pois, nbv, bnv = m.faces(xs, cs, n, mean, v)
    face = max(pois, nbv, bnv)
    M = xs.max()
    Ns = np.unique(np.concatenate([np.arange(max(M, 1), 120), np.geomspace(120, m.NCAP, 80).astype(int)]))
    prof = [(m.bb_profile_N(N, xs, cs, mean), N) for N in Ns]
    prof.sort(key=lambda z: -z[0][0])
    best, bestN = -np.inf, None
    for (val, logs), N in prof[:3]:
        if logs >= 24.9:
            continue
        pv, pl = m.bb_polish(N, xs, cs, mean, logs)
        if pl < 24.9 and pv > best:
            best, bestN = pv, N
    return best - face, bestN, v - mean


if __name__ == "__main__":
    n = int(float(sys.argv[1]))
    R = int(sys.argv[2])
    m.rng = np.random.default_rng([20261111, n])
    res = np.array([gap_sample(n) for _ in range(R)], dtype=object)
    gaps = res[:, 0].astype(float)
    Ns = res[:, 1]
    fin = gaps > 1e-7
    g = gaps[fin]
    print("n=%d R=%d finite (gap>1e-7): %.3f" % (n, R, fin.mean()))
    for t in [1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1.0]:
        print("  finite with gap < %g: %d of %d" % (t, (g < t).sum(), g.size))
    print("  gap quantiles among finite (10,25,50,75,90%%): %s" % np.round(np.quantile(g, [.1, .25, .5, .75, .9]), 5))
    near0 = np.abs(gaps) < 1e-4
    print("  samples with |gap| < 1e-4 (either sign): %d" % near0.sum())
    bigN = [N for N, f in zip(Ns, fin) if f and N is not None and N > 5000]
    print("  finite fits with N > 5000: %d" % len(bigN))
    from scipy.stats import chi2
    f3 = (gaps > 1e-3).mean()
    keep = 1 - chi2.cdf(2e-3, 1)  # share of genuine finite fits with gap > 1e-3 under 1/2 chi2_1
    print("  finite with gap > 1e-3: %.3f; corrected for the threshold: %.3f (se %.3f)"
          % (f3, f3 / keep, np.sqrt(f3 * (1 - f3) / R) / keep))
    g3 = gaps[gaps > 1e-3]
    q = [.1, .25, .5, .75, .9]
    ref = chi2.ppf(chi2.cdf(2e-3, 1) + (1 - chi2.cdf(2e-3, 1)) * np.array(q), 1) / 2
    print("  gap quantiles (gap > 1e-3): %s; 1/2 chi2_1 conditional reference: %s"
          % (np.round(np.quantile(g3, q), 4), np.round(ref, 4)))
    np.save("/tmp/bbgaps_%d.npy" % n, gaps)
