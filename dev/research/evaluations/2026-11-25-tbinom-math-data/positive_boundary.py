"""Numerical screening at the zero-truncated Poisson second-moment boundary."""
import math
import numpy as np
from search import bisect_rate, scores


def lower_extreme(mu, lam):
    # Under weighting by (X-1), the remaining mean is lam*mu/(mu-1).
    # Two adjacent points minimize convex losses for that weighted law.
    kbar = lam*mu/(mu-1)
    k = max(2, math.floor(kbar))
    p = (mu-1)*(k+1-kbar)/(k-1)
    q = (mu-1)*(kbar-k)/k
    hist = np.zeros(k+1)
    hist[0], hist[k-1], hist[k] = 1-p-q, p, q
    return hist


def scan():
    best = (0, None)
    successes = 0
    for mu in np.r_[1+np.geomspace(.001,.2,150), np.linspace(1.2,14,1601)]:
        lam = bisect_rate(lambda z: -z/np.expm1(-z), mu, 1e-9, mu+1)
        hist = lower_extreme(mu, lam)
        k = np.arange(1,len(hist)+1)
        if min(hist) < 0:
            continue
        grid = np.arange(len(hist), len(hist)+10)
        table = scores(1, 10000, len(hist), mu, grid)
        ll = table @ hist
        Z = np.expm1(lam)
        face = mu*np.log(lam)-np.log(Z)
        gap = np.max(ll)-face
        if gap > best[0]:
            best = (gap, (mu,lam,hist,grid[np.argmax(ll)], ll-face))
        if gap > 1e-10 and successes < 3:
            print("boundary witness", mu, hist.tolist(), (ll-face).tolist(), flush=True)
            successes += 1
    print("best positive excess", best, flush=True)


if __name__ == "__main__":
    scan()


def high_precision_scan():
    import mpmath as mp

    mp.mp.dps = 70
    means = ['1.001', '1.01', '1.1', '1.5', '1.9', '2', '2.01',
             '2.5', '3.01', '4.5', '6.1', '10.01', '14']
    max_excess = -mp.inf
    cases = 0
    for mean in map(mp.mpf, means):
        rate = mp.findroot(lambda t: t/(-mp.expm1(-t))-mean,
                           (mean-1, mean))
        vertex = rate*mean/(mean-1)
        k = int(mp.floor(vertex))
        p = (mean-1)*(k+1-vertex)/(k-1)
        q = (mean-1)*(vertex-k)/k
        face = mean*mp.log(rate)-mp.log(mp.expm1(rate))
        for size in range(k+1, k+11):
            def conditional_mean(eta):
                t = mp.exp(eta)
                return size*t/((1+t)*(-mp.expm1(-size*mp.log1p(t))))
            left, right = mp.mpf(-40), mp.mpf(40)
            for _ in range(190):
                center = (left+right)/2
                if conditional_mean(center) < mean:
                    left = center
                else:
                    right = center
            odds = mp.exp((left+right)/2)
            log_normalizer = mp.log(mp.expm1(size*mp.log1p(odds)))
            c = [sum(mp.log1p(-mp.mpf(j)/size) for j in range(x))
                 for x in (k, k+1)]
            excess = p*c[0]+q*c[1]+mean*mp.log(size*odds)-log_normalizer-face
            max_excess = max(excess, max_excess)
            cases += 1
    assert max_excess < 0
    print("70-digit checks", cases, "max excess", mp.nstr(max_excess, 25))


if __name__ == "__main__":
    high_precision_scan()
