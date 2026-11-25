"""Explore the proved scalar all-free profile on seeded dyadic histograms."""
import numpy as np

rng = np.random.default_rng(20261125)
shape = 2.0 ** np.linspace(-8, 6, 113)
shape = shape[:, None]


def values(y, p):
    r = np.exp(shape*y)
    dr = np.expm1(shape*y)
    average_r = r @ p
    average_dr = dr @ p

    def terms(logt):
        t = np.exp(logt)
        delta = t*dr
        z = t*r
        q = -np.expm1(-z)
        u = np.exp(-z)
        h = np.ones_like(z)
        left = z < 1
        middle = (z >= 1) & (z < 36)
        h[left] = -np.log(q[left])/u[left]
        h[middle] = -np.log1p(-u[middle])/u[middle]
        w = np.exp(-delta)
        S = (w*h) @ p
        Wh = (w*r/q) @ p / S
        return t, S, Wh

    def score(logt):
        t, S, Wh = terms(logt)
        return (1/t)[:, 0] - average_r + Wh*(1-np.exp(-t[:, 0])*S)

    lo, hi = -shape-18, np.full_like(shape, np.log(1e6))
    assert np.all(score(lo) > 0) and np.all(score(hi) < 0)
    for _ in range(48):
        mid = (lo+hi)/2
        positive = (score(mid) > 0)[:, None]
        lo, hi = np.where(positive, mid, lo), np.where(positive, hi, mid)
    logt = (lo+hi)/2
    t, S, _ = terms(logt)
    return (np.log(shape[:, 0])+logt[:, 0]+shape[:, 0]*(y @ p)
            - t[:, 0]*average_dr + np.exp(-t[:, 0])*S - np.log(S) - 1)


def main():
    for trial in range(1600):
        levels = np.unique(rng.integers(-50, 51, rng.integers(2, 11)))
        if len(levels) < 3:
            continue
        counts = np.maximum(1, np.rint(2.0**rng.uniform(0, 8, len(levels)))).astype(int)
        y = (levels-levels[0])/(levels[-1]-levels[0])
        ll = values(y, counts/sum(counts))
        hits = np.flatnonzero((ll[1:-1] > ll[:-2]+1e-8) & (ll[1:-1] > ll[2:]+1e-8))+1
        if len(hits) >= 2:
            print("dyadic candidate", trial, levels.tolist(), counts.tolist(),
                  list(zip(shape[hits, 0], ll[hits])))
            break
    else:
        print("no two grid peaks in 1600 seeded draws")


if __name__ == "__main__":
    main()
