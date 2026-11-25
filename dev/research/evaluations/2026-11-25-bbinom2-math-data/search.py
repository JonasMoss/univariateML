"""Search stationary three-cell real histograms for positive profile curvature."""
import numpy as np
import mpmath as mp

rng = np.random.default_rng(274)
tested = 0
minimum = np.inf
for m in (3, 4, 5, 10, 20, 50, 100, 300, 1000):
    for _ in range(500):
        a, b = 10 ** rng.uniform(-3, 4, 2)
        j = np.arange(m)
        x, y, c = 1 / (a + j), 1 / (b + j), 1 / (a + b + j)
        A = np.r_[0, x.cumsum()]
        B = np.r_[0, y.cumsum()][::-1]
        U = np.r_[0, (x*x).cumsum()]
        V = np.r_[0, (y*y).cumsum()][::-1]
        cells = np.sort(rng.integers(0, m + 1, size=(300, 3)), axis=1)
        cells[:150, 0] = 0
        cells[150:, 2] = m
        cells[:75, 2] = m
        cells = cells[np.all(np.diff(cells, axis=1) > 0, axis=1)]
        L, M, R = cells.T
        u, v = A[L] - A[M], A[R] - A[M]
        p, q = B[L] - B[M], B[R] - B[M]
        det = u*q - v*p
        keep = det != 0
        L, M, R, u, v, p, q, det = (s[keep] for s in (L,M,R,u,v,p,q,det))
        da, db = c.sum() - A[M], c.sum() - B[M]
        wl, wr = (da*q - db*v)/det, (db*u - da*p)/det
        wm = 1 - wl - wr
        keep = np.minimum(np.minimum(wl, wr), wm) > 1e-9
        ul = wl*U[L] + wm*U[M] + wr*U[R]
        vl = wl*V[L] + wm*V[M] + wr*V[R]
        ratio = ul[keep]*vl[keep]/(ul[keep]+vl[keep])/(c@c)
        tested += ratio.size
        if ratio.size and ratio.min() < minimum:
            minimum = ratio.min()
            i = np.flatnonzero(keep)[ratio.argmin()]
            worst = (m, float(a), float(b), tuple(map(int, (L[i], M[i], R[i]))))
        if ratio.size and ratio.min() < 1 - 1e-8:
            print("candidate", worst, minimum, flush=True)
            break
    else:
        continue
    break

print("feasible triples", tested, "minimum ratio", minimum, "at", worst)
mp.mp.dps = 80
m, a_float, b_float, cells = worst
a, b = mp.mpf(a_float), mp.mpf(b_float)
def sums(shape, count, power=1):
    return mp.fsum((shape + j)**(-power) for j in range(count))
c = sums(a+b, m)
A = [sums(a, k) for k in cells]
B = [sums(b, m-k) for k in cells]
w = mp.lu_solve(mp.matrix([[1]*3, A, B]), mp.matrix([1,c,c]))
assert min(w) > 0
U = mp.fsum(wi*sums(a,k,2) for wi,k in zip(w,cells))
V = mp.fsum(wi*sums(b,m-k,2) for wi,k in zip(w,cells))
ratio = U*V/(U+V)/sums(a+b,m,2)
assert abs(ratio - minimum) < mp.mpf("1e-9")
print("80-digit replay ratio minus one", mp.nstr(ratio-1, 40))
print("weights", [mp.nstr(wi, 30) for wi in w])
