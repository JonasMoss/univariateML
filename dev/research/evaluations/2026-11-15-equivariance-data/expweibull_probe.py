"""expweibull on tie3 = [q50, q50, q75]: where is the supremum of the likelihood?

Profiles the CDF exponent c exactly (c = -n / sum log(1 - exp(-t)), t = (x/a)^b),
then scans (log a, log b) on a grid in mpmath-free float64 (spread is O(1) here),
and compares with the inverse Weibull face (c -> 0, a -> ..., from invweibull).
"""
import numpy as np, sys
x = np.array([2.698972628080099, 2.698972628080099, 3.487363739010494])
n = len(x)
def prof(la, lb):
    a, b = np.exp(la), np.exp(lb)
    t = (x/a)**b
    h = np.log(-np.expm1(-t))
    c = -n/np.sum(h)
    if not (c > 0 and np.isfinite(c)): return -np.inf, c
    ll = n*(lb + np.log(c)) - n*la + np.sum((b-1)*np.log(x/a) - t + (c-1)*h)
    return ll - np.sum(np.log(x)) + np.sum(np.log(x)), c  # density is in x; no Jacobian beyond formula
best = (-np.inf, None)
res = []
for lb in np.linspace(-3, 12, 1501):
    row = []
    for la in np.linspace(np.log(x.min())-3, np.log(x.max())+3, 801):
        v, c = prof(la, lb)
        row.append(v)
        if v > best[0]: best = (v, (la, lb, c))
    res.append(max(row))
res = np.array(res); lbs = np.linspace(-3, 12, 1501)
print('grid best', best[0], 'at log a, log b, c =', best[1], ' a, b =', np.exp(best[1][0]), np.exp(best[1][1]))
for lb in (0, 2, 4, 6, 8, 10, 12):
    i = np.argmin(abs(lbs-lb)); print('log b = %g: max over log a = %.6f' % (lb, res[i]))

# ---- inverse Weibull face: maximise sum log f_IW(x; shape, scale) over (shape, scale) ----
def ll_iw(ls, lk):
    s, k = np.exp(ls), np.exp(lk)
    return np.sum(lk - 0 + k*ls - (k+1)*np.log(x) - (x/s)**(-k)) 
bi = (-np.inf, None)
for lk in np.linspace(-2, 8, 2001):
    for ls in np.linspace(0, 3, 1201):
        v = ll_iw(ls, lk)
        if v > bi[0]: bi = (v, (np.exp(ls), np.exp(lk)))
print('inverse Weibull face: max ll', bi[0], 'at scale, shape =', bi[1])
# ---- push the finite family towards the face: a small, c large ----
best2 = (-np.inf, None)
for la in np.linspace(np.log(x.min())-14, np.log(x.min())-1, 2600):
    for lb in np.linspace(-1, 6, 700):
        v, c = prof(la, lb)
        if v > best2[0]: best2 = (v, (np.exp(la), np.exp(lb), c))
print('finite family, a down to e^-14 x_min: best ll', best2[0], 'at (a, b, c) =', best2[1])
