"""Lomax / pareto2 exponential-limit margin: samples whose MLE exists with a per-observation gain
G = sup_q h(q) in (0, 1e-8], so the core's exponential stop (certificate.hpp: gain <= margin = 1e-8) rejects an existing MLE.

h(q) = log q - log A(q) - A(q),  A(q) = n^-1 sum log(1 + q y_i),  y_i = x_i / mean(x)   [lomax README "reason"].
The MLE exists iff sup_q h > 0 (h -> 0 as q -> 0).  pareto2 fits lomax to y = x - min(x), so the same stop decides.
"""
import mpmath as mp
mp.mp.dps = 60

def h_of(ys):
    ybar = sum(ys)/len(ys); w = [v/ybar for v in ys]; n = len(w)
    def h(z):                      # z = log q
        q = mp.e**z; A = sum(mp.log1p(q*v) for v in w)/n
        return z-mp.log(A)-A
    return h

def gain(ys, lo=-3, hi=14, grid=1700):
    """interior sup of h on z in [lo, hi] (z = log q): grid then golden refinement around the best grid point.
    Returns (G, z*).  The q -> 0 end contributes the exponential limit itself (h -> 0), so MLE exists iff G > 0."""
    h = h_of(ys); best = None
    for i in range(grid+1):
        z = mp.mpf(lo)+(hi-lo)*mp.mpf(i)/grid
        v = h(z)
        if best is None or v > best[0]: best = (v, z, i)
    _, z0, i = best; a = z0-mp.mpf(hi-lo)/grid; b = z0+mp.mpf(hi-lo)/grid
    g = (mp.sqrt(5)-1)/2
    c = b-g*(b-a); d = a+g*(b-a); fc, fd = h(c), h(d)
    for _ in range(160):
        if fc > fd: b, d, fd = d, c, fc; c = b-g*(b-a); fc = h(c)
        else: a, c, fc = c, d, fd; d = a+g*(b-a); fd = h(d)
    z = (a+b)/2
    return h(z), z

def bisect(f, a, b, it=200):
    fa = f(a)
    for _ in range(it):
        m = (a+b)/2; fm = f(m)
        if (fm > 0) == (fa > 0): a, fa = m, fm
        else: b = m
    return (a+b)/2

if __name__ == '__main__':
    # (1) lomax, two observations (1, r): transition ratio ~ 28.81 (README)
    G = lambda r: gain([mp.mpf(1), r])[0]
    rc = bisect(lambda r: G(r), mp.mpf(28), mp.mpf(30), it=70)
    print('lomax (1, r): interior sup crosses 0 at r_c = %s' % mp.nstr(rc, 25))
    for target in (mp.mpf('1e-9'), mp.mpf('5e-9'), mp.mpf('1e-8')):
        r = bisect(lambda r: G(r)-target, rc, rc+mp.mpf('0.01'), it=70)
        g, z = gain([mp.mpf(1), r])
        print('  gain %s at r = %s (G = %s at q = e^%s)' % (mp.nstr(target, 3), mp.nstr(r, 20), mp.nstr(g, 6), mp.nstr(z, 6)))
    # (2) the same data as doubles: the gain is evaluated at the doubles themselves
    print()
    for target in ('1e-9', '5e-9', '1e-8'):
        r = bisect(lambda r: G(r)-mp.mpf(target), rc, rc+mp.mpf('0.01'), it=70)
        rd = float(r)
        g, z = gain([mp.mpf(1), mp.mpf(rd)])
        print('double r = %r  (x = (1, r), pareto2 with min = 0 held, or lomax): exact gain over the best exponential = %s per observation, '
              'total n*G = %s' % (rd, mp.nstr(g, 8), mp.nstr(2*g, 8)))
    # (3) pareto2 = lomax on x - min with min held; with min = 0 the data are x itself.  A zero excess (x = min) is a separate
    # branch (zeros: Unbounded, point mass, unattained supremum) and never reaches the exponential stop.
