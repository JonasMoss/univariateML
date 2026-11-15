"""gompertz exponential-limit decision next to the exact boundary.

Criterion (README Details; mle.hpp `sum_xsq / n >= 2.0 * mean_x * mean_x`, on the standardized data z = x / unit):
    exponential limit  iff  delta = n Q - 2 S^2 >= 0,  S = sum x, Q = sum x^2   (homogeneous of degree 2).
Samples: n-1 random doubles, the last one the double next to a root of delta(y) = 0 (a quadratic in y),
then its ulp neighbours.  delta, and so the exact decision, is computed in rationals from the doubles.

Mirrors of gompertz_mle_impl (lines 71-115): unit = (long double sum)/n  (native)  or  double sum/n (double-only);
z = x / unit; sx += z; sq += z*z; m = sx/dn; decide sq/dn >= 2.0*m*m.  'fma': sq = fma(z, z, sq).
"""
import math, random, sys, json
from fractions import Fraction
import numpy as np
import mpmath as mp
mp.mp.prec = 400
LD = np.longdouble

def mirror(x, extended, fma):
    n = len(x)
    if extended:
        t = LD(0)
        for v in x: t += LD(v)
        unit = float(t/LD(n))
    else:
        t = 0.0
        for v in x: t += v
        unit = t/n
    z = [v/unit for v in x]
    sx = sq = 0.0
    for v in z:
        sx += v
        sq = math.fma(v, v, sq) if fma else sq+v*v
    m = sx/n
    return sq/n >= 2.0*m*m

def exact_delta(x):
    fx = [Fraction(v) for v in x]
    S = sum(fx); Q = sum(v*v for v in fx)
    return len(x)*Q-2*S*S, S

def root_last(base):
    n = len(base)+1
    S0 = mp.mpf(0); Q0 = mp.mpf(0)
    for v in base: S0 += mp.mpf(v); Q0 += mp.mpf(v)**2
    a = n-2; b = -4*S0; c = n*Q0-2*S0*S0
    disc = b*b-4*a*c
    if disc < 0: return []
    r = [(-b-mp.sqrt(disc))/(2*a), (-b+mp.sqrt(disc))/(2*a)]
    return [float(v) for v in r if v > 0]

def run(seed=20261115):
    rng = random.Random(seed)
    rows = []
    for n in (3, 5, 10, 30, 100, 300, 1000):
        made = 0; attempts = 0
        target = 8 if n <= 100 else 3
        while made < target and attempts < 400:
            attempts += 1
            scale = 10.0**rng.choice((-3, 0, 3, 8))
            base = [rng.expovariate(1.0)*scale for _ in range(n-1)]
            roots = root_last(base)
            if not roots: continue
            for root in roots:
                y0 = root
                for k in list(range(-6, 7))+([s*m for m in (16, 64, 256, 1024) for s in (-1, 1)] if n >= 30 else []):
                    y = y0
                    for _ in range(abs(k)): y = math.nextafter(y, math.inf if k > 0 else 0.0)
                    x = base+[y]
                    d, S = exact_delta(x)
                    truth = d >= 0
                    gap = float(d/(2*S*S))
                    dec = {(e, f): mirror(x, e, f) for e in (True, False) for f in (False, True)}
                    rows.append(dict(n=n, scale=scale, k=k, gap=gap, truth=truth, ldplain=dec[(True, False)], ldfma=dec[(True, True)],
                                     dplain=dec[(False, False)], dfma=dec[(False, True)], x=x if n <= 5 else None))
            made += 1
    return rows

if __name__ == '__main__':
    rows = run()
    json.dump(rows, open(sys.argv[1] if len(sys.argv) > 1 else 'gompertz_boundary.json', 'w'))
    u = 2.0**-53
    print('samples: %d (ulp neighbours -6..6, and for n >= 30 also +-16, 64, 256, 1024, of a root of delta(y) = 0)' % len(rows))
    names = ('ldplain', 'ldfma', 'dplain', 'dfma')
    print('\nby n:      samples  ' + '  '.join('wrong(%s)' % m for m in names) + '   median |gap|/u')
    for n in sorted({r['n'] for r in rows}):
        v = [r for r in rows if r['n'] == n]; g = sorted(abs(r['gap'])/u for r in v)
        print('  %5d %8d  ' % (n, len(v)) + '  '.join('%10d' % sum(r[m] != r['truth'] for r in v) for m in names) + '   %.2f' % g[len(g)//2])
    print('\nby |gap|/u:   samples  ' + '  '.join('wrong(%s)' % m for m in names))
    edges = [0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 1e99]
    for lo, hi in zip(edges, edges[1:]):
        v = [r for r in rows if lo <= abs(r['gap'])/u < hi]
        if v: print('  [%g,%g) %7d  ' % (lo, hi, len(v)) + '  '.join('%10d' % sum(r[m] != r['truth'] for r in v) for m in names))
    w = [r for r in rows if any(r[m] != r['truth'] for m in names)]
    print('\nlargest |gap|/u among wrong decisions: %.1f (n=%d)' % max((abs(r['gap'])/u, r['n']) for r in w))
    for r in sorted(w, key=lambda r: (r['n'], abs(r['gap'])))[:0]: pass
    print('direction of errors (exact -> computed):')
    from collections import Counter
    c = Counter()
    for r in rows:
        for m in names:
            if r[m] != r['truth']: c[('limit->finite' if r['truth'] else 'finite->limit', m)] += 1
    for k, v in sorted(c.items()): print('  ', k, v)
