"""High-precision checks for analytic bounds and the spacing cover."""
from collections import Counter
from fractions import Fraction
import itertools
import json
import math
import time

import mpmath as mp

from cover import pair_bound


def segment(lo, hi, shape):
    if shape == 0:
        return mp.log(hi/lo)
    return lo**shape * mp.expm1(shape*mp.log(hi/lo))/shape


def audit_boxes():
    probes = [mp.mpf(s) for s in ('0.00001', '0.01', '0.1', '0.3', '0.5', '0.8', '0.99', '0.99999')]
    shapes = [mp.mpf(s) for s in ('-1', '-0.999999', '-0.5', '0', '0.5', '0.999999', '1')]
    smallest = mp.inf
    count = 0
    for p, r in itertools.combinations(probes, 2):
        for a, b in itertools.product(shapes, repeat=2):
            d = segment(p, r, a)+segment(1-r, 1-p, b)
            head = p**(a-1)+(1-p)**(b-1)
            tail = r**(a-1)+(1-r)**(b-1)
            exact = d/mp.sqrt(head*tail)
            for width in (0., .002, .07):
                box = tuple((max(lo, float(c)-width), min(hi, float(c)+width))
                            for (lo, hi), c in zip(((0, 1), (-10, 10), (-1, 1), (-1, 1)),
                                                   (p, 0, a, b)))
                got = pair_bound(box, float(d))
                slack = got-exact
                assert slack >= -mp.mpf('2e-8'), (p, r, a, b, width, slack)
                smallest = min(smallest, slack)
                count += 1
    return {'evaluations': count, 'min_upper_minus_point': mp.nstr(smallest, 15)}


def examples():
    h, v, eps = (mp.mpf(s) for s in ('1e-6', '1e-5', '1e-14'))
    scale = v/(1-v*v)

    def density(x):
        gap = x/scale
        tail = 2/(mp.sqrt(gap*gap+4)+gap)
        return 1/(scale*(1+tail**-2))

    pair_edge = (1-h/10)**2/2
    sample_edge = pair_edge**2/(1-2*eps)**2
    sample_witness = mp.fprod(density(x) for x in (0, eps, 1-eps, 1))
    assert sample_edge < sample_witness < mp.mpf('0.25')
    assert (1-mp.mpf('1e-4')+2*h)*100**h < 1-h/10
    assert (1+h/50)/mp.sqrt(1+h/2) < 1-h/10
    assert mp.mpf('0.1')*(mp.log(100)+1+2*h) < mp.mpf('0.6')
    assert (mp.sqrt(2)*4*mp.power(2, -2**26))**(h/2)*2 < mp.mpf('1e-9')
    assert 16*128/(128-mp.mpf('4.8'))**2 < mp.mpf('0.14')
    assert mp.sqrt(mp.mpf('.008'))*(mp.log(125)+mp.mpf('4.8')) < 1
    assert mp.mpf('.008') < 1/(2*(1+mp.sqrt(mp.mpf('2.4'))))
    return {k: mp.nstr(x, 25) for k, x in (
        ('pair_edge_upper', pair_edge), ('four_point_edge_upper', sample_edge),
        ('four_point_corner_witness', sample_witness))}


def generic_log_cutoff(xs, face):
    """Sufficient log scale cutoff, with one threshold per tied value."""
    x = [mp.mpf(z.numerator)/z.denominator for z in xs]
    n, k = len(x), len(x)//2
    bf = mp.mpf(face.numerator)/face.denominator
    ordered = sorted(set(x))
    delta = min(y-z for z, y in zip(ordered, ordered[1:]))/2
    thresholds = [mp.log(delta)]
    for m in range(k):
        thresholds.append(
            (mp.log(bf)+(m-1)*mp.log(2)+2*(n-m)*mp.log(delta))/(n-2*m)
        )
    for t, m in Counter(x).items():
        if m != k:
            continue
        distances = [abs(z-t) for z in x if z != t]
        cap = max(mp.mpf(2), (mp.fprod((d-delta)**-2 for d in distances)/bf)**(mp.mpf(1)/k))
        extreme = t in (ordered[0], ordered[-1])
        rho = mp.mpf(1) if extreme else mp.power(2, -2*k-2)
        eta = min(mp.mpf('.5'), 1/(100*k*mp.log(cap)))
        for exponent in range(1, 24):
            w = -mp.power(2, exponent)
            v = mp.exp(w)
            common = mp.sqrt(v)*(-w+cap) < rho
            condition = (v < 1/(4*(1+mp.sqrt(cap/2))) if extreme else
                         v < 1/(100*cap) and 2*v**(eta/2)+cap*v**(1-eta/2) < rho)
            if common and condition:
                break
        else:
            raise AssertionError('threshold grid exhausted')
        log_gap = max(mp.log(2*cap), mp.log(64/rho), mp.log(3)-w, mp.log(3*mp.sqrt(cap)))
        thresholds.append(mp.log(min(distances))-log_gap)
    return min(thresholds)


def prior_samples():
    eps = Fraction(1, 10000)
    samples = [
        (0, 1), (0, 1, 2), (-2, 0, 0, 3),
        (-1-eps, -1+eps, 1-eps, 1+eps), (0, 0, 1, 3),
        (Fraction(1, 8), Fraction(1, 4), Fraction(1, 2)), (-1, 0, 2),
    ]
    rows = []
    for x in samples:
        n = len(x)
        uniform = Fraction(1)/(max(x)-min(x))**n
        faces = [
            Fraction(1, 2 if t in (min(x), max(x)) else 8)**k
            / math.prod((z-t)**2 for z in x if z != t)
            for t, k in Counter(x).items() if 2*k == n
        ]
        face = max(faces, default=0)
        rows.append(dict(sample=list(map(str, x)), uniform=str(uniform), face=str(face),
                         outcome=('strict ties' if not face else 'strict witness'
                                  if uniform > face else 'run cover'), boxes=0,
                         generic_log_scale_cutoff=(mp.nstr(generic_log_cutoff(x, face), 12)
                                                   if face else None)))
    assert [r['outcome'] for r in rows].count('run cover') == 1
    return rows


if __name__ == '__main__':
    import sys
    start = time.perf_counter()
    mp.mp.dps = 85
    result = {'python': sys.version.split()[0], 'mpmath': mp.__version__,
              'digits': mp.mp.dps, 'prior_samples': prior_samples(),
              'bounds': examples(), 'cover_audit': audit_boxes()}
    result['seconds'] = time.perf_counter()-start
    print(json.dumps(result, indent=2))
