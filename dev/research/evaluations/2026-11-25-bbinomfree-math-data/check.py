"""Exact rational tail-cover checks and independent high-precision seam replay."""
from collections import Counter
from csv import DictReader
from fractions import Fraction as F
from itertools import combinations, combinations_with_replacement
from math import comb, factorial
from pathlib import Path

import mpmath as mp
import sympy as sp


def rising(a, n):
    value = 1
    for j in range(n):
        value *= a + j
    return value


def bb_witness(sample, n, a, b):
    value = F(1)
    for x in sample:
        value *= comb(n, x) * rising(a, x) * rising(b, n-x) / rising(a+b, n)
    return value


def ratio(j, z, d, k):
    return (z+j*d)*(1-j*k)/((j+1)*(1-(j+1)*d*k))


def ratio_box(j, box, k, kmin=F(0)):
    zl, zh, dl, dh = box
    kl = k if (j+1)*dl < j else kmin
    kh = k if (j+1)*dh > j else kmin
    return ratio(j, zl, dl, kl), ratio(j, zh, dh, kh)


def tangent_excludes(counts, box, k, prefix, threshold, kmin):
    zl, zh, dl, dh = box
    z, d = (zl+zh)/2, (dl+dh)/2
    reference = [ratio(j, z, d, kmin) for j in range(prefix)]
    masses = [F(1)]
    for r in reference:
        masses.append(masses[-1]*r)
    normalizer = sum(masses)
    base = F(1)
    for x, count in counts.items():
        base *= (masses[x]/normalizer)**count
    if base >= threshold:
        return False
    h = sum(counts.values())
    tail = normalizer
    change = F(0)
    for j, r in enumerate(reference):
        tail -= masses[j]
        c = sum(count for x, count in counts.items() if x > j) - h*tail/normalizer
        lo, hi = ratio_box(j, box, k, kmin)
        if c > 0:
            change += c*(hi/r-1)
        elif c < 0:
            if not lo:
                return False
            change -= c*(r/lo-1)
        if change >= 1-base/threshold:
            return False
    return True


def upper(counts, box, k, prefix, kmin):
    bounds = [ratio_box(j, box, k, kmin) for j in range(prefix)]
    value = F(1)
    for x, count in counts.items():
        total = F(1)
        product = F(1)
        for j in range(x-1, -1, -1):
            if not bounds[j][1]:
                return F(0)
            product /= bounds[j][1]
            total += product
        product = F(1)
        for j in range(x, prefix):
            product *= bounds[j][0]
            total += product
        value /= total**count
    return value


def tail_cover(sample, threshold, cutoff, prefix, last=None):
    counts = Counter(sample)
    m = max(sample)
    k = F(1, cutoff)
    assert m < prefix < cutoff
    kmin = F(0) if last is None else F(1, last)
    # If the full likelihood is at least threshold then P(M) >= threshold.
    # P(prefix) >= P(M) times this lower bound for the ratio.
    z_bound = 1
    while True:
        product = F(1)
        for j in range(m, prefix):
            product *= z_bound*(1-j*k)/(j+1)
        if product*threshold > 1:
            break
        z_bound *= 2
    boxes = [(0, (F(0), F(z_bound), F(0), F(1)))]
    checked = 0
    depth = 0
    while boxes:
        level, box = boxes.pop()
        checked += 1
        if checked > 12000 or level > 40:
            raise AssertionError((sample, cutoff, prefix, checked, box))
        depth = max(depth, level)
        if (upper(counts, box, k, prefix, kmin) < threshold
                or tangent_excludes(counts, box, k, prefix, threshold, kmin)):
            continue
        axis = 2*(level % 2)
        mid = (box[axis]+box[axis+1])/2
        boxes.append((level+1, box[:axis+1]+(mid,)+box[axis+2:]))
        boxes.append((level+1, box[:axis]+(mid,)+box[axis+1:]))
    return z_bound, checked, depth


def fixed_upper(x, n, box):
    pl, ph, dl, dh = box
    value = comb(n, x)
    if x:
        value *= ph
    if x < n:
        value *= 1-pl
    factors = [(ph, j) for j in range(1, x)]
    factors += [(1-pl, j) for j in range(1, n-x)]
    for i, (p, j) in enumerate(factors):
        value *= max((p*(1-dl)+j*dl)/(1+i*dl), (p*(1-dh)+j*dh)/(1+i*dh))
    if 0 < x < n:
        value *= (1-dl)/(1+(n-2)*dl)
    return value


def fixed_cover(sample, n, threshold):
    boxes = [(0, (F(0), F(1), F(0), F(1)))]
    checked = 0
    while boxes:
        level, box = boxes.pop()
        checked += 1
        assert checked <= 12000 and level <= 40, (n, checked, box)
        bound = F(1)
        for x in sample:
            bound *= fixed_upper(x, n, box)
        if bound < threshold:
            continue
        axis = 2*(level % 2)
        mid = (box[axis]+box[axis+1])/2
        boxes.append((level+1, box[:axis+1]+(mid,)+box[axis+2:]))
        boxes.append((level+1, box[:axis]+(mid,)+box[axis+1:]))
    return checked


def nonunimodal_finite_prefix():
    sample, threshold = (1, 1, 5), F(1, 216)
    for n in range(6, 13):
        print('exact fixed exclusion', n, fixed_cover(sample, n, threshold), flush=True)
    for first, last in ((13, 15), (16, 19), (20, 25), (26, 39), (40, 63)):
        print('exact integer block', first, last,
              tail_cover(sample, threshold, first, min(16, first-1), last), flush=True)


def log_box(x, terms=36):
    t = (x-1)/(x+1)
    value = 2*sum(t**(2*j+1)/(2*j+1) for j in range(terms))
    error = 2*abs(t)**(2*terms+1)/((2*terms+1)*(1-t*t))
    return value-error, value+error


def stationary_size_five():
    a, t = sp.symbols('a t')
    def score_a(x):
        return 3/x+sum(1/(x+j) for j in range(1, 5))
    def score_b(x):
        return 2*sum(1/(x+j) for j in range(4))
    def shared(x):
        return 3*sum(1/(x+j) for j in range(5))
    p = sp.together(score_a(a)-shared(t)).as_numer_denom()[0]
    q = sp.together(score_b(t-a)-shared(t)).as_numer_denom()[0]
    resultant = sp.Poly(sp.resultant(p, q, a), t)
    assert not resultant.is_zero

    def inv_bracket(score, target):
        lo, hi = F(0), F(1)
        while score(hi) > target:
            hi *= 2
        for _ in range(100):
            mid = (lo+hi)/2
            if score(mid) > target:
                lo = mid
            else:
                hi = mid
        return lo, hi

    remaining = []
    intervals = resultant.intervals(eps=sp.Rational(1, 10**28))
    for (left, right), _ in intervals:
        if right <= 0:
            continue
        lo, hi = F(int(left.p), int(left.q)), F(int(right.p), int(right.q))
        assert lo > 0
        al = inv_bracket(score_a, shared(lo))[0]
        ah = inv_bracket(score_a, shared(hi))[1]
        bl = inv_bracket(score_b, shared(lo))[0]
        bh = inv_bracket(score_b, shared(hi))[1]
        if al+bl <= hi and ah+bh >= lo:
            remaining.append((al, ah, bl, bh))
    assert len(remaining) == 1
    al, ah, bl, bh = remaining[0]
    assert (ah-al)/al < F(1, 10**24) and (bh-bl)/bl < F(1, 10**24)
    print('exact size-five singleton', resultant.degree(), len(intervals),
          'alpha', float(al), 'beta', float(bl), flush=True)


def exact_nonunimodality_inputs():
    a0, a1 = F(451, 100), F(113, 25)
    m = F(7, 3)
    def score(a):
        lo, hi = log_box(1+m/a)
        total = 3/a+sum(1/(a+j) for j in range(1, 5))
        return total-3*hi, total-3*lo
    assert score(a0)[0] > 0 and score(a1)[1] < 0
    assert a0 > F(49, 11)
    nb_upper = sum(log_box((a1+j)/(j+1))[1] for x in (1, 1, 5) for j in range(x))
    nb_upper += 7*log_box(m/(a0+m))[1] - 3*a0*log_box(1+m/a0)[0]
    assert nb_upper < -3*log_box(F(6))[1]
    print('exact nonunimodality inputs passed', flush=True)


def seam():
    mp.mp.dps = 80
    sample = (1, 1, 5)
    mu = mp.mpf(7)/3
    h = len(sample)
    def score(a):
        return sum(sum(1/(a+j) for j in range(x)) for x in sample) - h*mp.log1p(mu/a)
    a = mp.findroot(score, (mp.mpf('4.51'), mp.mpf('4.52')))
    face = sum(mp.log(rising(a, x)/mp.factorial(x)) + x*mp.log(mu/(a+mu))
               - a*mp.log1p(mu/a) for x in sample)
    slope = h*(mu**2-a*mp.mpf(11)/9)/(2*(a+mu))
    for n in (100, 1000, 10000, 100000, 1000000):
        def objective(shape, c):
            b = c*n
            return sum(mp.log(comb(n, x)) + mp.log(rising(shape, x))
                       + mp.loggamma(b+n-x)-mp.loggamma(b)
                       + mp.loggamma(shape+b)-mp.loggamma(shape+b+n)
                       for x in sample)
        aa, cc = mp.findroot(lambda aa, cc: (mp.diff(lambda t: objective(t, cc), aa),
                                            mp.diff(lambda t: objective(aa, t), cc)),
                             (a, a/mu), tol=mp.mpf('1e-54'))
        delta = n*(objective(aa, cc)-face)
        assert delta < 0 and abs(delta-slope) < mp.mpf(10)/n
        print('seam', n, mp.nstr(delta, 22), flush=True)
    print('limiting slope', mp.nstr(slope, 22), flush=True)


def quartic():
    for counts, expected in (
        ({1: 2, 4: 1}, F(23, 15)),
        ({0: 2, 1: 3, 3: 1}, F(19, 240)),
        ({0: 9, 1: 8, 2: 6, 4: 1}, -F(7, 30)),
        ({0: 614, 1: 845, 2: 80, 3: 250, 4: 10, 5: 1}, F(0)),
    ):
        h = sum(counts.values())
        def avg(f):
            return sum(F(c, h)*f(x) for x, c in counts.items())
        m = avg(lambda x: x)
        assert avg(lambda x: x*(x-1)) == m*m
        assert avg(lambda x: x*(x-1)*(x-2)) == m**3
        b = -m*m/4
        d = (-m**3/6-m**4/4-m**5/15
             + avg(lambda x: sum(-j**4+m*m*(j+1)**2 for j in range(x)))/2)
        v = (m**3/2+m**4/4
             + avg(lambda x: sum(j**3-m*m*(j+1) for j in range(x))))
        w = -v/(2*b)
        coefficient = d-v*v/(4*b)
        assert coefficient == expected
        mu = mp.mpf(m.numerator)/m.denominator
        for n in (1000, 10000, 100000):
            k = mp.mpf(1)/n
            a = 1/(k+mp.mpf(w.numerator)/w.denominator*k*k)
            beta = a*(n/mu-1)
            p0 = mp.loggamma(a+beta)+mp.loggamma(beta+n)-mp.loggamma(beta)-mp.loggamma(a+beta+n)
            delta = p0+mu
            for x, count in counts.items():
                delta += mp.mpf(count)/h*sum(
                    mp.log1p(-j*k)+mp.log1p(j/a)
                    -mp.log1p(mu*(1/a-k)-mu*(j+1)*k/a) for j in range(x))
            scaled = delta*n**4
            target = mp.mpf(expected.numerator)/expected.denominator
            assert abs(scaled-target) < 100*k
            print('quartic', counts, n, mp.nstr(scaled, 16), 'target', expected, flush=True)


def binomial_likelihood(sample, n):
    p = F(sum(sample), n*len(sample))
    value = p**sum(sample)*(1-p)**(n*len(sample)-sum(sample))
    for x in sample:
        value *= comb(n, x)
    return value


def underdispersed_search():
    path = (Path(__file__).resolve().parent.parent
            / '2026-11-02-bbinom-map-data' / 'samples.csv')
    checked = 0
    examples = []
    with path.open() as data:
        for row in DictReader(data):
            if row['grid_type'] != 'interior' or not row['binom_N']:
                continue
            sample = tuple(map(int, row['data'].split()))
            h, total = len(sample), sum(sample)
            disp = h*total+total**2-h*sum(x*x for x in sample)
            if disp <= 0:
                continue
            cutoff = (total**2+disp-1)//disp
            n = int(row['grid_N'])
            nb = int(row['binom_N'])
            if not max(sample) <= n < cutoff < nb <= 200:
                continue
            checked += 1
            at_cutoff = binomial_likelihood(sample, cutoff)
            if binomial_likelihood(sample, cutoff+1) <= at_cutoff:
                continue
            best_bin = binomial_likelihood(sample, nb)
            if not (binomial_likelihood(sample, nb-1) < best_bin
                    and binomial_likelihood(sample, nb+1) < best_bin):
                continue
            a = F(row['grid_a']).limit_denominator(100)
            b = F(row['grid_b']).limit_denominator(100)
            if a > 0 and b > 0 and bb_witness(sample, n, a, b) > best_bin:
                examples.append((sample, n, a, b, cutoff, nb))
    print('exact underdispersed screening', checked, len(examples), flush=True)
    if examples:
        print('two finite peaks', min(examples, key=lambda row: (len(row[0]), max(row[0]), row[0])),
              flush=True)


def two_finite_peaks():
    m = 4

    def binom_increment(n, counts):
        h = sum(counts.values())
        total = m*h
        q, qnext = 1-F(m, n), 1-F(m, n+1)
        factors = [(F(n+1, n+1-x), c) for x, c in counts.items()]
        factors += [(qnext/q, h*n-total), (qnext, h), (F(n, n+1), total)]
        bounds = [log_box(factor, 12) for factor, _ in factors]
        lo = sum(c*bound[0] for (_, c), bound in zip(factors, bounds))
        hi = sum(c*bound[1] for (_, c), bound in zip(factors, bounds))
        return lo, hi

    for r in (8, 16, 32, 64):
        counts = {2: 2*r, 3: 2*r, 4: r+1, 7: 2*r}
        h = sum(counts.values())
        k = 4*h
        if binom_increment(k, counts)[0] <= 0:
            continue
        lo, hi = k, 2*k
        assert binom_increment(hi, counts)[1] < 0
        while hi-lo > 1:
            mid = (lo+hi)//2
            a, b = binom_increment(mid, counts)
            assert a > 0 or b < 0
            if a > 0:
                lo = mid
            else:
                hi = mid
        diff = F(0)
        for x, c in counts.items():
            # Each ratio is exactly BB(7,2,1)(x) / Bin(hi,4/hi)(x).
            diff += c*log_box(F((x+1)*factorial(x), 36*m**x), 180)[0]
            diff -= c*(hi-x)*log_box(1-F(m, hi), 12)[1]
            diff -= c*sum(log_box(1-F(j, hi), 12)[1] for j in range(x))
        if diff > 0:
            print('proved underdispersed double peak', counts, 'cutoff', k,
                  'binomial peak', hi, 'log gap lower', float(diff), flush=True)
            return
    raise AssertionError('double peak witness did not clear')


def tie_screen():
    mp.mp.dps = 60
    gaps = {}
    pairs = Counter()
    solved, missed = 0, 0
    for h in (3, 4, 5):
        for sample in combinations_with_replacement(range(5), h):
            m = max(sample)
            if m < 3 or not any(0 < x < m for x in sample):
                continue
            total = sum(sample)
            q = sum(x*x for x in sample)
            if total == h*m or q*h == total*total:
                continue
            values = []
            for n in range(m, m+3):
                disp = n*h*q-(n-1)*total**2-n*h*total
                if disp <= 0:
                    height = binomial_likelihood(sample, n)
                    values.append((n, False, mp.log(mp.mpf(height.numerator)/height.denominator)))
                    continue
                p = mp.mpf(total)/(h*n)
                variance = (mp.mpf(h*q)-total**2)/h**2
                rho = (variance/(n*p*(1-p))-1)/(n-1)
                a, b = p*(1/rho-1), (1-p)*(1/rho-1)

                def scores(u, v):
                    a, b = mp.exp(u), mp.exp(v)
                    c = h*sum(1/(a+b+j) for j in range(n))
                    return (a*(sum(1/(a+j) for x in sample for j in range(x))-c),
                            b*(sum(1/(b+j) for x in sample for j in range(n-x))-c))

                try:
                    root = mp.findroot(scores, (mp.log(a), mp.log(b)), maxsteps=70,
                                       tol=mp.mpf('1e-88'))
                    a, b = map(mp.exp, root)
                    if max(map(abs, scores(*root))) > mp.mpf('1e-40') or a+b > 10**6:
                        raise ValueError('score or concentration gate')
                except (ValueError, ZeroDivisionError):
                    missed += 1
                    continue
                height = sum(mp.log(comb(n, x)*rising(a, x)*rising(b, n-x)/rising(a+b, n))
                             for x in sample)
                solved += 1
                values.append((n, True, height))
            for (n1, a1, l1), (n2, a2, l2) in combinations(values, 2):
                if not (a1 or a2):
                    continue
                key = 'shape/shape' if a1 and a2 else 'shape/binomial'
                pairs[key] += 1
                gap = abs(l1-l2)
                if key not in gaps or gap < gaps[key][0]:
                    gaps[key] = (gap, sample, n1, n2)
    assert all(gap[0] > mp.mpf('1e-25') for gap in gaps.values())
    print('numerical tie screen', solved, 'stationary solves', missed, 'unresolved starts', flush=True)
    for key, (gap, sample, n1, n2) in gaps.items():
        print(key, pairs[key], mp.nstr(gap, 18), sample, n1, n2, flush=True)


if __name__ == '__main__':
    for sample, n, a, b, cutoff, prefix in (
        ((0, 1, 2), 2, 1, 1, 64, 12),
        ((1, 1, 5), 5, 1, 1, 64, 16),
        ((1, 1, 4), 4, 1, 1, 64, 16),
    ):
        w = bb_witness(sample, n, F(a), F(b))
        print('exact tail', sample, cutoff, prefix, tail_cover(sample, w, cutoff, prefix), flush=True)
    # The reachable binomial face is a valid likelihood threshold too.
    print('exact binomial tail', tail_cover((1, 2), F(27, 128), 64, 12), flush=True)
    exact_nonunimodality_inputs()
    nonunimodal_finite_prefix()
    stationary_size_five()
    seam()
    quartic()
    underdispersed_search()
    two_finite_peaks()
    tie_screen()
