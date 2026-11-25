"""Reproduce TASK-263 score and boundary checks using mpmath and fractions."""
import csv
from fractions import Fraction as F
from itertools import combinations
from pathlib import Path
import re
import mpmath as mp

mp.mp.dps = 90


def log_unit_bounds(x):
    """Enclose log(x) on [1, 2] by the positive atanh series."""
    assert 1 <= x <= 2
    z = (x - 1) / (x + 1)
    terms = 28
    lower = 2 * sum(z ** (2 * j + 1) / (2 * j + 1) for j in range(terms))
    remainder = 2 * z ** (2 * terms + 1) / ((2 * terms + 1) * (1 - z*z))
    return lower, lower + remainder


def log_bounds(x):
    assert x >= 1
    power = 0
    while x > 2:
        x /= 2
        power += 1
    lower, upper = log_unit_bounds(x)
    lo2, hi2 = log_unit_bounds(F(2))
    return lower + power * lo2, upper + power * hi2


def exact_counterexample():
    sample = (F(0), F(41, 100))
    counts = (2, 20)
    grid = ('0.50', '0.56', '0.90', '0.99')
    for t, bound in zip(map(F, grid), (55, -32, 26, -6)):
        y = [t + d for d in sample]
        powers = [v**48 for v in y]
        brackets = [log_bounds(1 + v) for v in powers]
        sum_lo = sum(k * b[0] for k, b in zip(counts, brackets))
        sum_hi = sum(k * b[1] for k, b in zip(counts, brackets))
        q_lo, q_hi = 22 / sum_hi, 22 / sum_lo
        u = [v / (1 + v) for v in powers]
        lower = sum(k * (47 - 48 * (1 + q_hi) * z) / v for k, z, v in zip(counts, u, y))
        upper = sum(k * (47 - 48 * (1 + q_lo) * z) / v for k, z, v in zip(counts, u, y))
        assert bound < lower <= upper < bound + 1
        print(f'Burr score t={t}: in ({bound}, {bound + 1})')


def gb_loglik(y, p, q, c, s):
    return mp.fsum(
        mp.log(c / mp.beta(p, q)) + (c*p - 1) * mp.log(v)
        - c*p * mp.log(s) - (p+q) * mp.log1p((v/s)**c)
        for v in y
    )


def bisect_increasing(fun, lo, hi):
    assert fun(lo) < 0 < fun(hi)
    for _ in range(260):
        mid = (lo + hi) / 2
        if fun(mid) < 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def scale_checks():
    for p, q, c in [(1, mp.mpf('0.5'), 1), (1, 1, 2), (2, mp.mpf('0.5'), 2)]:
        gaps = [0, 0, 1, 3]
        a, b = c*p, c*q
        r = ((mp.mpf(a)-1)/(b+1))**(mp.mpf(1)/c) if a > 1 else mp.mpf(0)
        # Use an interior ray even when the optimal ray has r=0.
        ray = r or mp.mpf(1)
        eps = [mp.mpf(10)**-j for j in (5, 10, 20, 30)]
        values = [gb_loglik([d+ray*s for d in gaps], p, q, c, s) for s in eps]
        exponent = 2*b-2
        slope = (values[-1]-values[-2]) / mp.log(eps[-1]/eps[-2])
        assert abs(slope-exponent) < mp.mpf('1e-18')
        if exponent == 0:
            face = 4*mp.log(c/mp.beta(p,q)) + 2*((a-1)*mp.log(r)-(p+q)*mp.log1p(r**c)) - (b+1)*mp.log(3)
            assert all(v < face for v in values)
            assert 0 < face-values[-1] < mp.mpf('1e-28')

    p, q, c = 2, 1, 3
    old = mp.inf
    for j in range(-12, 13):
        t = mp.power(10, mp.mpf(j)/4)
        y = [t, t, t+1, t+3]
        z = [c*mp.log(v) for v in y]

        def u(log_scale):
            return [1/(1+mp.exp(log_scale-w)) for w in z]

        root = bisect_increasing(lambda v: mp.mpf(4)*p/(p+q)-sum(u(v)), min(z)-10, max(z)+10)
        weights = [1/v for v in y]
        score = sum(w*(c*p-1-(p+q)*c*v) for v,w in zip(u(root),weights)) / sum(weights)
        assert score < old
        old = score
    assert old < 0
    print('Scale collapse slopes, equality bound and 25 normalized scores passed')


def beta_checks():
    z = [mp.mpf(1), mp.mpf(2), mp.mpf(5), mp.mpf(5)]
    a = mp.mpf('2.5')
    g = mp.exp(mp.fsum(map(mp.log, z))/len(z))

    def shape(w):
        h = mp.log(w/g)
        return bisect_increasing(lambda b: h-mp.digamma(b+a)+mp.digamma(b),
                                 mp.mpf('1e-10'), mp.mpf('1e10'))

    def width_score(w):
        return (a-1)*mp.fsum(v/(w-v) for v in z)/len(z)-shape(w)

    w = bisect_increasing(lambda w: -width_score(w), mp.mpf('5.01'), mp.mpf(50))
    b = shape(w)
    ratios = [v/(w-v) for v in z]
    curvature = -(a-1)*mp.fsum(v*(1+v) for v in ratios)/len(z)
    curvature += 1/(mp.polygamma(1,b)-mp.polygamma(1,b+a))
    assert abs(width_score(w)) < mp.mpf('1e-70') and curvature < 0

    eps = mp.mpf('1e-30')
    # At constant z=1, w=1+eps and b=1/eps, the optimal-order spike has
    # eps*f -> exp(-1)/Gamma(a).
    density = (1/(1+eps))**(1/eps)*(eps/(1+eps))**(a-1)/mp.beta(1/eps,a)
    assert abs(eps*density-mp.exp(-1)/mp.gamma(a)) < mp.mpf('1e-27')
    print(f'Beta mixed profile w={mp.nstr(w, 18)}, b={mp.nstr(b, 18)}')


def mask_checks():
    here = Path(__file__).resolve().parent
    distributions = here.parents[3] / 'univariateML-cpp/include/cppunivariate/distributions'
    regular = {
        'pareto2': ('shape', 'scale'),
        'pareto3': ('shape', 'scale'),
        'pareto4': ('shape1', 'shape2', 'scale'),
        'fpareto': ('shape1', 'shape2', 'shape3', 'scale'),
        'nsbeta': ('shape1', 'shape2', 'min', 'max'),
    }
    with (here / 'masks.csv').open(newline='') as source:
        table = list(csv.DictReader(source))
    actual = [(row['family'], frozenset(row['mask'].split('+'))) for row in table]
    expected = set()
    for family, names in regular.items():
        subsets = {frozenset(mask) for n in range(1, len(names)+1)
                   for mask in combinations(names, n)
                   if not {'min', 'max'}.issubset(mask)}
        expected.update((family, mask) for mask in subsets)
        dossier = (distributions / family / 'README.md').read_text()
        if family == 'pareto4':
            mle = (distributions / family / 'mle.hpp').read_text()
            assert 'else out.unsupported_threshold=true;' in mle
            continue
        holds = dossier.split('\nholds:', 1)[1].split('\nfit_args:', 1)[0]
        declared = re.findall(r'- held:(.*?)(?=\n  status:)', holds, re.S)
        declared = {frozenset(re.findall(r'shape[123]?|scale|min|max', entry))
                    for entry in declared}
        assert subsets == declared - {frozenset()}
    assert len(actual) == len(set(actual)) == len(expected) == 39
    assert set(actual) == expected
    print('All 39 held masks checked against coefficient subsets and source declarations')


if __name__ == '__main__':
    mask_checks()
    exact_counterexample()
    scale_checks()
    beta_checks()
