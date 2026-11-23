"""Hold-aware phenomenon cases and their exact maximizer sets.

Derivations: dev/research/evaluations/2026-11-22-zero-unique-report.md.
Predicates receive finite named coefficients with the holds already enforced.
"""
from dataclasses import dataclass
import math
from typing import Callable


COUNT_MAX = 2**53 - 1


@dataclass(frozen=True)
class Case:
    data: tuple
    held: dict
    member: Callable[..., bool]
    unique: bool
    loglik: float
    certified: bool
    witness_key: Callable[..., object] | None


def integer(x, lo=0, hi=math.inf):
    return lo <= x <= hi and x == int(x)


def probability(x):
    return 0 <= x <= 1


def urn(m, N):
    return integer(N, 0, COUNT_MAX) and integer(m, 0, N)


CASES = {}


def add(family, label, held, member, *, x=0, data=None, unique=False,
        loglik=0.0, certified=False, witness_key=None):
    data = (x,)*4 if data is None else data
    kind = 'pair' if len(set(data)) > 1 else 'zeros' if x == 0 else 'constant'
    key = kind+'-flat-'+label
    cases = CASES.setdefault(family, {})
    assert key not in cases
    cases[key] = Case(data, held, member, unique, loglik, certified, witness_key)


def patterns(family, member, variants, **kwargs):
    for label, held, unique in variants:
        add(family, label, held, member, unique=unique, **kwargs)


def nhyper_zero(m, N, r):
    return urn(m, N) and r == 0


for label, held, unique in (
    ('free', {}, False), ('m', {'m': 2}, False), ('N', {'N': 5}, False),
    ('m-N', {'m': 2, 'N': 5}, True), ('N-zero', {'N': 0}, True),
    ('m-max', {'m': COUNT_MAX}, True),
):
    for r_held in (False, True):
        args = dict(held, r=0) if r_held else held
        add('nhyper', label+'-r' if r_held else label, args, nhyper_zero,
            unique=unique, certified=True)

patterns('nhyper', lambda m, N, r: urn(m, N) and m == N and r == 7, (
    ('free', {}, False), ('r', {'r': 7}, False), ('N', {'N': 50}, True),
    ('m', {'m': 7}, True), ('N-r', {'N': 50, 'r': 7}, True),
    ('m-r', {'m': 7, 'r': 7}, True), ('m-N', {'m': 7, 'N': 7}, True),
    ('m-N-r', {'m': 7, 'N': 7, 'r': 7}, True),
), x=7)


def hyper_zero(m, N, k):
    return urn(m, N) and integer(k, 0, N) and (m == 0 or k == 0)


patterns('hyper', hyper_zero, (
    ('free', {}, False), ('N', {'N': 50}, False), ('m-zero', {'m': 0}, False),
    ('k-zero', {'k': 0}, False), ('m', {'m': 4}, False), ('k', {'k': 4}, False),
    ('N-m-zero', {'N': 50, 'm': 0}, False), ('N-k-zero', {'N': 50, 'k': 0}, False),
    ('N-m', {'N': 50, 'm': 4}, True), ('N-k', {'N': 50, 'k': 10}, True),
    ('m-k-zero', {'m': 4, 'k': 0}, False), ('m-zero-k', {'m': 0, 'k': 4}, False),
    ('N-zero', {'N': 0}, True), ('m-max', {'m': COUNT_MAX}, True),
    ('k-max', {'k': COUNT_MAX}, True),
))
patterns('hyper', lambda m, N, k: urn(m, N) and integer(k, 0, N) and
         ((m == N and k == 4) or (k == N and m == 4)), (
    ('free', {}, False), ('N', {'N': 50}, False), ('N-four', {'N': 4}, True),
    ('m-four', {'m': 4}, False), ('k-four', {'k': 4}, False),
    ('m-five', {'m': 5}, True), ('k-five', {'k': 5}, True),
    ('N-m', {'N': 50, 'm': 4}, True), ('N-k', {'N': 50, 'k': 4}, True),
    ('m-k', {'m': 4, 'k': 5}, True),
), x=4)


add('hyper', 'adjacent-compositions', {'N': 5, 'k': 2},
    lambda m, N, k: m in (2, 3), x=1, loglik=4*math.log(.6))
add('hyper', 'adjacent-draws', {'N': 5, 'm': 2},
    lambda m, N, k: k in (2, 3), x=1, loglik=4*math.log(.6))
add('hyper', 'adjacent-populations', {'m': 2, 'k': 2},
    lambda m, N, k: N in (3, 4), x=1, loglik=4*math.log(2/3))
add('nhyper', 'adjacent-compositions', {'N': 5, 'r': 1},
    lambda m, N, r: m in (2, 3), x=2, loglik=4*math.log(.3))
add('nhyper', 'adjacent-populations', {'m': 2, 'r': 1},
    lambda m, N, r: N in (3, 4), x=2, loglik=-4*math.log(3))
add('nhyper', 'adjacent-stops', {'m': 2, 'N': 5},
    lambda m, N, r: r in (1, 2), x=3, loglik=4*math.log(.2))


def binom_zero(size, prob):
    return integer(size) and probability(prob) and (size == 0 or prob == 0)


patterns('binom', binom_zero, (
    ('free', {}, False), ('size-zero', {'size': 0}, False),
    ('prob-zero', {'prob': 0}, False), ('size', {'size': 12}, True),
    ('prob', {'prob': .5}, True),
))
add('binom', 'adjacent-sizes', {'prob': .5}, lambda size, prob: size in (1, 2),
    x=1, loglik=-4*math.log(2))
add('binom', 'free', {}, lambda size, prob: size == 5 and prob == 1,
    x=5, unique=True)

patterns('bbinom', lambda size, alpha, beta: size == 0 and alpha > 0 and beta > 0, (
    ('free', {}, False), ('alpha', {'alpha': 1}, False), ('beta', {'beta': 1}, False),
    ('size-zero', {'size': 0}, False), ('size-alpha', {'size': 0, 'alpha': 1}, False),
    ('size-beta', {'size': 0, 'beta': 1}, False), ('shapes', {'alpha': 1, 'beta': 1}, True),
))

add('bbinom', 'adjacent-sizes', {'alpha': 1, 'beta': 2},
    lambda size, alpha, beta: size in (1, 2), x=1, loglik=-4*math.log(3))

patterns('cmb', lambda size, prob, nu: integer(size, 0, 10_000_000) and
         probability(prob) and (size == 0 or prob == 0), (
    ('size', {'size': 10}, False), ('free', {}, False),
    ('size-zero', {'size': 0}, False), ('prob-zero', {'prob': 0}, False),
    ('prob', {'prob': .5}, False), ('nu', {'nu': 1}, False),
    ('size-nu', {'size': 10, 'nu': 1}, True), ('prob-nu', {'prob': .5, 'nu': 1}, True),
    ('size-zero-prob', {'size': 0, 'prob': .5}, False),
    ('size-zero-nu', {'size': 0, 'nu': 1}, False),
))
patterns('cmb', lambda size, prob, nu: size == 3 and prob == 1, (
    ('free', {}, False), ('size', {'size': 3}, False),
    ('prob-one', {'prob': 1}, False), ('nu', {'nu': 1}, True),
), x=3)

add('cmb', 'adjacent-sizes', {'prob': .5, 'nu': 1},
    lambda size, prob, nu: size in (1, 2), x=1, loglik=-4*math.log(2))

for value in (0, 1):
    add('cmb', 'bernoulli', {'size': 1, 'prob': .5},
        lambda size, prob, nu: True, x=value, loglik=-4*math.log(2))

patterns('cmp', lambda **p: p['lambda'] == 0 and p['nu'] >= 0, (
    ('free', {}, False), ('lambda-zero', {'lambda': 0}, False),
    ('nu', {'nu': 1}, True),
))


def nb_zero(size, prob):
    return size >= 0 and 0 < prob <= 1 and (size == 0 or prob == 1)


patterns('nbinom', nb_zero, (
    ('free', {}, False), ('size-zero', {'size': 0}, False),
    ('prob-one', {'prob': 1}, False), ('size', {'size': 2}, True),
    ('prob', {'prob': .5}, True),
))
patterns('pig', lambda mean, shape: mean > 0 and shape == 0, (
    ('free', {}, False), ('shape-zero', {'shape': 0}, False), ('mean', {'mean': 1}, True),
))
add('dweibull', 'q', {'shape1': .5}, lambda shape1, shape2: shape2 > 0,
    loglik=-4*math.log(2))
add('skellam', 'free', {}, lambda mu1, mu2: mu1 == mu2 == 0, unique=True)
add('skellam', 'free', {}, lambda mu1, mu2: mu1 == 1 and mu2 == 0,
    x=1, unique=True, loglik=-4)

add('gompertz', 'a', {'a': 1}, lambda a, b: b > 0)
add('gpd', 'threshold-scale', {'mu': 0, 'sigma': 1}, lambda mu, sigma, xi: xi >= -1)
add('gpd', 'uniform-translation', {'sigma': 1, 'xi': -1},
    lambda mu, sigma, xi: -1 <= mu <= 0)
add('gpd', 'scale', {'sigma': 1},
    lambda mu, sigma, xi: (mu == 0 and xi >= -1) or (xi == -1 and -1 <= mu <= 0))
add('gev', 'location-scale', {'mu': 0, 'sigma': 1}, lambda mu, sigma, xi: -1 <= xi <= 1,
    loglik=-4)
add('sn', 'location-scale', {'xi': 0, 'omega': 1}, lambda xi, omega, alpha: True,
    loglik=-2*math.log(2*math.pi))
add('st', 'location-scale-df', {'xi': 0, 'omega': 1, 'nu': 1},
    lambda xi, omega, alpha, nu: True, loglik=-4*math.log(math.pi))
add('genbeta', 'power-product', {'shape2': 1, 'scale': 1},
    lambda shape1, shape2, shape3, scale:
    shape1 > 0 and shape3 > 0 and math.isclose(shape1*shape3, 1/math.log(2), rel_tol=1e-9),
    x=.5, loglik=4*math.log(2/(math.e*math.log(2))),
    witness_key=lambda shape1, **p: shape1)


for shape in (1, 2):
    width = 2/shape
    add('tlambda', 'uniform-%d' % shape, {'sigma': 2, 'lambda': shape},
        lambda mu, width=width, **p: -width <= mu <= width,
        loglik=-4*math.log(2*width))
add('tlambda', 'scale', {'sigma': 2},
    lambda mu, **p: p['lambda'] == 1 and -2 <= mu <= 2, loglik=-4*math.log(4))
add('tlambda', 'support-contacts', {'sigma': 3, 'lambda': 1.5},
    lambda mu, **p: mu in (-2, 2), loglik=-4*math.log(3))


def sged_at_held_mean():
    """The two reciprocal peaks for the centered, variance-one shape 9/4."""
    nu = 2.25
    g1, g2, g3 = (math.gamma(i/nu) for i in (1, 2, 3))
    a = 1 - g2*g2/(g1*g3)
    c = (g2/g1)**nu
    target = (4*a - 1)/(2*nu*c)
    lo, hi = .0008, .001
    for _ in range(53):
        u = (lo + hi)/2
        if u**(nu-2)*(2-u)*(1-u+a*u*u) < target:
            lo = u
        else:
            hi = u
    u = (lo + hi)/2
    xi = 1/math.sqrt(1-u)
    normalizer = math.log(nu/(2*g1*math.sqrt(g1/g3)))
    log_density = normalizer + .5*math.log1p(-u+a*u*u) - math.log1p(-u/2) - c*u**nu
    return xi, 4*log_density


_SGED_XI, _SGED_LL = sged_at_held_mean()
add('sged', 'reciprocal-skews', {'mean': 0, 'sd': 1, 'nu': 2.25},
    lambda mean, sd, nu, xi: xi > 0 and
    math.isclose(max(xi, 1/xi), _SGED_XI, rel_tol=1e-9), loglik=_SGED_LL,
    witness_key=lambda xi, **p: xi > 1)


def triang_member(a, b, c, points):
    return any(c == mode and math.isclose(a, lo, rel_tol=1e-9, abs_tol=1e-12)
               and math.isclose(b, hi, rel_tol=1e-9, abs_tol=1e-12)
               for lo, hi, mode in points)


add('triang', 'free', {}, lambda a, b, c: triang_member(a, b, c, ((0, 1.5, 0), (-.5, 1, 1))),
    data=(0., 1.), loglik=math.log(16/27), witness_key=lambda c, **p: c)
add('triang', 'held-endpoints', {'a': 0, 'b': 1}, lambda a, b, c: c in (.25, .75),
    data=(.25, .75), loglik=math.log(4/3), witness_key=lambda c, **p: c)

# Historical route IDs are the source-table keys, before the snapshot adds
# the public base name and modifier configuration to each record.
patterns('zib', lambda size, prob, pi: integer(size) and probability(prob) and probability(pi)
         and (size == 0 or prob == 0 or pi == 1), (
    ('free', {}, False), ('pi-one', {'pi': 1}, False), ('pi', {'pi': .5}, False),
    ('size-zero', {'size': 0}, False), ('prob-zero', {'prob': 0}, False),
    ('size', {'size': 3}, False), ('prob', {'prob': .5}, False),
    ('size-prob', {'size': 3, 'prob': .5}, True),
))
patterns('zinb', lambda size, prob, pi: size >= 0 and 0 < prob <= 1 and probability(pi)
         and (nb_zero(size, prob) or pi == 1), (
    ('free', {}, False), ('pi-one', {'pi': 1}, False), ('pi', {'pi': .5}, False),
    ('size-zero', {'size': 0}, False), ('prob-one', {'prob': 1}, False),
    ('size', {'size': 3}, False), ('prob', {'prob': .5}, False),
    ('size-prob', {'size': 3, 'prob': .5}, True),
))
patterns('zip', lambda **p: p['lambda'] > 0 and p['pi'] == 1, (
    ('free', {}, False), ('pi-one', {'pi': 1}, False), ('lambda', {'lambda': 1}, True),
))

HURDLES = {
    'zmpois': lambda **p: p['lambda'] >= 0,
    'zmgeom': lambda prob: 0 < prob <= 1,
    'zmlogarithmic': lambda prob: 0 <= prob < 1,
    'zmbinom': lambda size, prob: integer(size, 1) and probability(prob),
    'zmnbinom': lambda size, prob: size >= 0 and 0 < prob <= 1,
}
for family, base_domain in HURDLES.items():
    def hurdle_zero(p0, domain=base_domain, **base):
        return 0 < p0 <= 1 and domain(**base)

    add(family, 'free', {}, lambda p0, domain=base_domain, **base: p0 == 1 and domain(**base))
    add(family, 'p0-one', {'p0': 1}, hurdle_zero)
    add(family, 'p0-interior', {'p0': .5}, hurdle_zero, loglik=-4*math.log(2))

for family in ('ztbinom', 'zmbinom'):
    for label, held, unique in (
        ('one', {}, False), ('size-one', {'size': 1}, False),
        ('prob-zero', {'prob': 0}, False), ('prob-half', {'prob': .5}, True),
    ):
        if family == 'ztbinom':
            def one(size, prob):
                return integer(size, 1) and probability(prob) and (size == 1 or prob == 0)
        else:
            def one(size, prob, p0):
                return p0 == 0 and integer(size, 1) and probability(prob) and (size == 1 or prob == 0)
        add(family, label, held, one, x=1, unique=unique)

for family in ('ztnbinom', 'zmnbinom'):
    for label, held in (('one', {}), ('prob-one', {'prob': 1})):
        if family == 'ztnbinom':
            def one(size, prob):
                return size >= 0 and prob == 1
        else:
            def one(size, prob, p0):
                return p0 == 0 and size >= 0 and prob == 1
        add(family, label, held, one, x=1)

# The window bounds are inputs to the modifier seam and do not appear in
# its returned coefficient vectors. Both parent atoms miss these windows.
add('tbinom', 'singleton-window', {'a': 2, 'b': 3, 'size': 12},
    lambda size, prob: 0 < prob < 1, x=3)
add('tbinom', 'singleton-window-free-size', {'a': 2, 'b': 3},
    lambda size, prob: integer(size, 3) and (0 < prob < 1 or (size == 3 and prob == 1)), x=3)
add('tpois', 'singleton-window', {'a': 2, 'b': 3},
    lambda **p: p['lambda'] > 0, x=3)
add('zipf', 'singleton-window', {'N': 1}, lambda s: True, x=1)
add('tbinom', 'singleton-window', {'a': -1, 'b': 0, 'size': 12},
    lambda size, prob: 0 <= prob < 1)
add('tbinom', 'singleton-window-free-size', {'a': -1, 'b': 0},
    lambda size, prob: integer(size) and probability(prob) and (size == 0 or prob < 1))
add('tpois', 'singleton-window', {'a': -1, 'b': 0}, lambda **p: p['lambda'] >= 0)


# Typed pilot expectations. Their constants come from the sample and holds,
# independently of the fitter's returned representative or coefficients.
def axis(lower='-inf', upper='inf', closed=(False, False), integer=False):
    return dict(lower=lower, upper=upper, closed=list(closed), integer=integer)


def fixed_axis(value, integer=False):
    return axis(value, value, (True, True), integer)


def box(*axes, order=()):
    return dict(kind='box', axes=list(axes), order=[list(pair) for pair in order])


def points(*estimates):
    return dict(kind='points', points=[dict(estimate=list(x)) for x in estimates])


def typed_expected(family, case):
    held = case.held
    x = case.data[0]
    if family == 'binom':
        names = ('size', 'prob')
        if x == 0:
            pieces = [box(fixed_axis(0, True), axis(0, 1, (True, True))),
                      box(axis(0, 'inf', (True, False), True), fixed_axis(0))]
        elif 'prob' in held:
            upper = x / held['prob']
            pieces = [points((upper - 1, held['prob']), (upper, held['prob']))]
        else:
            pieces = [points((x, 1))]
    elif family == 'nhyper':
        names = ('m', 'N', 'r')
        count = axis(x, COUNT_MAX, (True, True), True)
        if all(t == x for t in case.data) and (x == 0 or case.loglik == 0):
            order = [(0, 1)] if x == 0 else [(0, 1), (1, 0)]
            pieces = [box(count, count, fixed_axis(x, True), order=order)]
        else:
            # The per-observation exact ratios at the crossing are one.
            if 'N' not in held:
                upper = held['m'] * x // held['r']
                pieces = [points((held['m'], upper - 1, held['r']), (held['m'], upper, held['r']))]
            elif 'm' not in held:
                upper = held['r'] * (held['N'] + 1) // x
                pieces = [points((upper - 1, held['N'], held['r']), (upper, held['N'], held['r']))]
            else:
                upper = 1 + x * held['m'] // (held['N'] + 1)
                pieces = [points((held['m'], held['N'], upper - 1), (held['m'], held['N'], upper))]
    elif family == 'gpd':
        names = ('mu', 'sigma', 'xi')
        sigma = held['sigma']
        pieces = [box(fixed_axis(x), fixed_axis(sigma), axis(-1, 'inf', (True, False))),
                  box(axis(x - sigma, x, (True, True)), fixed_axis(sigma), fixed_axis(-1))]
    elif family == 'cauchy':
        names = ('location', 'scale')
        lo, hi = min(case.data), max(case.data)
        radius, mid = (hi - lo) / 2, (hi + lo) / 2
        if 'scale' in held:
            from decimal import Decimal, localcontext
            with localcontext() as ctx:
                ctx.prec = 70
                delta = (Decimal(radius)**2 - Decimal(held['scale'])**2).sqrt()
                pieces = [points(*((float(Decimal(mid) + sign * delta), held['scale'])
                                   for sign in (-1, 1)))]
        else:
            pieces = [dict(kind='relation', form='semicircle', anchors=[lo, hi],
                           axes=[axis(lo, hi), axis(0)], driver=0, dependent=1)]
    else:
        return None
    return dict(version=1, coordinates=list(names), pieces=pieces)


def cauchy_flat(location, scale):
    return 1 < location < 5 and scale > 0 and math.isclose(
        scale**2, (location - 1) * (5 - location), rel_tol=1e-12)


_cauchy_ll = -2 * math.log(16 * math.pi**2)
add('cauchy', 'semicircle', {}, cauchy_flat, data=(1., 1., 5., 5.),
    loglik=_cauchy_ll, certified=True)
add('cauchy', 'reflected', {'scale': 1.}, lambda location, scale: (
    scale == 1 and math.isclose(abs(location - 3), math.sqrt(3), rel_tol=1e-10)),
    data=(1., 1., 5., 5.), loglik=_cauchy_ll, certified=True)
