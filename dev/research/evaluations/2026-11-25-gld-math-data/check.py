"""Fixed-grid checks for TASK-241. Run from the worktree root."""
import itertools
import json
import platform
from fractions import Fraction

import mpmath as mp


mp.mp.dps = 110


def power_term(shape, u):
    if shape == mp.inf:
        return mp.mpf(0)
    return mp.expm1(shape * mp.log(u)) / shape if shape else mp.log(u)


def quantile(a, b, u):
    return power_term(a, u) - power_term(b, 1 - u)


def derivatives(a, b, u):
    v = 1 - u
    left = (u ** (a - 1), (a - 1) * u ** (a - 2),
            (a - 1) * (a - 2) * u ** (a - 3)) if a != mp.inf else (0, 0, 0)
    right = (v ** (b - 1), -(b - 1) * v ** (b - 2),
             (b - 1) * (b - 2) * v ** (b - 3)) if b != mp.inf else (0, 0, 0)
    return tuple(x + y for x, y in zip(left, right))


def density(a, b, z):
    assert b != mp.inf and (a != mp.inf or b <= 1)
    lo, hi = mp.mpf(0), mp.mpf(1)
    support_lo = -1 / a if a > 0 else -mp.inf
    support_hi = 1 / b if b > 0 else mp.inf
    if z < support_lo or z > support_hi:
        return mp.mpf(0)
    if z == support_lo:
        return mp.mpf(0 if a < 1 else 1) if a != 1 else mp.mpf('0.5')
    if z == support_hi:
        if b < 1:
            return mp.mpf(0)
        return mp.mpf(1) / (1 + (b == 1 and a != mp.inf))
    for _ in range(355):
        u = (lo + hi) / 2
        if quantile(a, b, u) > z:
            hi = u
        else:
            lo = u
    assert quantile(a, b, lo) <= z <= quantile(a, b, hi)
    # Check that inversion accuracy is adequate for the reported densities.
    d_lo = derivatives(a, b, lo)[0]
    d_hi = derivatives(a, b, hi)[0]
    assert abs(d_lo / d_hi - 1) < mp.mpf('1e-65')
    return 1 / derivatives(a, b, (lo + hi) / 2)[0]


def loglik(sample, a, b, mu, sigma):
    return sum(mp.log(density(a, b, (x - mu) / sigma) / sigma) for x in sample)


def format_values(values):
    return [mp.nstr(value, 14) for value in values]


def envelope_and_curvature():
    shapes = list(map(mp.mpf, ['-1', '-0.5', '0', '0.5', '1', '2', '4', '16385']))
    tails = [mp.mpf(10) ** -k for k in (1, 3, 9, 24)]
    grid = sorted(set(tails + [1 - u for u in tails] +
                      [mp.mpf(k) / 20 for k in range(1, 20)]))
    largest = 0
    count = 0
    for a, b in itertools.product(shapes + [mp.inf], repeat=2):
        if min(a, b) > 1:
            continue
        for u in grid:
            d, dp, dpp = derivatives(a, b, u)
            assert d >= 1
            largest = max(largest, quantile(a, b, u) ** 2 / d)
            assert largest <= 9
            if max(a, b) <= 1:
                assert d * dpp >= mp.mpf('1.5') * dp ** 2
                if min(a, b) >= 0:
                    assert d * dpp >= 2 * dp ** 2
            if (a, b) in [(16385, mp.mpf('0.5')), (mp.inf, mp.mpf('0.5'))]:
                assert d * dpp > 2 * dp ** 2
            count += 1
    for a, b in [(mp.mpf('-0.01'), mp.mpf('0.5')),
                 (mp.mpf('1.01'), mp.mpf('0.5'))]:
        d, dp, dpp = derivatives(a, b, tails[-1])
        assert 2 * dp ** 2 > d * dpp
    return {'probabilities': [mp.nstr(u, 28) for u in grid], 'checks': count,
            'largest_q_squared_over_D': mp.nstr(largest, 20)}


def likelihood_checks():
    # Fixed-shape asymmetric collapse. Estimate only a difference quotient
    # of two fully specified path values, never an optimizer.
    scales = [mp.mpf(10) ** -k for k in (4, 8, 12)]
    results = {}
    for name, sample, a, b, mu, expected in [
        ('mixed_bounded_heavy', [0, 1, 2], 1, -4, lambda s: s, -mp.mpf('0.5')),
        ('asymmetric_two_heavy', [-2, 0, 0, 3], -2, -4, lambda s: 0, -mp.mpf('1.25')),
    ]:
        likelihoods = [loglik(sample, a, b, mu(s), s) for s in scales]
        slope = (likelihoods[-1] - likelihoods[-2]) / mp.log(scales[-1] / scales[-2])
        assert abs(slope - expected) < mp.mpf('1e-5')
        results[name] = {'expected': str(expected), 'last_secant': mp.nstr(slope, 15)}

    eps = mp.mpf(1) / 10000
    rational_eps = Fraction(1, 10000)
    assert Fraction(12, 410) ** 2 > 1 / (1296 * (1 - rational_eps ** 2) ** 2)
    sample = [-1 - eps, -1 + eps, 1 - eps, 1 + eps]
    centered_upper = 1 / (mp.mpf(1296) * (1 - eps ** 2) ** 2)
    off_center = mp.exp(loglik(sample, -1, -1, 1, mp.mpf('0.01')))
    assert off_center > (mp.mpf(12) / 410) ** 2 > centered_upper
    results['nonunique_fixed_shape'] = format_values([centered_upper, off_center])

    face = mp.mpf(1) / 36
    ratios = [mp.exp(loglik([0, 0, 1, 3], 1, -1, s, s)) / face for s in scales]
    assert 0 < ratios[0] < ratios[1] < ratios[2] < 1
    assert 1 - ratios[2] < mp.mpf('1e-20')
    results['extreme_half_tie_relative_deficit'] = format_values([1 - r for r in ratios])

    # Reflected paths follow from q_ab(u) = -q_ba(1-u).
    sample = [mp.mpf(1) / 8, mp.mpf(1) / 4, mp.mpf(1) / 2]
    b = mp.mpf(2)
    spikes = []
    for a in (100, 400, 1600, 6400):
        u = 1 - 1 / mp.sqrt(a)
        mu = sample[-1] - quantile(a, b, u)
        spikes.append(loglik(sample, a, b, mu, mp.mpf(1)))
    assert all(x < y for x, y in zip(spikes, spikes[1:]))
    assert abs((spikes[-1] - spikes[-2]) / mp.log(4) - mp.mpf('0.5')) < mp.mpf('0.03')
    results['mixed_spike_loglik'] = format_values(spikes)

    sample = [-1, 0, 2]
    spikes = [loglik(sample, a, a, 0, mp.mpf(4 * a)) for a in (12, 24, 48, 96)]
    assert spikes == sorted(spikes) and spikes[-1] > 40
    results['diagonal_spike_loglik'] = format_values(spikes)

    for b in map(mp.mpf, ['-1', '0', '0.5', '1']):
        sample = [mp.mpf('0'), mp.mpf('0.2'), mp.mpf('0.5')]
        face = [density(mp.inf, b, x) for x in sample]
        errors = [max(abs(density(a, b, x) - y) for x, y in zip(sample, face))
                  for a in (10, 20, 40)]
        assert errors[2] < errors[1] < errors[0]
        assert errors[2] < mp.mpf('0.06')
        results[f'GPD_b={b}_max_error'] = format_values(errors)
    assert density(40, 1, 1) == mp.mpf('0.5') < density(mp.inf, 1, 1) == 1
    expanded_errors = []
    for a in (100, 400, 1600):
        scale = 1 + 1 / mp.sqrt(a)
        expanded_errors.append(max(abs(density(a, 1, x / scale) / scale - 1)
                                   for x in [mp.mpf(0), mp.mpf('0.5'), mp.mpf(1)]))
    assert expanded_errors[2] < expanded_errors[1] < expanded_errors[0]
    assert expanded_errors[2] < mp.mpf('0.025')
    results['GPD_uniform_expanded_support_max_error'] = format_values(expanded_errors)
    return results


def information_checks():
    # Location information in consecutive log-probability bands of width 1.
    # A nonzero log-mass slope is the negative of the listed u exponent.
    rows = {}
    for a, exponent in [('0.25', '0.5'), ('0.5', '0'), ('0.75', '-0.5'),
                        ('1', '1'), ('1.25', '-0.5'), ('1.5', '0'), ('1.75', '0.5')]:
        shape = mp.mpf(a)
        b = mp.mpf('0.3')

        def integrand(t):
            u = mp.exp(-t)
            d, dp, _ = derivatives(shape, b, u)
            return u * (dp / d ** 2) ** 2

        bands = [mp.quad(integrand, [k, k + 1]) for k in (30, 50, 70)]
        secant = mp.log(bands[-1] / bands[-2]) / 20
        assert abs(secant + mp.mpf(exponent)) < mp.mpf('1e-5')
        rows[a] = {'bands_30_50_70': format_values(bands),
                   'log_mass_slope': mp.nstr(secant, 15)}
    return rows


if __name__ == '__main__':
    print(json.dumps({
        'python': platform.python_version(), 'mpmath': mp.__version__,
        'decimal_digits': mp.mp.dps,
        'envelope_and_curvature': envelope_and_curvature(),
        'likelihood': likelihood_checks(),
        'location_information': information_checks(),
    }, indent=2))
