"""Math-only verification. Run from any directory."""
import csv
import itertools
import json
import math
import re
from fractions import Fraction
from pathlib import Path

import mpmath as mp


EVALUATIONS = Path(__file__).resolve().parents[1]
ROOT = EVALUATIONS.parents[2]
DOSSIER = ROOT / 'univariateML-cpp/include/cppunivariate/distributions/nsbeta/README.md'


def check_masks():
    endpoints = {'min', 'max'}
    shapes = {'shape1', 'shape2'}
    coordinates = sorted(endpoints | shapes)
    all_masks = {
        frozenset(mask)
        for size in range(5)
        for mask in itertools.combinations(coordinates, size)
    }
    declared = {
        frozenset(filter(None, line.split(', ')))
        for line in re.findall(r'^- held: \[(.*)\]$', DOSSIER.read_text(), re.M)
    }
    assert len(declared) == 12
    assert declared == {mask for mask in all_masks if not endpoints <= mask}
    prior_path = EVALUATIONS / '2026-11-25-thresh-math-data/masks.csv'
    with prior_path.open() as source:
        prior = {
            frozenset(row[1].split('+'))
            for row in csv.reader(source)
            if row[0] == 'nsbeta'
        }
    assert prior == declared - {frozenset()}
    print('masks: 16 total, 4 implemented, 12 declared exclusions, 11 prior partial-hold rows')


def number(value):
    if isinstance(value, float):
        numerator, denominator = value.as_integer_ratio()
        return mp.mpf(numerator) / denominator
    return mp.mpf(value)


def archive():
    source = EVALUATIONS / '2026-11-09-audit-beta-data'
    references = {
        case['index']: case for case in json.loads((source / 'reference.json').read_text())
        if case['family'] == 'nsbeta'
    }
    with (source / 'after.csv').open() as data:
        output = {
            int(row[2]): tuple(float(entry.split('=')[1]) for entry in row[9:13])
            for row in csv.reader(data)
            if row[0] == 'nsbeta' and int(row[2]) in references and row[3] == '0'
        }
    assert len(references) == len(output) == 5
    errors = {}
    for index, case in sorted(references.items()):
        held = {key: number(value) for key, value in case['held'].items()}
        lower, upper = held['min'], held['max']
        free_lower = 'shape1' not in held
        other = held['shape2' if free_lower else 'shape1']
        x = [number(value) for value in case['data']]
        distances = [value - lower if free_lower else upper - value for value in x]
        opposite = [upper - value if free_lower else value - lower for value in x]
        # Directly form the small complement when log(y) is tiny.
        width = upper - lower
        target = -mp.fsum(
            mp.log1p(-right / width) if right < left else mp.log(left / width)
            for left, right in zip(distances, opposite)
        ) / len(x)
        guess = number(case['theta'][0 if free_lower else 1])
        def score(log_ratio):
            shape = guess * mp.exp(log_ratio)
            return (mp.digamma(shape + other) - mp.digamma(shape)) / target - 1
        solved = guess * mp.exp(mp.findroot(score, (mp.mpf('-.001'), mp.mpf('.001')), tol=mp.mpf('1e-120')))
        assert abs(solved / guess - 1) < mp.mpf('1e-90')
        fit = number(output[index][0 if free_lower else 1])
        errors[index] = abs(fit / solved - 1)
        assert errors[index] < mp.mpf('1e-10')
    return errors


def transformed_case():
    lower = -float(2**50)
    upper = 1.
    x = [0., .125]
    y = [(value - lower) / (upper - lower) for value in x]
    assert y == [1 - 2.**-50] * 2
    raw = [(number(v) - number(lower)) / (number(upper) - number(lower)) for v in x]
    assert raw[0] != raw[1]
    exact_shape = -1 / (mp.fsum(map(mp.log, raw)) / len(raw))
    delegate_shape = -1 / mp.log(number(y[0]))
    error = abs(delegate_shape / exact_shape - 1)
    assert mp.mpf('.0624') < error < mp.mpf('.0626')

    # Bisection as in beta/mle.hpp, with the exact h=1 score reduced to
    # reciprocal and rounded to binary64. This models the delegate target.
    target = math.log(y[0])
    lo, hi = 0., -2 / target
    for iterations in range(100):
        public = lo + (hi - lo) / 2
        if 1 / public + target > 0:
            lo = public
        else:
            hi = public
        if hi - lo <= 1.e-13 * public:
            break
    else:
        raise AssertionError('source-level bisection did not converge')
    assert abs(number(public) / delegate_shape - 1) < mp.mpf('1e-13')
    assert abs(number(public) / exact_shape - 1) > mp.mpf('.0624')
    return exact_shape, delegate_shape, error, public


def width_cases():
    distances = list(map(mp.mpf, (1, 2, 5, 5)))
    a = mp.mpf('2.5')
    geometric_mean = mp.exp(mp.fsum(map(mp.log, distances)) / len(distances))
    def equations(width, shape):
        h = mp.log(width / geometric_mean)
        ratios = mp.fsum(z / (width - z) for z in distances) / len(distances)
        return (a - 1) * ratios - shape, mp.digamma(shape + a) - mp.digamma(shape) - h

    width, shape = mp.findroot(equations, (7, 2))
    public_width = number(float(width))
    public_shape = number(float(shape))
    # Reflection gives the same coefficients up to endpoint sign for held 0.
    assert abs(public_width / width - 1) < mp.mpf('1e-15')
    assert abs(public_shape / shape - 1) < mp.mpf('1e-15')
    restored_shape = mp.findroot(
        lambda value: equations(public_width, value)[1], public_shape)
    assert abs(restored_shape / shape - 1) < mp.mpf('1e-15')

    # A uniform contact is exact in original data and can be lost in W.
    fixed_lower = -float(2**54)
    exact_upper = 2.
    assert float(fixed_lower + (exact_upper - fixed_lower)) == 0.

    # Exact W*=1+2^-55; the closest feasible positive-density endpoint is
    # nextafter(1, +infinity), at eight times the optimal endpoint gap.
    b = 2**55
    optimal = mp.mpf(1) + 1 / mp.mpf(b)
    best_public = number(math.nextafter(1., math.inf))
    assert Fraction.from_float(math.nextafter(1., math.inf)) - 1 == Fraction(1, 2**52)
    loss = (b + 1) * mp.log(best_public / optimal) - mp.log(8)
    assert mp.mpf('4.92') < loss < mp.mpf('4.93')
    assert abs(best_public / optimal - 1) < mp.mpf('1e-10')
    return width, shape, loss


def conditional_refit():
    spacing = mp.mpf(2)**-52
    maximum = 1 + spacing
    values = (mp.mpf(1), maximum)

    def score(gap, scaled_shape):
        width = maximum + spacing * gap
        h = mp.fsum(mp.log1p((width - z) / z) for z in values) / 2
        return 1 / scaled_shape + 1 / (scaled_shape + spacing) - h / spacing

    def width_score(gap, scaled_shape):
        width = maximum + spacing * gap
        return mp.fsum(spacing * z / (width - z) for z in values) / 2 - scaled_shape

    gap, shape = mp.findroot(
        (width_score, score), (mp.mpf('.2'), mp.mpf('2.8')))
    width = maximum + spacing * gap
    assert number(float(width)) == maximum
    safe_width = number(math.nextafter(float(maximum), math.inf))
    assert safe_width == maximum + spacing
    refit = mp.findroot(lambda value: score(1, value), (1, 2))
    error = abs(refit / shape - 1)
    assert mp.mpf('.528') < error < mp.mpf('.529')
    return gap, shape / spacing, refit / spacing, error


def limits():
    # Mixed width/shape tail: check ell_profile + log h -> constant.
    distances = [mp.mpf(1), mp.mpf(2), mp.mpf(5), mp.mpf(5)]
    a = mp.mpf('2.5')
    mean_log_z = mp.fsum(map(mp.log, distances)) / len(distances)
    for h in map(mp.mpf, (10, 100, 1000)):
        width = mp.exp(h + mean_log_z)
        b = mp.findroot(
            lambda v: mp.digamma(mp.exp(v) + a) - mp.digamma(mp.exp(v)) - h,
            -mp.log(h))
        b = mp.exp(b)
        profile = -mp.log(mp.beta(a, b)) - b * h - mean_log_z + (
            (a - 1) * mp.fsum(mp.log1p(-z / width) for z in distances) / len(distances))
        assert abs(profile + mp.log(h) + 1 + mean_log_z) < 3 / h

    # Spike and mixed constant collapse: check scaled endpoint densities.
    for epsilon in (mp.mpf('1e-5'), mp.mpf('1e-10'), mp.mpf('1e-20')):
        a = mp.mpf('2.5')
        b = 1 / epsilon
        width = 1 + epsilon
        v = 1 / width
        collapse = v**(b - 1) * (1 - v)**(a - 1) / (width * mp.beta(b, a))
        assert abs(epsilon * collapse * mp.e * mp.gamma(a) - 1) < 10 * epsilon

        width = 1 + epsilon
        # a=1/2 at the moving endpoint, b=1 at the held endpoint.
        value = mp.mpf('.5') / mp.sqrt(width * epsilon)
        assert abs(mp.sqrt(epsilon) * value - mp.mpf('.5')) < epsilon


if __name__ == '__main__':
    check_masks()
    comparisons = []
    for digits in (350, 450):
        with mp.workdps(digits):
            historical = archive()
            transformed = transformed_case()
            endpoint = width_cases()
            refit = conditional_refit()
            limits()
            results = list(historical.values()) + list(transformed) + list(endpoint) + list(refit)
            comparisons.append([mp.nstr(result, 50) for result in results])
            print(f'{digits} digits')
            for index, error in historical.items():
                print(f'  archived public result {index}: relative error {mp.nstr(error, 8)}')
            for label, result in zip(
                ('physical shape', 'rounded-input shape', 'rounded-input relative error',
                 'source-level public shape', 'mixed width', 'mixed held-side shape',
                 'best representable endpoint likelihood loss', 'mixed near-contact gap in ulps',
                 'joint free shape', 'safe-width refit shape', 'safe-width refit relative error'),
                transformed + endpoint + refit
            ):
                print(f'  {label}: {mp.nstr(result, 22)}')
    assert comparisons[0] == comparisons[1]
