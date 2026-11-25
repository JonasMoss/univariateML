"""Check archived public coefficients and replay the binary64 rate formulas."""
from collections import Counter
from fractions import Fraction
import gzip
import json
import math
from pathlib import Path

import mpmath as mp


EVALUATIONS = Path(__file__).resolve().parents[1]
SNAPSHOTS = EVALUATIONS / '2026-11-15-fit-snapshot-data'
GOLDEN = (EVALUATIONS.parents[2] / 'univariateML-cpp/include/cppunivariate'
          / 'distributions/dblpareto/golden.json')
TOL = 1e-10


def deviations(data, kink):
    left = mp.fsum(mp.log(kink / v) for v in data if v < kink) / len(data)
    right = mp.fsum(mp.log(v / kink) for v in data if v > kink) / len(data)
    return left, right


def free_rate(t, h):
    if not t:
        return mp.inf
    # Solve the quadratic with the conjugate root in rate coordinates.
    return 2 * h / (h * t + mp.sqrt((h * t)**2 + 4 * h * t))


def coefficients(data, kink, left=None, right=None):
    l, r = deviations(data, kink)
    if left is None and right is None:
        if not l and not r:
            return mp.inf, mp.inf
        return (1 / (l + mp.sqrt(l * r)) if l else mp.inf,
                1 / (r + mp.sqrt(l * r)) if r else mp.inf)
    return (free_rate(l, right) if left is None else left,
            free_rate(r, left) if right is None else right)


def loglik(data, kink, left, right):
    if mp.isinf(left) and mp.isinf(right):
        return mp.inf
    if mp.isinf(left) and any(v < kink for v in data):
        return -mp.inf
    if mp.isinf(right) and any(v > kink for v in data):
        return -mp.inf
    normalizer = 1 / (1 / left + 1 / right)
    return mp.fsum(mp.log(normalizer / v) -
                   (left * mp.log(kink / v) if v < kink else
                    right * mp.log(v / kink) if v > kink else 0)
                   for v in data)


def relative(value, reference):
    return abs(mp.mpf(value) - reference) / abs(reference)


def check_snapshot(mode, golden):
    counts = Counter()
    largest = (mp.mpf(0), '', '')
    likelihood_err = mp.mpf(0)
    with gzip.open(SNAPSHOTS / f'{mode}.jsonl.gz', 'rt') as stream:
        for line in stream:
            row = json.loads(line)
            if row['family'] != 'dblpareto':
                continue
            assert not row['held'], row['case']
            case = row['case']
            result = row['result']
            kind = result['kind']
            counts[kind] += 1
            if kind not in ('ok', 'limit'):
                continue
            data = list(map(mp.mpf, row.get('data', golden)))
            assert len(data) == row['n']
            if kind == 'ok':
                p = {k: float.fromhex(v) for k, v in result['estimates'].items()}
                b, left, right = (p[k] for k in ('scale', 'left_rate', 'right_rate'))
                recorded_ll = float.fromhex(result['logLik'])
            else:
                p = {k: float.fromhex(v) for k, v in result['limit_estimates'].items()}
                if result['model'] == 'pareto':
                    b, left, right = p['b'], math.inf, p['a']
                else:
                    assert result['model'] == 'power'
                    b, left, right = p['alpha'], p['beta'], math.inf
                recorded_ll = float.fromhex(result['limit_loglik'])
            b, left, right = map(mp.mpf, (b, left, right))
            assert b in data
            references = {k: coefficients(data, k) for k in set(data)}
            reference_lls = {k: loglik(data, k, *rates) for k, rates in references.items()}
            exact_at_kink = reference_lls[b]
            best = max(reference_lls.values())
            gap = best - exact_at_kink
            if result['certified']:
                assert gap < mp.mpf('1e-85'), (case, gap)
            elif gap >= mp.mpf('1e-85'):
                print(mode, case, 'uncertified ranking loss', mp.nstr(gap, 10))
            for name, value, reference in zip(('left_rate', 'right_rate'),
                                              (left, right), references[b]):
                if mp.isinf(value):
                    assert mp.isinf(reference)
                    continue
                error = relative(value, reference)
                assert error < TOL, (mode, case, name, error)
                largest = max(largest, (error, case, name))
            error = abs(recorded_ll - loglik(data, b, left, right)) / max(1, abs(exact_at_kink))
            assert error < TOL, (mode, case, error)
            likelihood_err = max(likelihood_err, error)
    assert sum(counts.values()) == 28, counts
    print(mode, dict(counts), 'max relative rate error', mp.nstr(largest[0], 12),
          largest[1:], 'max scaled public logLik error', mp.nstr(likelihood_err, 12))


def log_ratio(x, b):
    delta = (x - b) / b
    if abs(delta) < .5:
        return math.log1p(delta)
    ratio = x / b
    if math.isfinite(ratio) and ratio >= float.fromhex('0x1p-1022'):
        return math.log(ratio)
    return math.log(x) - math.log(b)


def rounded_distance(data, kink, adjacent):
    if not adjacent:
        return (math.fsum(log_ratio(kink, v) for v in data if v < kink) / len(data),
                math.fsum(log_ratio(v, kink) for v in data if v > kink) / len(data))
    x = sorted(data)
    gaps = [(a, b, i, log_ratio(b, a))
            for i, (a, b) in enumerate(zip(x, x[1:]), 1)]
    return (math.fsum(i / len(x) * gap for a, b, i, gap in gaps if b <= kink),
            math.fsum((len(x) - i) / len(x) * gap for a, b, i, gap in gaps if a >= kink))


def rounded_rates(data, kink, left, right, adjacent):
    l, r = rounded_distance(data, kink, adjacent)
    if left is None and right is None:
        s = math.sqrt(l) + math.sqrt(r)
        return tuple(1 / (math.sqrt(v) * s) if v else math.inf for v in (l, r))
    t, h = (l, right) if left is None else (r, left)
    inverse = t / 2 + math.hypot(t / 2, math.sqrt(t) / math.sqrt(h))
    rate = 1 / inverse if inverse else math.inf
    return (rate, right) if left is None else (left, rate)


def check_rounding_grid():
    smallest = math.ulp(0.0)
    widest = float.fromhex('0x1.fffffffffffffp+1023')
    data_sets = [
        [0.0625, 0.5, 1.0, 1.0, 1.125, 32.0],
        [smallest, 2 * smallest, 3 * smallest, 9 * smallest],
        [smallest, 1.0, 64.0, widest],
        [2.0**-10, 0.5, 0.5, 2.0, 2.0, 2.0**10],
        [0.5, 1.0, 1.0, 1.0, 1.0, 16.0],
    ]
    for b in (1e-300, 1.0, 1e300):
        data_sets.append([math.nextafter(b, 0), b, b, math.nextafter(b, math.inf)])

    errors = []
    holds = (smallest, 1e-300, .125, 1.0, 16.0, 1e300, widest)
    grid = [(data, sorted(set(data)), (False, True)) for data in data_sets]
    grid.append(([1.0, 3.0, 9.0], [0.0625, 2.0, 16.0], (False,)))
    for data, kinks, routes in grid:
        reference_data = list(map(mp.mpf, data))
        for kink in kinks:
            for adjacent in routes:
                for left, right in ([(None, None)] + [(h, None) for h in holds] +
                                    [(None, h) for h in holds]):
                    ref_left, ref_right = (None if v is None else mp.mpf(v)
                                           for v in (left, right))
                    expected = coefficients(reference_data, mp.mpf(kink), ref_left, ref_right)
                    public = rounded_rates(data, kink, left, right, adjacent)
                    for fixed, value, reference in zip((left, right), public, expected):
                        if fixed is not None:
                            assert Fraction(value) == Fraction(fixed)
                        elif mp.isinf(reference):
                            assert math.isinf(value)
                        else:
                            error = relative(value, reference)
                            assert error < TOL, (data, kink, left, right, error)
                            errors.append(error)
    print('binary64 formula grid:', len(errors), 'free finite rate checks, max relative error',
          mp.nstr(max(errors), 12))


if __name__ == '__main__':
    with mp.workdps(100):
        golden = json.loads(GOLDEN.read_text())['data']
        for mode in ('native', 'double'):
            check_snapshot(mode, golden)
        check_rounding_grid()
