"""Replay the named archive cases against independent stationary solves and ratios."""
import argparse
from fractions import Fraction
import json
import math
from pathlib import Path
import sys

import mpmath as mp

sys.path.insert(0, 'dev/validation')
from strict_common import archive_data, load_one, number

mp.mp.dps = 80


def continuous(x, k, start):
    logs = [mp.log(mp.mpf(v)) for v in x]
    mean = mp.fsum(logs) / len(logs)
    w = [l - mean for l in logs]
    width = max(w) - min(w)

    def score(log_power, intercept):
        power = mp.exp(log_power) / width
        residuals = [1 - (k+1) / (1 + mp.exp(intercept - power*v)) for v in w]
        return mp.fsum(residuals), len(x) + power * mp.fdot(w, residuals)

    l, b = mp.findroot(score, (mp.log(start*width), mp.log(k)), tol=mp.mpf('1e-75'))
    power = mp.exp(l) / width
    return power, mp.exp(mean + b/power)


def population_ratio(x, n, m, r):
    # Exact fraction from exact_nhyper.py in the refinement-cause study.
    a = n + 1
    return math.prod(Fraction((a-m)*(a-v), a*(a-m-v+r)) for v in x)


def population_peak(x, m, r):
    lo = m + max(x) - r
    hi = max(lo, m * max(x) // r)
    while lo < hi:
        mid = (lo + hi) // 2
        if population_ratio(x, mid, m, r) > 1:
            lo = mid + 1
        else:
            hi = mid
    return lo, m, r


def log_likelihood(x, n, m, r):
    def choose(n, k):
        return mp.loggamma(n+1) - mp.loggamma(k+1) - mp.loggamma(n-k+1)

    return mp.fsum(choose(v-1, r-1) + choose(n-v, m-r) - choose(n, m) for v in x)


def growing_urn_face(x, r):
    p = mp.mpf(r) * len(x) / sum(x)
    return mp.fsum(mp.loggamma(v) - mp.loggamma(r) - mp.loggamma(v-r+1)
                   + r*mp.log(p) + (v-r)*mp.log1p(-p) for v in x)


def best_boundary_window(x):
    """Exhaust N=max(x)..max(x)+2*range(x), profiling r on each m-r line."""
    best = (-mp.inf, None)
    for n in range(max(x), max(x) + 2*(max(x)-min(x)) + 1):
        for k in range(n-max(x)+1):
            lo, hi = 1, min(x)
            while lo < hi:
                t = (lo + hi) // 2
                ratio = math.prod(Fraction((v-t)*(t+k+1), t*(n-t-k)) for v in x)
                if ratio > 1:
                    lo = t + 1
                else:
                    hi = t
            point = (n, lo+k, lo)
            value = log_likelihood(x, *point)
            if value > best[0]:
                best = value, point
    return best


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('snapshot', type=Path)
    opts = parser.parse_args()
    rows = load_one(opts.snapshot)
    archive = archive_data()
    continuous_cases = [('burr', 638), ('burr', 639), ('pareto3', 5590),
                        ('pareto3', 5591), ('pareto3', 5593)]
    for family, case in continuous_cases:
        row = rows[family, 'archive', f'{case:06}']
        result = row['result']
        assert result['kind'] == 'ok'
        shape = 'shape2' if family == 'burr' else 'shape'
        estimates = {k: mp.mpf(number(v)) for k, v in result['estimates'].items()}
        k = mp.mpf(row['held'].get('shape1', 1))
        star = dict(zip((shape, 'scale'), continuous(archive[case], k, estimates[shape])))
        errors = {k: float(estimates[k]/ref - 1) for k, ref in star.items()}
        assert max(map(abs, errors.values())) < 1e-10
        print(json.dumps(dict(family=family, case=case,
                              reference={k: mp.nstr(v, 45) for k, v in star.items()},
                              relative_error=errors)))

    x = archive[5161]
    assert all(x == archive[case] for case in (5175, 5217, 5230))
    profile = [population_peak(x, 20, r) for r in range(1, 21)]
    for n, m, r in profile:
        assert population_ratio(x, n, m, r) <= 1
        assert n == m + max(x) - r or population_ratio(x, n-1, m, r) > 1
    expected = {5175: max(profile, key=lambda p: log_likelihood(x, *p)),
                5217: (max(x), 3, 3), 5230: profile[2]}
    window_ll, window_point = best_boundary_window(x)
    print(json.dumps(dict(boundary_window_best=window_point, loglik=float(window_ll))))
    ll = {}
    for case in (5161, 5175, 5217, 5230):
        result = rows['nhyper', 'archive', f'{case:06}']['result']
        assert result['kind'] == 'ok'
        point = tuple(int(number(result['estimates'][k])) for k in ('N', 'm', 'r'))
        ll[case] = log_likelihood(x, *point)
        assert abs(number(result['logLik']) - ll[case]) < mp.mpf('1e-10')
        if case == 5161:
            assert ll[case] >= window_ll
            n, m, r = point
            assert population_ratio(x, n-1, m, r) > 1
            assert population_ratio(x, n, m, r) <= 1
            neighbor_gap = {}
            for dn, dm, dr in ((-1,0,0), (1,0,0), (0,-1,0),
                               (0,1,0), (0,0,-1), (0,0,1)):
                gap = ll[case] - log_likelihood(x, n+dn, m+dm, r+dr)
                neighbor_gap[str((dn, dm, dr))] = float(gap)
            face_r = max(range(min(x)-2*(max(x)-min(x)), min(x)+1),
                         key=lambda rr: growing_urn_face(x, rr))
            face = growing_urn_face(x, face_r)
            assert face - ll[case] < 1e-9 * (1 + abs(ll[case]))
            print(json.dumps(dict(case=case, adjacent_improvements={
                k: -v for k, v in neighbor_gap.items()}, face_r=face_r,
                face_advantage=float(face - ll[case]))))
        if case in expected:
            assert point == expected[case], (case, point, expected[case])
        print(json.dumps(dict(family='nhyper', case=case, result=point,
                              certified=result['certified'], exact_input_loglik=mp.nstr(ll[case], 45))))
    assert ll[5161] >= max(ll[5175], ll[5217]) > ll[5230]
    assert ll[5175] > ll[5230] and ll[5217] > ll[5230]


if __name__ == '__main__':
    main()
