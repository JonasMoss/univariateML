"""Likelihood profiles and applicability checks used by fit-refine.py."""
import math
from collections import Counter

import mpmath as mp


def profile_result(values, hold, checked, reason, method, loglik):
    if not checked:
        return {'error': 'no independently refinable coefficient: ' + '; '.join(dict.fromkeys(reason.values())),
                'not_refined': {**reason, **dict.fromkeys(hold, 'held')}}
    return {
        'estimates': {k: mp.nstr(mp.mpf(checked.get(k, v)), 25) for k, v in values.items()},
        'not_refined': {k: ('held' if k in hold else 'closed form or profile (independently checked)'
                            if k in checked else reason[k]) for k in values},
        'logLik': mp.nstr(loglik, 25),
        'method': method,
    }


def triang_endpoint(data, values, hold):
    """Optimize the ends conditional on the reported mode, on its endpoint face."""
    lower = values['a'] == values['c']
    near, far = ('a', 'b') if lower else ('b', 'a')
    direction = 1 if lower else -1
    with mp.workdps(750):
        mode = mp.mpf(values['c'])
        distances = [direction * (mp.mpf(x) - mode) for x in data]
        if min(distances) < 0:
            return {'error': 'reported endpoint mode excludes an observation'}
        checked = {}
        if near not in hold:
            # Moving this endpoint away from c increases the width and does
            # not change any of the occupied branch factors.
            checked[near] = mode
        width = direction * (mp.mpf(values[far]) - mode)
        radius = max(distances)
        if far in hold and width <= radius:
            return {'error': 'held far endpoint has zero or infeasible likelihood'}
        def loglik(w):
            return len(data)*mp.log(2/w) + mp.fsum(mp.log1p(-d/w) for d in distances)

        if far not in hold:
            if radius == 0:
                return {'error': 'endpoint profile has no finite optimum (zero radius)'}
            scaled = [d / radius for d in distances]
            lo, hi = mp.mpf(0), mp.mpf(1)
            for _ in range(2200):
                u = (lo + hi) / 2
                score = len(data) / u - mp.fsum(d / (1 - u*d) for d in scaled)
                if score > 0:
                    lo = u
                else:
                    hi = u
                if lo == 0 or hi == 1:
                    continue
                left, right = sorted(float(mode + direction*radius/v) for v in (lo, hi))
                if left == right or math.nextafter(left, math.inf) == right:
                    break
            else:
                return {'error': 'reciprocal-width profile could not isolate adjacent public endpoints'}
            # Concavity in reciprocal width restricts the public optimum
            # to the binary64 neighbors of the continuous root.
            candidates = {left, right, math.nextafter(left, -math.inf), math.nextafter(right, math.inf)}
            feasible = [(end, direction*(mp.mpf(end)-mode)) for end in candidates if math.isfinite(end)]
            feasible = [(end, w) for end, w in feasible if w > radius]
            if not feasible:
                return {'error': 'endpoint profile has no feasible finite public endpoint'}
            checked[far], width = max(feasible, key=lambda pair: loglik(pair[1]))
        ll = loglik(width)
        return profile_result(values, hold, checked, {'c': 'mode held at reported kink for endpoint profile'},
                              'strictly concave reciprocal-width profile, conditional on mode', ll)


def zmbinom_profile(data, values, hold):
    """Separate the zero indicator, then solve the positive mean at fixed size."""
    with mp.workdps(80):
        n = int(values['size'])
        prob, p0 = (mp.mpf(values[k]) for k in ('prob', 'p0'))
        positive = [x for x in data if x]
        nz = len(data) - len(positive)
        checked, reason = {}, {}
        if 'p0' not in hold:
            checked['p0'] = p0 = mp.mpf(nz) / len(data)
        if not positive:
            reason.update(prob='probability unidentified without positives',
                          size='size unidentified without positives')
        else:
            mean = mp.mpf(sum(positive)) / len(positive)
            # A constant positive count >1 attains the saturated likelihood
            # only at that size with prob=1 (or size is forced if prob=1 held).
            if min(positive) == max(positive) and (('prob' not in hold and mean > 1)
                                                   or ('prob' in hold and prob == 1)):
                if 'size' not in hold:
                    checked['size'] = n = int(mean)
            else:
                reason['size'] = 'free integer size: no global positive-count tail envelope'
            if max(positive) > n:
                return {'error': 'conditional probability profile requires size at least the largest count'}
            if 'prob' not in hold:
                if n == 1:
                    reason['prob'] = 'probability unidentified at size one'
                elif mean == 1 or mean == n:
                    checked['prob'] = prob = mp.mpf(int(mean == n))
                else:
                    # E[X | X>0] = n*p/(1-(1-p)^n), strictly increasing.
                    # Starting with [0, 1] gives a final bracket of width 2^-220.
                    lo, hi = mp.mpf(0), mp.mpf(1)
                    for _ in range(220):
                        p = (lo + hi)/2
                        if n*p / (-mp.expm1(n*mp.log1p(-p))) < mean:
                            lo = p
                        else:
                            hi = p
                    checked['prob'] = prob = (lo + hi)/2
        if (nz and p0 == 0) or (positive and p0 == 1):
            return {'error': 'held zero probability excludes an observed zero indicator'}
        ll = (nz*mp.log(p0) if nz else 0) + (len(positive)*mp.log1p(-p0) if positive else 0)
        if positive and n > 1:
            if prob == 0 or prob == 1:
                if any(x != (n if prob == 1 else 1) for x in positive):
                    return {'error': 'held probability endpoint excludes a positive count'}
            else:
                norm = mp.log(-mp.expm1(n*mp.log1p(-prob)))
                ll += mp.fsum(c*(mp.loggamma(n+1)-mp.loggamma(x+1)-mp.loggamma(n-x+1)
                                 + x*mp.log(prob)+(n-x)*mp.log1p(-prob)-norm)
                              for x, c in Counter(positive).items())
        return profile_result(values, hold, checked, reason,
                              'Bernoulli zero fraction and monotone positive mean at fixed size', ll)


def nhyper_composition(data, values, hold):
    """Exact adjacent composition ratio, conditional on N and r."""
    N, r, fitted = (int(values[k]) for k in ('N', 'r', 'm'))
    reason = {'N': 'population conditioned on reported value', 'r': 'stopping count conditioned on reported value'}
    if r == 0:
        return {'error': 'composition unidentified when stopping count is zero'}
    counts = Counter(data)
    lo, hi = r, N - max(data) + r
    if hi < lo or min(data) < r:
        return {'error': 'empty feasible composition interval'}
    # Each factor (m+1)/(m+1-r) * (N-m-x+r)/(N-m)
    # decreases in m. Find the first ratio <= 1 using exact products.
    def ratio_sign(m):
        num = math.prod(((m+1)*(N-m-x+r))**c for x, c in counts.items())
        den = ((m+1-r)*(N-m))**len(data)
        return (num > den) - (num < den)

    while lo < hi:
        mid = (lo + hi)//2
        if ratio_sign(mid) > 0:
            lo = mid + 1
        else:
            hi = mid
    best = lo
    upper = N - max(data) + r
    # If there is a plateau, accept a fitted composition on it. Ratios are
    # strictly decreasing for r>0 on this feasible interval.
    if fitted == best + 1 <= upper and ratio_sign(best) == 0:
        best = fitted
    with mp.workdps(60):
        def choose(n, k):
            return mp.loggamma(n+1) - mp.loggamma(k+1) - mp.loggamma(n-k+1)
        ll = mp.fsum(c*(choose(x-1, r-1) + choose(N-x, best-r) - choose(N, best))
                     for x, c in counts.items())
        return profile_result(values, hold, {'m': best}, reason,
                              'exact adjacent-ratio bisection, conditional on population and stopping count', ll)


def series_preflight(ident, data, values):
    """The finite symbolic density specs accept only starts with a small tail bound."""
    if ident == 'cmp':
        rate, nu = values['lambda'], values['nu']
        if rate <= 0 or nu < 0:
            return 'CMP series guard requires positive rate and nonnegative dispersion'
        logs = [k*math.log(rate)-nu*math.lgamma(k+1) for k in range(129)]
        peak = max(logs[:128])
        log_ratio = math.log(rate) - nu*math.log(129)
        if log_ratio >= 0:
            return 'CMP series guard: 128-term normalizer remainder has no geometric bound'
        log_sum = peak + math.log(sum(math.exp(w-peak) for w in logs[:128]))
        if logs[128] - math.log(-math.expm1(log_ratio)) - log_sum >= math.log(1e-25):
            return 'CMP series guard: 128-term normalizer remainder is not below 1e-25'
    elif ident == 'dgamma':
        shape, rate = values['shape'], values['rate']
        with mp.workdps(40):
            for k in set(data):
                t = mp.mpf(k+1)*rate
                ratio = t/(shape+65)
                if ratio >= 1:
                    return 'discrete-gamma series guard: 64-term remainder has no geometric bound'
                remainder = mp.exp(-t)*t**(shape+64)/mp.gamma(shape+65)/(1-ratio)
                mass = mp.gammainc(shape, k*rate, t)/mp.gamma(shape)
                if mass <= 0 or remainder/mass >= mp.mpf('1e-20'):
                    return 'discrete-gamma series guard: 64-term remainder is not below 1e-20 of cell mass'
    return None
