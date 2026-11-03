#!/usr/bin/env python3
"""Reproduce TASK-138's mathematical checks; no package fitter is modified.

Run from the worktree root through /tmp/umvl-cpu/run. Deterministic seed 138.
Profiles are numerical conditional optima, never unrestricted MLE estimates.
Exact witness certificates live separately in cert.py.
"""
import csv
import hashlib
import json
from pathlib import Path
import platform
import random

import mpmath as mp
import numpy as np
import scipy

from cert import certificates

mp.mp.dps = 75
HERE = Path(__file__).resolve().parent
ROWS = []


def M(x):
    return mp.mpf(str(x))


def sp(z):
    return max(z, 0) + mp.log1p(mp.exp(-abs(z)))


def sig(z):
    return mp.exp(-sp(-z))


def ell(y, k, c, a):
    z = [a + c*t for t in y]
    return len(y)*mp.log(k*c) - k*sum(sp(t) for t in z) - sum(sp(-t) for t in z)


def pareto(y):
    n = len(y)
    return n*mp.log(n/sum(y))-n


def weibull(y):
    n = len(y)

    def score(c):
        weights = [mp.exp(c*t) for t in y]
        return n/c + sum(y) - n*sum(t*w for t, w in zip(y, weights))/sum(weights)

    lo, hi = M('.000001'), M(1)
    while score(hi) > 0:
        hi *= 2
    # Bracketing is monotone by strict concavity; 260 iterations at 75 digits.
    for _ in range(260):
        mid = (lo+hi)/2
        if score(mid) > 0:
            lo = mid
        else:
            hi = mid
    c = (lo+hi)/2
    eta = mp.log(n/sum(mp.exp(c*t) for t in y))
    v = [mp.exp(eta+c*t) for t in y]
    value = n*mp.log(c) + n*eta + c*sum(y)-n
    A = sum(t*t/2-t for t in v)
    J = mp.matrix([[n/c**2+sum(t*t*w for t, w in zip(y, v)), sum(t*w for t, w in zip(y, v))],
                   [sum(t*w for t, w in zip(y, v)), n]])
    d = mp.matrix([sum(t*w*(w-1) for t, w in zip(y, v)), sum(w*(w-1) for w in v)])
    B = sum(w*w/2-w**3/3 for w in v)
    C = B + (d.T * mp.lu_solve(J, d))[0]/2
    assert abs(score(c)) < M('1e-65')
    return value, A, C, c, eta


def conditional(y, k, w=None):
    n = len(y)
    if w is None:
        w = weibull(y)
    m = sum(t == 0 for t in y)
    if k < M('.1'):
        c0 = n/(k*sum(y))
        a0 = mp.log((m-(n-m)*k)/(n*k))
    else:
        c0, a0 = w[3], w[4]-mp.log(k)

    def score(c, a):
        t = [1-(k+1)*sig(a+c*u) for u in y]
        return n/c+sum(u*v for u, v in zip(y, t)), sum(t)

    def hess(c, a):
        v = [(k+1)*sig(a+c*t)*sig(-a-c*t) for t in y]
        return mp.matrix([[-n/c**2-sum(t*t*u for t, u in zip(y, v)), -sum(t*u for t, u in zip(y, v))],
                          [-sum(t*u for t, u in zip(y, v)), -sum(v)]])

    c, a = mp.findroot(score, (c0, a0), J=hess, tol=M('1e-62'), maxsteps=100)
    residual = max(abs(t) for t in score(c, a))
    assert c > 0 and residual < M('1e-48'), (k, c, a, residual)
    return ell(y, k, c, a), c, a, residual


def row(kind, name, parameter, value, target, tolerance):
    error = abs(value-target)
    assert error < tolerance, (kind, name, parameter, error, tolerance)
    ROWS.append(dict(kind=kind, sample=name, parameter=mp.nstr(parameter, 12),
                     value=mp.nstr(value, 24), target=mp.nstr(target, 24),
                     absolute_error=mp.nstr(error, 8), tolerance=mp.nstr(tolerance, 8)))


def main():
    certs = certificates()
    negative = [mp.mpf(t)/15 for t in [0, 5, 7, 8, 8, 15, 15]]
    nonexist = list(map(M, [0, 0, 1]))

    def seam(t):
        return [M(0), t, mp.mpf(7)/15, mp.mpf(8)/15, mp.mpf(8)/15, M(1), M(1)]

    root = mp.findroot(lambda t: weibull(seam(t))[0]-pareto(seam(t)), (M('.23826'), M('.23827')))
    samples = {'negative_slope_finite': negative, 'equal_faces_finite': seam(root),
               'positive_slope_nonexistence': nonexist}
    summary = {'environment': {'python': platform.python_version(), 'mpmath': mp.__version__,
                              'numpy': np.__version__, 'scipy': scipy.__version__,
                              'platform': platform.platform()},
               'seed': 138, 'precision_decimal_digits': mp.mp.dps,
               'script_sha256': {name: hashlib.sha256((HERE/name).read_bytes()).hexdigest()
                                 for name in ['check.py', 'cert.py']},
               'certificates': certs, 'samples': {},
               'seam_t_numerical': mp.nstr(root, 65)}
    profiles = []
    for name, y in samples.items():
        n, m = len(y), sum(t == 0 for t in y)
        w, p = weibull(y), pareto(y)
        summary['samples'][name] = {'log_data': [mp.nstr(t, 65) for t in y],
                                  'P': mp.nstr(p, 30), 'W': mp.nstr(w[0], 30),
                                  'A': mp.nstr(w[1], 30), 'C': mp.nstr(w[2], 30),
                                  'weibull_c': mp.nstr(w[3], 30),
                                  'weibull_eta': mp.nstr(w[4], 30)}
        # Keep the plot grid small. Every conditional solve has a checked score.
        for j in range(65):
            k = mp.exp(-6+mp.mpf(j)*16/64)
            val, c, a, residual = conditional(y, k, w)
            profiles.append(dict(sample=name, k=mp.nstr(k, 20),
                                 H=mp.nstr(val, 25), P=mp.nstr(p, 25), W=mp.nstr(w[0], 25),
                                 gap_per_observation=mp.nstr((val-max(p,w[0]))/n, 20),
                                 c=mp.nstr(c, 20), a=mp.nstr(a, 20),
                                 max_score=mp.nstr(residual, 8)))
        for k in [M('.01'), M('.003'), M('.001')]:
            A, B = n*k, m*(1+k)
            R = A*mp.log(A)+(B-A)*mp.log(B-A)-B*mp.log(B)
            c0 = n/(k*sum(y))
            a0 = mp.log((m-(n-m)*k)/(n*k))
            E = (k+1)*mp.exp(-a0)*sum(mp.exp(-c0*t) for t in y if t > 0)
            val = conditional(y, k, w)[0]
            assert p+R-E-M('1e-60') <= val <= p+R+M('1e-60')
            row('pareto_expansion', name, k, (val-p)/(n*k)-mp.log(k),
                mp.log(mp.mpf(n)/m)-1, M('.06'))
            row('pareto_second_order', name, k,
                (val-p-n*k*mp.log(k)-n*k*(mp.log(mp.mpf(n)/m)-1))/(k*k),
                mp.mpf(n)*(n-2*m)/(2*m), M('.5'))
        for k in [M('1000'), M('10000'), M('100000')]:
            val = conditional(y, k, w)[0]
            row('weibull_first_order', name, k, k*(val-w[0]), w[1], M('.02'))
            row('weibull_second_order', name, k, k*k*(val-w[0]-w[1]/k), w[2], M('.1'))
        # Fixed-shape Weibull paths, not profiled approximations.
        for k in [M('100'), M('10000'), M('1000000')]:
            val = ell(y, k, w[3], w[4]-mp.log(k))
            row('weibull_path', name, k, val, w[0], M('.12'))
        alpha = n/sum(y)
        for rho in [M('.2'), M('.5'), M('.8')]:
            c = M('1000')
            a = mp.log(rho/(1-rho))
            val = ell(y, alpha/c, c, a)
            row('pareto_endpoint_penalty', name, rho, val, p+m*mp.log(rho), M('.05'))

    # Envelope, entropy upper bound, and the majority-minimum inequality.
    rng = random.Random(138)
    max_envelope_excess = mp.ninf
    max_entropy_excess = mp.ninf
    max_majority_logratio = mp.ninf
    max_taylor_excess = mp.ninf
    for j in range(180):
        k, c, a = [mp.exp(M(rng.uniform(-6, 6))) for _ in range(2)] + [M(rng.uniform(-10, 10))]
        lam = min(c, k*c)
        mode = (-a-mp.log(k))/c
        t = M(rng.uniform(-4, 4))
        excess = ell([t], k, c, a)-(mp.log(lam)+1-lam*abs(t-mode)/2)
        assert excess < M('1e-60')
        max_envelope_excess = max(max_envelope_excess, excess)
        y = [M(0), M(0), M('.2'), M('.7'), M(1)]
        n, m = len(y), 2
        kk = M(rng.uniform(.0001, .6))
        A, B = n*kk, m*(1+kk)
        R = A*mp.log(A)+(B-A)*mp.log(B-A)-B*mp.log(B)
        excess = ell(y, kk, c, a)-pareto(y)-R
        assert excess < M('1e-60')
        max_entropy_excess = max(max_entropy_excess, excess)
        n = rng.randrange(2, 15)
        m = (n+1)//2
        y = [M(0)]*m + sorted(M(rng.uniform(.01, 2)) for _ in range(n-m))
        z = [a+c*t for t in y]
        S, T = sum(sp(t) for t in z), sum(sp(-t) for t in z)
        logratio = n*mp.log(c*sum(y)/S)-T
        assert logratio < 0
        max_majority_logratio = max(max_majority_logratio, logratio)
        u = mp.exp(M(rng.uniform(-10, 10)))
        excess = u-u*u/2-mp.log1p(u)
        assert excess < M('1e-60')
        max_taylor_excess = max(max_taylor_excess, excess)
    summary['inequalities'] = {'trials': 180, 'max_envelope_excess': mp.nstr(max_envelope_excess, 15),
                               'max_entropy_excess': mp.nstr(max_entropy_excess, 15),
                               'max_majority_logratio': mp.nstr(max_majority_logratio, 15),
                               'max_taylor_excess': mp.nstr(max_taylor_excess, 15)}

    # A genuinely zero first-order slope needs the profiled second order.
    def zero_y(t):
        return [(1-t)*a+t*b for a, b in zip(negative, [0, 0, 0, 0, 0, 1, 1])]

    t0 = mp.findroot(lambda t: weibull(zero_y(t))[1], (M('.0439'), M('.0440')))
    yz = zero_y(t0)
    wz = weibull(yz)
    assert abs(wz[1]) < M('1e-65') and wz[0] > pareto(yz) and wz[2] > M('.3')
    summary['zero_slope'] = {'t_numerical': mp.nstr(t0, 65),
                             'W_minus_P': mp.nstr(wz[0]-pareto(yz), 30),
                             'A': mp.nstr(wz[1], 10), 'C': mp.nstr(wz[2], 30)}
    for k in [M('1000'), M('10000'), M('100000')]:
        val = conditional(yz, k, wz)[0]
        assert val > wz[0]
        row('zero_slope_second_order', 'zero_slope_finite', k,
            k*k*(val-wz[0]), wz[2], M('.01'))

    # A family joining a Pareto-only sample to a strict finite winner,
    # staying strictly on the P>W side. The report proves that some
    # intermediate sample has a finite MLE exactly tied with Pareto.
    # These numerical checks do not claim to locate that global crossing.
    family = []
    last_gap = mp.ninf
    for q in map(M, ['0', '.25', '.5', '.75', '.99', '1']):
        yq = [M(0), q*M('.23826'), q*mp.mpf(7)/15,
              q*mp.mpf(8)/15, q*mp.mpf(8)/15, M(1), M(1)]
        wq, pq = weibull(yq)[0], pareto(yq)
        fixed = ell(yq, M(1), M('4.932'), M('-2.624'))
        assert last_gap < wq-pq < 0
        last_gap = wq-pq
        family.append({'q': mp.nstr(q, 8), 'W_minus_P': mp.nstr(wq-pq, 25),
                       'fixed_finite_minus_P': mp.nstr(fixed-pq, 25)})
    assert M(family[-1]['fixed_finite_minus_P']) > M('.026')
    summary['finite_P_tie_family'] = family

    # Collapse: k=1, c->infinity, scale=x0 gives g(log x0)=c/4 exactly.
    for c in map(M, [10, 1000, 1000000]):
        row('collapse', 'constant', c, ell([M(0)]*3, M(1), c, M(0)),
            3*mp.log(c/4), M('1e-65'))
    for q in [M('.2'), M('.5'), M('.8')]:
        for t in [M(-2), M(2)]:
            row('endpoint_mixture_cdf', 'weak_only', q,
                sig(mp.log(q/(1-q))+M('.0001')*t), q, M('.0001'))

    summary['compact_bounds'] = {}
    for name, y in samples.items():
        n, R = len(y), max(y)
        B = max(weibull(y)[0], pareto(y))
        L = mp.exp(B/n)
        U = 4*n/R*(1+mp.log(2*n/R)-B/n)
        k0 = mp.mpf(sum(t == 0 for t in y))/(n-sum(t == 0 for t in y))
        C = U/min(1, k0)
        bounds = {'L': mp.nstr(L, 20), 'U': mp.nstr(U, 20),
                  'k0': mp.nstr(k0, 20), 'c_upper': mp.nstr(C, 20)}
        if name == 'negative_slope_finite':
            # The exact rational finite witness is more than .06 above B.
            bounds['log10_k_cutoff_for_gap_0.06'] = mp.nstr((mp.log(2*n/M('.06'))+2*U*R)/mp.log(10), 25)
        summary['compact_bounds'][name] = bounds
        for r in (r for r in profiles if r['sample'] == name):
            k, value, c, a = map(M, [r['k'], r['H'], r['c'], r['a']])
            if value >= B:
                eta = a+mp.log(k)
                assert L-M('1e-15') <= c <= C+M('1e-15')
                assert -c*R-M('1e-15') <= eta <= M('1e-15')
                assert k > k0
            if k >= 1:
                # This is an upper bound, not a claimed global MLE.
                upper = B+n*mp.exp(2*U*R)/k
                assert value < upper
    # Power transformations preserve decisions while raw CV crosses one.
    summary['raw_cv'] = {}
    for name, y in [('finite', negative), ('nonexistent', nonexist)]:
        for power in [M('.01'), M('10')]:
            x = [mp.exp(power*t) for t in y]
            mean = sum(x)/len(x)
            cv = mp.sqrt(sum((t-mean)**2 for t in x)/len(x))/mean
            summary['raw_cv'][f'{name}_power_{power}'] = mp.nstr(cv, 25)
    assert M(summary['raw_cv']['finite_power_0.01']) < 1
    assert M(summary['raw_cv']['finite_power_10.0']) > 1
    assert M(summary['raw_cv']['nonexistent_power_0.01']) < 1
    assert M(summary['raw_cv']['nonexistent_power_10.0']) > 1
    for filename, rows in [('checks.csv', ROWS), ('profiles.csv', profiles)]:
        with (HERE/filename).open('w', newline='') as handle:
            writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)
    summary['checked_rows'] = len(ROWS)
    summary['conditional_profiles'] = len(profiles)
    (HERE/'summary.json').write_text(json.dumps(summary, indent=2)+'\n')
    print(f'PASS: exact witness certificates; {len(ROWS)} convergence checks; '
          f'{len(profiles)} conditional profiles; 720 inequality checks')


if __name__ == '__main__':
    main()
