#!/usr/bin/env python3
"""Research checks only; no fitted estimate or global certificate is returned.

Read the prior map without modifying it. Check proved inequalities at 70 digits,
measure their usefulness, and exhibit failures of a mixed-Poisson relaxation.
"""
import os
for key in ('OPENBLAS_NUM_THREADS', 'OMP_NUM_THREADS', 'MKL_NUM_THREADS'):
    os.environ[key] = '1'
import csv
import json
import math
from pathlib import Path
import random
import sys
import mpmath as mp
import numpy as np
import scipy
from scipy.optimize import minimize_scalar
from scipy.special import gammaln

HERE = Path(__file__).resolve().parent
MAP = HERE.parent / '2026-11-02-bbinom-map-data'

def read(name):
    with (MAP / name).open() as stream:
        return list(csv.DictReader(stream))

def write(name, rows):
    with (HERE / name).open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)

def pois(x, mu):
    return x * math.log(mu) - mu - gammaln(x + 1)

def nb(x, a, mu):
    # Finite products avoid large-shape log-gamma subtraction.
    return (sum(math.log1p(j / a) for j in range(int(x)))
            + x * math.log(mu) - (a + x) * math.log1p(mu / a)
            - gammaln(x + 1))

def small_bound(x, n, t):
    h = len(x)
    m = sum(k > 0 for k in x)
    M = max(x)
    H = math.log1p(n / t)
    K = sum(sum(math.log(t + j) for j in range(1, k))
            - gammaln(k + 1) for k in x if k)
    return (K + sum(x) * (-math.log1p(-M / n))
            + m * (math.log(m / h) - 1 - math.log(H)))

def tail_bound(x, n, t, face):
    # face must be an UPPER bound in a real certificate. Here it is the
    # prior map's NUMERICAL candidate: this is only a bound-size diagnostic.
    G = sum(k * k / (2 * (n - k)) for k in x)
    error = sum(min(1., 2 * (2 * k + 1) / math.sqrt(t) + 4 / t) for k in x)
    large = G + np.logaddexp(face, math.log(error))
    return max(small_bound(x, n, t), large)

def necessary_tuning_check(x, n, candidate, face):
    """Necessary condition for *any* T to clear this bound, in log space.

    Still numerical: candidate and face originate in the old map. This
    rules out fixing the measured weakness just by refining the T grid.
    """
    G = sum(k*k/(2*(n-k)) for k in x)
    if candidate-G <= face:
        return False, '', ''
    logD = candidate-G + math.log(-math.expm1(face-candidate+G))
    C = sum(2*(2*k+1) for k in x)
    logT = 2*(math.log(C)-logD)
    z = math.log(n)-logT
    logH = z if z < -30 else math.log(math.log1p(math.exp(z)))
    K = sum(sum(float(np.logaddexp(logT, math.log(j))) for j in range(1,k))
            - gammaln(k+1) for k in x if k)
    h, m, M = len(x), sum(k>0 for k in x), max(x)
    lower = K-sum(x)*math.log1p(-M/n)+m*(math.log(m/h)-1-logH)
    return bool(lower < candidate), logT, lower

def mixture_improvement(x, face, a, mu):
    x = np.asarray(x, dtype=int)
    qlog = np.array([nb(k, a, mu) if math.isfinite(a) else pois(k, mu) for k in x])
    assert abs(float(sum(qlog)) - face) < 2e-7
    # A mixture of this NB/Poisson and a Poisson point is mixed Poisson.
    # Its likelihood is a valid witness, not an optimized envelope claim.
    best = (face, 0., mu)
    for lam in np.unique(np.r_[np.geomspace(.001, 2 * max(x) + 5, 90), mu, x[x > 0]]):
        plog = x * math.log(lam) - lam - gammaln(x + 1)
        def value(w):
            return float(np.sum(np.logaddexp(math.log1p(-w) + qlog, math.log(w) + plog)))
        fit = minimize_scalar(lambda w: -value(w), bounds=(1e-12, 1 - 1e-12), method='bounded',
                              options={'xatol': 1e-12})
        ll = value(float(fit.x))
        if ll > best[0]:
            best = (ll, float(fit.x), float(lam))
    return best

def high_precision_checks():
    mp.mp.dps = 70
    rng = random.Random(113)
    maxima = dict(kernel=-mp.inf, coupling=-mp.inf, small_total=-mp.inf,
                  mixed_poisson_coupling=-mp.inf)
    for _ in range(600):
        n = rng.choice([10, 30, 100, 1000, 1000000])
        k = rng.randint(0, min(n - 1, 30))
        a = mp.mpf(10) ** rng.uniform(-5, 6)
        b = mp.mpf(10) ** rng.uniform(-5, 7)
        t = a + b
        p = mp.mpf(rng.uniform(.00001, .99999))
        # Kernel comparison, including p arbitrarily close to the endpoints.
        lbin = mp.loggamma(n + 1) - mp.loggamma(k + 1) - mp.loggamma(n - k + 1) + k * mp.log(p) + (n - k) * mp.log1p(-p)
        lpois = k * mp.log(n * p) - n * p - mp.loggamma(k + 1)
        g = mp.mpf(k) ** 2 / (2 * (n - k))
        maxima['kernel'] = max(maxima['kernel'], lbin - lpois - g)
        def bb(j):
            return mp.exp(mp.loggamma(n + 1) - mp.loggamma(j + 1) - mp.loggamma(n - j + 1)
                          + mp.loggamma(j + a) + mp.loggamma(n - j + b) - mp.loggamma(n + t)
                          - mp.loggamma(a) - mp.loggamma(b) + mp.loggamma(t))
        mu = n * a / t
        q = mp.exp(mp.loggamma(k + a) - mp.loggamma(a) - mp.loggamma(k + 1)
                   + a * mp.log(t / (t + n)) + k * mp.log(mp.mpf(n) / (t + n)))
        delta = 2 * (2 * k + 1) / mp.sqrt(t) + 4 / t
        # This checks the resulting BB <= exp(g) (NB + delta) consequence;
        # the intermediate mixed-Poisson integral is not evaluated here.
        maxima['coupling'] = max(maxima['coupling'], bb(k) - mp.exp(g) * (q + delta))
        if _ < 80 and n <= 1000:
            # Exact beta Laplace transform, independent of the coupling
            # inequality. Avoid a singular beta-density quadrature.
            z = (mp.mpf(n)**k/mp.factorial(k)*mp.rf(a,k)/mp.rf(t,k)
                 *mp.hyp1f1(a+k,t+k,-n))
            maxima['mixed_poisson_coupling'] = max(
                maxima['mixed_poisson_coupling'], abs(z-q)-min(mp.mpf(1),delta))
        x = [rng.randint(0, min(n - 1, 8)) for _ in range(rng.randint(3, 8))]
        if not max(x):
            x[0] = 1
        h, m, M, S = len(x), sum(j > 0 for j in x), max(x), sum(x)
        H = mp.log1p(n / t)
        K = sum(sum(mp.log(t + j) for j in range(1, v)) - mp.loggamma(v + 1) for v in x if v)
        bound = K + S * mp.log(mp.mpf(n) / (n - M)) + m * (mp.log(mp.mpf(m) / h) - 1 - mp.log(H))
        actual = sum(mp.log(bb(j)) for j in x)
        maxima['small_total'] = max(maxima['small_total'], actual - bound)
    assert all(v <= mp.mpf('1e-55') for v in maxima.values())
    return {key: str(value) for key, value in maxima.items()}

def main():
    checked = high_precision_checks()
    print('70-digit inequality checks:', checked, flush=True)
    samples, faces, profiles, classifications = (read(name) for name in
        ('samples.csv', 'faces.csv', 'certified.csv', 'classification.csv'))
    extended = {int(r['index']): r for r in read('extended.csv')}
    rows = []
    for i, (sample, face, profile, classification) in enumerate(zip(samples, faces, profiles, classifications)):
        assert int(face['index']) == int(profile['index']) == int(classification['index']) == i
        x = list(map(int, sample['data'].split()))
        best = extended.get(i, profile)
        candidate = float(best['loglik'])
        f = float(face['nb'])
        n = max(1001, max(x) + 1)
        # Search only over a tuning parameter in an already proved bound.
        # A better unsearched T can improve the bound; no optimum is claimed.
        ts = np.geomspace(.01, 1e12, 100)
        values = [tail_bound(x, n, t, f) for t in ts]
        j = int(np.argmin(values))
        possible, logTmin, small_lower = necessary_tuning_check(x,n,candidate,f)
        rows.append(dict(index=i,region=classification['region'],data=sample['data'],
                         candidate_N=best['N'],candidate_loglik=candidate,face_loglik=f,
                         tail_from_N=n,T=float(ts[j]),diagnostic_upper_loglik=float(values[j]),
                         clears_numerical_candidate=bool(values[j] < candidate - 2e-8 * len(x)),
                         any_T_not_ruled_out=possible,necessary_logT_lower=logTmin,
                         small_branch_at_T_lower=small_lower))
    assert len(rows) == 2202
    write('tail-diagnostics.csv', rows)
    hard = []
    for i in extended:
        sample, face = samples[i], faces[i]
        x = list(map(int, sample['data'].split()))
        a = float(face['nb_a']) if face['nb_a'] else math.inf
        witness = mixture_improvement(x, float(face['nb']), a, float(sample['mean']))
        hard.append(dict(index=i,data=sample['data'],candidate_N=extended[i]['N'],
                         candidate_loglik=float(extended[i]['loglik']),face_loglik=float(face['nb']),
                         mixture_loglik=witness[0],weight=witness[1],lambda_=witness[2],
                         mixture_minus_candidate=witness[0]-float(extended[i]['loglik'])))
    # Seven published profile-grid cases, including both slope counterexamples.
    grid = read('profile-grid.csv')
    for name in dict.fromkeys(r['data'] for r in grid):
        x = list(map(int, name.split('-')))
        h = len(x); mu = sum(x) / h
        v = sum((k-mu)**2 for k in x) / h
        a = math.inf
        f = sum(pois(k, mu) for k in x)
        if v > mu:
            opt = minimize_scalar(lambda z: -sum(nb(k, math.exp(z), mu) for k in x),
                                  bounds=(-12, 18), method='bounded')
            a = math.exp(opt.x); f = -opt.fun
        witness = mixture_improvement(x, f, a, mu)
        best = max((r for r in grid if r['data'] == name), key=lambda r:float(r['loglik']))
        hard.append(dict(index=name,data=' '.join(map(str,x)),candidate_N=best['N'],
                         candidate_loglik=float(best['loglik']),face_loglik=f,
                         mixture_loglik=witness[0],weight=witness[1],lambda_=witness[2],
                         mixture_minus_candidate=witness[0]-float(best['loglik'])))
    write('mixture-witnesses.csv', hard)
    gap = mp.mpf(1)/27-mp.exp(-3)/2
    T0 = (18/gap)**2
    H0 = 2/(3*mp.e)*mp.sqrt((T0+1)/(2*mp.mpf(1)/27))
    minimum_log10_N = (mp.log(T0)+H0+mp.log1p(-mp.exp(-H0)))/mp.log(10)
    summary = dict(seed=113,mpmath_dps=70,random_trials=600,samples=len(rows),
                   python=sys.version.split()[0],numpy=np.__version__,scipy=scipy.__version__,
                   mpmath=mp.__version__,
                   checks=checked,tail_diagnostics_clear=sum(r['clears_numerical_candidate'] for r in rows),
                   any_T_not_ruled_out=sum(r['any_T_not_ruled_out'] for r in rows),
                   nonconstant_samples=sum(len(set(r['data'].split()))>1 for r in rows),
                   nonconstant_any_T_not_ruled_out=sum(r['any_T_not_ruled_out']
                       and len(set(r['data'].split()))>1 for r in rows),
                   easy_sample_necessary_log10_size=str(minimum_log10_N),
                   hard_witnesses=hard)
    (HERE/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
    print('map diagnostics:',len(rows),'clear:',summary['tail_diagnostics_clear'],flush=True)
    for row in hard:
        print('mixture witness',row['index'],'minus checked candidate',row['mixture_minus_candidate'],flush=True)
if __name__ == '__main__':
    main()
