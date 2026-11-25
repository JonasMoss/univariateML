"""Replay saved public coefficients and check selected CMB examples."""
import gzip
import hashlib
import json
from fractions import Fraction
from math import comb, inf, nextafter
from pathlib import Path

import mpmath as mp

mp.mp.dps = 80
HERE = Path(__file__).resolve().parent
EVAL = HERE.parent
SNAP = EVAL / '2026-11-15-fit-snapshot-data'
ARCHIVE = EVAL / '2026-11-07-fit-audit-data' / 'cases.json.gz'
NAMES = ('size', 'prob', 'nu')


def exact_checks():
    def likelihood(size):
        return Fraction(comb(size, 6), 2**(2*size))

    grid = [likelihood(size) for size in range(6, 33)]
    assert [size for size in range(6, 33) if likelihood(size) == max(grid)] == [7, 8]
    for size in range(6, 32):
        assert likelihood(size+1)/likelihood(size) == Fraction(size+1, 4*(size-5))
    for zero in range(1, 31):
        for one in range(1, 31):
            assert 2**one * zero**zero * one**one != (zero+one)**(zero+one)
    assert Fraction(3, 4) == Fraction(3, 1)/(1+3)
    assert Fraction(9, 10) == Fraction(3**2, 1)/(1+3**2)
    print('Exact checks: binomial tie grid 6..32, binary comparisons 1..30, held-odds saturation')

    def cubic(p):
        return 4*(1-p)*(p*p+(1-p)**2)-p

    root = mp.findroot(cubic, (mp.mpf(2)/3, mp.mpf(3)/4))
    rounded = float(root)
    lower, upper = ((rounded, nextafter(rounded, inf))
                    if cubic(Fraction(rounded)) > 0 else (nextafter(rounded, -inf), rounded))
    for value, sign in ((lower, -1), (upper, 1)):
        p = Fraction(value)
        assert Fraction(2, 3) < p < Fraction(3, 4)
        finite = p*p/(4*(p*p+(1-p)**2))
        face = p*(1-p)
        assert sign*(finite-face) > 0
        gap = (finite-face)/face
        print('Held-p crossing neighbor', value.hex(), 'log likelihood gap',
              mp.nstr(mp.log1p(mp.mpf(gap.numerator)/gap.denominator), 18))
    print('Held-p crossing root:', mp.nstr(root, 32))


def audit():
    with gzip.open(ARCHIVE, 'rt') as stream:
        inputs = {f"{row['index']:06d}": row for row in json.load(stream) if row['family'] == 'cmb'}
    for mode in ('native', 'double'):
        provenance = json.loads((SNAP / f'{mode}.provenance.json').read_text())
        worst_coef = worst_ll = mp.mpf(0)
        counts = {}
        with gzip.open(SNAP / f'{mode}.jsonl.gz', 'rt') as stream:
            for record in map(json.loads, stream):
                if record['family'] != 'cmb' or record['source'] != 'archive':
                    continue
                result = record['result']
                if result.get('kind') != 'ok':
                    continue
                raw = inputs[record['case']]
                x, held = raw['data'], raw['held']
                size = int(float.fromhex(result['estimates']['size']))
                p, nu = (mp.mpf(float.fromhex(result['estimates'][k])) for k in NAMES[1:])
                if size < 2 or size > 200 or not 0 < p < 1:
                    continue
                free = [k for k in NAMES[1:] if k not in held]
                if not free:
                    continue
                line = ' '.join([str(record['mask']), str(len(x)),
                                 *(float(held.get(k, 0)).hex() for k in NAMES),
                                 *(float(v).hex() for v in x)])+'\n'
                assert hashlib.sha256(line.encode()).hexdigest()[:16] == record['input_sha256']
                assert all(int(v) == v for v in x)
                x = list(map(int, x))
                h = [mp.log(comb(size, k)) for k in range(size+1)]
                mean = mp.mpf(sum(x))/len(x)
                stat = sum(h[k] for k in x)/len(x)
                eta = mp.log(p/(1-p))

                def stats(a, b):
                    weights = [mp.exp(k*a+b*hk) for k, hk in enumerate(h)]
                    z = sum(weights)
                    residuals = (sum(k*w for k, w in enumerate(weights))/z-mean,
                                 sum(hk*w for hk, w in zip(h, weights))/z-stat)
                    ll = len(x)*(a*mean+b*stat-mp.log(z))
                    return residuals, ll

                if len(free) == 2:
                    a, b = mp.findroot(lambda a, b: stats(a, b)[0], (eta, nu))
                elif free == ['prob']:
                    a, b = mp.findroot(lambda a: stats(a, nu)[0][0], eta), nu
                else:
                    a, b = eta, mp.findroot(lambda b: stats(eta, b)[0][1], nu)
                p_star = 1/(1+mp.exp(-a))
                err = max(abs(p-p_star)/p_star if 'prob' in free else 0,
                          abs(nu-b)/max(1, abs(b)) if 'nu' in free else 0)
                ll = mp.mpf(float.fromhex(result['logLik']))
                ll_public = stats(eta, nu)[1]
                ll_star = stats(a, b)[1]
                ll_err = max(abs(ll-ll_public), abs(ll-ll_star))/max(1, abs(ll_star))
                assert err <= mp.mpf('1e-10'), (mode, record['case'], err)
                assert ll_err <= mp.mpf('1e-10'), (mode, record['case'], ll_err)
                worst_coef = max(worst_coef, err)
                worst_ll = max(worst_ll, ll_err)
                key = str(record['mask'])
                counts[key] = counts.get(key, 0)+1
                if counts[key] == 1:
                    print(mode, key, record['case'], x, held, size,
                          result['estimates']['prob'], result['estimates']['nu'],
                          mp.nstr(err, 6), mp.nstr(ll_err, 6))
        assert counts
        print(mode, provenance['commit'], counts, 'worst coefficient / likelihood',
              mp.nstr(worst_coef, 8), mp.nstr(worst_ll, 8))


def limit_checks():
    poisson = -4 + 4*mp.log(2)-mp.log(24)
    previous = -mp.inf
    for N in (4, 5, 8, 32, 512, 10**7):
        p = mp.mpf(2)/N
        ll = mp.log(comb(N, 4))+4*mp.log(p)+(2*N-4)*mp.log1p(-p)
        assert previous < ll < poisson
        previous = ll
    print('Poisson dominance grid: gap at size 1e7 =', mp.nstr(poisson-ll, 12))

    M = 5_000_001
    p = mp.mpf(2)**-30
    r = p/(1-p)
    ratio = (M+1)*(1-p)**(M+1)*(1+r**M)
    assert ratio > (M+1)/2
    r_star = (mp.mpf(2)**100+mp.sqrt(mp.mpf(2)**200+12))/2
    p_star = r_star/(1+r_star)
    assert float(p_star) == 1
    p_binary = 1-mp.mpf(2)**-53
    r_binary = p_binary/(1-p_binary)

    def binary_ll(odds):
        z = 1+mp.mpf(2)**100*odds+odds**2
        return mp.log(mp.mpf(2)**100*odds**3/z**2)

    assert binary_ll(r_star)-binary_ll(r_binary) > 30
    print('High constant likelihood ratio:', mp.nstr(ratio, 12))
    print('Extreme public-probability likelihood loss:',
          mp.nstr(binary_ll(r_star)-binary_ll(r_binary), 12))

    a = mp.log(mp.mpf(2)/3)
    odds = (mp.mpf(1)/3, mp.mpf(2)/3, mp.mpf(4)/5)
    last = [mp.inf]*4
    for N in (64, 256, 1024, 4096, 16384):
        cases = [(mp.log(q), mp.mpf(1)) for q in odds]
        cases.append((mp.log(3)/N, mp.mpf(1)/4))
        errors = []
        for eta, mass in cases:
            nu = (a-eta)/mp.log(N)
            logw = mp.mpf(0)
            weights = [mp.mpf(1)]
            for k in range(N):
                logw += eta+nu*mp.log(mp.mpf(N-k)/(k+1))
                weights.append(mp.exp(logw))
            error = max(abs(weights[k]/sum(weights)-mass*(1-mp.exp(a))*mp.exp(k*a))
                        for k in range(3))
            errors.append(error)
        assert all(e < old for i, (e, old) in enumerate(zip(errors, last)) if i != 1)
        assert errors[1] < mp.mpf('1e-10') or N == 64
        last = errors
    print('Geometric path grid: max cell errors at size 16384 =',
          [mp.nstr(v, 6) for v in last])


exact_checks()
audit()
limit_checks()
