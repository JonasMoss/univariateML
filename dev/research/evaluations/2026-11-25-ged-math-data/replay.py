"""Exact-input mpmath replay of frozen public fits and GED shape examples."""
import gzip
import hashlib
import json
import math
from pathlib import Path

from mpmath import mp

EVALUATIONS = Path(__file__).resolve().parent.parent
ARCHIVE = EVALUATIONS / '2026-11-07-fit-audit-data/cases.json.gz'
SNAPSHOTS = EVALUATIONS / '2026-11-15-fit-snapshot-data'
GOLDEN = (EVALUATIONS.parents[2] /
          'univariateML-cpp/include/cppunivariate/distributions/ged/golden.json')
IDS = ('002360', '002372', '002389', '002404', '002420', '002436',
       '002452', '002468', 'golden')
NAMES = ('mean', 'sd', 'nu')


def dyadic(value):
    a, b = float(value).as_integer_ratio()
    return mp.mpf(a)/b


def score(x, m, s, p):
    t = 1/p
    log_b = mp.log(s) + (mp.loggamma(t) - mp.loggamma(3*t))/2
    beta = (3*mp.digamma(1+3*t) - mp.digamma(1+t))/2
    location_terms, power_terms, power_log_terms = [], [], []
    for xi in x:
        d = xi-m
        if d:
            z = mp.log(abs(d))-log_b
            w = mp.exp(p*z)
            location_terms.append(w/d)
            power_terms.append(w)
            power_log_terms.append(w*z)
    total = mp.fsum(power_terms)
    return (p*mp.fsum(location_terms), p*total-len(x),
            len(x)*(mp.digamma(1+t)-beta) + p*beta*total - p*p*mp.fsum(power_log_terms))


def reference(x, held, start):
    m, s, p = start
    if p == 1 and 'mean' not in held:
        ordered = sorted(x)
        lo, hi = ordered[(len(x)-1)//2], ordered[len(x)//2]
        assert lo <= m <= hi or lo == hi
        m = min(hi, max(lo, m))
    if p == 1 and 'sd' not in held:
        s = mp.sqrt(2)*mp.fsum(abs(v-m) for v in x)/len(x)
    if p == 1:
        return m, s, p
    free = [i for i, name in enumerate(NAMES) if name not in held]
    initial = [m, mp.log(s), mp.log(p)]

    def unpack(args):
        q = initial[:]
        for i, arg in zip(free, args):
            q[i] = arg
        return (q[0], s if 1 not in free else mp.exp(q[1]),
                p if 2 not in free else mp.exp(q[2]))

    if not free:
        return start

    def equations(*args):
        scores = score(x, *unpack(args))
        return [scores[i] for i in free]

    result = mp.findroot(equations, [initial[i] for i in free],
                         tol=mp.power(10, 12-mp.dps))
    assert max(abs(v) for v in equations(*result)) < mp.power(10, 15-mp.dps)
    if 2 in free:
        def gradient(*args):
            m_fit, s_fit, p_fit = unpack(args)
            scores = score(x, m_fit, s_fit, p_fit)
            g = (scores[0], scores[1], scores[2]/p_fit)
            return [g[i] for i in free]

        hessian = mp.jacobian(gradient, result)
        information = -(hessian+hessian.T)/2
        assert all(mp.det(information[:i, :i]) > 0 for i in range(1, len(free)+1))
    return unpack(result)


def snapshot_errors():
    with gzip.open(ARCHIVE, 'rt') as source:
        data = {f"{r['index']:06d}": r['data'] for r in json.load(source)
                if r['family'] == 'ged'}
    data['golden'] = json.loads(GOLDEN.read_text())['data']
    for mode in ('native', 'double'):
        with gzip.open(SNAPSHOTS / f'{mode}.jsonl.gz', 'rt') as source:
            fits = {r['case']: r for r in map(json.loads, source)
                    if r['family'] == 'ged' and r['case'] in IDS}
        for label in IDS:
            r = fits[label]
            estimates = r['result']['estimates']
            assert r['result']['kind'] == 'ok' and r['result']['certified']
            public = tuple(dyadic(float.fromhex(estimates[name])) for name in NAMES)
            text = " ".join([str(r["mask"]), str(len(data[label])),
                             *(float(r["held"].get(k, 0)).hex() for k in NAMES),
                             *(float(v).hex() for v in data[label])]) + "\n"
            assert hashlib.sha256(text.encode()).hexdigest()[:16] == r["input_sha256"]
            x = tuple(map(dyadic, data[label]))
            exact = reference(x, r['held'], public)
            floors = (exact[1], 0, 0)
            errors = [abs(a-b)/max(abs(b), floor)
                      for name, a, b, floor in zip(NAMES, public, exact, floors)
                      if name not in r['held']]
            worst = max(errors, default=mp.mpf(0))
            assert worst <= mp.mpf('1e-10')
            # At held shape, recompute exact conditional sd at the emitted
            # location and compare it to sd at the exact joint location.
            if 'nu' in r['held'] and 'mean' not in r['held'] and 'sd' not in r['held']:
                def conditional_sd(m):
                    p = exact[2]
                    return (p * mp.fsum(abs(v-m)**p for v in x)/len(x))**(1/p) * mp.sqrt(
                        mp.gamma(3/p)/mp.gamma(1/p))
                rerounded = abs(conditional_sd(public[0])/conditional_sd(exact[0])-1)
                assert rerounded <= mp.mpf('1e-10')
            else:
                rerounded = mp.mpf(0)
            print(mode, label, 'coefficient error', mp.nstr(worst, 10),
                  'conditional sd drift', mp.nstr(rerounded, 10))


def held_value(p, radii, counts):
    t = 1/p
    g1, g3 = mp.loggamma(1+t), mp.loggamma(1+3*t)
    return (g3-3*g1)/2 - mp.exp((g3-g1)/(2*t))*mp.fsum(
        k*(r/mp.sqrt(3))**p for k, r in zip(counts, radii))/sum(counts)


def near_contact():
    r = mp.sqrt(3)
    a = dyadic(math.sqrt(3))
    # Exact binary64 width classification precedes numerical optimization.
    aa = math.sqrt(3).as_integer_ratio()
    assert aa[0]**2 < 3*aa[1]**2 and a < r
    delta = -mp.log(a/r)

    def value(q):
        return held_value(q/delta, [a], [2])

    lower, upper = mp.mpf(70), mp.mpf(100)
    assert mp.diff(value, lower) > 0 > mp.diff(value, upper)
    for _ in range(100):
        q = (lower + upper)/2
        if mp.diff(value, q) > 0:
            lower = q
        else:
            upper = q
    q = (lower + upper)/2
    assert mp.diff(value, q, 2) < 0 and value(q) > 0
    assert abs(mp.diff(value, q)) < value(q)*mp.mpf("1e-24")
    assert mp.log(6/mp.pi)/2 < mp.exp(-mp.euler-10**6*delta)
    assert mp.log(6)/2 < mp.mpf(3)/2*mp.exp(-2*delta)
    for p in (10**6, 10**12, 10**16, 10**18, 10**19):
        print('near-contact grid', p, mp.nstr(value(p*delta), 12))
    print('near-contact local root', mp.nstr(q/delta, 16),
          'excess', mp.nstr(value(q), 12))


def two_shape_crossing():
    outer = dyadic(1.4239025460520294)

    def value(p, a):
        return held_value(p, [a, outer, 0], [46, 24, 8])

    def equations(a, lo, hi):
        return (mp.diff(lambda p: value(p, a), lo),
                mp.diff(lambda p: value(p, a), hi),
                value(lo, a)-value(hi, a))

    a, lo, hi = mp.findroot(equations, tuple(map(mp.mpf, ("0.33", "1.11", "25.4"))),
                            tol=mp.power(10, -mp.dps+8))
    assert mp.mpf("0.329") < a < mp.mpf("0.331") and 1 < lo < 2 and 8 < hi < 100
    for p in (lo, hi):
        assert mp.diff(lambda p: value(p, a), p, 2) < 0
    print("held-sd local branch crossing", *(mp.nstr(v, 20) for v in (a, lo, hi)))
    print("exact decimal-rounding inputs", float("0.33").hex(), float(outer).hex())


def adjacent_laplace_tie():
    a = 3/mp.e-1
    nearest = float(a)
    if dyadic(nearest) < a:
        pair = nearest, math.nextafter(nearest, math.inf)
    else:
        pair = math.nextafter(nearest, -math.inf), nearest
    gaps = [mp.log(3/(1+dyadic(z)))-1 for z in pair]
    assert gaps[0] > 0 > gaps[1]
    for value, gap in zip(pair, gaps):
        print('free-scale adjacent', value.hex(), mp.nstr(gap, 12))


def exact_fixtures():
    knots = (0, 9, 25)
    for counts, expected in (((3, 2, 3), [21, 21, 23]),
                              ((8, 5, 9), [60, 60, 60])):
        assert [sum(w*math.isqrt(abs(x-m)) for x, w in zip(knots, counts))
                for m in knots] == expected
    d = mp.mpf(2)**-52
    exact_m = 1 + d/2
    assert dyadic(float(exact_m)) == 1
    assert mp.sqrt((d*d)/2)/(d/2) - 1 > mp.mpf("0.4")


if __name__ == '__main__':
    for precision in (85, 115):
        mp.dps = precision
        print('decimal precision', precision)
        exact_fixtures()
        snapshot_errors()
        near_contact()
        two_shape_crossing()
        adjacent_laplace_tie()
