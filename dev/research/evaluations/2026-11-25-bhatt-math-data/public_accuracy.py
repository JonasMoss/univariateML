"""Recheck archived public binary64 coefficients against the joint root."""
import gzip
import hashlib
import json
from pathlib import Path

import mpmath as mp

from crossings import logmass, phi

ROOT = Path(__file__).resolve().parents[4]
EVAL = ROOT/'dev/research/evaluations'
SNAPSHOTS = EVAL/'2026-11-15-fit-snapshot-data'
ARCHIVE = EVAL/'2026-11-07-fit-audit-data/cases.json.gz'
GOLDEN = ROOT/'univariateML-cpp/include/cppunivariate/distributions/bhatt/golden.json'
KEYS = ('mu', 'sigma', 'a')
CASES = ('000348', '000350', '000351', '000355', '000382', '000410', 'golden')


def objective_score(data, point):
    mu, sigma, a = point
    lls, sm, ss, sa = [], [], [], []
    for x in data:
        z = (x-mu)/sigma
        if a == 0:
            lls.append(-mp.log(2*mp.pi)/2-mp.log(sigma)-z*z/2)
            sm.append(z/sigma)
            ss.append((z*z-1)/sigma)
            sa.append(mp.mpf(0))
        else:
            lo, hi = z-a/sigma, z+a/sigma
            assert lo < hi, 'insufficient precision for the normal interval'
            lm = logmass(lo, hi)
            left, right = mp.exp(mp.log(phi(lo))-lm), mp.exp(mp.log(phi(hi))-lm)
            lls.append(lm-mp.log(2*a))
            sm.append((left-right)/sigma)
            ss.append((lo*left-hi*right)/sigma)
            sa.append((left+right)/sigma-1/a)
    return mp.fsum(lls), tuple(mp.fsum(v) for v in (sm, ss, sa))


def reference(data, before, held):
    free = [i for i, name in enumerate(KEYS) if name not in held]
    if before[2] == 0:
        mu = mp.fsum(data)/len(data) if 0 in free else before[0]
        variance = mp.fsum((x-mu)**2 for x in data)/len(data)
        sigma = mp.sqrt(variance) if 1 in free else before[1]
        if 2 in free:
            z2 = [(x-mu)**2/sigma**2 for x in data]
            if 1 in free:
                assert mp.fsum(v*v-6*v+3 for v in z2) > 0
            else:
                assert variance < sigma*sigma
        return (mu, sigma, mp.mpf(0)), free

    units = (before[1], before[1], before[2])

    def physical(args):
        point = list(before)
        for i, dx in zip(free, args):
            point[i] += dx*units[i]
        return tuple(point)

    def score(*args):
        _, totals = objective_score(data, physical(args))
        return tuple(units[i]*totals[i] for i in free)

    root = mp.findroot(score, tuple(mp.mpf(0) for _ in free))
    assert max(map(abs, score(*root))) < mp.mpf('1e-65')
    info = -mp.matrix(len(free))
    for i in range(len(free)):
        for j in range(len(free)):
            def change(d):
                args = list(root)
                args[j] += d
                return score(*args)[i]
            info[i,j] = -mp.diff(change, 0)
    mp.cholesky(info)
    return physical(root), free


def verify(label, held, result, input_data):
    assert result['kind'] == 'ok'
    before = tuple(mp.mpf(float.fromhex(result['estimates'][name])) for name in KEYS)
    assert all(before[KEYS.index(name)] == mp.mpf(value) for name, value in held.items())
    data = [mp.mpf(float(x)) for x in input_data]
    optimum, free = reference(data, before, held)
    units = (max(abs(optimum[0]), optimum[1]), optimum[1], max(1, optimum[2]))
    errors = [abs(before[i]-optimum[i])/units[i] if i in free else 0
              for i in range(len(KEYS))]
    ll = objective_score(data, before)[0]
    ll_error = None
    if 'logLik' in result:
        reported_ll = mp.mpf(float.fromhex(result['logLik']))
        ll_error = abs(ll-reported_ll)/max(1, abs(ll))
        assert ll_error < mp.mpf('1e-10')
    assert max(errors) < mp.mpf('1e-10')
    if optimum[2] and 2 in free:
        assert abs(before[2]-optimum[2])/optimum[2] < mp.mpf('1e-10')
    errors_text = ' '.join(mp.nstr(v, 8) for v in errors)
    print(label, result['certified'], errors_text,
          '-' if ll_error is None else mp.nstr(ll_error, 8))


def check(row, input_data):
    held = row['held']
    args = [str(row['mask']), str(len(input_data))]
    args += [float(held.get(name, 0)).hex() for name in KEYS]
    args += [float(x).hex() for x in input_data]
    fingerprint = hashlib.sha256((' '.join(args)+'\n').encode()).hexdigest()[:16]
    assert fingerprint == row['input_sha256']
    verify(row['case'], held, row['result'], input_data)


def main():
    with gzip.open(ARCHIVE, 'rt') as stream:
        archive = {f'{r["index"]:06d}': r for r in json.load(stream)
                   if r['family'] == 'bhatt'}
    archive['golden'] = {'data': json.loads(GOLDEN.read_text())['data']}
    mp.mp.dps = 85
    for mode in ('native', 'double'):
        with gzip.open(SNAPSHOTS/f'{mode}.jsonl.gz', 'rt') as stream:
            rows = {r['case']: r for line in stream if '"family": "bhatt"' in line
                    and (r := json.loads(line))['case'] in CASES}
        assert set(rows) == set(CASES)
        print(mode, 'case certified errors(mu,sigma,a) logLik error')
        for case in CASES:
            # Case 410's held width is 150 orders below its fitted sigma.
            with mp.workdps(380 if case == '000410' else 85):
                check(rows[case], archive[case]['data'])
    mp.mp.dps = 105
    recent = EVAL/'2026-11-22-render-fu-data/bhatt-recheck.jsonl'
    for line in recent.read_text().splitlines():
        row = json.loads(line)
        mask, n, mu, sigma, _, *data = row['input'].split()
        assert int(mask) == 3 and int(n) == len(data)
        verify(row['run'], {'mu': float(mu), 'sigma': float(sigma)},
               row['result'], list(map(float, data)))
    far = EVAL/'2026-11-21-mode-kind-data/case-outcomes.txt'
    for line in far.read_text().splitlines():
        if '"family": "bhatt"' in line:
            row = json.loads(line)
            case = archive[row['case']]
            result = dict(row, estimates={name: value.hex()
                                           for name, value in row['coefficients'].items()})
            verify(row['mode']+' far-tail '+row['case'], case['held'],
                   result, case['data'])
    shift = mp.mpf(10)**16
    x = (shift-10, *([shift]*20), shift+12)
    mu = mp.fsum(x)/len(x)
    rounded = mp.mpf(float(mu))
    joint_s = mp.sqrt(mp.fsum((v-mu)**2 for v in x)/len(x))
    refit_s = mp.sqrt(mp.fsum((v-rounded)**2 for v in x)/len(x))
    jump = abs(refit_s/joint_s-1)
    assert rounded == shift and jump > mp.mpf('1e-4')
    print('offset counterfactual refit scale error', mp.nstr(jump, 18))


if __name__ == '__main__':
    main()
