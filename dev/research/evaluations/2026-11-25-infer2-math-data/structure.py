"""High-precision cross-checks for the transition normalization and pivots."""
import csv
from pathlib import Path

import mpmath as mp


mp.mp.dps = 80
TOL = mp.mpf('1e-72')
ROWS = []


def check(label, cell, computed, expected):
    error = abs(computed - expected)
    assert error < TOL, (label, cell, error)
    ROWS.append((label, cell, mp.nstr(computed, 30), mp.nstr(error, 8)))


def transition():
    for exponent in [8, 60, 200]:
        log_n = exponent * mp.log(10)
        lower = -(1+mp.log(log_n)/log_n)/2
        for tau in map(mp.mpf, ['-5', '-1', '0', '1', '5']):
            observed = mp.quad(lambda t: mp.exp(tau*t), [lower, 0])
            analytic = -mp.expm1(tau*lower)/tau if tau else -lower
            check('transition-cutoff', f'log10_n={exponent};tau={tau}', observed, analytic)
            limit = -mp.expm1(-tau/2)/tau if tau else mp.mpf('.5')
            check('transition-limit', f'tau={tau}',
                  mp.quad(lambda t: mp.exp(tau*t), [-mp.mpf('.5'), 0]), limit)


def spacings():
    for n in [4, 32, 512]:
        for xi in map(mp.mpf, ['-1', '-.75', '-.5', '-.001']):
            for p in map(mp.mpf, ['.0125', '.5', '.9875']):
                k = n*(1-p)/((n-1)*p)
                t = 1/mp.expm1(-xi*mp.log1p(k))
                recovered = mp.expm1(-mp.log1p(1/t)/xi)
                check('gev-spacing-cdf', f'n={n};xi={xi};p={p}',
                      n/(n+(n-1)*recovered), p)
                # Let m be a unit-width GPD maximum quantile.
                m = -mp.expm1(-xi*mp.log1p(-p**(mp.mpf(1)/n)))
                check('gpd-max-cdf', f'n={n};xi={xi};p={p}',
                      (-mp.expm1(-mp.log1p(-m)/xi))**n, p)


def run():
    transition()
    spacings()
    here = Path(__file__).parent
    with (here/'procedures.csv').open(newline='') as stream:
        data = list(csv.reader(stream))
    assert all(len(row) == 7 for row in data)
    assert len({tuple(row[:3]) for row in data[1:]}) == len(data) - 1
    with (here/'structure.csv').open('w', newline='') as stream:
        writer = csv.writer(stream, lineterminator='\n')
        writer.writerow(['check', 'cell', 'value', 'absolute_error'])
        writer.writerows(ROWS)
    print(f'{len(ROWS)} checks passed at 80 decimal digits, {len(data)-1} procedure rows')


if __name__ == '__main__':
    run()
