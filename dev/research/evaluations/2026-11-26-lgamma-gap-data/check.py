"""360-digit direct log likelihood and score for exact binary64 inputs."""
import itertools
import json
import subprocess
import sys
from functools import lru_cache

import mpmath as mp

mp.mp.dps = 360
POWERS = (2, 4, 16, 128, 512, 1023)
COUNTS = (10, 15, 31, 127)


@lru_cache(None)
def reference(power, count, ulps, mask):
    outlier = mp.log(1 + mp.mpf(ulps) * mp.power(2, -52))
    anchor = power * mp.log(2)
    total = outlier + count * anchor
    log_total = mp.log(outlier) + count * mp.log(anchor)
    n = count + 1
    mean = total / n
    gap = mp.log(mean) - log_total / n
    if mask & 1:
        shape = mp.mpf(2)
    else:
        def score(z):
            k = mp.exp(z)
            rate = 3 if mask & 2 else k / mean
            return mp.log(rate) - mp.digamma(k) + log_total / n

        shape = mp.exp(mp.findroot(score, (-2, -1), maxsteps=100))
    rate = mp.mpf(3) if mask & 2 else shape / mean
    ll = n * (shape * mp.log(rate) - mp.loggamma(shape)) + (shape - 1) * log_total - (rate + 1) * total
    return shape, rate, ll, gap


def main():
    cases = list(itertools.product(POWERS, COUNTS, (1, 2, 8), (0, 1), range(4)))
    inputs = ''.join(' '.join(map(str, case)) + '\n' for case in cases)
    output = subprocess.run([sys.argv[1]], input=inputs, text=True, capture_output=True, check=True)
    rows = []
    for (power, count, ulps, position, mask), line in zip(cases, output.stdout.splitlines(), strict=True):
        shape, rate, ll, gap = reference(power, count, ulps, mask)
        status, certified, unique, *values = line.split()
        error = None if status != '0' else max(
            abs(mp.mpf(float(value)) - exact) / scale
            for value, exact, scale in zip(values, (shape, rate, ll), (shape, rate, max(1, abs(ll)))))
        rows.append(dict(power=power, count=count, ulps=ulps, outlier_last=bool(position), mask=mask,
                         reference=[mp.nstr(v, 70) for v in (shape, rate, ll, gap)],
                         status=int(status), certified=bool(int(certified)), unique=bool(int(unique)),
                         fitted=values, max_error=None if error is None else float(error)))
    with open(sys.argv[2], 'w') as stream:
        for row in rows:
            print(json.dumps(row), file=stream)
    for mask in range(4):
        subset = [r for r in rows if r['mask'] == mask]
        print(mask, 'stops', sum(r['status'] != 0 for r in subset),
              'outside tolerance', sum((r['max_error'] or 0) > 1e-10 for r in subset),
              'max error', max(r['max_error'] or 0 for r in subset))


if __name__ == '__main__':
    main()
