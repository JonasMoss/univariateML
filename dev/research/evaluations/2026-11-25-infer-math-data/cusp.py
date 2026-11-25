"""Null LR for raw two-piece power cusp, exact knot enumeration."""
import csv
from pathlib import Path
import numpy as np

rng = np.random.default_rng(533)
nu = .35
reps = 4000
rows = []


def lr(n, skew, count):
    results = []
    for start in range(0, count, 80):
        batch = min(count-start, 80)
        right = rng.random((batch,n)) < skew**2/(1+skew**2)
        x = rng.gamma(1/nu, size=(batch,n))**(1/nu) * np.where(right, skew, -1/skew)
        # Holding skew and exponent, profiling scale is equivalent to
        # minimizing the weighted sum of powers. It is concave between knots.
        delta = x[:, None, :] - x[:, :, None]
        sums = (np.abs(delta)**nu * np.where(delta >= 0, skew**(-nu), skew**nu)).sum(axis=2)
        at_null = (np.abs(x)**nu * np.where(right, skew**(-nu), skew**nu)).sum(axis=1)
        best = sums.min(axis=1)
        assert np.all(best <= at_null + 1e-12)
        results.extend((2*n/nu)*np.log(at_null/best))
    return np.asarray(results)


for skew in (1., 1.7):
    for n in (48, 192):
        training = lr(n, skew, 7999)
        q = np.sort(training)[7599]
        validation = lr(n, skew, reps)
        for method, cutoff in [('independent null LR', q), ('ordinary chi-square LR', 3.841458820694124)]:
            hits = int((validation <= cutoff).sum())
            p = hits/reps
            if method == 'independent null LR':
                assert abs(p-.95) < .02
            rows.append((n, nu, skew, method, cutoff, reps, hits, p, np.sqrt(p*(1-p)/reps)))

with Path(__file__).with_name('cusp.csv').open('w', newline='') as output:
    w = csv.writer(output)
    w.writerow(('n','nu','skew','method','cutoff','replications','accepted','coverage','mcse'))
    w.writerows(rows)
print(f'{len(rows)} cusp coverage cells passed')
