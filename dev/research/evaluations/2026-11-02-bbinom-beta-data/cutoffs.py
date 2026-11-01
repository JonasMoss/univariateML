"""Reproduce the held-beta cutoff workload; standard Python, seed 113."""
import csv
import random
import math
from pathlib import Path

rng = random.Random(113)
out = []
for n, a, b, h in [(5, 2, .5, 30), (10, 2, 3, 30),
                    (20, .5, 8, 30), (40, 5, 20, 100),
                    (10, 2, 3, 100)]:
    for rep in range(100):
        xs = []
        for _ in range(h):
            p = rng.betavariate(a, b)
            xs.append(sum(rng.random() < p for _ in range(n)))
        maximum = max(xs)
        m = sum(x > 0 for x in xs)
        if maximum == 0 or min(xs) == maximum:
            out.append((n, a, b, h, rep, maximum, m, 'face'))
            continue
        H = sum(1 / (b + j) for j in range(maximum + 1))
        k = maximum
        while b > 1 and m * (k + 1 - maximum) < h * maximum * (b - 1) * H:
            k += 1
            H += 1 / (b + k)
            if k > 1000000:
                break
        out.append((n, a, b, h, rep, maximum, m, k))
path = Path(__file__).with_name('cutoffs.csv')
with path.open('w', newline='') as stream:
    writer = csv.writer(stream)
    writer.writerow(('true_size','alpha','beta','sample_size','rep','maximum','positive','cutoff'))
    writer.writerows(out)
for params in sorted({row[:4] for row in out}):
    rows = [row for row in out if row[:4] == params]
    values = sorted(row[-1] for row in rows if isinstance(row[-1], int))
    print(params, 'median', values[len(values)//2] if values else None,
          'range', (min(values), max(values)) if values else None,
          'over 1000', sum(v > 1000 for v in values), 'faces', len(rows)-len(values))
