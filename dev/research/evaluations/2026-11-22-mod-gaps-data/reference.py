"""Independent 100-digit conditional NB reference; run with mpmath."""
import json

import mpmath as mp

mp.mp.dps = 100
size = mp.mpf(10**8)
prob = mp.mpf('0.5')
results = {}
for a, b in [(2, 12), (0, 5)]:
    counts = list(range(a, b + 1))
    logs = [
        mp.loggamma(size + k) - mp.loggamma(size) - mp.loggamma(k + 1)
        + size * mp.log(prob) + k * mp.log1p(-prob)
        for k in counts
    ]
    mass = mp.fsum(mp.exp(v) for v in logs)
    pmf = [mp.exp(v) / mass for v in logs]
    cdf = [mp.fsum(pmf[:i + 1]) for i in range(len(counts))]
    survival = [mp.fsum(pmf[i + 1:]) for i in range(len(counts))]
    targets = [mp.mpf(t) for t in ('1e-60', '1e-12', '1e-7', '0.5', '0.9', '0.9999999999')]
    results[f'{a}:{b}'] = {
        'log_mass': mp.nstr(mp.log(mass), 70),
        'log_pmf': [float(mp.log(v)) for v in pmf[:-1]]
                   + [float(mp.log1p(-mp.fsum(pmf[:-1])))],
        'log_cdf': [float(mp.log(v)) for v in cdf[:-1]] + [0.0],
        'log_survival': [float(mp.log(v)) for v in survival[:-1]],
        'probabilities': [float(v) for v in targets],
        'lower_quantiles': [next(k for k, f in zip(counts, cdf) if f >= t) for t in targets],
        'upper_quantiles': [next(k for k, s in zip(counts, survival) if s <= t) for t in targets],
    }
print(json.dumps(results, indent=2))
