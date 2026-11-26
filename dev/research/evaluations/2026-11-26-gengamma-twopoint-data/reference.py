"""Evaluate the profiled power and Pareto densities at 80 decimal digits."""

import json
from pathlib import Path

import mpmath as mp

mp.mp.dps = 80
samples = {
    "four": ["1", "1", "2", "2"],
    "six": ["8.00000095367431640625", "8"] * 3,
    "near_miss": ["1", "1", "1", "2"],
}
results = {}
for name, sample in samples.items():
    x = list(map(mp.mpf, sample))
    lo, hi = min(x), max(x)
    logs = list(map(mp.log, x))
    beta = len(x) / mp.fsum(mp.log(hi) - y for y in logs)
    a = len(x) / mp.fsum(y - mp.log(lo) for y in logs)
    power = mp.fsum(mp.log(beta / hi) + (beta - 1) * mp.log(v / hi) for v in x)
    pareto = mp.fsum(mp.log(a / lo) - (a + 1) * mp.log(v / lo) for v in x)
    results[name] = {
        "data": sample,
        "power": {"alpha": sample[x.index(hi)], "beta": str(beta), "logLik": str(power)},
        "pareto": {"a": str(a), "b": sample[x.index(lo)], "logLik": str(pareto)},
    }
Path(__file__).with_suffix(".json").write_text(json.dumps(results, indent=2) + "\n")
