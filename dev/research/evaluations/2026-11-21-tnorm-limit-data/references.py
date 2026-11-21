"""Compute face MLEs for the exact binary64 inputs in norm/test.cpp."""
import gzip
import json
from pathlib import Path

import mpmath as mp

mp.mp.dps = 60


def finite(x, center=None):
    a, b = min(x), max(x)
    w = b - a
    u = [(v - a) / w for v in x]
    m = mp.fsum(u) / len(x) if center is None else (center - a) / w
    t = mp.findroot(lambda t: 1 / t - 1 / mp.expm1(t) - m, 2)
    rate = -t / w
    ll = len(x) * (mp.log(abs(rate)) - t * m - mp.log(abs(mp.expm1(-t))))
    return rate, ll


archive = [mp.mpf(float.fromhex(s)) for s in (
    '-0x1.4f4831e30a680p-2', '-0x1.4f4831e308f75p-2',
    '-0x1.4f4831e30786bp-2', '-0x1.4f4831e306161p-2',
    '-0x1.4f4831e30334cp-2',
)]
a, w = mp.mpf(1), mp.mpf(2) ** -40
window = [a, a, a, a + w]
rate = len(window) / w
for name, values in (
    ('archive', finite(archive)),
    ('finite window', finite(window)),
    ('one-sided window (rate magnitude)', (rate, len(window) * (mp.log(rate) - 1))),
    ('balanced finite window', (mp.mpf(0), -2 * mp.log(w))),
):
    print(name, *(mp.nstr(v, 32) for v in values))

rounded_center = mp.mpf(float(mp.fsum(archive) / len(archive)))
print("archive with rounded absolute mean", *(mp.nstr(v, 32) for v in finite(archive, rounded_center)))

root = Path(__file__).resolve().parents[4]
with gzip.open(root / "dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz", "rt") as f:
    record = next(c for c in json.load(f) if c["index"] == 7779)
two = [mp.mpf(v) for v in record["data"]]
mean = mp.fsum(two) / len(two)
d = mean - min(two)
variance = mp.fsum((v - mean)**2 for v in two) / len(two)
print("7779 rate", mp.nstr(1 / d, 32), "loglik", mp.nstr(2 * (-mp.log(d) - 1), 32))
print("7779 score in quadratic direction", mp.nstr(variance - d**2))
