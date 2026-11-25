"""Reproduce the census and check selected identities at 80 decimal digits."""
import csv
from pathlib import Path
import re

import mpmath as mp

HERE = Path(__file__).resolve().parent
EVAL = HERE.parent
ROOT = HERE.parents[3]
PROOF = {"closed-form", "proof-exact-comparison", "proof-practical-margin"}
UNIQUE = {"universal-proof", "per-input-certificate"}
with (EVAL / "2026-11-24-claims-inventory-data/ledger.csv").open() as stream:
    ledger = list(csv.DictReader(stream))

families = {row["family"] for row in ledger}
covered = {
    row["family"] for row in ledger
    if row["outcome"] == "estimate"
    and ((row["global_backing"] in PROOF and row["uniqueness_basis"] in UNIQUE)
         or (row["global_backing"] == "proof-exact-comparison"
             and row["uniqueness_basis"] == "typed-set"))
}
assert len(families) == 86 and len(families - covered) == 51
with (HERE / "triage.csv").open() as stream:
    reader = csv.DictReader(stream)
    assert reader.fieldnames == ["family", "hold pattern", "claim", "evidence", "source"]
    triage = list(reader)
observed = {row["family"] for row in triage}
assert len(triage) == 51 and observed == families - covered, (len(triage), sorted((families - covered) - observed), sorted(observed - (families - covered)))
for row in triage:
    assert all(row.values())
    for source in row["source"].split(" | "):
        filename, line = source.rsplit(":", 1)
        assert 1 <= int(line) <= len((ROOT / filename).read_text().splitlines()), source
report = EVAL / "2026-11-25-transfer-math-report.md"
for target in re.findall(r"\]\(([^)]+)\)", report.read_text()):
    if not target.startswith("https://"):
        assert (EVAL / target.split("#", 1)[0]).is_file(), target

mp.mp.dps = 80
grid = [mp.mpf(j) / 4 for j in range(-16, 17)]
tol = mp.mpf("1e-73")
for z in grid:
    x = mp.exp(z)
    p = x / (1 + x)
    assert abs(mp.log(p / (1 - p)) - z) < tol
    for c in map(mp.mpf, ("0.2", "1", "1.5", "4")):
        a = mp.mpf("1.3")
        burr = lambda y, s: a * c / y * (y / s)**c / (1 + (y / s)**c)**(a + 1)
        s = mp.mpf("2.5")
        inverse = a * c / x * (s / x)**c / (1 + (s / x)**c)**(a + 1)
        assert abs(mp.log(inverse) - (mp.log(burr(1 / x, 1 / s)) - 2*z)) < tol
        exponentiated = lambda w: w - mp.exp(w) + (c - 1)*mp.log(-mp.expm1(-mp.exp(w)))
        pgw = lambda w: w + (c - 1)*mp.log1p(mp.exp(w)) - (1 + mp.exp(w))**c
        assert mp.diff(exponentiated, z, 2) < 0
        assert mp.diff(pgw, z, 2) < 0
        for power in map(mp.mpf, ("1", "1.5", "4")):
            physical = lambda y: pgw(power*mp.log(y)) - mp.log(y)
            if power*c >= 1:
                assert mp.diff(physical, x, 2) < tol
        fatigue = lambda w: mp.log(mp.cosh(w / 2)) - (mp.cosh(w) - 1)/c**2
        expected = 1/(4*mp.cosh(z / 2)**2) - mp.cosh(z)/c**2
        assert abs(mp.diff(fatigue, z, 2) - expected) < tol

# Exact Huber interval formula checked at its endpoints and midpoint.
epsilon = mp.mpf("0.5")
rho = lambda t: t*t/2 if abs(t) <= epsilon else epsilon*abs(t) - epsilon**2/2
likelihood = lambda mu, sigma: -2*mp.log(sigma) - rho(-mu/sigma) - rho((1-mu)/sigma)
assert likelihood(mp.mpf(1)/8, mp.mpf(1)/4) == likelihood(mp.mpf(1)/2, mp.mpf(1)/4)
assert likelihood(mp.mpf(7)/8, mp.mpf(1)/4) == likelihood(mp.mpf(1)/2, mp.mpf(1)/4)
assert likelihood(0, mp.mpf(1)/4) < likelihood(mp.mpf(1)/2, mp.mpf(1)/4)

# At the Tukey transition the maximizing hyperbolic ratio touches one.
d, v = mp.findroot(
    lambda d, v: (d/mp.tanh(d*v) - mp.tanh(v),
                  mp.sinh(d*v)**2/mp.cosh(v)**2 - d/(2+d)),
    (mp.mpf("0.75"), mp.mpf("1.8")), tol=tol
)
assert 0 < d < 1 and v > 0
assert abs(d - mp.mpf(2)/3) < tol
assert abs(mp.sinh(v/3) - mp.mpf(1)/2) < tol
assert abs(d/mp.tanh(d*v) - mp.tanh(v)) < tol
assert abs(mp.sinh(d*v)**2/mp.cosh(v)**2 - d/(2+d)) < tol
print(f"Ledger: {len(families)} families, {len(triage)} triage rows")
print(f"80-digit grid checks passed at {len(grid)} log observations and 4 shapes")
print(f"Tukey transition lambda = {mp.nstr(2+d, 35)}")
