"""Exact fixture checks and numerical corroboration (80 decimal digits)."""
from fractions import Fraction as F
from math import comb
import mpmath as mp
import sympy as sp

mp.mp.dps = 80


def hyper(x, N, k, m):
    if not max(0, m+k-N) <= x <= min(m, k):
        return F(0)
    return F(comb(m, x) * comb(N-m, k-x), comb(N, k))


def nhyper(x, N, m, r):
    return F(comb(x-1, r-1) * comb(N-x, m-r), comb(N, m))


for N in range(2, 50, 2):
    assert hyper(0, N, 1, N//2) * hyper(1, N, 1, N//2) == F(1, 4)
    assert hyper(0, N, 1, N//2) == hyper(0, N, N//2, 1)
x = [1, 1, 1, 3]
face = F(2, 3)**4 * F(1, 3)**2
assert max(
    nhyper(1, N, m, 1)**3 * nhyper(3, N, m, 1)
    for N in range(3, 10) for m in range(1, N-1)
) / face == F(455625, 458752)
assert nhyper(1, 5, 2, 1)**2 * nhyper(4, 5, 2, 1) / F(1, 2)**6 == F(128, 125)

p, h = sp.symbols("p h")
assert sp.series((1-sp.exp(-p*h))/(1-sp.exp(-h)), h, 0, 3).removeO().expand() == (
    p - p*(p-1)*h/2 + p*(p-1)*(2*p-1)*h*h/12
).expand()

# Check mode-centered GB2 formula and transfers to all faces.
def beta_log_density(d, a, A, B):
    p, q = A/a, B/a
    z = mp.log(A/B) + a*d
    return mp.log(a) - mp.log(mp.beta(p, q)) + p*z - (p+q)*mp.log1p(mp.exp(z))


def centered_score(d, a, A, B):
    return A*B*mp.expm1(a*d)/(B+A*mp.exp(a*d))


def gg_log_density(d, t, k):
    return mp.log(t) + k*mp.log(k)-k-mp.loggamma(k) - k*(mp.expm1(t*d)-t*d)


grid = list(map(mp.mpf, ["-2", "-.5", "0", ".5", "2"]))
for a in map(mp.mpf, [".01", "1", "50"]):
    for A, B in [(mp.mpf(".7"), mp.mpf("3")), (mp.mpf("4"), mp.mpf(".2"))]:
        for d in grid:
            assert abs(mp.diff(lambda x: beta_log_density(x, a, A, B), d)
                       + centered_score(d, a, A, B)) < mp.mpf("1e-72")

eps = mp.mpf("1e-16")
a = 1/eps
normal = lambda d: -(mp.log(2*mp.pi)+d*d)/2
al = lambda d: mp.log(mp.mpf(6)/5) - 2*max(-d,0) - 3*max(d,0)
for d in grid:
    assert abs(beta_log_density(d, eps, 2/eps, 2/eps)-normal(d)) < 10*eps
    assert abs(beta_log_density(d, 1, 1/eps, mp.mpf(2)) -
               gg_log_density(-d, mp.mpf(1), mp.mpf(2))) < 100*eps
    assert abs(beta_log_density(d, a, mp.mpf(2), mp.mpf(3))-al(d)) < 20*eps
for d in grid[:3]:
    assert abs(beta_log_density(d, a, mp.mpf(2), a) - (mp.log(2)+2*d)) < 100*eps
    assert abs(gg_log_density(d, a, 2/a) - (mp.log(2)+2*d)) < 100*eps
assert beta_log_density(mp.mpf(".5"), a, mp.mpf(2), a) < -100
assert gg_log_density(mp.mpf(".5"), a, 2/a) < -100

c = mp.mpf("1e-8")
Z = 2*mp.quad(lambda t: mp.exp(-t*t/2), [0,c]) + 2*mp.exp(-c*c/2)/c
assert abs((c*c/2-mp.log(c*Z/2))/c**4 - mp.mpf(1)/6) < mp.mpf("1e-15")

# GPD critical scale likelihood on (0, 1) at xi=1.
for s in map(mp.mpf, [".1", ".01", ".001"]):
    factor = (1+s)**-2
    assert abs((1/s)**2*(1+1/s)**-2-factor) < mp.mpf("1e-75")
    assert factor < 1

print("Exact fixture and symbolic checks passed.")
print("80-digit checks passed: GB2 scores and five log-density points per face,")
print("generalized-gamma contact, Huber all-tail expansion, GPD critical trace.")

import csv
from pathlib import Path
import re

data_dir = Path(__file__).resolve().parent
evaluations = data_dir.parent
atlas_dir = evaluations.parent / "existence-atlas"
report = evaluations / "2026-11-27-crit-open-math-report.md"


def read_csv(path):
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def anchors(path):
    headings = re.findall(r"^#{1,6} (.+)$", path.read_text(), re.M)
    return {re.sub(r"[^\w -]", "", heading.lower()).replace(" ", "-")
            for heading in headings}


columns, rows = read_csv(data_dir / "proofs.csv")
assert columns == read_csv(atlas_dir / "proofs.csv")[0]
_, atlas = read_csv(atlas_dir / "atlas.csv")
masks = {(row["family"], row["held"]) for row in atlas}
expected = {(family, held) for family, held in (
    ("bbinom", ""), ("betapr", "shape1"), ("betapr", "shape2"), ("bnbinom", ""),
    ("gb2", ""), ("gengamma", ""), ("gengamma", "mu"), ("gpd", "mu+xi"),
    ("huber", ""), ("hyper", ""), ("hyper", "k"), ("hyper", "m"),
    ("nhyper", ""), ("st", ""), ("tlambda", ""),
)}
keys = [(row["family"], row["held"], row["question"]) for row in rows]
assert len(set(keys)) == len(keys)
for question in ("criterion", "failure"):
    assert {(family, held) for family, held, tag in keys if tag == question} == expected
for row in rows:
    assert (row["family"], row["held"] or "all free") in masks
    path, anchor = row["evidence"].split("#")
    assert path == f"dev/research/evaluations/{report.name}"
    assert anchor in anchors(report), row
    assert row["status"] == "written"
    assert row["direction"] in {"iff", "exact", "one-sided"}

for link in re.findall(r"\]\(([^)\s]+)\)", report.read_text()):
    rel_path, _, anchor = link.partition("#")
    path = evaluations / rel_path
    assert path.is_file(), link
    if anchor and path.suffix == ".md":
        assert anchor in anchors(path), link
    if anchor and path.suffix == ".tex":
        assert rf"\label{{{anchor}}}" in path.read_text(), link

print(f"All {len(rows)} atlas rows and all report links validated.")
