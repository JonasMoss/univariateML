"""Generate the route table, audit its source pointers, and illustrate the GPD tie."""
import csv
from collections import Counter
from pathlib import Path

import mpmath as mp

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
DIST = ROOT / "univariateML-cpp/include/cppunivariate/distributions"

with (HERE.parent / "2026-11-24-claims-inventory-data/ledger.csv").open() as src:
    ledger = list(csv.DictReader(src))
groups = {
    "default requiring evidence": [
        row for row in ledger if "unique-default-unsubstantiated" in row["mismatch"]
    ],
    "per-input": [r for r in ledger if r["uniqueness_basis"] == "per-input-certificate"],
    "universal": [r for r in ledger if r["uniqueness_basis"] == "universal-proof"],
}
assert [len(rows) for rows in groups.values()] == [291, 13, 150]

with (HERE / "routes.tsv").open() as src:
    reader = csv.DictReader(src, delimiter="\t")
    routes = list(reader)
    fields = reader.fieldnames

sources = set()
for row in routes:
    assert all(row[field].strip() for field in fields)
    path, line = row["certificate file:line"].rsplit(":", 1)
    assert 1 <= int(line) <= len((DIST / path).read_text().splitlines()), row
    sources.add(path)
assert {str(p.relative_to(DIST)) for p in DIST.glob("*/certificate.hpp")} <= sources
assert len({(r["family"], r["hold mask"]) for r in routes}) == len(routes)
for label, rows in groups.items():
    covered_families = {r["family"] for r in routes}
    overlap = [r for r in rows if r["family"] in covered_families]
    print(label, len(rows), len(overlap), "in route-table families")
    print(", ".join(f"{key}:{value}" for key, value in
                    sorted(Counter(r["family"] for r in overlap).items())))
print("route decisions", dict(Counter(r["proves-uniqueness"] for r in routes)))

mp.mp.dps = 80

def boundary(k):
    b = k * mp.exp(1 + k)
    if not b:
        return (3 / mp.e - 1) / 2
    return mp.expm1((3 * k - mp.log1p(b)) / 2) / b

def gain(k, a):
    b = k * mp.exp(1 + k)
    d = ((2 * mp.log1p(b * a) + mp.log1p(b)) / (3 * b)
         if b else (2 * a + 1) / 3)
    return -mp.log(d) - b * d - 1

grid = [mp.mpf(j) / 200 for j in range(-199, 1601)]
peak = max(grid, key=boundary)
k = mp.findroot(lambda x: mp.diff(boundary, x), (peak - mp.mpf(".01"), peak + mp.mpf(".01")))
a = boundary(k)
assert all(boundary(t) <= a for t in grid)
assert abs(gain(k, a)) < mp.mpf("1e-70")
assert abs(mp.diff(lambda x: gain(x, a), k)) < mp.mpf("1e-70")
assert mp.diff(lambda x: gain(x, a), k, 2) < 0
print("GPD k, a, sigma", *(mp.nstr(v, 30) for v in (k, a, mp.exp(-1 - k))))
for da in (mp.mpf("-1e-5"), mp.mpf("1e-5")):
    root = mp.findroot(lambda x: mp.diff(lambda z: gain(z, a + da), x), (k, k + mp.mpf(".01")))
    delta = 3 * gain(root, a + da)
    assert delta * da < 0
    print("perturbation and local stationary log likelihood", da, mp.nstr(delta, 30))

with (HERE / "uniqueness-by-certificate.csv").open("w", newline="") as dst:
    writer = csv.DictWriter(dst, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    writer.writerows(routes)
