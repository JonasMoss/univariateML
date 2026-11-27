"""Validate coverage of the simulation matrix and replay named screen misses."""
import csv
from itertools import product
from pathlib import Path

import numpy as np

from pilot import DESIGNS, OUT, burr, skew_ged, skew_student, symmetric_student


with (OUT / "simulation.csv").open(newline="") as file:
    rows = list(csv.DictReader(file))

expected = [(family, "/".join(map(str, parameters)), n)
            for family, designs in DESIGNS.items()
            for parameters, n in product(designs, (10, 30, 100))]
assert [(r["family"], r["parameters"], int(r["n"])) for r in rows] == expected
for r in rows:
    counts = [int(r[name]) for name in ("attain", "escape", "gap")]
    assert sum(counts) == int(r["repetitions"]) == 200 and min(counts) >= 0

student_escape = np.r_[[-1.5], np.repeat(-1.,99), np.repeat(1.,99), [1.5]]
std_attained = np.sort(np.repeat([-1., -3/16, 3/16, 1.], [2,5,5,2]))
lst_attained = np.sort(np.array([-4,-3,-2,-1,1,2,3,4,-101,-99,99,101])/100)
sstd_attained = np.array([-67,-47,-38,-27,-20,-19,-8,-8,-8,-7,-6,-6,-5,0,15,17,22,27,74,85])
sstd_escape = np.r_[np.zeros(6), 2*np.sqrt(2), np.repeat(3.,4)]
sged_escape = np.r_[np.zeros(7), 4., 5.]
burr_escape = np.array([0, .23826/32, 7/480, 8/480, 8/480, 1, 1])

cases = (
    ("std, finite", symmetric_student(std_attained, 2)),
    ("std, normal", symmetric_student(student_escape, 2)),
    ("lst, finite", symmetric_student(lst_attained, 1)),
    ("lst, normal", symmetric_student(student_escape, 1)),
    ("sstd, finite", skew_student(sstd_attained)),
    ("sstd, half-normal", skew_student(sstd_escape)),
    ("sged, half-Laplace", skew_ged(sged_escape)),
    ("burr, Pareto", burr(burr_escape)),
    ("invburr, power on reciprocal logs", burr(burr_escape)),
)
for name, label in cases:
    assert label == "gap", (name, label)
    print(name, "missed by the screen")
print(len(expected), "design cells;", sum(int(r["repetitions"]) for r in rows), "replicates")

report = OUT.parent / "2026-11-27-checkable-math-report.md"
if report.exists():
    headers = {line[4:].strip().replace(" ", "-") for line in report.read_text().splitlines()
               if line.startswith("### ")}
    with (OUT / "proofs.csv").open(newline="") as file:
        proof_reader = csv.DictReader(file)
        proofs = list(proof_reader)
    assert proof_reader.fieldnames == [
        "family", "held", "question", "claim", "direction", "status", "evidence", "frontier", "origin"]
    assert {(r["family"], r["held"], r["question"]) for r in proofs} == {
        (family, "", question) for family in DESIGNS for question in ("criterion", "failure")}
    assert len(proofs) == 12
    for r in proofs:
        path, anchor = r["evidence"].split("#")
        assert Path(path).resolve() == report.resolve() and anchor in headers
        assert r["status"] == "written" and r["origin"] == "TASK-425"
    print(len(proofs), "proof rows with valid report anchors")
