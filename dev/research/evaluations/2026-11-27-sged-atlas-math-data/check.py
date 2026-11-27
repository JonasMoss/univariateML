"""Strict interval inequalities and numerical checks for TASK-402."""
import csv
from fractions import Fraction
from pathlib import Path
import re
import runpy

from mpmath import iv, mp

prior = Path(__file__).resolve().parents[1] / "2026-11-25-ged-math-data"
cover = runpy.run_path(str(prior / "cover.py"))
I = cover["interval"]
iv.dps = 40


def ged_tie_inequalities():
    # The proof uses 1/(3N) < 10^-6 for N = 2^131072.
    n = 128
    gamma_upper = (
        sum(I(Fraction(1, k)) for k in range(1, n+1))
        - iv.ln(n) - I(Fraction(1, 2*n)) + I(Fraction(1, 12*n*n))
    )
    assert gamma_upper < I("0.578")
    relative_error = I("0.000001")
    low, high = I("0.103"), I("0.105")
    peak = lambda a: iv.ln(3 / (1 + a)) - 1
    assert iv.ln(3 / (1 + low + 6 * relative_error)) > 1
    assert peak(high) + relative_error < -I("0.001")
    assert peak(high) - 6 * relative_error > -I("0.002")
    assert (iv.ln(3) - iv.ln(2) - 1 + I("0.578")
            + relative_error) / 2 < -I("0.005")
    assert (1 + iv.ln(65536)) / 65536 < I("0.001")
    print("GED held-mean exact tie: compact branch and valley inequalities passed")


def reflection_inequalities():
    a = I(Fraction(1, 1024))
    mean_absolute = (1 + 3 * a) / 4
    density = 1 / (2 * iv.exp(1) * mean_absolute)
    assert density > 1 / iv.sqrt(2)
    assert 2 * iv.sqrt(6) * mean_absolute < 2
    assert 2**I(Fraction(-5, 8)) < 1 / iv.sqrt(2)
    print("SGED reflection witness: half-face and held-width inequalities passed")


def numerical_contacts():
    mp.dps = 90

    def moments(p):
        t = 1 / p
        k = mp.gamma(2*t) / mp.gamma(t)
        v = mp.gamma(3*t) / mp.gamma(t)
        return k, v, 3*v - 4*k*k

    for p in map(mp.mpf, ('0.5', '1', '2.75', '15')):
        k, v, d = moments(p)
        expected = 2*k*v / (v+d)
        endpoint = -2*k / mp.sqrt(v+d)
        for epsilon in map(mp.mpf, ('1e-12', '1e-25', '1e-40')):
            z = 1 - epsilon
            q = endpoint * mp.sqrt(v+d*z*z)/2 + k*z
            actual = -2*q / epsilon
            assert abs(actual - expected) < 5 * epsilon * (1 + expected)
    for p in map(mp.mpf, ('1e3', '1e5', '1e7')):
        k, v, d = moments(p)
        z = 1 - 1/mp.sqrt(p)
        u = mp.sqrt(3*(v+d*z*z))
        penalties = [((u+e*2*k*z)/(1+e*z))**p for e in (-1, 1)]
        assert max(abs(q - mp.exp(-mp.euler)) for q in penalties) < 10/mp.sqrt(p)
    print("90-digit replay: 12 fixed-shape contacts and 3 joint uniform paths passed")


def verify_atlas_rows():
    data = Path(__file__).resolve().parent
    repo = data.parents[3]
    atlas_dir = repo / "dev/research/existence-atlas"
    with (atlas_dir / "atlas.csv").open() as stream:
        masks = {
            (row["family"], "" if row["held"] == "all free" else row["held"])
            for row in csv.DictReader(stream)
        }
    with (atlas_dir / "proofs.csv").open() as stream:
        expected_header = next(csv.reader(stream))
    with (data / "proofs.csv").open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == expected_header
        rows = list(reader)
    sged_holds = ("", "mean", "mean+nu", "mean+nu+sd", "mean+sd",
                  "mean+sd+xi", "mean+xi", "nu", "nu+sd", "sd", "sd+xi", "xi")
    expected = {("sged", held, question) for held in sged_holds
                for question in ("criterion", "failure", "uniqueness")}
    expected.update(("ged", held, "uniqueness") for held in ("mean", "mean+sd", "sd"))
    actual = set()
    for row in rows:
        key = row["family"], row["held"], row["question"]
        assert key not in actual and key[:2] in masks
        actual.add(key)
        assert row["status"] == "written" and row["claim"]
        file, anchor = row["evidence"].split("#")
        headings = re.findall(r"^#+ (.*)$", (repo / file).read_text(), re.M)
        anchors = {re.sub(r"[^\w -]", "", h.lower()).replace(" ", "-") for h in headings}
        assert anchor in anchors, (file, anchor)
    assert actual == expected
    print(f"{len(rows)} atlas rows: masks, coverage, schema and evidence anchors passed")


if __name__ == "__main__":
    cover["main"]()
    ged_tie_inequalities()
    reflection_inequalities()
    numerical_contacts()
    verify_atlas_rows()
