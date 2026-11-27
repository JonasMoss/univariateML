"""Replay TASK-448's exact witnesses, numerical values and proposal inventory."""

import argparse
import csv
from fractions import Fraction
import json
from pathlib import Path
import re

import mpmath as mp


DATA = Path(__file__).resolve().parent
REPORT = DATA.with_name("2026-11-27-trunc-est-math-report.md")
RESEARCH = DATA.parent.parent
ROOT = RESEARCH.parent.parent
ATLAS = RESEARCH / "existence-atlas"
FIELDS = ("family", "held", "modifier", "question", "claim", "direction",
          "status", "evidence", "frontier", "origin")
REGIMES = ("truncate:lower-estimated", "truncate:upper-estimated",
           "truncate:both-estimated")
QUESTIONS = ("criterion", "failure", "uniqueness")
PRECISION = 70


def proof_rows():
    with (ATLAS / "modifier-proofs.csv").open(newline="") as stream:
        known = [row for row in csv.DictReader(stream)
                 if row["modifier"] == "truncate:known"]
    assert len(known) == 90
    expected = {(row["family"], row["held"], modifier, row["question"])
                for row in known for modifier in REGIMES}
    pattern = re.compile(
        r"^### (\S+) (\S+) (truncate:\S+)\n"
        r"\nHere ([^\n]+)\n"
        r"\n\*\*Criterion\.\*\* ([^\n]+)\n"
        r"\n\*\*Failure\.\*\* ([^\n]+)\n"
        r"\n\*\*Uniqueness\.\*\* ([^\n]+)$", re.MULTILINE,
    )
    report = REPORT.read_text()
    rows = []
    for match in pattern.finditer(report):
        family, mask, modifier, window, *claims = match.groups()
        held = "" if mask == "free" else mask
        anchor = f"{family}-{mask}-{modifier.replace(':', '')}"
        evidence = f"{REPORT.relative_to(ROOT)}#{anchor}"
        for question, claim in zip(QUESTIONS, claims, strict=True):
            rows.append(dict(zip(FIELDS, (
                family, held, modifier, question, f"{window} {claim}",
                "iff" if question == "criterion" else "exact",
                "written", evidence, "", "trunc-est-math",
            ), strict=True)))
    found = {tuple(row[field] for field in FIELDS[:4]) for row in rows}
    assert len(rows) == len(found) == 270 and found == expected
    assert len({row["evidence"] for row in rows}) == 90
    with (ATLAS / "modifier-atlas.csv").open(newline="") as stream:
        atlas = {(row["family"], row["held"], row["modifier"])
                 for row in csv.DictReader(stream)}
    assert {(f, h or "all free", m) for f, h, m, _ in found} <= atlas
    return rows


def bisect(function, left, right, steps=200):
    """Bisect an increasing function with opposite endpoint signs."""
    for _ in range(steps):
        middle = (left + right) / 2
        if function(middle) > 0:
            right = middle
        else:
            left = middle
    return (left + right) / 2


def fraction_text(value):
    return str(value.numerator) + "/" + str(value.denominator)


def numerical_checks():
    mp.mp.dps = PRECISION

    # The exact sign and gap bound use only rationals.
    d = Fraction(349525, 1048576)
    second = (1 + 3 * d**2) / 4
    delta = Fraction(1, 3) - second
    gap_bound = delta**2 / Fraction(4, 25)
    assert 0 < delta < Fraction(1, 1_000_000)
    assert gap_bound < Fraction(1, 100_000_000)
    assert (1 + 3 * Fraction(1, 3)**2) / 4 == Fraction(1, 3)
    normal_target = mp.mpf(second.numerator) / second.denominator

    def normal_moment(t):
        normalizer = mp.quad(lambda x: mp.exp(t * x**2), [0, 1])
        moment = mp.quad(lambda x: x**2 * mp.exp(t * x**2), [0, 1])
        return moment / normalizer

    normal_left = -mp.mpf(1) / 100
    assert normal_moment(normal_left) < normal_target < normal_moment(0)
    normal_tilt = bisect(lambda t: normal_moment(t) - normal_target,
                         normal_left, mp.mpf(0))
    normal_gain = (normal_tilt * normal_target -
                   mp.log(mp.quad(lambda x: mp.exp(normal_tilt * x**2), [0, 1])))
    assert 0 < normal_gain < mp.mpf(gap_bound.numerator) / gap_bound.denominator

    gamma_mean = mp.quad(lambda x: mp.mpf(1), [1, 4]) / mp.log(4)
    assert gamma_mean < mp.mpf(5) / 2
    beta_lo, beta_hi = mp.mpf(1) / 4, mp.mpf(3) / 4
    beta_z = mp.quad(lambda x: 1 / (x * (1 - x)), [beta_lo, beta_hi])
    beta_e = mp.quad(lambda x: mp.log(x) / (x * (1 - x)),
                     [beta_lo, beta_hi]) / beta_z
    beta_sample = (mp.log(beta_lo) + mp.log(beta_hi)) / 2
    assert beta_sample < beta_e

    ig_z = mp.quad(lambda x: x**(-mp.mpf(3)/2), [1, 4])
    ig_x = mp.quad(lambda x: x**(-mp.mpf(1)/2), [1, 4]) / ig_z
    ig_recip = mp.quad(lambda x: x**(-mp.mpf(5)/2), [1, 4]) / ig_z
    assert mp.almosteq(ig_x, 2) and mp.almosteq(ig_recip, mp.mpf(7) / 12)
    assert Fraction(5, 2) > 2 and Fraction(5, 8) > Fraction(7, 12)

    nb_mean = Fraction(2 + 2 * 3, 2 + 3)
    assert nb_mean == Fraction(8, 5) < Fraction(5, 3)
    ns = [2, 3, 4, 17, 100]
    for n in ns:
        p = Fraction(1, n)
        assert (n - 1) * p / (2 * (1 - p)) == Fraction(1, 2)

    alias_results = []
    for sample in [(1, 2), (2, 3)]:
        mean = mp.mpf(sum(sample)) / len(sample)

        def log_series_mean(p):
            return (1 - p) / (-p * mp.log(p))

        left, right = mp.mpf("0.0001"), mp.mpf("0.9999")
        assert log_series_mean(right) < mean < log_series_mean(left)
        prob = bisect(lambda p: mean - log_series_mean(p), left, right)
        assert abs(log_series_mean(prob) - mean) < mp.mpf("1e-58")
        ll = mp.fsum(k * mp.log1p(-prob) - mp.log(k)
                     - mp.log(-mp.log(prob)) for k in sample)
        assert mp.isfinite(ll)
        alias_results.append(dict(sample=list(sample), prob=mp.nstr(prob, 35),
                                  log_likelihood=mp.nstr(ll, 35)))

    return {
        "mpmath_decimal_digits": PRECISION,
        "bisection_steps": 200,
        "normal": {
            "sample": ["-1", "-d", "-d", "-d", "d", "d", "d", "1"],
            "d": fraction_text(d),
            "variance_gap_exact": fraction_text(delta),
            "quadratic_tilt": mp.nstr(normal_tilt, 35),
            "likelihood_gain_per_observation": mp.nstr(normal_gain, 35),
            "gain_upper_bound_exact": fraction_text(gap_bound),
        },
        "gamma_corner": {"sample": [1, 4], "model_mean": mp.nstr(gamma_mean, 35)},
        "beta_corner": {"sample": ["1/4", "3/4"],
                        "model_log_mean": mp.nstr(beta_e, 35),
                        "sample_log_mean": mp.nstr(beta_sample, 35)},
        "invgauss_corner": {"sample": [1, 4], "model_mean_X": mp.nstr(ig_x, 35),
                            "model_mean_reciprocal": mp.nstr(ig_recip, 35)},
        "nbinom_weighted_boundary": {"sample": [1, 2, 2], "size": 2,
                                     "boundary_mean_exact": fraction_text(nb_mean)},
        "binom_plateau": {"sample": [1, 1, 2], "sizes_replayed": ns,
                          "odds_exact": "1/2"},
        "nbinom_size_zero_alias": alias_results,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true", help="refresh checked tables")
    args = parser.parse_args()
    rows = proof_rows()
    results = numerical_checks()
    results_path = DATA / "checks.json"
    proofs_path = DATA / "proofs.csv"
    if args.write:
        results_path.write_text(json.dumps(results, indent=2) + "\n")
        with proofs_path.open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=FIELDS, lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
    else:
        assert json.loads(results_path.read_text()) == results
        with proofs_path.open(newline="") as stream:
            reader = csv.DictReader(stream)
            assert tuple(reader.fieldnames) == FIELDS
            assert list(reader) == rows
    print("270 written claims, 90 regime anchors, exact and 70-digit checks passed")


if __name__ == "__main__":
    main()
