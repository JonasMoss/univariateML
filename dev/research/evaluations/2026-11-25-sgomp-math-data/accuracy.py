"""Check archived public coefficients against fresh roots on the exact inputs."""
from fractions import Fraction
import gzip
import hashlib
import json
import math
from pathlib import Path

import mpmath as mp

from profile import Q, curvature, likelihood, ratescore, shapescore

ROOT = Path(__file__).resolve().parents[4]
EVAL = ROOT / "dev/research/evaluations"
GOLDEN = ROOT / "univariateML-cpp/include/cppunivariate/distributions/sgomp/golden.json"
ARCHIVE = EVAL / "2026-11-07-fit-audit-data/cases.json.gz"
SNAPSHOT = EVAL / "2026-11-15-fit-snapshot-data"
PICKS = ("006222", "006223", "006239", "006241", "006260", "golden")


def reference(x, b, eta, held):
    if eta == 0:
        if "b" not in held:
            b = len(x) / sum(x)
        if "eta" not in held:
            assert shapescore(x, b, 0) < 0
        return b, eta
    if "b" in held:
        return b, mp.findroot(lambda e: shapescore(x, b, e), eta)
    if "eta" in held:
        return mp.findroot(lambda r: ratescore(x, r, eta), b), eta
    b, eta = mp.findroot(lambda r, e: (ratescore(x, r, e), shapescore(x, r, e)),
                        (b, eta))
    assert curvature(x, b, eta) < 0
    return b, eta


def inspect(mode, archive, golden):
    with gzip.open(SNAPSHOT / f"{mode}.jsonl.gz", "rt") as stream:
        records = {}
        for line in stream:
            if "sgomp" not in line:
                continue
            row = json.loads(line)
            if row["family"] == "sgomp" and row["case"] in PICKS:
                records[row["case"]] = row
    print(mode)
    for case in PICKS:
        row = records[case]
        x = archive[int(case)]["data"] if case != "golden" else golden["data"]
        held = row["held"]
        mask = ("b" in held) + 2 * ("eta" in held)
        payload = " ".join([str(mask), str(len(x)),
                            *(float(held.get(k, 0)).hex() for k in ("b", "eta")),
                            *(float(v).hex() for v in x)]) + "\n"
        assert hashlib.sha256(payload.encode()).hexdigest()[:16] == row["input_sha256"]
        assert row["result"]["kind"] == "ok"
        b, eta = (Q(float.fromhex(row["result"]["estimates"][k])) for k in ("b", "eta"))
        for label, value in zip(("b", "eta"), (b, eta)):
            if label in held:
                assert value == Q(held[label])
        x = [Q(float(v)) for v in x]
        b_ref, eta_ref = reference(x, b, eta, held)
        err = abs(b - b_ref) / b_ref, abs(eta - eta_ref) / max(Q(1), eta_ref)
        assert max(err) < Q("1e-10")
        shift = Q(0)
        if not held and eta > 0:
            # Stress the public-coordinate issue separately. A refit conditional
            # on the emitted b is not the joint reference.
            eta_refit = mp.findroot(lambda e: shapescore(x, b, e), eta)
            shift = abs(eta_refit - eta_ref) / max(Q(1), eta_ref)
            assert shift < Q("1e-10")
            assert likelihood(x, b_ref, eta_ref) > likelihood(x, len(x) / sum(x), 0)
        print(case, *(mp.nstr(v, 5) for v in (*err, shift)))


def main():
    golden = json.loads(GOLDEN.read_text())
    with gzip.open(ARCHIVE, "rt") as stream:
        archive = {row["index"]: row for row in json.load(stream) if row["family"] == "sgomp"}
    for mode in ("native", "double"):
        inspect(mode, archive, golden)
    exact = Fraction(2, 3)
    nearest = float(exact)
    error = abs(Fraction(nearest) - exact) / exact
    assert error == Fraction(1, 2**54) > Fraction.from_float(1e-18)
    assert all(abs(Fraction(math.nextafter(nearest, way)) - exact) >=
               abs(Fraction(nearest) - exact) for way in (-math.inf, math.inf))
    print("x=(1,2), eta=0 held. Smallest possible relative error:", error)


if __name__ == "__main__":
    main()
