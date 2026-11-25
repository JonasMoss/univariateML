"""Check transfer rounding and independently solve against stored public fits."""

from fractions import Fraction
import gzip
import json
from pathlib import Path

import mpmath as mp


EVALUATIONS = Path(__file__).resolve().parents[1]
ARCHIVE = EVALUATIONS / "2026-11-07-fit-audit-data" / "cases.json.gz"
SNAPSHOTS = EVALUATIONS / "2026-11-15-fit-snapshot-data"
M = mp.mpf
mp.mp.dps = 100
mp.iv.dps = 85


def mean(values):
    return mp.fsum(values) / len(values)


def show(value):
    return mp.nstr(value, 15)


def held_scale_rounding():
    """Interval signs for the unique true and rounded-input roots."""
    iv = mp.iv
    unit = 2**54

    def solve(offsets):
        z = [unit * iv.ln(1 + iv.mpf(v) / unit) for v in offsets]

        def parts(scaled_power):
            soft = [iv.ln(1 + iv.exp(scaled_power * v)) for v in z]
            rates = [v / (1 + iv.exp(-scaled_power * v)) for v in z]
            return sum(soft) / 2, sum(rates) / 2

        def sign(at):
            b = iv.mpf(at)
            a, ap = parts(b)
            return 1 / b + sum(z) / 2 - (1 + 1 / a) * ap

        lo, hi = Fraction(1, 1024), Fraction(8)
        assert sign(float(lo)).a > 0 and sign(float(hi)).b < 0
        for _ in range(35):
            mid = (lo + hi) / 2
            score = sign(float(mid))
            if score.a > 0:
                lo = mid
            else:
                assert score.b < 0
                hi = mid
        low, high = float(lo), float(hi)
        box = iv.mpf([low, high])
        alpha = 1 / parts(box)[0]
        print("held scale offsets", offsets, "power / 2^54 in", (low, high),
              "shape1 in", show(M(alpha._mpi_[0])), show(M(alpha._mpi_[1])))
        return box

    x = (-5, 10)
    offsets = tuple(int(float(v) + float(unit)) - unit for v in x)
    assert offsets == (-4, 8)
    true_power = solve(x)
    rounded_power = solve(offsets)
    assert (rounded_power / true_power - 1).a > iv.mpf(".22")

    x = (5, 9)
    offsets = tuple(int(float(v) + float(unit)) - unit for v in x)
    assert offsets == (4, 8)
    exponents = [1 / mean([mp.log1p(M(v) / unit) for v in u]) for u in (x, offsets)]
    assert M(".1666") < exponents[1] / exponents[0] - 1 < M(".1667")
    print("held Pareto face offsets", x, offsets, "exponents", *map(show, exponents),
          "relative error", show(exponents[1] / exponents[0] - 1))


def rounded_scale_refit():
    unit = M(2**54)
    y = [unit + 1, unit + 5]
    ideal_scale = mp.sqrt(y[0] * y[1])
    scaled_logs = [unit * mp.log(v / ideal_scale) for v in y]
    public_scale = M(float(ideal_scale))
    assert public_scale - unit == 4
    public_logs = [unit * mp.log(v / public_scale) for v in y]

    def power(z):
        return mp.findroot(lambda b: 1 / b - mean(
            [v * mp.tanh(b * v / 2) for v in z]), (M(".5"), M(1)))

    ideal_power = power(scaled_logs)
    refitted_power = power(public_logs)
    error = abs(refitted_power / ideal_power - 1)
    assert error > M(".05")

    def objective(b, z):
        return mp.log(b) - 2 * mean([mp.log(2 * mp.cosh(b * v / 2)) for v in z])

    loss = objective(ideal_power, scaled_logs) - objective(ideal_power, public_logs)
    assert loss > M(".05")
    print("Pareto III rounded-scale refit", "ideal and conditional power / 2^54",
          show(ideal_power), show(refitted_power), "relative change", show(error),
          "mean likelihood loss at rounded scale and ideal power", show(loss))


def reference(data, family, held, returned):
    """Solve analytic scores without fitting a second model at rounded scale."""
    shape_name = "shape" if family == "pareto3" else "shape2"
    alpha = M(1 if family == "pareto3" else held.get("shape1", returned.get("shape1")))
    power = M(held.get(shape_name, returned[shape_name]))
    scale = M(held.get("scale", returned["scale"]))
    threshold = M(held["min"])
    logs = [mp.log(M(x) - threshold) for x in data]
    free_alpha = family == "pareto4" and "shape1" not in held
    free_power = shape_name not in held
    free_scale = "scale" not in held
    center = mean(logs) if free_scale else mp.log(scale)
    width = max(abs(v - center) for v in logs) or M(1)
    z = [(v - center) / width for v in logs]
    coordinates = (mp.log(alpha), mp.log(power * width), power * (center - mp.log(scale)))
    free = (free_alpha, free_power, free_scale)
    assert any(free)

    def unpack(args):
        supplied = iter(args)
        return [next(supplied) if f else v for f, v in zip(free, coordinates)]

    def score(*args):
        log_a, log_p, b = unpack(args)
        a, p = mp.exp(log_a), mp.exp(log_p)
        u = [p * v + b for v in z]
        residuals = [1 - (a + 1) / (1 + mp.exp(-v)) for v in u]
        gradient = (1 - a * mean([mp.log1p(mp.exp(v)) for v in u]),
                    1 + p * mean([v * w for v, w in zip(z, residuals)]),
                    mean(residuals))
        return tuple(g for g, f in zip(gradient, free) if f)

    start = tuple(v for v, f in zip(coordinates, free) if f)
    root = mp.findroot(score, start, tol=M("1e-175"))
    assert max(map(abs, score(*root))) < M("1e-75")
    jacobian = mp.jacobian(score, root)
    mp.cholesky(-(jacobian + jacobian.T) / 2)
    log_a, log_p, b = unpack(root)
    p = mp.exp(log_p) / width
    truth = (mp.exp(log_a), p, mp.exp(center - b / p))
    names = ("shape1", shape_name, "scale")
    return {n: abs(M(returned[n]) / v - 1)
            for n, v, f in zip(names, truth, free) if f}


def archived_coefficients():
    selected = {
        "pareto3": {"005584", "005585", "005590", "005591", "005593"},
        "pareto4": {"005644", "005648", "005665", "005666", "005674"},
    }
    with gzip.open(ARCHIVE, "rt") as source:
        archive = {f"{c['index']:06}": c["data"] for c in json.load(source)}
    checked = 0
    for mode in ("native", "double"):
        with gzip.open(SNAPSHOTS / f"{mode}.jsonl.gz", "rt") as source:
            for line in source:
                row = json.loads(line)
                family, case = row["family"], row["case"]
                if row["source"] != "archive" or case not in selected.get(family, ()):
                    continue
                result = row["result"]
                if result["kind"] != "ok":
                    print(mode, family, case, result["kind"], result["reason"])
                    continue
                returned = {k: float.fromhex(v) for k, v in result["estimates"].items()}
                errors = reference(archive[case], family, row["held"], returned)
                print(mode, family, case, result["certified"], row["held"],
                      {k: show(v) for k, v in errors.items()})
                assert max(errors.values()) < M("1e-10")
                checked += 1
    assert checked == 18


if __name__ == "__main__":
    held_scale_rounding()
    rounded_scale_refit()
    archived_coefficients()
