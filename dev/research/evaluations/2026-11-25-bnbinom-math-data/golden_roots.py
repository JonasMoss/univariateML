"""Verify two distinct canonical stationary roots with rational comparisons."""
import hashlib
import json
from collections import Counter
from fractions import Fraction as Q
from functools import cache
from pathlib import Path

import mpmath as mp

ROOT = Path(__file__).resolve().parents[4]
GOLDEN = ROOT / "univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json"
sample = json.loads(GOLDEN.read_text())["data"]
tail = [Q(sum(x > j for x in sample), len(sample)) for j in range(max(sample))]
GRID = 2**100


def round_down(x):
    return Q((x * GRID).__floor__(), GRID)


def round_up(x):
    return -round_down(-x)


class Interval:
    def __init__(self, lo, hi=None):
        self.lo = round_down(Q(lo))
        self.hi = round_up(Q(lo if hi is None else hi))

    def __add__(self, other):
        other = interval(other)
        return Interval(self.lo + other.lo, self.hi + other.hi)

    __radd__ = __add__

    def __neg__(self):
        return Interval(-self.hi, -self.lo)

    def __sub__(self, other):
        return self + -interval(other)

    def __rsub__(self, other):
        return interval(other) + -self

    def __mul__(self, other):
        other = interval(other)
        endpoints = [x * y for x in (self.lo, self.hi) for y in (other.lo, other.hi)]
        return Interval(min(endpoints), max(endpoints))

    __rmul__ = __mul__

    def inverse(self):
        assert self.lo * self.hi > 0
        return Interval(1 / self.hi, 1 / self.lo)

    def magnitude(self):
        return max(abs(self.lo), abs(self.hi))


def interval(value):
    return value if isinstance(value, Interval) else Interval(value)


def small_log(x):
    assert 1 <= x <= 2
    t = (x - 1) / (x + 1)
    square = t * t
    power = t
    total = Q(0)
    for j in range(32):
        total += power / (2 * j + 1)
        power *= square
    return Interval(2 * total, 2 * (total + power / (65 * (1 - square))))


LOG2 = small_log(Q(2))


def log(x):
    k = 0
    while x >= 2:
        x /= 2
        k += 1
    return small_log(x) + k * LOG2


@cache
def psi_endpoint(x, derivative):
    # Shift up and bound the remaining asymptotic error on both sides.
    shift = 512
    z = x + shift
    reciprocal = 1 / z
    if derivative:
        base = reciprocal + reciprocal**2 / 2 + reciprocal**3 / 6
        correction = sum((Interval(1 / (x + j)**2) for j in range(shift)), Interval(0))
        return Interval(base - reciprocal**5 / 30, base) + correction
    base = log(z) - reciprocal / 2 - reciprocal**2 / 12
    correction = sum((Interval(1 / (x + j)) for j in range(shift)), Interval(0))
    return base + Interval(0, reciprocal**4 / 120) - correction


def psi(x, derivative=False):
    low_at, high_at = (x.hi, x.lo) if derivative else (x.lo, x.hi)
    return Interval(psi_endpoint(low_at, derivative).lo, psi_endpoint(high_at, derivative).hi)


def terms(x, derivative):
    power = 2 if derivative else 1
    return sum((w * ((x + j).inverse() if power == 1
                     else (x + j).inverse() * (x + j).inverse())
                for j, w in enumerate(tail)), Interval(0))


def score(r, a, b):
    t = a + r + b
    common = psi(t) + terms(t, False)
    return [psi(a + r) + terms(r, False) - common,
            psi(a + r) + psi(a + b) - psi(a) - common,
            psi(a + b) + terms(b, False) - common]


def hessian(r, a, b):
    t = a + r + b
    common = psi(t, True) - terms(t, True)
    ar, ab = psi(a + r, True), psi(a + b, True)
    return [[ar - terms(r, True) - common, ar - common, -common],
            [ar - common, ar + ab - psi(a, True) - common, ab - common],
            [-common, ab - common, ab - terms(b, True) - common]]


def contraction(center, radius, diagonal=False):
    dimension = len(center)
    box = [Interval(x - radius, x + radius) for x in center]

    def pullback(values):
        if diagonal:
            s, a = values
            return s, a, s
        return values

    def jacobian(values):
        h = hessian(*pullback(values))
        if diagonal:
            return [[h[i][0] + h[i][2], h[i][1]] for i in (0, 1)]
        return h

    point = [Interval(x) for x in center]
    g = score(*pullback(point))[:dimension]
    hc = jacobian(point)
    matrix = mp.matrix([[mp.mpf(str(float((v.lo + v.hi) / 2))) for v in row] for row in hc])
    proposal = matrix**-1
    preconditioner = [[Q(mp.nstr(proposal[i, j], 30)) for j in range(dimension)]
                      for i in range(dimension)]
    hb = jacobian(box)
    residual = [sum((m * s for m, s in zip(row, g)), Interval(0))
                for row in preconditioner]
    contraction_bound = max(
        sum((int(i == j) - sum((preconditioner[i][k] * hb[k][j] for k in range(dimension)),
                               Interval(0))).magnitude() for j in range(dimension))
        for i in range(dimension)
    )
    displacement = max(v.magnitude() for v in residual)
    assert contraction_bound < 1
    assert displacement + contraction_bound * radius < radius
    result = {
        "center": [str(float(x)) for x in center],
        "radius": str(radius),
        "contraction_upper_rounded_up": str((contraction_bound * 10**12).__ceil__() / 10**12),
        "center_displacement_upper_rounded_up": str((displacement * 10**20).__ceil__() / 10**20),
    }
    if diagonal:
        h = hessian(*pullback(box))
        curvature = h[0][0] - h[0][2]
        assert curvature.lo > 0
        result["antisymmetric_eigenvalue_lower_rounded_down"] = str(
            (curvature.lo * 10**9).__floor__() / 10**9
        )
    return result


def main():
    mp.mp.dps = 80
    previous = ROOT / "dev/research/evaluations/2026-11-08-bnbinom-allfree-data"
    record = json.loads((previous / "joint.json").read_text(), parse_float=Q)
    assert record["proved"]
    candidate = list(map(Q, ("1.067223439275812", "2.141979220255064", "5.364734296253785")))
    radius = Q("0.00000002")
    for x, y in zip(candidate, record["coefficients"]):
        assert abs(x - y) + y * Q("0.0000000025") < radius
    assert candidate[0] + radius < candidate[2] - radius
    off_diagonal = contraction(candidate, radius)
    diagonal = contraction(list(map(Q, ("2.099578384354076", "1.833901265112290"))),
                           Q("0.00000001"), diagonal=True)
    print(json.dumps({
        "sample_size": len(sample),
        "sample_sha256": hashlib.sha256(json.dumps(sample).encode()).hexdigest(),
        "histogram": dict(sorted(Counter(sample).items())),
        "off_diagonal": off_diagonal,
        "diagonal": diagonal,
    }, indent=2))


if __name__ == "__main__":
    main()
