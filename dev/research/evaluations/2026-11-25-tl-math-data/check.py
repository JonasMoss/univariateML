"""Exact example checks and 70-digit probes for TASK-240."""
from fractions import Fraction

import mpmath as mp

mp.mp.dps = 70


def qmag(p, shape):
    if shape == 0:
        return mp.log1p(-p) - mp.log(p)
    return (mp.expm1(shape * mp.log1p(-p))
            - mp.expm1(shape * mp.log(p))) / shape


def smaller_tail(z, shape):
    if z == 0:
        return mp.mpf(1) / 2
    if shape > 0 and z == 1 / shape:
        return mp.mpf(0)
    lo, hi = mp.log(2), mp.mpf(2)
    while qmag(mp.exp(-hi), shape) < z:
        hi *= 2
    for _ in range(245):
        mid = (lo + hi) / 2
        if qmag(mp.exp(-mid), shape) < z:
            lo = mid
        else:
            hi = mid
    return mp.exp(-(lo + hi) / 2)


def density(z, shape):
    p = smaller_tail(abs(z), shape)
    if p == 0:
        return mp.mpf(1)
    return 1 / (p ** (shape - 1) + (1 - p) ** (shape - 1))


def negative_score(p, a):
    q = 1 - p
    y, v = -mp.log(p), -mp.log(q)
    ratio = p / q
    weight = ratio ** (a + 1)
    pa_over_p = ((y - 1 / a) - ratio ** a * (v - 1 / a)) / (a * (1 + weight))
    return (-(y + weight * v)
            + (a + 1) * (1 - ratio ** (a + 2)) * pa_over_p) / (1 + weight)


def anchored(p, shape):
    b = shape - 1
    d = p ** b + (1 - p) ** b
    du = b * (p ** (b - 1) - (1 - p) ** (b - 1))
    dshape = p ** b * mp.log(p) + (1 - p) ** b * mp.log1p(-p)
    # Derivative of the distance from the left endpoint, divided by scale.
    power_difference = 1 + p ** shape - (1 - p) ** shape
    integral_derivative = (p ** shape * mp.log(p)
                           - (1 - p) ** shape * mp.log1p(-p)) / shape
    integral_derivative -= power_difference / shape ** 2
    return -(dshape - du * integral_derivative / d) / d


def show(name, value):
    print(f"{name} = {mp.nstr(value, 35)}")


# Exact U-branch witness with six copies of the interior point.
p = Fraction(40, 401) ** 2
q = Fraction(399, 401) ** 2
assert p + q == 1
interior = Fraction(40 ** 3 - 399 ** 3, 401 ** 3)
u_ratio = Fraction(2, 3) ** 2 * Fraction(1604, 1317) ** 6
assert u_ratio > 1
print(f"U witness interior point = {interior}")
show("U witness / junction", mp.mpf(u_ratio.numerator) / u_ratio.denominator)
shape = mp.mpf("1.5")
z = abs(mp.mpf(interior.numerator) / interior.denominator) / shape
assert abs(density(z, shape) - mp.mpf(401) / 439) < mp.mpf("1e-65")

# U-branch interior anchored shape-score probes.
anchor_scores = [
    anchored(p, mp.mpf(1) + b)
    for p in [mp.mpf(10) ** -k for k in (1, 2, 4, 12, 32)]
             + [mp.mpf(".25"), mp.mpf(".5"), mp.mpf(".75"), mp.mpf(".99")]
    for b in map(mp.mpf, (".01", ".1", ".5", ".9", ".99"))
]
assert min(anchor_scores) > 0
show("minimum of 45 anchored log-density shape scores", min(anchor_scores))

# Probes of the small-scale score bound and one independent differentiation.
score_slacks = []
for a in map(mp.mpf, (".5", ".6", ".8", ".95", "1")):
    for y in map(mp.mpf, (".7", "1", "2", "3.99", "4", "5", "10", "32", "80")):
        score = negative_score(mp.exp(-y), a)
        lower = y / 2 - 4 if y >= 4 else -4
        score_slacks.append(score - lower)
assert min(score_slacks) > 0
show("minimum of 45 negative-shape score slacks", min(score_slacks))
a, p = mp.mpf(".75"), mp.exp(-10)
z = qmag(p, -a)
h = mp.mpf("1e-22")
derivative = (mp.log(density(z, -a - h)) - mp.log(density(z, -a + h))) / (2 * h)
assert abs(derivative - negative_score(p, a)) < mp.mpf("1e-39")

# Exact constants for one attained and one unattained half-tie sample.
eps = Fraction(1, 10 ** 20)
assert 1024 / eps < 1 / (64 * eps ** 2)
# e < 3, sqrt(32) < 6.
assert 1 / (12 * eps) > 2 * 3 ** 32
assert Fraction(1, 16) > Fraction(1, 64)
a = mp.mpf(-1)
eps_mp = mp.mpf("1e-20")
for sigma in (eps_mp / 10, eps_mp / 100, eps_mp / 10000):
    ll = (density(mp.mpf(0), a) ** 2 * density(eps_mp / sigma, a)
          * density(1 / sigma, a) / sigma ** 4)
    ratio = 64 * eps_mp ** 2 * ll
    assert 0 < ratio < 1
    show(f"collapse example L/B at sigma/epsilon={mp.nstr(sigma / eps_mp)}", ratio)

# Explicit large-separation held-scale examples and near-uniform collar.
d = 10 ** 20
assert d > 4 * 3 ** 24
assert Fraction(16, d ** 3) < Fraction(1, 64 * (d + 4) ** 2)
assert Fraction(32, d ** 3) < Fraction(1, 8 * (d + 4) ** 2)
assert Fraction(2, d ** 4) < Fraction(1, 64 * (d + 4) ** 2)
assert Fraction(4, 20 ** 2 * d ** 2) < Fraction(1, 64 * (d + 4) ** 2)
assert Fraction(4, 37 * d ** 2) < Fraction(1, 8 * (d + 4) ** 2)
assert Fraction(9 * (d - 5) ** 2, 8 * (d + 4) ** 2) > 1
assert Fraction(3, (d - 1) ** 2) < Fraction(3, 25)
for n in (3, 5, 31):
    epsilon = mp.exp(-4 * n) / 2
    log_ratio = -n * mp.log1p(-epsilon) + epsilon * mp.log(epsilon) / 2
    assert log_ratio < 0

# Shared-scale envelope check on [-1, 0, 1], with mu in [-.1, .1].
distance = mp.mpf(".9")
breakpoint = distance / mp.sqrt(2)
shared = breakpoint / (2 * distance ** 4)
independent = 50 / (2 * breakpoint) ** 2
for j in range(1001):
    scale = mp.exp(mp.log(mp.mpf(".01")) * (1 - mp.mpf(j) / 1000))
    modal = 1 / (2 * scale)
    assert modal * min(modal, scale / distance ** 2) ** 2 <= shared
show("shared-scale envelope", shared)
show("observation-wise independent-scale envelope", independent)
print("All checks passed.")
