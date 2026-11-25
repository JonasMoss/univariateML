"""High-precision likelihoods and roots on the two-level path."""
import math

import mpmath as mp

mp.mp.dps = 85
Q = mp.mpf


def terms(x, b):
    return [(mp.exp(-b * t), -mp.expm1(-b * t)) for t in x]


def likelihood(x, b, eta):
    return len(x) * mp.log(b) - b * sum(x) + sum(
        mp.log1p(eta * a) - eta * v for v, a in terms(x, b)
    )


def shapescore(x, b, eta):
    return sum(a / (1 + eta * a) - v for v, a in terms(x, b))


def ratescore(x, b, eta):
    return len(x) / b - sum(x) + sum(
        t * eta * v * (1 + 1 / (1 + eta * a))
        for t, (v, a) in zip(x, terms(x, b))
    )


def curvature(x, b, eta):
    values = [(t, v, a, 1 + eta * a)
              for t, (v, a) in zip(x, terms(x, b))]
    bb = -len(x) / b**2 - sum(
        eta * t**2 * v * (1 + (1 + eta) / d**2) for t, v, _, d in values)
    be = sum(t * v * (1 + 1 / d**2) for t, v, _, d in values)
    ee = sum((a / d)**2 for _, _, a, d in values)
    return bb + be**2 / ee


def equations(b, eta, m):
    x = [Q(1)] * 9 + [m]
    return (ratescore(x, b, eta), shapescore(x, b, eta),
            likelihood(x, b, eta) - likelihood(x, 10 / (9 + m), 0))


def main():
    b, e, m = mp.findroot(equations, (Q(".98"), Q("1.45"), Q("8.6")))
    print("tie b, eta, M:", *(mp.nstr(v, 55) for v in (b, e, m)))
    x = [Q(1)] * 9 + [m]
    a, t = mp.findroot(lambda a, t: (ratescore(x, a, t), shapescore(x, a, t)),
                       (Q(".67"), Q(".3")))
    print("valley b, eta, curvature:", *(mp.nstr(v, 30) for v in
                                         (a, t, curvature(x, a, t))))
    print("interior maximum curvature:", mp.nstr(curvature(x, b, e), 30))
    nearest = float(m)
    neighbors = (math.nextafter(nearest, -math.inf), nearest) if nearest > m else (
        nearest, math.nextafter(nearest, math.inf))
    for value in neighbors:
        r, s = mp.findroot(lambda r, s: equations(r, s, Q(value))[:2], (b, e))
        print("dyadic", value.hex(), "interior gain:", mp.nstr(equations(r, s, Q(value))[2], 35))
    seam = mp.findroot(lambda u: shapescore([Q(1)] * 9 + [u], 10 / (9 + u), 0), 8)
    print("seam M, curvature from right:", mp.nstr(seam, 32),
          mp.nstr(curvature([Q(1)] * 9 + [seam], 10 / (9 + seam), 0), 32))
    assert Q("8.60") < m < Q("8.62")
    assert Q(".60") < a < Q(".80") and Q(".90") < b < Q("1.05")


if __name__ == "__main__":
    main()
