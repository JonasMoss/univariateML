"""Exact equal-multiplicity threshold cases and high-precision face checks."""
import mpmath as mp
import sympy as sp

mp.mp.dps = 80
delta = sp.Symbol("delta")
beta = sp.Integer(1)


def squared_profile(data):
    shifted = [delta + x for x in data]
    q = sum(t / beta + beta / t - 2 for t in shifted) / len(data)
    return sp.cancel(
        sp.prod((t + beta)**2 / t**3 for t in shifted) / (beta * q)**len(data)
    )


def rational_cases():
    for data in [(0, 1, 1), (0, sp.Rational(1, 10), 1)]:
        value = squared_profile(data)
        edge = sp.limit(value, delta, 0, dir="+")
        comparison = sp.cancel(value - edge)
        numerator = sp.Poly(sp.fraction(comparison)[0], delta)
        while numerator.eval(0) == 0:
            numerator = numerator.exquo(sp.Poly(delta, delta))
        positive_roots = numerator.count_roots(0, sp.oo)
        if data[1] == 1:
            assert comparison.subs(delta, sp.Rational(1, 2)) > 0
            assert positive_roots > 0
        else:
            assert positive_roots == 0  # The difference is negative at infinity.
            sturm = numerator.sturm()
            for signs in ([sp.sign(p.eval(0)) for p in sturm],
                          [sp.sign(p.LC()) for p in sturm]):
                print("Sturm signs:", "".join({-1: "-", 0: "0", 1: "+"}[s] for s in signs))
        print(data, "edge =", edge, "difference at 1/2 =", comparison.subs(delta, sp.Rational(1, 2)),
              "positive roots =", positive_roots)


def density(t, a, b):
    return (t + b) * mp.exp(-(t / b + b / t - 2) / (2 * a*a)) / (
        2 * a * mp.sqrt(2 * mp.pi * b) * t**mp.mpf("1.5")
    )


def face_paths():
    for k in (1, 2, 3):
        d = [mp.mpf(0)] * k + [mp.mpf(1), mp.mpf(3)]
        for j in (4, 8, 16, 24):
            e = mp.mpf(10)**(-j)
            s = mp.mpf(2)
            t = [e*e / s + x for x in d]
            likelihood = mp.fprod(density(v, mp.sqrt(s/e), e) for v in t)
            limit = (s * mp.exp(mp.mpf("-.5")) / (2 * mp.sqrt(2 * mp.pi)))**k
            limit *= mp.fprod(mp.exp(-x / (2*s)) / (2 * mp.sqrt(2 * mp.pi * s * x))
                              for x in d[k:])
            assert abs(likelihood * e**(2*k) / limit - 1) < 10*e
    print("80-digit unrestricted endpoint spike, k=1,2,3 and e=10^(-4,-8,-16,-24)")


if __name__ == "__main__":
    rational_cases()
    face_paths()
