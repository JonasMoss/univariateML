#!/usr/bin/env python3
"""Evaluate the collapse path at 85 digits and calculate the exact pair fit."""
from decimal import Decimal as D, localcontext
from pathlib import Path
import json

ROOT = Path(__file__).resolve().parents[4]
GOLDEN = ROOT / "univariateML-cpp/include/cppunivariate/distributions/tlambda/golden.json"


def path_density(x, mu, sigma):
    # Solve |z| = 1/u - 1/(1-u) by rationalizing the smaller quadratic root.
    z = abs(x - mu) / sigma
    u = 2 / (z + 2 + (z * z + 4).sqrt())
    return 1 / (sigma * (u**-2 + (1 - u)**-2))


def main():
    sample = sorted(json.loads(GOLDEN.read_text())["data"])
    q = [sample[int(f * (len(sample) - 1))] for f in (0.25, 0.50, 0.75)]
    print("Quartiles (exact binary64):", *(x.hex() for x in q))
    with localcontext() as context:
        context.prec = 85
        lo, centre, hi = map(D.from_float, q)
        gap = hi - lo
        print("Pair mu:", (lo + hi) / 2)
        print("Pair sigma:", gap / 2)
        print("Pair lambda: 1")
        print("Pair log likelihood:", -2 * gap.ln())
        expected = D(8)**-4 / ((lo - centre) * (hi - centre))**2
        print("Tie asymptotic sigma^2 L:", expected)
        for exponent in (5, 10, 20, 40):
            sigma = D(10)**-exponent
            scaled = sigma**2 * path_density(centre, centre, sigma)**4
            scaled *= path_density(lo, centre, sigma) * path_density(hi, centre, sigma)
            print(f"sigma=1e-{exponent}: log L={ (scaled / sigma**2).ln():.20f},"
                  f" scaled/asymptotic={scaled / expected:.45f}")
            assert abs(scaled / expected - 1) < 10 * sigma


if __name__ == "__main__":
    main()
