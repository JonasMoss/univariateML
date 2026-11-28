"""Replay the exact examples and high-precision estimated-window checks."""

import csv
from fractions import Fraction
from pathlib import Path

import mpmath as mp


DATA = Path(__file__).resolve().parent
DIGITS = 80


def main():
    rows = []
    mp.mp.dps = DIGITS

    def approximate(case, setting, observed, target, tolerance):
        error = abs(observed - target)
        assert error <= tolerance, (case, setting, observed, target, error)
        rows.append((case, setting, DIGITS, mp.nstr(observed, 30),
                     mp.nstr(target, 30), mp.nstr(error, 8)))

    def exact(case, setting, observed, target):
        assert observed == target, (case, setting, observed, target)
        rows.append((case, setting, "rational", str(observed), str(target), "0"))

    def positive(case, setting, gap):
        assert gap > 0, (case, setting, gap)
        rows.append((case, setting, "rational", str(gap), "strictly positive", ""))

    for d in map(Fraction, ("1/32", "1/2", "1", "2", "16", "1024")):
        # Held shape one, x=(−1,0), tight upper ray. Optimize c and remove exp(−2).
        reciprocal_gap = 1 / d - 1 / (d + 1)
        scale = 2 / reciprocal_gap
        product_times_e_squared = scale**2 / (d * (d + 1))**2
        exact("frechet-upper-ray-plateau", f"d={d}; scale={scale}", product_times_e_squared, 4)

    for d in map(mp.mpf, ("1000", "1000000", "1000000000000")):
        for shape in map(mp.mpf, ("1", "1.5", "2", "4")):
            h = d**(-shape) - (d + 1)**(-shape)
            coefficient = 2 / h
            product = (shape * coefficient / mp.e)**2 / (d * (d + 1))**(shape + 1)
            approximate("frechet-upper-ray-tilt-height", f"d={d}; shape={shape}",
                        product, 4 / mp.e**2, 8 / d**2)
            cdf = mp.exp(-coefficient * ((d + mp.mpf("0.5"))**(-shape)
                                        - (d + 1)**(-shape)))
            approximate("frechet-upper-ray-tilt-cdf", f"d={d}; shape={shape}; x=-1/2",
                        cdf, mp.exp(-1), 2 / d)

    for d in map(mp.mpf, ("1e-2", "1e-5", "1e-10", "1e-20")):
        # x=(0,1), shape=1, scale=d*q, tight [0,1] and [0,Inf].
        q = d
        t = d / (1 + d)
        numerator = q**2 * t**2 * mp.exp(-q * (1 + t)) / d**2
        for upper, u in ((1, t), ("Inf", 0)):
            retained_mass = mp.exp(-q * u) - mp.exp(-q)
            approximate("frechet-lower-half-contact", f"d=q={d}; upper={upper}",
                        numerator / retained_mass**2, mp.mpf(1), 3*d)

    for d in map(Fraction, ("1/10", "1/2", "1", "2")):
        pole_product = 4 * d / (1 + d)**3
        factored_gap = (2*d-1)**2 * (4*d+16) / (27*(1+d)**3)
        exact("pareto2-ray-pole", f"s={d}", Fraction(16, 27) - pole_product, factored_gap)
    finite_trial = Fraction(8, 3)**4 * Fraction(10, 11)**3 / 8
    positive("pareto2-finite-pole", "x=(1,1,11/10,2); k=2; s=1", finite_trial - 1)
    for scale in map(mp.mpf, ("1e-8", "1e-16", "1e-32", "1e-64")):
        shape = scale**mp.mpf("0.25")
        survival = mp.exp(-shape * mp.log1p(1/scale))
        likelihood_ratio = (shape/scale)**2 * (1 + 1/scale)**(-shape-1) * mp.sqrt(scale)
        tolerance = 2*shape * mp.log1p(1/scale)
        approximate("pareto2-escaping-mass", f"s={scale}; y=1", survival, mp.mpf(1), tolerance)
        approximate("pareto2-divergent-height", f"s={scale}; x=(1,2)",
                    likelihood_ratio, mp.mpf(1), tolerance)

    for w in map(mp.mpf, ("1e-2", "1e-5", "1e-10", "1e-20")):
        # Held unit-rate Gompertz, lower endpoint 0 and shrinking upper cut.
        mass = -mp.expm1(-mp.expm1(w))
        approximate("gompertz-shrinking-window", f"width={w}; x=(0)", w / mass, mp.mpf(1), w**2)

    for gap in map(mp.mpf, ("1", "2", "4")):
        # x=(1,exp(gap)), bounded log interval. At the log midpoint the uniform
        # face wins for the free positive power tilt; the derivative is zero.
        approximate("power-zero-shape-threshold", f"log gap={gap}",
                    (mp.log(1) + mp.log(mp.exp(gap))) / 2, gap / 2, mp.mpf("1e-77"))

    positive("lst-inner-vs-uniform", "trial center 4 relative to uniform", Fraction(40000, 4*65**2) - 2)
    positive("lst-inner-vs-centered", "trial center 4 relative to center 0", Fraction(17**4, 4*65**2) - 2)
    positive("lst-full-vs-uniform", "50 copies each of -4 and 4, endpoints ±100",
             Fraction(2**50 * 40000, 4*10817**4) - 1)
    positive("lst-full-vs-centered", "50 copies each of -4 and 4, endpoints ±100",
             Fraction(2**50 * 10001**4, 4*10817**4) - 1)
    positive("slash-full-vs-uniform", "50 copies each of -4 and 4, endpoints ±100",
             Fraction(2**50 * 40000, 36*104**4) - 1)
    positive("slash-full-vs-centered", "50 copies each of -4 and 4, endpoints ±100",
             Fraction(3, 2)**50 / 4 - 1)

    with (DATA / "checks.csv").open("w", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(("case", "setting", "precision", "observed", "target", "absolute_error"))
        writer.writerows(rows)
    print(f"{len(rows)} exact or {DIGITS}-digit checks passed")


if __name__ == "__main__":
    main()
