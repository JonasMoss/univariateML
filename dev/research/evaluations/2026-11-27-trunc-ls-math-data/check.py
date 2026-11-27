"""Finite-path check of exact boundary and multiplicity formulas."""
import csv
from pathlib import Path

import mpmath as mp

mp.mp.dps = 80
rows = []


def check(name, exponent, actual, expected, tolerance):
    error = abs(actual / expected - 1) if expected else abs(actual)
    assert error < tolerance, (name, exponent, error, tolerance)
    rows.append((name, exponent, mp.nstr(actual, 30), mp.nstr(expected, 30),
                 mp.nstr(error, 12)))


for exponent in (3, 6, 12, 20):
    h = mp.mpf(10) ** -exponent
    for name, p, k in (("cauchy", 1, 1), ("student-nu-3", 2, 3)):
        n = k + 1
        # Exterior location and positive scale with scale / distance -> 0.
        z = mp.quad(lambda x: ((x + h)**2 + h**4)**-p, [0, h, mp.sqrt(h), 1])
        prod = ((h*h + h**4)**(-p*k) * ((1 + h)**2 + h**4)**-p) / z**n
        check(name + "-exterior-endpoint", exponent, prod,
              mp.mpf(2*p - 1)**n, 25*h)

        # Held location, center of a symmetric window.
        if p == 1:
            z = 2 * mp.atan(1/h) / h
            j = mp.pi
        else:
            z = 1/(h*h*(1+h*h)) + mp.atan(1/h)/h**3
            j = mp.pi / 2
        prod = h**(-2*p*k) * (1+h*h)**-p / z**n
        check(name + "-held-interior", exponent, prod, j**-n, 10*h)

        # Held half-Cauchy endpoint and one-sided effective window.
        if p == 1:
            z = mp.atan(1/h) / h
            prod = h**-2 * (1+h*h)**-1 / z**2
            check("half-cauchy-held-endpoint", exponent, prod, 4/mp.pi**2, 5*h)

    for upper in (mp.mpf(2), mp.inf):
        # Gumbel on [-upper,0], approaching the increasing tilt of rate 2.
        v = mp.mpf(2)
        den = -mp.expm1(-v*mp.expm1(h*upper)/h)
        face_den = -mp.expm1(-v*upper)
        actual = mp.fprod(v*mp.exp(h*x-v*mp.expm1(h*x)/h)/den
                          for x in (0, 1))
        expected = (v/face_den)**2 * mp.exp(-v)
        check("gumbel-left-" + str(upper), exponent, actual, expected, 5*h)

    for r in (mp.mpf("0.25"), mp.mpf(1), mp.mpf(4)):
        c = h
        lo, hi = 1, mp.exp(r)
        z = (hi-lo)/((lo+c)*(hi+c))
        actual = mp.fprod(r*y/(y+c)**2/z for y in (lo, hi))
        expected = (r/(2*mp.sinh(r/2)))**2
        check("logistic-balanced-endpoints-r-" + str(r), exponent,
              actual, expected, mp.mpf("1e-75"))

with Path(__file__).with_name("checks.csv").open("w", newline="") as out:
    writer = csv.writer(out, lineterminator="\n")
    writer.writerow(("case", "minus_log10_step", "path_product",
                     "boundary_product", "relative_error"))
    writer.writerows(rows)
print(f"{len(rows)} checks passed at {mp.mp.dps} decimal digits")
