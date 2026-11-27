"""Check selected conditional likelihood limits at 100 decimal digits."""
import csv
from pathlib import Path

import mpmath as mp

mp.mp.dps = 100
HERE = Path(__file__).resolve().parent
M = mp.mpf
rows = []


def likelihood(density, mass, sample):
    return mp.fprod(density(x) / mass for x in sample)


def continuous(kernel, low, high, sample):
    return likelihood(kernel, mp.quad(kernel, [low, high]), sample)


def check(label, powers, finite, limit, tolerance):
    errors = []
    for power in powers:
        value = finite(power)
        error = abs(value / limit - 1)
        errors.append(error)
        rows.append(
            (label, power, mp.nstr(value, 32), mp.nstr(limit, 32), mp.nstr(error, 12))
        )
    assert all(current > next_error for current, next_error in zip(errors, errors[1:])), label
    assert errors[-1] < M(tolerance), (label, errors[-1])


sample = [M("1.25"), M("1.75")]


def fold(power):
    sigma = M(10) ** power
    return continuous(lambda x: mp.exp(-x*x/(2*sigma*sigma))*mp.cosh(2*x), 1, 2, sample)


check("foldnorm cosh, [1,2]", [1, 2, 3, 4], fold,
      continuous(lambda x: mp.cosh(2*x), 1, 2, sample), "1e-8")


def shifted_gompertz(power):
    b = M(10) ** (-power)
    eta = 2/b

    def reduced_cdf(x):
        offset = -mp.expm1(-b*x)
        return offset * mp.exp(eta*offset) / b

    return likelihood(lambda x: mp.diff(reduced_cdf, x),
                      reduced_cdf(2)-reduced_cdf(1), sample)


check("sgomp onset, [1,2]", [2, 4, 6, 8], shifted_gompertz,
      continuous(lambda x: (1+2*x)*mp.exp(2*x), 1, 2, sample), "1e-7")


def gev(power):
    distance = M(10) ** power
    c = 2*distance**2

    def relative_cdf(x):
        return mp.exp(2*distance*(x-2)/(distance+x) * distance/(distance+2))

    return likelihood(lambda x: c/(distance+x)**2 * relative_cdf(x),
                      M(1), sample)


check("gev xi=1 increasing exponential, (-inf,2]", [2, 4, 6, 8], gev,
      likelihood(lambda x: 2*mp.exp(2*(x-2)), M(1), sample), "1e-7")


def gpd(power):
    xi = mp.exp(M(10)**power)
    sigma = M(1)
    h1 = mp.log1p(xi)/xi
    delta = mp.log((sigma+2*xi)/(sigma+xi))/xi
    mass = mp.exp(-h1)*(-mp.expm1(-delta))
    return likelihood(lambda x: mp.exp(-mp.log1p(xi*x)/xi)/(sigma+xi*x),
                      mass, sample)


check("gpd log-uniform, [1,2]", [0, 1, 2, 3], gpd,
      continuous(lambda x: 1/x, 1, 2, sample), "1e-50")


def pareto_contact(power):
    gap = M(10)**(-power)
    density = lambda y: (1+y)**-2
    mass = 1-1/(1+gap)
    return gap*density(gap)/mass


check("pareto shrinking upper cut, scaled density", [2, 4, 6, 8],
      pareto_contact, M(1), "1e-7")


def shape_trace(power, family):
    p = M(10)**power
    c = 2/p
    sample_at_scale = [M("0.9"), M(1)] if family == "EW" else [M(1), M("1.1")]
    if family == "EW":
        def cdf(x):
            return (-mp.expm1(-x**p))**c
        low, high = M("0.8"), M("1.2")
    else:
        def cdf(x):
            return -mp.expm1(-mp.expm1(c*mp.log1p(x**p)))
        low, high = M("0.8"), M("1.2")
    return likelihood(lambda x: mp.diff(cdf, x), cdf(high)-cdf(low), sample_at_scale)


ew_target = (2*M("0.9"))*(2/mp.expm1(1))/(1-M("0.8")**2)**2
pgw_target = (2*M("1.1")*mp.exp(1-M("1.1")**2))/(1-mp.exp(1-M("1.2")**2))**2
check("ew power endpoint trace", [2, 3, 4], lambda p: shape_trace(p, "EW"),
      ew_target, "1e-3")
check("pgw threshold endpoint trace", [2, 3, 4], lambda p: shape_trace(p, "PGW"),
      pgw_target, "1e-3")


for numerator in range(-4, 9):
    mode = M(numerator)/4
    if mode <= 0:
        kernel = lambda x: (2-x)/(2-mode)
    elif mode >= 1:
        kernel = lambda x: (x+1)/(mode+1)
    else:
        kernel = lambda x: (x+1)/(mode+1) if x < mode else (2-x)/(2-mode)
    cuts = [0, mode, 1] if 0 < mode < 1 else [0, 1]
    value = likelihood(kernel, mp.quad(kernel, cuts), [0, 1])
    target = M(8)/9
    if 0 < mode < 1:
        assert value < target
    else:
        assert mp.almosteq(value, target)
    rows.append(("triang mode grid", numerator, mp.nstr(value, 32),
                 mp.nstr(target, 32), mp.nstr(abs(value/target-1), 12)))

with (HERE / "limit-checks.csv").open("w", newline="") as handle:
    writer = csv.writer(handle, lineterminator="\n")
    writer.writerow(("case", "grid_index", "value", "reference", "relative_error"))
    writer.writerows(rows)
print(f"{len(rows)} checks at {mp.mp.dps} decimal digits")
