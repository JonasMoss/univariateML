"""Integrate the two power laws and invert their separate masses."""
import mpmath as mp

from quality.truth_reference import Reference, complement, pair


def build(t):
    b, a, r = (t[k] for k in ('scale', 'left_rate', 'right_rate'))
    left = -mp.log1p(a / r)
    right = -mp.log1p(r / a)

    def tails(x):
        if x <= 0:
            return pair(-mp.inf, True)
        if x <= b:
            return pair(left + a * mp.log(x / b), True)
        return pair(right - r * mp.log(x / b), False)

    def quantile(lp, lower):
        if lp in (-mp.inf, 0):
            return 0 if (lp == -mp.inf) == lower else mp.inf
        l, u = (lp, complement(lp)) if lower else (complement(lp), lp)
        return b * mp.exp((l - left) / a if l <= left else (right - u) / r)

    out = Reference(tails, 0, mp.inf)
    out.quantile = quantile
    out.normalization_integrand = lambda y: mp.exp(-abs(y)) * mp.exp(left if y < 0 else right)
    out.normalization_domain = [-120, -10, -1, 0, 1, 10, 120]
    return out


extreme_parameter_sets = [
    {'scale': 1, 'left_rate': 1e8, 'right_rate': 1e-8},
    {'scale': 1, 'left_rate': 1e-8, 'right_rate': 1e8},
]


def endpoint_log_density(_parameters, x):
    if x == 0:
        return -mp.inf  # The declared support is open at zero.
    return None
