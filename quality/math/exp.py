"""Exponential distribution — symbolic spec.

Density:    f(x; rate) = rate * exp(-rate * x),  x > 0
Support:    (0, oo)
MLE:        rate_hat = 1 / mean(x)  (closed form)

Cost::operator() takes `rate` directly (natural-space; no phi transform).
Reference: stats::dexp / stats::rexp (R base).
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x = sp.Symbol("x", positive=True)
rate = sp.Symbol("rate", positive=True)
p = sp.Symbol("p", positive=True)

SPEC = Spec(
    id="exp",
    params=(Param(name="rate", start=1.0, domain="(0, oo)"),),
    x_symbol=x,
    log_density=sp.log(rate) - rate * x,
    support="(0, oo)",
    cdf=1 - sp.exp(-rate * x),
    quantile=-sp.log(1 - p) / rate,
    fisher_info_per_obs=sp.Matrix([[1 / rate**2]]),
    references={"r": "stats::dexp(x, rate)"},
    notes="Closed-form MLE = 1/mean(x). Fisher info I(rate) = 1/rate^2.",
)
