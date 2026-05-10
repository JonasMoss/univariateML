"""Zero-Inflated Poisson — symbolic spec.

PMF (in natural params (λ > 0, π ∈ [0, 1)), x ∈ {0, 1, 2, ...}):
    For x = 0: f = π + (1 − π)·exp(−λ)
    For x > 0: f = (1 − π)·exp(−λ)·λ^x / x!

Per `ZipCost::operator()`, the x>0 branch uses lgamma(x+1) — i.e.
the data-only constant is *not* dropped. The spec mirrors that.

The piecewise structure means gradient and Hessian are also
piecewise. sympy's Piecewise lambdifies through mpmath fine on the
condition `Eq(x, 0)` since mpmath's mpf(0) == sympy's Integer(0).
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", nonnegative=True, integer=True)
lam = sp.Symbol("lambda", positive=True)
pi_p = sp.Symbol("pi", positive=True)  # name "pi" matches golden; sp.Symbol distinct from sp.pi

log_density_expr = sp.Piecewise(
    (
        sp.log(pi_p + (1 - pi_p) * sp.exp(-lam)),
        sp.Eq(x_sym, 0),
    ),
    (
        sp.log(1 - pi_p) - lam + x_sym * sp.log(lam) - sp.loggamma(x_sym + 1),
        True,
    ),
)

SPEC = Spec(
    id="zip",
    params=(
        Param(name="lambda", start=1.0, domain="(0, oo)"),
        Param(name="pi", start=0.5, domain="(0, 1)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="{0, 1, 2, ...}",
    references={"r": "extraDistr::dzip(x, lambda, pi)"},
)
