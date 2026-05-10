"""Laplace distribution — symbolic spec.

Density (in natural params (mu, sigma), sigma > 0):
    f(x; μ, σ) = exp(−|x − μ| / σ) / (2σ)
    log f = −log(2σ) − |x − μ| / σ

The mu-direction is non-smooth: |x − μ| has a kink at each x_i, so
the *observed* Hessian (= ∂²(−log f)/∂μ²) has Dirac deltas at the
observations and is zero almost everywhere — degenerate. The C++
`LaplaceCost::hessian` returns the *Fisher information × n* instead,
which is the right asymptotic-variance quantity (and for σ at the
MLE happens to coincide with the observed Hessian). The harness
mirrors that via `cost_hessian_is_fisher=True`.

Closed-form Fisher info per obs:
    I(μ, σ) = diag(1/σ², 1/σ²)
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mu = sp.Symbol("mu", real=True)
sigma = sp.Symbol("sigma", positive=True)

log_density_expr = -sp.log(2 * sigma) - sp.Abs(x_sym - mu) / sigma

SPEC = Spec(
    id="laplace",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="sigma", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    fisher_info_per_obs=sp.Matrix(
        [[1 / sigma**2, 0], [0, 1 / sigma**2]]
    ),
    references={"r": "extraDistr::dlaplace(x, mu, sigma)"},
    cost_hessian_is_fisher=True,
)
