"""Standardised Student-t — symbolic spec.

Density (in natural params (mean, sd, nu), sd > 0, nu > 2):
    z = (x - mean) / sd
    f(x) = (1/sd) * f_unit_t(z; nu)
where f_unit_t is the unit-variance Student-t (i.e. f_t(t; nu) rescaled
so the resulting variance is 1, not nu/(nu-2)).

Cost::operator() takes phi = (mean, log(sd), log(nu - 2)). The symbolic
log-density is written in phi-space directly so derivatives match
Cost::gradient and Cost::hessian without a Jacobian transform.

Cost::gradient is **not** implemented analytically in std/mle.hpp; BFGS
fits via finite_diff_cost<StudentTCost>. The harness sets
`cost_has_gradient=False` so the 1e-12 grad-vs-symbolic check is skipped
(the FD gradient is implicitly verified by the nll match: the FD stencil
reads from the same operator()).

Cost::hessian is analytical and sympy-derived — see std/mle.hpp lines
37–50 for the closed-form expressions; the harness independently
re-derives via sp.diff and checks the implementation.
"""

import math

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mu = sp.Symbol("mu", real=True)
log_sigma = sp.Symbol("log_sigma", real=True)
log_nu_shift = sp.Symbol("log_nu_shift", real=True)

sigma = sp.exp(log_sigma)
nu = sp.exp(log_nu_shift) + 2  # nu - 2 = exp(log_nu_shift) > 0

z = (x_sym - mu) / sigma  # standardised
half = sp.Rational(1, 2)

# log_dstd_unit(z, nu) with the (nu-2) cancellation:
#   = lgamma((nu+1)/2) - lgamma(nu/2)
#     - (1/2) log((nu-2) pi)
#     - ((nu+1)/2) log(1 + z^2/(nu-2)).
log_dstd_unit_expr = (
    sp.loggamma(half * (nu + 1))
    - sp.loggamma(half * nu)
    - half * sp.log((nu - 2) * sp.pi)
    - half * (nu + 1) * sp.log(1 + z**2 / (nu - 2))
)

# f(x; mu, sigma, nu) = (1/sigma) * dstd_unit(z; nu)  ⇒
# log f = log_dstd_unit(z; nu) - log_sigma.
log_density_expr = log_dstd_unit_expr - log_sigma

SPEC = Spec(
    id="std",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="log_sigma", start=0.0, domain="real"),
        Param(name="log_nu_shift", start=math.log(4.0), domain="real"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    cdf=None,
    quantile=None,
    fisher_info_per_obs=None,  # no closed form for general nu
    references={"r": "fGarch::dstd / fGarch::rstd"},
    notes=(
        "Phi-space (mu, log sigma, log(nu - 2)). Cost has analytical "
        "Hessian but FD-only gradient; BFGS uses finite_diff_cost wrapper."
    ),
    cost_has_gradient=False,
    golden_to_cost=lambda e: [
        float(e["mean"]),
        math.log(float(e["sd"])),
        math.log(float(e["nu"]) - 2.0),
    ],
)
