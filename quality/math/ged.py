"""Generalized Error Distribution (GED, symmetric) — symbolic spec.

Density at (mu, sigma, nu), sigma > 0, nu > 0:
    f(x; mu, sigma, nu) = (1/sigma) · f_unit((x - mu) / sigma; nu)

Per-obs nll (in phi-space (mu, log sigma, log nu)):
    a = x - mu
    lambda(nu) = sqrt(2^(-2/nu) · Γ(1/nu) / Γ(3/nu))
    w = |a| / (sigma · lambda(nu))
    nll = log sigma + 0.5 · w^nu + C(nu)
    C(nu) = -log nu + 1.5·loggamma(1/nu) - 0.5·loggamma(3/nu) + log 2

Cost::operator() takes phi-space; BFGS uses finite_diff_cost
(no analytical gradient).
"""

import math

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mu = sp.Symbol("mu", real=True)
log_sigma = sp.Symbol("log_sigma", real=True)
log_nu = sp.Symbol("log_nu", real=True)

sigma = sp.exp(log_sigma)
nu = sp.exp(log_nu)
half = sp.Rational(1, 2)

a = x_sym - mu
lambda_nu = sp.sqrt(2 ** (-2 / nu) * sp.gamma(1 / nu) / sp.gamma(3 / nu))
# Express |a|^nu as (a^2)^(nu/2) so sympy doesn't introduce sign/Abs (and
# their DiracDelta second derivatives) — equivalent for real a, smoother
# under sp.diff for nu > 2 which is the regime our pilot lives in.
abs_a_pow_nu = (a**2) ** (nu / 2)
W = half * abs_a_pow_nu / (sigma * lambda_nu) ** nu
C_nu = (
    -sp.log(nu)
    + sp.Rational(3, 2) * sp.loggamma(1 / nu)
    - half * sp.loggamma(3 / nu)
    + sp.log(2)
)

log_density_expr = -log_sigma - W - C_nu

SPEC = Spec(
    id="ged",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="log_sigma", start=0.0, domain="real"),
        Param(name="log_nu", start=math.log(2.0), domain="real"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    references={"r": "fGarch::dged / fGarch::rged"},
    cost_has_gradient=False,
    golden_to_cost=lambda e: [
        float(e["mean"]),
        math.log(float(e["sd"])),
        math.log(float(e["nu"])),
    ],
)
