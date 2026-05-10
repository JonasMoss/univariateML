"""Skew-Normal (Fernandez-Steel) — symbolic spec.

Density at (mu, sigma, xi), sigma > 0, xi > 0:
    (sigma_u, mu_u) = snorm_sigma(xi)        — standardisation constants
    z = sigma_u * (x − mu)/sigma + mu_u
    log f = log(sigma_u/sigma) + log(2/(xi + 1/xi))
          − 0.5 log(2π) − 0.5 z² / Xi²
    where  Xi = xi^sign(z)  → xi   if z > 0, else 1/xi.

Cost::operator() takes phi = (mu, log sigma, log xi); BFGS uses
finite_diff_cost (no analytical gradient).

The Xi-pivot makes this a parameter-conditional piecewise (the
condition `z > 0` depends on (μ, σ, ξ)). The earlier `xi^sp.sign(z)`
form caused sympy's chain rule to explode under double differentiation.
The Piecewise form below is mathematically equivalent — and the F-S
density is *continuous at z = 0* (both branches give the same value
when z = 0 since `xi^0 = (1/xi)^0 = 1`). So the DiracDelta term that
sympy emits at the moving boundary is multiplied by zero (in the
limit), and the harness's `DiracDelta → 0` substitution gives the
correct answer.
"""

import math

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mu = sp.Symbol("mu", real=True)
log_sigma = sp.Symbol("log_sigma", real=True)
log_xi = sp.Symbol("log_xi", real=True)

sigma = sp.exp(log_sigma)
xi = sp.exp(log_xi)
half = sp.Rational(1, 2)

m1 = 2 / sp.sqrt(2 * sp.pi)
mu_u = m1 * (xi - 1 / xi)
sigma_u = sp.sqrt(
    (1 - m1**2) * (xi**2 + 1 / xi**2) + 2 * m1**2 - 1
)

z = sigma_u * (x_sym - mu) / sigma + mu_u

# log f_core(z; xi) = log(2/(xi + 1/xi)) + log_dnorm_unit(z / xi^sign(z))
# Expand log_dnorm_unit(w) = -0.5*log(2π) - 0.5*w².
# w² = z² / xi^(2 sign(z)) → z² / xi²  if z > 0, else z² · xi².
log_dnorm_pos = -half * sp.log(2 * sp.pi) - half * z**2 / xi**2
log_dnorm_neg = -half * sp.log(2 * sp.pi) - half * z**2 * xi**2

log_dsnorm_core = sp.log(2 / (xi + 1 / xi)) + sp.Piecewise(
    (log_dnorm_pos, z > 0),
    (log_dnorm_neg, True),
)

log_density_expr = sp.log(sigma_u) - log_sigma + log_dsnorm_core

SPEC = Spec(
    id="snorm",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="log_sigma", start=0.0, domain="real"),
        Param(name="log_xi", start=0.0, domain="real"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    references={"r": "fGarch::dsnorm / fGarch::rsnorm"},
    cost_has_gradient=False,
    golden_to_cost=lambda e: [
        float(e["mean"]),
        math.log(float(e["sd"])),
        math.log(float(e["xi"])),
    ],
)
