"""Skew Student-t (Fernandez-Steel on standardised t) — symbolic spec.

Density at (mu, sigma, nu, xi), sigma > 0, nu > 2, xi > 0:
    (sigma_u, mu_u) = sstd_sigma(nu, xi)        — standardisation constants
    z = sigma_u * (x − mu)/sigma + mu_u
    log f = log(sigma_u/sigma) + log(2/(xi + 1/xi))
          + log_dstd_unit(z / xi^sign(z); nu)

Cost::operator() takes phi = (mu, log sigma, log(nu - 2), log xi); BFGS
uses finite_diff_cost (no analytical gradient).

Uses the same data-conditional-Piecewise rewrite of `xi^sign(z)` as
snorm.py — the F-S branches agree at z = 0 (since `xi^0 = (1/xi)^0 = 1`),
so the parameter-conditional Piecewise's DiracDelta-on-the-boundary
contribution is multiplied by zero and the harness's `DiracDelta → 0`
substitution is correct here.
"""

import math

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mu = sp.Symbol("mu", real=True)
log_sigma = sp.Symbol("log_sigma", real=True)
log_nu_shift = sp.Symbol("log_nu_shift", real=True)
log_xi = sp.Symbol("log_xi", real=True)

sigma = sp.exp(log_sigma)
nu = sp.exp(log_nu_shift) + 2  # phi[2] = log(nu - 2)
xi = sp.exp(log_xi)
half = sp.Rational(1, 2)

# sstd_sigma: |.|-mean of variance-standardised t with df=nu, var=1.
# m1 = Γ((nu-1)/2)/Γ(nu/2) · √((nu-2)/π).
m1 = (
    sp.gamma((nu - 1) / 2) / sp.gamma(nu / 2) * sp.sqrt((nu - 2) / sp.pi)
)
mu_u = m1 * (xi - 1 / xi)
sigma_u = sp.sqrt(
    (1 - m1**2) * (xi**2 + 1 / xi**2) + 2 * m1**2 - 1
)

z = sigma_u * (x_sym - mu) / sigma + mu_u


def log_dstd_unit(w, nu_):
    """Log-density of the unit-variance Student-t at w (matches the C++
    log_dstd_unit in special/skewed_densities.hpp)."""
    return (
        sp.loggamma((nu_ + 1) / 2)
        - sp.loggamma(nu_ / 2)
        - half * sp.log((nu_ - 2) * sp.pi)
        - half * (nu_ + 1) * sp.log(1 + w**2 / (nu_ - 2))
    )


# w = z / xi^sign(z) → w_pos = z/xi  (when z > 0); w_neg = z·xi (when z < 0).
log_dsstd_core = sp.log(2 / (xi + 1 / xi)) + sp.Piecewise(
    (log_dstd_unit(z / xi, nu), z > 0),
    (log_dstd_unit(z * xi, nu), True),
)

log_density_expr = sp.log(sigma_u) - log_sigma + log_dsstd_core

SPEC = Spec(
    id="sstd",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="log_sigma", start=0.0, domain="real"),
        Param(name="log_nu_shift", start=math.log(4.0), domain="real"),
        Param(name="log_xi", start=0.0, domain="real"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    references={"r": "fGarch::dsstd / fGarch::rsstd"},
    cost_has_gradient=False,
    golden_to_cost=lambda e: [
        float(e["mean"]),
        math.log(float(e["sd"])),
        math.log(float(e["nu"]) - 2.0),
        math.log(float(e["xi"])),
    ],
)
