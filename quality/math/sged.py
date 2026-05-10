"""Skew GED (Fernandez-Steel on symmetric GED) — symbolic spec.

Density at (mu, sigma, nu, xi), sigma > 0, nu > 0, xi > 0:
    (sigma_u, mu_u) = sged_sigma(nu, xi)        — standardisation constants
    z = sigma_u * (x − mu)/sigma + mu_u
    log f = log(sigma_u/sigma) + log(2/(xi + 1/xi))
          + log_dged_unit(z / xi^sign(z); nu)

Cost::operator() takes phi = (mu, log sigma, log nu, log xi); BFGS uses
finite_diff_cost (no analytical gradient).

Same data-conditional-Piecewise pattern as snorm and sstd — F-S branches
agree at z = 0, so the harness's `DiracDelta → 0` substitution gives the
right answer at the moving boundary. The `(z²)^(nu/2)` rewrite of `|z|^nu`
from ged.py is reused for each branch.
"""

import math

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", real=True)
mu = sp.Symbol("mu", real=True)
log_sigma = sp.Symbol("log_sigma", real=True)
log_nu = sp.Symbol("log_nu", real=True)
log_xi = sp.Symbol("log_xi", real=True)

sigma = sp.exp(log_sigma)
nu = sp.exp(log_nu)
xi = sp.exp(log_xi)
half = sp.Rational(1, 2)

ged_lambda = sp.sqrt(2 ** (-2 / nu) * sp.gamma(1 / nu) / sp.gamma(3 / nu))
ged_g = nu / (ged_lambda * 2 ** (1 + 1 / nu) * sp.gamma(1 / nu))
m1 = 2 ** (1 / nu) * ged_lambda * sp.gamma(2 / nu) / sp.gamma(1 / nu)
mu_u = m1 * (xi - 1 / xi)
sigma_u = sp.sqrt(
    (1 - m1**2) * (xi**2 + 1 / xi**2) + 2 * m1**2 - 1
)

z = sigma_u * (x_sym - mu) / sigma + mu_u


def log_dged_unit(w):
    """Log-density of the unit-variance GED at w (matches the C++
    log_dged_unit). Uses (w²)^(nu/2) instead of |w|^nu — equivalent for
    real w, smoother under sp.diff for nu > 2; the harness's
    DiracDelta → 0 patch handles the residual boundary terms."""
    return sp.log(ged_g) - half * (w**2) ** (nu / 2) / ged_lambda**nu


log_dsged_core = sp.log(2 / (xi + 1 / xi)) + sp.Piecewise(
    (log_dged_unit(z / xi), z > 0),
    (log_dged_unit(z * xi), True),
)

log_density_expr = sp.log(sigma_u) - log_sigma + log_dsged_core

SPEC = Spec(
    id="sged",
    params=(
        Param(name="mu", start=0.0, domain="real"),
        Param(name="log_sigma", start=0.0, domain="real"),
        Param(name="log_nu", start=math.log(2.0), domain="real"),
        Param(name="log_xi", start=0.0, domain="real"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="real",
    references={"r": "fGarch::dsged / fGarch::rsged"},
    cost_has_gradient=False,
    golden_to_cost=lambda e: [
        float(e["mean"]),
        math.log(float(e["sd"])),
        math.log(float(e["nu"])),
        math.log(float(e["xi"])),
    ],
)
