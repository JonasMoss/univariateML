"""Birnbaum-Saunders Fatigue distribution — symbolic spec.

Density (in natural params (alpha, beta), both > 0, x > 0; mu fixed at 0
per the wrapper's convention; same parameterisation as
extraDistr::dfatigue):
    inner = x/beta + beta/x       (≥ 2 by AM-GM)
    log f = log(x + beta) - 1.5 log(x) - 0.5 log(beta)
          - log(2 alpha) - 0.5 log(2 pi) - (inner - 2)/(2 alpha²)

History: an earlier `fatigue_mle` recovered (α, β) using the formula
α² = x̄/β + β/H(x), but the standard BS α-MLE FOC has a − 2 that was
missing. The α-bias was ≈ +2 in α² in expectation (so + ~0.3 in α
for the golden's α=3 truth), regardless of sample size. The harness
caught it on 2026-05-10 and the C++ side was patched to use
`α² = x̄/β + β/H(x) − 2` in the same commit. See
roadmap/2026-05-10-fatigue-cost-mismatch.md for the full investigation.
"""

import sympy as sp

from quality.harness.spec import Param, Spec

x_sym = sp.Symbol("x", positive=True)
alpha = sp.Symbol("alpha", positive=True)
beta = sp.Symbol("beta", positive=True)
half = sp.Rational(1, 2)

inner = x_sym / beta + beta / x_sym
log_density_expr = (
    sp.log(x_sym + beta)
    - sp.Rational(3, 2) * sp.log(x_sym)
    - half * sp.log(beta)
    - sp.log(2 * alpha)
    - half * sp.log(2 * sp.pi)
    - (inner - 2) / (2 * alpha**2)
)

SPEC = Spec(
    id="fatigue",
    params=(
        Param(name="alpha", start=1.0, domain="(0, oo)"),
        Param(name="beta", start=1.0, domain="(0, oo)"),
    ),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support="(0, oo)",
    references={"r": "extraDistr::dfatigue(x, alpha, beta, mu=0)"},
    notes=(
        "mu = 0 fixed today; the location-shift variant is roadmapped "
        "(2026-05-08-variable-parameter-selection)."
    ),
    golden_to_cost=lambda e: [float(e["alpha"]), float(e["beta"])],
)
