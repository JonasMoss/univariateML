"""Binomial distribution — symbolic spec.

PMF (size = n_trials fixed, prob = p ∈ (0, 1), x ∈ {0, 1, ..., size}):
    f(x; size, p) = C(size, x) · p^x · (1 − p)^(size − x)

Per `BinomCost::operator()`'s "+ const" convention, we drop the
data-only `log C(size, x)` term — it cancels in derivatives, so
gradient and Hessian aren't affected.
    log f (up to const) = x · log(p) + (size − x) · log(1 − p)

The C++ Cost takes `size` as a constructor argument (the `BinomCost`
struct field). The harness's dispatch lambda hardcodes the same value;
this spec hardcodes it as well — if the golden is regenerated with a
different `size`, both must be updated in lockstep.
"""

import sympy as sp

from quality.harness.spec import Param, Spec

# Hardcoded from quality/golden/binom.json::estimates.size; if you
# regenerate that golden with a different size, update here AND in the
# corresponding dispatch lambda in cppunivariate/tests/test_validation.cpp.
SIZE = 9

x_sym = sp.Symbol("x", nonnegative=True, integer=True)
p = sp.Symbol("prob", positive=True)

log_density_expr = x_sym * sp.log(p) + (SIZE - x_sym) * sp.log(1 - p)

SPEC = Spec(
    id="binom",
    params=(Param(name="prob", start=0.5, domain="(0, 1)"),),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support=f"{{0, ..., {SIZE}}}",
    references={"r": f"stats::dbinom(x, size={SIZE}, prob)"},
    notes=f"size = {SIZE} hardcoded — see top of file.",
)
