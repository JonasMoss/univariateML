"""Zipf distribution — symbolic spec.

PMF (N = upper-support fixed at max(x), s > 0, x ∈ {1, ..., N}):
    f(x; N, s) = x^(−s) / H_N(s),  where H_N(s) = Σ_{k=1..N} k^(−s)

`ZipfCost` takes `N` as a constructor argument; the harness dispatch
lambda hardcodes the same value (matching the golden's recovered
N̂ = max(data) = 3 here). If the golden is regenerated against
different data with a different max, update both in lockstep.

N is a non-regular boundary parameter (sits at max(x)), but since the
Cost is 1-D in `s` only and the harness just verifies the smooth-s
gradient/Hessian, no boundary handling is needed.
"""

import sympy as sp

from quality.harness.spec import Param, Spec

# From quality/golden/zipf.json::estimates.N; update here AND in the
# dispatch lambda in cppunivariate/tests/test_validation.cpp if regenerated.
N_FIXED = 3

x_sym = sp.Symbol("x", positive=True, integer=True)
s = sp.Symbol("s", positive=True)

# H_N(s) is a finite sum of N terms — fully symbolic.
H_N = sum(sp.Integer(k) ** (-s) for k in range(1, N_FIXED + 1))

log_density_expr = -s * sp.log(x_sym) - sp.log(H_N)

SPEC = Spec(
    id="zipf",
    params=(Param(name="s", start=1.0, domain="(0, oo)"),),
    x_symbol=x_sym,
    log_density=log_density_expr,
    support=f"{{1, ..., {N_FIXED}}}",
    references={"r": f"sads::dzipf(x, N={N_FIXED}, s)"},
    golden_to_cost=lambda e: [float(e["s"])],
)
