# Validation and goldens (substantially addressed 2026-05-10)

**Status:** substantially addressed by the v1 sympy-derivative
harness (commits `d448e11` Phase 1 → `f3e91cf` Phase 5). Goldens
explicitly demoted from primary correctness signal to drift-detection
layer. dpqr round-trip and simulation recovery split into their own
deferred entries; edge-cases and cross-language parity still TODO.

## The idea

Keep golden regression tests, but make them one layer in a broader validation
system.

## Why it matters

Goldens detect accidental numerical drift. They do not prove an estimator is
correct. A wrong implementation can reproduce its own goldens forever.

The project also needs tests that ask whether the score is close to zero, the
Hessian is plausible, simulations recover parameters, and edge cases fail in
documented ways.

## What landed (v1 harness, May 2026)

The substantive new layer is **symbolic-derivative round-trip**: each
pilot distribution declares its `log_density` once in sympy at
`dev/validation/quality/math/<id>.py`, and the harness:

- symbolically differentiates → `nll`/gradient/Hessian on a Sobol grid
- evaluates at `mpmath` precision (50 dps)
- diffs against the C++ `Cost::operator()`/`gradient`/`hessian` at
  the same grid points
- claims `validated` in `dev/validation/quality/derived/<id>/quality_report.md` when
  all gating checks match at 1e-12

Coverage: 41 of 42 distributions (all that have their own `mle.hpp`).
The pattern proved itself across closed-form 1-D / closed-form 2-D /
3-D BFGS-with-FD-grad / Boost.Math seam / Fernandez-Steel skew family
/ discrete / boundary-MLE / Fisher-info-as-Hessian — see
[2026-05-10-validation-harness.md](2026-05-10-validation-harness.md)
for the full shape catalogue.

## Layer status

| Validation layer | Status |
|---|---|
| Golden regression | landed (existing, unchanged) — drift-detection layer |
| Score residual at MLE | landed (existing) — `‖g‖₂ < 1e-3` gate now uniform via the harness |
| Analytical-vs-FD Hessian | landed (existing in `test_hessians.cpp`) |
| Closed-form Fisher info match | landed for normal/exp/gamma/beta/laplace (existing) |
| **Symbolic ∇/H vs `Cost::gradient`/`hessian`** | **landed v1 harness, 41 of 42 distros at 1e-12** |
| Reference `d/p/q/r` cross-check | offline (`dpqr_check.R`) — split out to [dpqr-validation roadmap entry](2026-05-10-dpqr-validation.md) |
| Simulation recovery | implemented and reverted; preserved at acb5e43 — split out to [sim-recovery roadmap entry](2026-05-10-sim-recovery.md) |
| Edge-case tests for boundaries / degeneracies | TODO |
| Cross-language consistency | TODO (when Python shell exists) |

## Trigger to revisit (the still-deferred parts)

Take this off the shelf when the **edge-case** or **cross-language**
layers become urgent — typically:

- A distribution where the MLE has known boundary pathology (uniform's
  `min(x)`, Pareto's `min(x)`, paralogis at `kappa → ∞`) is touched
  and we want explicit boundary tests.
- Python bindings start landing (the spec hashes already cross-validate
  the math; what's missing is a parallel test target on the Python side).

## Verification when fully closed

- Edge-case test layer covers each distribution where MLE existence
  conditions are non-trivial (lomax mean-variance condition, std `nu>2`,
  fatigue with `mu`, etc.).
- Python shell, when it exists, produces the same `cpp_eval.json`
  shape against the same `eval_request.json`, with identical `spec_hash`.
