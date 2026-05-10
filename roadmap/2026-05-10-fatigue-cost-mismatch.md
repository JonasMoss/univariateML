# FatigueCost vs fatigue_mle nll mismatch (deferred fix)

**Status:** found by the AI-first validation harness during the
batch sweep on 2026-05-10. Bug is real, fix deferred — flagging so
the user can decide whether to fix the C++ Cost or revise the harness
expectations.

## What the harness found

The first-principles harness flagged that `FatigueCost::operator()`
in `cppunivariate/include/cppunivariate/distributions/fatigue/mle.hpp`
computes a **different** negative log-likelihood from what `fatigue_mle`
internally maximises. Both formulas are documented in the file's
comments; they happen to disagree by an O(1) term.

## The discrepancy

`fatigue_mle` maximises the standard Birnbaum-Saunders log-density
(numerically verified by reproducing the golden `logLik` to 1e-13):

```
log f_BS = log(x + β) - 1.5·log(x) - 0.5·log(β) - log(2α)
        - 0.5·log(2π) - (x/β + β/x - 2) / (2α²)
```

`FatigueCost::operator()` evaluates a different formula:

```
log f_Cost = 0.5·log(x² + β²) - 1.5·log(x) - 0.5·log(β) - log(2α)
           - 0.5·log(2π) - (x/β + β/x - 2) / (2α²)
```

The only difference is `log(x + β)` vs `0.5·log(x² + β²)`. These are
identically equal only when one of `x`, `β` is zero; they differ by
`log((x + β)/√(x² + β²))` otherwise. On the 100-sample golden at the
recovered `(α, β)`, the gap is **84 in total log-likelihood**.

## Why existing tests didn't catch it

- `test_estimators.cpp::fatigue` only asserts that `fatigue_mle`
  recovers the golden estimates to 1e-10. It calls `fatigue_mle`,
  never `FatigueCost::operator()`.
- The score-residual `‖g‖₂ < 1e-3` smoke check from CLAUDE.md only
  applies to multi-D *BFGS-fitted* estimators (paralogis, std,
  snorm, sstd, ged, sged). Fatigue uses 1D Newton on β, so the gate
  was never wired in.
- `test_hessians.cpp` would compare `FatigueCost::hessian` against
  `finite_diff_hessian(FatigueCost)` — both inherit `operator()`,
  so the comparison is internally consistent and silent on the bug.

## What the harness did

The symbolic spec written for fatigue (in the now-deleted
`quality/math/fatigue.py`) used the standard BS density. Round-trip
showed:

- Symbolic spec ↔ `FatigueCost::operator()` would diverge at every
  evaluation point (the Cost is computing a different formula);
- but if the spec is rewritten to match `FatigueCost::operator()`,
  the gradient at the supposed MLE has `‖g‖₂ ≈ 30.83`, not zero,
  because `(α, β)` was found for the *correct* BS likelihood, not
  for the Cost's variant.

Either way the spec and the C++ pair don't form a self-consistent
unit. That's the bug.

## Fix options

1. **Fix `FatigueCost::operator()`** to use `log(x + β)`. Then
   re-derive `Cost::gradient` and `Cost::hessian` symbolically (they
   change too). Re-run the existing `test_hessians.cpp` against the
   new `FatigueCost`. Add the spec back to the harness.
2. **Leave the C++ as-is** and document that `FatigueCost` is a
   diagnostic-only object that doesn't represent fatigue's nll
   (rejected: subtle and surprising; the comment in `mle.hpp` will
   bit-rot).
3. **Revisit when the location-shift fatigue (`fatigue3`)
   roadmap entry comes back** — that work touches the density
   anyway. Fix `FatigueCost` then.

Recommended: option 1 if the reader has time; option 3 otherwise.

## Trigger to revisit

- If asymptotic-SE / observed-information work starts using
  `FatigueCost::hessian` from R, the bug becomes user-facing
  (the SEs will be wrong).
- If the location-shift fatigue variant comes back from
  `2026-05-08-variable-parameter-selection`, fix at that time.
- A user reports a fatigue-related numerical inconsistency.

## Verification when fixed

- `python -m quality.harness validate fatigue` passes at 1e-12 with
  `cost_has_gradient=True`, `cost_has_hessian=True`.
- `quality/derived/fatigue/quality_report.md` claims `validated`.
- `test_estimators.cpp::fatigue` continues to pass.
- `test_hessians.cpp` still passes (analytic-vs-FD on the *new*
  Cost).
- Golden `fatigue.json` is unchanged (fatigue_mle's internal
  formula didn't move).
