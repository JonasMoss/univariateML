# Simulation-recovery harness (deferred)

**Status:** deferred — full implementation reverted at `7d2569d`; original
code preserved at parent `acb5e43`.

## The idea

Verify each estimator's *fitting machinery* (separate from the math
correctness verified by the symbolic-derivative round-trip) by repeatedly
sampling from the true distribution and fitting:

- **Consistency:** mean(θ̂) → θ_true and median(θ̂) → θ_true as N grows.
- **Asymptotic normality:** (θ̂ − θ_true)·√N approximately
  N(0, I(θ_true)⁻¹) under regularity. Tested via KS on each parameter
  marginal.
- **Cramér–Rao agreement:** N · Cov(θ̂) → I(θ_true)⁻¹ as N → ∞. When
  Fisher info has no closed form (e.g. Student-t for general ν),
  evaluate I numerically as the Monte-Carlo expectation of the outer
  product of the score over fresh samples.

## Why it's not in the v1 harness

The "AI-first validation" core (Phases 1–3a) is *sympy-derived tests*:
write the density once, the harness symbolically differentiates and
checks the C++ implementation matches at 1e-12. Sim recovery is a
**separate apparatus** — it tests the optimiser (BFGS / Newton / closed
form), not the math. Both are valuable; conflating them makes the v1
harness more complex than it needs to be for its stated purpose.

The v1 harness already proves that gradient and Hessian formulas are
correct against the symbolic ground truth. If those are right, sim
recovery basically *has to* succeed (asymptotically); when it fails it
points at the optimiser, line search, starting values, or boundary
handling — different debugging space.

## What landed (and is now reverted)

Commit `acb5e43` shipped:

- `univariateML-cpp/tests/sim_driver.cpp` — JSON-in/out batch fit driver.
  For each (N, seed): seed an `mt19937_64` `StdRng`, draw N samples
  from the named distribution at supplied true params, fit via `*_mle`,
  record estimates / logLik / iterations / status. Built but not a
  ctest; invoked by simulate.py.
- `dev/validation/quality/harness/simulate.py` — builds the request, shells to
  sim_driver once for the whole sweep, summarises per-N: mean bias,
  median bias (robust to MLE-pathology outliers), empirical SD,
  N · Var(θ̂). Outputs `dev/validation/quality/derived/<id>/simulation_recovery.json`.
- `__main__.py` `simulate` subcommand and `--N` / `--reps` flags.

Validated on exp (true rate=1) and std (true mean=0, sd=1, ν=3). For
std at N=200, ~3% of fits drift to the ν→2 boundary; median bias
remained essentially zero, mean bias was outlier-skewed — surfaced
honestly via the mean-vs-median report.

A subtle bug was fixed during bring-up: mt19937_64 seeds round-tripped
through JSON as doubles, exactly representable only to 2^53. Mixing
schemes that produced large seeds caused adjacent reps to collide.
The fix (sequential counter offset by base seed) is in `simulate.py`
at acb5e43; whoever resurrects this should keep the counter approach.

## Trigger to revisit

Take this off the shelf when **any** of:

- The symbolic-derivative harness flags an estimator as `validated` but
  empirical SE / CI behaviour looks wrong on real data.
- We add an estimator using a non-trivial optimiser path (e.g.
  augmented Lagrangian, EM, MCMC initialisation) where the math is
  algebraically correct but convergence behaviour is the question.
- We start exposing asymptotic SEs to users (post-fit `vcov`-style
  output) and need the Cramér–Rao agreement check live in CI.
- A bug-report points at a specific distribution where the user's
  empirical bias diverges from theory — the harness becomes a debugging
  tool.

## How to resurrect

```sh
git show acb5e43 -- cppunivariate/tests/sim_driver.cpp \
  univariateML-cpp/tests/CMakeLists.txt \
  dev/validation/quality/harness/simulate.py \
  dev/validation/quality/harness/__main__.py
```

Cherry-pick or re-apply the diff. Validation pilots (`exp`, `std`)
already had `simulation_recovery.json` artefacts — they're at acb5e43
too if you want a snapshot for comparison.

## Verification when revisited

- The harness produces simulation_recovery.json for at least one
  closed-form (exp), one BFGS-fitted multi-D (std), and one
  Boost.Math-backed (norm) estimator.
- For each, median bias → 0 as N grows on a clean sweep
  (200 / 1000 / 5000), and the report flags MLE-pathology outliers
  rather than hiding them in the mean.
- KS p-values on each marginal of (θ̂ − θ_true)·√N stay above 0.001
  at N ≥ 5000 — alarm threshold, not a tight fit.
- Fisher-info check (closed form when available, MC otherwise)
  agrees with N·Cov(θ̂) to within 5–10% at N ≥ 5000.
- `just simulate <id>` recipe wired in `Justfile`.
