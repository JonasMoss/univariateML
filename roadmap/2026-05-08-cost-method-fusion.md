# Cost-method fusion (deferred)

**Status:** deferred — no production callsite today.

## The redundancy

The Cost structs in `cppunivariate/include/cppunivariate/distributions/<id>/mle.hpp`
(`GumbelCost`, `NormCost`, `StudentTCost`, …) implement `operator()`,
`gradient`, and `hessian` as three independent methods, each looping
over `x` from scratch. Per-obs
subexpressions (e.g. Gumbel's `z` and `e = exp(-z)`, Normal's
`d = x - μ`) are recomputed in each method.

If `gradient` and `hessian` are called at the same theta, that is two
passes over the data and two recomputations of the shared subexpression
where one would do.

## Why it doesn't matter today

Audit on 2026-05-08:

- BFGS (`cppunivariate/include/cppunivariate/core/bfgs.hpp`) only calls
  `operator()` and `gradient`. The line search caches `f_new` at the
  accepted theta, so even those two are not back-to-back at the same
  point.
- Newton-MD (`cppunivariate/include/cppunivariate/core/newton_md.hpp`)
  *does* call `gradient(theta_new)` and `hessian(theta)` at the same
  theta (lines 188 and 210, after the swap on line 207) — but its only
  callers are in `cppunivariate/tests/test_newton_md.cpp`. No
  production estimator uses `newton_md_minimize`.
- 1D Newton (`newton_raphson_1d` / `_abs` in
  `cppunivariate/include/cppunivariate/core/newton.hpp`) takes a scalar
  `f_over_df` lambda; it does not touch the Cost concept.
- `Cost::hessian` therefore has zero production callsites today.

The Phase-3 analytic-Hessian commits (batch 2d/2e/2f/3a/3b in the git
log around late April 2026) are groundwork for a future Newton-MD
swap-in or an asymptotic-SE pass; they do not currently exercise the
redundancy.

## Why per-theta caching on the Cost is the wrong fix

The natural first thought — let the struct cache `theta` plus the
shared subexpression and short-circuit on a hit — does not survive
contact with how theta is used: it mutates by small amounts every
iteration, array equality is brittle (1-ULP perturbations look like
misses), invalidation logic has to be added, and thread-safety becomes
a concern if the same Cost is ever shared. The savings on small-N
likelihoods do not justify it.

## Design when revisited

Extend the Cost concept with an **optional** fused method:

```cpp
void grad_hessian(const double *theta, double *g, double *H) const;
```

No `operator()` value in the signature — Newton-MD already has `f_new`
cached from its line search, so an extra `f` field would be unused. If
a future optimiser wants all three at once, add `value_grad_hessian`
as a second optional method rather than widening this one.

Wire it via the existing trait machinery in
`cppunivariate/include/cppunivariate/core/concept_traits.hpp`: add
`has_grad_hessian<C>` mirroring `has_gradient` / `has_hessian`. `newton_md_minimize` dispatches with
`if constexpr` — fused method when present, falls back to separate
`gradient` + `hessian` otherwise. Zero overhead for Costs that do not
opt in.

Pilot order (clean shared subexpressions, analytic Hessians already
present):

1. `distributions/gumbel/mle.hpp` — shared `z` and `e = exp(-z)`.
2. `distributions/norm/mle.hpp` — shared `d = x - μ`.
3. `distributions/std/mle.hpp` — shared `(x - μ) / σ` and `1 + z² / ν`.

After the pilot, sweep the rest of the estimators with both analytic
gradient and analytic Hessian.

## Trigger to revisit

Take this off the shelf when **any** of the following lands:

- A production estimator switches its fitting path to
  `newton_md_minimize`.
- A post-fit asymptotic-SE or standard-error pass starts calling
  `Cost::hessian` from `univariateML/src/ml_exports.cpp` or an
  inference layer.
- Profiling shows BFGS is gradient-bound on a hot estimator and the
  gradient computation has a heavy shared subexpression with `f`. Note
  that this last case would motivate a `value_and_gradient` method
  instead — different fix, different optional trait.

## Verification when revisited

- `just cpp-test` — Newton-MD doctest must pass on both fused and
  non-fused estimators.
- `just test` — full testthat + golden suite stays green at 1e-12.
- For each estimator with `grad_hessian`, add a doctest that calls the
  fused method and the separate `gradient` + `hessian` at a few random
  thetas and checks element-wise equality to ~1e-14.
