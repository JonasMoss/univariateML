# dpqr round-trip validation in the harness (deferred)

**Status:** substantially addressed 2026-10-30: every family's d/p/q/r is checked against an independent mpmath truth table (TASK-78.2, `distributions/<id>/truth.json`) and boundary closure was audited (TASK-78.14). Earlier: deferred — symbolic-derivative harness handles MLE math
first; dpqr is the natural next layer.

## The idea

Extend the validation harness to verify each distribution's `d`/`p`/`q`/`r`
implementation against:

1. **Self-consistency** (no external reference needed):
   - ∫ f(x) dx = 1 (numerical, e.g. `scipy.integrate.quad` with the
     symbolic density already in the spec).
   - F(F⁻¹(p)) = p on a grid of p ∈ (0, 1).
   - F⁻¹(F(x)) = x on a grid of x in the support.
   - r(N) → samples whose empirical CDF approaches F as N grows
     (Glivenko–Cantelli; KS test for alarm).
2. **Reference-package cross-check** (extends the existing offline
   `bindings/r/scripts/dpqr_check.R`):
   - For each distribution with a trusted reference (extraDistr,
     actuar, fGarch, scipy.stats, statmod, etc.), compare d/p/q at a
     fixed parameter setting on a grid of x values. Tolerance: 1e-10
     for d and p, 1e-8 for q (compounds errors).
3. **Symbolic CDF verification** (when the spec carries `cdf` or
   `quantile`):
   - Differentiate the symbolic CDF, recover the density, check it
     equals the symbolic `log_density` (exp'd) at the eval grid.
     This catches CDF formulas that don't match their stated density.

## Why this isn't in v1

The v1 harness focuses on MLE math (gradient / Hessian / Fisher), which
is the part most prone to algebra bugs and most expensive to check by
hand. dpqr is largely composition of well-tested building blocks
(Boost.Math primitives) and is already cross-checked offline against
external R packages — adding the wrapper layer doesn't unlock new
correctness signals, it just makes existing checks live in CI.

Doing it later also means the spec's optional `cdf` / `quantile`
fields (already declared in `Spec`) can be filled in lazily as the
harness sweeps each distribution, rather than blocking the v1 release
on having all 42 dpqr forms in sympy form.

## Existing assets to lean on

- `bindings/r/scripts/dpqr_check.R` — current offline reference
  comparison against extraDistr/actuar/fGarch/logitnorm/sads/statmod.
  Promote into the harness flow: refdpqr.py shells to R or
  re-implements in Python via scipy where possible.
- `dev/validation/quality/math/<id>.py::Spec.cdf` and `.quantile` — already declared
  in the dataclass, currently `None` for most distributions.
- `dev/validation/quality/golden/<id>.json` — fixed reference dataset to check
  empirical-CDF agreement on.

## Trigger to revisit

Take this off the shelf when **any** of:

- The MLE-math harness has now covered all 41 of 42 distributions
  with a Cost; dpqr cross-check is the natural second layer to add
  on top.
- A user reports a `d`/`p`/`q`/`r` bug for a specific distribution
  (rare but happens — the harness becomes a debugging surface).
- We add a new distribution where dpqr is novel (not just composition
  of Boost primitives) — for those we want both the MLE-math check
  and dpqr self-consistency from day one.

## Verification when revisited

- Each distribution that ships with a closed-form `dpqr.hpp` has a
  `dpqr_consistency.json` derived artifact (a pilot trio of exp /
  std / norm is enough to prove the layer; the sweep is mechanical
  once that's in place):
  - `integrate_density` < 1e-9 from 1
  - `quantile_inverse_grid_max_err` < 1e-10
  - `cdf_inverse_grid_max_err` < 1e-10
  - `reference_max_diff` < 1e-10 (d, p) or 1e-8 (q) when a reference
    package is declared in `Spec.references`
- The check is wired into `python -m quality.harness validate <id>`
  as one more report alongside `gradient_report.json` /
  `hessian_report.json`.
- For distributions that declare `Spec.cdf`, the symbolic ∂F/∂x
  matches `exp(log_density)` at the eval grid to 1e-12 (catches
  internally inconsistent specs).
- The R-side `dpqr_check.R` is either retired or thinly wraps the
  Python harness, removing the offline manual step.
