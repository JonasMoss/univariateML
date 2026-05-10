# Fatigue MLE recovers a non-MLE point — α-formula off by 2 (deferred fix)

**Status:** found by the AI-first validation harness on 2026-05-10.
The bug is real and runs deeper than initially suspected; fix
deferred pending the user's decision on approach + scope of golden
regeneration.

## What the harness found

The harness flagged `FatigueCost` first, but on closer inspection the
bug lives in `fatigue_mle` itself. Three layers all disagree about
what fatigue's likelihood is:

1. **Standard Birnbaum-Saunders density** (what `extraDistr::rfatigue`
   samples from, what `extraDistr::dfatigue` evaluates):
   ```
   log f = log(x + β) - 1.5 log(x) - 0.5 log(β)
         - log(2α) - 0.5 log(2π) - (x/β + β/x - 2) / (2α²)
   ```
2. **`fatigue_mle`'s reported logLik**: matches (1) — it computes the
   logLik via standard `dnorm(z)` after standardising. ✓
3. **`fatigue_mle`'s recovered α**: closed-form
   `α = √(x̄/β + β/H(x))` where `H` is the harmonic mean of `x`.
   But the standard-BS α-FOC says `α² = x̄/β + β/H(x) − 2`
   (the −2 is missing). So the recovered α is biased high.
4. **`FatigueCost::operator()`**: a third formula again,
   `0.5·log(x²+β²)` instead of either `log(x+β)` or any consistent
   choice — disagrees with (1) by an O(1) term.

## Numerical evidence

On the 500-sample `quality/golden/fatigue.json` (sampler args
α=3, β=2):

| Quantity            | Value      |
|---------------------|------------|
| Sampler α           | 3.000      |
| Recovered α         | 3.189      |
| **Actual BS MLE α** | **2.858**  |
| Recovered β         | 1.964      |
| BS MLE β            | 1.964 (same) |
| Recovered logLik    | -1360.392  |
| **BS MLE logLik**   | **-1354.813** (5.6 units higher) |

In expectation: under BS(α, β) the harmonic-mean identity gives
`E[X̄/β + β/H(X)] = 2 + α²`, so `α²_code = α²_true + 2`. So as N
grows, α_code → √(α_true² + 2), not α_true. Bias is fixed in the
limit, not vanishing.

## Why existing tests didn't catch this

- `test_estimators.cpp::fatigue` only asserts
  `fatigue_mle(x).{alpha, beta, logLik}` match the goldens — and
  the goldens were generated from this same `fatigue_mle`. Self-
  consistent regression test, blind to algorithmic correctness.
- `test_hessians.cpp::fatigue` compares analytical-vs-FD Hessian on
  `FatigueCost`, which has its own (different again) likelihood
  formula. Internally consistent within `FatigueCost`, also blind.
- The score-residual `‖g‖₂ < 1e-3` smoke check from CLAUDE.md is a
  BFGS-family-only convention; fatigue uses 1D Newton on β so the
  gate was never wired in.

The harness caught it on the first attempt by *independently
deriving* the gradient symbolically and evaluating at the recovered
MLE — a check no existing R-side test does.

## Why fixing this is a real engineering decision

Three options, all with consequences:

1. **Fix `fatigue_mle` to use `α² = x̄/β + β/H(x) − 2`.** Mathematically
   correct. But this changes recovered α for *every* user since the
   package shipped, regenerates the fatigue golden (5.6 logLik units
   off), and may break user analyses that depend on the current
   behaviour. Right move scientifically; loud move
   release-management-wise.
2. **Leave `fatigue_mle` as-is, document it as recovering a particular
   non-MLE estimator.** Add a note to the `fatigue` man page that
   what it returns is *not* the standard-BS MLE. Avoids breaking
   users; entrenches the bug.
3. **Add a flag to `mlfatigue()` for which estimator to use,**
   defaulting to current behaviour with a deprecation warning. Ramp
   over a CRAN release cycle.

Independent of the choice on `fatigue_mle`, `FatigueCost` should be
made consistent with whichever density `fatigue_mle` is meant to
target.

## Trigger to revisit

- A user reports fatigue MLE numbers that disagree with a textbook
  reference or `fitdistrplus::fitdist(., "fatigue")`.
- The location-shift fatigue variant (`2026-05-08-variable-parameter-selection`)
  comes back from the roadmap — that work touches the density anyway,
  natural place to also rectify α.
- The package starts publishing standard-error / asymptotic-CI
  output that consumes `FatigueCost::hessian`; users would then see
  visibly-wrong SEs.

## Verification when fixed

- `fatigue_mle` recovers α within FP-noise of the actual BS MLE on
  the golden (or the golden is intentionally regenerated from the
  fixed code).
- `FatigueCost` is consistent with `fatigue_mle`'s reported logLik
  formula (i.e. they evaluate the same density at the same point).
- `python -m quality.harness validate fatigue` passes at 1e-12
  with `claimed_level: validated`.
- `quality/golden/fatigue.json` either:
  - is unchanged (option 2: the package's "fatigue" remains a
    non-MLE estimator on purpose), or
  - is regenerated with documented logLik-shifts (option 1 / 3).

## Files involved

- `cppunivariate/include/cppunivariate/distributions/fatigue/mle.hpp`
  — both `FatigueCost` and `fatigue_mle`.
- `quality/golden/fatigue.json` — sampler args / recovered estimates
  / logLik baseline.
- `roadmap/2026-05-08-variable-parameter-selection.md` — natural
  place to bundle the fix when the 3-parameter fatigue lands.
