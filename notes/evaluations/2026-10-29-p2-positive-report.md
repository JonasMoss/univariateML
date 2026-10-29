# TASK-81: p2-positive

Implemented the positive-family holds in the current parametrizations. No
commits, board changes, shared-source edits, installations, or sub-agents.

## Files changed

Within each of exp, gamma, weibull, lnorm, invgamma, invgauss, invweibull,
invexp, llogis, rayleigh, naka, lomax, gompertz, invpareto, pareto and power:
`README.md`, `mle.hpp`, `adapter.cpp`, `test.R`, `test.cpp`.
Within lgamma: the same files except `mle.hpp` (it uses gamma directly).
Also this report. sgomp already implements every hold; its sources stay unchanged.

## Holds

- exp rate, invexp rate, rayleigh sigma: evaluate the fixed likelihood.
- gamma shape: rate = shape / mean(x); gamma rate: unique digamma root.
- invgamma alpha/beta and lgamma shapelog/ratelog: the corresponding gamma
  holds on reciprocal/log data, with Jacobians.
- weibull shape: exact power-mean scale; weibull scale: monotone shape score.
  invweibull shape/rate: reciprocal Weibull with its Jacobian.
- lnorm meanlog/sdlog: exact normal profiles on log data.
- invgauss mean/shape: exact quadratic profile; finite parameters only (R7).
- llogis shape: monotone logistic-location score on log data; rate: monotone
  shape score.
- naka shape: scale = mean(x^2); scale: unique log(shape)-digamma(shape) root
  against mean(z-1-log(z)), z=x^2/scale. This differs from a gamma-rate hold.
- lomax lambda: exact kappa profile; kappa: monotone lambda score.
  invpareto shape/scale: reciprocal Lomax with its Jacobian.
- gompertz b: exact a profile; a: monotone b score, detecting the exponential
  face at sum(x) <= a*sum(x^2)/2. With a held and all-zero data the likelihood
  is flat in b; choose b=1. With b held and all-zero data, a diverges: stop.
- pareto a: b=min(x), including ties/constants; b: exact shape profile,
  rejecting b>min(x) and collapse when every x=b.
- power alpha: exact beta profile, requiring alpha>max(x). Both coefficients
  may be held when that support condition holds.
- Every successful subset returns held values exactly and records `fixed`.
  The 13 newly holdable two-parameter families support all three nonempty
  subsets; power supports alpha and alpha+beta; the three scalar families
  support their sole hold: 44 successful subset types.

Converted power's beta-only exception to `no-mle`: with alpha free the
reference excludes the maximizing upper endpoint. Its default endpoint
approximation is preserved for P3, as requested. No other `not-implemented`
entries remain in this group. P4 rate-to-scale changes remain out of scope.

Data-dependent collapse, zero-likelihood and escape conditions are documented
in README prose and `limits`, with held-set scope where applicable.
Held-path scalar solvers obey iteration caps and stop instead of returning
an unresolved root; convergence failure is distinguished from collapse.
Regressions cover tiny held Weibull shape, distant held lognormal mean and
intermediate overflow in the gamma held-shape rate profile. Existing all-free algorithms and golden files are preserved.

## Evidence

The formulas and monotonicity/existence arguments are proved in the P0 positive
note. New scalar searches bisect those unique roots; they do not rely on a
multidimensional optimizer. The new held cases therefore require no conjectured
multistart classification. The existing sgomp joint search remains numerical,
without a certified enumeration of every stationary point.

Added 33 C++ cases and 51 R test blocks.
Per-family R tests compare every new successful subset to five-start BFGS on
reference log densities, using n=20/100, ties, small n, singleton and constant
samples. Additional samples cover exponential, inverse-exponential, Levy and
half-normal limit laws. The Pareto endpoint is included analytically. For naka,
a separately written log-density formula is used; Gompertz uses expm1 to avoid
cancellation in the external reference. Optimization agreement is numerical
evidence, not a global certificate. Boundary/error and iteration-limit tests
exercise the new rejection paths. C++ tests check each new golden hold subset,
reference-density likelihoods, free-coordinate scores, and core boundary cases.

## Final validation

- Simbox `just cpp-test`: 6 CTest targets; 5 passed, distribution_tests has
  only the two permitted machine failures. Distribution totals: 449 cases
  (447 passed), 513,955 assertions (513,953 passed). Failures are precisely
  betapr/test.cpp:33 and std/test.cpp:40.
- Simbox `just quality exp gamma weibull lnorm invgamma invgauss invweibull
  invexp llogis rayleigh naka lomax gompertz sgomp lgamma invpareto pareto
  power`: 18/18 passed.
- Final simbox `just test`: 12,057 passed assertions, 4 failures, no warnings
  or skips (406.9 seconds of tests). The three permitted R failures are frozen
  sstd qsstd and the bnbinom/std goldens. The fourth is the incompatible shared
  assertion below. All assigned-family tests, assigned goldens and generic
  hold-conformance checks pass; the full green gate is blocked by that assertion.
- Local `just gen` passed. No golden file was edited.

## Shared integration change needed

`univariateML/tests/testthat/test_argument_names.R:7` expects
`mlweibull(x, shape=2)` to error because the hold was unsupported. That is
incompatible with TASK-81. The hold must succeed; the adjacent `shap=2`
unknown-argument test must still error. Replace that old expectation with:

```r
f <- mlweibull(x, shape = 2)
expect_identical(f[["shape"]], 2)
expect_identical(attr(f, "fixed"), "shape")
```

The file is shared and explicitly outside this job's edit scope, so it is
unchanged. Full R green apart from the five listed machine failures requires
this integration correction. No family implementation should reproduce the
obsolete unsupported-hold behavior merely to satisfy that assertion.

## Open questions

- P3: power endpoint closure, including the free-alpha/beta-held fit.
- P4: requested rate-to-scale renaming/reparametrization.
- Existing sgomp global numerical-search certification is unchanged.
