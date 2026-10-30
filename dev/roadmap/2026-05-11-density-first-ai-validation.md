# Density-first AI validation

**Status:** deferred design target. The v1 harness is useful and has
already found real bugs, but it is still too per-distribution and too
close to the C++ implementation shape. This entry captures the next
AI-first direction: a human should ideally verify only the density,
support, parameter domains, and unavoidable irregularity notes; the
rest should be derived or stress-tested mechanically.

## The idea

Move the quality harness from "one symbolic implementation per
distribution" toward "one canonical distribution definition per
distribution".

The canonical input should be as small and inspectable as possible:

- distribution id and name
- support
- natural parameters and domains
- log density or log pmf
- optional CDF / quantile, when simple
- optional parameter transforms used by optimizers
- explicit irregularity notes, when mathematically unavoidable

Everything else should be generated, derived, or checked by generic
machinery. In the best case, adding a distribution means checking a
single density definition and a few declarative metadata fields, not
writing a distribution-specific validation program.

## Why it matters

The repository is meant to be AI-first. That means correctness work
should be easy for an agent to run, hard for an agent to accidentally
fake, and minimally dependent on human review of hand-derived formulae.

The current `dev/validation/quality/math/<id>.py` specs are a strong start because
the harness derives gradients and Hessians from `log_density` and
compares C++ against that derived truth. But the specs still contain
too much individual glue:

- some specs are written directly in C++ cost parameter space
  (`log_sigma`, `log_nu_shift`, etc.) rather than natural
  distribution parameter space;
- some specs carry custom `golden_to_cost` adapters;
- boundary cases require hand flags such as `Param.boundary`;
- Laplace manually declares Fisher information because the observed
  Hessian is singular at the kinks;
- fixed-argument distributions such as binomial and Zipf hard-code
  constants in both the spec and the C++ validation dispatch.

Those exceptions are reasonable for v1. The next step is to make them
declarative enough that they are auditable metadata, not bespoke
validation code.

## Target validation layers

These layers are allowed to be computationally heavy. Fast PR checks
can run a smaller subset; nightly or local audit checks can run the
expensive versions.

| Layer | Goal | Human input |
|---|---|---|
| Density normalization | Numerically integrate or sum `exp(log_density)` over support for many parameter points. | Density, support, domains |
| Symbolic derivatives | Derive score and Hessian from the canonical density. | Density only |
| Transform chain rule | If C++ optimizes in transformed parameters, derive transformed gradients/Hessians mechanically from declared transforms. | Transform declaration |
| C++ cost equivalence | Compare C++ NLL / gradient / Hessian against the symbolic derived version. | None beyond the canonical spec |
| Slow MLE oracle | Fit the symbolic density with a generic high-precision optimizer and compare C++ MLE estimates/logLik. | None beyond the canonical spec |
| Simulation recovery | Sample known parameters, fit repeatedly, and check recovery / bias / failure rates. | Sampler or inverse-CDF route |
| dpqr self-consistency | Check `p(q(u))`, `q(p(x))`, density-CDF agreement, and sampling distributional tests. | CDF/quantile if available; otherwise numerical inversion |
| External references | Compare against trusted R/Python reference packages where available. | Reference mapping |
| Mutation tripwires | Intentionally perturb formulas or replay known historical bugs and ensure the harness fails. | Known-bug fixtures |

## Desired shape

The long-term API should make the distinction between mathematical
truth and implementation adapters explicit:

```text
distribution:
  id: gamma
  support: x > 0
  params:
    shape: (0, inf)
    rate: (0, inf)
  log_density: shape*log(rate) - loggamma(shape)
               + (shape - 1)*log(x) - rate*x
  optimizer_params:
    shape: identity
    rate: identity
```

For transformed estimators, the density should still be stated in
natural parameters, with a separate transform block:

```text
optimizer_params:
  mu: identity(mean)
  log_sigma: log(sd)
  log_nu_shift: log(nu - 2)
```

The harness should then apply the chain rule itself. A spec written
directly in C++ cost space should become the exception, not the normal
pattern.

## Relationship to current harness

The existing harness should be kept. It already proves an important
property: C++ `Cost` math matches an independently evaluated symbolic
spec on a Sobol grid. This entry does not replace that work; it
raises the bar for what counts as "AI-first" around it.

The main semantic adjustment is that `validated` should eventually be
reserved for a stronger claim than "C++ cost math matches
`dev/validation/quality/math/<id>.py`." A future ladder could distinguish:

- `cost-validated`: C++ NLL / gradient / Hessian match symbolic
  derivations;
- `density-validated`: density normalizes and metadata domains/support
  are mechanically checked;
- `fit-validated`: C++ MLE matches an independent symbolic optimizer
  and passes simulation recovery;
- `audited`: dpqr, references, edge cases, and known-bug tripwires pass.

## Trigger to revisit

Take this off the shelf when any of these happen:

- a new distribution is added and the current per-distribution harness
  work feels repetitive;
- a transformed-parameter family is touched and the natural-vs-cost
  parameter split becomes confusing;
- `validated` starts being used as a user-facing confidence claim;
- CI/runtime budget is available for heavier audit checks.

## Verification when fully closed

- A new distribution can be added to the quality system by writing one
  canonical density spec plus declarative metadata, without editing
  C++ validation dispatch code.
- Natural-parameter specs and optimizer-parameter transforms are
  separated; transformed gradients/Hessians are derived by the harness.
- Every distribution has a density-normalization report.
- Every C++ estimator has an independent slow-oracle MLE comparison,
  not only a golden regression comparison.
- The quality ladder distinguishes cost-level validation from
  full distribution / fitting validation.
- At least one historical bug, such as the fatigue alpha-formula
  mismatch, is preserved as a mutation or fixture test that fails under
  the bad implementation and passes under the fixed one.
