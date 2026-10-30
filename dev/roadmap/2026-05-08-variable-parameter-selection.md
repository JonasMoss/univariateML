# Variable parameter selection (deferred)

**Status:** landed through the parameter rules (holds, P1-P2b, TASK-80/81/93/97); remaining hyper/nhyper holds are TASK-98. Earlier: in progress (2026-10-28). Decided: named arguments with the
coefficient names (`mlburr(x, scale = 1)`, `mlbinom(x, size = 10)`), `NULL`
estimates. README `params` keys `fixable: true` / `fixed: <value>`
(schema: `distributions/README_SCHEMA.md`); adapters return `fixed`, the
object's `fixed` attribute drives `logLik()` df, AIC/BIC, `model_select`'s
`p` and `bootstrapml`. Done: binom/nbinom `size`, fatigue `mu` (always
fixed: the three-parameter MLE does not exist), std `nu`, lst `df`,
burr/invburr/betapr/paralogis `scale` (`scale = 1` reproduces the 1.x
fits). Limits may carry `fixed: [name]`. Each fixable subset gets an exact
fitter; no generic optimiser fallback. Candidates later: pareto `b`,
power `alpha`, zipf `N`, gamma/weibull shape. The
rest of this file is the original (May 2026) note; its `metadata.hpp` seam
no longer exists.

## The idea

Let the user pick which canonical parameters of a distribution to
estimate and which to fix. The fitter takes a subset of `params` as
"free" and treats the rest as known / supplied / pinned at a given
value.

## Why it matters

A surprising number of distributions have multiple sensible
parameterisations whose parameter counts differ. The motivating
example is the fatigue (Birnbaum–Saunders) distribution: today we
ship the two-parameter version `(alpha, beta)`, but the literature
also has a three-parameter form with a location shift `mu`. Today,
adding the shift would mean a new distribution `fatigue3` (or
similar) parallel to `fatigue`. With variable parameter selection,
the same code path would handle "fit `(alpha, beta)` with `mu = 0`
fixed" and "fit `(alpha, beta, mu)`".

The same idea generalises to: profiling out a known scale, fitting a
location-only Cauchy on contaminated data, or holding `df` fixed in a
Student-t when prior information is available.

## Design when revisited

The metadata seam is already in place: `ParamDescriptor` in
`univariateML-cpp/include/cppunivariate/core/metadata.hpp` has a
`bool estimable` field (currently always `true` — see comment
"forward-looking; today always true (see roadmap)"), and every
per-distribution `metadata.hpp` populates it positionally as the
4th slot of each row, e.g. for Lomax:

```cpp
{"lambda", "rate",  1.0, true, {SupportType::Real, 0.0, kInf, false, false}},
{"kappa",  "shape", 2.0, true, {SupportType::Real, 0.0, kInf, false, false}},
```

`bindings/r/scripts/parse_cpp_metadata.R` parses it and the pilots
(`distributions/lomax/`, `distributions/exp/`) assert the values via
`static_assert` in `univariateML-cpp/tests/test_metadata.cpp`. Today no
downstream code consumes the field — every fitter estimates every
parameter regardless.

When this work resumes, the design questions are:

- **API surface**. The R-side wrapper `ml<id>(x, ...)` would gain a
  way to pin parameters, e.g. `ml<id>(x, kappa = 4)` or
  `ml<id>(x, fixed = list(kappa = 4))`. Choose between
  parameter-as-named-arg (more discoverable, conflicts with `tol`/
  `iterlim` etc.) vs. an explicit `fixed=` argument (no conflicts,
  one extra layer).
- **C++ surface**. `*_mle()` currently has a fixed signature per
  distribution. Either:
  - dispatch on the free-parameter set at runtime via a small struct
    (`struct LomaxFix { std::optional<double> lambda, kappa; }`) —
    the `Cost` struct profiles out the fixed parameters before
    optimising,
  - or generate one specialised `*_mle()` per estimable subset at
    compile time and pick at the wrapper layer.
- **Identifiability and existence**. Some fixed-parameter subsets
  have closed-form MLEs that don't currently exist. E.g. fatigue with
  `mu` fixed to a specific value reduces to a 2D problem; `mu` free
  is a 3D problem with non-trivial existence conditions on `mu`.
  Each distribution that wants this needs its dossier's "MLE existence"
  section extended.
- **Goldens**. Per-distribution goldens currently fix all parameters
  to defaults. Goldens for variable selection need a way to encode
  which subset was free; either expand the JSON schema or maintain a
  separate goldens-with-fixed-params directory.

## Trigger to revisit

Take this off the shelf when **any** of:

- A distribution like fatigue gets a request for the location-shift
  variant.
- A user actively asks for "fit X with Y fixed" — pinning is a
  common request in applied work.
- Profile likelihood / asymptotic-CI work starts and needs to fit
  with one parameter held at a hypothesised value.

## Verification when revisited

- The `estimable` field becomes a behaviour driver, not just a
  metadata seam. At least one pilot distribution exposes the new
  fitting path end-to-end (R wrapper → C++ estimator → golden).
- The dossier "MLE existence" section is extended for any
  distribution where fixing a parameter changes the existence story.
- Existing tests still pass — the default (`estimable: true` for all
  params) reproduces the current behaviour bit-for-bit.
- Cross-shell parity: if Python bindings exist by then, the same
  fixed-parameter API works there.
