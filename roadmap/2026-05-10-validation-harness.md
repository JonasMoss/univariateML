# AI-first validation harness — state and retrospective (2026-05-10)

This document is the index + retrospective for the
sympy-derivative validation harness that landed across commits
`d448e11` … `dff34d2`. It captures **what the harness is**, **what
it has validated**, **what it has surfaced**, **what's deferred**,
and **the lessons** from building it.

The focused per-topic deferral entries are linked from each section.

## TL;DR

35 of 42 distributions are now machine-checked at 1e-12 against an
independently-derived sympy ground truth. The harness already paid
for itself by surfacing a real algorithmic bug in `fatigue_mle`
(α-formula off by 2 from the standard BS MLE). Adding a new
distribution to the harness now takes ~30 minutes for the
well-behaved cases (one ~50-line `quality/math/<id>.py` + one
dispatch line in `cppunivariate/tests/test_validation.cpp`).

---

## The goal

Goldens are great regression nets — they tell you "the output didn't
change" — but they cannot tell you "the output is correct." A wrong
implementation reproduces its own goldens forever. So adding a new
distribution to univariateML traditionally involved a math-derivation
step (writing `Cost::gradient` and `Cost::hessian` by hand or with
sympy assistance) that was *human-only*: there was no machine-readable
statement of what the distribution *is* against which to verify.

The AI-first vision is the inverse:

1. The user writes the density once, in sympy, in a file the harness
   can read (`quality/math/<id>.py`).
2. The harness symbolically differentiates that density to derive
   gradient and Hessian.
3. The C++ `Cost` analytical implementation is checked against the
   symbolic ground truth at high precision (mpmath dps=50) on a
   Sobol theta sweep around the recovered MLE.
4. Goldens drop to a regression-only role.

The math becomes the source of truth; the C++ implementation has to
match it, and any drift between the two is detected automatically.
This is what makes the *recipe* AI-actionable: an agent given a new
distribution can author the spec, run `just derive <id> && just
validate <id>`, and read a machine-checkable verdict.

---

## Architecture

End-to-end flow for `<id>`:

```
quality/math/<id>.py                                        ← author writes
    │
    │  python -m quality.harness derive <id>
    ▼
quality/derived/<id>/eval_request.json     (data + θ sweep)
quality/derived/<id>/symbolic_grid.json    (mpmath-50 truth)
    │
    │  just cpp-test  (validation_tests reads request,
    │                  evaluates the C++ Cost, writes cpp_eval)
    ▼
quality/derived/<id>/cpp_eval.json
    │
    │  python -m quality.harness validate <id>
    ▼
quality/derived/<id>/{gradient,hessian}_report.json
quality/derived/<id>/quality_report.md  (claimed_level: validated)
```

### Files

- `quality/harness/spec.py` — `Spec` and `Param` dataclasses.
  Validation rule: every symbol in `log_density` must be either
  `x_symbol` or in `params`. The 16-hex `spec_hash` is the SHA-256
  of `srepr(log_density)` and serves as the
  [implementation-provenance ID](2026-05-08-implementation-provenance.md).
- `quality/harness/derive.py` — sympy diffs, mpmath lambdification,
  per-obs evaluation summed over the dataset.
- `quality/harness/grids.py` — Sobol low-discrepancy θ sweep
  centred on the MLE pulled from `quality/golden/<id>.json`.
  Per-Param domain logic decides additive vs multiplicative; the
  `Param.boundary` flag handles non-regular MLEs.
- `quality/harness/validate.py` — diffs `symbolic_grid.json` and
  `cpp_eval.json`, emits per-check JSON reports + the front-matter
  `claimed_level: validated|experimental` in `quality_report.md`.
- `cppunivariate/tests/test_validation.cpp` — the C++ side of the
  round-trip. One `TEST_CASE` per id, dispatched via either the
  generic `eval_with<Cost>` / `eval_int<Cost>` / `eval_fd_grad<Cost>`
  templates or a per-id lambda for ctor-arg cases.
- `cppunivariate/tests/eval_io.hpp` — hand-rolled JSON I/O for the
  request/response files (handles 2D and 3D nested arrays without
  pulling in nlohmann/json).

### Per-distribution surface area

The thing the user writes per distribution is `quality/math/<id>.py`:
~30–80 lines of sympy. Plus one line in `test_validation.cpp`'s
dispatch (`run_validation("<id>", eval_with<...>)`). Everything else
is generic.

The Spec carries optional flags for distributions that don't fit the
default pattern:

- `cost_has_gradient: bool = True` — set False for BFGS-with-FD-grad
  distros (std-family); harness skips the 1e-12 grad-vs-symbolic
  check.
- `cost_hessian_is_fisher: bool = False` — set True when
  `Cost::hessian` returns Fisher info × n instead of the observed
  Hessian (laplace).
- `golden_to_cost: Optional[Callable]` — natural-space → Cost-space
  mapping when the spec is written in a phi-reparameterised space
  (std, ged, paralogis).
- `Param.boundary: Optional["upper"|"lower"]` — non-regular MLEs
  (unif, dunif, pareto, power); the harness sweeps inside the smooth
  interior rather than centring on the boundary.

---

## What's validated (35 of 42)

| Bucket | Count | Distributions |
|---|---|---|
| 1-D closed-form, full-analytical Cost | 4 | exp, rayleigh, pois, geom |
| 2-D closed-form, full-analytical Cost | 11 | norm, weibull, gumbel, logis, lnorm, cauchy, llogis, kumar, gompertz, invgauss, logitnorm |
| Boost.Math seam (lgamma / digamma / trigamma) | 7 | gamma, beta, betapr, naka, invgamma, burr, invburr |
| 2-D analytical without Boost | 1 | invweibull |
| Phi-space + FD-grad (BFGS family) | 3 | std, paralogis, ged |
| Discrete (ispan) | 5 | pois, geom, nbinom, lgser, dunif |
| Boundary-MLE (non-regular) | 4 | unif, dunif, pareto, power |
| Cost-with-fixed-ctor-arg | 2 | binom (size=9), zipf (N=3) |
| Fisher-info-as-Hessian | 1 | laplace |

(Distributions appear in multiple rows where they exercise multiple
patterns — e.g. `pois` is both 1-D-closed-form and discrete; `dunif`
is both discrete and boundary-MLE.)

All 35 reach `claimed_level: validated` at 1e-12 in their
`quality/derived/<id>/quality_report.md`.

---

## What's deferred (4 + 2)

### 1. Fernandez-Steel skew family — `snorm`, `sstd`, `sged`

Sympy explodes on the `xi^sign(z)` chain rule when differentiating
the F-S skew construction twice. After 2 minutes of evaluation the
mpmath grid evaluation didn't finish. **Documented in
[2026-05-10-fs-skew-family.md](2026-05-10-fs-skew-family.md)** with
three approaches to try when revisited (Piecewise spec /
two-phase-sign-baking / hand-derived Hessian).

### 2. `fatigue` — open algorithmic bug surfaced by the harness

The harness flagged that the recovered `(α, β)` from `fatigue_mle`
is **not** the standard Birnbaum-Saunders MLE. The α-formula uses
`α² = x̄/β + β/H(x)` but the BS MLE FOC is `α² = x̄/β + β/H(x) − 2`.
On the 500-sample golden, recovered α=3.189 vs true MLE α=2.858;
nll differs by 5.6 units. Bias is *fixed in the limit*, not a
finite-sample effect. **Documented in
[2026-05-10-fatigue-cost-mismatch.md](2026-05-10-fatigue-cost-mismatch.md)**
with three fix options (correct fatigue_mle / leave alone /
ramp-via-flag) and the release-management consequences.

### 3. `zip` — piecewise PMF, not yet attempted

Zero-inflated Poisson has a piecewise density (x = 0 vs x > 0 use
different formulas). Sympy can express this with `Piecewise`, but
the second derivatives of `Piecewise` plus the chain rule may
produce DiracDelta-on-the-boundary-condition expressions similar
to the F-S family case. **Will be attempted in the next session;
this document will be updated with the result.**

### 4. `lgamma` — wrapper-only, intentionally out of harness scope

`lgamma_mle` doesn't have its own `Cost` struct; it wraps
`gamma_mle` on log-transformed data via the R-side wrapper (per
CLAUDE.md). There's nothing C++-side for the harness to validate.

---

## Problems encountered, with resolutions

### A. F-S skew chain-rule explosion

**Problem.** Differentiating `xi^sign(z)` (where `z = z(μ, σ, ξ)`)
twice causes sympy expression-tree blowup. Lambdifying through
mpmath produces functions whose evaluation per grid point exceeds 2
minutes.

**Resolution.** Deferred — see roadmap entry. The default v1 pattern
("symbolic diff → lambdify → mpmath evaluate") doesn't scale to F-S
skew. Three alternative approaches captured for when this is picked
up.

### B. fatigue bug — surfaced, not fixed

**Problem.** The harness flagged that `FatigueCost::operator()`
disagreed with `fatigue_mle`'s logLik output by an O(1) term. On
investigation, the deeper issue: `fatigue_mle`'s α-formula is
non-MLE for the standard BS density.

**Resolution.** Cost patch attempted and reverted — fixing
`FatigueCost` doesn't fix the algorithm, just shifts the
inconsistency. The proper fix is in `fatigue_mle` itself, but
changes recovered α for every existing user. Documented and
deferred for user release-management decision.

### C. Discrete distributions need integer span

**Problem.** Costs for discrete distros (pois, geom, nbinom, lgser,
dunif, binom, zipf, zip) take `cppunivariate::ispan` (an int span),
not the regular `span` (double span). The harness's `data` is double.

**Resolution.** Added `eval_int<Cost>` template that round-trips
double `data` through a transient `std::vector<int>` whose lifetime
spans the cost evaluation. ~10 LOC; works for all
single-ctor-arg discrete Costs.

### D. Boundary MLEs are non-regular

**Problem.** Distros where the MLE sits AT a feasibility boundary
(unif's `a = min(x)`, pareto's `b = min(x)`, etc.) have undefined
gradient/Hessian formulas at the MLE itself. The harness's default
"first theta = MLE" + score-residual gate both break.

**Resolution.** Added `Param.boundary: Optional["upper"|"lower"]`.
- `_shift_inside` in grids.py replaces the boundary-param coord of
  the first theta with `mle * 0.99` (upper) or `mle * 1.01` (lower).
- The Sobol sweep on a boundary param uses log-uniform on
  `[mle*0.5, mle*0.99]` ("upper") or `[mle*1.01, mle*2.0]` ("lower").
- `test_validation.cpp::run_validation` takes a `skip_score_check`
  flag (the boundary-shifted point isn't where `‖g‖₂ ≈ 0`).
- Validates 4 boundary distros at machine epsilon (~1e-16).

### E. DiracDelta in symbolic Hessians of |·|^ν densities

**Problem.** GED's density has `|x − μ|^ν`; sympy's second derivative
w.r.t. μ contains `DiracDelta(x − μ)`. Lambdifying through mpmath
fails: `NameError: name 'DiracDelta' is not defined`.

**Resolution.** `derive.py::_lambdify` substitutes
`DiracDelta(...) → 0` before passing to `sp.lambdify`. Mathematically
correct for our use case (the Sobol grid never hits exact `x = μ`)
and fixes GED cleanly. Same patch handles laplace's `|x − μ|`
gradient (where `sign(x − μ)` is well-defined off the boundary).

### F. Fisher info vs observed Hessian (laplace)

**Problem.** `LaplaceCost::hessian` returns the Fisher information
matrix, not the observed Hessian. The observed Hessian for laplace
has Dirac deltas at the kinks of `|x − μ|` and is degenerate. The
harness's default `sp.diff(nll, μ, μ)` would be inconsistent.

**Resolution.** New `Spec.cost_hessian_is_fisher: bool = False` flag.
When True (laplace), the harness compares against the per-obs Fisher
info (already declared in `Spec.fisher_info_per_obs`) summed over
data — which is exactly what the C++ side computes.

### G. Score-zero relative-diff scaling

**Problem.** At the MLE, the C++ `Cost::gradient` returns *exact*
zero (because Newton FOCs give explicit closed forms). The symbolic
side returns ~1e-12 due to accumulated lgamma rounding over
~500-observation sums. Per-entry `rel = abs_diff / max(|t|, |o|, 1.0)`
makes this 1e-12 abs diff look like a 1e-12 *relative* miss, even
though the surrounding sweep values are O(1e3+).

**Resolution.** `validate.py::_summarize_against_sweep_max` scales
by the sweep-wide max-absolute-value of the quantity. Algebra bugs
perturb every entry; rounding only the score-zero one. The new
measure catches the former and forgives the latter. `sweep_max`
is recorded in the report JSONs for audit.

### H. JSON-precision seed collisions (sim recovery, since reverted)

**Problem.** During the (now-reverted) Phase-3b sim-recovery work,
mt19937_64 seeds were routed through JSON as doubles, which
exactly represent integers only up to 2^53 (~9e15). The seed-mixing
formula produced values past that range, where neighbours rounded
to identical seeds — so adjacent reps duplicated their fits. This
caused boundary-collapse fits to be triple-counted, inflating bias
estimates.

**Resolution.** Sequential counter offset by base seed (every (N,
rep) gets a globally unique seed without arithmetic mixing). The
fix is preserved in the [sim-recovery roadmap entry](2026-05-10-sim-recovery.md)
at `acb5e43` so whoever resurrects it doesn't redo the debug.

### I. (0, 1) prefix-match bug in grids

**Problem.** `_sweep_halfwidth` checked `"(0," in p.domain` first,
which matches `"(0, 1)"` (prefix). Bounded-interval params got the
multiplicative sweep that shoots past 1. lgser's
`log(-log(1 − θ))` then went complex.

**Resolution.** Reorder: `"(0, 1)"` checked first, before any `"(0,…)"`
prefix match. Fixed in both `_sweep_halfwidth` and the inner sweep
loop.

### J. Cost-with-extra-ctor-args (binom, zipf)

**Problem.** `BinomCost{ispan x, double size}` and
`ZipfCost{ispan x, double N}` need an extra constant at construction
time that isn't in `data` or `theta`. The default `eval_with<Cost>`
template doesn't know how to construct these.

**Resolution.** Per-id dispatch lambdas in `test_validation.cpp`
that construct the Cost with the hardcoded constant. The constant
is also hardcoded in the corresponding `quality/math/<id>.py` (lockstep
documented in both files). If the golden is regenerated with a
different size/N, both must be updated. Acceptable because we
control the goldens; if this becomes a sweep concern (many distros
with ctor args), promote to a `Spec.cost_extra_args` field with
generic plumbing.

### K. Constant terms in Cost vs spec (pois, lgser)

**Problem.** Several Cost::operator() functions drop "data-only"
constants (e.g. pois drops `lgamma(x+1)`, lgser drops `log(x)`,
binom drops `log C(size, x)`) per a "+ const" convention. If the
spec includes them, the absolute nll values disagree.

**Resolution.** Match the C++ convention in the spec. The constants
cancel in derivatives, so gradient and Hessian are unaffected.
Documented in each spec's docstring.

---

## What the harness has caught

1. **`fatigue` α-formula is non-MLE** — fatigue_mle returns the wrong
   α. Bias O(2) in expectation, not finite-sample noise. Has been in
   the package for years; no existing test caught it because the
   goldens are self-consistent against the buggy fitter and there's
   no score-residual gate on 1D-Newton-fitted distros.
2. **`FatigueCost` formula inconsistent with `fatigue_mle`** —
   discovered en route to the deeper bug. The Cost computes
   `0.5·log(x²+β²)` where the standard BS density has `log(x+β)`.
   Different formula even before the α issue.
3. **`grids.py` (0, 1) prefix bug** — would have produced silently
   wrong sweeps for any future distribution adding a probability
   parameter.

(2) and (3) are pre-existing latent bugs; (1) is a real algorithmic
bug. None would have been caught by the existing testthat or ctest
suite alone.

---

## Open questions / extensions

### Short-term (next session)

- **Try `zip`.** Piecewise pmf via `sp.Piecewise`. May explode like
  the F-S family, may work cleanly. This document will be updated
  with the result.

### Medium-term

- **F-S skew family** — try the Piecewise-spec approach
  ([2026-05-10-fs-skew-family.md](2026-05-10-fs-skew-family.md))
  to handle `snorm`, `sstd`, `sged`.
- **`fatigue` fix** — release-management decision required. See
  [2026-05-10-fatigue-cost-mismatch.md](2026-05-10-fatigue-cost-mismatch.md).

### Long-term roadmap items the harness already substantially addresses

- [validation-and-goldens](2026-05-08-validation-and-goldens.md) —
  goldens demoted to drift-detection layer; sympy-derivative
  round-trip is the substantive correctness signal.
- [quality-ladder](2026-05-08-quality-ladder.md) — `quality_report.md`
  carries `claimed_level`, computed mechanically from passing
  checks. v1 emits `validated`/`experimental`; future
  `audited`/`stable` layer on as more checks land.
- [implementation-provenance](2026-05-08-implementation-provenance.md) —
  `Spec.spec_hash()` is the provenance ID; supporting record fields
  map to existing `Spec` / git / `derived/` infrastructure.

### Long-term roadmap items the harness *enables* but doesn't yet do

- [dpqr-validation](2026-05-10-dpqr-validation.md) — extend the harness
  to verify `d`/`p`/`q`/`r` self-consistency (∫f=1, q∘p=id) and
  reference cross-check.
- [sim-recovery](2026-05-10-sim-recovery.md) — testing the *fitting
  machinery* separately from math correctness. Code preserved at
  `acb5e43`; revert at `7d2569d`.

---

## Recipe summary

CLAUDE.md "Adding a new distribution" has the 10-step flow.
Steps 8–10 are the harness layer (added 2026-05-10):

```
8.  Author quality/math/<id>.py exporting SPEC = Spec(...).
9.  Register the id in cppunivariate/tests/test_validation.cpp's
    dispatch — eval_with<Cost> for analytical full Cost,
    eval_fd_grad<Cost> for FD-grad-only, eval_int<Cost> for
    discrete, or a per-id lambda for ctor-arg cases.
10. just derive <id> && just validate <id> — quality_report.md
    should claim 'validated'. Commit the derived artifacts.
```

Templates by shape (in increasing complexity):

| Shape | Template spec | Time to add |
|---|---|---|
| 1-D / 2-D continuous, full analytical Cost | `quality/math/exp.py` (1-D) or `quality/math/norm.py` (2-D) | ~30 min |
| Boost.Math (lgamma/digamma) | `quality/math/gamma.py` | ~30 min |
| Phi-space + FD-grad | `quality/math/std.py` | ~1 h (read mle.hpp comment for reparam) |
| Phi-space natural reparam, no skew | `quality/math/paralogis.py` | ~30 min |
| Discrete | `quality/math/pois.py` | ~30 min |
| Boundary-MLE | `quality/math/pareto.py` | ~30 min |
| Cost ctor with extra args | `quality/math/binom.py` | ~30 min (+ lockstep dispatch) |
| Fisher-info-as-Hessian | `quality/math/laplace.py` | ~45 min |

The 2 remaining-doable distros (`zip`) need experimentation; the
3 deferred (F-S skew) need infrastructure work first.
