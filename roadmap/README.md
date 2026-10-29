# Roadmap

Deferred design ideas for univariateML — things worth doing later but
not now. Each entry documents a specific change, why it isn't being
made today, and the trigger that should bring it back to the top of
the queue.

## Conventions

- One markdown file per idea: `YYYY-MM-DD-short-slug.md`. Date is the
  day the idea was recorded; entries sort chronologically.
- Each entry has a **Status** line, the substance of the idea, a
  **Trigger to revisit** section, and a **Verification** sketch for
  when work resumes.
- Entries are not in-flight work. In-flight work lives on a branch
  with tasks; a roadmap entry is the opposite — paused, captured, and
  out of head-state.
- The directory is `.Rbuildignore`d so it does not ship to CRAN.
- This is separate from `CLAUDE.md`: that file documents rules for the
  *current* codebase; this directory documents intentions for the
  future one.

## Index

- [2026-10-29 — Density scope decisions](2026-10-29-density-scope.md) (full coverage plan added):
  deliberate extraDistr and actuar exclusions, remaining study candidates,
  and unrestricted models requiring nonexistence errors.

- [2026-05-08 — C++ core as source of truth](2026-05-08-cpp-core-source-of-truth.md)
  *(closed 2026-05-10)*: now the architecture; see CLAUDE.md "Vision".
- [2026-05-08 — Cost-method fusion](2026-05-08-cost-method-fusion.md):
  fused `grad_hessian` on the Cost concept; deferred until a
  production callsite exists for `Cost::hessian`.
- [2026-05-08 — Distribution dossiers](2026-05-08-distribution-dossiers.md)
  *(closed 2026-05-09)*: one source-of-truth note per distribution,
  covering math, implementation, tests, docs, and references.
- [2026-05-08 — Implementation provenance](2026-05-08-implementation-provenance.md)
  *(substantially addressed 2026-05-10)*: Spec.spec_hash() is the
  provenance ID; supporting fields map to git + Spec + derived/.
- [2026-05-08 — Quality ladder](2026-05-08-quality-ladder.md)
  *(substantially addressed 2026-05-10)*: claimed_level mechanism in
  quality_report.md; v1 emits validated/experimental, additional levels
  (audited/stable) layer on as more checks land.
- [2026-05-08 — Validation and goldens](2026-05-08-validation-and-goldens.md)
  *(substantially addressed 2026-05-10)*: sympy-derivative round-trip
  is the new substantive layer; goldens demoted to drift-detection.
  dpqr + sim recovery split out to their own deferred entries.
- [2026-05-08 — Documentation from metadata](2026-05-08-documentation-from-metadata.md):
  generate docs from the same metadata and dossiers used by code and
  tests.
- [2026-05-08 — Monorepo layout](2026-05-08-monorepo-layout.md)
  *(mostly landed)*: C++-primary layout in place; `python/package/`,
  `quality/{simulations,references}/`, and `docs/` remain placeholders
  awaiting their respective roadmap entries.
- [2026-05-08 — Benchmarks](2026-05-08-benchmarks.md):
  benchmark runtime and robustness after correctness mechanisms mature.
- [2026-05-08 — Variable parameter selection](2026-05-08-variable-parameter-selection.md):
  let users fix some canonical parameters and estimate the rest;
  motivating example is fatigue with/without a location shift.
- [2026-05-08 — R package scope](2026-05-08-r-package-scope.md)
  *(closed 2026-05-09)*: trim the R package to ML only, let
  extraDistr / actuar provide d/p/q/r through Imports so we don't
  fight R's flat man-page space.
- [2026-05-10 — Simulation-recovery harness](2026-05-10-sim-recovery.md):
  bias / Cramér–Rao / KS verification of the *fitting machinery*,
  separate from the symbolic-derivative MLE-math harness. Code lives
  at acb5e43; reverted to keep v1 scope focused.
- [2026-05-10 — dpqr round-trip validation](2026-05-10-dpqr-validation.md):
  extend the harness to verify d/p/q/r self-consistency
  (∫f=1, q∘p=id) and reference cross-check, on top of the MLE-math
  layer.
- [2026-05-10 — FatigueCost vs fatigue_mle nll mismatch](2026-05-10-fatigue-cost-mismatch.md)
  *(closed 2026-05-10)*: fatigue_mle's α-formula was missing a −2
  vs the standard BS α-FOC. Fixed; FatigueCost rewritten to match;
  goldens regenerated; harness validates at 1e-15.
- [2026-05-10 — F-S skew family in the harness](2026-05-10-fs-skew-family.md)
  *(closed 2026-05-10)*: snorm/sstd/sged validate at 1e-12 once
  rewritten with explicit Piecewise (branches agree at z=0) and the
  `print_derivatives` simplify-hang is removed.
- [2026-05-10 — Validation harness state and retrospective](2026-05-10-validation-harness.md):
  index + lessons-learned for the AI-first sympy-derivative harness.
  41 of 42 distributions validated at 1e-12 (only lgamma is out of
  scope, as a wrapper around gamma_mle(log(x))). Catalog of validated
  shape-buckets + the problems hit and how each was resolved. Read
  this first to orient on the harness.
- [2026-05-11 — Density-first AI validation](2026-05-11-density-first-ai-validation.md):
  next-step design target for making the harness more AI-first: human
  checks the density/support/domains and the machinery derives or
  stress-tests the rest, including normalization, transform chain rule,
  slow MLE oracle, dpqr self-consistency, and mutation tripwires.
- [2026-10-28 — One folder per distribution](2026-10-28-per-distribution-layout.md)
  *(landed 2026-10-28; harness rework still open)*: everything for one
  distribution in its folder, one declarative `README.md` (YAML front
  matter + prose) replacing metadata.hpp / JSON sidecar / sympy spec,
  per-distribution tests and adapters, harness checks the real fitter.
- [2026-10-28 — R argument names vs CRAN](2026-10-28-cran-argument-names.md)
  *(decided, applied in the migration)*: reltol / abstol / gradtol /
  iterlim / `<param>0`, with the per-distribution audit table.
- [2026-10-28 — R package scope revisited](2026-10-28-r-package-scope-revisited.md):
  ML-only with external d/p/q/r (author's strong preference; argument maps
  where names differ) vs one package exposing the C++ d/p/q/r later;
  extraDistr's maintainer is unresponsive.
- [2026-10-28 — New distributions and reference-package imports](2026-10-28-new-distributions.md):
  evaluation of issues #52/#7 (ZINB and constrained GPD first) and of
  sads (imports VGAM; buggy `pzipf`/`qzipf`).
- [2026-10-29 — Larger reference repairs](2026-10-29-reference-repairs.md):
  held-back runtime repairs for actuar trgamma/invtrgamma and extraDistr
  tnorm/gev d/p/q (patches in `patches/`); needs a policy decision.
- [2026-10-29 — Classical ML-failure families and a failure gallery](2026-10-29-classical-failure-families.md):
  skew-normal (in progress), threshold families, skew-t; taxonomy of failures.
