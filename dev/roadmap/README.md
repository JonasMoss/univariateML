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

- [2026-11-08 — Quantile-defined families](2026-11-08-quantile-defined-families.md)
  *(banked; not highest priority)*: g-and-k first, then g-and-h and the FKML
  generalized lambda; shared inverse and profile machinery from TASK-151, the
  GLD log-concavity proof, GPD face and spacings notes.

- [2026-11-08 — Pre-generated tlambda inverse table](2026-11-08-tlambda-pregenerated-inverse-table.md)
  *(banked option)*: replace the first-use seed table with generated constants
  if startup cost matters.

- [2026-11-05 — Censoring and truncation](2026-11-05-censoring-truncation.md)
  *(idea; TASK-142)*: likelihood pieces, Burr worked example, the extra
  Hellinger boundary $\overline{T(M)} \setminus T(\overline{M})$ created by an
  observation map, and families made redundant by truncation.

- [2026-11-01 — Estimation beyond ML](2026-11-01-estimation-beyond-ml.md)
  *(banked; preliminary exploration)*: closure-compatible coordinates,
  model-restricted Hellinger/KL Bayes actions, posterior propriety gates,
  maximum product of spacings and competing CDF/quantile estimators.

- [2026-10-31 — Useful density proposals](2026-10-31-useful-density-proposals.md)
  *(banked)*: five application-oriented candidates, parameter domains, priorities,
  references and preliminary ML/dpqr difficulty assessment.

- [Decision log](decisions.md): dated project decisions with evidence; read
  this first for what was chosen and why.

- [2026-10-30 — Python package for teaching and simulation](2026-10-30-python-teaching-and-simulation.md)
  *(banked; after cleanup and the standalone C++ foundation)*: distribution and
  fit objects, NumPy arrays and RNGs, simulation outcomes, student installation,
  staged family coverage and API decisions to refine.

- [2026-10-30 — Inference regularity and derivative support](2026-10-30-inference-and-dqm.md)
  *(initial audit; implementation deferred)*: 98-family derivative inventory,
  conditional DQM and information classification, recovered Hessian history,
  sandwich requirements and procedures for subsets of hypotheses.

- [2026-10-30 — Package structure and mathematical documentation](2026-10-30-package-and-documentation.md)
  *(banked; after current density and MLE work)*: light landing page,
  R-independent C++ library, generated R package, custom distribution website,
  and preservation of algorithm choices, numerical rationale and validation history.

- [2026-10-29 — Parameter rules](2026-10-29-parameter-rules.md) *(implemented P0–P5, 2026-10-30; 15 holds open)*:
  R1–R8 for coefficients, holds, defaults, thresholds and restrictions;
  parameter kinds; schema, API and C++ changes; phases P0–P5.

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
- [2026-05-08 — Documentation from metadata](2026-05-08-documentation-from-metadata.md) *(folded into the 2026-10-30 package plan)*:
  generate docs from the same metadata and dossiers used by code and
  tests.
- [2026-05-08 — Monorepo layout](2026-05-08-monorepo-layout.md)
  *(mostly landed)*: C++-primary layout in place; `python/package/`,
  `dev/validation/quality/{simulations,references}/`, and `docs/` remain placeholders
  awaiting their respective roadmap entries.
- [2026-05-08 — Benchmarks](2026-05-08-benchmarks.md):
  benchmark runtime and robustness after correctness mechanisms mature.
- [2026-05-08 — Variable parameter selection](2026-05-08-variable-parameter-selection.md) *(landed via parameter-rule holds)*:
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
- [2026-05-10 — dpqr round-trip validation](2026-05-10-dpqr-validation.md) *(substantially addressed 2026-10-30 by truth tables)*:
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
  ML-only with external d/p/q/r vs one package exposing the C++ d/p/q/r;
  *(2026-10-29: own internal d/p/q/r for every non-stats family, stats kept
  for now but not banked; 2026-10-30: no densities exported)*.
- [2026-10-28 — New distributions and reference-package imports](2026-10-28-new-distributions.md):
  evaluation of issues #52/#7 (ZINB and constrained GPD first) and of
  sads (imports VGAM; buggy `pzipf`/`qzipf`).
- [2026-10-29 — Larger reference repairs](2026-10-29-reference-repairs.md) *(superseded 2026-10-30: repairs deleted with extraDistr/actuar)*:
  held-back runtime repairs for actuar trgamma/invtrgamma and extraDistr
  tnorm/gev d/p/q (patches in `patches/`); needs a policy decision.
- [2026-10-29 — Classical ML-failure families and a failure gallery](2026-10-29-classical-failure-families.md):
  skew-normal landed (mlsn); threshold families and skew-t deferred; taxonomy of failures.
- [2026-10-30 — Frozen distributions through the fitters](2026-10-30-frozen-distributions.md)
  *(landed 2026-10-30)*: `mlX()` and `mlX(coef = ...)` without
  data build a fixed model at README defaults; replaces `simulate_default`
  and `:::` in examples; no densities exported.
