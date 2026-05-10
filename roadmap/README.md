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

- [2026-05-08 — C++ core as source of truth](2026-05-08-cpp-core-source-of-truth.md)
  *(closed 2026-05-10)*: now the architecture; see CLAUDE.md "Vision".
- [2026-05-08 — Cost-method fusion](2026-05-08-cost-method-fusion.md):
  fused `grad_hessian` on the Cost concept; deferred until a
  production callsite exists for `Cost::hessian`.
- [2026-05-08 — Distribution dossiers](2026-05-08-distribution-dossiers.md)
  *(closed 2026-05-09)*: one source-of-truth note per distribution,
  covering math, implementation, tests, docs, and references.
- [2026-05-08 — Implementation provenance](2026-05-08-implementation-provenance.md):
  explicit dated implementation IDs and validation baselines for
  estimators and distribution functions.
- [2026-05-08 — Quality ladder](2026-05-08-quality-ladder.md):
  status levels and checklist evidence for distribution quality.
- [2026-05-08 — Validation and goldens](2026-05-08-validation-and-goldens.md):
  keep goldens, but place them inside a broader correctness strategy.
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
