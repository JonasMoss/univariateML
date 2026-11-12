# Repository audit (2026-11-12)

Read-only audit of master `9aac3b40` by Claude, at Jonas's request: where the
repository is weak in vision, certificates, verification, cross-family
consistency, architecture and development setup. The plan that follows from it
is [consolidation](../../design/consolidation.md).

## Method and limits

Read: `AGENTS.md`, the design and validation documents, the decision log, the
board, the shared core headers, the common bindings, the test support header,
the reference generator, and the fitters of lomax, weibull, norm and gamma.
Censuses over all family folders were done with `grep` and short Python scripts
(counts below). One C++ probe was compiled and run:
[probe.cpp](2026-11-12-repo-audit-data/probe.cpp), output
[probe.out](2026-11-12-repo-audit-data/probe.out), built with
`g++ 15.2.0 -std=gnu++17 -O2 -DNDEBUG` against the headers and BH.

Not done: no test suite was run; the in-flight worktree diffs, the proofs, the
site, the JS binding and the inference layer were not reviewed. Counts are for
the audited revision.

## Findings

### Vision

- `AGENTS.md` Principle 1 lists five outcomes and the purpose says every fit
  returns a verified MLE or a correct stop. Since the 2026-11-09 decision an
  uncertified search result is returned as `Ok`. Dossiers: 71 `certified: yes`,
  29 `partial` (100 families; expweibull in progress). `showcase.md` section 1
  still says "none is uncertified" and "never returns an unchecked optimizer
  iterate".
- The per-fit flag is an attribute on the R estimates. `print` and
  `model_select` do not use it. `st` and `tlambda` carry `bool certified` in the
  C++ result, but their common bindings do not forward it on success.
- Limit stops can be wrong by design: lomax reports the exponential limit when
  the certified gain is at most $10^{-8}n$, which its dossier says can reject a
  finite MLE. 121 of 293 `limits` entries are `numerical: true`.
- Non-unique maxima have no single policy: SGED, ZINB and bnbinom stop on ties;
  Laplace with even $n$ returns the upper median endpoint and Cauchy with two
  points returns one point of a continuum, both as `Ok` without a flag.
- Breadth (d/p/q over the whole parameter domain, reference-package parity,
  modifiers, new families) is ahead of the 2.0 consolidation: uniform controls
  (TASK-150.1), stress goldens (TASK-149) and the seam rerun (TASK-213) wait.

### Certificates

- No certificate uses directed rounding or interval arithmetic (no
  `fesetround`, no Boost.Interval). Margins are multiples of machine epsilon
  chosen per site, about 30 distinct multipliers from 2 to 32768. 42 dossiers
  and the paper abstract say this.
- No standing check replays certificate comparisons in high precision. The
  60- to 70-digit checks are per result.

### Verification

- All 100 goldens are one $n = 500$ sample drawn from the family; none has
  stress `cases`. The 50-digit references start Newton from the stored
  estimates: 48 cite a dossier proof for globality, 51 an eight-start
  multistart, one is local only.
- The fit-audit archive (11,185 cases, $n$ from 2 to 12) is replayed only by
  `seam-check.py`, which no Justfile recipe or test tier runs.
- The diagnostic sweep's input classes are empty, one, constant, zero,
  negative, unit-boundary, NaN and infinite. It has no magnitude classes. The
  probe found, in ten fitters on ten samples:
  - gamma on three subnormal observations returns `Ok` with scale 0 and a NaN
    log likelihood (the common binding turns this into a classless error);
  - norm on $(1, 1, 1 + 2^{-52})$ returns `Ok` with sd $1.282\times10^{-16}$;
    the MLE is $2^{-52}\sqrt2/3 = 1.047\times10^{-16}$. The fitter profiles the
    sd at the rounded mean. lnorm returns the correct value in the analogous
    case;
  - logis on $(10^{-8}, 1, 10^{8})$ returns `IterationFailure` after 100
    iterations, although the likelihood is log-concave;
  - constant data: norm `Unbounded`, gamma `Degenerate`, cauchy `NoMLE`, logis
    `InvalidInput`, lomax `ExponentialLimit`.
- `tests/test_support.hpp` has about 100 per-family golden tolerances fitted to
  observed error, some below machine epsilon (`ztgeom` 1e-17, `unif` 1e-16).
  The accuracy contract promises 1e-10 and a uniform test tolerance.
- All three GitHub workflows are `workflow_dispatch` only since 8d00e925
  (Actions quota spent, 2026-11-06). The validation guide says they run on
  every push.
- Truth tables and golden references both derive from the README
  `log_density`; the definition has no external anchor (the guide says so).

### Consistency

- 94 status enums with 123 distinct enumerator names. Examples: `InvalidInput`
  (58), `InvalidData` (20), `InvalidStart` (9); `Budget`, `BudgetExhausted`,
  `CertificateBudget`; `Tie`, `CertificateTie`, `FaceTie`, `ComparisonTie`;
  `...Limit` and `...Boundary` for the same thing. 33 result structs default
  their status to `Ok`.
- Controls in dossier `r.control`: `reltol` 49, `gradtol` 14, `abstol` 12,
  start values 16, `tol` 1. 72 dossiers have a legacy control. The accuracy
  contract and `AGENTS.md` describe the target in the present tense.
- 23 fitter headers clamp with `tol = std::min(tol, 1e-10)`; others honour a
  looser `tol`.
- Dossier prose runs from 76 bytes (gengamma) to 41 KB (sged).
- Entry-point names differ: `norm_mle`, `fit_cauchy`, `logistic_mle`,
  `lognormal_mle`.

### Architecture

- Outcome classification lives in each family's hand-written
  `bindings/common/distributions/<id>/fit.hpp`: 534 plain `fail(...)` returns
  against 271 structured stops. A C++ consumer of the core gets 94 unrelated
  enums.
- `core/newton.hpp` serves 2 families and `core/bfgs.hpp` 9. 60 files contain
  their own bisection loop and 26 an inline compensated sum.
- Code density: `lomax/mle.hpp` has a 985-character line with a 64-term series
  written out twice.

### Development setup

- 982 commits and 374 merges since 2026-10-28; no single pre-merge recipe.
- `dev/research` holds 6,790 of 8,422 tracked files: 382 reports without an
  index, 111 with an "Open" section.
- Tracked generated files collide between jobs:
  `bindings/js/modifier-metadata.mjs` was modified in every running worktree.
- Stale documents: `STATUS.md` (dated 2026-10-30, 98 families), family counts
  of 98 and 99 in the validation guide, JS README, showcase and paper plan,
  `README_SCHEMA.md` still prescribing the CRAN argument names, and `NEWS.md`
  listing unreleased changes under the 1.5.0 heading.
- Four merged branches were not deleted (removed during this audit:
  `integ/dpqr-w2`, `-w3`, `-w4`, `opus/audit`). `codex/sstd-cert` and
  `park/std-free` each hold one unmerged commit.

## Open

- The certificate margins have not been checked as a class; see step 8 of the
  consolidation plan.
- The probe covered ten families. Other families are expected to show similar
  magnitude-class defects; the snapshot fuzzer (step 3) will say.
