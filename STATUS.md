# Status and open items

Updated 2026-10-29, master `27a65fa`. Deferred design ideas with their
own rationale live in `roadmap/`; work in flight is on the board
(`~/.local/bin/board`). This file is the short list of what is open.

## Status

- 98 distributions, one folder each. Coverage of extraDistr and actuar:
  `notes/evaluations/2026-10-29-density-coverage.html` (rebuild with
  `python3 notes/evaluations/2026-10-29-density-coverage.py`). actuar is
  fully covered except `dphtype` (out of scope); extraDistr except the
  vector-parameter tier below and four documented restrictions (fatigue
  `mu = 0`, GEV and GPD shape bounds, `lst` df >= 1).
- Tests on master 89bbdf4 (local, 2026-10-30; simbox was off): C++ 6/6
  ctest targets pass (truth-table cases are `may_fail`); R
  `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 22782 ]`; `just quality` 98/98. The
  TASK-75 tolerance changes still need a simbox run to confirm that the
  simbox-only failures are gone. Truth-table checks
  (TASK-78) are `may_fail`.
- simbox runs the full suite in about 15 minutes:
  `scripts/simbox-test.sh <label> [tree]` (rsync, then `cpp-test`, `test`,
  `quality`; setup notes in the script). Six known failures there, see
  "Robustness".
- `R CMD check` with vignettes and examples: Status OK. Suggests is
  jsonlite, testthat, knitr, rmarkdown (TASK-76).
- Nothing is pushed: master is 385 commits ahead of `origin/master`.

## Decisions for Jonas

- Whether and when to push or publish (CLAUDE.md: never without an
  explicit request).
- GitHub issues #52 and #7: every density listed there is now
  implemented; the boxes can be ticked (not edited from here).
- Upstream reports for the extraDistr and actuar defects in
  `KNOWN_ISSUES.md`. Ready-to-post drafts with verified reproducers:
  `notes/upstream/extraDistr.md` (12 grouped issues; extraDistr
  1.10.0.5) and `notes/upstream/actuar.md` (7 issues; actuar 3.3.7),
  checked by `notes/upstream/verify.R` (output in `verify.out`). The
  extraDistr maintainer has been unresponsive; a draft actuar patch for
  `dpoisinvgauss` is in `notes/evaluations/2026-10-29-pig/`. Nothing has
  been posted.
- Tier 4 below: start it or not.
- Parameter rules (R1–R8, amendments A1–A7 adopted):
  `roadmap/2026-10-29-parameter-rules.md`.
  - Done: P0 (existence analysis, `notes/evaluations/2026-10-29-p0-summary.md`),
    P1 (schema, required defaults, conformance test; TASK-80), P2 (holds,
    ten group jobs; TASK-81; reports `notes/evaluations/2026-10-29-p2-*-report.md`)
    and P3 (restrictions, closure, domains, failed-optimizer stops; four
    jobs; TASK-82; reports `notes/evaluations/2026-10-29-p3-*-report.md`).
  - Left after P3: 41 `not-implemented` holds:
    - sstd and sged, 12 each: classified by P0b (TASK-88,
      `notes/evaluations/2026-10-29-p0b.md`) as data-dependent, with
      algorithms outlined; implementation pending;
    - hyper and nhyper, 3 each: C6;
    - bbinom 3, cmb 4, tbinom 4 (free size): classified by P0b as
      data-dependent, except cmb {prob, nu} held, which always has an MLE;
      implementation pending.

    Also 60 `no-mle` entries.
  - Next: P4 parametrization (TASK-83): scale over rate, half-family
    translation via `r.shift`, hypergeometric `N`; P5 docs (TASK-84).
  - C5 and C6 were picked in the design and still await a look.
  - Refinement: the missing-coefficient message offers `NULL` even where
    estimating is unsupported (e.g. pareto2); derive that from `holds:`.
  - For TASK-78 (own d/p/q/r): R7a closure in `dpqr.hpp`; NB/ZINB size 0;
    legacy `special/cmp.hpp` API (notes on the board).

## Coverage: tier 4 (vector parameters)

`dcat`, `dmixpois`, `dmixnorm` (`roadmap/2026-10-29-density-scope.md`).

- Infrastructure first: parameter vectors whose length the user chooses
  (number of categories or components) in README metadata, adapters,
  `coef`, `logLik` degrees of freedom, `bootstrapml`, `model_select`.
- `mlcat`: empirical frequencies; trivial once the infrastructure exists.
- `mlmixpois`: bounded likelihood; EM with many starts; label switching;
  merged or empty components are boundary points.
- `mlmixnorm`: unbounded with free component scales (a component
  collapses on an observation); support held scales and equal scales;
  local maxima cannot be certified.

## Robustness

- TASK-85 and its leftovers (TASK-87) are done:
  - the binomial size search has no cap (a large-size cancellation bug
    was fixed);
  - GEV/Fréchet compare the k = n/2 tie case with the collapse face.

  Open: the zib/ztbinom size searches remain numerical (unimodality
  unproved).
- TASK-77 (scale equivariance) is fixed: f9e1222, with
  `test_scale_equivariance.R`.
- TASK-75, cross-machine fragility. On simbox (gcc 15.2 or 13.3, BH 1.90,
  glibc 2.39, i7-12700) six tests fail that pass locally (glibc 2.43,
  i7-1355U); the compiler is not the cause:
  - C++ `betapr/test.cpp:33`: the n = 20 slow-oracle fixture returns
    status 1 instead of 0 (a borderline sample);
  - C++ `std/test.cpp:40`: golden `nu` 3.97765 vs 3.97766 at `kFdTol`;
  - R golden regression: `betapr` (relative 7e-8), `bnbinom` (1e-10),
    `std` (`nu` 8e-6);
  - R `test-dist-sstd.R`: `qsstd` at `nu = 100, xi = 4` differs from the
    frozen fGarch value by a relative 1e-12, exactly the tolerance; the
    frozen value is the same file on both machines, so our `qsstd`
    moves.
  Golden tolerances should match each fitter's convergence tolerance;
  status fixtures need margin. Likely to matter on CRAN machines.
- Performance (TASK-89, `notes/evaluations/2026-10-29-perf-report.md`):
  GB2 at n = 1e6 is now about 169 s (was 329 s); the remaining time is in
  the burr, invburr, betapr and trgamma seed fitters it calls.
  Bhattacharjee at n = 1000 is about 7 s, bounded by a long-double
  `erfc` in `bhatt/dpqr.hpp` (TASK-78's file).

## Write-up

- `notes/ml-behaviour.tex` (19 pages, TASK-90) now covers:
  - the 31 families added on 2026-10-29;
  - existence under holds and restrictions: the R5 mechanisms, a
    proposition that $c \ge 1$ is the weakest restriction excluding
    collapse for every $n$, R7a, R7, and the P0 counterexamples;
  - bugs B1–B8 and TASK-77;
  - the open mathematics.
- Failure gallery (TASK-91): `notes/failure-gallery.tex` (11 pages), ten
  phenomena, each with condition, reproducer, frequency and a profile
  figure (`notes/failure-gallery/fig*.R`).

## Code cleanup

- `univariateML_continuous_quantile` (`code_helpers.R`) checks `sigma`
  and `a` by name (slash, Bhattacharjee); make the parameter checks
  generic before a third family uses it.
- Done: test argument switches replaced by golden `fit_args` (P1); the
  `mlhnorm` special case removed (P3); actuar `dgumbel(x, alpha, scale)`
  equals `extraDistr::dgumbel(x, mu, sigma)` with alpha = mu, scale =
  sigma (`notes/upstream/dgumbel-mapping.md`). Held `N` for `bootstrapml`
  waits for P4.

## Open mathematics (from the job reports)

- Global-search certification: most new fitters use grids plus local
  refinement (half-t df, dgamma shape, Skellam, slash, Bhattacharjee,
  Tukey lambda, generalized beta, GB2); none is a certified global
  optimum.
- dgamma: is the shape profile unimodal?
- Skellam: uniqueness of the joint maximum.
- Slash: a closed sample condition for the exactly-half-tied case (the
  implemented criterion is variational).
- Generalized beta: is the list of limit models complete?
- Tukey lambda: the U-shaped branch 1 < lambda < 2 is not fitted (the
  uniform junction at lambda = 1 is not upper semicontinuous).
- Negative hypergeometric: the inverse-sampling design (estimate the
  population size) is unsupported.
- Truncated binomial: estimating `size` is unsupported.
- Free-threshold fits with another parameter held (Pareto II-IV,
  Feller-Pareto, nonstandard beta) stop as "not supported"; each subset
  is its own existence question.
- Translated half-t needs a df lower bound to have an MLE.
- Beta-binomial with fixed size: existence rule proved for size 2, open
  for size >= 3.

## Deferred families (roadmap)

`roadmap/2026-10-29-classical-failure-families.md`: threshold
(three-parameter) Weibull, gamma and lognormal; skew-t; Prentice's
extended generalized gamma; Student t continued into Pearson type II.
