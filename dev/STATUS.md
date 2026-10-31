# Status and open items

Updated 2026-10-30, master `32b8dfe`. Decisions and their evidence:
`dev/roadmap/decisions.md`. Deferred design ideas live in `dev/roadmap/`; work in
flight and planned work are on the board (`~/.local/bin/board`): the package
migration is TASK-101, inference TASK-102, the optional holds and existence
work TASK-113 to TASK-120. This file is the short list of what is open.
Validation methods and coverage limits: [validation guide](validation/README.md).

## Status

- 98 distributions, one folder each. Coverage of extraDistr and actuar:
  `dev/research/evaluations/2026-10-29-density-coverage.html` (rebuild with
  `python3 dev/research/evaluations/2026-10-29-density-coverage.py`). actuar is
  fully covered except `dphtype` (out of scope); extraDistr except the
  dropped vector-parameter families and four documented restrictions (fatigue
  `mu = 0`, GEV and GPD shape bounds, `lst` df >= 1).
- Tests on master 32b8dfe (simbox, 2026-10-30): C++ 6/6 ctest targets
  pass; R `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 30814 ]`; `just quality`
  98/98. TASK-78 merged (8471607): own internal d/p/q/r for every
  non-stats family, all 98 truth tables pass; extraDistr and actuar are
  no longer dependencies (simbox: R `[ FAIL 0 | PASS 34188 ]`, R CMD check OK).
- simbox runs the full suite in about 15 minutes:
  `dev/tools/simbox-test.sh <label> [tree]` (rsync, then `cpp-test`, `test`,
  `quality`; setup notes in the script). No known failures there since
  TASK-75.
- The earlier TASK-94 check on simbox had a missing-cross-reference warning.
  The later [standalone R check](research/evaluations/2026-10-30-r-standalone-report.md)
  passed with packaged fixtures and no ERROR, WARNING or NOTE.
- Nothing is pushed: master is 516 commits ahead of `origin/master`.

## Decisions for Jonas

- Whether and when to push or publish (CLAUDE.md: never without an
  explicit request).
- GitHub issues #52 and #7: every density listed there is now
  implemented; the boxes can be ticked (not edited from here).
- Upstream reports for the extraDistr and actuar defects in
  `dev/KNOWN_ISSUES.md`. Ready-to-post drafts with verified reproducers:
  `dev/research/upstream/extraDistr.md` (12 grouped issues; extraDistr
  1.10.0.5) and `dev/research/upstream/actuar.md` (7 issues; actuar 3.3.7),
  checked by `dev/research/upstream/verify.R` (output in `verify.out`). The
  extraDistr maintainer has been unresponsive; a draft actuar patch for
  `dpoisinvgauss` is in `dev/research/evaluations/2026-10-29-pig/`. Nothing has
  been posted.
- Parameter rules (R1–R8, amendments A1–A7 adopted):
  `dev/roadmap/2026-10-29-parameter-rules.md`.
  - Done: P0 (existence analysis, `dev/research/evaluations/2026-10-29-p0-summary.md`),
    P1 (schema, required defaults, conformance test; TASK-80), P2 (holds,
    ten group jobs; TASK-81; reports `dev/research/evaluations/2026-10-29-p2-*-report.md`)
    and P3 (restrictions, closure, domains, failed-optimizer stops; four
    jobs; TASK-82; reports `dev/research/evaluations/2026-10-29-p3-*-report.md`),
    P4 (parametrization, TASK-83, `dev/research/evaluations/2026-10-29-p4-report.md`:
    scale for gamma, invexp, invweibull, llogis; hnorm, hcauchy, ht
    translated by `mu`; hyper and nhyper with `N`) and P2b (TASK-93,
    `dev/research/evaluations/2026-10-29-p2b-{skew,binom}-report.md`: all 24
    sstd/sged subsets, tbinom free size, cmb {prob, nu}). TASK-94 aligned
    the eight C++ d/p/q/r signatures with the P4 parameters.
  - TASK-97 (terra high, 2026-10-30) analysed the rest: reports
    `dev/research/evaluations/2026-10-30-holds-{bbinom,cmb,hyper}-report.md`.
  - Left: 11 `not-implemented` holds, and 60 `no-mle` entries:
    - bbinom {}, {alpha}, {beta} and cmb {}, {prob}, {nu} with free size.
      Held-beta bbinom has a proved finite maximizer, but its tail bound
      decays like 1/log N, too loose to search. The others lack a full
      list of limit faces and a tail bound: fixed-size two-point and
      point-mass faces, NB, Poisson, CMP and geometric limits, and
      mass escaping to infinity.
    - hyper {}, {k}, {m} and nhyper {}, {r}: the population can grow
      without bound; each needs a tail certificate against its binomial,
      Poisson or negative-binomial face. The finite grids (hyper {N},
      {m, N}; nhyper {N}, {m}, {m, r}, {m, N}) are implemented by exact
      enumeration (TASK-98). The four former `no-mle` labels were wrong:
      constant samples attain likelihood one.
  - P0b corrections are recorded in `dev/research/evaluations/2026-10-29-p0b.md`
    (P5) and in the TASK-97 reports.
  - P5 docs (TASK-84) are done: README_SCHEMA, CLAUDE.md,
    NEWS, `dev/research/ml-behaviour.tex`.
  - Refinement: the missing-coefficient message offers `NULL` even where
    estimating is unsupported (e.g. pareto2); derive that from `holds:`.
  - TASK-78.14: R7a endpoint closure and attained boundary parameters in
    every `dpqr.hpp` (pig shape 0 and ZINB size 0 done); NB size 0 in
    `special::dnbinom`; legacy `special/cmp.hpp` API; sstd/sged multistart
    gate (`core/multistart.hpp`).

## Coverage

The vector-parameter tier (`dcat`, `dmixpois`, `dmixnorm`) was dropped on
2026-10-30: out of scope (CLAUDE.md, "Out of vision").

## Robustness

- TASK-85 and its leftovers (TASK-87) are done:
  - the binomial size search has no cap (a large-size cancellation bug
    was fixed);
  - GEV/Fréchet compare the k = n/2 tie case with the collapse face.

  Open: the zib/ztbinom size searches remain numerical (unimodality
  unproved).
- TASK-77 (scale equivariance) is fixed: f9e1222, with
  `test_scale_equivariance.R`.
- TASK-75, cross-machine fragility, is done: golden tolerances follow
  each fitter's convergence tolerance (finite-difference BFGS fitters
  3e-5, about three times the observed drift), status fixtures have
  margin, and frozen quantiles compare at 1e-9. The simbox-only failures
  are gone (C++ and R, 2026-10-30).
- Performance (TASK-89, `dev/research/evaluations/2026-10-29-perf-report.md`):
  GB2 at n = 1e6 is now about 169 s (was 329 s); the remaining time is in
  the burr, invburr, betapr and trgamma seed fitters it calls.
  Bhattacharjee at n = 1000 is about 7 s, bounded by a long-double
  `erfc` in `bhatt/dpqr.hpp` (TASK-78's file).

## Write-up

- `dev/research/ml-behaviour.tex` (19 pages, TASK-90) now covers:
  - the 31 families added on 2026-10-29;
  - existence under holds and restrictions: the R5 mechanisms, a
    proposition that $c \ge 1$ is the weakest restriction excluding
    collapse for every $n$, R7a, R7, and the P0 counterexamples;
  - bugs B1–B8 and TASK-77;
  - the open mathematics.
- Failure gallery (TASK-91): `dev/research/failure-gallery.tex` (11 pages), ten
  phenomena, each with condition, reproducer, frequency and a profile
  figure (`dev/research/failure-gallery/fig*.R`).

## Code cleanup

- Done: test argument switches replaced by golden `fit_args` (P1); the
  `mlhnorm` special case removed (P3); actuar `dgumbel(x, alpha, scale)`
  equals `extraDistr::dgumbel(x, mu, sigma)` with alpha = mu, scale =
  sigma (`dev/research/upstream/dgumbel-mapping.md`). `bootstrapml` rebuilds a
  held `N` (checked in P4).

## Open mathematics (from the job reports)

Cards: TASK-113 to TASK-117 (TASK-98 for hyper/nhyper).

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
- Truncated binomial with free size and free probability: a numerical
  profile search compared with the truncated-Poisson face; not certified
  unimodal.
- sstd and sged hold subsets: global optimality is numerical; equality
  in the Student-t collapse criterion is uncertified, and a held uniform
  width equal to the sample range (sged, mean free) stops with
  `ScaleBoundary`.
- Free-threshold fits with another parameter held (Pareto II-IV,
  Feller-Pareto, nonstandard beta) stop as "not supported"; each subset
  is its own existence question.
- Translated half-t: with `mu` free the fit uses the restriction
  nu >= 1 (P4); the nu search is numerical.
- Beta-binomial with fixed size: existence rule proved for size 2, open
  for size >= 3.

## Deferred families (roadmap)

[Useful density proposals](roadmap/2026-10-31-useful-density-proposals.md) banks
exponentiated Weibull, GIG, PGW, sinh–arcsinh and unified Prentice gamma, with
priorities and preliminary ML/dpqr risks. No implementation is underway.

Card: TASK-110.

`dev/roadmap/2026-10-29-classical-failure-families.md`: threshold
(three-parameter) Weibull, gamma and lognormal; skew-t; Prentice's
extended generalized gamma; Student t continued into Pearson type II.
