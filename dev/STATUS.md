# Status and open items

Updated 2026-11-13, master `c167a975`. Decisions and their evidence:
`dev/roadmap/decisions.md`. Deferred design ideas live in `dev/roadmap/`; work in
flight and planned work are on the board (`~/.local/bin/board`). The current
priority is the [consolidation plan](design/consolidation.md) (TASK-214), which
follows the [repository audit](research/evaluations/2026-11-12-repo-audit-report.md);
new families are held until its later steps. This file is the short list of
what is open. Sections below "Status" were last revised on 2026-10-30 and may
be behind the board.
Validation methods and coverage limits: [validation guide](validation/README.md).

## Status

- 86 families, one folder each: 85 with fitters and one, pgw, with d/p/q/r
  only (dossier `stage: dpqr`). The 16 truncated, zero-modified and
  zero-inflated families were removed on 2026-11-13 (TASK-191); their laws are
  `zero` and `truncate` settings of the base families
  ([modifiers](design/modifiers.md), migration entry in `bindings/r/NEWS.md`).
  Coverage of extraDistr and actuar (before that removal):
  `dev/research/evaluations/2026-10-29-density-coverage.html` (rebuild with
  `python3 dev/research/evaluations/2026-10-29-density-coverage.py`). actuar is
  fully covered except `dphtype` (out of scope); extraDistr except the
  dropped vector-parameter families and four documented restrictions (fatigue
  `mu = 0`, GEV and GPD shape bounds, `lst` df >= 1).
- Tests on master c167a975 (workstation, GCC 15.2, 2026-11-13, after the
  outcome mapping, the family removal and the quantile-tie change):
  `just cpp-test` 109/109 ctest entries; `just cpp-test-no-long-double`
  153/153; `just test` `[ FAIL 0 | WARN 0 | SKIP 157 | PASS 28842 ]`;
  `just test-full` `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 57977 ]`; `just js-test`
  578/578; `just relations` 158 edges, 0 failed; `just site-check` green.
  `just quality` passed for all 101 then-fitted families on `6a757613`, before
  the removal. Fit snapshots of `c167a975`, with the shared outcome of every
  case: [baseline report](research/evaluations/2026-11-13-fit-snapshot-report.md);
  `dev/validation/fit-judge.py` passes against its 21 known failures. Not run:
  any second compiler or platform.
- GitHub Actions run only on manual dispatch since 2026-11-06 (quota spent);
  there is no macOS or Windows result for anything merged after that date.
- simbox was unreachable on 2026-11-12 and 2026-11-13; the jobs of those
  days were validated on the workstation only. `dev/tools/simbox-test.sh <label> [tree]`
  runs the suite there when it is back.
- Known wrong answers on master, each with a card: the tie rule for discrete
  quantiles is not yet applied to the `zero` and `truncate` routes (TASK-215,
  job `qtie-2`; the 20 public discrete families have it); fits that miss the accuracy
  contract or return `Ok` with a non-finite value (TASK-214.3, TASK-214.5);
  stop kinds that depend on the precision mode (TASK-214.13).
- Nothing is published to CRAN, and `origin/master` is the old published
  tree (about 1,300 commits behind). The backup branch `origin/cpp-core`
  mirrors master; pushed on 2026-11-13 at Jonas's request.

## Decisions for Jonas

- Whether and when to push or publish (AGENTS.md: never without an
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
2026-10-30: out of scope (AGENTS.md, "Out of scope").

## Robustness

- TASK-85 and its leftovers (TASK-87) are done:
  - the binomial size search has no cap (a large-size cancellation bug
    was fixed);
  - GEV/Fréchet compare the k = n/2 tie case with the collapse face.

  Open: the zib/ztbinom size searches remain numerical (unimodality
  unproved).
- d/p/q accuracy at extreme parameters is unchecked outside the t/GED
  families, expweibull and gengamma: truth tables span about 0.1 to 10
  times each default. At large shapes, `shape*(log(x) - log(scale))`
  loses about `shape * eps * |log(scale)|`; against 60-digit mpmath at
  `scale = 1000`, `x = scale*(1 + 0.5/shape)`, Weibull's log density is
  off by 2.7e-9 at shape 1e8 and 1.5e-6 at 1e12, its upper log tail by
  4.4e-5 at 1e12, log-logistic and inverse Weibull by up to 9e-7 at 1e12.
  GEV and GPD near `xi = 0` are accurate. Audit and contract: TASK-189;
  Fréchet: TASK-187.
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
- dgamma (discretised gamma): is the shape profile unimodal? Reduced to
  the rate-profiled shape curve; conjectured, unproved (TASK-117 report).
- Skellam: uniqueness of the joint maximum. Reduced to a scalar
  Bessel-ratio equation; conjectured, unproved (TASK-117 report).
- Slash: a closed sample condition for the exactly-half-tied case. A
  sufficient condition is proved (nonzero reciprocal sum at the
  largest-collapse tie, TASK-117 report); a zero sum remains undecided.
- Generalized beta: is the list of limit models complete for maximising
  sequences? Conjectured; mixed weak limits exist but do not win on the
  paths checked (TASK-117 report).
- Tukey lambda: the U-shaped branch 1 < lambda < 2 is not fitted. Proved
  (TASK-117 report): as lambda decreases to 1 the endpoint density is twice
  the uniform one, so with sample extremes on the support endpoints the
  branch supremum can be unattained; a fitter needs a junction status.
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
- Beta-binomial with fixed size: overdispersion is sufficient for every
  size and necessary for size 2 (proved); necessity for size >= 3 is
  conjectured (TASK-117 report). The fitter could use the rule.

## Deferred families (roadmap)

[Useful density proposals](roadmap/2026-10-31-useful-density-proposals.md) banks
exponentiated Weibull, GIG, PGW, sinh–arcsinh and unified Prentice gamma, with
priorities and preliminary ML/dpqr risks. No implementation is underway.

Card: TASK-110.

`dev/roadmap/2026-10-29-classical-failure-families.md`: threshold
(three-parameter) Weibull, gamma and lognormal; skew-t; Prentice's
extended generalized gamma; Student t continued into Pearson type II.
