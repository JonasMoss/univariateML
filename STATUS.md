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
- Tests on master d72f6bd (simbox): C++ 632 doctest cases, only the two
  known TASK-75 fixtures fail; R `[ FAIL 3 | WARN 0 | PASS 22310 ]`, the
  three known TASK-75 failures; `just quality` passes. Truth-table checks
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
  `KNOWN_ISSUES.md` (about 20 added on 2026-10-29). The extraDistr
  maintainer has been unresponsive; a draft actuar patch for
  `dpoisinvgauss` is in `notes/evaluations/2026-10-29-pig/`.
- Tier 4 below: start it or not.
- Parameter rules (R1–R8, amendments A1–A7 adopted):
  `roadmap/2026-10-29-parameter-rules.md`.
  - Done: P0 (existence analysis, `notes/evaluations/2026-10-29-p0-summary.md`),
    P1 (schema, required defaults, conformance test; TASK-80) and P2 (holds,
    ten group jobs; TASK-81; reports `notes/evaluations/2026-10-29-p2-*-report.md`).
  - Left after P2: 53 `not-implemented` holds:
    - sstd and sged, 12 each: existence unresolved;
    - tnorm, 12: needs R7a;
    - hyper and nhyper, 3 each: C6;
    - bbinom 3, cmb 4, tbinom 4: free size unresolved.

    Also 61 `no-mle` entries in 9 families.
  - Next: P3 restrictions, closure (R7a), domains and failed-optimizer
    stops (TASK-82); P4 parametrization (TASK-83); P5 docs (TASK-84).
  - C5 and C6 were picked in the design and still await a look.
  - Refinement: the missing-coefficient message offers `NULL` even where
    estimating is unsupported (e.g. pareto2); derive that from `holds:`.

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

- TASK-85 (P0 fitter bugs) is fixed. Open from it: binomial size search
  capped at 1e6; zib/ztbinom size grids uncertified; free gev/frechet
  reject the tie case k = n/2 conservatively.
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
- Performance: GB2 at n = 1e6 takes about 309 s; Bhattacharjee at
  n = 1000 takes tens of seconds.

## Write-up

- `notes/ml-behaviour.tex` does not yet cover any family added on
  2026-10-29: bnbinom, cmp, cmb, zeta, yulesimon, gb2, sgomp, foldnorm,
  emg, sn, huber with free epsilon, hcauchy, ht, hnorm with held `mu`,
  tpois, tbinom, dlaplace, nhyper, dnorm, dgamma, skellam, pareto2,
  pareto3, pareto4, fpareto, nsbeta, frechet, tlambda, genbeta, slash,
  bhatt. Their analyses are in each `README.md` and in
  `notes/evaluations/2026-10-29-*` (reports, oracles, escape-frequency
  tables).
- Failure gallery: one entry per phenomenon with a reproducer and a
  profile plot (`roadmap/2026-10-29-classical-failure-families.md`).
  Candidates from 2026-10-29: two-adjacent-integer escape of the
  discretized families; Bhattacharjee's normal face at about 46% for
  every n; generalized beta's power samples escaping to a shorter power
  law; Tukey lambda unbounded as lambda grows; the slash half-mass case;
  bimodal half-t df profile; skew-normal bimodal profile.

## Code cleanup

- Test argument switches in `univariateML/tests/testthat/test_input_checks.R`
  and `test-audit-input.R` list every `required` design argument by hand,
  and every new family edits the same line (merge conflicts). Read
  required arguments from each golden's `fit_args`; keep the switch only
  for test-specific holds (`mlht nu = 3`, `mlsn alpha = 0`, ...).
- `univariateML_to_function` special-cases `mlhnorm` with held `mu`
  (closed endpoint, unlike the translated `dtnorm` convention). Replace by
  a README-level mechanism.
- `univariateML_continuous_quantile` (`code_helpers.R`) checks `sigma`
  and `a` by name (slash, Bhattacharjee); make the parameter checks
  generic before a third family uses it.
- `bootstrapml` cannot rebuild the held population size `N` of `mlhyper`
  and `mlnhyper` (it is not a coefficient).
- actuar `dgumbel`: confirm the location/scale mapping to `mlgumbel`
  (flagged in the coverage inventory).

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
