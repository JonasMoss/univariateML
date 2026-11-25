# Status and open items

Updated 2026-11-15, master `a253d272`. Decisions and their evidence:
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
- Tests on master 810653f8 (workstation, GCC 15.2, 2026-11-16, after the
  2026-11-15 and 2026-11-16 merges: certificate replay, strict rules, face,
  margin and unique-flag fixes, step 5c jobs 1, 2a, 2b, equivariance
  batches A to F (TASK-214.42), near-constant maximizers and limits
  (TASK-214.39)): fresh fit snapshots of all four modes equal the stored
  [2026-11-15 baselines](research/evaluations/2026-11-15-fit-snapshot-report.md)
  as patched after each merge (11905 cases in each of native and double,
  1950 in each route mode);
  `just strict` passes against `dev/validation/strict-known.json`;
  `just cpp-test` 110/110; `just cpp-test-no-long-double` 154/154;
  `just test` `[ FAIL 0 | WARN 29 | SKIP 157 | PASS 36337 ]` (the warnings
  are hyper/nhyper non-unique messages, TASK-214.45); `just test-full`
  (with `just truth-check`) `[ FAIL 0 | WARN 29 | SKIP 1 | PASS 65737 ]`;
  `just js-test` 586/586; `just relations` and `just site-check` green.
  The known list has 87 entries, each with a card.
  Not run: any second compiler or platform.
- Batch check of master 1252894e (2026-11-20, workstation, after step 5c
  jobs 2c, 2d and 3: [ctrl-c](research/evaluations/2026-11-20-ctrl-c-report.md),
  [ctrl-d](research/evaluations/2026-11-20-ctrl-d-report.md),
  [ctrl-r](research/evaluations/2026-11-20-ctrl-r-report.md)): fresh snapshots
  of all four modes equal the patched baselines (0 differing cases);
  `just strict` passes after two updates in 23f4b523: the dweibull replay
  now checks the `tol = 1e-10` claims of job 2d (84 root records hold),
  the 8 burr refine entries are fixed and pruned, and fpareto archive
  001625 and 001628 (native numerical, double-only unresolved) are listed
  under TASK-214.13. The known list has 81 entries. The suites of that run
  were stopped to free CPU for the running jobs; ctrl-js (job 4, merged as
  bab2a84e) ran them on 1252894e plus its change: `just cpp-test` 110/110,
  `just cpp-test-no-long-double` 154/154, `just js-test` 587/587,
  `just site-check` green. R `just test-full` was not rerun on this batch.
- Batch check of master dda89f62 (2026-11-20, workstation, GCC 15.2, after
  step 5c job 4 ([ctrl-js](research/evaluations/2026-11-20-ctrl-js-report.md)),
  [refine-fix](research/evaluations/2026-11-20-refine-fix-report.md),
  [root-ulp](research/evaluations/2026-11-20-root-ulp-report.md) and
  [route-acc](research/evaluations/2026-11-20-route-acc-report.md)): fresh
  snapshots of all four modes differ from the stored baselines only as the job
  reports describe (rounding-level moves up to 1.9e-13 in 14 families and the
  limit payloads of betapr, fpareto, gb2; 23 tnorm route failures now
  certified estimates; tbinom 7325 a certified tpois limit), and the baselines
  were patched from them (b3f1e1ad). `just cpp-test` 110/110;
  `just cpp-test-no-long-double` 154/154; `just test-full`
  `[ FAIL 0 | WARN 29 | SKIP 1 | PASS 65782 ]`; `just js-test` 587/587;
  `just relations` and `just site-check` green. `just strict`: 50 known
  entries fixed (40 root widths, 6 nhyper/tbinom nesting, 4 pareto3 refine)
  and pruned; 4 new nesting entries list TASK-214.46 (a tnorm limit payload
  that predates the batch and became visible once the held-sd fits
  converged). The known list has 35 entries, each with a card.
- Batch check of master 66a1d9a7 (2026-11-21, workstation, GCC 15.2, after
  [ew-limits](research/evaluations/2026-11-21-ew-limits-report.md),
  [test-refs](research/evaluations/2026-11-21-test-refs-report.md),
  [tnorm-limit](research/evaluations/2026-11-21-tnorm-limit-report.md) and
  [strict-par](research/evaluations/2026-11-21-strict-par-report.md)): fresh
  snapshots differ from the stored baselines only as the reports describe
  (native and double: 20 expweibull records, 18 of them numerical stops that
  are now flagged uncertified limits, estimate moves up to 2.7e-15; each route
  mode: 19 tnorm truncated-exponential limit payloads re-solved, largest
  nonzero rate move 2.3e-4 at 7657, and 7779 now a certified limit), and the
  baselines were patched from them (3dcce713). `just strict` (four workers,
  cold cache, 77 min) passes; 8 known entries are fixed and pruned (the 4
  TASK-214.46 tnorm nesting entries and 4 expweibull near-constant and
  two-point entries of TASK-214.33), none new. `just cpp-test` 110/110;
  `just cpp-test-no-long-double` 154/154; `just test-full`
  `[ FAIL 0 | WARN 29 | SKIP 1 | PASS 65793 ]`; `just js-test` 587/587;
  `just relations` and `just site-check` green; `capabilities.csv` regenerated
  (expweibull limit count 3 to 5). The known list has 27 entries, each with a
  card.
- Batch check of master 10caef8e (2026-11-21, workstation, GCC 15.2, after
  [mode-kind](research/evaluations/2026-11-21-mode-kind-report.md),
  [strict-33](research/evaluations/2026-11-21-strict-33-report.md),
  [loose-tests](research/evaluations/2026-11-21-loose-tests-report.md) and
  [gb2-acc](research/evaluations/2026-11-21-gb2-acc-report.md); the first run
  in the memory-capped `umvl-work.slice`): fresh snapshots differ from the
  stored baselines only as the reports describe (168 native and 177
  double-only records in 15 families, per-family counts equal to the reports;
  about 60 fpareto/GB2 archive stops now estimates within 2.7e-12 of 100-digit
  references; outcome changes in bhatt, dgamma, snorm, st, gompertz, kumar,
  sgomp, gengamma, sstd; routes only a zinb iteration count), and the
  baselines were patched from them (9ac216ad). `just strict` (warm cache,
  10 min) passes; 23 known entries are fixed and pruned (all 11 of
  TASK-214.13, all 12 of TASK-214.33), none new. fit-refine's cached run
  lists 26 cases without a stored reference; a full refine run judged them
  too (3848 cases, no failure); why they are not stored is noted on
  TASK-214.48. `just cpp-test` 110/110; `just cpp-test-no-long-double`
  154/154; `just test-full` `[ FAIL 0 | WARN 29 | SKIP 1 | PASS 65854 ]`;
  `just js-test` 587/587; `just relations` and `just site-check` green. The
  known list has 4 entries (tlambda, TASK-214.17).
- Batch check of master 175d507d (2026-11-22, workstation, GCC 15.2, after
  [tlambda-ties](research/evaluations/2026-11-21-tlambda-ties-report.md),
  [loose-2](research/evaluations/2026-11-21-loose-2-report.md),
  [margin-2](research/evaluations/2026-11-21-margin-2-report.md) and
  [speed-e](research/evaluations/2026-11-21-speed-e-report.md)): fresh
  snapshots differ from the stored baselines only in 4 tlambda records per
  base mode (ties now `N:unbounded`, distinct pairs a certified uniform
  estimate) and one zinb route iteration count, and the baselines were
  patched from them (40e7d731). `just strict` passes with **no known entry
  left**: the 4 tlambda entries are fixed and pruned, none new. Its replay
  and seam caches were cold (speed-e changed modules in the replay key and
  the seam grouping), 75 min. `just cpp-test` 134/134 and
  `just cpp-test-no-long-double` 179/179 (speed-e registers expensive cases
  as their own tests; same cases and assertions); `just test-full`
  `[ FAIL 0 | WARN 29 | SKIP 1 | PASS 65860 ]`; `just js-test` 587/587;
  `just relations` and `just site-check` green.
- Batch check of master 89b214ed (2026-11-22/23, workstation, GCC 15.2, after
  [nhyper-zero](research/evaluations/2026-11-22-nhyper-zero-report.md),
  [mod-gaps](research/evaluations/2026-11-22-mod-gaps-report.md),
  render-fu (TASK-214.18), zero-unique (TASK-227), short-window (TASK-228),
  flat-cont, [qged-affine](research/evaluations/2026-11-22-qged-affine-report.md)
  and flat-count): fresh snapshots differ in 56 records per base mode, all
  explained (hyper 29, nhyper 3, triang 7, sged 15, binom 1, cmb 1), plus
  112 new flat-maximum records per base mode and 53 per route mode; routes
  otherwise unchanged. Baselines patched (7ce38eb0). `just strict`:
  fit-consistency 70 known entries fixed and pruned (db70789a), none new;
  114 `phenomenon:flat-maximum` entries remain, all TASK-229 (flat maxima
  without a maximizer payload, mostly zero-modified and inflated count
  routes; the typed maximizer set fixes them). The certificate replay
  failed on new records only on the replay side (bbinom nothing held,
  cmb prob held at 1, cmp lambda held at 0, pig shape held at 0; fixed in
  049256a8, 7748 cases, none new). fit-refine still has pre-existing
  reference crashes (triang, zero-modified binom, nhyper, cmp, dgamma),
  TASK-232. `just cpp-test` 134/134; `just cpp-test-no-long-double`
  179/179; `just test-full` `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 66848 ]`;
  `just js-test` 588/588; `just relations` and `just site-check` green.
- Batch check of master 83ed77a9 (2026-11-23, workstation, GCC 15.2, after
  shift-payload (TASK-214.30),
  [refine-crash](research/evaluations/2026-11-22-refine-crash-report.md)
  and [maxset-core](research/evaluations/2026-11-22-maxset-core-report.md),
  typed maximizer sets part A): fresh snapshots differ only by the new
  nullable `maximizer_set` field, typed payloads on 25 pilot records per
  base mode, 11 unproved `unique = FALSE` now `NA` (fatigue 10, triang 1)
  and two new cauchy fixtures; no estimate, limit or kind changed.
  Baselines patched (33a473f4). `just strict` passes: 114 known
  (TASK-229, unchanged), none new; fit-refine judges 3274 with no
  reference exception (the 42 gpd references were refilled, 7cf212f8);
  cert-replay 7752 cases. `just cpp-test` 135/135;
  `just cpp-test-no-long-double` 180/180; `just test-full`
  `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 66915 ]`; `just js-test` 593/593;
  `just relations` and `just site-check` green.
- Batch check of master 2d3a806d (2026-11-23, workstation, GCC 15.2, after
  [maxset-cont](research/evaluations/2026-11-23-maxset-cont-report.md) and
  [maxset-count](research/evaluations/2026-11-23-maxset-count-report.md),
  typed maximizer sets part B): per base mode 122 records changed and 13
  new (typed payloads replacing legacy rows; `unique` FALSE -> NA for 26
  hyper swap fits, 18 of them also uncertified, the triang subnormal class
  and the bnbinom golden), routes 84 changed and 3 new; no estimate, limit
  or outcome kind changed. Baselines patched (789cfc68). `just strict`:
  all 114 TASK-229 entries pass, none new; **the known list is empty**
  (6b9d3b2c). `just cpp-test` 135/135; `just cpp-test-no-long-double`
  180/180; `just test-full` `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 66973 ]`;
  `just js-test` 593/593; `just relations` and `just site-check` green.
  The follow-up [maxset-legacy](research/evaluations/2026-11-23-maxset-legacy-report.md)
  (f7cf7045, after this check) removes the legacy non-uniqueness path;
  its snapshots equal these baselines in all four modes and its own
  `just test-full` passed (67,025).
- Batch check of master a529330e (2026-11-24, workstation, GCC 15.2, after
  maxset-legacy and [payload-offsets](research/evaluations/2026-11-23-payload-offsets-report.md),
  TASK-233): per base mode 150 fpareto/gb2 records gain face payloads in
  fields that were empty and 21 nonzero-threshold fixtures are new; routes
  unchanged; baselines patched (e807ef23). `just strict`: fit-density now
  evaluates face candidates and finds 66 findings that already existed
  (TASK-236, added to the known list, dd86aea1; 36 are uniform candidates whose
  support excludes observations); the pareto2 transfer rule did not know the
  shifted exponential at nonzero `min` (replay fixed). `just cpp-test`
  135/135; `just cpp-test-no-long-double` 180/180; `just test-full`
  `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 67041 ]`; `just js-test` 593/593;
  `just relations` and `just site-check` green.
- Batch check of master 55463f6c (2026-11-24, workstation, GCC 15.2, after
  [cand-bounds](research/evaluations/2026-11-24-cand-bounds-report.md),
  TASK-236): **one certified wrong outcome fixed**: GED class near-constant
  returned a certified Laplace-shape estimate (logLik 125.0623) though the
  uniform limit attains 126.8459 (the rounded midpoint fell on an endpoint
  and doubled the face width); it now returns the certified uniform limit.
  Otherwise candidate likelihoods and supports at their published
  coefficients: native 94, double 93 records; routes unchanged. Baselines
  patched (a9581049). `just strict`: all 66 TASK-236 entries pass, none
  new; the known list is empty again (9ea18e5e). `just cpp-test` 135/135;
  `just cpp-test-no-long-double` 180/180; `just test-full`
  `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 67041 ]`; `just js-test` 593/593;
  `just relations` and `just site-check` green.
- Batch check of master c8381ffa (2026-11-24, workstation, GCC 15.2, after
  [ged-speed](research/evaluations/2026-11-24-ged-speed-report.md), TASK-225,
  and [dblpareto](research/evaluations/2026-11-24-dblpareto-report.md),
  TASK-235): 86 families. New family dblpareto (28 cases per base mode); 11
  gb2/fpareto double-Pareto escapes per base mode now carry dblpareto
  payloads (names only; outcome, certification and values unchanged); routes
  unchanged; the GED speed-up moves nothing. Baselines patched (61e8589f).
  `just strict`: nothing new, known list still empty. `just cpp-test`
  136/136; `just cpp-test-no-long-double` 181/181; `just test-full`
  `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 67514 ]`; `just js-test` 599/599;
  `just relations` and `just site-check` green.
- Batch check of master c27bef30 (2026-11-24, workstation, GCC 15.2, after
  [claims inventory](research/evaluations/2026-11-24-claims-inventory-report.md),
  TASK-237, and [label-fixes](research/evaluations/2026-11-24-label-fixes-report.md),
  TASK-238): 25 records per base mode, all flags: Skellam joint interior
  estimates and near-face selections no longer certified by a likelihood gap
  (16 + 2), fatigue's analytic constant pair and six constant-sample
  lomax/invpareto/pareto2 exponential limits now certified; routes
  unchanged. Baselines patched (bc13fd40). `just strict`: the pareto2/
  invpareto transfer-flag replay rule still expected certified=false on
  constant samples (8 NEW, rule fixed); rerun clean, known list still empty.
  `just cpp-test` 136/136; `just cpp-test-no-long-double` 181/181;
  `just test-full` `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 67528 ]`;
  `just js-test` 599/599; `just relations` and `just site-check` green.
- Batch check of master 19b2fb7b (2026-11-25, workstation, GCC 15.2, after
  [select-cond](research/evaluations/2026-11-24-select-cond-report.md),
  TASK-239, the 1.x `attr(support, "type")` shim (68550caa), the
  cert-replay transfer-rule fix and the revdep PR proposals): no record moves
  in any snapshot mode. `just strict`: nothing new, known list still empty.
  `just cpp-test` 136/136; `just cpp-test-no-long-double` 181/181;
  `just test-full` `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 67733 ]`;
  `just js-test` 599/599; `just relations` and `just site-check` green.
  Merged afterwards, documents only: five math reports of 2026-11-25
  ([transfers](research/evaluations/2026-11-25-transfer-math-report.md),
  [uniqueness from certificates](research/evaluations/2026-11-25-uniq-cover-math-report.md),
  [skew-normal](research/evaluations/2026-11-25-sn-math-report.md),
  [Student t](research/evaluations/2026-11-25-tcert-math-report.md),
  [Tukey lambda roots](research/evaluations/2026-11-25-tlroot-math-report.md);
  TASK-249 to TASK-253) and the bifrost PR proposal (TASK-247).
- GitHub Actions run only on manual dispatch since 2026-11-06 (quota spent);
  there is no macOS or Windows result for anything merged after that date.
- simbox was unreachable on 2026-11-12 and 2026-11-13; the jobs of those
  days were validated on the workstation only. `dev/tools/simbox-test.sh <label> [tree]`
  runs the suite there when it is back.
- Known wrong answers on master: none listed by the strict layer; its known
  list is empty since 2026-11-24 (batches 15 to 17). Every `unique = FALSE` fit
  carries a typed maximizer set (rule `unique-false-needs-set`); fits whose
  maximizer count is not proved report `unique = NA` with candidates.
  Exception found 2026-11-25 (TASK-254): free sn and snorm fits report
  `unique = TRUE` on samples with an exact reflected global tie, e.g. sn on
  (-1, 0, 0, 0, 0, 0, 1) ([proof](research/evaluations/2026-11-25-sn-math-report.md));
  fixed in a1b53b58. `confint` gives percentile intervals for uniform, Pareto
  and power endpoints that exclude the true endpoint by construction (zero
  coverage, [proof and simulations](research/evaluations/2026-11-25-infer-math-report.md);
  confirmed on master; TASK-269). Open,
  flagged rather than wrong: fpareto archive 001625/001628 end `unresolved`;
  they are now proved to have no finite MLE (inverse transformed-gamma limit,
  [report](research/evaluations/2026-11-25-fpareto-math-report.md)), and the
  fitter change is a To Do card. The sged held-sd two-point location is solved
  in closed form since TASK-150.9
  ([check](research/evaluations/2026-11-25-sstdsged-math-report.md)).
- Nothing is published to CRAN, and `origin/master` is the old published
  tree (about 1,300 commits behind). The backup branch `origin/cpp-core`
  mirrors master; pushed by Claude after each validated batch (allowed by Jonas, 2026-11-20).

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
