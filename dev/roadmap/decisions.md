# Decision log

Dated project decisions: what was decided, why, and where the evidence is.
Newest first. A decision that replaces an earlier one says so; the earlier
entry stays, marked superseded. Design detail lives in the linked roadmap
entries, reports and code; this file is the index of what was chosen.
Active work lives on the board (`~/boards/univariateML`), not here.

## 2026-11-14

- **`logLik` is the log likelihood at the returned coefficients** (Jonas:
  "sounds reasonable ... so we can use your suggestion for this version, at
  least"). When the exact maximizer is not representable, the returned point
  can lie measurably below the supremum: for the normal on `(1, 1, 1 + 2^-52)`
  the mean rounds by `delta = -2^-52/3`, and the returned point is
  `n delta^2 / (2 sd^2) = 0.75` below it (117.86 against 118.61). `logLik`
  reports the value at the returned point, so `logLik(fit)` equals the summed
  `dml(x, fit, log = TRUE)` and the strict layer's density check applies.
  This narrows the contract's "maximized value" rule; its documented cusp and
  endpoint allowances (SGED, GED) stand until reviewed. Alternative considered: report the supremum (right for model
  selection, but not reproducible from `coef()`); it can return later as a
  separate field if model selection needs it. It matters only for data whose
  relative spread is near `1e-11` or below. Rule:
  [accuracy contract](../design/accuracy-contract.md#log-likelihood);
  families that break it today: TASK-214.15 (norm in branch
  `claude/norm-loglik`, unfinished).

## 2026-11-13

- **The family status enums stay, as family detail under the shared outcome**
  (Jonas: "lets keep in"). Each fitter keeps its `<Id>Status` and gains an
  `outcome()` mapping to the shared type; bindings and generic code read only
  the outcome. This amends "it replaces the per-family status enums" of
  2026-11-12: the outcome is the one cross-family vocabulary, and the enum is
  the family's own explanation of how it got there. Reason: about 34,000 lines
  of C++ tests assert on the enums, and a status such as `ScaleCollapse` says
  more than `Nonexistence(Unbounded)`. Cost accepted: an enumerator can still
  be mapped to the wrong outcome, which the snapshot's `--outcomes` check and,
  later, the strict layer have to catch.
- **The draft kinds and reasons of `core/outcome.hpp` are accepted** (Jonas:
  "lets keep"): five kinds, the reasons of the pilot, flags `certified` and
  `unique`.
- **The 16 retiring families are removed before the outcome mapping reaches
  them** (Jonas: "remove first"). Replacement calls and the removal list:
  [modifiers-2 report](../research/evaluations/2026-11-12-modifiers-2-report.md).
- **`zero` and `truncate` become explicit formals** of the generated fitters
  (`zero = "none"`, `truncate = NULL`) instead of entries in `...` (Jonas:
  "explict entries i think, breaking changes are fine"). Done with the uniform
  controls, step 5 of the [consolidation plan](../design/consolidation.md).
- **q-Gaussian adopted as proposed; `lst` stays** (Jonas: "we keep the lst
  and adopt this"; not high priority). Coefficients `(mu, sigma, eta)` with
  `eta` the signed inverse degrees of freedom (normal at 0, Student t with
  `df = 1/eta` above, Pearson II below, uniform at -1), not Tsallis `q`; id
  `qgauss`, quantile `qqgauss`; all-free domain `-1 <= eta <= 1` (below -1
  endpoint spikes make the likelihood unbounded; above 1 the Student scale
  collapses, exempt when `mu` or `sigma` is held); no restriction at `-1/3`.
  `lst` remains its own family although it is `qgauss` with `eta > 0`
  held. Analysis: [qgauss theory](../research/evaluations/2026-11-12-qgauss-theory-report.md).
  Work (TASK-202, TASK-203) waits for step 8 of the consolidation plan.
- **Discrete quantiles resolve ties with the left-continuity fuzz of `stats`**
  (Jonas: "lets do 1"): the smallest support point whose computed tail reaches
  `p` reduced by a relative `64 * DBL_EPSILON`. A `p` inside that band above a
  jump returns the lower value, as in `stats`. Alternatives considered:
  comparing on the scale `p` was given (the round trip holds, exact ties stay
  wrong), and a band equal to each CDF's error bound (narrowest, but needs a
  bound per family). Evidence:
  [quantile ties](../research/evaluations/2026-11-13-discrete-quantile-ties.md).
  Not implemented yet (TASK-215).

## 2026-11-12

- **Consolidation before breadth** (Jonas: "cleanup first, then add fuzzers,
  then have two layers of testing while developing"; "We can hold off (at
  least partly!) new breadth"). Order amended in discussion: a snapshot fuzzer
  comes before the cross-family refactor, as its safety net. The stricter
  layer keeps a list of known failures that may only shrink. Plan and steps:
  [consolidation](../design/consolidation.md); evidence:
  [repository audit](../research/evaluations/2026-11-12-repo-audit-report.md).
  The entries below are targets; none is implemented yet.
- **One outcome type for every family** (Jonas: "I strongly agree enum output
  should be the same"). It replaces the per-family status enums and lives in
  the core, so classification no longer happens in the bindings.
  `AGENTS.md` Principle 1 now lists the outcomes, including the uncertified
  estimate of the 2026-11-09 decision.
- **A limit decided only by the margin is a limit stop with
  `certified = FALSE`** (Jonas: "i dont know", then "lets go" on the
  recommendation). Its message says that no improvement above the margin was
  found, not that none exists. Alternative considered: an "unresolved" stop,
  rejected because naming the limit model is the useful answer when the gain
  is below anything a likelihood-ratio test could detect.
- **Proved non-unique maxima are estimates with `unique = FALSE`, returned in
  full** (Jonas: "shouldn't we return all with a flag?"). A finite set is
  listed, a continuum described; each family states its representative for
  `coef()`; `vcov` and `confint` refuse. Supersedes the tie stops of SGED,
  ZINB and bnbinom ("proved distinct tied maxima still stops", 2026-11-09).
  Candidates a certificate cannot separate remain the uncertified case.
- **`tol` is a ceiling on the coefficient error** (Jonas: "this is kinda
  arbitrary and much stricter than what im used to"). A fitter may deliver
  more accuracy than asked, so clamping is within the contract. The default
  stays `1e-10` and is revisited when the strict layer shows how often default
  fits stop on tolerance or conditioning.
- **Pathological families use the shared outcome type; no mixtures for now**
  (Jonas: "Its probably cleanest to do without mixtures"). Families where ML
  fails for every sample, such as the sine family, are added after the outcome
  type as its acceptance test and as teaching examples.
- **PGW is ordinary PGW** (Jonas: "Going with recommendations"). Id `pgw`,
  coefficients `scale`, `shape`, `exponent`, all positive, with
  $H(x)=\{1+(x/a)^b\}^c-1$. Adapted PGW is a different family and, for
  negative `kappa`, defective; not added. Evidence:
  [pgw-theory report](../research/evaluations/2026-11-11-pgw-theory-report.md);
  work: TASK-182, TASK-183.
- **Underflowed natural outputs round** (Jonas: "Going with recommendations").
  Natural densities and probabilities below the normal range return the
  correctly rounded subnormal or zero, exempt from the relative requirement;
  log outputs keep it. Design:
  [accuracy contract](../design/accuracy-contract.md#dpq); TASK-199.
- **R evaluates all families through the generated d/p/q** (Jonas: "we go
  with routing, the rule was made under assumptions"). `dml`, `pml` and `qml`
  of the 14 families that called `stats` move to the C++ d/p/q, so one
  contract covers every family and `logLik` agrees with the summed `dml`.
  Supersedes the 2026-10-30 rule that kept `stats` for those families.
  Evidence: [dpqr-extreme](../research/evaluations/2026-11-11-dpqr-extreme-report.md)
  (386 `stats` misses), seam check (`stats::dbeta` relative 1.9e-7 at an
  archived fit); work: TASK-198.
- **Volkov is out of scope** (Jonas: "Yeah, lets bank volkov out"). The
  neutral-theory likelihood is a joint law for one community's abundance
  vector (it sums to $J$), not an iid univariate family; GUILDS (Etienne's
  sampling formula, `maxLikelihood.ESF`) and untb serve it. MZSM stays
  banked at low priority as an iid model whose dossier must call the fit a
  composite approximation. Reasons and the unresolved $m\to1$ discrepancy:
  [MZSM and Volkov](2026-11-12-mzsm-volkov.md#volkov-neutral-local-community-with-immigration).
- **Family coverage: parity with extraDistr, actuar and flexsurv** (Jonas:
  "we want parity with it for eligible distros i think? along with actuar and
  flexsurv?"). Every in-scope univariate family of the three is included;
  aliases and reparameterizations map to an existing family, truncated and
  zero-modified variants are modifiers. Checked 2026-11-12 (extraDistr,
  actuar 3.3.7, flexsurv): parity holds. The mapping is in the
  [new-distributions roadmap](2026-10-28-new-distributions.md#reference-package-parity).
- **Defective distributions are out of scope** (Jonas: "yeah, this sounds out
  of scope"). Members with mass at infinity, such as flexsurv's Gompertz with
  negative shape, are not supported; our gompertz keeps both coefficients
  positive and escapes to the exponential.
  When the likelihood of a proper family is maximized only along a
  defective limit (possible with complete data: Geyer 1990, Examples
  4.4-4.5, [TASK-171](../research/evaluations/2026-11-10-defective-limit-lit-report.md)),
  the fit stops with a nonexistence error that names the escaping mass; it
  never names a defective model to use instead (Jonas: "we would rather have
  errors with explanation").
- **Retiring families go as soon as their replacement lands** (Jonas: "we'll
  remove all redundant truncated and zero-mod/trunc/infl distributions once
  they land"). Each of the 16 families in
  [family-reductions.csv](../validation/family-reductions.csv) is removed once
  `zero`/`truncate` on its base reproduces it in the retirement audit, not
  held back for a single 2.0 cut. The half families (hnorm, hcauchy, ht) and
  other families that are not windows of a parent stay. Design:
  [modifiers roadmap](2026-11-11-zero-and-truncation-modifiers.md); work:
  TASK-191.

## 2026-11-11

- **`logLik` is the maximized value** (Jonas: "Lets do option A"; and a
  general rule). At a non-Lipschitz maximizer (a power cusp below one, an
  endpoint reached through transformed coefficients) no representable
  coefficients attain the supremum; `logLik` reports the supremum, the
  summed density at the returned coefficients is below it by a dossier-stated
  allowance, and seam checks there are one-sided. Elsewhere `logLik` agrees
  with the summed density to `1e-10`. Design:
  [accuracy contract](../design/accuracy-contract.md#log-likelihood).
- **Two accuracy-failure kinds** (Jonas: "sounds reasonable"). A `tol`
  below the binary64 spacing at the maximizer is a `tolerance` stop; noise
  over curvature above a representable `tol` is a `conditioning` stop. One
  shared classification replaces the per-family `AccuracyFailure`,
  `accuracy_failure` flag and `NumericalFailure` variants; R keeps
  `univariateML_numerical` as their parent class. Design:
  [accuracy contract](../design/accuracy-contract.md#contract); work:
  TASK-150.4, sequenced with TASK-150.1.
- **d/p/q accuracy over the whole parameter domain** (Jonas: "lets go").
  Every `dpqr.hpp` function meets the truth tables' tolerances for the exact
  binary64 inputs at every parameter value, not only at the tabulated points,
  or returns `NaN` in a region its dossier names. Forward accuracy was chosen
  over a conditioning-relative bound because fitted parameters are exact
  doubles. A quantile also passes if its probability matches the input to
  relative `1e-12` on the input's own tail and scale (Jonas: "sure, ok"), which
  covers quantiles that cancel to nearly zero. Design:
  [accuracy contract](../design/accuracy-contract.md#dpq); work: TASK-189,
  Fréchet TASK-187.
- **Uncertified fits meet the accuracy contract** (Jonas: "lets go with a").
  `certified = FALSE` withdraws only the global-optimality claim. Every `Ok`
  fit, certified or not, is within `tol` of a local maximizer, justified by its
  stopping rule; fallback searches get a contract-conforming final step, and a
  fit whose `tol` is unreachable reports a numerical-accuracy failure. Design:
  [accuracy contract](../design/accuracy-contract.md); work: TASK-150.2.5.

## 2026-11-09

- **Uncertified fallback instead of refusals** (Jonas). When a certificate
  cannot close a case on ordinary data (an unproved face or candidate
  comparison, an unresolved sign, an exhausted certificate allowance), the fit
  returns the search's successful result marked uncertified (C++ `certified`,
  R logical attribute, JS 0/1). A fit stops only when the certificate proves
  that result wrong, the search itself fails, or the mathematics proves a stop
  (winning limit model, nonexistence, exact tie). "Legacy carries no weight"
  means legacy results need not be kept as golden truth, not that they may be
  replaced by refusals. Existing tests are not deleted or rewritten to fit new
  stops. Families written before this rule are converted on the board
  (TASK-168).

## 2026-11-08

- **Discrete quantiles beyond 2^53 return the nearest representable value**
  (Jonas: "Lets choose B"): choose the smallest double reaching the target CDF;
  evidence: [qdiscrete-round report](../research/evaluations/2026-11-08-qdiscrete-round-report.md).

## 2026-11-07

- **Tukey lambda: no further certification or speed work** (Jonas: "lets keep
  them won't do, yeah. very low priority"). ML for the symmetric Tukey lambda
  is rarely used; the family stays as it is (`certified: partial`). Open items
  are listed on the board (TASK-151); evidence in the
  [tlambda-impl report](../research/evaluations/2026-11-07-tlambda-impl-report.md).
- **One accuracy contract and uniform controls for 2.0** (Jonas: "we should
  aim for the same sort of tolerance across everything, compatibility is not
  important. this is a version bump++"). Every `Ok` fit is within relative
  `1e-10` of a maximizer in each free coefficient, justified by its stopping
  rule; every fitter takes only `tol` and `iterlim`. Supersedes D3 (CRAN
  argument names, 2026-10-28). Design: [accuracy contract](../design/accuracy-contract.md).
- **Agent instructions** (Jonas: "i dont want old stuff there, and this has
  clearly drifted"): one lean `AGENTS.md` replaces `CLAUDE.md` and the old
  `AGENTS.md`. It states the purpose (a proof of concept for solid maximum
  likelihood) and four principles: no silent failures, fitters follow the
  likelihood's structure, legacy carries no weight, claims name their
  evidence. Procedures moved to the dossier schema and validation guide
  (TASK-147).
- **Original univariateML algorithms carry no weight.** Goldens no longer
  justify keeping inherited algorithms; the beta free fit was rebuilt after it
  failed silently (TASK-148).
- **Goldens are frozen samples with independent references** (Jonas chose all
  three options; TASK-149). (1) Existing samples are frozen fixtures, decoupled
  from README `simulate` (76 of 99 had been drawn at older values); `just
  goldens` only creates missing ones. (2) Stored estimates are independent
  50-digit MLEs from the README density (`just golden-reference`), with local
  and global evidence, so fitters are tested against correct values and
  improvements need no regeneration. (3) Stress cases from the fit audit
  (TASK-101.7.1) join `golden.json` as a `cases` list.

## 2026-10-31

- **Test tiers** (Jonas: "we dont need supertesting all the time"): R and
  C++ tests run in tiers. `cran` (R CMD check without NOT_CRAN: API, one
  golden fit per family, sampled truth rows, about a minute); `dev`, the
  default for `just test` and agent jobs (all goldens, conformance on
  core held subsets, more truth rows; 1-2 minutes with four workers);
  `full` (`just test-full`, before merges and releases: everything); audits
  on demand (quality, no-long-double, standalone check, relations,
  dpqr-truth). Tiers follow measured cost. testthat runs in parallel and
  the C++ doctest cases run through ctest in parallel.
- **Family taxonomy before theory articles** (Jonas: organisation and
  classification first; "make something plausible"): families are tagged by
  support, coefficients and existence profile (derived), and by group,
  construction, relations, fitting method and certainty, and application
  areas (written, controlled vocabularies). Special-case and
  transformation relations are checked symbolically. The theory articles
  (TASK-101.6.3) wait and will explain these tags. Plan:
  `dev/roadmap/2026-10-31-family-taxonomy.md`.

## 2026-10-30

- **Website and JS binding** (Jonas set the goals and left the machinery
  to Claude): the JS library is the C++ core compiled to WebAssembly
  (Emscripten, `bindings/js/`), checked by the same goldens and truth
  tables; the site is an Astro project in `site/` reading the README
  dossiers directly, with Svelte islands for interactivity and MathML at
  build time. Family pages are micropapers with generated notices, usage
  tabs for R, C++, Python and JS, and folded implementation detail and
  decision history. Supersedes the Quarto preference. Plan and staging:
  `dev/roadmap/2026-10-30-site-and-js-binding.md`.
- **Dossier prose schema adopted** (Jonas; TASK-101.2): family READMEs
  carry the fitting method in `## Details` and an optional
  `## Decision history`, which stays source-only (dossier and future
  site, not R help). Pilot: exp, gamma, gb2; proposal
  `dev/research/evaluations/2026-10-30-dossier-schema-proposal.md`; schema in
  `README_SCHEMA.md` ("Prose"). The per-family classification is finished
  while filling dossiers (TASK-101.6) and the numerical inventory before
  the simplification reviews (TASK-101.7).
- **Migration layout** (TASK-101.3; Jonas: "do what you think fits the
  vision best"): the plan's layout. `univariateML-cpp/` is the C++ library
  (include paths and namespace stay `cppunivariate/`); each family folder
  keeps the canonical dossier, `mle.hpp`, `dpqr.hpp`, `test.cpp`, golden
  and truth table. R adapters and R tests move to
  `bindings/r/distributions/<id>/`, with the authored R sources in
  `bindings/r/`; the installable package is generated into the ignored
  `univariateML-r/`. Status, roadmap, research notes, validation harness,
  tools and the logo sources move under `dev/`. Why: a standalone C++
  export cannot carry Rcpp adapters or testthat files, and a Python
  binding will mirror `bindings/r/`. Per-family folders survive in each
  layer, so D2 holds per layer.

- **User-facing defaults are standard members** (TASK-104): README
  `simulate:` values, which `mlX()` uses for unspecified coefficients,
  follow the scipy/distributions3 convention: location 0, scale 1, and the
  standard or symmetric member where a family has one (GEV/GPD $\xi = 0$,
  skew-normal $\alpha = 0$, Fernandez–Steel $\xi = 1$, GED $\nu = 2$);
  otherwise a simple interior value. Because such members are often special
  code branches, truth-table sampler checks also run at the golden-fit
  point, a generic interior point. Named simulation scenarios can be added
  later as a README list if needed.
- **No densities are exported.** The package exports fitters,
  `dml`/`pml`/`qml`/`rml`, model selection, bootstrap and plots. Every
  d/p/q/r is internal, including the 18 families that had exported d/p/q/r
  before any release (v1.5.0 exported none). A companion density package
  remains possible later. Supersedes the 2026-10-29 state "18 own families
  exported". Evidence: `dev/roadmap/2026-10-28-r-package-scope-revisited.md`,
  merge 8471607.
- **Frozen distributions through the fitters** (agreed direction, TASK-99).
  Without data nothing is estimated and unspecified coefficients take their
  README `simulate:` values (`mlgamma()`, `mlgamma(shape = 3)`); with every
  coefficient held and data, `logLik` is evaluated at those values with
  df = 0. Replaces `simulate_default` (never released) and
  `univariateML:::` in examples. Design:
  `dev/roadmap/2026-10-30-frozen-distributions.md`.
- **Examples** use real data only where the family is a plausible model;
  otherwise they simulate from a frozen model. Every README has
  `examples:`. Part of TASK-99.
- **extraDistr and actuar dropped** (TASK-78.9, merge 8471607). The 66
  families that used them run on the package's own generated d/p/q/r;
  runtime repair registries deleted; Imports is Rcpp only. Defects found
  in those packages stay on record in `dev/KNOWN_ISSUES.md` and
  `dev/research/upstream/`. Supersedes `dev/roadmap/2026-10-29-reference-repairs.md`.
- **logitnorm's `...` dropped** from its d/p/q/r: the functions were never
  released, and `...` swallowed misspelt arguments.
- **Generated d/p/q/r entrypoints share each family's adapter translation
  unit** at assembly (`src/dist_<id>.cpp`); repository sources stay
  separate files (Jonas: separate files, merged before compiling if
  needed). Library headers come before `Rcpp.h`, because `Rmath.h`
  #defines names such as `pnorm` and `log1mexp`. Evidence: clean
  single-core install 546 s with 184 translation units, 378 s with 100
  (simbox, no ccache); commits 4b5f60f, a7cd7d0.
- **Multistart selection follows R6** (`core/multistart.hpp`): a candidate
  counts only if it converged or its total free-coordinate score norm is
  below 1e-3; the best objective among those wins. Applied to every BFGS
  and Nelder–Mead multistart (burr/invburr, betapr, lst, std, ged, emg,
  bhatt, bnbinom, sstd, sged). Evidence: `mlinvburr` failed on a well-posed
  sample (seed 3, n = 200); reports
  `dev/research/evaluations/2026-10-30-switchfix-multistart-report.md`,
  `...-bounds-report.md`.
- **Attained boundary parameters and R7a endpoints are exact in d/p/q/r.**
  Any value a fit can return (closed domain ends, holds, attained limit
  laws) evaluates the limiting law in closed form, as do densities at
  parameter-dependent support endpoints. Truth tables cover interior
  parameters only, so these cases carry closed-form doctests. Evidence:
  `dev/research/evaluations/2026-10-30-bounds-report.md`, merge d32bc62.
- **Fixed, finite parameter lists only**: the vector-parameter tier (dcat,
  dmixpois, dmixnorm) is dropped (commit 8c800e5).
- **Hold exceptions are kept, not guessed.** A hold subset is implemented
  only with a proved existence result and stopping rule, or with a
  documented numerical search that stops with a status when its budget
  runs out (never a capped or unchecked estimate). Otherwise it stays
  `not-implemented` with the precise blocker in `holds:`; `no-mle` needs a
  proof for every sample. Reports must separate proved from numerical
  results. Four P4 `no-mle` labels (hyper {}, {k}; nhyper {}, {r}) were
  wrong, since constant samples attain likelihood one, and became
  `not-implemented`. Evidence: `dev/research/evaluations/2026-10-29-p2b-*-report.md`,
  `2026-10-30-holds-{bbinom,cmb,hyper}-report.md` (TASK-93, TASK-97).
- **Parameter rules P0–P5 are implemented** (TASK-79 to TASK-84, TASK-93,
  TASK-97). P4 (merge d10eb61) made C4 to C6 concrete: `scale` instead of
  `rate` for gamma, invexp, invweibull and llogis; hnorm, hcauchy and ht
  translated by a required threshold `mu` through `r.shift`; hyper and
  nhyper report the population size `N` as a design coefficient. C5 and
  C6 had been picked in the design; Jonas approved P4 as written. Holds
  left: 15 `not-implemented` (TASK-98 and later cards), 60 `no-mle`.
- **C++ d/p/q/r take the README coefficients in README order**
  (`codegen_dpqr.R` checks every family; TASK-94 aligned the P4 families,
  merge 387dd05).
- **Golden tolerances follow the fitter's own convergence tolerance**
  (TASK-75, 7e6af75): finite-difference BFGS fitters compare at 3e-5,
  about three times the largest observed cross-machine drift (2.5e-6 in
  C++, 9.4e-6 in R, simbox against local), with the fitters' gradtol
  1.22e-4 as the upper bound; frozen-reference quantiles at 1e-9; status
  fixtures keep margin. Supersedes D4 (1e-6). Confirmed on simbox: no
  machine-dependent failures remain. Evidence:
  `dev/research/evaluations/2026-10-29-task75-report.md`.
- **Backup branch `cpp-core` on origin** (Jonas, 2026-10-30): local master
  is mirrored with `git push origin master:refs/heads/cpp-core`; CI runs
  only on `main`/`master` and PRs into them, so the branch triggers none.
  Pushing `master` or publishing stays gated on an explicit request.
  Merged and stale remote branches (actions, cran-release, devel,
  more-dist, probability-plots) were deleted; local copies are under
  `refs/archive/*`.
- **Codex model for agent jobs** (Jonas, 2026-10-30): gpt-6.1-sol at low
  effort, or gpt-6.1-terra at low effort, for everything not expected to be
  genuinely hard; gpt-6.1-terra at high effort only for work that is
  genuinely hard and needs substantial judgement (existence proofs, limit
  faces, stopping rules, documentation that needs judgement). Evidence for
  terra high on such work: four TASK-84/97 jobs finished without resumes
  and kept uncertified cases open; sol (P2b, P4, TASK-94, TASK-78) was
  reliable on implementation and plumbing.
- **Package structure and documentation plan banked** as a direction, with
  implementation after the density and parameter work:
  `dev/roadmap/2026-10-30-package-and-documentation.md`; inference regularity
  plan `dev/roadmap/2026-10-30-inference-and-dqm.md`.

## 2026-10-29

- **Own d/p/q/r for every family, in C++**: implement and fix the
  densities and remove external dependencies first; whether to offer a
  density layer was left for later (closed on 2026-10-30, above). Board
  TASK-78.
- **Families on stats stay on stats** (14). Not banked: this can change
  with the project's direction (an exported density layer, the Python
  shell, one parametrization source).
- **New R d/p/q/r are internal**; the namespace does not grow.
- **Test oracle: high-precision truth tables**, computed with mpmath from
  each README's `log_density` (`distributions/<id>/truth.json`,
  `just dpqr-truth <id>`), not the former reference packages. Checked from
  C++ (`mltest::check_dpqr_truth`) and R (`test-generated-dpqr.R`).
  Tolerances: 1e-10 on the log scale for d and p, 1e-9 relative for q,
  adjacent-double brackets accepted. Design:
  `dev/research/evaluations/2026-10-29-dpqr-truth/README.md`.
- **Numerical contract for d/p/q** (`special/tails.hpp`): compute the
  smaller tail in log space and complement with `log1mexp`; quantiles from
  the accurate log tail; log-probability input to -1e4 and beyond;
  endpoints exact at p = 0 and 1. Double precision only, with range
  guarded by log-space algebra; `long double` may add precision but never
  range, because it equals `double` on some CRAN platforms.
- **Discrete conventions**: p uses the floor of its argument; q is the
  smallest support point with $P(X \le x) \ge u$ (lower tail) or
  $P(X > x) \le u$ (upper tail), on the log scale where needed; a
  non-integer x has density zero, with a warning in R.
- **Iterative failure is an error** (R6, A6; CLAUDE.md "Wrapper failure
  handling", bdd7735): a failed solve is kept only when its free-coordinate
  score norm is below 1e-3 or passes the family's documented gate; invalid
  starts and nonfinite results stop; a rescue must meet its own criterion.
  Replaces the nlminb-style rule that let `mlstd` return its start values
  (TASK-77).
- **Parameter rules R1–R8 and amendments A1–A7**:
  `dev/roadmap/2026-10-29-parameter-rules.md` (board TASK-79 to TASK-86).
- **Density coverage and exclusions**: `dev/roadmap/2026-10-29-density-scope.md`.
- **Suggests trimmed** to jsonlite, testthat, knitr, rmarkdown; fGarch,
  logitnorm, nakagami and sads replaced by own d/p/q/r (TASK-76).

## Earlier

- **D1–D7** (escape policy, per-distribution layout, R argument names,
  cross-machine goldens, Nakagami shape, GED/SGED shape bound, mlhnorm and
  mllst): board TASK-6 to TASK-10, TASK-17, TASK-25; analysis in
  `dev/research/ml-behaviour.tex`. D4 (golden tolerance 1e-6) is superseded by
  the 2026-10-30 tolerance rule; D6 is now part of R5; D7's hnorm mapping
  (`extraDistr::dtnorm`) is superseded by C5 and the own d/p/q/r.
- **R argument names follow CRAN** (`reltol`, `abstol`, `gradtol`,
  `iterlim`, `<param>0`): `dev/roadmap/2026-10-28-cran-argument-names.md`.
- **One folder per distribution**:
  `dev/roadmap/2026-10-28-per-distribution-layout.md`.
- **C++ core as source of truth**:
  `dev/roadmap/2026-05-08-cpp-core-source-of-truth.md`.
