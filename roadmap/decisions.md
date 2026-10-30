# Decision log

Dated project decisions: what was decided, why, and where the evidence is.
Newest first. A decision that replaces an earlier one says so; the earlier
entry stays, marked superseded. Design detail lives in the linked roadmap
entries, reports and code; this file is the index of what was chosen.
Active work lives on the board (`~/boards/univariateML`), not here.

## 2026-10-30

- **No densities are exported.** The package exports fitters,
  `dml`/`pml`/`qml`/`rml`, model selection, bootstrap and plots. Every
  d/p/q/r is internal, including the 18 families that had exported d/p/q/r
  before any release (v1.5.0 exported none). A companion density package
  remains possible later. Supersedes the 2026-10-29 state "18 own families
  exported". Evidence: `roadmap/2026-10-28-r-package-scope-revisited.md`,
  merge 8471607.
- **Frozen distributions through the fitters** (agreed direction, TASK-99).
  Without data nothing is estimated and unspecified coefficients take their
  README `simulate:` values (`mlgamma()`, `mlgamma(shape = 3)`); with every
  coefficient held and data, `logLik` is evaluated at those values with
  df = 0. Replaces `simulate_default` (never released) and
  `univariateML:::` in examples. Design:
  `roadmap/2026-10-30-frozen-distributions.md`.
- **Examples** use real data only where the family is a plausible model;
  otherwise they simulate from a frozen model. Every README has
  `examples:`. Part of TASK-99.
- **extraDistr and actuar dropped** (TASK-78.9, merge 8471607). The 66
  families that used them run on the package's own generated d/p/q/r;
  runtime repair registries deleted; Imports is Rcpp only. Defects found
  in those packages stay on record in `KNOWN_ISSUES.md` and
  `notes/upstream/`. Supersedes `roadmap/2026-10-29-reference-repairs.md`.
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
  `notes/evaluations/2026-10-30-switchfix-multistart-report.md`,
  `...-bounds-report.md`.
- **Attained boundary parameters and R7a endpoints are exact in d/p/q/r.**
  Any value a fit can return (closed domain ends, holds, attained limit
  laws) evaluates the limiting law in closed form, as do densities at
  parameter-dependent support endpoints. Truth tables cover interior
  parameters only, so these cases carry closed-form doctests. Evidence:
  `notes/evaluations/2026-10-30-bounds-report.md`, merge d32bc62.
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
  `not-implemented`. Evidence: `notes/evaluations/2026-10-29-p2b-*-report.md`,
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
  `notes/evaluations/2026-10-29-task75-report.md`.
- **Backup branch `cpp-core` on origin** (Jonas, 2026-10-30): local master
  is mirrored with `git push origin master:refs/heads/cpp-core`; CI runs
  only on `main`/`master` and PRs into them, so the branch triggers none.
  Pushing `master` or publishing stays gated on an explicit request.
  Merged and stale remote branches (actions, cran-release, devel,
  more-dist, probability-plots) were deleted; local copies are under
  `refs/archive/*`.
- **Codex model for agent jobs**: gpt-6.1-terra at high effort for work
  that is mathematics plus implementation (existence proofs, limit faces,
  stopping rules, documentation needing judgement); gpt-6.1-sol at medium
  for plumbing. Evidence: four TASK-84/97 terra jobs finished without
  resumes and kept uncertified cases open; sol (P2b, P4, TASK-94) was
  reliable and more ready to accept documented numerical searches.
- **Package structure and documentation plan banked** as a direction, with
  implementation after the density and parameter work:
  `roadmap/2026-10-30-package-and-documentation.md`; inference regularity
  plan `roadmap/2026-10-30-inference-and-dqm.md`.

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
  `notes/evaluations/2026-10-29-dpqr-truth/README.md`.
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
  `roadmap/2026-10-29-parameter-rules.md` (board TASK-79 to TASK-86).
- **Density coverage and exclusions**: `roadmap/2026-10-29-density-scope.md`.
- **Suggests trimmed** to jsonlite, testthat, knitr, rmarkdown; fGarch,
  logitnorm, nakagami and sads replaced by own d/p/q/r (TASK-76).

## Earlier

- **D1–D7** (escape policy, per-distribution layout, R argument names,
  cross-machine goldens, Nakagami shape, GED/SGED shape bound, mlhnorm and
  mllst): board TASK-6 to TASK-10, TASK-17, TASK-25; analysis in
  `notes/ml-behaviour.tex`. D4 (golden tolerance 1e-6) is superseded by
  the 2026-10-30 tolerance rule; D6 is now part of R5; D7's hnorm mapping
  (`extraDistr::dtnorm`) is superseded by C5 and the own d/p/q/r.
- **R argument names follow CRAN** (`reltol`, `abstol`, `gradtol`,
  `iterlim`, `<param>0`): `roadmap/2026-10-28-cran-argument-names.md`.
- **One folder per distribution**:
  `roadmap/2026-10-28-per-distribution-layout.md`.
- **C++ core as source of truth**:
  `roadmap/2026-05-08-cpp-core-source-of-truth.md`.
