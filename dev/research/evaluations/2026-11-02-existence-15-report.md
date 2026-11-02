# Existence-15 report — TASK-101.6.4.1

Completed 2026-11-02 in `existence-15`, branch `codex/existence-15`.
Tested source: base revision `9578745158d3d15c210707f34a7ce83f37b4296e`
plus the uncommitted changes to the seven family dossiers listed below.
No fitting code, default fit, golden, status, held set, parameter domain,
limit firmness/numerical flag, or other protected metadata changed.
No commits, board edits, dependency installations, sub-agents or simbox runs.

Each dossier now contains `existence`, revised limit sentences and a reader-facing
prose pass. R-specific material is under `R notes`; implementation history is
separated from the statistical explanation. Existing probability-function
contracts and numerical search limitations were retained. All seven have empty
`holds` and no `advice` or `fit_domain` reasons needing revision. Consequently,
no binding messages or R test regexes needed changes. No new external citation
is used for an existence or uniqueness claim; the arguments are given directly.
Existing bibliographies were left unchanged. Web searches used: zero.

## Family results

The statements assume a nonempty sample on the model support and
support-compatible held coefficients. A proved uniqueness label can include
proved non-identification in explicitly stated exceptional cases; it does not
assert a unique complete coefficient vector for every sample.

| Family | Statement in one line | Grade and argument | Status change |
|---|---|---|---|
| `tpois` | All-free fits attain a maximum; nonconstant data identify the rate on the minimal integer window, while real bounds and singleton-support rates are nonunique; held windows have the conditional-mean criterion and stated endpoint exceptions. | **Proved.** Removing unobserved support increases likelihood. The mean derivative in log rate is the positive conditional variance, giving strict concavity and exactly one interior root. Zero is attained only when retained; positive lower atoms and finite upper atoms of nonsingleton windows are excluded limits. | None. |
| `tbinom` | General joint free-size/free-probability existence and uniqueness remain unclassified; fixed size has a unique interior probability root, and held interior probability has a finite integer maximum outside the specified upper-truncation escape. | **Unknown jointly; proved conditional statements.** Fixed-size curvature in logit probability is minus sample size times conditional variance. With interior probability held, the integer tail tends to the upper truncation atom, or leaves every finite sample when there is no upper bound. A positive finite likelihood with a vanishing tail attains an integer maximum. Constant data attain likelihood one with free size/probability. Two-point windows attain their saturated likelihood, as retained in the implementation history. | None. |
| `bnbinom` | Joint existence has no asserted general data criterion; exchanging size and beta defeats labeled uniqueness off the diagonal; all-zero data escape whenever any coefficient is free. | **Unknown generally; proved symmetry and zero-sample results.** Expanding the beta/gamma factors makes the size/beta symmetry explicit. Every finite positive vector has zero-count mass strictly below one; freeing size or beta toward zero, or alpha toward infinity, makes that mass tend to one. Numerical multistart and face comparisons do not prove that all finite maxima were found. | None. |
| `skellam` | A finite maximum exists for every integer sample, also under compatible holds; one free rate has a unique maximum, while general joint uniqueness is unknown. | **Proved existence and conditional uniqueness; unknown joint uniqueness.** Each convolution mass is bounded by the maximal mass of either Poisson factor, tending to zero as that rate diverges. Continuity on the closed quadrant gives compact positive likelihood level sets and attainment. The paired-count variance is strictly below its mean, giving strict single-rate concavity; a held zero rate reduces to a signed Poisson fit. | None. |
| `yulesimon` | A unique finite shape MLE exists iff some observation exceeds one; all ones give an excluded point-mass supremum; a held shape permits likelihood evaluation. | **Proved.** The log-shape score is sample size minus the sum of rho/(rho+j), for j from one through each observed count. It decreases strictly from sample size to sample size minus the count sum, so there is exactly one root outside the all-ones case. | None. |
| `zeta` | A unique finite exponent MLE exists iff some observation exceeds one; all ones give an excluded point-mass supremum; a held exponent permits likelihood evaluation. | **Proved.** The score per observation is model minus empirical mean log count. Its derivative is minus the model variance of log count. The model mean falls from infinity to zero as the exponent runs from one to infinity. | None. |
| `zipf` | All-free fits attain a maximum at the sample maximum endpoint; shape is unique unless that endpoint is one; a held endpoint above one makes all-ones data escape. | **Proved.** The normalizer strictly increases with the endpoint at every finite shape. On support of size greater than one, shape curvature is minus sample size times the positive variance of log count. The uniform boundary is selected when empirical mean log count is at least log(N!)/N; otherwise there is one positive root, except for all-ones data. | None. |

## Consistency findings and open entries

- **`tpois`: clarified, with no unresolved existence claim.** The old generic
  “edge-only data escape” wording omitted attained zero and singleton support.
  The new statement and limit conditions distinguish them. Real lower and upper
  bounds are nonunique even when the retained integer window is determined.
  The existing limit records list `fixed: [b]` and `fixed: [a]`; they are not a
  complete enumeration of held subsets. The same escapes can occur with both
  bounds held, as the new statement explains. Held-set metadata was preserved.
- **`tbinom`: conditional scope corrected; general certification open.** The old
  conditional-mean paragraph read like a joint uniqueness theorem. It now
  explicitly holds size fixed. Point-mass descriptions exclude singleton support
  and distinguish underlying binomial endpoint atoms from zero-probability
  conditioning limits. For held probability and free size, the escape is at the
  upper *truncation* edge, unless a feasible size gives singleton support.
  A full general existence criterion and a proof covering the entire
  free-size/free-probability profile are still missing. The completed finite-window
  tail-cutoff pass supplies the documented certificate conditional on computed
  comparisons and its practical margin; the general fallback and face decision
  remain numerical. The fitter can retain its numerical candidate when the
  extra certification pass exceeds its budget. This gap is in `mle.hpp`, which
  this job is prohibited from changing. It is not a requirement for outward-rounded
  interval arithmetic.
- **`bnbinom`: global classification and conditional uniqueness open.** The
  negative binomial and Poisson paths are proved distributional limits, but the
  numerical comparison does not establish that either is the global sample
  supremum. A complete finite-maximum criterion, exhaustive boundary analysis
  supporting that criterion, and general conditional uniqueness proofs remain
  missing. The size/beta symmetry is an exact obstruction to labeled uniqueness
  whenever a finite maximum is off the diagonal. No ordering is imposed when
  either interchangeable shape is held.
- **`skellam`: joint uniqueness open.** The existing search certificate concerns
  likelihood accuracy, not uniqueness. The compactness proof establishes
  existence independently of that search. No further existence gap was found
  for compatible held rates.
- **`yulesimon`, `zeta`, `zipf`: no open existence or uniqueness question within
  the stated sample and parameter domains.** Their coefficient-held cases and
  attained versus excluded boundary behavior are now explicit. Representable
  quantile grids and the finite integer ceiling for Zipf remain documented
  implementation limits, not mathematical non-existence claims.
- **Generated-help overclaim: open outside the permitted edits.** In
  `bindings/r/scripts/codegen_wrappers.R`, `limit_sentence()` categorically
  introduces records without `numerical: true` as “The maximum likelihood
  estimator does not exist”. The four `tbinom` truncated-Poisson records and the
  `bnbinom` negative-binomial/positive-mean Poisson comparisons lack that flag.
  Their `firmness: proved` is justified for the limiting laws, not a general
  data-dependent face-win criterion. The new `detect` text explicitly states the
  numerical limitation, but the generated introductory sentence still overclaims.
  The `tbinom` binding also categorically reports non-existence after its numerical
  face comparison. Resolving this needs an authorized numerical-flag/generator
  change and, where appropriate, a binding diagnostic change or a global proof.
  Those edits are forbidden here. No status was changed because this audit has
  not proved that a particular returned status misclassifies a particular sample.
- **Historical research text: discrepancies recorded, not edited.**
  `dev/research/ml-behaviour.tex` still describes rejected `tpois` singleton
  fits and unimplemented `tbinom` free-size subsets, contrary to the current
  fitter and dossiers. Its assertion that joint Skellam fitting matches empirical
  mean and variance exactly is also false in general: observations zero and one
  have mean 1/2 and variance 1/4 with divisor two, whereas any Skellam law has
  variance at least the absolute value of its mean. The new dossier makes no
  such moment-matching claim. That file is explicitly outside this job's scope.

## Validation

All runs used the **local workstation**, through `/tmp/umvl-cpu/run4`, with
`nice -n 10`, at most four build/test workers, and
`OMP_NUM_THREADS=OPENBLAS_NUM_THREADS=MKL_NUM_THREADS=1`.
`TESTTHAT_CPUS=4` was set for the validation sequence. Temporary files were in
`build/tmp`; logs and scratch scripts were in `build/scratch`. Waiting commands
were left queued until the shared CPU locks became available. Every foreground
command finished before this report was written.

Environment: R 4.6.1 (2026-06-24), testthat 3.3.2, roxygen2 8.0.0,
GNU C++ 15.2.0, BH headers from the workstation R 4.6 library. C++ used the
repository CMake defaults and shared ccache configured by the Justfile.
No new simulations, random seeds, numerical tolerances or golden updates were
introduced. The dossier histories retain the existing likelihood margins:
10^-9 times sample size for `tbinom`, 10^-8 for `bnbinom`, and 10^-8 total
for the Skellam certificate, conditional on computed quantities.

| Check | Result |
|---|---|
| `just regen` | Passed; repeated for final wording before checks, and again as a dependency of final `just test`. No regeneration warnings/errors. |
| Requested targeted `testthat::test_local` filter | **FAIL 0, WARN 0, SKIP 7, PASS 8652**, 77.0 seconds. The seven skips require the full tier. |
| `tools::checkRd` over all `man/ml*.Rd`, excluding Non-ASCII messages | Passed; output file had zero bytes, including for all seven owned families. |
| `python3 site/scripts/data.py` | Passed; 99 families generated. |
| `just relations` | Passed; **157 edges, 0 failed**. This includes symbolic and numerical relation checks, not a proof of MLE existence. |
| Protected-metadata comparison against `git show HEAD:<dossier>` | Passed for all seven dossiers after removing only the new `existence` and permitted limit `when`/`detect` text from comparison. All other YAML fields were exactly equal. |
| Requested control-character grep | Passed; no filenames printed. |
| `git diff --check` | Passed. |
| `just cpp-test` (dev) | **106/106 passed**, zero failed; ctest elapsed 51.22 seconds. |
| Final `just test` (R dev) | **FAIL 0, WARN 0, SKIP 151, PASS 20121**, elapsed 127.002 seconds. Matches the supplied baseline. |

The C++ build succeeded with existing warnings in unchanged sources: CMake's
CMP0167/FindBoost policy warning, misleading indentation, unused variables,
range-loop copies, and inline declarations for gamma tail functions. These are
build warnings, distinct from the zero-warning R test results. No permitted
failures were used. Full-tier slow tests and the no-long-double, JS and full
site-build suites were not run: this job changed prose/metadata only, with no
fitting, d/p/q/r or test-infrastructure code changes.

The targeted R command was run from `univariateML-r` with
`UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions`:

```r
testthat::test_local(
  '.',
  filter = 'dist-(tpois|tbinom|bnbinom|skellam|yulesimon|zeta|zipf)|conformance|restricted|generated-dpqr|parameter',
  stop_on_failure = TRUE
)
```

The help check used exactly:

```r
for (f in Sys.glob("man/ml*.Rd")) {
  r <- tools::checkRd(f)
  r <- r[!grepl("Non-ASCII", r)]
  if (length(r)) print(r)
}
```

These tests establish generated-package compatibility and regression behavior.
The existence and uniqueness grades above rest on the displayed mathematical
arguments, not on test passes or a numerical search.

## Report display

The required `codex-math-web/bin/show` call was attempted after completion.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report remains in this worktree and is linked in the final
response. No viewer configuration or file outside the worktree was changed.
