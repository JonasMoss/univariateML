# TASK-81: p2-binomial

## Changes

Changed only README.md, mle.hpp, adapter.cpp, test.R and test.cpp in the
assigned family folders, plus this report.
All nine families: README.md and adapter.cpp. The six implemented families
(binom, bbinom, tbinom, zib, zmbinom, ztbinom): mle.hpp, test.R and test.cpp. No goldens or shared sources changed.
No commits, board changes, sub-agents, or package installations.

Implemented 28 previously missing coefficient-hold subsets:

- binom: probability held, with size held or free. Integer adjacent likelihood
  ratios decrease; bracket and bisect their sign change. Endpoints require
  positive-likelihood feasibility.
- bbinom: both shapes held, with size held or free. Evaluate the likelihood
  directly; enumerate sizes through the bound after which every observed mass
  decreases. Held shapes are returned without arithmetic reconstruction.
- tbinom: the six fixed-size subsets with one or both bounds free; maximize
  by removing unobserved conditioning support. Singleton supports attain their
  maximum with an interior representative. Also the four free-size subsets
  with probability held: geometric tail bounds for a finite upper bound,
  binomial stochastic ordering for an unbounded upper bound. Nonattained
  constant upper-edge fits stop with the named point-mass reason.
- zib: the six subsets involving held prob and/or pi. Profile free inflation
  analytically; with pi held, isolate all fixed-size probability score roots
  using the quadratic turning-point equation, also for the matching ZIP face.
  Held-probability size profiles
  stop only after a bound excludes the remaining integer tail. Held-pi size
  profiles retain the existing log-size search and compare the matching ZIP face.
- zmbinom: the six subsets involving held prob and/or p0. Factor indicator
  likelihood from the positive ztbinom likelihood; preserve a held p0 exactly.
- ztbinom: probability held, with size held or free; binomial mass and increasing
  truncation probability bound the remaining integer tail.

Every new R argument is documented, defaults to NULL when regular, and is
recorded in fixed only when supplied. Required design arguments retain no default.
Incompatible holds and numerical/work-budget failures stop. Existing default
fit paths and golden files are preserved.

## Retained exceptions

No subset was converted to no-mle: P0 did not establish a structural never case.
The following 17 not-implemented entries remain, as allowed by the free-size
scope exception:

- bbinom: free size with either shape free (held sets {}, {alpha}, {beta}).
  P0 explicitly leaves its compactification/exhaustive boundary proof incomplete.
- tbinom: free size and free probability, for each of the four bound subsets.
  P0 leaves this compactification unproved. NULL gives the declared P0 reason.
- cmb: all four free-size entries. P0's classifications depend on treating the
  reference's 1e7 allocation cap as a statistical parameter bound; the note
  explicitly calls that interpretation unresolved. NULL gives that reason.
- hyper and nhyper: three entries each, reason "P4 (C6)" as requested.
  NULL k/r now reaches that declared reason, rather than an Rcpp type error.

## Evidence and limits

The binomial adjacent-ratio argument, beta-binomial fixed-shape mass-ratio
bound, truncation-support reduction, and held-probability integer tail bounds
are analytic arguments. They do not depend on finite oracle scan ranges.
The fixed-size ZIB probability root partition is algebraic; floating-point
root isolation and probability evaluation are not interval-certified.

R tests use independent reference densities, finite integer enumeration and
optim multistarts on n=20/100 samples, ties, small/constant samples and limit
samples. Numerical agreement is evidence, not a proof of global optimality.
The pre-existing free-size ZIB/zero-modified/zero-truncated searches, and the
new held-pi ZIB extension of that search, remain numerical log-size profiles;
no certified global integer-tail algorithm is claimed for free probability.

## Validation

All required commands completed on simbox via /tmp/umvl-codex/simbox-run.sh:

- `just cpp-test`: 6 CTest targets; 5 passed, distribution_tests contains
  exactly the 2 permitted failures. Distribution suite: 421 cases, 419 passed;
  514,516 assertions, 514,514 passed. Failures: betapr/test.cpp:33 and std/test.cpp:40.
- `just test`: 11,218 passed expectations; exactly 3 permitted failures;
  0 warnings and 0 skips. Failures: frozen sstd reference, golden bnbinom,
  golden std. All assigned-family tests and their default goldens passed.
- Generic conformance: 1,801 checks passed, including the golden hold sweep.
- `just quality binom bbinom cmb tbinom zib zmbinom ztbinom hyper nhyper`:
  all 9 passed.
- `git diff --check`: passed. No golden file changed.

Final command logs: /tmp/umvl-codex/p2-binomial/cpp-verified.log,
r-verified.log and quality-verified.log. The raw full-suite exit codes are
nonzero solely because of the five explicitly permitted machine failures.

## Open questions

- Complete the P0 free-size proofs before removing the retained exceptions.
- Decide whether CMB's 1e7 cap is a model bound or a resource limit.
- Certify the free-probability integer profiles and exact Poisson-face ties.
- CMB probability endpoints and other R7/P3 boundary work remain separate.
