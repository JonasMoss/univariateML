# TASK-78.13 — fixF

Base: `5ccf059`, branch `codex/fixF`. No commits made.

## Files changed

- `cppunivariate/include/cppunivariate/distributions/bnbinom/dpqr.hpp`
- `cppunivariate/include/cppunivariate/distributions/bnbinom/test.cpp`
- `cppunivariate/include/cppunivariate/distributions/pig/dpqr.hpp`
- `cppunivariate/include/cppunivariate/distributions/pig/test.cpp`
- `cppunivariate/include/cppunivariate/distributions/rng_std.hpp`
- `cppunivariate/include/cppunivariate/special/tails.hpp`
- This report.

Fitters, adapters, READMEs, R sources, goldens, truth generators and truth
JSON files are unchanged. No board edits or package installations.

## Numerical changes

**bnbinom:** remove integer/count-budget restrictions from public d/p/q.
Evaluate large-count masses using log gamma ratios. Compute survival from
swapped beta integrals (a 3F2 expansion); after j >= beta-1 the absolute
remainder is bounded by `abs(next)*(j+n+size+alpha)/(n-1)`.
Stop at a relative bound of 2e-16. Symmetry reduces alternating cancellation.
For small counts use a direct lower sum or positive addition to a survival
computed at count 300, avoiding the slow n=1 expansion and subtraction.
Quantiles use the shared discrete inversion of accurate log tails, with
representable-count stopping and overflow to infinity when appropriate.
The pre-existing fitter-facing `zero`, `increment`, and `logpmf` are unchanged.

**pig:** retain the existing small-count mass recurrence. Above count 100000,
when `c/x < 1/4`, evaluate the large-order Bessel expansion as
`E[exp(-c/T)]`, T gamma with shape x-1/2. The alternating Taylor remainder
is bounded by the next absolute term. Sum the upper tail with a positive
series and a geometric remainder bound. At `n*(1-A)>1e16`, the constant-ratio
suffix has relative error of order `1/(n*(1-A))` and avoids
count increments that no longer change a double. Quantiles use the shared
log-tail inversion, including lower tails whose natural probability underflows.

**Shared helper:** both families need `special::log_gamma_ratio`, so it lives
in `special/tails.hpp`. It uses Stirling through inverse power 11 (omitted
term below 3e-17 per log gamma at arguments >=16), and a small-relative-shift
series to avoid cancellation. New numerical paths use double arithmetic.

**StdRng:** retain `poisson_distribution<int>` below INT_MAX-1e6, use
`poisson_distribution<int64_t>` up to mean 1e18, and use PTRS transformed
rejection in double count coordinates beyond that. The log acceptance uses
a stable deviance series and Stirling, avoiding huge log-gamma subtraction.
Draws beyond exact integer representation are returned as rounded doubles.
The beta negative binomial truth sampler now calls StdRng directly; its
test-only range guard and both truth cases' `may_fail` markers are removed.

## Evidence

All builds and suites ran through `/tmp/umvl-simbox/run`, with one build
thread. The standalone mpmath probes ran through `/tmp/umvl-cpu/run`.
Logs and oracle scratch are in `/tmp/umvl-codex/fixF/`.

- Baseline `just cpp-test`: 5/6 ctest targets passed. Distribution tests:
  772 cases, 770 passed, 2 failed; 1,037,990 assertions, 702 failures.
  The only failing cases were the documented simbox betapr and std cases;
  700 assertion failures were tolerated inside the two old truth cases.
- Baseline `just test`: `[ FAIL 3 | WARN 0 | SKIP 0 | PASS 22475 ]`.
  Exactly the documented simbox sstd reference, bnbinom golden and std golden.
- Initial targeted run: 18/18 cases, 199,384/199,384 assertions passed.
- `just quality bnbinom pig`: both pass.
- Final `just cpp-test`: 5/6 ctest targets passed. Distribution tests:
  775 cases, 773 passed, 2 failed; 1,198,017 assertions, 1,198,015 passed,
  2 failed. Only the same documented betapr and std cases fail.
- Final targeted run: 18/18 cases, 199,384/199,384 assertions passed.
- Strict truth-only run: 2/2 cases, 38,984/38,984 assertions passed.
- Final `just test`: `[ FAIL 3 | WARN 0 | SKIP 0 | PASS 22475 ]`, identical
  to baseline; only the same three documented simbox failures.

Default truth tolerances are unchanged. The regression tests contain
mpmath snippets (100 digits), exact bnbinom rational masses and survival,
the upper 1e-300 quantile 5.477225575051661e150, PIG at x=1e100, the PIG
asymptotic transition at x=100001, log quantiles down to -10000 (and PIG
-1e6), DBL_MAX counts, and Poisson mean/variance checks at means 3e9,
1e12, 1e18 and 1e30 (20,000 draws each). No oracle correction was needed.

## Open questions

No changes requested to the documented TASK-75 machine-specific failures.
The validation covers every supplied truth parameter point; extreme
parameter limits outside those tables have not been exhaustively audited.
