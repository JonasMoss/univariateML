# TASK-82: p3-closure

Implemented R7a endpoint closure within the assigned files. No commits,
board edits, installations, sub-agents, or dpqr/truth-file edits.

## Files changed

- `univariateML/scripts/{load_distributions,codegen_metadata}.R`;
  `univariateML/R/code_helpers.R` (constructor untouched);
  new `univariateML/tests/testthat/test_closure.R`.
- `distributions/README_SCHEMA.md`: support-expression documentation.
- `distributions/tnorm/{README.md,mle.hpp,adapter.cpp,test.R,test.cpp}`.
- `distributions/power/{README.md,mle.hpp,adapter.cpp,test.R,test.cpp,golden.json}`.
- `distributions/hnorm/{README.md,adapter.cpp,test.R,golden.json}`.
- Support declarations only in README.md for bbinom, binom, cmb, dunif,
  fatigue, fpareto, frechet, genbeta, gev, gpd, hyper, nhyper, nsbeta,
  pareto, pareto2, pareto3, pareto4, tbinom, tpois, triang, unif,
  zib, zipf, zmbinom, ztbinom.
- This report. `code_misc.R` needed no changes.

## Schema and R evaluation

Support bounds accept validated R expressions in coefficients, including
conditional bounds for GPD/GEV and effective lattice bounds for truncations.
Declared `endpoint_log_density` expressions cover zero, finite, and infinite
limits. Undeclared smooth limits use the reference value when already finite,
otherwise two nearby reference log densities with linear extrapolation.

Generated metadata keeps numeric envelopes for pre-fit checks and existing
object attributes, with expressions and closure flags resolved by
`univariateML_support(fit)` for evaluation. The constructor is unchanged.
Density, CDF and quantile adapters share these resolved endpoints; both tails
and log-probability modes are covered. Returned reference functions retain
formal argument order and scalar coefficient overrides. Vector coefficient
overrides retain reference recycling semantics.

The half-normal special case was removed from `univariateML_to_function`.
The truncated-normal reference repair uses the normal/half-normal identity
when a equals the parent mean, including a small-probability inverse-integral
expansion to preserve lower tails without cancellation. This applies by
parameter identity rather than by model name or held/free status.

## Families and mathematical checks

- **tnorm:** NULL bounds profile exactly to min(x)/max(x), because tightening
  a bound increases every feasible parent-normal likelihood. Finite endpoints
  are closed in the core and R evaluation. Held bounds are recorded only when
  supplied as values. Removed all twelve obsolete R7a `holds` entries.
  Existing exponential, reflected exponential, truncated-exponential and
  uniform escapes remain errors with the declared limit. Coincident bounds
  are rejected: they do not define a Lebesgue density. Tests cover held/free
  bound combinations, endpoint likelihoods and exponential boundary samples.
- **power:** alpha is max(x) exactly. `epsilon` remains accepted and validated
  for compatibility but is ignored. A held alpha may equal max(x); beta-only
  holds now fit, including singleton/constant data. The profile beta is
  n/sum(log(alpha/x)). If that sum is zero and beta is free, the likelihood
  is unbounded as beta tends to infinity; this now stops explicitly.
  Tests use the analytic closed density rather than TASK-78's open dpower.
- **hnorm:** removed the predecessor-of-minimum adapter convention and its
  reprofiling. The adapter returns the core's exact endpoint, scale and
  likelihood, including held-scale fits. Tests include ties, huge translations,
  constants with held scale, and likelihood agreement at the minimum.
- **tpois:** audited without changing its fitter. The conditioning event stays
  a < x <= b. Free a is min(x)-1; the actual included lower endpoint is
  max(0,floor(a)+1), with upper floor(b). R7a does not add an atom at a.
  Tests check strict exclusion, endpoint PMFs, q(0)/q(1), and logLik agreement.
- Added support expressions for all currently coefficient-dependent supports,
  including finite binomial bounds and hypergeometric total-draw bounds.
  hcauchy and ht currently have fixed-zero supports and no mu coefficient;
  their existing closed support remains correct until P4 adds translation.
- Direct tests check GPD/GEV xi=-1 endpoint densities and singular/finite/zero
  endpoint formulas for Pareto variants, Feller-Pareto, nonstandard beta,
  generalized beta, Frechet, fatigue and triangular distributions.

These endpoint/profile formulas are analytic. Truncated-normal interior
optimization retains its existing concave natural-parameter solver and face
classification; these changes do not claim a new numerical global certificate.

## Golden changes

Only power and hnorm goldens changed; data, seeds and sampler arguments are
unchanged. Tnorm's default held infinite bounds and tpois fits are unchanged.

- Power alpha: 0.9998067208221304 -> 0.9998067059238506, the exact maximum.
  Beta: 2.0804423132540011 -> 2.080442377749803.
  LogLik: 106.72041810311472 -> 106.72043360361705.
  These are the required removal of the relative endpoint offset.
- Hnorm mu: 0.003132226774118849 -> 0.0031322267741188494 (one double).
  Recorded sigma and logLik are unchanged.

## Final validation

All three required commands completed on simbox using the supplied runner.
They are green under the requested exceptions; nonzero cpp-test/test exit
codes reflect only those known failures, with truth cases explicitly may_fail.

- `just cpp-test`: six CTest targets completed; five passed outright.
  Distribution suite: **634 cases, 632 passed, 2 known failures, 3 skipped**;
  **957,799 assertions, 948,850 passed, 8,949 failed**. Of the failed assertions,
  8,947 belong to 58 truth-table cases explicitly marked may_fail; the other
  two are exactly betapr/test.cpp:33 and std/test.cpp:40. No unexpected case
  failed. Duration: 242.86 seconds.
- `just test`: **22,475 passed expectations, 3 known failures, 0 warnings,
  0 skips** (22,478 total). Failures are exactly test-dist-sstd.R:237 and
  the bnbinom/std golden estimate comparisons. New closure file: **124 passed**.
  Both changed goldens passed. Generic hold/parameter conformance: **2,638 passed**.
- `just quality tnorm power hnorm tpois`: **4/4 passed**, exit zero.
- Local schema/codegen and R parsing passed. Direct reference/endpoint checks
  passed, including positional coefficient overrides and half-normal log tails.
  Added two C++ test cases and seven shared R closure test blocks; updated
  old family assertions to the required closed-endpoint behavior.
- `git diff --check` passed. Logs are under `/tmp/umvl-codex/p3-closure/`:
  `cpp-final.log`, `test-final2.log`, `quality-final.log` and `smoke.log`.
  No command started for this job remains running. No commit was made.
- Artifact-pane display was attempted; the viewer returned HTTP 403 because
  this worktree is outside its configured readable roots.

## Integration outside the assigned files / open questions

- TASK-78 must close own dpower and dtnorm endpoints, GPD's lower endpoint
  and its xi=-1 upper endpoint, and GEV's xi=-1 upper endpoint. Hnorm's own
  density is already closed. The audited Pareto/beta/triangular endpoint
  implementations already handle their interior limits. No dpqr file changed.
- The Python quality support parser must understand the documented R
  conditional expressions before checking GPD/GEV with their new metadata;
  that parser is outside this job. The four assigned fitter quality checks
  accept their support expressions.
- Fitted object `support` attributes retain numeric envelopes. If actual
  coefficient-resolved attributes are desired, integrate the new resolver
  in `univariateML_construct` in the session that owns it.
- No unresolved endpoint/profile mathematical question remains in this scope.
  P4 translation of hcauchy/ht and other P3 restrictions/domains remain with
  their assigned jobs.
