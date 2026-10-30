# TASK-94: C++ d/p/q/r alignment after P4

Worktree `dpqr-align`, branch `codex/dpqr-align`; no commits or board edits.

## Changes

Under `univariateML-cpp/include/cppunivariate/distributions/`, changed
`dpqr.hpp`, `truth.py`, `truth.json`, and `test.cpp` in each of the eight
families below. Also changed delegating `frechet/dpqr.hpp`,
`pareto3/dpqr.hpp`, `lgamma/dpqr.hpp`; removed the obsolete nhyper `n`
example in `README_SCHEMA.md` and updated the corresponding comment in
`bindings/r/tests/testthat/test-generated-dpqr.R`.
No fitter, golden, fixture, codegen script, or production R source changed.

All four operations now use README coefficient names and order:

| Family | Old coefficients | New coefficients |
| --- | --- | --- |
| gamma | shape, rate | shape, scale |
| invexp | rate | scale |
| invweibull | shape, rate | shape, scale |
| llogis | shape, rate | shape, scale |
| hcauchy | sigma | mu, sigma |
| ht | nu, sigma | mu, nu, sigma |
| hyper | m, n, k | m, N, k |
| nhyper | n, m, r | m, N, r |

Scale families retain the rate calculations with `rate = 1 / scale`;
logarithmic fallbacks handle finite scales whose reciprocal overflows.
Half families subtract mu in d/p and add it in q/r. Population validation
requires integer N with N >= m; nhyper derives n = N - m internally.
Invalid scales and nonfinite thresholds return NaN. Existing direct-tail
paths are retained.

Removed P4 reciprocal-scale likelihood adapters, manual half-family
likelihood shifts, hyper's N-to-n adapters, and reciprocal scales in
Frechet/Pareto III delegation. Log-gamma now supplies 1/ratelog to gamma.
Existing scalar regression and sampler points were reparameterized.

## Truth evidence

Ran `/tmp/umvl-cpu/run just dpqr-truth gamma invexp invweibull llogis hcauchy ht hyper nhyper`.
All eight routes were updated and independently verified against README
densities: 34 parameter points, 710 value rows, 1,496 quantile rows.
README sampling/stress points are regenerated, so the new tables are not
simple row permutations of the old tables.

Separately reevaluated every old density/tail/quantile cell at its
corresponding new parameters using mpmath (100 digits), exact scale =
1/rate, N = m+n, and mu = 0 for old half-family points: 3,626 cells.
Exact-mapping largest relative difference: 5.6011e-14 (natural tail probability);
densities 5.5392e-14; quantiles 1.0586e-16. Relative stored log-density
and log-tail discrepancies are at most 1.0902e-16 (denominator max(1, |log value|)).
With mapped parameters also rounded to binary64, the largest relative
difference is 7.4856e-13 (inverse-Weibull density); tails 7.2603e-13,
quantiles 1.9991e-16; stored log cells differ by at most 3.6190e-15.
Endpoint/overflow cells match after binary64 projection; relative
natural-scale comparisons exclude underflowed/subnormal values.

## Validation

Focused local binary: 103 cases, 217,427 assertions, all passed.
Simbox `just cpp-test`: 6/6 binaries; 891 cases and 1,210,559 assertions
passed (distribution binary: 869 cases, 1,199,512 assertions).
Simbox `just quality`: 98/98 families passed.
Simbox `just test`: 29,777 expectations passed; 0 failures, warnings, skips
(generated-dpqr file: 6,247; golden regressions: 196).
Simbox `just check`: exit 0; 0 errors, 1 warning, 0 notes. Packaged tests:
29,764 passed, 0 failures/warnings, 1 skip (`test_special.R`, On CRAN).
Final check command: `/tmp/umvl-codex/simbox-run.sh bash -c 'export UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions"; just check'`.
Plain `just check` first failed 32 fixture-reading tests because that
variable is unset in the check recipe; the configured rerun passed.
Compiler warnings observed: existing gamma_p/gamma_q inline declarations
in special/tails.hpp and misleading indentation in dlaplace/dpqr.hpp.
Regeneration emitted 159 unresolved roxygen-link notices. The final check
warning is missing Rd cross-references (including `0, 1` links).
Final logs: `cpp-test-final.log`, `test.log`, `quality.log`, `check.log`;
comparison scripts/results and original tables are also retained in
scratch: `/home/jonas/.cache/umvl-codex/dpqr-align/`.

## TASK-78.9 merge

Regenerate package sources after merging; assembly now accepts all eight
core signatures. Generated R wrappers automatically use the new README
coefficient names/order; the generic truth test passes coefficients by
name. Production R reference adapters intentionally remain untouched.
The check recipe needs the dossier environment already used by `just test`.
No codegen change is needed. Keep TASK-78.9's R resolver/export changes;
only a comment in its generic truth test overlaps this work.
