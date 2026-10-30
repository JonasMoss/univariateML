# C++ independence — TASK-101.4

2026-10-30; base `237c05276aa4326925c353a2ec9b9f1cdf7f56c5`, branch `codex/cpp-standalone`.
Validation applies to this uncommitted working tree. No commits, Git writes,
board edits, installs or simbox; `just check` deferred as requested.

## Build and export

CMake no longer discovers or invokes R/BH. It accepts plain `Boost_INCLUDE_DIR`
headers, otherwise tries Boost CONFIG then MODULE discovery (`BOOST_ROOT`
supported), and names both options in its failure diagnostic. The monorepo
`cpp-test` recipe alone resolves BH with Rscript after failed configuration;
its ccache launcher is retained. This machine uses BH's headers as plain Boost.

`dev/tools/standalone-export.sh` copies only the C++ artifact to a fresh directory
outside the repository (under the writable ccache directory), excludes build
output, and removes it on exit. It accepts a header path, environment variable,
or the existing CMake cache; it never invokes R. A whitelist PATH contains only
C++ build tools: neither R nor Rscript is available during configure/build/ctest.
The source audit rejects escaping/missing quoted includes, symlinks, repository
absolute paths and sibling-directory CMake references. Dossier prose links and
optional Python truth-regeneration routes are documentation, not build inputs.
Golden/truth fixtures resolve inside the exported source tree. Export result: **6/6 ctest targets passed**, 159.02 seconds.

`BOOST_ROOT`-only configuration also passed; an intentionally missing header
path failed with the expected diagnostic naming both supported options. Modern
CMake emitted development-policy notices for the legacy FindBoost fallback.

## Adapter audit

All **98 adapters** were audited; the complete family-by-family table is in
[the audit CSV](2026-10-30-cpp-adapter-audit.csv). Summary:

| Family | Statistical behaviour found | Moved or planned |
|---|---|---|
| lgamma | log transform, gamma delegate, held rate, Jacobian, statuses | New `lgamma_mle`; all held subsets preserve former arithmetic |
| fatigue | held-threshold data shift and support validation | New `fatigue_mle_shifted`; unit Jacobian, delegate status preserved |
| hnorm | duplicated held-scale likelihood | Return existing identical core calculation |
| sstd | unconditional BFGS-failure total-score rescue | Same finite-difference norm < 1e-3 gate now in core; conditional failures remain failures |
| nbinom | held-size probability rounds to one | Core `PrecisionFailure`; same R diagnostic |
| paralogis, invparalogis, gompertz | fully free constant-data rejection | Core `Unbounded`, preserved by reciprocal delegate |
| logis | held-location constant-data rejection | Core already rejects zero radius; specific R diagnostic retained |
| trgamma, invtrgamma | lognormal likelihood gap in error text | Diagnostic only; escape selection already core |
| cmp | eta-to-lambda result conversion | Coordinate marshalling only |
| bbinom, cmb | unsupported free-size subsets | Plan: certified integer-tail/face comparisons (TASK-113/114) before exposing these searches |
| hyper | unsupported joint design subsets | Plan: certified joint-design/limit comparison; supported searches already core |
| Remaining families | holds, support checks, status diagnostics and R result types | Fitting already core; no further numerical logic found |

Seven appended doctests compare moved transforms/likelihoods and decisions with
former adapter computations. Default algorithms, tolerances, starts and supported
held subsets are preserved. Canonical dossiers record the changes. The Jacobian
identities are mathematical; regression and score checks are numerical evidence,
not new global-maximization proofs or guarantees for deferred searches.

## RNG

All 98 family samplers take an explicit RNG reference. The five required methods
are `runif`, `rnorm`, `rgamma`, `rt`, `rpois`; `StdRng` supplies all five using
object-owned generator/distribution state. No global RNG, `rand`/`srand`, R calls
or Rcpp dependencies were found in C++ headers. Truth tests instantiate all family
samplers with `StdRng` in the standalone build. `bindings/r/src/rng_r.h` alone
provides `RRng`; `rng_emg.h` is the binding-only specialization preserving the
frozen R EMG stream. No RNG numerical changes or TASK-78 edits were needed.

## Validation

Local GCC 15.2.0, CMake 4.2.3, R 4.6.1; all builds/tests under
`/tmp/umvl-cpu/run4` (`nice -n 10`, four build slots, BLAS/OpenMP one thread).
`TMPDIR=$PWD/build/tmp`, `JUST_TEMPDIR=$PWD/build/tmp`; scratch logs in
`build/scratch/{cpp,r,quality,export}-final.log`. Same fixtures/seeds/tolerances
as existing suites; new transform regressions compare exact double arithmetic.
The first final attempt found one test-only NaN-equality assertion; corrected
without changing fitter behaviour and reran the required suites.

| Check | Before (layout handoff, same base) | After |
|---|---:|---:|
| `just cpp-test` ctest | 6/6 | 6/6 |
| distribution cases / assertions | 915 / 1,281,358 | 922 / 1,281,403 |
| Other C++ cases / assertions | 22 / 11,047 | 22 / 11,047 |
| `NOT_CRAN=true just test` FAIL/WARN/SKIP/PASS | 0/0/1/39,040 | 0/0/1/39,040 |
| `just quality` | 98/98 | 98/98 |
| standalone export ctest | not present | 6/6 |

Only R skip: optional `{sn}` absent (`test-dist-sn.R:135`), unchanged.
The brief's earlier 30,743-pass R baseline predates the layout handoff's 39,040.
All **294** golden/truth files are byte-identical to HEAD (98 each golden.json,
truth.json, truth.py). Protected d/p/q/r, special functions and truth cases are
unchanged. No default fits or fixtures were regenerated.
