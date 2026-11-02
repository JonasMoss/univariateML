# ZIB free size with all-zero data: correction and validation

2026-11-02. Completed in `codex/zib-zero`, based on source revision
`55149028ca17515418b2df50d92d4b220d822e2d`, with the uncommitted changes described
below. No commits, board edits, dependency installations, or other worktree
changes were made.

## Result

`mlzib(c(0, 0, 0), size = NULL, prob = 0.5, pi = 0.2)` now returns
`size = 0`, preserves both probabilities, and has log likelihood zero. Previously
the held-probability branch chose size one and returned `3 * log(0.6)`, about
−1.532476871, although size zero attains likelihood one.

The fix changes the free-size all-zero held-parameter path from
`held_profile(fp ? 1 : 0)` to `held_profile(0)`. It does not alter the positive-data
path, the fixed-size path, or the existing all-free all-zero representative
`(size, prob, pi) = (1, 0, 0)`. Goldens are unchanged. The existing shared binding
already preserves the resulting values and status; no adapter change was needed.
The [canonical dossier](../../../univariateML-cpp/include/cppunivariate/distributions/zib/README.md)
now explains the maximizing sets and replaces the obsolete R workaround.

## Exact all-zero result and identification

Write `s` for size, `p` for probability, `i` for inflation, and `m > 0` for the
number of observations. For all-zero data, likelihood is
`[i + (1-i)(1-p)^s]^m`. The domain includes every nonnegative integer size and
both probability endpoints. Size zero is the point mass at zero, including at
`p = 1`. Thus the zero probability is at most one, and equals one exactly on the
union `s = 0` or `p = 0` or `i = 1`. This proves global attainment and identifies
all maxima without a numerical search. The supremum is one and the maximum log
likelihood is zero in every valid free-size held subset.

| Held probabilities | Complete maximizing set | Returned representative | Identification among free coefficients |
|---|---|---|---|
| Neither | `s=0` or `p=0` or `i=1` | `(1,0,0)` | No coefficient has a unique maximizing value. |
| Only `p`, with `p>0` | `s=0` or `i=1` | `(0,p,0)` | Size and inflation are nonunique. |
| Only `p=0` | Every size and inflation | `(0,0,0)` | Size and inflation are unidentified. |
| Only `i`, with `i<1` | `s=0` or `p=0` | `(0,0,i)` | Size and probability are nonunique. |
| Only `i=1` | Every size and probability | `(0,0,1)` | Size and probability are unidentified. |
| Both, with `p>0` and `i<1` | `s=0` only | `(0,p,i)` | Size is unique. |
| Both, with `p=0` or `i=1` | Every size | `(0,p,i)` | Size is unidentified. |

These statements include `p=1` and `i=0`. At any returned size-zero member,
every free probability is unidentified because changing it leaves the law
unchanged. Nonuniqueness does not imply nonattainment. ZIB already reports an
attained representative as C++ `Ok` and an ordinary R fit, without a dedicated
identification warning/status or identification attribute. That convention is
preserved and documented; `fixed` and likelihood degrees of freedom retain their
existing coefficient-counting semantics and do not assert information rank.
There is no pre-existing ZIB unidentified stop to retain and no free-size
all-zero subset here needs a nonexistence stop. No `holds:` exception was added:
these are data-dependent boundary cases, consistent with R6 and A7.

The proof is exact, rather than numerical evidence for an optimizer. This branch
returns a boundary member directly with zero iterations and exact log likelihood
zero; no tolerance, search cap, or computed likelihood comparison determines its
answer. Existing validity checks and positive-data failure/escape statuses are
unchanged.

## Tests and reproducibility

The appended C++ case and new R case enumerate the Cartesian product
`prob in {free, 0, 0.5, 1}` and `pi in {free, 0, 0.2, 1}` on three zeros. They
cover all four held subsets, the reproducer, and both endpoints of each
probability. Checks include the chosen representative, exact held values,
likelihood zero, agreement with the separate density evaluator, C++ status and
zero iterations, and R fixed-coefficient/df metadata. Existing goldens,
conformance, positive-sample oracles, and fixed-size boundary tests also ran.
New tests are deterministic; seeds and numerical tolerances of existing tests
remain unchanged in their sources. No fixture regeneration occurred.

All runs used the local workstation, through `/tmp/umvl-cpu/run4`, with at most
four slots, one BLAS/OpenMP thread per worker and the launcher's `nice -n 10`.
No simbox was used. Commands remained in the foreground and were awaited through
completion, including substantial CPU-slot waits. Temporary files and logs are
under this worktree's ignored `build/tmp` and `build/scratch`.

Environment: Linux 7.0.0-38-generic, x86_64; GCC 15.2.0; R 4.6.1
(2026-06-24); Node v22.22.1. The no-long-double recipe defines
`BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`.

Before running recipes:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
```

| Final command/check | Result |
|---|---|
| `just cpp-test` | 106/106 dev shards passed; 0 failed. |
| `just cpp-test-no-long-double` | 127/127 shards passed; 0 failed. |
| Targeted `testthat::test_local("univariateML-r", filter="dist-zib", stop_on_failure=TRUE)` after `just regen`, dev tier | PASS 185, FAIL 0, WARN 0, SKIP 3; all skips require the full tier. |
| `just test` | PASS 20350, FAIL 0, WARN 0, SKIP 151; expected tier skips. |
| Related-family read-only R check below | Completed; output retained. |
| `just test-full` | PASS 41953, FAIL 0, WARN 0, SKIP 0. |
| `just js-build` | Exit 0. |
| `just js-test` | 413/413 passed; 0 failed/cancelled/skipped/todo. |
| `git diff --check` | Passed. |

The initial R dev run had one error in the newly written test: its expected
empty held set was `NULL`, which `expect_setequal()` rejects. The test now uses
`character()` for the empty set. The targeted test and both final R suites above
passed after that correction. No fitter failure was suppressed or permitted.
Build output contains existing compiler warnings, including a binom
self-comparison warning in the Wasm build; successful build status is not a
claim of warning-free compilation.

SHA-256 of the tested edited fitting/test sources:

```text
fe8a2bf64fc734d8759401212d133546a31eb3ce1b6b707a9abaef343b060d79  zib/mle.hpp
f3fb71810b3953c7ef18e36524974349e2e1a51ff97f9c1d7ca137d2e8decc4b  zib/test.cpp
e7b74a7e4ef5ae8628f71f0424551ffbd67a842c3f3b19389ce738228a006f4e  bindings/r/distributions/zib/test.R
```

The first two paths are relative to the core distributions directory.

The targeted R command used `UMVL_TEST_TIER=dev`, `TESTTHAT_CPUS=1`, and
`UMVL_DISTRIBUTIONS_DIR="$PWD/univariateML-cpp/include/cppunivariate/distributions"`.
The full R suite includes the three targeted tier skips, including ZIB's
reference-density comparisons on nonzero samples. Golden regression supports
unchanged recorded fits; it does not prove globality for arbitrary nonzero data.

## Read-only check of related families

The check combines source inspection of each family's `mle.hpp` and shared
binding with an R sweep. No source or test of these families was edited.
Reproduce after package generation with:

```sh
/tmp/umvl-cpu/run Rscript dev/research/evaluations/2026-11-02-zib-zero/check-related.R
```

The [script](2026-11-02-zib-zero/check-related.R) and
[observed output](2026-11-02-zib-zero/check-related.txt) are retained beside this
report. The sweep uses three zeros, free or held probability at 0, 0.5 and 1;
ZMBINOM additionally uses free or held zero mass at 0, 0.2 and 1; TBINOM uses
bounds `(-1,0]`, `(-1,5]`, and `(0,5]`; BBINOM checks all shape hold subsets with
held alpha 2 and beta 3. These are finite checks, supplemented by the following
likelihood arguments, not exhaustive numerical parameter-space searches.

| Family | All-zero/free-size behavior and assessment |
|---|---|
| `binom` | Free probability returns `(size,prob)=(0,0)`. Held probability, including 0 and 1, returns size zero and preserves it. The zero probability `(1-p)^s` is one at size zero. No corresponding defect. |
| `zmbinom` | Size must be at least one: size zero is outside this family's domain. The zero likelihood factor is `p0^m`, independent of base size/probability. With free `p0`, returns `p0=1`; with held positive `p0`, every valid base pair maximizes and log likelihood is `m*log(p0)`. Held `p0=0` rejects zeros. The all-free path warns that base parameters are unidentified; held paths return representatives without that warning. This identification-reporting distinction exists, but none of these returned pairs is a non-maximizer. |
| `ztbinom` | Zero is outside its positive-count support, so all checked subsets stop. Size zero is not allowed. No all-zero fitting branch applies. |
| `tbinom` | When bounds admit zero, free size can be zero and the truncated law is a singleton at zero. Free probability returns size zero/probability one; held probability at 0, 0.5 or 1 also returns size zero, with log likelihood zero. Bounds excluding zero stop. Estimated bounds on zeros become `a=-1,b=0`, matching the singleton case inspected. No corresponding defect. |
| `bbinom` | Both shapes held: integer search includes size zero and returns it with log likelihood zero; the zero probability decreases with size for positive shapes. Beta held/alpha free: stops explicitly that size zero attains the maximum and alpha is unidentified. Both shapes free, or only alpha held: existing structural not-implemented stops, rather than a fitted non-maximizer. No corresponding returned-estimate defect. |

## Open scope and limitations

No all-zero ZIB item remains open. No changes to d/p/q/r, special functions,
truth fixtures, generators, or other families are needed for this correction.
The pre-existing nonzero ZIB free-size/free-probability grid still lacks a global
unimodality/tail certificate, as recorded in the dossier and prior size-search
report; passing goldens and oracles does not close that separate gap. BBINOM's
structural free-size unsupported subsets remain as found and were not edited.
