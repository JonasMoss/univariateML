# rvinecopulib test results

Upstream `main` at
[`2ac9d35c1bc25ba310f9e65f32579229b2dbcfd0`](https://github.com/vinecopulib/rvinecopulib/commit/2ac9d35c1bc25ba310f9e65f32579229b2dbcfd0),
rvinecopulib 1.0.0.1.0. All runs used the workstation, R 4.6.1,
GCC 15.2.0 and testthat 3.3.2. Both installed univariateML copies declare
version 1.5.0. The library path distinguishes the CRAN copy (`lib-old`)
from the upcoming build (`lib-new`, recorded at univariateML
`3fe10413` in the [reverse-dependency results](../../roadmap/2026-11-23-revdep-and-package-name.md)).

| Test files | univariateML | Before patch, pass / fail | After patch, pass / fail | After patch, elapsed |
|---|---|---:|---:|---:|
| `test_margins.R` | CRAN 1.5.0 | 145 / 0 | 145 / 0 | 1.90 s |
| All | CRAN 1.5.0 | 1,224 / 0 | 1,224 / 0 | 13.94 s |
| `test_margins.R` | Upcoming build | 142 / 3 | 144 / 1 | 1.55 s |
| All | Upcoming build | 1,213 / 4 | 1,215 / 2 | 14.53 s |

Counts are testthat expectations. All tabulated runs had zero warnings and
zero skips with `NOT_CRAN=true`, including the Monte Carlo covariance test.
A baseline run with `NOT_CRAN=false` also reproduced the earlier check
counts (1,223 passes, 0 failures on CRAN and 1,212 passes, 4 failures on
the upcoming build, each with one skip).

The two Rayleigh expectations at baseline `test_margins.R:602` and
`:606` pass with the patch. The stock upcoming build still reports
Poisson as continuous (`test_margins.R:686` after the patch) and rejects
the discrete candidates in the vine test (`test_vine.R:561`).

## Compatibility diagnostics on the patched suite

| In-memory change to the upcoming build | Margins file, pass / fail | Full suite, pass / fail |
|---|---:|---:|
| Restore the support `type` attribute from the metadata list element | 145 / 0 | 1,221 / 1 |
| Also route omitted binomial `size` to explicit `size = NULL` | 145 / 0 | 1,224 / 0 |

Both runs had zero warnings and skips. Restoring the support attribute
cleared both known failures and exposed the later binomial call at
`test_vine.R:595`. On the fixture's 200 counts, CRAN
`mlbinom(x)` returned size 4 and probability 0.41375. The upcoming build's
`mlbinom(x)` raised `univariateML_invalid`. Explicit `size = NULL` and
`size = 4` each returned size 4, probability 0.41375 and support
`[0, 4]` in both builds. The second diagnostic wrapper was confined to the
R process and tests a possible compatibility behavior.

## Test setup

Installed the upstream package into `build/revdep-pr/lib-old` and
`build/revdep-pr/lib-new` with `R CMD INSTALL --install-tests`, setting
`R_LIBS` to the matching validation library and `lib-deps`. Each test run
prepended the matching scratch library, verified both package paths, set
seed 248, and called `testthat::test_dir()` with `package = "rvinecopulib"`
and `load_package = "installed"`. The file run filtered for
`test_margins.R`, and the suite run used every test file. The tested
package code was identical before and after the patch. The post-patch
runs read a scratch copy of the changed test tree.

R commands ran in the foreground through `/tmp/umvl-cpu/run nice -n 10`
with `MAKEFLAGS=-j1` and a worktree-local `TMPDIR`. The two validation
libraries were read-only. Install logs, test logs and replay scripts are
in `build/revdep-pr/` in the job worktree.
