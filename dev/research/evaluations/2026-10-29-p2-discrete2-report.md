# TASK-81: p2-discrete2

## Changes

- `univariateML-cpp/include/cppunivariate/distributions/{yulesimon,zeta,zipf,dunif,tpois}/`:
  `README.md`, `mle.hpp`, `adapter.cpp`, `test.R`, `test.cpp`.
- `univariateML-cpp/include/cppunivariate/distributions/cmp/`: `adapter.cpp`, `test.R`.
- This report. No shared source, goldens, dpqr files, or board files changed; no commits.

## Hold support

| Family | Result |
| --- | --- |
| yulesimon | `rho` held: direct likelihood evaluation, including all-ones samples. |
| zeta | `s` held: direct likelihood evaluation, including all-ones samples. |
| zipf | `N`, `s`, or both held. A free `N` is `max(x)`; free shape uses the existing concave scalar profile with its attained zero boundary. Held `N > 1` and all ones stop at the declared point-mass limit. |
| dunif | `min`, `max`, or both held. Free endpoints tighten to sample extremes. Invalid integer endpoints or incompatible support stop. Core failures have an explicit status. |
| tpois | All six previously unsupported combinations with at least one bound free. Explicit `NULL` tightens bounds to `a = min(x)-1`, `b = max(x)`. Existing conditional-mean solve estimates the rate; a singleton support returns the representative rate one, or the supplied rate exactly. Zero rate is accepted only if zero belongs to the conditioning event and all counts are zero. |
| cmp | Both coefficients were already holdable. Return held `lambda` exactly rather than after a log/exp round trip. Boundary-member work remains P3 as instructed. |
| skellam | Every subset already implemented; retained and included in validation. |
| dlaplace | Every subset already implemented; retained, including the documented integer-location model. |
| dnorm | Every subset already implemented; retained, including held-mean zero-scale detection. |
| dgamma | Every subset already implemented; retained, including held-parameter all-zero escapes. |
| dweibull | Every subset already implemented; retained, including its held Bernoulli escape and unidentified all-zero case. |
| bnbinom | Every subset already implemented; retained, including the NB/Poisson face comparison and score gate. |

Fourteen newly supported combinations; 52 total subsets across the twelve families.
All twelve now have `holds: []`. None were converted to `no-mle`: P0 proves no
structural never case in this group. No requested missing hold remains.
Design arguments `tpois.a/b` remain required, without defaults; regular and
endpoint coefficients default to `NULL`. New arguments are documented under
`r.control`. Held values are returned exactly and included in `fixed`.

The tpois lower-point-mass escape requires a held `b`; its upper-point-mass
escape requires a held `a`. Metadata records those hold scopes. Free lower
bounds use the specified representative of the nonunique strict-bound interval.

## Evidence and limits of the mathematics

The endpoint-tightening arguments and scalar concavity/existence arguments are
proved in the supplied P0 note and retained in family prose. The implementation
uses these exact profiles or solutions. The new all-held paths evaluate the
likelihood without optimization.

New R tests sweep every new subset on n=20 and n=100 samples, small samples,
ties, constants, and relevant limiting samples. They compare independent
log-beta/series formulas for Yule–Simon and zeta, enumerated endpoint and
multistart BFGS fits for Zipf and tpois, and exhaustive reference-PMF endpoint
comparisons for discrete uniform. They also check invalid holds, non-existence,
exact coefficient preservation, `fixed`, and degrees of freedom. Five added
C++ test cases cover the directly testable core paths; a CMP regression checks
exact held-rate preservation.

These numerical checks do not certify every existing multistart search globally.
In particular, BNB's explicit sample criterion for its NB/Poisson faces remains
open; its existing numerical likelihood-tie policy was not changed. Existing
global-search limitations for discretized gamma and joint Skellam remain.

## Validation

All required commands completed on simbox via `/tmp/umvl-codex/simbox-run.sh`:

- `just cpp-test`: 6 CTest targets completed. Five passed outright; the
  distribution target had **419/421 passing cases** and **513740/513742 passing
  assertions**, with exactly the allowed failures at `betapr/test.cpp:33` and
  `std/test.cpp:40`. Duration 120.92 seconds.
- `just test`: **11030 passing expectations, 3 failures, 0 warnings, 0 skips**
  (11033 expectations total), duration 441.2 seconds. Failures are exactly the
  allowed frozen `sstd` quantile fixture and the `bnbinom` / `std` goldens.
  Conformance: **1753 passing expectations**. Tpois: **295 passing expectations**
  (both counts included in the suite total).
- `just quality cmp yulesimon zeta zipf tpois skellam dlaplace dnorm dgamma
  dweibull dunif bnbinom`: **12/12 pass**, exit zero.
- Local syntax check: all twelve family R test files parse successfully.
- `git diff --check` passes. Every golden file remains unchanged.

The C++ and R commands return nonzero solely because of the five explicitly
allowed machine-dependent fixtures; there are no unexpected failures.
Logs are in `/tmp/umvl-codex/p2-discrete2/`: `cpp-test-final.log`, `test.log`,
and `quality.log`.

## Open questions / deferred work

- CMP attained zero/geometric boundary members remain P3, explicitly out of scope.
- The pre-existing BNB global face criterion and global optimization certification
  remain mathematical questions; no shared change was needed for these holds.
