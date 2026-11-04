# TASK-141: structured stop details

Completed 2026-11-04 in `codex/stop-detail`, based on
`27f5f36314f0e6b968dd65dcb6876073b3dddba6`. Changes are uncommitted.
All requested implementations and final validation runs are complete and green.
No commits, board edits, installs of dependencies, pushes or publication were
performed. Work stayed in this worktree; simbox was not used.

## Implemented contract

The canonical convention is
[structured-stop-details.md](../../design/structured-stop-details.md), linked
from the [distribution schema](../../../univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md).

- New standalone C++17 `core/stop_detail.hpp` contains `FitCandidate`,
  `StopDetail` and the string-free `StopModel` enum. Family result structs
  append a default-empty payload without changing existing status values or
  aggregate-field positions. No exceptions, printing, new special-function
  dependencies or host-language dependencies were introduced in the core.
- A candidate carries public-order coefficients and total log-likelihood
  bounds in original data units. The payload carries the selected limit fit,
  global supremum bounds when available, and Burr's last unresolved segment
  upper bound separately from a global upper bound.
- `bindings/common/fit.hpp` translates core model enums and coefficient
  orders into named candidate and limit estimates, with error kinds `tie`,
  `limit`, `budget`, `nomle`. It performs no fitting.
- R public fitters signal base R conditions of class `univariateML_<kind>`,
  `univariateML_stop`, `error`, `condition`. Fields include `kind`,
  `candidates` (a data frame), `model`, named `estimates`, `limit_loglik`,
  `loglik_lower`, `loglik_upper`, `segment_loglik_upper`. The internal Rcpp
  adapter returns the condition; the public wrapper signals it with `stop`.
- JS throws the existing `Error` with corresponding properties; candidates
  are row objects, and limit estimates are a named object. The exported
  `FitStop` TypeScript interface documents the fields.

The design was refined to distinguish face candidates from finite candidates:
Burr can retain Pareto, Weibull and finite witnesses in the same stopped search.
`StopModel` supplies that distinction without strings in the core. An empty
candidate model id in a binding denotes the requested family. Bounds on an
individual candidate are not global bounds. `segment_loglik_upper` preserves
an already-computed local certificate quantity without claiming that it covers
unclosed tails. NaN means unavailable, and positive infinity is an intentional
unclosed upper bound.

The one-line edit to `bindings/r/scripts/codegen_wrappers.R` is necessary for
the requested classed conditions: generated public wrappers call adapters
directly, bypassing the older decorator. Both paths now signal returned
conditions. No d/p/q/r generator was changed. User-facing catching examples
are in the R distributions vignette and JS README.

## Family adoption

| Family / stop | Retained information |
|---|---|
| Cauchy held-scale `AmbiguousMaximum` | All isolated location candidates whose likelihood bounds overlap the best witness, held scale and likelihood enclosures. |
| Cauchy `NoMLE`, including held location | Classed NoMLE stop; no fabricated candidate or limit estimate. |
| Burr `WeibullBoundary`, `ParetoBoundary` | Fitted face coefficients and likelihood, plus retained face and finite witnesses. |
| Burr held-scale `AllGeOne` | Pareto fit with endpoint held at the supplied scale, computed in the core. |
| Burr held-power Weibull face | Lomax's exponential fit transported to Weibull in original units. |
| Burr `CertificateTie`, `CertificateBudget` | Both fitted faces, best evaluated finite witness, isolated root witnesses when available, lower bound, conservatively infinite global upper bound, available segment upper bound. |
| Burr `Unbounded` | Classed NoMLE stop. |
| Inverse Burr corresponding limits, tie, budget, unbounded stops | Reciprocal coefficients and likelihood bounds transported with the Jacobian. Power's endpoint uses the original sample maximum on the free endpoint face. Held-scale power fits retain the supplied endpoint. |
| Lomax `ExponentialLimit` | Core-computed exponential rate and total likelihood. |
| Lomax certificate budget | Best evaluated finite witness, exponential face, isolated root if available, global lower and upper bounds from the active segment, queue, root and tail margin. |

Input and unclassified numerical/iteration errors retain ordinary errors.
An exhausted zero budget may precede all candidate computation. A `tie` can
mean a numerical ambiguity or sign-uncertain seam; it does not assert an exact
mathematical tie. Stopped results never become successful fits.

All diagnostic message literals in the four common fit headers were compared
with `git show HEAD:<path>` and are unchanged, including punctuation. Successful
fitter coefficients, decisions and golden files were not edited. No changes
were made to `dpqr.hpp`, `special/`, truth files, generated d/p/q/r, NEWS,
STATUS, KNOWN_ISSUES or roadmap files.

## Mathematical and numerical evidence

This work adds reporting, not a new existence or global-optimality theorem.
It preserves the existing certificate contract: certification in exact
arithmetic conditional on computed comparisons, with the fitters' practical
floating-point margins. It does not add outward-rounded interval arithmetic.
The family dossiers remain authoritative for the underlying proofs and gaps.

The new deterministic C++ cases are appended to each family's `test.cpp`.
They check the analytic held-scale Cauchy modes at `-sqrt(3)` and `sqrt(3)` for
`{-2,2}` with scale 1, direct likelihood enclosure, NoMLE payload emptiness,
Pareto face coefficients, finite Burr witness likelihoods, reciprocal power
coefficients/Jacobians, the held-power Weibull transformation, and Lomax's
exponential fit and budget bounds. These are analytic-formula checks and
numerical regression/identity checks, not proofs over all floating-point inputs.

R and JS exercise public handlers for all four error kinds, candidate names,
coefficient values, bounds, held-scale endpoint faces, held-power faces, and
reciprocal transport. The fixed `sign-uncertain-seam`, `nearly-constant` and
`burr-k1000-r0` workloads from Burr's retained certificate fixtures are reused
across the bindings. Existing suite seeds and oracle tolerances were unchanged.
New checks use analytic comparisons at approximately `1e-7` for Cauchy locations,
`1e-12` for simple face coefficients/likelihoods and `1e-10` slack for direct
Burr likelihood enclosure; the exact assertions are in the retained tests.

## Final validation

All runs were local on Linux x86_64, GCC 15.2.0, R 4.6.1, Node 22.22.1,
Emscripten 6.0.13, with Boost from the installed R 4.6 BH include tree.
C++ builds used C++17 and the Justfile's shared ccache. The no-long-double
build used `-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`. No test machine other
than the workstation was used.

Builds and tests ran as foreground commands through `/tmp/umvl-cpu/run4`,
which reserves all four local slots, applies `nice -n 10`, and pins OpenMP,
OpenBLAS and MKL to one thread. Initial runs waited for slots without being
interrupted. Temporary files and logs stayed under this worktree's `build/`.

| Check | Final result | Command wall time excluding slot wait |
|---|---|---:|
| Initial `just cpp-test` (dev) | 106/106 passed | ctest 62.71 s |
| `UMVL_TEST_TIER=full just cpp-test` | 127/127 passed, no failures | 153.06 s (ctest 106.19 s) |
| `just cpp-test-no-long-double` | 127/127 passed, no failures | 234.15 s (ctest 155.55 s) |
| `just js-build` | Green; all 99 families generated and compiled | 65.44 s |
| `just js-test` | 418 passed; 0 failed, skipped or cancelled | 104.43 s |
| Final `just test` (dev) | 20,606 passed; FAIL 0, WARN 0, SKIP 154 | 213.72 s (R test elapsed 120.501 s) |
| `just test-full` | 43,421 passed; FAIL 0, WARN 0, SKIP 0 | 304.61 s (R test elapsed 284.105 s) |
| Message-literal comparison; `git diff --check`; protected-file diff | Green; unchanged messages, clean whitespace, protected files unchanged | — |

The 154 dev-tier skips explicitly require the full tier; the final full run
executes them and has no skips. There are no permitted test failures. Existing
compiler style/forward-declaration warnings and roxygen cross-reference
diagnostics remain in build output; the zero warning counts above are
testthat's counts. No new test-warning exemption was added.

The final six build/test commands ran sequentially inside one slot-locked
foreground script, with `set -euo pipefail` and `/usr/bin/time -p` per command:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
# Reserve slots around this sequence, e.g. /tmp/umvl-cpu/run4 bash <script>
env UMVL_TEST_TIER=full just cpp-test
just cpp-test-no-long-double
just js-build
just js-test
just test
just test-full
```

## Source identification and open scope

The tested source is the base revision above plus the uncommitted changes in
this worktree. The SHA-256 fingerprint below covers the **32** changed
and new source/documentation files, excluding this report. It hashes each sorted
relative path, NUL, its file bytes, NUL:

`9cc02c4aa868e334724cd619c29643c997f1906905d3a1ed71263a698141cf72`

Reproduce the fingerprint with read-only Git commands:

```python
from pathlib import Path
import hashlib, subprocess
report = 'dev/research/evaluations/2026-11-04-stop-detail-report.md'
paths = sorted(set(subprocess.check_output(['git','diff','--name-only'], text=True).splitlines()
    + subprocess.check_output(['git','ls-files','--others','--exclude-standard'], text=True).splitlines()) - {report})
h = hashlib.sha256()
for p in paths:
    h.update(p.encode() + b'\0' + Path(p).read_bytes() + b'\0')
print(h.hexdigest())
```

No implementation or validation blocker remains within the requested four
families. Artifact-pane display is open: the required `bin/show` call was
attempted after completion, but the viewer returned HTTP 403 because this
worktree is absent from its readable roots. The viewer configuration is outside
this worktree and was not changed. The report is saved at its requested path. Open follow-up scope is adoption in the other 95 families below.
For early Burr stops, deriving a finite *global* upper bound when a tail is
still unclosed remains outside this reporting change; the payload explicitly
reports infinity rather than substituting the interior segment's bound.
There is no claim of universal outward-rounded numerical enclosure.

## Remaining-family stop inventory

The table lists explicit named status branches that stop in each current
`bindings/common/distributions/<id>/fit.hpp`; “other non-Ok” denotes a generic
failure branch. Independent input, finite-result and score checks can also
stop. This is a dispatch inventory at the tested revision, not a claim that
every listed status should gain a new condition class. In particular invalid
inputs can remain ordinary errors, and attained boundary statuses that merely
warn are not listed as stops. Delegates such as `pareto2`, `invpareto` and
`pareto4` also need payload transport from their underlying Lomax/Burr fits.

| Family | Existing stop dispatch to inspect during adoption |
|---|---|
| bbinom | `BinomialLimit`, `FaceUnresolved`, `InvalidInput`, `PointMass`, `ProfileTie`, `SizeBudget`, `SizeTooSmall`, `TwoPointLimit`, `Unidentified`; other non-Ok (generic) |
| beta | `Degenerate`, `InvalidInput` |
| betapr | `BfgsFailure`, `GammaBoundary`, `InvGammaBoundary`, `InvalidStart` |
| bhatt | `NormalLimit`, `Unbounded`, `UniformLimit`; other non-Ok (generic) |
| binom | `InvalidInput`, `SizeTooSmall`, `Underdispersed`; other non-Ok (generic) |
| bnbinom | `InvalidInput`, `NegativeBinomialLimit`, `PointMass`, `PoissonLimit`; other non-Ok (generic) |
| cmb | `AdjacentLimit`, `EndpointLimit`, `FaceTie`, `FaceUnresolved`, `InvalidInput`, `NotImplemented`, `PointMass`, `Unidentified`; other non-Ok (generic) |
| cmp | `GeometricLimit`, `InvalidInput`, `PointMassLimit`, `TwoPointLimit`; other non-Ok (generic) |
| dgamma | `InvalidControl`, `InvalidData`, `IterationFailure`, `PointMass`, `TwoPointLimit` |
| dlaplace | `PointMass`; other non-Ok (generic) |
| dnorm | `InvalidControl`, `InvalidData`, `IterationFailure`, `PointMass`, `ScaleLimit`, `TwoPointLimit` |
| dunif | other non-Ok (generic) |
| dweibull | `BernoulliLimit`, `HeldBernoulliLimit`, `InvalidControl`, `InvalidData`, `IterationFailure`, `PointMass`, `TwoPointLimit`, `Unrepresentable` |
| emg | `InvalidInput`, `IterationFailure`, `NormalLimit`, `TranslatedExponentialLimit`, `Unbounded` |
| exp | `InvalidInput`, `Unbounded` |
| fatigue | `InvalidInput`, `SearchFailure`, `Unbounded` |
| foldnorm | `InvalidInput`, `Unbounded`; other non-Ok (generic) |
| fpareto | `DoubleParetoBoundary`, `InvTrGammaBoundary`, `InvalidData`, `InvalidFixed`, `LognormalBoundary`, `ParetoBoundary`, `PowerBoundary`, `ScaleOutOfRange`, `TrGammaBoundary`; other non-Ok (generic) |
| frechet | `GumbelLimit`, `InvalidData`, `NoMLE`; other non-Ok (generic) |
| gamma | `Degenerate`, `InvalidInput`, `IterationFailure`, `TransformFailure` |
| gb2 | `DoubleParetoBoundary`, `InvTrGammaBoundary`, `InvalidData`, `InvalidFixed`, `LognormalBoundary`, `ParetoBoundary`, `PowerBoundary`, `ScaleOutOfRange`, `TrGammaBoundary`; other non-Ok (generic) |
| ged | `BfgsFailure`, `InvalidShape`, `InvalidStart`, `UniformBoundary` |
| genbeta | `InvalidData`, `InvalidFixed`, `PowerBoundary`, `SearchUnresolved`, `UnitGammaBoundary` |
| geom | other non-Ok (generic) |
| gev | `InvalidData`, `NoMLE`; other non-Ok (generic) |
| gompertz | `Overdispersed`, `Unbounded`; other non-Ok (generic) |
| gpd | `InvalidData`; other non-Ok (generic) |
| gumbel | `InvalidInput`, `IterationFailure`, `Unbounded` |
| hcauchy | `InvalidData`, `ScaleBoundary`, `Unbounded`; other non-Ok (generic) |
| hnorm | `DegenerateData`; other non-Ok (generic) |
| ht | `InvalidData`, `IterationFailure`, `NormalLimit`, `ScaleBoundary`, `Unbounded`; other non-Ok (generic) |
| huber | `InvalidData`, `LaplaceLimit`, `NormalLimit`, `UnboundedLikelihood`; other non-Ok (generic) |
| hyper | `AllZero`, `BothFree`, `FaceTie`, `FaceUnresolved`, `Infeasible`, `InvalidInput`, `NumericalFailure`, `TooLarge` |
| invexp | `InvalidInput`; other non-Ok (generic) |
| invgamma | `Degenerate`, `InvalidInput`, `IterationFailure`, `TransformFailure` |
| invgauss | `InvalidInput`; other non-Ok (generic) |
| invparalogis | `BfgsFailure`, `InvalidStart`, `Unbounded` |
| invpareto | `ExponentialLimit`; other non-Ok (generic) |
| invtrgamma | `InvalidData`, `IterationFailure`, `LognormalBoundary`, `ParetoBoundary`, `ScaleOutOfRange` |
| invweibull | `InvalidInput`, `IterationFailure`, `Unbounded` |
| kumar | `Unbounded` |
| laplace | `InvalidInput`, `Unbounded`; other non-Ok (generic) |
| lgamma | `Degenerate`, `IterationFailure`, `TransformFailure` |
| lgser | `InvalidInput`, `PointMass`; other non-Ok (generic) |
| llogis | `InvalidInput`, `IterationFailure`, `Unbounded` |
| lnorm | `InvalidInput`, `Unbounded`; other non-Ok (generic) |
| logis | `IterationFailure` |
| logitnorm | `InvalidInput`, `Unbounded`; other non-Ok (generic) |
| lst | `BfgsFailure`, `InvalidDf`, `IterationFailure`, `NoMLE`, `NormalBoundary`, `ScaleBoundary`, `Unbounded` |
| naka | `Degenerate`, `InvalidInput`, `IterationFailure`, `TransformFailure` |
| nbinom | `PrecisionFailure`, `Underdispersed`; other non-Ok (generic) |
| nhyper | `FaceTie`, `FaceUnresolved`, `Infeasible`, `InvalidInput`, `NumericalFailure` |
| norm | `InvalidInput`, `Unbounded`; other non-Ok (generic) |
| nsbeta | `Degenerate`, `InvalidInput`, `IterationFailure` |
| paralogis | `BfgsFailure`, `InvalidStart`, `Unbounded` |
| pareto | `InvalidInput`; other non-Ok (generic) |
| pareto2 | `ExponentialLimit`; other non-Ok (generic) |
| pareto3 | `IterationFailure` |
| pareto4 | `AllGeOne`, `InvalidInit`, `IterationFailure`, `ParetoBoundary`, `Unbounded`, `WeibullBoundary` |
| pig | `InvalidInput`, `PoissonLimit`, `SearchFailure`, `ZeroLimit` |
| pois | other non-Ok (generic) |
| power | `Unbounded` |
| rayleigh | `ZeroLikelihood`; other non-Ok (generic) |
| sged | `BfgsFailure`, `InvalidStart`, `LeftHalfGedBoundary`, `RightHalfGedBoundary`, `ScaleBoundary`, `Unbounded`, `UniformBoundary`, `UnsupportedHold` |
| sgomp | `Collapse`, `InvalidInput`; other non-Ok (generic) |
| skellam | `Impossible`, `InvalidInput`; other non-Ok (generic) |
| slash | `CollapseLimit`, `Unbounded`; other non-Ok (generic) |
| sn | `InvalidInput`, `LeftHalfNormalBoundary`, `RightHalfNormalBoundary`; other non-Ok (generic) |
| snorm | `BfgsFailure`, `InvalidStart`, `LeftHalfNormalBoundary`, `RightHalfNormalBoundary`, `Unbounded` |
| sstd | `BfgsFailure`, `DfTwoBoundary`, `InvalidStart`, `LeftHalfNormalBoundary`, `LeftHalfTBoundary`, `RightHalfNormalBoundary`, `RightHalfTBoundary`, `ScaleBoundary`, `SnormBoundary`, `Unbounded`, `UnsupportedHold` |
| st | `InvalidInput`, `LeftHalfNormalBoundary`, `LeftHalfTBoundary`, `NumericalFailure`, `RightHalfNormalBoundary`, `RightHalfTBoundary`, `ScaleBoundary`, `SkewNormalBoundary`, `Unbounded` |
| std | `BfgsFailure`, `DfTwoBoundary`, `InvalidNu`, `InvalidStart`, `IterationFailure`, `NormalBoundary`, `ScaleBoundary`, `Unbounded` |
| tbinom | `InvalidInput`, `LowerPointMass`, `TpoisLimit`, `Unidentified`, `UpperPointMass`; other non-Ok (generic) |
| tlambda | `CollapseLimit`, `InvalidData`, `InvalidShape`, `InvalidSupport`, `Unbounded`; other non-Ok (generic) |
| tnorm | `DegenerateData`, `ExponentialBoundary`, `InvalidBounds`, `InvalidFixed`, `IterationFailure`, `OutsideBounds`, `ReflectedExponentialBoundary`, `TruncatedExponentialBoundary`, `UnboundedLocation`, `UniformBoundary`; other non-Ok (generic) |
| tpois | `InvalidInput`, `LowerPointMass`, `Unidentified`, `UpperPointMass`; other non-Ok (generic) |
| trgamma | `InvalidData`, `IterationFailure`, `LognormalBoundary`, `PowerBoundary`, `ScaleOutOfRange` |
| triang | `Incompatible`, `InvalidInput`, `IterationFailure`, `Unbounded` |
| unif | `Incompatible`, `InvalidInput`, `Unbounded` |
| weibull | `InvalidInput`, `IterationFailure`, `Unbounded` |
| yulesimon | `InvalidInput`, `PointMass`; other non-Ok (generic) |
| zeta | `InvalidInput`, `PointMass`; other non-Ok (generic) |
| zib | `InvalidInput`, `InvalidSize`, `IterationFailure`, `ZipLimit` |
| zinb | `InvalidInput`, `InvalidSize`, `PoissonLimit`, `ZipLimit`; other non-Ok (generic) |
| zip | `InvalidInput`, `ZeroLimit`; other non-Ok (generic) |
| zipf | `AllOnes`, `InvalidInput`, `PointMass`, `ShapeNegative`; other non-Ok (generic) |
| zmbinom | `InvalidInput`, `InvalidSize`, `IterationFailure`, `PoissonLimit` |
| zmgeom | `InvalidInput` |
| zmlogarithmic | `InvalidInput` |
| zmnbinom | `InvalidInput`, `InvalidSize`, `IterationFailure`, `PoissonLimit` |
| zmpois | `InvalidInput` |
| ztbinom | `InvalidInput`, `InvalidSize`, `IterationFailure`, `PoissonLimit` |
| ztgeom | `InvalidInput` |
| ztnbinom | `InvalidInput`, `InvalidSize`, `IterationFailure`, `PoissonLimit` |
| ztpois | `InvalidInput` |
