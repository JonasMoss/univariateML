# TASK-134: dpqr boundary findings

2026-10-31. Source revision `7cc7ac2925aa080dd7a526e9fdf3cfc0dccb09f0`
plus the uncommitted changes described here. No commits were made. Goldens,
fitters, shared special/core headers and code generators are unchanged.
Generated R files were rebuilt only through the standard test recipes.
The source findings are in [TASK-106](2026-10-31-truth-boundaries-report.md).

## Fixes and mathematical status

**Triangular endpoint modes.** For `c=a`, the standardized lower CDF is
`z*(2-z)`, hence `z=p/(1+sqrt(1-p))`. The reflected identity applies at
`c=b`. The inverse selects the rationalized short distance or direct square
root as appropriate on both tails. Width scaling is combined in log
coordinates before exponentiation, including log probabilities whose ordinary
probabilities underflow. The identities are algebraic; the binary64 accuracy
checks are numerical. The sampling transform is unchanged to retain seeded
sampling behavior. All eight excluded triangular quantiles are active again.
Appended tests cover natural probabilities `1e-300, 1e-100, 1e-30, 1e-12`,
reflection, the square-root inverse and log probability -800 with width 1e300.

**Skellam log tails.** A zero second rate delegates to the Poisson CDF with
the caller's log/tail flags; a zero first rate reflects the count and reverses
the tail. This avoids taking the logarithm of an already rounded subnormal
Poisson probability. The general two-positive-rate path now switches to log
mass summation below `1e-280`, rather than only when the ordinary probability
has underflowed completely. Log-concavity of the Skellam mass sequence follows
from convolution of Poisson and reflected Poisson sequences; once descending,
the next ratio bounds the remaining ratios. A geometric remainder test at
`2e-16` of the accumulated mass replaces the former fixed log-mass cutoff.
For negative counts, the Bessel-series termwise bound
`I_(n+1)(z)/I_n(z) <= z/(2*(n+1))` bounds every remaining descending
ratio by `mu2/(1-v)`. This also certifies enormous-count single-term tails
without needing a representable next integer. Failure to certify the remainder
or advance the count returns NaN. This certificate uses computed
masses and is not an interval proof for the Bessel approximations. All three
excluded value rows at counts 174–176, rates (1,0), are active again. Appended
checks compare against TASK-106 references and test the nearby rates (1,1e-8)
and their reflection. Coupling bounds the near-face log survival between the
zero-face value minus 1e-8 and the zero-face value: the independent negative
Poisson is zero with probability exp(-1e-8), and every other shift reduces the
positive survival. Numerical tolerances are 2e-12 additive for that bound and
2e-13 relative for the frozen log references.

**Positive truncation.** Neither archived law is an attained tpois/tbinom
member under its current dossier. Both explicitly require the conditioning
event to have positive probability at a held boundary parameter; the tpois
dossier specifically distinguishes the attained ztpois boundary. Thus public
NaNs for `(lambda,a,b)=(0,0,10)` and
`(prob,a,b,size)=(0,0,10,10)` remain intentional. Their weak limits are atoms
at one, proved by the leading power of the rate/probability in the conditional
masses. Both complete independent laws remain archived, with updated reasons
in their local routes and JSON fixtures. Each retains 15 value rows and 44
quantile rows, with no reference values changed. Added R checks exercise
all d/p/q/r exclusions and attained zero atoms when the window includes zero.
The other positive-truncation families named in TASK-106 already implement
their attained atomic limits; their endpoint mismatches are fixed below.

**Endpoint policy.** Explicit finite design supports retain nominal endpoints
at lower-tail probabilities zero and one, reversed for upper-tail queries and
with the same rule in log scale. Interior probabilities invert actual mass.
R 4.6.1 returns `(0,10)` for `qbinom(c(0,1),10,prob)` at both prob=0 and
prob=1; our policy agrees. Design bounds remain visible to callers and the
endpoint queries remain continuous in probability parameters through collapse.
Interior quantiles can still jump at atoms. Zero modification includes zero
even when its mass is zero; positive truncation begins at one. Unbounded laws
that collapse to finite atoms retain their actual last atom as upper endpoint.

The implementation changes zib's shortcut ordering, makes zero-modified
families retain zero as minimum, and keeps the finite binomial design maximum
in the shared family-local zero helper. Modification wrappers explicitly pass minimum zero; the default preserves
the positive-truncation minimum, including the logarithmic-series caller. The finite-window zero-rate tpois shortcut now also respects
the design upper endpoint; an infinite window still uses its actual atom.
All 40 contract-finding quantile rows are active again: zib 8, zmbinom 12,
ztbinom 4, and zmgeom/zmlogarithmic/zmnbinom/zmpois 4 each. Additional C++ and
R endpoint regressions cover absent endpoint masses and an interior mixture
quantile. The canonical policy is in
[README_SCHEMA.md](../../../univariateML-cpp/include/cppunivariate/distributions/README_SCHEMA.md#discrete-quantile-endpoints-task-134-2026-10-31),
because step7 owns `special/tails.hpp`.

**CMB workload.** d/p/q short-circuit size-zero and probability-zero/one
atoms, with the nominal design endpoints for q. `nu=1` delegates to binomial;
size one is binomial for every nu. `nu=0` uses finite geometric sums, reflected
for positive log odds and uniform at zero log odds. Quantiles bisect on the
finite support using those direct log tails. These reductions follow exactly
from the weights and binomial/geometric series identities; floating-point
agreement is numerical. General nondegenerate members still enumerate the
full support, so no new performance guarantee is claimed for those at size
10,000,000. All 204 workload-excluded values and 42 quantiles are active again.
Tests cover size-cap atoms and finite geometric agreement with enumeration.
No new sampler algorithm was introduced; existing inverse sampling now uses
the equivalent q shortcuts. See the family dossier for formulas.

## Fixture integrity and remaining exclusions

297 individual rows were reactivated: 207 value rows and 90 quantile rows.
Every previously active row and every reactivated independent reference is
retained verbatim. Local exclusion declarations/workload filters were removed
so regeneration keeps these rows active. Semantic input fingerprints were
updated for the changed local routes; formulas and parameter grids were not
regenerated or altered. The reproducible integrity check compares each edited
table with `git show HEAD:<path>`: new active rows must equal the old active
rows followed by the old excluded rows, and parameter grids must be identical.
The script and its output are in `build/scratch/fixture-check.py` and
`fixture-check.log`. The only remaining exclusions in these families are the
two complete tpois/tbinom laws described above. No shared numerical fix is
required outside this worktree's family headers.

## Validation

Environment: Linux 7.0.0-38 x86_64, GCC 15.2.0, R 4.6.1, Python 3.14.5,
Node 22.22.1, mpmath 1.3.0, SymPy 1.14.0 and PyYAML 6.0.3.

All computations use `/tmp/umvl-cpu/run` or `/tmp/umvl-cpu/run4`, with nice 10
and one-thread BLAS/OpenMP settings. Build/full runs reserve all four slots.
Temporary/runtime paths are this worktree's `build/tmp`; logs are under
`build/scratch`. No simbox or dependency installation was used. The test recipes
assemble the generated R package and use the repository's existing R test
workflow. Existing truth/sampling tolerances and seeds are unchanged; these
are regression and numerical correctness checks, not inference calibration.

The initial C++ iteration failed on the shared helper's unintended lgser
minimum, the Skellam stagnation check at counts 1e100/1e300, a newly added
approximate-infinity assertion, and one pre-existing zib assertion of the old
endpoint policy. The helper now preserves positive-truncation defaults,
the analytic Bessel bound handles the huge counts, infinities are compared
exactly, and the zib expectation follows the new policy. No tolerance was
relaxed. The first R dev run had zero failures and eight uncaptured
`NaNs produced` warnings from the new intentional-rejection tests. Those tests
now assert both the warnings and NaN values; production behavior is unchanged.
All final runs below completed successfully through `/tmp/umvl-cpu/run4`.
R full ran once, after the clean R dev rerun and JS checks. Reported durations
are test-run times, excluding slot waits and compilation.

| Command | Final result | Test duration |
|---|---|---|
| `just cpp-test` | 105/105 dev shards pass | 36.14 s |
| `just cpp-test-no-long-double` | 126/126 shards pass, including full labels | 106.38 s |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 19,842 | 71.224 s |
| `just js-build` | successful wasm build | — |
| `just js-test` | 400/400 pass; zero failures/skips | 62.161 s |
| `just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41,288 | 169.795 s |

R full's only skip is the optional `{sn}` reference package, which was not
installed. No native macOS/Windows validation or inferential calibration is
claimed. CMB's general size-cap enumeration workload and the lack of an
interval error bound for Skellam's existing Bessel approximations remain the
limitations described above, rather than new fixture exclusions.

Final logs: `build/scratch/final-cpp-test.log`,
`final-cpp-test-no-long-double.log`, `final-test-clean.log`,
`final-js-build.log`, `final-js-test.log` and `final-test-full.log`.
`final-runs.log` records command completion timestamps; its first R dev entry
is the warning-bearing run, superseded by `final-test-clean.log`.
`fixture-check.log` records the integrity check and environment. The initial
C++ iteration is retained in `cpp-initial.log`; the first R dev run is retained
in `final-test.log`. The final whitespace check passed, and no forbidden
paths or goldens changed. No commands started for this job remain running.
