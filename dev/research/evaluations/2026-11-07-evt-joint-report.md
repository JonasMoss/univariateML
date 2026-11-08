# GEV and Fréchet free-shape certificates — TASK-116.38

Job date: 2026-11-07. Final validation completed 2026-11-08 UTC in the
`codex/evt-joint` worktree, based on
`16067f5ee0f283952daa948d80b91694e2608dd6`. No commits or board edits were made.

## Delivered scope

The GEV shape-only search now has global coverage, rather than a finite mesh.
There are also exact global resolutions for all-free two-point GEV samples,
the positive-shape half-minimum collapse equality, and its held-public-location
variant. The ordinary joint searches for general samples remain uncertified;
both families correctly retain `fit.certified: partial`. Their `fit.note`
entries distinguish the certified routes from the remaining searches.

| Requested free coefficients | Delivered result | Remaining scope |
|---|---|---|
| GEV `{xi}`, public `mu` and `sigma` held | Global scalar certificate with support-edge bounds and derivative coverage; certified estimate or numerical, tie, or work-limit stop | Conservative floating-point guards can stop on difficult support seams or unresolved comparisons |
| GEV `{mu,xi}`, `sigma` held | The held-minimum half-tie member is certified when the held scale is its attaining value | General joint search remains open |
| GEV `{sigma,xi}`, `mu` held | Exact half-minimum resolution when held `mu` equals the minimum: finite reverse-exponential member or finite unattained supremum | Other held-location joint searches remain open |
| GEV `{mu,sigma,xi}` | Unique global member for every two-point sample with minimum fraction at most one half, subject to coefficient precision guards; exact unattained half-minimum supremum for other half ties; existing strict-majority nonexistence retained | General samples with more than two distinct values remain open |
| Fréchet `{lambda,mu}`, `sigma` held | Proved effective compactness for every nonconstant sample, preserving the public scale constraint; constant-sample shape divergence retained | No implemented global joint coefficient certificate |
| Fréchet `{lambda,mu,sigma}` | Exact half-minimum finite unattained supremum; existing strict-majority nonexistence retained | General free-location/free-shape search and finite-versus-Gumbel comparison remain open |

The mathematical existence and compactness results below do **not** promote
legacy joint numerical output to a certified estimate. In particular, an
ordinary default all-free GEV fit is not globally certified by this job.

## Implemented certificates and stop semantics

### GEV shape alone

[shape_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/gev/shape_certificate.hpp)
covers the feasible fitted shape interval in `[-1,1]`. Interval jets bound the
likelihood and its first two shape derivatives. A convergent series with
explicit value/derivative remainders crosses zero shape; explicit likelihood
envelopes exclude open support-edge neighbourhoods. Monotonicity eliminates
segments, and strict local concavity plus a bracket isolates an interior
maximum. Endpoints and all surviving candidates are compared globally.
An `Ok` root enclosure must justify the requested relative coefficient width.

The claim is certified in exact arithmetic conditional on computed comparisons,
with practical floating-point margins, not outward-rounded interval arithmetic.
Likelihood comparisons use `4096 * epsilon * n`; root enclosures use
`32 * epsilon * n`. Unresolved comparisons, precision stalls and critical
support seams stop instead of returning the best scanned point. The solver
limit is `min(iterlim,100000)`; certificate evaluations are limited to 128 times
that quantity. Actual budget exhaustion is distinguished from an inner solve
failure and an unresolved numerical/tie comparison.

Failure details retain feasible candidates in original public coordinates,
a likelihood lower bound when available, and an infinite upper bound when
global coverage is unresolved. The binding-neutral GEV adapter preserves
NoMLE details and classifies budget/tie/numerical failures for both R and JS.
No new shared core header or R-only routing was needed.

Uniqueness cannot be claimed for every supported hold: if all observations
equal held `mu`, the likelihood is `-n*log(sigma)-n` for **every** fitted shape.
The certificate returns the maximizing representative `xi=0`. This is an exact
counterexample, not a numerically flat example.

### Two-point GEV and the half-minimum equality

Let `m < M`, `d=M-m`, and let the minimum occur with fraction `p <= 1/2`.
The all-free GEV maximum is the unique reverse-exponential member

$$\xi=-1,\qquad \sigma=pd,\qquad \mu=M-pd,\qquad
\ell_{\max}=-n[1+\log(pd)].$$

Scale profiling in reflected-Weibull and Fréchet coordinates gives weighted
pair likelihood ratios bounded by the strict unweighted pair bound. The
Gumbel limit is strictly smaller for finite scale. Equality is possible only
at reflected-Weibull shape one with endpoint `M`. The derivation is in the
[proof note](2026-11-07-evt-joint-data/nuisance-bounds.md#all-free-two-point-global-member)
and GEV dossier. Counts compute `pd` directly, avoiding cancellation in
`M-mean(x)`.

For a general half-minimum sample (`k=n/2`), the common sharp global bound is

$$A=n\log 2-n-2\sum_{x_i>m}\log(x_i-m).$$

It is the attained GEV likelihood only for two distinct values with equal
counts. For all other GEV half ties it is an unattained supremum. For the
all-free Fréchet model it is always unattained. These stops now carry the
finite global bound; the old numerical half-face comparison is unnecessary.
The Fréchet delegate propagates the certificate details.

With public GEV location held at `m`, use the sharp uniform standardized
density bounds `h_xi(z)<=1` and `z^2*h_xi(z)<=1`. The half-tie bound becomes

$$A_m=-k-2\sum_{x_i>m}\log(x_i-m).$$

It is attained exactly when all nonminimum observations have one common value
`M`, at `xi=-1` and `sigma=M-m`. Otherwise the `xi=1`, `sigma` tending to zero
path approaches it without attaining it. This calculation respects the public
hold. The same finite member remains globally valid if scale is also held at
its attaining value.

The analytic two-point route includes floating-point coefficient guards:
stable half-tie midpoints, subnormal halving residual checks, a TwoDiff endpoint
feasibility check, bounded scale corrections, and a conservative relative
location-error allowance. It can correctly stop numerically even when a
finite mathematical maximum is known. For example, `{-9,1,1,1,1,1,1,1,1,1}`
triggers the near-zero-location precision guard. A nonrepresentable subnormal
midpoint is not silently replaced with a rounded candidate. Appended tests
cover these stops and a nearly cancelling, large, opposite-sign midpoint.

## Proved joint ingredients and exact open entries

The retained [uniform nuisance and face proof](2026-11-07-evt-joint-data/nuisance-bounds.md)
is algebraic evidence, independent of the numerical probes. It supplies:

- Uniform density and exponential left-tail bounds; explicit scale upper and
  location bounds from a feasible incumbent.
- Positive scale lower bounds using order statistics, plus a minimum/left-tail
  split covering **every** sample whose minimum multiplicity is below half,
  including samples with a nonminimum majority.
- Finite GEV attainment for minimum minority, the sharp half-minimum
  classification above, and unbounded likelihood for minimum majority on the
  fitted branch. Existence does not imply uniqueness or certified coefficients.
- A uniform one-sided reverse-exponential neighbourhood bound using a common
  upper endpoint, and a uniform two-sided Gumbel remainder on a compact
  nuisance box. Their domains and restrictions under public holds are stated.
- Strictly concave inner natural-coordinate profiles for fixed nonpositive
  GEV shape, including the Gumbel member.
- Effective compactness for held-scale Fréchet with free location and shape
  for every nonconstant sample. The map enforces
  `mu_G=mu_F+sigma_F`, `sigma_G=sigma_F/lambda`, `xi=1/lambda`;
  an unconstrained GEV certificate is not substituted for that surface.

**Open J1 — general GEV `{mu,xi}` with scale held.** Missing is a complete
bounded profile/root inclusion procedure across the moving support and shape
faces, followed by a global comparison and relative coefficient enclosure.
The old joint scan still discovers roots numerically and cannot exclude a
missed maximum. The scalar certificate does not cover this two-coordinate
problem merely because one can solve some fixed-shape subproblems.

**Open J2 — general GEV `{sigma,xi}` with location held.** The new scale
bounds and held-minimum classification settle nuisance escape and the exact
half face, but the remaining outer shape/scale search has no complete root
count or global interval coverage. It needs a bounded, metered enclosure of
all competing profiles, not a denser scale/shape mesh.

**Open J3 — general all-free GEV with more than two distinct values.**
A compact incumbent superlevel box and face upper remainders are now proved
for minimum minority. Missing is a practical complete outer certificate and
coefficient isolation, especially on the positive-shape side. The analytic
reverse-exponential and strictly concave nonpositive-shape inner problems are
useful components, but do not certify the outer maximization.

**Open J4 — held-scale Fréchet free shape and location.** Compactness is
proved for all nonconstant data, including ties, and constants diverge. A
complete constrained two-coordinate global solver with coefficient inclusion
is still missing. Changing to unconstrained all-free GEV would violate the
held public scale.

**Open J5 — all-free Fréchet outside the resolved half tie.** In addition to
finite-root coverage, a practical finite-versus-Gumbel-face comparison is
missing. Public Fréchet coefficients diverge along that face, although mapped
GEV nuisance coordinates stay bounded. Current numerical `GumbelLimit`
selection remains labelled numerical in the dossier.

The obstacle is not a requirement for outward rounding. The proved bounds
are too conservative for direct global subdivision at `1e-10`. On the GEV
500-point golden sample, strengthened scale bounds improve the elementary
lower bound from `7.04e-13` to `0.001587`, but the whole-box standardized radius
is still about 18,486. The conservative Gumbel-error constant has logarithm
36,991.5; the corresponding shape slab for one log-likelihood unit has
log-width about -36,997.7. The analogous Fréchet values are worse. These
[computed bound sizes](2026-11-07-evt-joint-data/bound-scales.json) show why
feeding the uniform box straight into a generic grid is ineffective.
Profile-dependent feasible regions and tighter face enclosures are needed
before complete outer enumeration is practical. No general joint uniqueness
claim is made.

## Numerical validation

All commands below completed successfully on the final fitting code. Long
suites used simbox as requested; workstation checks used `/tmp/umvl-cpu/run4`.
The wrappers run at nice level 10, cap workers at four locally, and pin
BLAS/OpenMP to one thread per worker. No dependencies were installed.

| Final command/check | Machine | Result |
|---|---|---|
| Targeted GEV/Fréchet native tests | simbox | 44 cases, 41,938 assertions, all passed |
| `just cpp-test` | simbox | 106/106; 78.76 s test time |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129; 201.35 s |
| `just cpp-test-no-long-double` | simbox | 129/129; 206.39 s |
| `just test` | simbox | 21,483 passes, 154 skips, zero failures/warnings; 101.8 s |
| `just test-full` | simbox | 44,298 passes, zero skips/failures/warnings; 321.1 s |
| `just cpp-test-no-long-double` | workstation | 129/129; 238.36 s |
| `just js-build` and `just js-test` | workstation | build green, 493/493 tests; test time 156.0 s |
| `just site-check` | workstation | green; 99 family pages and 99 compiled C++17 snippets, browser checks and build-time MathML |
| Independent bound/reference checks and benchmark | workstation | all assertions passed |

Native tests and per-family R tests were appended; the shared JS test file
was also changed append-only. Tests cover support endpoints, zero shape,
flat/nonunique likelihood, precision and work stops, exact collapse bounds,
held-coordinate preservation, two-point count accuracy, and both bindings.
Compiler warnings already present in other families and missing optional
shell startup files did not produce test failures; R test warnings were zero.

Reproduction commands are retained in
[targeted.sh](2026-11-07-evt-joint-data/targeted.sh),
[run-suites.sh](2026-11-07-evt-joint-data/run-suites.sh) and
[workstation-suites.sh](2026-11-07-evt-joint-data/workstation-suites.sh).
The final logs are
[simbox](2026-11-07-evt-joint-data/simbox-validation.log.gz),
[workstation](2026-11-07-evt-joint-data/workstation-validation.log.gz) and
[targeted](2026-11-07-evt-joint-data/targeted-simbox.log.gz).

The [interval probe](2026-11-07-evt-joint-data/interval-check.json) checked 900
value/derivative bounds against 60-digit mpmath evaluations, with an absolute
comparison margin of `1e-10`. The
[pair/face probe](2026-11-07-evt-joint-data/pair-check.json), seed 11638, checked
20,000 unweighted and 20,000 weighted pair bounds, 3,000 each of the density,
reverse-face and Gumbel-face bounds, and 1,292 left-tail values at margin
`1e-12`. These are numerical cross-checks of the stated algebra, not proofs
by random sampling.

## Golden preservation and timing cost

The original GEV and Fréchet golden fixtures were not edited. Default
coefficients and likelihoods at `n=7` and on the 500-point goldens are
bit-identical in the paired benchmark, and goldens satisfy `1e-10` tolerance.
The independent 50-digit shape-only score root under the exact held golden
coefficients is
`0.30843601546575407414414057145477165268237668091511`;
the certificate's relative error is `1.431e-11`.

[benchmark.sh](2026-11-07-evt-joint-data/benchmark.sh) reconstructs the two
baseline family headers from the pinned base using read-only `git show`, then
builds before/after executables with C++17 and `-O2`. Times below are median
microseconds per fit from three paired batches (100 fits per batch, except
three fits for shape-only `n=500` and two-point `n=10001`). They are a single
workstation measurement, not a broad performance claim.

| Workload | Before, microseconds | After, microseconds |
|---|---:|---:|
| GEV default n=7 | 23.703 | 47.467 |
| GEV shape-only n=7 | 17.415 | 2201.863 |
| GEV half tie n=4 | 11.165 | 0.076 |
| Frechet default n=7 | 49.393 | 48.254 |
| Frechet half tie n=4 | 28.351 | 0.072 |
| GEV default n=500 | 1399.330 | 1421.217 |
| Frechet default n=500 | 3170.691 | 3193.756 |
| GEV shape-only n=500 | 1017.869 | 232705.848 |
| GEV two-point n=10001 | 15765.110 | 87.830 |

The scalar certificate costs about 126 times the old shape mesh at `n=7`
and 229 times at `n=500`. This is a substantial performance regression for
that conditional fit and remains an optimization opportunity. General default
500-point timings changed by about 1.6% for GEV and 0.7% for Fréchet; the
small GEV default was about twice as slow in this measurement, so no uniform
zero-overhead claim is warranted. Exact half-tie stops and the analytic
large two-point route are faster.

For the two-point sample with one zero and 10,000 ones, the baseline reported
`Ok` with scale relative error `1.130e-9`; the new count-based certificate has
zero error against double `1/10001`. This deliberate change is outside the
frozen continuous golden samples and corrects an accuracy-contract failure.
Raw batches and checks are retained beside the
[timing summary](2026-11-07-evt-joint-data/timing-summary.json).

## Change boundary

Changes are confined to the GEV/Fréchet fitters, dossiers and tests, GEV's
binding-neutral adapter, appended JS tests, and this report's evidence.
No `core/`, `special/`, `dpqr.hpp`, truth fixtures, code generators, generated
R d/p/q/r, NEWS, STATUS, KNOWN_ISSUES or roadmap file was changed. No public
fitter was removed or renamed. The unrelated screenshot regenerated by
`site-check` was restored byte-for-byte from the base.

The [source manifest](2026-11-07-evt-joint-data/source-state.txt) records final
source hashes and unchanged fixture hashes. `git diff --check` is green.
