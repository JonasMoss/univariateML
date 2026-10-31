# Step 7: numerical simplification review, phase A (TASK-101.7)

Source base: `91d538253b6c448be78eedadde8abd9c44fb99c6`, branch `codex/step7`.
This report describes the uncommitted phase A changes in this worktree.
No goldens/truth fixtures, fitter arithmetic, R binding sources, R code generators, board records or roadmap/status files were changed.

## Inventory and decisions

The [canonical numerical inventory](2026-10-30-numerical-inventory.md) is
updated in place. Its [generated table](2026-10-31-numerical-call-paths.md)
and [JSON](2026-10-31-numerical-call-paths.json) cover all 98 families' direct
standard/seam calls, the Boost implementations reachable through named shared-function bodies
and explicit family delegates, named own helpers and shared include closure. Solver/tail markers
include source locations. This lexical scan is reproducible and deliberately
labels include availability separately from executed calls. It does not prove
conditional runtime reachability or resolve all unqualified/template calls.
Local helper headers and family delegation remain explicit review locations.

| Candidate | Phase A decision | Evidence and reason |
|---|---|---|
| Boost.Math dependency | Keep provisionally | Core seam supplies incomplete beta/gamma and inverses, polygamma, zeta, Owen T, distribution CDFs/quantiles and gamma-ratio primitives. Libm and custom continuations remain distinct paths. Removing Boost would require several new algorithms and stronger validation. |
| Lambert W | Keep legacy path provisionally; propose a caller-level replacement/reformulation | Boost is faster on this workload but rounded results differ and both implementations lose forward accuracy at the branch point. Custom W−1 has severe failures and no live family caller. No silent global substitution. |
| Repeated scalar solvers | Propose shared safeguarded abstraction, retaining family-specific acceptance | Existing Newton helpers preserve only two step conventions. Most local roots require a bracket, different coordinates, a hybrid step or profile/global comparisons; they cannot be replaced byte-identically by changing a helper name. |
| Tail helpers | Keep existing shared implementation; propose family consolidation individually | `fs_log1mexp` already delegates to `log1mexp`. Sampled finite double helpers often agree bitwise, but endpoints, NaNs, extended precision and Mills approximations differ. |
| GB2 beta-tail machinery | Relocate to `special/beta_tails.hpp` | Five direct include users; 12 affected family dpqr include closures. All 12,752 existing fixture d/p/q evaluations compare byte-identically against HEAD. |

The [shared-routine note](2026-10-31-step7-routines.md) is linked from core
Lambert W, Newton, inverse digamma, BFGS, finite-difference, multistart,
reductions and tie-collapse headers, and from special/tails, special/special,
CMP, skew densities and the relocated beta header. It retains original
rationales where known and explicitly marks unknown ones. It records concrete
legacy validation gaps rather than asserting convergence from golden passes.
Family dossiers remain the canonical descriptions of their objectives and
searches; no family solver was replaced in this phase.

## Measurements

Environment: Linux x86_64, glibc 2.43, g++ 15.2.0, C++17, BH Boost 1.90,
Python 3.14.5, mpmath 1.3.0 at 100 decimal digits. Probe compiled with `-O3`;
compatibility comparison with `-O1`. CPU jobs use the shared slot wrappers,
`nice -n 10`, and one BLAS/OpenMP thread. No random seeds are needed: all
probe inputs are deterministic. Absolute errors below compare the exact
binary64 argument promoted to mpmath, not its printed decimal approximation.

### Lambert W

The grid combines ZIP arguments `-g exp(-g)` from g near 1 through underflow,
53 geometric branch-point offsets, logarithmically spaced negative inputs to
subnormal range, zero, invalid arguments and positive W0 stress points.
There are 353 valid W0 inputs and 347 valid W−1 inputs (the latter has no
current fitter caller). The rounded double `-1/e` can fall below the true
real-domain boundary; the reference-domain check uses 100-digit `-1/e`.

| Metric | Own | Boost with `NoThrowPolicy` |
|---|---:|---:|
| W0 nonfinite results on valid grid | 0/353 | 0/353 |
| W0 largest absolute error | 1.071e-8 | 5.769e-9 |
| W−1 nonfinite results on valid grid | 151/347 | 6/347 |
| W−1 largest finite absolute error | 503.387 | 5.769e-9 |
| W0 ns/call, 200,000 calls | 49.73 | 21.64 |
| W−1 ns/call, 200,000 calls | 271.19 | 29.74 |

Timing inputs are `g=1.001+0.02*i`, i=0,...,999, repeated 200 times with a
volatile accumulator, after the accuracy grid. These single-run local timings
are exploratory performance measurements, not confidence intervals or fit
benchmarks. The invalid W−1 output contributes to the own-branch timing.

At `z=-0.3678794411714423`, W0 reference is `-0.9999999846957459`, own returns
`-0.9999999954031741` and Boost `-0.9999999789265759`. W−1 at
`z=-0.36754409778908503` should be `-1.0433178274150032`; own returns
`502.3434369388715`, crossing branches. At zero budget, own W0(-0.3) returns
−0.3 unchecked. At z=0, own W−1 returns NaN and Boost returns −Inf;
at z=−0.4 both branches return NaN in both implementations, without an
exception in this probe. Of valid finite comparisons, only 150 W0 and 149 W−1 outputs
are exactly equal between implementations. Boost's six valid-grid nonfinite
W−1 cases are in the subnormal-input regime. The experiment completed without
exceptions under the explicit policy; this is sampled behavior, not a
proof that all internal Boost paths honor the policy.

The custom W0 caller is ZIP only. The old claim that lgser uses W−1 is
obsolete: lgser explicitly solves the transformed monotone equation to avoid
branch-point cancellation. A stale comment in `ztpois/profile.hpp` also
mentions a Lambert iteration; the executable path calls the current lgser
solver and an independent probability bisection. Correct that comment during
a coordinated phase B family edit. An isolated W0 replacement would not fix
`g+W0(-g exp(-g))` cancellation near g=1 or information lost when forming z.
Prefer reviewing ZIP's rate equation using its original sufficient statistic
alongside Boost, not merely substituting special-function calls. The
[Boost documentation](https://www.boost.org/doc/libs/1_88_0/libs/math/doc/html/math_toolkit/lambert_w.html)
also identifies branch-point conditioning and policy-controlled domain errors.
No replacement is accepted in phase A. The function-level grid does not
establish alternative ZIP fitter compatibility: a caller-level comparison
of goldens, near-boundary sufficient statistics and held subsets remains
a concrete validation gap.

### Scalar Newton and inverse digamma

Eighteen deterministic quadratic-root trials (starts 0.1, 1, 10; tolerances
1e-4, 1e-8, 1e-12; relative/absolute rules) give identical iterates and counts
between explicit historical loops and the two shared Newton helpers. This
checks the loop mechanics, not family-level replacement compatibility.
A relative solve at zero with zero quotient does not converge (11 iterations
reported for a budget of 10); an absolute zero quotient reports convergence
at its starting point without seeing a score. These demonstrate why step-only
success cannot be the shared safeguarded solver's acceptance contract.

Inverse digamma is called only by Kumaraswamy initialization. Twelve of
thirteen probe inputs from −1e6 to 700 return positive finite values; x=710
returns NaN (the inverse itself exceeds binary64 range). Absolute digamma
residual is 1.442e-11 at −1e6 and at most 1.170e-14 on the remaining finite
points; this is not a forward-error enclosure. The fixed polishing budget
has no general convergence certificate and the 1e-14 early exit is not
verified on return. Keep its numerical path during phase A; propose an
explicit range/failure contract before broader reuse.

### Tail overlap

For 20,000 negative arguments `x=-exp(-700+1400*i/20000)`, i=1,...,20000,
foldnorm and Fernandez–Steel complements and the `fs_log1mexp` alias agree
byte-for-byte with the shared double primitive: 60,000/60,000 comparisons.
For pairs `(-0.001*i,-0.003*i)`, foldnorm, Skellam and PIG log-add helpers
agree with shared `log_sum_exp`: 60,000/60,000 comparisons. These grids
establish finite-input sampled equivalence only. The alias has no duplicate
body to consolidate. At +infinity the shared complement and foldnorm return
NaNs with differing signs; log-add infinity and NaN precedence also differ.
EMG uses extended intermediates and a different branch constant; replacing
it with double arithmetic is not justified. Normal log-upper/Mills helpers
use different cutoffs and depths (foldnorm 26/17-term asymptotic; shared 8/80-level
fraction; EMG 12/40-level fraction). Those require accuracy/range comparisons,
not deduplication based on names.

The additional tail probe compares GED's local and shared gamma log tails
on 100 cases: shapes {0.01,0.1,1,10,100}, log x in
{−1000,−100,−36,−10,−1,0,1,3,5,10}, and both tails. Both paths return finite
log values throughout, but only 66/100 pairs are exactly equal. Maximum
absolute error against 100-digit mpmath is 4.1861e-12 for both paths at
shape=100, log x=−1000, where the log probability is about −100363.7394.
Thus sampled accuracy supports retaining both, not a byte-identical global
substitution.

For normal log upper tails at z in
{0,1,7.999,8,11.999,12,25.999,26,37,100,1000,1e150}, maximum
`abs(log_tail-reference)/max(1,abs(reference))` is 2.0301e-16 for shared
`normal_log_upper`, 2.8322e-16 for foldnorm's helper, and 9.3741e-17 for
both the fs and extended EMG paths. This is sampled log-tail accuracy, not
a relative probability-error bound or a proof of all-input equivalence.
These different cutoffs/depths are accuracy/range choices rather than
identical implementations; retain pending family-level comparison.

### Beta bridge relocation

Direct include sites changed: beta/dpqr, GB2/dpqr, genbeta/dpqr,
betapr/beta_quantile and lst/tails. Namespace `missing_detail` and every
arithmetic token are retained; there is no compatibility shim at the old
internal path; external code including that internal path must update its
include. The standalone library exports the new header automatically
through its existing header-tree handling. No Boost includes move into a
family. Genbeta's fitter still depends on GB2 independently of this change.

The baseline is extracted read-only from `git archive HEAD` to this worktree's
ignored scratch directory. Identical generated programs evaluate each existing
affected family's truth density (log), CDF (both tails and log flags), and
quantile rows before/after the relocation. Hexadecimal output, including signed
zero, is compared as bytes. Families and result counts:
bbinom 868, beta 953, betapr 1311, fpareto 973, GB2 1241, genbeta 1091,
hcauchy 596, ht 1033, lst 1231, nsbeta 953, sstd 1251, std 1251.
All 12,752 agree; output SHA256
`32745ee68f1029763ac120645c7c605252a7d02996321e55c341b4db570a3bb9`.
This directly establishes sampled output compatibility; fitter arithmetic
is unchanged by construction and existing golden regressions remain required.

## Phase B proposals by family

Source review confirms **46 families with local continuous scalar fitting-root loops**
(Newton, bracket bisection, safeguarded Newton or Brent), including held paths
and snorm's polynomial isolation. Gamma/fatigue count because they also have
local fallback/held solves, although their main path already calls shared
Newton. Integer-size and discrete quantile searches are separate contracts;
multidimensional Newton (even when holds reduce its dimension) and scalar objective minimization are not counted as
scalar root solvers. Delegating families such as invweibull are not counted
again for a root implemented in another family's header. This is a source
inventory, not a claim that each loop has a global certificate.

The table gives a disposition for each confirmed family. The generated JSON
contains local headers and source nominations; the implementation is the
final authority, since a comment-based search alone misses several loops.
All proposed extractions must first run actual family goldens and held subsets
under before/after hexadecimal comparison. The 18-loop synthetic check above
does **not** establish any of these family substitutions as byte-identical.
No per-family replacement is accepted yet.

| Family | Local root contract / proposed action | Expected benefit | Risk / acceptance requirement |
|---|---|---|---|
| bbinom | Extract only bracketed Newton refinement in `maximise_on_grid` | Shared finite/bracket failure diagnostics | Preserve grid, global bounds, summed score rescue, iteration accounting; local root does not certify other intervals |
| beta | Extract held-shape bracket solve; retain joint damped Newton | Less repeated monotone-root control | Preserve positive shapes and total score; joint Hessian solve is not scalar |
| betapr | Extract held-shape/scale score bisection separately from beta quantile bridge | Common bracket checks | Fixed-count midpoint output can change; retain beta inverse candidate validation and extreme-tail scaling |
| bhatt | Extract location-profile safeguarded Newton | Reuse finite/bracket diagnostics | Extended score sums, strict convolution log-concavity and 60-step legacy fallback need explicit acceptance; outer simplex stays local |
| burr | Extract decreasing-score log-coordinate bisection | Common adjacency and exhaustion handling | Preserve all profile/boundary comparisons and existing score acceptance |
| cauchy | Extract conditional log-scale score bisection only | Common monotone conditional solver | Retain joint two-dimensional Newton and location candidate isolation; do not assume joint concavity |
| cmb | Extract mean-matching safeguarded Newton and dispersion score bisection | Common nested root statuses | Preserve finite-table moments and endpoint/adjacent-support faces; current inner fixed-budget return needs a checked failure contract |
| dgamma | Extract profiled log-rate safeguarded Newton | Reuse bracket/budget handling | Keep conditional cell moments, shape-profile candidates and discretization; Newton fraction alone does not preserve rate-score validity |
| dweibull | Candidate source for safeguarded Newton/Brent contract | Reusable explicit root/budget result | Preserve bracket signs, derivative safeguard, log coordinates and evaluation counts; profile uniqueness is family mathematics |
| emg | Extract location-profile safeguarded Newton | Common accepted-root result | Mills-ratio score/derivative use extended precision; preserve strict-concavity bracket and normal/exponential limit comparisons |
| fatigue | Keep existing relative Newton; extract geometric fallback | Avoid another positive-domain bracket loop | Arithmetic/geometric midpoint and start must remain distinct; preserve likelihood comparison |
| foldnorm | Extract local scalar score root refinement | Common bracket termination | Multiple roots and interval screening remain family-owned; geometric/log1p coordinates affect rounding |
| frechet | Extract only bracket-refinement mechanics | Share GEV-related mechanics | Endpoint restrictions, held mu and multiple profile candidates cannot be generalized away |
| gamma | Keep absolute Newton; extract positive shape fallback and held-shape solve | Better shared budget/domain diagnostics | Geometric midpoint and cancellation-safe `log(k)-psi(k)` are essential; preserve default path |
| ged | Extract held-shape location-score bisection | Common failure reporting | nu=1 nonsmooth/flat cases and support symmetry require family treatment |
| genbeta | Extract envelope-score sign-change refinement | Less repeated bisection | Keep all scanned brackets and boundary comparisons; no generic unimodality assumption |
| gev | Extract bracketed score interpolation only | Reuse guarded interpolation | Restricted endpoint, profile validity and multiple roots remain separate |
| gompertz | Propose a hybrid-step Newton variant plus held log-root helper | Centralized iteration status | Current `abs(step)<abs(new)*tol+tol` differs from both shared variants; standardized data and domain exits must survive |
| gpd | Extract scalar profile roots only | Common bracket checks | Nonconcave profile/endpoint candidates require all brackets and boundary comparison |
| gumbel | Propose exact historical log-scale Newton policy plus conditional bracket solve | Shared mechanics for Weibull transforms | Current test uses new theta and multiplied-step arithmetic; existing relative helper uses old theta and division, so results can change |
| ht | Extract held-location/scale profile bisection | Common termination/status | Preserve known-vs-fitted nuisance conditions and t scale conversion |
| huber | Extract location score bisection only | Common finite/adjacency result | Preserve nonsmooth zero-score interval and midpoint convention; joint scale/location Newton stays local |
| kumar | Propose hybrid Newton preserving both stages; extract profile/held brackets | Remove repeated loop mechanics; document inverse-digamma failure | Initializer stage differs from ML stage; preserve extreme-data fallback and score gate |
| lgser | Extract positive transformed-equation geometric bisection | Common bracket certificate | Keep cancellation-free `q(u)` series and exact count excess; do not restore W−1 formula |
| llogis | Extract monotone conditional log-location/log-shape bisections | Shared finite/exhaustion checks | Absolute log-coordinate width and fixed iteration behavior affect goldens |
| logis | Extract only conditional one-dimensional solves | Common monotone bracket handling | Joint location/log-scale Newton is two-dimensional and stays local |
| lomax | Retain custom safeguarded default candidate; extract held-score bisection | Shared conditional solve | Legacy default candidate selection and profile/global comparisons protect fits; damping is not plain Newton |
| lst | Extract held-location conditional log-scale score root | Common bracket termination | Preserve degrees-of-freedom restrictions, exact scale parametrization and default multistart fit |
| naka | Extract conditional log-shape bisection | Share gamma-root mechanics | Shape lower bound and gamma cancellation handling differ from unrestricted gamma |
| nbinom | Keep legacy Newton first; extract log/geometric fallback and held solve | Common domain/failure handling | Preserve Poisson escape decision, size range, total score and iteration convention |
| paralogis | Extract historical safeguarded shape-score Newton | Reusable bracketed Newton policy | Preserve shape-scaled score tolerance, positive shape and finite objective; full joint BFGS remains separate |
| skellam | Extract held-mean score bisection | Shared bracket/budget diagnostics | Mean-zero face and Bessel approximations affect score; do not replace profile objective maximization with a root |
| slash | Extract held-location score refinement | Shared adjacency/finite handling | Keep per-interval candidate selection; a found root need not be the global maximum |
| sn | Extract local stationary-point bisection | Shared adjacency termination | Preserve location profile/multiple candidate search and skew-normal limits |
| snorm | Keep recursive polynomial root isolation; potentially share extended-real interval refinement | Fewer repeated bisection steps | Must retain derivative-root cuts, multiple roots and extended precision; ordinary scalar Newton is unsuitable |
| sstd | Extract held-scale/profile root loops from `hold_search.hpp` | Common status/adjacency behavior | Objective/profile nonconcavity, coordinate transforms and boundary comparisons remain family-owned |
| tnorm | Keep joint moment Newton; review conditional one-dimensional Newton separately | Common root result if safeguards can match | Truncation moments, positive scale and degeneracy require model-specific validity checks |
| tpois | Extract mean matching from `profile.hpp` | Shared natural-parameter root and status | Mode-centered truncated mass calculation and finite/infinite endpoints remain family-owned; tbinom delegates here and is concurrently edited |
| trgamma | Review Brent refinement as another source for shared bracket solver | Reuse guarded interpolation | Preserve envelope slope in log t and global/boundary search; evaluation counts and result rounding matter |
| weibull | Keep Gumbel delegation; extract held-scale shape bisection | Share conditional solver | Default fits must continue through unchanged Gumbel path; log transform Jacobian remains |
| yulesimon | Extract log-rho score bracket/bisection | Common monotone root failure | Preserve point-mass escape, unbounded bracket growth and score normalization |
| zeta | Extract bracketed exponential-family score root | Common bracket/budget result | Harmonic-moment approximation accuracy is separate from solver convergence |
| zib | Extract fixed-size probability score roots only | Consistent root failure diagnostics | Integer size search is owned by size-search/diagnostics coordination; mixture boundary candidates remain |
| zip | Review stable sufficient-statistic rate root versus Boost W0; extract held-inflation bracket only after existence audit | Improve branch-point stability; eliminate unchecked W iteration | Finite fixed-inflation likelihood need not be concave; retain every profile candidate; respect concurrent statuses job |
| zipf | Propose hybrid Newton plus checked fallback bracket | Common failure and bracket checks | Euler–Maclaurin moments and uniform/point-mass boundaries remain; current fallback cap is not a certificate |
| ztpois | Extract mean-equation probability/rate bisections in mle/profile headers | Reuse several zero-truncated conditional equations | Preserve exact point-mass members and log-domain denominators; 100-step fallback returns currently need explicit verification |

There are also **13 owners of local continuous distribution-inversion loops**
(in `dpqr.hpp`, `tails.hpp` or `beta_quantile.hpp`), with nine overlapping the
46 fitting-root families: **50 distinct families** across these two categories.
Tukey lambda inverts its quantile to obtain a CDF; the others here invert CDFs.
Discrete quantile lattice searches remain a separate category. Shared root
implementations in core/special and delegated family calls are not counted
again as local implementations. No drop-in byte-identical replacement by
`core/newton.hpp` is established for any of these inversion paths.

| Inversion owner | Phase B proposal / expected benefit | Risk / acceptance requirement |
|---|---|---|
| betapr (`beta_quantile.hpp`) | Share checked log-odds bracket refinement | Retain candidate residual validation and scale-rescuable finite range; existing 120-step fallback needs explicit exhaustion handling |
| bhatt | Share monotone quantile bracket/status | Keep convolution tail evaluation, affine range rescue and endpoint behavior; legacy fixed cap is not a proof |
| emg | Share safeguarded quantile Newton | Keep extended Mills terms, cancellation-sensitive log CDF and bracket checks; preserving operation order is required |
| foldnorm | Share quantile bisection/adjacency result | CDF quadrature, competing tail approximations and origin endpoint remain family-specific |
| ged (`tails.hpp`) | Share log-gamma inverse control flow | Local gamma CDF differs from the shared gamma tail; compare accuracy and cap/failure behavior before combining them |
| invgauss | Share log-scale quantile bracketing | Two normal-tail terms can cancel; a root solver cannot repair an inaccurate/nonfinite CDF callback |
| logitnorm | Share adjacency-certified logit-coordinate inversion | Preserve scale-dependent range and invalid-probability diagnostics; do not exponentiate deep input log tails |
| naka | Share monotone log-coordinate inversion | Preserve scale rescue and shape-specific gamma continuation; current 100-step return needs checked acceptance |
| sgomp | Share root in the exponential coordinate | Keep shape-zero member, log-tail representation and 220-step range behavior; plain Newton has no matching contract |
| slash | Share monotone quantile bracketing | Retain affine rescue and accurate tail callback; a width criterion must be tied to the final coordinate |
| sn | Share quantile bracket refinement | Owen-T/cancellation tails, transformed scale and rounding at support endpoints require separate checks |
| tlambda | Share inverse-quantile Newton/bracket result | Both log-deficit and log-magnitude routes are protected; tail shape/support conditions and central symmetry differ |
| tnorm | Share safeguarded conditional-quantile root | Truncated-normal normalization, nearby finite endpoints and probability differences must retain their stable representations |

Separate scalar objective-search candidates are the golden-section variants
in `ztpois/profile.hpp` and `ztbinom/mle.hpp`, scan/minimize helpers under
snorm/sstd, ternary refinement of sged half-family profiles, and one/two-
dimensional simplex searches for EMG/Bhattacharjee. They could share loop
mechanics after their unimodality, multiple-basin selection, budget and score
contracts are compared. These are proposals only; an objective optimum cannot
be accepted by a generic root solver's sign test.

Other root/search overlap is intentionally separate: binom/hyper integer
searches, tbinom's concurrently edited size search, discrete quantiles, and
cauchy/logis/huber joint Newton. invweibull delegates its default scalar work
to Gumbel; other transformed families should inherit an accepted change only
after testing their Jacobians, fixed subsets and boundary mappings.

A proposed shared safeguarded solver should return root, enclosing bracket,
iterations/evaluations and an explicit status (invalid bracket, nonfinite
callback, exhausted budget, accepted residual, adjacent endpoints). It must
accept coordinate/midpoint and stopping policies without silently changing
the historical arithmetic. A family supplies the sign orientation, continuity
and monotonicity/uniqueness proof or retains its multiple-root search. A residual-only acceptance needs a caller-supplied derivative/error bound
that turns it into a root-error certificate; otherwise it is only a candidate
for the family's separately documented R6 score gate. A valid enclosing
bracket with certified width/adjacency can establish root accuracy under the
caller's continuity/monotonicity assumptions, with floating-point evaluation
limitations recorded separately. Capped last iterates fail.
The current plain Newton helpers alone cannot satisfy this contract.

Tail-helper proposals (all family edits deferred):

| Family/helper | Expected benefit | Risk / required evidence |
|---|---|---|
| foldnorm `logadd`, `log1mexp` | Replace duplicate finite-double arithmetic; 40,000 sampled equalities across the two helpers | Check NaN/+Inf behavior and branch-threshold neighbors; keep its normal-tail approximation until separately measured |
| snorm `fs::log1mexp` (also GED/skew users) | One complement implementation; 20,000 sampled equalities | Preserve callers' input validation and compare every delegated truth table |
| skellam `logadd` | Shared log-sum behavior; 20,000 sampled equalities | NaN and +Inf precedence differs; Bessel/log-mass paths remain custom |
| pig `logadd` | Shared log-sum behavior; 20,000 sampled equalities | Current nonfinite-maximum shortcut has a distinct contract; retain recurrence order |
| nhyper, sgomp, tlambda, cmb `logadd`; tlambda complement | Further candidates exposed by inventory | No replacement evidence yet; boundary/nonfinite probes and whole-family byte comparison needed |
| dweibull fitter/dpqr `log1mexp_log` | Eliminate duplicate log-hazard composition | Keep underflowed hazard in log coordinates; this is not ordinary `log1mexp(x)` |
| EMG extended log complement/sum, Mills helpers | Potential precision-generic primitives | Double and extended types, cutoffs and rounding differ; no replacement proposed without precision-specific comparison |
| beta bridge `softplus` vs special `softplus` | Potential single shared softplus | Algebraic equality does not establish signed-zero/+Inf or byte equality; keep relocated body unchanged in phase A |

## Changes and reproducibility

Only numerical layout changed: the beta header moved and five include sites
followed it. Other header edits are durable-contract links and corrected stale
Lambert/inverse-digamma comments, plus precise NaN-rejection and
asymptotic-accuracy wording. No fitting, distribution or optimizer
arithmetic was altered. In particular, known fixed-budget tail/quantile gaps
were recorded instead of opportunistically changing settled TASK-78 behavior.
The next numerical review must address gamma/normal quantile exhaustion,
gamma-tail fallback exhaustion, invalid tail callback propagation, beta-fraction
remainder guarantees, skew-GED raw-gamma range, unused W−1 API policy,
large-shape lbeta cancellation and the legacy shared gamma-density endpoint.
The latter helper returns −Inf at x=0 with shape<1 in log mode, whereas the
interior density limit is +Inf; its callers need a separate usage audit.
These concrete gaps are in the header-linked note, not only in scratch logs.

Reproduce from the worktree root (all output goes under ignored build paths):

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run python3 dev/validation/numerical_inventory.py
/tmp/umvl-cpu/run python3 dev/validation/step7_measure.py
/tmp/umvl-cpu/run python3 dev/validation/step7_compatibility.py
/tmp/umvl-cpu/run python3 dev/validation/step7_tail_measure.py
/tmp/umvl-cpu/run python3 dev/validation/step7_pareto4_probe.py
```

The compatibility script compares against its current HEAD; rerun against the
recorded base for the exact phase A comparison after any later commit. The
scripts use installed dependencies only. Measurement summaries are retained
in [the evidence JSON](2026-10-31-step7-measurements.json), while full CSVs,
baseline/current executables and logs are reproducible ignored scratch output.
An early compatibility harness compile failed because its generated array
initializer lacked one opening brace; corrected before the successful byte
comparison. This was a harness defect, not a library/test regression.

## Portability finding from the first final run

The first `just cpp-test-no-long-double` run passed 124/125 ctest shards.
Only `distribution_pareto4` failed: three coefficient comparisons in the
recent default-golden case exceeded `kPareto4GoldenTol`; the status and
likelihood checks passed. The native C++ dev run passed all 104 shards.
This failure is preserved in `build/scratch/initial-cpp-test-no-long-double.log`.

This is a **pre-existing failure on this base, from the 68428d9-era tests**,
not a regression introduced by phase A. Commit
`a402cabf8a80b4fcfc3346f8daf2242f583f906f` declared Approx epsilon `5e-7`
on wasm and `1e-8` otherwise; its
[numerical evidence](2026-10-31-js-fits-report.md) records the earlier
60-digit likelihood/score justification.

The user/orchestrator confirmed that master already resolves this in merge
**`bdc1b88fd373111b38a9aad9323c06a0d0532343`** (diagnostics, TASK-132).
Reading that commit's `test_support.hpp` confirms `5e-7` for wasm,
`BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`, and platforms with
`LDBL_MANT_DIG <= DBL_MANT_DIG`, with the same 60-digit evidence
(likelihood difference about −1.08e-12 and total log-coordinate score norm
about 2.0e-5). Per the user's final instruction, **no local variant was
applied**: `test_support.hpp` remains byte-identical to this worktree's HEAD,
avoiding a merge conflict with the master fix.

`dev/validation/step7_pareto4_probe.py` confirms **byte-identical no-long-double
fit output on unchanged HEAD and current headers**. The measured point is
shape1=0.3157265376327026, shape2=0.6623715356754056,
scale=1.088747513651919, logLik=−3607.2382431667843, status=Ok.
Relative coefficient differences from the stored golden are respectively
3.04794e-7, 2.65616e-7 and 4.17293e-7.

Independent 60-digit calculations hold the threshold fixed and evaluate the
total likelihood/score in free natural coordinates (shape1,shape2,scale).
The score norm is 3.03308890866322e-5 (golden: 3.78637041844022e-5);
the shape1 score is −7.05e-13. The total log-profile envelope-score norm in
(log shape2, log scale/geometric-mean) coordinates is 2.01362473953417e-5.
Recomputed likelihood differs from the recomputed golden by
−1.07220946249447e-12; reported-vs-recomputed likelihood error is
2.56991403302595e-13. These establish numerical stationarity and likelihood
agreement for this sample, not existence/global optimality. Both score norms
are below 1e-3. Together with the byte-identical baseline/current output,
these independently confirm the failure's existing precision-policy cause.
The resolution is already on master in `bdc1b88`; this branch deliberately
retains its base test support. No coefficients, goldens or likelihood tolerance
were changed.

## Open decisions for phase B

- Choose the shared safeguarded solver's coordinate, stopping and failure
  contract before migrating any family; the per-family tables identify
  protected arithmetic and remaining proof/validation requirements.
- Decide whether the unused W−1 helper is retained, deprecated or replaced
  after reviewing external API users. ZIP's W0 replacement/reformulation
  still needs caller-level golden, boundary and held-subset comparisons.
- Select tail-helper consolidations only after precision-specific endpoint,
  NaN, range and byte-compatibility checks. Resolve the concrete legacy
  exhaustion/callback/error-bound gaps listed in the shared-routine note.

These are report-only proposals, coordinated after diagnostics and size-search;
none requires a further decision to finish phase A.

## Final verification

All commands below run in the foreground through `/tmp/umvl-cpu/run4`,
with temporary/runtime directories under this worktree's `build/tmp` and
logs under `build/scratch/final-<recipe>.log`. All five other required
commands finished green. The sole no-long-double failure is the documented
pre-existing Pareto IV comparison resolved on master by `bdc1b88`; per the
user's final instruction it is recorded rather than patched or rerun here.
The full R suite ran exactly once. All started commands have finished.

| Command (after `/tmp/umvl-cpu/run4`) | Actual result |
|---|---|
| `just cpp-test` | PASS: 104/104 dev shards |
| `just cpp-test-no-long-double` | BASELINE FAILURE: 124/125 shards; only Pareto IV coefficient comparisons. Byte-identical HEAD/current fit; resolved on master by `bdc1b88`, no local variant applied |
| `just test` | PASS: FAIL 0, WARN 0, SKIP 151, PASS 18887 |
| `just js-build` | PASS |
| `just js-test` | PASS: 400 tests, zero failures/cancellations/skips |
| `just test-full` | PASS: FAIL 0, WARN 0, SKIP 1, PASS 39040; ran once |

R dev skips are the declared full-tier checks. The sole R full skip is
`test-dist-sn.R:136` because the optional `{sn}` reference package is not
installed; no packages were installed for this task. C++/R compilation can
emit existing style/documentation diagnostics; the zero warning counts above
refer specifically to testthat outcomes, not an assertion that every build
log is warning-free. Existing fixtures and likelihood tolerances are unchanged.

Final source review: the relocated beta body and all 17 modified existing
production headers retain identical non-comment, non-include tokens against
HEAD. No golden/truth fixtures, protected status/roadmap/NEWS files, family
solver arithmetic or test support were modified. Measurements and source/script
fingerprints are retained in the evidence JSON; no commits were made.

Artifact-pane delivery was attempted with the prescribed `bin/show` command.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report remains at the requested worktree path; nothing
was copied into another checkout or directory. Registering this worktree with
the viewer is an operational open item, not a numerical validation gap.
