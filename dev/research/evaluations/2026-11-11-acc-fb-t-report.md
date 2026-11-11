# Student-type fallback accuracy and LST refusals — TASK-150.2.4

The owned uncertified routes in `std`, `lst`, `sstd`, `sged`, `st`, and `slash`
now finish with a coefficient-aware local accuracy check. The two requested
`lst` refusals have been replaced by real, locally checked searches when no
statistical certificate proves a stop. Public controls retain their names.

All required suites passed locally. Across 687 cases, the 657 owned
uncertified finite fits have maximum scaled coefficient error
**2.573e-12** in both native and double-only builds. The R/WASM spot maxima
are **1.304e-12** and **2.943e-12**. There are **no new stops**; 24 previously
refused cases now return accurate finite fits. Two accuracy issues remain
open under explicit scope/preservation constraints: 17 certified SGED replay
misses, and one unchanged GED-parent transport. They are detailed below.

Baseline: `bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714`. The worktree remains
uncommitted, as requested.

## Route inventory

The inventory follows public dispatch, result flags, and delegate calls in
all six families, including their certificate headers. “Before” describes
the baseline stopping rule, rather than whether a particular sample happened
to be accurate. Dedicated certificate routes that return finite `Ok` results
mark them certified; their non-`Ok` failures are not uncertified fits.

| Family / route | Trigger and baseline stopping rule | Contract before? | Final step |
|---|---|---|---|
| `std`: held mean, free sd and nu | constrained profile retained when face exclusion does not close; profile nominations and score-gated search | no | analytic observed-information Newton correction in public mean/sd/nu coordinates; held mean constant |
| `std`: held sd, free mean and nu | constrained profile retained when face exclusion does not close; profile nominations and score-gated search | no | analytic observed-information Newton correction with variance-standardization derivative and held sd constant |
| `std`: held mean and sd, free nu fallback | scalar certificate budget or comparison unresolved and old search succeeds; legacy scalar search and score gate | no | one free-coordinate Newton correction mapped from log(nu-2) to nu |
| `std`: held nu, free location and scale through lst | parent joint inclusion fails after EM convergence; parent previously refused; certification flag was not transported | not a finite fallback before this job | transport parent's uncertified accurate point, then check standardized sd including its physical-scale conversion |
| `lst`: held mu, free sigma and df | finite constrained search retained without face exclusion; profile nominations and score gate | no | analytic Newton correction; free df uses log(df) when the location hold exempts the df>=1 restriction |
| `lst`: held sigma, free mu and df | finite constrained search retained without a closed certificate; profile nominations and score gate | no | analytic Newton correction with sample-range location floor; natural df near one, one-sided restriction check and interior continuation |
| `lst`: held mu and sigma, free df fallback | shape certificate cannot close and numerical search succeeds; legacy scalar search and score gate | no | one free-coordinate Newton correction mapped to public df |
| `lst`: held df<1, free mu and sigma | strict atom inequalities and no proved symmetry exclusion; generic asymmetric samples refused without a joint search | not a finite fallback before this job | 15 quantile/scale BFGS starts; positive-information contracting Newton step for each candidate; best checked likelihood returned uncertified |
| `lst`: held df>=1, free mu and sigma | EM converges but joint_inclusion fails; numerical refusal despite a converged search | not a finite fallback before this job | retain converged point, analytic contracting Newton correction, finite physical-point likelihood witness, uncertified |
| `sstd`: all free | ordinary finite joint fit without a global certificate; analytic positive-information Newton correction with contraction and rounding checks | yes | same all-free stopping rule; no change to default golden outputs |
| `sstd`: shared held-subset search | subset not dispatched to a dedicated certified fitter; shared numerical optimizer with free-coordinate score gate | no | differentiate held values as constants, pack free coordinates, normalize about candidate, propagate Newton errors to mean/sd/nu/xi |
| `sged`: all-free smooth fit | finite smooth joint fit without a global certificate; analytic positive-information Newton correction | yes | unchanged all-free stopping rule |
| `sged`: all-free shape-one corner | shape-one nomination and positive right profile cost score; data-knot enumeration and closed-form Laplace side scales | yes | unchanged exact-knot and closed-form rule |
| `sged`: smooth held-subset search | shared numerical search retained without a certificate; shared optimizer free-coordinate score gate | no | packed analytic Newton correction, natural shape near one, one-sided restriction check and interior continuation |
| `sged`: held-subset shape-one contact | search nominates a density mode at an observation; smooth optimizer near a nonsmooth contact | no | fix exact data knot; solve smooth side-scale tangent or closed forms; strict one-sided mode scores and positive free-shape right score |
| `sged`: held subunit-shape contact | held 0<nu<1 and nominated density-mode knot with one held moment; shared optimizer at a nonsmooth contact | no | variance-scaled side coordinates; tangent Newton; opposing infinite mode slopes from positive cusp |
| `sged`: xi=1 with nu free, delegated to ged | exact unit-skew held value; GED parent coefficients and certification flag transported unchanged | parent dependent; independent counterexample fails | transport deliberately unchanged; parent held-scale/free-location/free-shape refinement remains open |
| `st`: all-free joint fit | generic finite search without a certificate; analytic Newton correction including Student-CDF derivatives | yes | original all-free coordinate scaling and CDF axis-3 margin retained |
| `st`: shared held-subset joint fit | subset not dispatched to a dedicated certified fitter; profile/joint optimizer and score checks | no | packed analytic Newton correction; held shape constant even below one; natural shape near one; public alpha and df error maps |
| `st`: df-only held-subset search | held xi, omega and alpha; df derivative moves to packed axis zero; score-gated scalar search; no coefficient error bound | no | CDF fraction derivative checks follow df jet axes, allow stated arithmetic floor near normal face, and defer accuracy to projected-score Newton check; final likelihood uses values only |
| `slash`: all-free joint fit | finite profiled fit after existing global checks; analytic information-weighted Newton correction | yes | unchanged all-free refinement |
| `slash`: held sigma, free mu | finite held-scale location search; golden-section location width in data-range units | no | analytic scalar observed-information Newton correction in fitted-scale coordinates |
| `slash`: held mu, free sigma | monotone scale score has a finite root; bounded log-scale bisection without a checked final contract width | not uniformly justified | computed-sign bracket and final log-width <= tol/8; public scale representability checked |
| `all six`: all coefficients held | no free coordinate; evaluate supplied point | vacuous | holds remain exact; no numerical coefficient search |

[The machine-readable inventory](2026-11-11-acc-fb-t-data/route-inventory.json) also identifies every untouched dedicated certificate dispatcher.

The successful `sstd` and `st` common bindings omit the C++ certification
field; `slash` has no such member. A JSON `null` flag is therefore not an
observed `false`. The measurement harness conservatively checks these finite
rows too. An additional audit of the C++ dispatch found no hidden certified
rows in this corpus. All 21 explicitly certified replay outputs are
byte-identical before/after in each build mode.

## Local accuracy and mathematical evidence

The shared analytic second-order jet is packed over free coordinates only;
held values are differentiated as constants. The stopping rule requires
positive observed information and a mapped Newton correction `H^-1 g` no
larger than `tol/8` in any free public coefficient, followed by another
evaluated correction. Full-step contraction must be quadratic, or have
reached the practical arithmetic floor. Projected score-rounding checks and
public-coefficient representability checks guard ill-conditioned cases.
Objective decrease and score norms are search devices, not acceptance rules.
Refinement uses `min(requested_tol, 1e-10)`; solver/certificate work follows
the existing `iterlim` budgets.

The parameter maps include the offset in `nu-a`, the public skew coordinate,
and the ordinary-scale to standard-deviation conversion
`sigma_physical = sd * sqrt((nu-2)/nu)` for `std`. Natural shape coordinates
near one prevent a vanishing log-excess Jacobian or a likelihood tie from
silently replacing an interior root by the restriction. Shape scores are
rechecked after nuisance refinement. Very large residuals use logarithmic
kernel forms. Candidate-centered normalization avoids derivative underflow
from unrelated data/held-parameter units.

For SGED corners, let `p=nu`, `c` be an observed mode knot, and `a,b` be raw
left/right side scales. The independent reference uses

```
K = Gamma(2/p)/Gamma(1/p),  G = Gamma(3/p)/Gamma(1/p)
mean = c + K*(b-a)
sd^2 = G*(a^2+b^2-a*b) - K^2*(b-a)^2
xi = sqrt(b/a)
```

Production instead uses variance-scaled sides `A=a/lambda`, `B=b/lambda`,
where `lambda=sqrt(Gamma(1/p)/Gamma(3/p))` and `M=K*lambda`. This removes raw
gamma-moment overflow from the tangent problem. At `p=1`, raw mean and variance
reduce to `c+b-a` and `a^2+b^2`. Holds leave a smooth tangent solve or closed
forms. Strict left/right mode cost scores verify a shape-one corner; a free
shape additionally needs a positive right profile-cost score. For held
`0<p<1`, the positive `|mode-knot|^p` cost dominates smooth perturbations,
giving opposing infinite mode slopes. The tangent coefficients still need
their own Newton accuracy check. The unchanged all-free shape-one route uses
its existing knot enumeration and closed forms.

For `st`, CDF fraction convergence follows the actual df jet after packing,
including both derivative orders. Packed searches near the normal face allow
the stated multiplier-rounding floor; the final information-weighted score
check still controls coefficient accuracy. The final likelihood is evaluated
without unnecessary derivative axes and must be finite. In seeded case
33101 (held location 0, scale 1, alpha 2), native df improves from
999.994992094385 to 1000.000000000754; the independent root is
1000.0000000000064.

The `lst` subunit search uses 15 quantile/scale BFGS starts followed by the
analytic check, retaining the best checked likelihood. A searched witness
and its reflection may strengthen the existing symmetry proof by beating
the entire centered scale profile. Witnesses are not reported as maximizer
enclosures. At df at least one, failed joint inclusion retains the converged
and polished point uncertified. Likelihood witnesses use the rounded public
coefficients. Statistical atom/symmetry proofs and successful global
certificates keep their existing roles.

These are local certificates in exact arithmetic conditional on the computed
comparisons, with practical binary64 margins. They are not outward-rounded
interval proofs or new global-optimality certificates. The independent roots
below are numerical evidence, with separate derivative code and arithmetic.

Implementation: [shared jets](../../../univariateML-cpp/include/cppunivariate/distributions/sstd/local_accuracy.hpp), [Student refinement](../../../univariateML-cpp/include/cppunivariate/distributions/lst/accuracy.hpp), [SGED corner chart](../../../univariateML-cpp/include/cppunivariate/distributions/sged/accuracy.hpp), [Student-CDF derivatives](../../../univariateML-cpp/include/cppunivariate/distributions/st/local_accuracy.hpp), [slash](../../../univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp).

## Independent adjudication and measurements

The main replay has 686 rows: all 121 requested held misses, all 462
uncertified finite rows from the 2026-11-08 Student coverage CSV (221 `lst`,
241 `std`), and 103 additional coverage rows. The CSV replay checks the exact
stored samples and held masks. A supplementary 500-observation all-free
`sstd` golden, seed 12791, closes coverage of the unchanged all-free route,
giving 687 cases in total.

Every returned free coefficient is compared with an independent 90-digit
mpmath stationary point, using direct densities and numerical derivatives.
References require positive information on their smooth free/tangent
coordinates and Newton correction below `1e-55` in normalized coordinates
(`1e-60` for knot tangents). Boundary nominations are not final evidence:
the completed audit checks the final one-sided scores for all 33 boundary/knot
rows. SGED references use raw sides, independently of production's normalized
chart. There are 679 finite reference cases; the eight existing stops have
no returned coefficient vector to compare.

The reported error is

```
E = max over free i of |theta_i - theta_i_reference|
                         / max(|theta_i_reference|, s_i).
```

Location floors are the fitted scale, except held-scale `lst`, which uses
the sample range (the supplied scale for constant data). `st` alpha uses
floor 1; positive coefficients use relative error. Under this metric, 117
of the 121 original held misses are confirmed. ST rows 7008, 7122, 7124,
and 7134 were already within `1e-10`. The 17 remaining certified SGED misses
are preserved and separated from the owned-uncertified statistics.

All 657 owned uncertified finite cases satisfy `E <= 1e-10` in native and
double-only builds. Twenty-four newly finite cases consist of two subunit
joint `lst` cases, 21 joint-inclusion fallback cases (including the translated
adjacent-ULP sample), and one `std` transport. The eight baseline stop indices
remain 31100, 32004, 32010, 32012, 34006, 34007, 34008, and 34009. They include
proved/retained statistical outcomes and three existing tiny-shape numerical
initialization failures; no new stop is introduced by refinement.

[Per-row held adjudication](2026-11-11-acc-fb-t-data/held-adjudication.json), [all coefficient errors](2026-11-11-acc-fb-t-data/coefficient-errors.json), [final reference signs](2026-11-11-acc-fb-t-data/reference-sign-verification.json), and [the all-free SSTD reference](2026-11-11-acc-fb-t-data/golden-sstd-verification.json) retain the detailed evidence.

### Maximum scaled error by family

| Family | Native before | Native after | Double before | Double after |
|---|---:|---:|---:|---:|
| `std` | 8.57e-06 | 2.57e-12 | 8.57e-06 | 2.57e-12 |
| `lst` | 1.06e-05 | 2.77e-13 | 1.06e-05 | 1.84e-13 |
| `sstd` | 4.7e-07 | 2.75e-14 | 2.74e-07 | 2.46e-14 |
| `sged` | 0.00314 | 1.3e-12 | 0.00314 | 4.37e-13 |
| `st` | 5.01e-06 | 7.47e-13 | 2.57e-06 | 9.26e-13 |
| `slash` | 5.12e-08 | 3e-14 | 5.37e-08 | 3e-14 |

### Maximum scaled error by route

Held names below use the public API. Corner and near-restriction routes are
split from smooth searches. “Refused” means that every baseline case in that
group stopped, so no before-error exists. The certified and inherited GED
exceptions are excluded and reported separately below.

| Route | Cases | Native before | Native after | Double before | Double after |
|---|---:|---:|---:|---:|---:|
| lst: joint held df<1 | 2 | Refused | 3.15e-16 | Refused | 3.15e-16 |
| lst: joint inclusion fallback df>=1 | 21 | Refused | 3.64e-16 | Refused | 3.64e-16 |
| lst: mu | 103 | 1.06e-05 | 2.77e-13 | 1.06e-05 | 1.84e-13 |
| lst: mu,sigma | 8 | 8.51e-06 | 4.17e-14 | 8.51e-06 | 6.9e-14 |
| lst: near df=1 / held sigma | 1 | 5.11e-08 | 4.26e-16 | 5.22e-08 | 2.04e-16 |
| lst: sigma | 112 | 6.46e-06 | 6.72e-14 | 6.46e-06 | 5.35e-14 |
| sged: all free | 1 | 1.62e-15 | 1.62e-15 | 1.01e-15 | 1.01e-15 |
| sged: mean,sd | 3 | 8.27e-09 | 1.17e-15 | 8.27e-09 | 1.17e-15 |
| sged: mean,sd,xi | 15 | 2.04e-06 | 1.3e-12 | 2.04e-06 | 4.37e-13 |
| sged: mean,xi | 2 | 9.05e-09 | 2.9e-16 | 9.05e-09 | 2.9e-16 |
| sged: near nu=1 / held mean,sd,xi | 1 | 0.0001 | 2.76e-18 | 0.0001 | 4.47e-16 |
| sged: sd,xi | 2 | 9.47e-08 | 2.31e-13 | 9.47e-08 | 1.5e-13 |
| sged: shape-one corner / all free | 1 | 3.18e-17 | 3.18e-17 | 3.18e-17 | 3.18e-17 |
| sged: shape-one corner / mean,nu | 2 | 1.06e-08 | 3.21e-16 | 1.06e-08 | 3.21e-16 |
| sged: shape-one corner / mean,sd | 1 | 0 | 0 | 0 | 0 |
| sged: shape-one corner / sd,nu | 3 | 4.15e-05 | 3.32e-16 | 4.15e-05 | 3.32e-16 |
| sged: shape-one corner / sd,xi | 3 | 9.49e-16 | 9.78e-17 | 9.49e-16 | 9.78e-17 |
| sged: subunit corner / mean,nu | 4 | 6.18e-08 | 3.75e-15 | 6.18e-08 | 3.75e-15 |
| sged: subunit corner / sd,nu | 4 | 0.00314 | 3.98e-17 | 0.00314 | 3.98e-17 |
| slash: all free | 12 | 1.34e-14 | 1.34e-14 | 1.76e-14 | 1.76e-14 |
| slash: mu | 1 | 4.96e-17 | 4.96e-17 | 4.96e-17 | 4.96e-17 |
| slash: sigma | 7 | 5.12e-08 | 3e-14 | 5.37e-08 | 3e-14 |
| sstd: all free (golden seed 12791) | 1 | 7.36e-15 | 7.36e-15 | 8.2e-16 | 8.2e-16 |
| sstd: mean,sd | 2 | 1.12e-07 | 2.78e-16 | 1.12e-07 | 1.52e-16 |
| sstd: mean,sd,xi | 9 | 6.32e-09 | 5.33e-16 | 5.18e-08 | 8.67e-16 |
| sstd: mean,xi | 2 | 1.43e-07 | 1.4e-14 | 2.74e-07 | 7.68e-15 |
| sstd: nu,xi | 13 | 2.28e-07 | 5.65e-16 | 2.28e-07 | 5.65e-16 |
| sstd: sd,xi | 5 | 1.79e-07 | 8.23e-15 | 2.17e-07 | 1.07e-14 |
| sstd: xi | 4 | 4.7e-07 | 2.75e-14 | 1.59e-07 | 2.46e-14 |
| st: all free | 3 | 3.54e-13 | 3.54e-13 | 1.28e-13 | 1.28e-13 |
| st: alpha | 6 | 5.06e-08 | 1.39e-14 | 5.19e-08 | 1.47e-14 |
| st: alpha,nu | 16 | 1.37e-08 | 5.41e-16 | 1.1e-08 | 3.91e-16 |
| st: large-df CDF / held xi,omega,alpha | 1 | 5.01e-06 | 7.47e-13 | 2.57e-06 | 9.26e-13 |
| st: near nu=1 / held xi,omega,alpha | 1 | 4.77e-08 | 2.04e-16 | 6.07e-08 | 1.83e-17 |
| st: nu | 4 | 1.27e-08 | 4.24e-15 | 2.72e-09 | 3.87e-15 |
| st: omega | 2 | 2.88e-09 | 2.96e-15 | 6.3e-09 | 5.43e-15 |
| st: omega,alpha | 2 | 2.14e-08 | 1.12e-15 | 3.65e-09 | 7.23e-15 |
| st: omega,nu | 3 | 6.83e-09 | 1.14e-15 | 2.13e-09 | 1.4e-15 |
| st: xi | 5 | 6.33e-08 | 9.04e-15 | 7.91e-09 | 7.6e-15 |
| st: xi,alpha | 5 | 2.8e-08 | 4.69e-15 | 6.98e-09 | 1.03e-14 |
| st: xi,alpha,nu | 13 | 2.99e-08 | 1.94e-16 | 1.27e-08 | 1.72e-16 |
| st: xi,nu | 6 | 1.21e-08 | 2.66e-15 | 5.4e-09 | 2.66e-15 |
| st: xi,omega | 2 | 1.6e-08 | 2.62e-15 | 1.6e-08 | 3.99e-15 |
| st: xi,omega,alpha | 1 | 1.49e-09 | 2.67e-17 | 1.35e-09 | 1.31e-15 |
| std: held nu / lst transport | 1 | Refused | 2.25e-17 | Refused | 2.25e-17 |
| std: mean | 100 | 8.57e-06 | 1.24e-13 | 8.57e-06 | 8.85e-14 |
| std: mean,sd | 22 | 5.96e-06 | 1.14e-13 | 5.96e-06 | 9.3e-13 |
| std: sd | 119 | 3.86e-06 | 2.57e-12 | 3.86e-06 | 2.57e-12 |

The 85 R and 85 WASM spot cases include the all-free SSTD supplement and
expected stops. Owned finite maxima are `1.304e-12` in R and `2.943e-12` in
WASM; the supplementary SSTD errors are `7.362e-15` and `9.239e-16`.
The explicit control mapping in the R driver preserves the existing names;
generated wrappers expose `x/...` rather than those names through `formals()`.

[Public-interface measurements](2026-11-11-acc-fb-t-data/spot-verification.json) and [route summaries](2026-11-11-acc-fb-t-data/route-contract-summary.json) provide the underlying values.

## Preservation and tests

In each build mode, all 21 certified replay rows are byte-identical, including
coefficients, likelihoods and iteration counts. All six bare-default golden
outputs are also byte-identical before/after. No golden samples or golden
estimates were rewritten. Dedicated certificate logic is unchanged apart
from allowing the new searched subunit LST witness to strengthen the existing
symmetry exclusion. That changes an old numerical refusal to a proved
nonuniqueness refusal, not a formerly successful fit to a stop.

New C++ cases are appended in all six families; R cases are appended for
LST subunit search and SGED shape-one/subunit contacts. Two existing LST
expectations are tightened: the symmetric subunit case now requires the
specific nonuniqueness proof and retains its witness checks; the translated
four-adjacent-ULP sample now requires an uncertified accurate fit, an analytic
scale reference and the actual rounded-point likelihood. No test was deleted
or weakened to accept a new refusal.

Family dossiers record only the relevant scale, shape/corner and transport
facts. No `core/`, settled d/p/q/r, `special/`, truth-table, codegen, NEWS,
status, known-issues or roadmap source was changed.

All final suites used the workstation. No simbox/GCC-13 pass is claimed:
the brief explicitly required local runs because simbox was unreachable.
Compute used the provided slot wrappers, at most four cores for this job,
with BLAS/OpenMP pinned and lowered priority. Wall times that include waiting
for shared reservations are not performance benchmarks.

| Final run | Result |
|---|---|
| `just cpp-test` | 107/107 |
| `just cpp-test-no-long-double` | 134/134 |
| `just js-build` | PASS |
| `just js-test` | 615/615; zero failures/skips |
| `just test-full` | 46,072 passes; FAIL 0, WARN 0, SKIP 0 |
| `just test` | 23,245 passes; FAIL 0, WARN 0, SKIP 154 |
| `just site-check` | PASS, including browser/static/live checks |
| Independent final-reference sign audit | 33/33 boundary/knot rows |
| Native/double, R/WASM coefficient and preservation checks | PASS, with the two explicit exceptions below |

The additional repository-wide Markdown scan found 45 broken destinations in unchanged baseline files, including parser matches on C++ lambda syntax and missing local literature/site targets. The report and six changed dossiers have zero broken destinations. [The baseline and scoped link audit](2026-11-11-acc-fb-t-data/baseline-link-audit.json) verifies that all 45 source files/lines are inherited; unrelated files were left untouched.

Toolchain: c++ (Ubuntu 15.2.0-16ubuntu1) 15.2.0; R version 4.6.1 (2026-06-24); Node v22.22.1; Python 3.14.5; mpmath 1.3.0; emcc (Emscripten gcc/clang-like replacement + linker emulating GNU ld) 6.0.13 (e13ee988ce3d87a738dd8f3661d02223308c412a).

[Final machine-readable summary](2026-11-11-acc-fb-t-data/final-summary.json), [test-run record](2026-11-11-acc-fb-t-data/test-runs.txt), [certified-output comparison](2026-11-11-acc-fb-t-data/certified-preservation.json), and [source hashes](2026-11-11-acc-fb-t-data/source-sha256.json) record provenance.

## Open items under explicit constraints

1. **Certified SGED: 17 coefficient misses remain unchanged.** Their maximum
   scaled error is `7.979474832513266e-10` in both builds. The fixed-shape/skew
   certificate ignores the requested `tol` and uses mode/public enclosures
   that are too coarse for `1e-10`. In case 6170, the returned mean is
   `-1.8564032985102169e149`, versus the independent
   `-1.8564033054720791e149`, with scale about `8.7247123865106e149`.
   This is beyond rounding. Fixing it requires a tol-aware certificate/root
   refinement and permission to change certified coefficients, conflicting
   with this brief's preservation requirement. The numerical root and
   affected rows are recorded; they were not relabelled uncertified to hide
   the error.

2. **Unchanged GED delegation: case 31101 fails the local contract.** Data are
   `(0.7321666961121761, 2.2766042947262384, -0.5491691631533211)`, with held
   `sd=1.0178559902092952` and `xi=1`. Parent and child return
   `nu=27.497341303893556`; the independent stationary value is
   `27.4973366168316843`, a relative error of `1.704551221489073e-7`.
   Mean error is only about `2.4e-15`. The required fix belongs to GED's
   held-scale/free-location/free-shape accuracy step in sibling job
   `acc-fb-cont`. This job preserves the exact parent transport, as explicitly
   instructed, and leaves GED files untouched. The added transport regression
   and the independent counterexample make the needed parent change concrete.

These are the only measured remaining coefficient misses: neither is counted
as an owned repaired fallback success. Both remain visible in the saved
verification data.

## Reproduction

From this worktree, the data directory's `reproduce.sh` extracts the baseline
headers/common bindings with `git archive` into ignored build scratch, builds
native/double capture binaries, and runs the root/comparison/preservation
checks. No source archive is kept in evaluation data. `cases.json`,
`inputs.txt`, `stationary.jsonl`, before/after captures and golden captures are
the frozen main inputs/results. `run-tests.sh` records the required suites.
Run `final-scientific-checks.sh` through a single `/tmp/umvl-cpu/run` reservation;
it contains no nested reservations or background jobs. It regenerates the
public-interface captures, final sign audit and supplementary SSTD root.
`finalize-evidence.py` and `write-report.py` collect the completed results and
render this document. All inputs, scripts and results remain in this
worktree; no other worktree was changed.
