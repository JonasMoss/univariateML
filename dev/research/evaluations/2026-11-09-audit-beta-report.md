# TASK-101.7.3: audit-beta

Completed on 2026-11-10 in the audit-beta worktree, starting from
594c491dd495892b9cffd8977fcc9809fcbec151. No commits, Git writes, board edits,
package installations, or changes to other worktrees were made.

All 16 requested cases now return successful fits matching independent
references within 1e-10 relative error in every coefficient and in the log
likelihood. The replay also identified and repaired betapr case 272.
Across these 17 cases, the largest coefficient error is 9.030e-14; the largest
log-likelihood error against the maximum is 1.110e-11. The largest discrepancy
between a reported likelihood and a 700-digit evaluation at its returned
coefficients is 6.230e-16.

All required local validation finished successfully. The second-machine
GCC 13 run remains open because simbox first had a read-only filesystem and
subsequently became unreachable. Two unchanged all-free nsbeta cases and the
excluded density kernels also require follow-up, specified below.

## Reproduction and independent references

Before editing, the archived audit harness and its original reproduce.py were
restored under build/scratch/fit-audit. Only their absolute repository paths were
relocated. Each of the 16 requested cases was run through reproduce.py on the
unmodified starting revision; their erroneous successful outputs reproduced. All 460 frozen
cases for the three families were then replayed before and after the repairs.

The retained [build script](2026-11-09-audit-beta-data/build-harness.sh),
[replay script](2026-11-09-audit-beta-data/replay.py), and
[provenance](2026-11-09-audit-beta-data/provenance.json) identify the frozen
harness, inputs, source hashes, and final validation. The original reproducer
outputs are retained as
[before](2026-11-09-audit-beta-data/before-reproduce.log) and
[after](2026-11-09-audit-beta-data/after-reproduce.log).

[reference.py](2026-11-09-audit-beta-data/reference.py) independently solves the
raw-model score equations using mpmath at 350 and 700 digits. Input observations
and held parameters are interpreted as their exact binary64 values. For the
two-free-shape beta-prime cases it solves coupled log-shape scores with an
analytic Jacobian. For single-free-shape cases it bisects the strictly monotone
digamma-increment score 450 times. For Pareto IV it solves the two raw Burr score
equations in centered log-excess coordinates. The printed 100-digit results at
the two precisions agree, passing a relative comparison threshold of 1e-90.

These are high-precision numerical references, supported by uniqueness from
likelihood concavity; they are not outward-rounded interval certificates.
[reference.json](2026-11-09-audit-beta-data/reference.json) contains all 17 results.
[compare.py](2026-11-09-audit-beta-data/compare.py) additionally evaluates every
successful replay result's raw density at 700 digits, independently of the C++
likelihood and the protected density kernels.

## Arithmetic repairs

### Beta prime

The fixed-scale likelihood is a beta likelihood under y = x/(s+x), plus a
parameter-independent Jacobian. Reconstructing it with raw log-gammas discarded
the fitted likelihood. The repaired route retains the stable beta likelihood
and adds a log-sum-exp form of the Jacobian. For dispersed samples it transforms
the smaller beta tail, avoiding rounding large observations onto an endpoint.

Concentrated samples need more than stable normalization: forming absolute y
values loses the relative information in their dispersion. The family-local
centered.hpp preserves

$$
y_i-y_0=\frac{s(x_i-x_0)}{(s+x_i)(s+x_0)}
$$

using normalized ratios that also avoid overflow in s+x. It solves the
concentration and centered-mean scores using existing beta increment and
Stirling-remainder helpers. Strict concavity of the fixed-scale beta
log likelihood identifies the unique maximum for nonconstant interior data.
Computed root brackets, rather than Newton guides, decide convergence.

Independently rounding the two large shapes can lose the tiny fitted mean
displacement. The implementation assembles their ratio jointly using division
and product residuals, then reevaluates the likelihood at the actual returned
pair. A separated restoration loss in log-X density gives the existing
arithmetic-failure outcome. This comparison excludes the data Jacobian so that
changing measurement units cannot change its criterion.

For a single held shape and scale, the score now uses a stable digamma increment.
The normalizer reuses the existing family-local stable log-beta helper. This
also repairs case 272, whose previous free-shape error was about 1.97e-10.
Insufficient subnormal score signal is rejected rather than blessed as a root.

### Nonstandard beta

With one shape held, nsbeta now delegates to the repaired beta solver, forwarding
both tolerance and maxit. This removes its separate cancelling digamma and
log-gamma differences. For held h and free z, the score equation is

$$
\psi(z+h)-\psi(z)=T,\qquad T>0.
$$

The left side decreases strictly from infinity to zero, so the root is unique.
Case 5348 now returns shape2 = 4.734938298732659e150, against the reference
4.734938298732929e150, instead of approximately 4.27914928881e14.

Family-local guards reject a data statistic or small-increment intermediate
whose subnormal resolution cannot support relative shape accuracy. A positive
control with held shape 1e-320 and ordinary observations remains estimable:
its free shape agrees with the independent asymptotic
sqrt(h)/sqrt(T), whose omitted relative correction is below 1e-150.
The all-held evaluator retains its previous arithmetic/status path, including
non-success on overflowing evaluations. The all-free shape algorithm is unchanged.

### Pareto IV

With the first shape held and concentrated excesses, the previous Burr
translation lost information when separately formed logarithms were subtracted.
The family-local centered.hpp instead standardizes
log1p((x_i-x_0)/(x_0-min)) and solves the location/power scores by bracketing.
The likelihood is strictly concave in power and power times log scale;
nonconstant excesses therefore have one finite maximum on this held-shape route.

An error-free subtraction residual retains the exact excess anchor.
Scale restoration uses expm1 and fused operations, and the final likelihood is
evaluated at the represented scale using centered differences. A separated
loss against the latent optimum gives NumericalFailure. This detects, for
example, a distant threshold for which a double-precision scale cannot encode
the needed offset.

Both restoration comparisons use a practical floating-point allowance of
128 times machine epsilon times the magnitudes of the stable likelihood terms,
in addition to the requested tolerance. The mathematical route is certified in
exact arithmetic conditional on the computed comparisons; the allowance is
practical floating-point protection, not an interval-arithmetic proof.
Exhausted numerical roots return failure, as tested with maxit = 1.
No inconclusive face comparison was replaced with a new refusal.

## Requested cases: before and after

Numbers below are rounded for display; native CSV and
[comparison.json](2026-11-09-audit-beta-data/comparison.json) retain full values
and every per-case coefficient and likelihood error. All rows have successful
status after the repair.

| Family | Case | Before log likelihood | After log likelihood |
|---|---:|---:|---:|
| betapr | 248 | 80 | 84.4419505470592 |
| betapr | 249 | -50465865728 | 130.493688859928 |
| betapr | 251 | -136 | 82.2957378963924 |
| betapr | 252 | -48318382080 | 128.347415322572 |
| betapr | 254 | 126.821259321377 | 126.822300925596 |
| betapr | 255 | -205.828955697000 | 184.386928250447 |
| betapr | 256 | -2368.01041915481 | 2291.25228834000 |
| betapr | 257 | -1582.05921394046 | 4135.70557078078 |
| betapr | 260 | 62.4599609375 | 62.3962048182284 |
| nsbeta | 5348 | -3367.37205723773 | 4145.54956203009 |
| nsbeta | 10791 | 148.587322732861 | 184.386787523338 |
| nsbeta | 10792 | -1971.55457106092 | 2291.25214761289 |
| nsbeta | 10799 | 126.542386615592 | 126.549700761956 |
| nsbeta | 11011 | 150.373510750481 | 172.078653972959 |
| pareto4 | 5666 | 108.039174917414 | 108.039536726702 |
| pareto4 | 5669 | 108.039174917414 | 108.039536726702 |
| betapr, additional | 272 | 57.7446973782770 | 57.7446973782729 |

For concentrated cases 249 and 252, the reported likelihood matches the
likelihood of the represented coefficient pair. Its small difference from the
real-valued optimum is included in the 1e-10 reference check; it is not hidden
by evaluating only the latent root. The worst such relative difference is
1.110e-11 in case 249.

## All-family replay

A likelihood mismatch here means
abs(reported - raw)/max(1, abs(raw)) > 1e-10 at a successful returned point.
This threshold differs from the original audit's ranking threshold.

| Family | Frozen cases | Successful fits before → after | Likelihood mismatches before → after |
|---|---:|---:|---:|
| betapr | 184 | 135 → 136 | 11 → 0 |
| nsbeta | 100 | 80 → 80 | 8 → 1 |
| pareto4 | 176 | 122 → 122 | 4 → 0 |
| Total | 460 | 337 → 338 | 23 → 1 |

No replay time guard fired. The only status change is betapr case 258:
the smaller-tail transformation converts a previous failure to success.
Its reported likelihood -4154.3409505052032 agrees with raw high-precision
evaluation to 5.818e-17 relative error. It has no new independent coefficient
reference; that check establishes likelihood evaluation, not a separate
coefficient-accuracy result.

The three dossiers describe the family-specific arithmetic. Regression cases
were appended to each C++ and R family test file; existing tests were retained.
Additional tests cover forwarded iteration limits, exhausted roots, finite
scales despite overflow of scale plus data, coefficient/scale restoration loss,
and resolved versus unresolved subnormal score arithmetic.
Default all-free search algorithms and all goldens were retained.

## Final validation

All commands below finished on the workstation, under the shared four-slot
reservation with numerical libraries pinned to one thread. Compiler:
GCC 15.2.0; R 4.6.1; BH 1.90.0.1; Python 3.14.5; mpmath 1.3.0.
The tested source hashes still match the recorded final hashes.

| Command/check | Final result |
|---|---|
| Targeted three-family C++ tests | 82/82 cases; 11247/11247 assertions |
| just test | FAIL 0, WARN 0, SKIP 154, PASS 22503 |
| just cpp-test | 106/106 shards |
| UMVL_TEST_TIER=full just cpp-test | 129/129 shards |
| just cpp-test-no-long-double | 129/129 shards, workstation |
| just test-full | FAIL 0, WARN 0, SKIP 0, PASS 45338 |
| just site-check | Passed, including 99 family pages and 99 compiling C++ snippets |
| git diff --check | Passed |

The full tiers were run once after the final fitter changes. Final compressed
logs, the environment record, and the equivalent
[validation command sequence](2026-11-09-audit-beta-data/validate-local.sh) are
retained beside the evidence. Site-check regenerated an unrelated existing
screenshot; its original content was restored after the check.
No core, common/JS binding, or binding message string changed, so the conditional
JS build/test requirement was not triggered.

## Open findings and excluded changes

1. **GCC 13 no-long-double validation.** Simbox initially accepted SSH but
   returned I/O errors; creating a reservation failed with a read-only
   filesystem. The verified, unstarted reservation launcher was terminated,
   and all final suites used the workstation fallback. A later SSH recheck
   timed out. No remote test result is claimed. What is missing is one
   successful run of just cpp-test-no-long-double on simbox after recovery.
   The [availability record](2026-11-09-audit-beta-data/simbox-unavailable.txt)
   retains the failures.

2. **Unchanged all-free nsbeta concentration.** Cases 5329 and 10786 hold only
   the bounds 0 and 1 and contain nearly constant observations. Their maximum
   free-coefficient discrepancies against the saved refined references remain
   6.044e-10 and 1.666e-10. Reported versus raw likelihood relative discrepancies
   are respectively 4.833e-11 and 1.573e-10. For 10786 the reported likelihood is
   135.27888712882361 and the raw likelihood is
   135.2788871075539013266. Both outputs are unchanged before/after this job.
   Settling them needs a centered-mean and coefficient-restoration repair in
   the general all-free beta path, with independent raw-score references and
   permission to change those default fits. This job repairs the specified
   held-shape nsbeta route.

3. **Protected density kernels.** The required ML fits are repaired, but the
   excluded dpqr.hpp implementations can still disagree drastically at the
   repaired extreme coefficients. Examples:

   | Case | Fitted/raw log likelihood | Sum of current dpqr log densities |
   |---:|---:|---:|
   | betapr 249 | 130.493688859928 | -6241124352 |
   | nsbeta 5348 | 4145.54956203009 | -4183.37205723773 |
   | pareto4 5666 | 108.039536726702 | 108.039174782808 |

   [density-seams.csv](2026-11-09-audit-beta-data/density-seams.csv) records all
   17 comparisons, with a retained independent driver. The needed work is a
   centered beta/log-beta density calculation, endpoint-safe nsbeta logarithms,
   and centered raw-excess log ratios in the Burr/Pareto density calculation.
   These kernels can therefore also affect dml at such fitted coefficients.
   No dpqr.hpp, special-function seam, truth table, code-generation script,
   or generated density implementation was edited.

## Replaying the retained checks

From the repository root, restore/build the archived harness with the build
script directly; it acquires its own CPU slot. Then run the current replay and
comparison through one-slot reservations:

~~~sh
bash dev/research/evaluations/2026-11-09-audit-beta-data/build-harness.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-09-audit-beta-data/replay.py after
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-09-audit-beta-data/compare.py
~~~

The high-precision reference script resumes from its retained checkpoint.
To recompute all references, first preserve and remove that checkpoint, then
run reference.py through a one-slot reservation. The archived before CSV was
captured before editing and must not be regenerated from the repaired headers.
The validation script is invoked directly through run4, without nesting slot
wrappers. No generated build products are needed to interpret the retained
before/after and reference evidence.

The artifact-pane display was attempted after validation. The viewer returned
The report remains at the requested worktree path; no viewer configuration or
other checkout was changed.
