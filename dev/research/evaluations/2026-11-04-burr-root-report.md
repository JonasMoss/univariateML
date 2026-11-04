# TASK-138: Burr XII root boxes and global certificates

Tested source: `498f4178c0361cbdb0dd597c408a18a797a9fb2b`, branch
`codex/burr-root`. The additions are this report and its
[evidence directory](2026-11-04-burr-root-data/README.md). Every final research
run and regression suite completed locally before this report was written.

The coefficient-isolation gap now has a constructive solution: three-score
root boxes, uniform nuisance containment, and a negative profile-curvature
bound on a larger neighborhood. An **exact rational certificate isolates the
golden stationary maximum within its coefficient tolerance**. With propagated
practical arithmetic allowances, the research engine additionally certifies
global outcomes on **49 of the 50 retained workloads**. The remaining workload
returns an explicit subdivision-budget outcome.

**The requested production fitter has not been delivered.** A guarded,
standalone C++ implementation and its diagnostic propagation and performance
validation remain open. The Python research engine cannot substitute for that
implementation. Consequently the brief's no-implementation branch applies:
no library fitter, family dossier, binding, production test, default fit or
golden is changed. This report does not claim TASK-138 is finished.

The [derivation](2026-11-04-burr-root-data/derivation.md) contains the proofs,
constants, arithmetic contract and rejected curvature diagnostic. The results
below distinguish exact rational evidence, certificates conditional on enlarged
computed enclosures, and independent high-precision numerical checks.

For a finite candidate, use coordinates `(k,c,b)`, with `b=-c log(s)`.
The score and its analytic Hessian are those of the **total sample log
likelihood**, with the constant log-data Jacobian omitted. The negative
Hessian is used for optimization curvature; it is not a model Fisher
information or an inference calculation.

A box about a candidate encloses all three scores through a preconditioned
fixed-point map. The componentwise condition `e + Q r < r` gives existence
and uniqueness of a stationary point by Brouwer and contraction. A separate,
larger product box contains the conditional nuisance optimum uniformly over
a shape interval. An enclosed congruence of its full negative Hessian proves
strict positivity there. The Schur complement then supplies a uniformly
negative profile-curvature bound. This closes the earlier missing local step;
fixed-shape concavity alone was insufficient.

[isolation.json](2026-11-04-burr-root-data/isolation.json) records 33 root boxes,
including 27 local maxima and six profile minima. All 27 maxima have certified
larger neighborhoods. The old root survey supplies proposals, not an
assumption that the survey found every root. Outside the retained winning
neighborhood, the global engine must strictly exclude **every** interval by
upper bounds. A missed competing root therefore prevents closure rather than
silently being discarded. Four of the local maxima are dominated by Pareto.
No general joint-uniqueness theorem is asserted.

The golden root enclosure, with displayed endpoints rounded outward, is:

| Coefficient | Enclosure | Maximum relative discrepancy from frozen golden |
|---|---|---:|
| k | (1.33463745875, 1.33463748545) | < 1.94e-7 |
| c | (1.86041768553, 1.86041772274) | < 5.34e-8 |
| s | (1.19699110925, 1.19699112644) | < 1.68e-7 |

Every point in the exact enclosure meets the golden tolerance `1e-6`.
[exact_root.py](2026-11-04-burr-root-data/exact_root.py) interprets the stored
binary64 observations, trial centers and preconditioner entries as exact
rationals. It reuses the existing rational exp/log Taylor enclosures and
compares Fraction endpoints; floating-point root convergence is not a premise.
[exact-root.json](2026-11-04-burr-root-data/exact-root.json) preserves the
rational endpoints, contraction ratios and positive principal minors of the
negative Hessian throughout the small box. The largest contraction-image
ratio is below `1.74e-6`.

For global exclusion, the larger golden neighborhood is
`1.3306335596855252 <= k <= 1.3386413845181364`. Its practical enclosure
proves `F''(k) < -3.07` throughout, using a padded Frobenius-norm bound rather
than an unchecked numerical singular value. There are 76 conditional profiles;
all 74 intervals outside this neighborhood are pruned, and the unsigned
Weibull envelope excludes `k >= 4000`. The existing small-k theorem excludes
the opposite end. This is a global certificate conditional on the stated
arithmetic enclosures, stronger than the earlier likelihood-gap diagnostic.
Only the small golden root box is additionally checked by exact rational
arithmetic; the entire global search is not represented as a rational proof.

The production-arithmetic mathematics has also advanced. A monotone Weibull
power score brackets the exact solution. Its bracket is propagated into the
intercept, each fitted exponential, the optimized value W, the slope A, and
the seam constants. The seam comparison remains relative to the **same exact
W**; the width of its value enclosure is not inserted as a constant error
in the signed tail inequality. Thus the nonzero-score obstruction from
substituting an approximate Weibull center is avoided.

Dual feasibility is controlled by defining one weight exactly through the
sum constraint and enclosing that definition. Only a correction enclosure
inside `[0,1]` is accepted. Entropy and the positive dual denominator are
then enclosed. This removes reliance on a merely small intercept residual or
the previous empirical `1e-9*n` dual padding. Numerical conditional fits are
only proposals; their earlier padded upper values are discarded.

The practical arithmetic allowance is `64*epsilon*(1+max(abs(lo),abs(hi)))`
per elementary interval operation, with `epsilon=2^-52`. An n-term sum uses
`64*epsilon*(n+1)*(1+sum(max(abs(lo_i),abs(hi_i))))`. The practical libm
assumption is absolute error at most `8*epsilon*max(1,abs(result))` for the
listed elementary functions. All dominance tests are strict after padding;
box and curvature tests require a further 0.1 norm margin. This states the
conditional arithmetic contract explicitly; it does not establish a universal
libm theorem or demand outward-rounded arithmetic. A production implementation
of that contract, including all invalid-domain and nonfinite returns, is still
missing.

The [global results](2026-11-04-burr-root-data/global.json) are:

| Outcome | Workloads |
|---|---:|
| Certified finite root and global exclusion | 23 |
| Certified Pareto supremum, unattained by a finite fit | 15 |
| Certified Weibull supremum, unattained by a finite fit | 11 |
| Subdivision budget exhausted | 1 |

These use the same 50 stored binary64 samples as the seam study: original
seed-138 workloads, the n=500 golden, and the closure counterexamples and
crossing proxies. No new Monte Carlo replication or sample regeneration is
claimed. There are 9,131 cached profiles, including the failed workload's
4,096-profile budget. Likelihoods in this search omit the constant associated
with the chosen log-data normalization; comparisons are unchanged. They must
not be compared directly with the benchmark's original-data log likelihoods.

For the nearly constant sample, a signed tail is certified beyond `k=16384`,
but compact-interval exclusion does not close. Its W enclosure has width
about `4.14e-6`, maximum primal/dual width is `2.34e-5`, and the remaining
upper bound exceeds the feasible lower bound by `1.45e-6`. The result is
`subdivision-budget`, **not Weibull**. Conservative log-input allowances
amplified by normalization, together with the basic one-norm seam remainder,
limit this attempt. The brief permits this stop; the current R fitter has
not acquired that diagnostic.

The degenerate-slope logic does not round a small A to zero or select Weibull.
All three retained near-zero workloads have A enclosures containing zero and
nevertheless receive finite global certificates through a finite witness and
the unsigned tail bound. Four additional synthetic nonnegative or sign-uncertain
slope enclosures reject signed-tail selection. With neither a negative-slope
certificate nor a strict finite/other-face improvement, the permitted outcome
is a tie/numerical budget stop. No automatic exact finite/face tie classifier
or general higher-order seam classifier is claimed. The numerical zero-slope
sample remains a binary64 proxy for the exact family studied previously.

[verification.json](2026-11-04-burr-root-data/verification.json) records independent
80-digit solutions for all 33 stationary roots, containment in every box,
all 50 W/A/P enclosures, the decisive signed-tail comparisons, and the
near-zero tests. Root residual target is `1e-60`. These are numerical
corroboration, separate from the box proofs and the exact golden certificate.
There are no permitted failures in the final reproducer. The one subdivision
budget is an asserted research outcome, not a passed face decision.

A material numerical finding is retained: two earlier 40-node seam-quadrature
curvatures have the wrong sign at stationary maxima.

| Sample | Earlier quadrature | Independent analytic Schur complement in xi |
|---|---:|---:|
| burr-k0.08-r1 | +0.11635844 | -0.05847217 |
| burr-k0.08-r2 | +0.08866641 | -0.04145599 |

At stationarity the exact identity is `H''(xi)=k^4*F''(k)`, so these signs
must agree. The new analytic Hessian boxes and independent calculation agree
on negative curvature. Reusing the old quadrature as a curvature certificate
is rejected. The old evidence remains unchanged, and the new root and tail
certificates do not use that quadrature.

[benchmark.json](2026-11-04-burr-root-data/benchmark.json) compares ten repetitions
of each unchanged C++ fit with the research Python global phase:

| Workload | Current C++ fit | Python global certificate phase |
|---|---:|---:|
| Golden | 21.2 ms | 176 ms |
| Median over 50 samples | 1.65 ms | 108 ms |
| Sum over one pass of 50 samples | 0.124 s | 13.55 s |

The Python column excludes proposal discovery, root-box construction and the
exact rational check. It includes the nearly constant budget exhaustion.
This compares implementations in different languages and is not a C++ port
benchmark. The fast-fit requirement has not been established for a production
replacement; no runtime improvement is claimed.

**Open — guarded standalone implementation and public behavior.** The engine
still imports SciPy proposal solves and uses Python assertions/exceptions,
NumPy matrix factorizations and dynamically typed intervals. A complete
bounded C++ engine with explicit domain, bracket, finiteness, linear-solve and
budget failure returns has not been supplied or validated. It must retain
finite proposals independently of the old tolerance-based face decisions,
and allow the certificate to decide whether any result may be returned.
A local score rescue cannot replace a failed global certificate.

Diagnostic propagation also needs implementation through the
[binding-neutral Burr adapter](../../../bindings/common/distributions/burr/fit.hpp).
The [InvBurr delegation](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp)
explicitly maps the existing Burr statuses and otherwise starts from `Ok`.
Adding a new Burr tie/budget status without handling this boundary would be
unsafe. A compatible failure reason carried with `IterationFailure`, or an
explicitly coordinated delegation change, must be tested. No other-family
file was changed to bypass the brief's scope. Held-subset behavior and default
coefficient tolerances also require validation after the actual port.

**Open — production efficiency and narrow-data allowances.** The demonstrated
budget stop is correct under the proposed contract, but its guarded C++
implementation and cost remain untested. Tighter relative log allowances,
the earlier sharper matrix tail bound, and better conditional proposals may
improve it. None is presented as already implemented. These are concrete
remaining engineering and validation gaps, not a claim that joint uniqueness,
outward rounding or an exact tie classifier is necessary before implementation.
No changes to the settled d/p/q/r, special-function seam, truth fixtures or
code generation are needed for the certificate method developed here.

The final regression and research runs all completed on the **local workstation**
through the prescribed slot locks, which set nice priority 10 and thread limits.
No simbox, sub-agent, installation, commit, git write, board edit or publication
was used. The existing board was read at session start.

| Final command, after setting the temporary directories | Result |
|---|---|
| `/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-04-burr-root-data/run.sh` | All root, global-outcome, exact rational, independent, seam and benchmark checks passed |
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed |
| `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 153, PASS 20507 |
| `/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test` | 127/127 full shards passed |
| `/tmp/umvl-cpu/run4 just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43324 |

The dev skips are expected tier/optional checks. These suites establish
regression health of the unchanged package; they do not certify its current
multistart fitter. Compact final output and toolchain versions are retained in
[validation.json](2026-11-04-burr-root-data/validation.json). Full scratch logs
are under `build/scratch/`.

Reproduction starts with:

```sh
mkdir -p build/tmp build/scratch
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
```

Environment: Linux `7.0.0-38-generic` x86-64, glibc 2.43, GCC 15.2.0,
R 4.6.1, BH 1.90.0.1, Python 3.14.5, NumPy 2.4.6, SciPy 1.18.0,
mpmath 1.3.0. [summary.json](2026-11-04-burr-root-data/summary.json) records
source/script/input hashes, counts, widths and timings. All lasting evidence
is beside this report; no figure or linked result depends on ignored build
output. Pre-existing tracked source, goldens and tolerances are unchanged.

Final syntax, JSON, whitespace and local-link checks passed for all 19 retained
files and 34 local Markdown links; `git diff --check` passed and the tracked
source diff is empty. The prescribed artifact-pane command returned HTTP 403
because this worktree is outside the viewer's configured readable roots.
The report remains at its requested path; no viewer configuration or destination
outside this worktree was changed.
