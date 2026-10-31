# Fit speed: TASK-133 remainder and TASK-120 (2026-10-31)

Baseline: `68428d91bb027e99dbbe2972b4e17c78807769f1`; final source is the
uncommitted fit-speed worktree plus the recorded source hashes. No commits,
board edits, installs, adapter changes, signature/status changes, golden
regeneration, or site changes were made. Compiler: c++ (Ubuntu 15.2.0-16ubuntu1) 15.2.0; BH 1.90.0.1. See the [source hash manifest](2026-10-31-fit-speed-data/source-sha256.csv) for the tested files.

The largest measured gains are Tukey lambda inversion reuse, fixed-shape skew-t
bisection, fixed-shape skew GED normalizers, and GB2 member-seed subsampling.
The experiments do not establish a universal speed ceiling for the other fits.

## Paired timings

These are single wall-clock runs at `-O2`, tol `1e-8`, maxit `1000`, on the
starting revision's n=500 golden data; held coefficients are its golden
estimates. Both binaries run in one shared CPU slot under nice 10, with
BLAS/OpenMP pinned to one thread. No simbox. Frequency/load variation remains;
small differences should not be interpreted as demonstrated speed-ups.
The table includes every subset exceeding one second in either paired run and
the requested representative subsets even where this machine took less time.
The complete table, including both statuses and likelihoods, is in
[the timing CSV](2026-10-31-fit-speed-data/timings.csv).

| Family | Held | Before s | After s | Status old/new |
|---|---|---:|---:|---|
| tlambda | none | 3.923 | 2.593 | 0 / 0 |
| tlambda | mu | 1.045 | 0.689 | 0 / 0 |
| tlambda | sigma | 15.305 | 11.790 | 0 / 0 |
| tlambda | mu, sigma | 3.939 | 2.967 | 0 / 0 |
| emg | none | 3.694 | 3.706 | 0 / 0 |
| bhatt | none | 2.340 | 2.400 | 0 / 0 |
| sstd | nu, xi | 2.884 | 1.566 | 0 / 0 |
| sstd | sd, nu, xi | 1.002 | 0.399 | 0 / 0 |
| sged | sd, nu, xi | 3.250 | 0.718 | 0 / 0 |
| slash | none | 1.406 | 1.284 | 0 / 0 |
| fpareto | min | 0.898 | 0.894 | 6 / 6 |
| gb2 | none | 0.126 | 0.120 | 0 / 0 |
| gb2 | shape2, shape3 | 1.316 | 0.941 | 9 / 9 |
| std | sd | 5.196 | 4.089 | 0 / 0 |
| lst | sigma | 2.034 | 2.053 | 0 / 0 |

Status 0 is Ok; fpareto status 6 is DoubleParetoBoundary; GB2 status 9 is
SearchUnresolved. Other status codes follow the family enum. The GB2 n=500
rows do not execute subsampling, so their timing variation is not evidence of
a speed-up from that change. Bhattacharjee is unchanged; EMG and scale-t show
no demonstrated default-fit gain in the final timing pair.

TASK-120 uses the golden observations repeated to n=1e6 (GB2) and n=1000
(Bhattacharjee), one before run and one final after run each. These are
reproducible workload timings, not a distribution-wide performance estimate.

| Family | n | Before s | After s | Status old/new | logLik old/new |
|---|---:|---:|---:|---|---|
| gb2 | 1000000 | 227.126 | 103.358 | 0 / 0 | -2645148.1725353771 / -2645148.1725353771 |
| bhatt | 1000 | 5.649 | 5.824 | 0 / 0 | -1792.6209196544758 / -1792.6209196544758 |

The final GB2 large-workload estimates, status and likelihood are byte
identical to the baseline (timing excluded). [Both parameter vectors and
classifications](2026-10-31-fit-speed-data/large-timings.csv) are retained.

## Equivalence evidence

The paired driver uses seed `13320261031`, 200 independent samples **at each**
of n=8,16,32, and 20 at n=500, for every held subset, including none/all where
supported. Positive families use exponentiated normal samples; other families
use normal samples with a quadratic perturbation every fifth sample. Fpareto
always holds min. Controls and fixed values match the timing driver.

The numerical-equivalence rule requires the same status, coefficient error
<=1e-8 and likelihood error <=1e-12, scaled by max(absolute old value, absolute new value). NaNs match only NaNs and infinities only the same infinity.
Byte identity refers to the emitted estimates, likelihoods and statuses; the
driver does not emit iteration counts or restriction bits. Matched non-Ok
statuses remain boundary/failure outcomes, not successful fits; their counts
are recorded in the CSV. It is not a proof
for arbitrary data, or an independent statistical validation of the fitter.

| Family | Rows | Byte identical | Tolerance equivalent | Listed differences |
|---|---:|---:|---:|---:|
| tlambda | 4960 | 4960 | 0 | 0 |
| emg | 4960 | 4960 | 0 | 0 |
| bhatt | 4960 | 4960 | 0 | 0 |
| sstd | 9920 | 9920 | 0 | 0 |
| sged | 9920 | 9920 | 0 | 0 |
| slash | 2480 | 2480 | 0 | 0 |
| fpareto | 9920 | 9920 | 0 | 0 |
| gb2 | 9920 | 9920 | 0 | 0 |
| std | 4960 | 4960 | 0 | 0 |
| lst | 4960 | 4960 | 0 | 0 |

[Summary and output hashes](2026-10-31-fit-speed-data/equivalence.csv).
The 20 additional GB2 golden-bootstrap samples at n=1500 (seed 13320261031) gave 14 byte-identical, 4 tolerance-equivalent and 2 listed differences. [Both statuses and likelihoods for exceptions](2026-10-31-fit-speed-data/gb2-bootstrap-differences.csv); [all twenty paired records, including every last-bit difference](2026-10-31-fit-speed-data/gb2-bootstrap-comparison.csv).

## Changes, rejected approaches and remaining costs

### Tukey lambda

The retained change shares `log(1-u)` between quantile magnitude and derivative
calculations in the ordinary smaller-tail Newton solve, and retains the
computed magnitude for the slope. It uses the same elementary expressions,
bracket expansion, Newton iterates, stopping rules and reduction order. No
support/tail branch or optimizer start changes. Reusing a computed value is
exact for deterministic arithmetic; the paired experiment checks the compiled
implementation. The existing multistart search remains numerical, without a
new global MLE theorem.

A fitter-local sorted warm-start evaluator was implemented and tested. Its
36,000 pointwise log-density checks over both tails, both shape boundaries and
near-boundary shapes had zero failures at 1e-13 scaled relative tolerance;
worst error was 2.56e-14. It tries six residual-checked Newton steps, falling
back to the existing inversion. Golden timings: sigma held 15.166 -> 15.069 s;
mu and sigma held 5.227 -> 5.786 s. 3 of 20 sigma-held pilot samples exceeded
the strict coefficient tolerance. One example has old/new status Ok, logLik
-11.318242314023870 / -11.318242314023873; mu
-0.71229457815189479 / -0.71229456127118618. Pointwise agreement did not justify
retaining the fit-level change, especially on the slowest path. The trial is
[preserved](../../validation/fit-speed-trials/fit-speed-warm-tlambda.patch), with
[the pilot difference](2026-10-31-fit-speed-data/tlambda-warm-differences.csv).
The remaining cost is the unchanged multistart likelihood search.

### EMG

Only the location score's heap-allocated two-element vector is replaced by a
fixed-size array. Arithmetic, starts, convergence and failure-score gates are
unchanged. An initial paired default timing was 3.443 -> 3.259 s. The final pair
above is the relevant measurement; variation limits interpretation of a small
gain. A full-profile cache gave 3.694 -> 3.680 s and was rejected as unhelpful;
likelihood-invariant hoisting gave 3.694 -> 3.791 s and was also rejected.
The expensive location profile and all 25 starts remain.

### Bhattacharjee

No production change survives. Substituting double erfc in the central
extended calculation gave 2.256 -> 2.041 s on the default fit, but moved a
coefficient beyond the existing golden tolerance: a
1.7336058787117143 -> 1.7336059088831273, status Ok in both, logLik
-896.31045982723822 -> -896.31045982723845. It was rejected before a truth-table
claim was made. The final code retains its original erfc and tail rules.
A profile cache alone gave 2.340 -> 2.366 s and was rejected. Extended density
and location calculations remain the bottleneck; no claim that a faster
accurate erfc/erfcx primitive is impossible is made.

### Skew Student-t

The nu/xi-held scale profile formerly continued its 110 bisections after the
midpoint rounded to an endpoint. The retained exit skips only that redundant
suffix. Either the interval stays fixed or collapses to that endpoint; the
represented final midpoint and scale are unchanged. All location grid points
and observation-interval refinements remain. In fixed-shape/skew conditional
location/scale fits, density constants are also hoisted without changing
per-observation grouping. This is evaluation equivalence, not a new global
mode-isolation proof.

### Skew GED

The fixed shape/skew kernel scale and normalizers are computed once per
conditional search, preserving each density contribution's grouping and the
input reduction order. Grids, kink refinements and failure decisions remain.
The remaining work is evaluation of the unchanged profile and all nominated
observation intervals.

### Slash

The scale bisection skips its suffix only after the midpoint rounds to an
endpoint, preserving the final represented scale. Location grids/refinements,
collapse comparisons and failure statuses remain. The modest single-run timing
difference is not a robust performance claim. The full location grid and its
scale solves still dominate; no search reduction was accepted.

A further exact residual cache was retained after a 1.108728 -> 1.051878 s
paired trial (5.1%). Each double residual v-m is computed once per location
profile and reused across scale bisections. Division by the represented scale,
squaring, score reduction and final likelihood remain in their original order.
This removes repeated subtractions without a different root or search. A
separate full prescribed comparison checks the retained implementation.


### Flexible Pareto

No fpareto-specific production change survives. It continues to delegate to
GB2 after shifting by the held threshold, and inherits the GB2 member-seed
change above n=1000 when all four GB2 coefficients are free. Its own shift,
validation and final status handling are unchanged. No large-fpareto timing
claim is made. The tested shape-profile cache
changed 0.898 -> 0.883 s on the min-held golden case (boundary status 6 and
logLik -1303.3400029943041 in both), while slowing ordinary GB2
0.126 -> 0.157 s. This did not justify retaining the cache. The boundary is
still reported as a boundary status, never as an interior estimate. Full-data
GB2 profiles and boundary comparisons remain the cost.

### GB2 seeds

When n>1000 and all GB2 parameters are free, only the Burr, inverse-Burr and
beta-prime member fits use a deterministic 1000-observation sample, one distinct index from each of 1000 contiguous index blocks with a
fixed deterministic within-block offset. This changes candidate starts. All transformed-gamma and
reciprocal transformed-gamma boundary fits, likelihoods, profile solves,
refinements, boundary comparisons and final score/failure gates still use all
observations. For n<=1000 the original seed calls remain. No result signature,
status, score threshold or iteration budget changes.

This is a numerical search change, not an exact-equivalence proof. The
prescribed small-sample comparison alone cannot validate the new branch;
the large repeated-golden workload, a regression above the threshold and the
supplemental bootstrap comparison address that gap numerically. They do not
prove identical basin selection for every large sample. Two supplemental
bootstrap cases lie outside the 1e-8 coefficient tolerance (largest relative
coefficient difference below 3.4e-7): sample 3 has status Ok in both and logLik
-3906.4731639727415 in both; sample 14 has status Ok in both and logLik
-3920.7855193896871 -> -3920.7855193896862. These are explicitly listed
differences, not tolerance-equivalent parameter vectors. Changed starts led
the unchanged refinement/score gate to nearby represented points with the
same or slightly higher computed likelihood. That is numerical evidence, not
a proof that the parameter difference is immaterial or that the optimum is
unique. All estimates and both likelihoods/statuses are in the supplemental CSV.
Among the four tolerance-equivalent cases, samples 4, 12 and 18 have computed
likelihood decreases of about 1.82e-12, 9.09e-13 and 3.64e-12 absolute
respectively (under 1e-15 relative). These are reported as permitted last-bit
differences inside the stated equivalence tolerance, not strict floating-point
nondecrease. Sample 8 has a last-bit increase. No status changes occurred. Deterministic sampling
can depend on input order; the unchanged grid and final full-data checks remain
necessary. An initial modular stride was replaced because, for n=104729, it
would select the same index repeatedly. Disjoint blocks prevent that aliasing
and the quotient/remainder implementation avoids index-product overflow. The transformed-
gamma faces and full-data profile refinements bound the remaining cost.

A separate exact profile cache was rejected: the ordinary golden GB2 fit
slowed 0.126 -> 0.157 s. The intermediate large-sample result including this
cache was 106.007 s; the final uncached result is the one in the table above.

### Standardized and location-scale Student-t

The shared fixed-df nuisance search computes its normalizer once. It retains
exp(log(s)) scale reconstruction, input order and per-observation LstCost
arithmetic. There are no grid or convergence changes. The standardized-t
held-scale timing improved in the paired run; scale-t's 2.034 -> 2.053 s pair
shows no demonstrated gain. The remaining nested profile grid was already
memoized in [part 1](2026-10-31-slow-fits-report.md); this job does not claim a
new certificate for its nominated modes.

A further exact cache of refinement-point objective values was tested beyond
the existing grid cache: 2.033496 -> 2.011174 s, with identical estimates,
likelihood and status. The 1.1% single-run difference is too small to establish
a benefit, and the cache adds allocations/lookups. It was rejected. The
[preserved patch](../../validation/fit-speed-trials/fit-speed-lst-cache.patch)
keeps this experiment reviewable. No search change was accepted for scale-t.


### Fresh baseline profiles

These `-O2 -pg` runs are separate from the uninstrumented timing table. gprof
sampling does not reliably attribute dynamically linked libm work, and inlining
can move costs into callers. Counts support workload attribution, not a proof
of correctness or a lower bound on achievable runtime.

| Baseline case | Instrumented wall s | Named work counted by gprof |
|---|---:|---|
| bhatt, none held | 2.408 | 16 starts; 3,071 profile evaluations; 1,615 location solves; 4,243,500 central-density calls |
| emg, none held | 3.732 | 25 starts; 4,145 location/profile evaluations; 36,359 two-element extended-score vector destructions |
| slash, none held | 1.120 | Most samples map to an ambiguous `std::ctype`/inlined symbol; no numerical call attribution claimed |
| fpareto, min held | 0.727 | 1,086 GB2 shape profiles; 46,335 inner evaluations; 618 transformed-gamma CGF evaluations |
| lst, sigma held | 2.071 | 100 conditional profiles and 100 inner location maximizations |

In particular, do not read gprof's apparent EMG vector-destructor percentage
as time spent allocating, or the slash `std::ctype` label as an I/O bottleneck.
Inlining/aliases and unaccounted libm time make those percentages unreliable.
The named call counts above support the narrower workload statements.
Commands: build the baseline driver with `-O2 -pg -std=c++17`; run mode 0 with
masks 0,0,0,1,2 respectively, setting `GMON_OUT_PREFIX` under `build/scratch/`;
then `gprof -b <binary> <prefix>.*`. The code and data are otherwise the same as
the paired uninstrumented runs.


The [trial patches and reproduction notes](../../validation/fit-speed-trials/README.md)
retain the rejected approaches rather than leaving their rationale only in chat
or ignored build output.

## Validation and reproduction

All required final commands completed successfully through one foreground
`/tmp/umvl-cpu/run4 bash build/scratch/final-tests.sh` invocation:

- `just cpp-test`: 104/104 shards passed.
- `just cpp-test-no-long-double`: 125/125 shards passed.
- `just test`: FAIL 0, WARN 0, SKIP 151, PASS 18887.
- `just test-full`: FAIL 0, WARN 0, SKIP 1, PASS 39040.
- `just quality tlambda emg bhatt sstd sged slash fpareto gb2 std lst`: all ten families passed.
- The C++ dev and precision-proxy suites include the new GB2 regression above the sampling threshold, using public defaults.
- The independent truth tables for the retained Tukey lambda inversion and original Bhattacharjee primitive are exercised by the C++ suites; no truth or golden fixture was regenerated.

The dev-tier skips are intentional tier exclusions. The full-tier skip is
`test-dist-sn.R:136:3`: `{sn}` is not installed; no package was installed for
this job. Quality also printed shell-startup notices about a missing
`~/.cargo/env`; all ten quality checks passed. No unmatched status or likelihood change
occurred in the prescribed small-sample matrices. The supplemental GB2
coefficient/last-bit differences are disclosed separately above.


From the worktree root, the concrete paired-build commands are:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp"
export XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
BH_INCLUDE=$(Rscript -e 'cat(system.file("include", package="BH"))')
/tmp/umvl-cpu/run c++ -O2 -std=c++17 -Ibuild/scratch/before-include -I"$BH_INCLUDE" dev/validation/fit-speed-comparison.cpp -o build/scratch/before
/tmp/umvl-cpu/run c++ -O2 -std=c++17 -IunivariateML-cpp/include -I"$BH_INCLUDE" dev/validation/fit-speed-comparison.cpp -o build/scratch/final-array
/tmp/umvl-cpu/run build/scratch/before tlambda 1 > build/scratch/tlambda-before-equivalence.txt
/tmp/umvl-cpu/run build/scratch/final-array tlambda 1 > build/scratch/tlambda-after-equivalence.txt
/tmp/umvl-cpu/run python3 dev/validation/compare-fit-speed.py build/scratch/tlambda-before-equivalence.txt build/scratch/tlambda-after-equivalence.txt --differences build/scratch/tlambda-differences.csv
```

Repeat the run/compare commands for each family listed above. The job used one
locked foreground script to retain its slot across the comparison batch.
Run from the worktree root. Mode 0 times golden subsets; mode 1 emits the full
seeded comparison; mode 2 runs the TASK-120 size; mode 3 emits twenty n=1500
bootstrap comparisons. Optional arguments select a held mask and sample count.
Mask bits follow the family's C++ held-argument order (for fpareto, min first).
The driver reads the unchanged golden data and freezes the starting held
estimates. `dev/validation/compare-fit-speed.py` classifies paired outputs and
writes every exception with both statuses and likelihoods.

Before/after include trees, binaries and raw logs remain under `build/scratch/`;
the baseline source revision, commands, workloads, numerical conclusions,
timing/equivalence CSVs and source hashes are retained here. Final validation
uses `/tmp/umvl-cpu/run4`; single-thread comparisons use `/tmp/umvl-cpu/run`.
TMPDIR, JUST_TEMPDIR and XDG_RUNTIME_DIR are set to this worktree's `build/tmp`.

## Interface or metadata changes that could help

- None is required for the retained changes.

No adapter, `*_mle` signature, result/status definition, front matter, generated R d/p/q/r, special-function
backend or truth fixture was changed. A future accurate Bhattacharjee primitive
would need its own pointwise, tail, golden and precision-proxy evidence.
