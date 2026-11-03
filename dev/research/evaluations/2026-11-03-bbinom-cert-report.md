# TASK-113: joint beta-binomial tail bounds and the global-fit certificate

Source revision: `f14d43321b89e5a685eec2889da9fa42679de958`. All pre-existing
tracked files remain unchanged. This report and its supporting scripts and
results are new. No all-free fitter was enabled; the existing C++ and R
behavior, default fits, goldens, and settled d/p/q/r remain unchanged.

## Outcome

There is now a proved joint-shape tail envelope, and the positive-likelihood
escape classification conjectured in the [map study](2026-11-02-bbinom-map-report.md)
is settled by the derivation here. The envelope includes an eventual
`O(1/N)` correction to the unrestricted NB/Poisson face, with data-dependent
constants. Its strongest tested version nevertheless gives no finite-shape
tail clearance at the existing 1,000-size budget on the 2,202 map samples.
It misses all six difficult extended samples even at size `10^12`.

A usable certified global fitting procedure was not obtained. In accordance
with the research-report branch of the brief, no fitter, metadata, binding,
or production test was changed. The missing practical result is substantially
sharper exclusion of large integer-size ranges, followed by the complete
numerical comparison certificate. It is **not** an unresolved closure
classification or a requirement for outward-rounded arithmetic.

## Proved results

The [full derivation](2026-11-03-bbinom-cert-data/derivation.md) supplies the
inequalities and proofs, including the rejected coarse bounds. These are
exact-arithmetic claims; the research programs are numerical checks and do
not constitute a production floating-point certificate.

Let `h` be sample size, `M >= 1` the largest observed count, `S` their sum,
`m` the number of positive observations, and `t=a+b`. Let `L_*` be the
supremum of the sample likelihood over every NB shape and mean, including
the Poisson limit. No NB uniqueness theorem or numerical optimizer
convergence claim is used to prove the bounds.

1. **Uniform kernel comparison.** For every `N > M` and `p` in `[0,1]`,
   the binomial probability at `x` is at most the Poisson probability at
   mean `Np`, multiplied by `exp(x^2 / (2(N-x)))`. Its absolute difference
   from that Poisson probability tends uniformly to zero as `N` grows.

2. **Uniform beta/gamma comparison.** For independent unit-rate gamma
   variables `U,V` with shapes `a,b`, the beta ratio `p=U/(U+V)` is
   independent of the gamma total. Comparing Poisson intensities `Np`
   and `NU/t` gives an NB with shape `a` and mean `Na/t`. A second-order
   expansion, using that independence to cancel the linear term, bounds
   each probability difference by
   `d_x(t) = min(1, A_x/t)`, where
   `A_x = max(8*x^2 + 8*x + 4, 4*x + 6)`.
   This remains valid uniformly over both shapes and every mean.

3. **Joint small-concentration optimization.** For `t <= T`, the zero
   probability is at most `exp(-a*H)`, with `H=log(1+N/T)`. Exact ratios
   to the zero probability reduce the whole-likelihood bound to a shared
   scalar alpha optimization. Two explicit bounds are retained: a
   quadratic-root bound using `(a)_x <= a(a+M)^(x-1)`, and a harmonic
   bound using `log(1+a/j) <= a/j`. Their smaller value is `B_N(T)`.
   Neither replaces alpha by the concentration cutoff before maximizing.

4. **Likelihood error at the empirical scale.** Write `r_k` for empirical
   frequencies and `L_sat = product(r_k^n_k)` for the unrestricted pmf
   likelihood upper bound. Expanding products of shifted NB probabilities
   and bounding every nonempty term by its subsample empirical likelihood
   gives the sharper tail envelope

   $$
   \sup_{a,b>0}L(N,a,b)\le
   \max\left\{B_N(T),\ e^{G_N}\left[
   L_*+L_{\rm sat}\left\{
   \prod_i\left(1+\frac{e\,d_{x_i}(T)}{r_{x_i}}\right)-1
   \right\}\right]\right\},
   \qquad G_N=\sum_i\frac{x_i^2}{2(N-x_i)}.
   $$

   For fixed `T` this bounds every size from `N` onward, since both
   branches decrease with size. Substituting any certified upper bound
   for `L_*` is valid. The numerical face candidates in the map are not
   such upper bounds. Taking `T=cN`, with sufficiently small fixed `c`,
   makes the small-concentration branch less than `L_*` eventually,
   while the other branch is `L_* + O(1/N)`.

5. **Exhaustive growing-size closure.** Any sequence with `N -> infinity`
   and sample likelihood bounded away from zero must have `t -> infinity`.
   Its probabilities are then uniformly approximated, at every fixed
   count, by the NB comparison above. A subsequence with `a -> 0` loses
   every positive observed count; finite positive limiting `a` forces a
   finite positive limiting mean and an NB law; `a -> infinity` forces
   a Poisson law with finite positive mean. Both latter limits are proper,
   ruling out defective limits with additional mass escaping to infinity.
   The bounded-size compactification supplies the binomial, endpoint
   two-point and point-mass faces. Oscillation is handled by subsequences.
   The positive-count hypothesis excludes all-zero samples, which need
   their separate size-zero/nonidentification treatment.

Consequently `sup_(a,b) L(N,a,b) -> L_*`. A finite candidate strictly above
`L_*` has a finite eventual tail cutoff. That statement does not identify
its winning integer size or exclude a better finite-size binomial face.

## Measurements against the map

[Original diagnostics](2026-11-03-bbinom-cert-data/tail-diagnostics.csv),
[refined diagnostics](2026-11-03-bbinom-cert-data/refined-diagnostics.csv),
and [refined summary](2026-11-03-bbinom-cert-data/refined-summary.json)
retain every sample, rather than only the successes. Numerical clearance
means that the bound, evaluated with the map's numerical NB face candidate,
is below the best retained candidate by more than `2e-8*h`. It is an
optimistic tail diagnostic, **not** a certified global fit. Intervening
sizes were not newly enumerated to the large cutoffs in this table.

| Tail begins at size | All numerical clearances | Nonconstant samples clearing | Clearances whose candidate has finite-shape status 0 |
|---:|---:|---:|---:|
| 1,001 | 72 | 56 | 0 |
| 1,000,000 | 1,187 | 1,153 | 716 |
| 1,000,000,000,000 | 1,953 | 1,919 | 1,350 |

Tuning uses the monotonic crossing of the small- and large-concentration
bounds, with 65 bisections in `log(T)` over `[-30,100]`. The script returns
only diagnostics, never an estimate at a size or tuning cap. No claim is
made about unsearched tuning values outside that range.

The first pass used a first-order coupling, a coarse `(T+1)_(x-1)` bound,
and unnormalized product error. It cleared only ten constant samples at
size 1,001. A necessary-condition calculation ruled out every tuning for
that coarse envelope on all 2,168 nonconstant samples. Even `(0,1,2)` would
have required size above `10^586` with that envelope. This is a rejected
bound, **not** a requirement of the final bound or of BB fitting. The
refinement removes that artificial obstruction: for `(0,1,2)` it gives
upper log likelihood `-2.60226835` at 1,001 and `-3.69013623` at one million,
against the exact finite maximum `-3.29583687` at `BB(2,1,1)`.

The six difficult extended samples still fail the refined comparison:

| Map index | Best checked size | Checked BB log likelihood | Refined upper log likelihood at `10^12` | Feasible Poisson-mixture improvement over checked BB |
|---:|---:|---:|---:|---:|
| 1233 | 386 | -58.25262908 | -58.17835142 | 0.41595964 |
| 1266 | 1,000 | -59.12434578 | -59.09752942 | 0.24283089 |
| 1309 | 110 | -158.54873046 | -147.57052439 | 0.65783661 |
| 1565 | 297 | -88.63331239 | -88.13597229 | 0.12031828 |
| 1695 | 131 | -142.45697000 | -142.25438748 | 0.58540293 |
| 1935 | 730 | -83.94523981 | -83.90306165 | 0.00064213 |

Size 1,000 in row 1266 is still a capped checked candidate, not an estimate.
None of the other displayed sizes is claimed to be globally optimal.

The last column tests a tempting alternative: replace the NB face by all
Poisson mixtures. [Explicit witnesses](2026-11-03-bbinom-cert-data/mixture-witnesses.csv)
mix the map's NB candidate with one Poisson component. Every hard sample
has a feasible mixture exceeding its checked BB candidate. Such a larger
face therefore obstructs this shortcut even at infinite size. More
generally, the derivation proves that every finite-shape NB candidate has
a strictly improving direction in the unrestricted Poisson-mixture class.
That class cannot simply be substituted without measuring the resulting gap.

## Open items

- **Practical global size exclusion.** The new joint bound is valid, but
  its constants are too large for the existing 1,000-size enumeration
  budget. A sharper bound or a certificate excluding integer-size intervals
  without evaluating every profile is missing. One-million-size tail
  clearances do not justify skipping the intervening profiles; the current
  fixed-size solver itself costs `O(N)` per likelihood evaluation.
- **Complete numerical comparison certificate.** A production procedure
  must supply a certified NB-face upper likelihood, propagate the existing
  finite-profile upper allowances across all retained integer sizes and
  faces, and document its practical floating-point margin and budget
  behavior. The research scripts use numerical face candidates and do not
  implement these steps. An optimizer stopping criterion alone is not an
  upper bound. Outward rounding is not required to address this gap.
- **Winning-face decisions and hard-case answers.** The 158 numerical NB
  cases in the map remain unclassified globally. The six extended cases
  above remain unresolved, including the improving candidate at the cap.
  Local NB or binomial face slopes do not settle these comparisons.
  The exact empirical-distribution examples remain valid special cases,
  rather than validation of a general search.

These are the concrete blockers to enabling this held subset and adding its
production C++/R tests. No numerical clearance above is exposed as an MLE,
a globally winning limit, or a substitute size-budget outcome.

## Validation and reproduction

Every final command finished successfully on the local workstation before
this report was written. Computation used the supplied CPU slot locks,
`nice -n 10`, and one BLAS/OpenMP thread per worker. Environment: Linux
x86-64 `7.0.0-38-generic`, GCC 15.2.0, R 4.6.1, Python 3.14.5, NumPy 2.4.6,
SciPy 1.18.0, mpmath 1.3.0. Seeds are 113. No simbox, sub-agents, dependency
installation, commit, publishing, or board edit was used.

| Final check | Result |
|---|---|
| `just cpp-test` | 106/106 dev shards passed; built-in BH-header fallback used. |
| `just test` | 20,507 passes; 0 failures, 0 warnings, 153 expected tier skips. |
| `just test-full` | 43,324 passes; 0 failures, 0 warnings, 0 skips. |
| Original research checks | 600 seeded trials at 70 digits; all tested inequalities passed at tolerance `1e-55`. |
| Refined research checks | 120 seeded trials at 70 digits; second-order coupling, both small-concentration bounds and normalized-product inequality passed at tolerance `1e-55`. |
| Fresh C++ map checks | 3,214 retained profiles re-evaluated; statuses matched and maximum log-likelihood difference was zero. |
| Extra `(3,4,7)` check | Finite `N=7` log likelihood `-5.6545863016222677`, exceeding its `N=13` binomial comparison `-5.7841803291758035`. |
| Mixture witness checks | All 13 re-evaluated at 70 digits; maximum discrepancy `2.61e-8`, within the declared `2e-7` tolerance; all six hard-case mixture improvements retained. |

The 3,214 calls cover all 2,202 retained map winners, the six extensions and
all 1,006 selected profile-grid points, including `(1,1,5)`, both OPZ
samples and the one-count perturbation. These reuse the existing fixed-size
solver; they do not supply independent global certificates. The high-precision
checks provide numerical corroboration of the derivations, not proofs of
floating-point comparison reliability. The C++ and R suites establish
regression health of the existing supported fitters, not support for the
all-free subset.

From the worktree root:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
/tmp/umvl-cpu/run sh dev/research/evaluations/2026-11-03-bbinom-cert-data/run.sh
/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-03-bbinom-cert-data/refine.py
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test
/tmp/umvl-cpu/run4 just test-full
```

[Original check summary](2026-11-03-bbinom-cert-data/summary.json) and
[fresh profile/witness verification](2026-11-03-bbinom-cert-data/verification.json)
retain measured results. Scratch executables and test logs are under
`build/scratch/`; all lasting evidence needed to interpret the findings is
kept beside this report.
