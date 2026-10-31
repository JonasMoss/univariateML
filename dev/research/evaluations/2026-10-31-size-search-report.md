# Integer size searches: hybrid certificate

Source revision `2e58bf8b9e76eaa63c721ac6af9f27c5716163a7` plus the
uncommitted changes in this worktree. The finite-window tbinom branch retains
the master search and its statuses, then attempts a numerical certificate on
successful fits that beat the truncated-Poisson face by `1e-9 * n`. Infinite
windows and the ztbinom and zib searches are unchanged.

For a finite upper support integer U, the proof below bounds the finite-size
profile above by the Poisson-face profile plus a monotone correction. The
implementation evaluates that correction with `log1p`, brackets and bisects
the smallest cutoff N* with `face + correction(N*) < best - 1e-9*n`, and
counts all previously unvisited feasible sizes through N*, including those
below U where the conditioning support changes. Each fixed-size likelihood
is concave in logit probability: its second derivative is minus the sample
size times the conditional variance. The existing mean solver profiles it.

The optional pass runs only when the estimated work is at most 40 million
units. One profile is charged `max(400, n + sum(x) + 100 * (U-L+1))`
units, so at most 100,000 profiles are admitted. This is a cost model, not a
wall-time guarantee. If the cutoff is unavailable, too costly, the upper
support exceeds the two-million-term numerical limit, or any added profile
fails, the master result is returned unchanged. If a better candidate appears,
the cutoff is recalculated. An entirely completed pass gives a numerical
certificate under the `1e-9*n` margin; it is not interval arithmetic or a
proof of floating-point rounding error bounds. TpoisLimit and all fallback
results remain numerical, not certified. The work budget does not change a
master Ok to failure.

## Comparison

The same seeded comparison as the earlier work uses `std::mt19937(1152026)`:
four window configurations, 200 samples of cycling sizes 8, 16 and 32 plus
20 samples of size 200 per window. Source sizes 8, 12, 16, 20, 24 and
probabilities .15, .30, .45, .60, .75 cycle by sample, with rejection outside
1..8. Windows are (0) both bounds estimated, (1) a=0 and b estimated,
(2) a estimated and b=10, (3) a=0 and b=12. Status values are 0 Ok,
5 NumericalFailure, 6 TpoisLimit. The single-slot runs used C++17 `-O2` with
installed BH headers; timings are fitter wall time, not sample generation,
and were not controlled benchmarks.

All 880 statuses match master: 698 Ok and 182 TpoisLimit. No fitted size
changed. The optional pass completed for 600 samples; the other 280 include
182 limit results and 98 successful results without a completed certificate,
including two-point and constant cases. Total fitter time was 2.098 seconds
on master and 14.200 seconds on the hybrid; the largest hybrid fit took
1.078 seconds. This comparison is numerical evidence, not a general proof
that no sample changes size. The cutoff 327 test verifies enumeration against
an independent fixed-size profile; the large-window fixture has a cutoff of
approximately 26.85 billion and checks fallback to the master fit.

[Master results](size-search/2026-10-31-master.csv),
[hybrid results](size-search/2026-10-31-hybrid.csv), and
[comparison sources](size-search/compare-hybrid.cpp) contain per-sample status,
estimates, likelihood, cutoff and time. Hybrid columns additionally record
whether the optional pass completed and the estimated work; the diagnostic
macro is defined only in the standalone comparison build. To reproduce master,
compile [the master harness](size-search/compare-master.cpp) against an include-tree copy
whose `tbinom/mle.hpp` is obtained using `git show 2e58bf8:univariateML-cpp/include/cppunivariate/distributions/tbinom/mle.hpp`;
compile the hybrid source with `-DCPPUNIV_SIZE_SEARCH_DIAGNOSTICS` against the
changed include tree. Both runs use the slot lock and one BLAS/OpenMP thread.

## Validation

All four commands ran through `/tmp/umvl-cpu/run4`, with temporary files under
`build/tmp` and BLAS/OpenMP pinned to one thread. `just cpp-test` passed
104/104 development shards; `just test` passed with 0 failures, 0 warnings,
151 skips and 18,887 passes; `just quality tbinom` passed; `just test-full`
passed with 0 failures, 0 warnings, 1 skip and 39,040 passes. The full-tier
skip is the unavailable optional `sn` package. The comparison CSV confirms
that the cutoff-327 test sample (window 0, sample 7) ran the certificate,
while the large-window fixture's 26.85-billion cutoff triggers the fallback.

## Mathematical investigation

## Zero-truncated binomial

**Literature report, not independently reproduced.** DeRiggi (1994),
“Sufficient conditions for unimodality of the positive binomial likelihood
function”, *Statistics & Probability Letters* 19(1), 1–4,
[doi:10.1016/0167-7152(94)90061-2](https://doi.org/10.1016/0167-7152(94)90061-2),
reports a counterexample to general unimodality with a local maximum and a
saddle point. The abstract does not provide its observations or establish an
integer-size counterexample; the full paper and its 1996 correction have not
been checked. Thus this observation cannot justify an integer descent stop.
DeRiggi (1983), “Unimodality of Likelihood Functions for the Binomial
Distribution”, *Journal of the American Statistical Association* 78(381),
181–183, [doi:10.1080/01621459.1983.10477949](https://doi.org/10.1080/01621459.1983.10477949),
concerns the untruncated model.

The current free-size, free-probability search examines an exponential grid,
refines apparent peaks, and stops after non-improving cells. It compares the
best sampled value with the zero-truncated Poisson face with a numerical
allowance. Neither this stop nor the face comparison rules out an unsampled
finite size. This remains unchanged; the resulting `Ok` and `PoissonLimit`
statuses do not carry the global certificate required by R6. A proof or a
validated tail enclosure is still required. The all-ones point mass and
held-probability branch are separate cases and are not covered by this claim.

## Zero-inflated binomial

**Proved reduction.** For a fixed size and probability, write the base zero
probability as q, the empirical zero fraction as z, and the conditional
positive density as h. If q <= z, maximizing over the inflation parameter
sets the fitted zero fraction to z, and the positive likelihood is the
zero-truncated binomial likelihood times a factor depending only on z. If
q > z, the constrained optimum is zero inflation and the likelihood is the
ordinary binomial likelihood. This follows by maximizing
`z log(r) + (1-z) log(1-r)` over r in [q,1]. The branch boundary depends on
size and probability. Therefore a theorem for either unconstrained profile
alone does not prove unimodality of the constrained maximum, and a bound must
also cover the binomial branch. This reduction does not cover held inflation.

The current grid and numerical ZIP comparison remain unchanged. A ZIP face
within tolerance of the sampled best does not rule out a larger unsampled
finite-size likelihood. The held-inflation free-size search has the same
unresolved global issue. No general integer-profile theorem or computable
bound covering those cases was established here.

## Truncated binomial

**Proved finite-window upper bound in exact arithmetic.** Let the conditioning
support be the fixed integers L through U, with finite U, and let N >= U.
For `lambda = N p/(1-p)`, write the unnormalised binomial mass as
`lambda^k/k! * exp(c_N(k))`, where
`c_N(k) = sum_{j=0}^{k-1} log(1-j/N)`. The corresponding Poisson kernel is
`lambda^k/k!`. For every k in [L,U],
`c_N(U) <= c_N(k) <= c_N(L)`. Consequently its normaliser is at least
`exp(c_N(U))` times the Poisson normaliser, and for observations x_i,

```
log L_N(lambda) <= log L_Pois(lambda)
                    + sum_i c_N(x_i) - n c_N(U).
```

Taking the supremum over lambda yields
`profile_N <= profile_Pois + sum_i c_N(x_i) - n c_N(U)`.
Each difference `c_N(x_i)-c_N(U)` is nonnegative and decreases with N,
so the rightmost correction bounds all sizes at least N and tends to zero.
The inequality includes endpoint parameter values by limits. For a finite
candidate whose likelihood strictly exceeds the Poisson-face supremum, it
therefore gives an eventually finite tail cutoff. All smaller integer sizes
must still be checked or excluded by another proof. If U=N, `c_N(U)` is
finite; N>=U is essential. This bound does not cover an infinite upper
window. It also does not prove the Poisson face wins when the finite best is
at or below it. A rigorous numerical certificate would need outward-rounded
evaluations of both the face supremum and the correction, and certified
comparisons of the finite profiles; the implemented floating-point profile
and tolerance checks do not supply these.

The existing P2b search scans an initial run, expands geometrically, refines
observed peaks using local unimodality, and stops after four values close to
the truncated-Poisson face. Proximity to a limit is not a bound on all
subsequent values; `Ok` is certified only when the additional finite-window
pass described above completes, and `TpoisLimit` remains numerical. The untruncated delegation, the two-point
window and held-probability paths have distinct arguments and were not
revalidated here.

