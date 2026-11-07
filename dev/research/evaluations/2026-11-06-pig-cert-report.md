# PIG fit certification — TASK-116.9

Every supported free subset now has a global certificate or a structured limit, tie, budget or numerical stop. The dossier declares `certified: yes`. The existing golden file and all golden tolerances are unchanged. All required final test runs finished successfully.

The tested base is `bb734cd78b846a565d8835606328bb1285a82db6`, branch `codex/pig-cert`, with uncommitted changes. The final fitter SHA256 is `b12182f99326b42ff9524a7c2acff4858178b647da4405b9252fa5decdbb2ccc`. [The validation manifest](2026-11-06-pig-cert-data/validation.json) records the source hashes, commands, machines and results. No commit was made.

## Mathematical result and supported subsets

For a sample containing at least one positive count, let $m=\bar x$ and $v=n^{-1}\sum_i(x_i-m)^2$.

| Free coefficients | Existence and uniqueness | Implemented certificate |
|---|---|---|
| Mean and shape | Unique finite maximum iff $v>m$; otherwise an unattained Poisson supremum | Exact mean reduction to $m$, followed by the strictly concave shape profile |
| Shape, mean held at $\mu>0$ | Unique finite maximum iff $v+(m-\mu)^2>m$; equality also selects Poisson | The same shape certificate at the held mean; a closed form for binary counts |
| Mean, positive shape held | Always a unique finite maximum | A strictly monotone mean-score equation with a proved finite bracket |
| Neither | Likelihood evaluation at the held coefficients | Direct evaluation with count-work and numerical guards |

All-zero data with free shape attain the zero mass at shape zero. A held mean is preserved; a free, unidentified mean is represented by one. This is a concrete failure of joint uniqueness: `(mean, shape) = (1, 0)` and `(2, 0)` give the same maximum. With positive shape held, all-zero data instead have the unattained mean-zero supremum, reported as a Poisson-zero limit. Shape zero held against positive counts remains incompatible.

The new proof uses the finite half-integer Bessel polynomial $Q_{k-1}(1/z)$. If $E_k,V_k$ are the mean and variance of its degree under its positive term weights, then

$$
V_k<\frac{E_k(k^2-E_k^2)}{k^2+E_k^2},\qquad k\ge2.
$$

The polynomial differential equation supplies an explicit positive derivative at every hypothetical crossing of this bound, while its expansion at infinity is strictly below the bound. This excludes a crossing. In the compact shape coordinate

$$
w=\frac{\sqrt{1+2\mu^2/\lambda}-1}{\sqrt{1+2\mu^2/\lambda}+1},
$$

the resulting upper bound on log-mass curvature factors into a square times a negative term. Every positive count therefore has strictly concave log mass in $w$; zeros contribute a linear term. The boundary score gives the criteria in the table and excludes all missed peaks and exterior tails. The fixed-$z$ mean reduction then establishes joint uniqueness as well.

The weaker degree-variance bound $V_k\le E_k$ proves the held-shape mean equation is strictly monotone. Its unique root lies in $(m/2,2m)$; the implementation uses the wider starting bracket $[m/4,2m]$ to protect tiny-shape cases. The infinite-mean Levy face is strictly dominated when mean is free.

The [canonical dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pig/README.md) contains the proofs, score normalization, initialization, stopping rules and arithmetic margins. The [expanded concavity proof](2026-11-06-pig-cert-data/concavity-proof.md) and [symbolic identity check](2026-11-06-pig-cert-data/check-proof.py) retain the derivation. The [initial rejected routes](2026-11-06-pig-cert-data/shape-analysis.md) explain why the weaker variance bound, a denser mesh and polynomial root isolation were insufficient or unnecessary. The [curvature scan](2026-11-06-pig-cert-data/curvature.txt) is numerical evidence, not the proof.

## Implementation and numerical contract

The finite log-parameter mesh and local peak refinements have been replaced by bracket expansion and a signed root enclosure. A positive ratio recurrence streams both $E_k$ and $zE_k$, retaining the Poisson-side scaled limit without subtracting large logarithms. Aggregation uses compensated sums and 64-bit multiplicities. Poisson witnesses use the existing stable Poisson log-pmf helper, preserving the held mean when supplied.

The shape certificate encloses the unique root to log-width at most `max(reltol, 2e-7)`. A point inside that verified box is then polished toward the requested solver resolution. Raw polishing signs do not replace the global certificate, and exhausting the solver cap stops. This polishing was necessary for the stricter existing R golden tolerance: an initial certified midpoint missed it, although the independent optimum is within tolerance. [The original failure](2026-11-06-pig-cert-data/r-golden-before-polish.txt) is retained; neither fixture nor tolerance was relaxed.

The certificate is exact in arithmetic conditional on the computed comparisons, with the practical floating-point margins stated in the dossier. No outward-rounded intervals or formal machine-error proof are claimed. Uncertain boundary signs stop as ties; uncertain root signs require independently signed nearby points or stop numerically. A small residual or a last iterate never supplies the global certificate.

Solver work, including shape bracketing, bisection and polishing, is capped by `min(iterlim, 100000)`. Needed count recurrences are guarded by `100000 * min(iterlim, 100000)` cells before scanning the count range. Analytic Poisson decisions bypass that recurrence. A required interior mass evaluation at `INT_MAX` stops numerically; an analytic Poisson face can still be evaluated there. Moment evaluations take $O(\max x)$ time and constant additional storage; the final settled mass recurrence retains $O(\max x)$ storage.

`PigResult` gains an appended `StopDetail`. The pig-specific common binding transports the supplied face, candidates and bounds to both R and JavaScript without refitting or reconstructing a search. No cross-family shared header changed. The R adapter remains thin. C++ cases were appended; pig-specific JavaScript cases were appended to the existing stop suite.

One deliberate behavior correction replaces the old `1e-8*n` face policy: the sample with 10,002 zeros, 10,000 twos and one three now returns its proved finite maximum. Previously its small finite improvement was reported as Poisson. The existence decision now follows the analytic boundary score rather than a likelihood-gain tolerance.

## Numerical evidence and golden accuracy

Independent reference calculations use explicit finite polynomial coefficients at 90 or 180 decimal digits of working precision, with 220 bisections. Thus the arithmetic precision is not a claim of 180 accurate solution digits. Small-count likelihoods are also checked by the separate Bessel-K formula.

| Independent check | Cases | Largest measured error |
|---|---:|---|
| Held-shape mean profiles | 16 | Relative mean `2.90e-11`; absolute log likelihood `1.43e-14` |
| General free/held-mean shape profiles | 10 | Relative shape `3.45e-10`; absolute log likelihood `9.14e-13` |

The shape fixtures include counts around 1,000, a held mean of `1e100`, a small held mean, the near-Poisson sample above and the stored golden sample. The held-shape checks include extreme concentration, an exact one-count solution, insufficient work, unattainable requested precision and the integer endpoint guard. The core tests additionally exercise shapes from `1e-300` to `1e300`, every hold subset, equality boundaries and FMA residuals on opposite sides of a rounded binary-count seam. R and JavaScript check structured limit, tie, budget and numerical stops.

On the golden sample, the stored shape is `3.5119674554047671`; the final fit is `3.5119672758706271`, a relative difference of about `5.12e-8`, inside the existing R `1e-7` allowance and the C++ tolerance. Mean remains `1.978`; total log likelihood changes by approximately one final rounding unit. All golden regression checks pass without regenerating a golden.

References and results are kept in [the held-shape oracle summary](2026-11-06-pig-cert-data/oracle-check.json), [the shape oracle summary](2026-11-06-pig-cert-data/shape-oracle-check.json), their adjacent scripts/JSON/CSV files and the appended C++ fixtures. Reproduce the independent checks and timings from the worktree root with:

```sh
/tmp/umvl-cpu/run bash dev/research/evaluations/2026-11-06-pig-cert-data/reproduce.sh
```

## Timings

Each entry is microseconds per fit over 1,000 repeated calls on the unchanged 500-observation golden data, seed 12665, compiled with GCC 15.2.0, C++17 and `-O2`. The baseline header is reconstructed read-only from `bb734cd7`, with the same unchanged density headers. Runs used one workstation slot under nice 10. These are single batches on the shared workstation, not confidence intervals.

| Fit | Before | After |
|---|---:|---:|
| Both free | 157.20 | 19.09 |
| Mean held at 3 | 126.32 | 10.99 |
| Shape held at 3 | 134.45 | 22.99 |
| Shape held at `1e-20` | 134.92 | 15.37 |
| Shape held at `1e20` | 95.53 | 17.24 |
| Both held at 3 | 1.92 | 3.73 |

The default fit was about 8.2 times faster in this batch. Direct evaluation pays additional aggregation overhead and was slower. The tiny-held-shape case also illustrates the numerical change: the old flat-likelihood search returned mean `0.00215695`, while the certified mean root is approximately `1.592`, at indistinguishable displayed likelihood. The normalized score resolves the coefficient even when direct likelihood comparisons cannot.

Raw [before](2026-11-06-pig-cert-data/timing-golden-before.csv) and [after](2026-11-06-pig-cert-data/timing-golden-after.csv) timings retain coefficients and likelihoods. Adjacent timing files retain the additional nine-observation workload.

## Final validation

| Command | Machine | Final result |
|---|---|---|
| `just cpp-test` | simbox | 106/106 shards passed; 45.71 s test time |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 128/128 passed; 59.91 s |
| `just cpp-test-no-long-double` | simbox | 128/128 passed; 48.43 s |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20,912; 60.385 s elapsed |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43,732; 138.039 s elapsed |
| Full pig R file | workstation and simbox | 179 passes, no warnings or skips |
| `just js-build` and `just js-test` | workstation, one pinned CPU | Build passed; 434/434 tests passed, no skips |
| `just site-check` | workstation, one pinned CPU | Passed, including browser, native and MathML checks |
| `just relations` | workstation, one pinned CPU | 157 edges, zero failures |
| Independent arithmetic and oracle reproducer | workstation | Passed |

Final C++ runs include the added R-tolerance regression and both oracle shards. The earlier R failure was corrected before these final R runs. The 154 development-tier skips are the declared full-tier tests; the final full R run has no skips. Web builds emit the existing `binom/mle.hpp:158` tautological-comparison warning in unchanged code; all build and test commands exit zero.

Workstation checks used Linux x86_64, GCC 15.2.0, R 4.6.1, BH 1.90.0-1, Python 3.14.5, mpmath 1.3.0, SymPy 1.14.0, Node 22.22.1 and Emscripten 6.0.13. Simbox used WSL2 x86_64, GCC 13.3.0 and R 4.6.1. Native CMake/ctest jobs used three simbox workers; R used four testthat workers with BLAS/OpenMP pinned to one thread. All computations used the required slot wrappers and nice 10. No simbox-drop fallback was needed.

The kept [C++ summary](2026-11-06-pig-cert-data/final-cpp.txt), [R summary](2026-11-06-pig-cert-data/final-r.txt), [web summary](2026-11-06-pig-cert-data/final-web.txt), environment records and manifest identify the tested source. The site checker regenerated a historical screenshot as a side effect; its original bytes were restored. Changes are confined to pig and its binding/tests, plus this report and its evidence directory. Density, special-function, truth, code-generation and golden sources are unchanged.

No open certification case remains for the supported finite-parameter interface. Arithmetic ambiguity, representability limits and exhausted work are deliberate terminal stops under the stated conditional floating-point contract.
