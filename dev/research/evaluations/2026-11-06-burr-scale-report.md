# TASK-116.4: Burr with scale held and both shapes free

Completed on 2026-11-06 in `codex/burr-scale`, based on `164d41784756606a48ddc0cedc67752acab4050b`. No commits were made. The held-scale Burr fit now certifies its unique finite maximum or stops with structured boundary, unboundedness, comparison-uncertainty, budget or numerical diagnostics. Inverse Burr, Pareto IV and the GB2/Feller–Pareto held unit-shape reductions inherit this behavior.

`fit.certified` is now `yes` for Burr, inverse Burr and Pareto IV. GB2 and Feller–Pareto remain `partial`: their unit-shape reductions are certified, while their other numerical profiles remain outside this task. Default all-free algorithms and all golden files and tolerances are unchanged.

The mathematical result and algorithm are documented in the [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md#held-scale-certificate-task-1164), with reciprocal and threshold transport documented in the other four dossiers. The implementation is [scale_certificate.hpp](../../../univariateML-cpp/include/cppunivariate/distributions/burr/scale_certificate.hpp).

For held scale $s$, put $z_i=\log(x_i/s)$, $G(c)=\sum_i\log(1+e^{cz_i})$ and $R(c)=\sum_i\log(1+e^{-cz_i})$. Profiling $k=n/G(c)$ gives

$$\ell_p(c)=n\log n-n-\sum_i\log x_i-n\log(G(c)/c)-R(c).$$

The dossier proves that $G(c)/c$ is strictly log-convex and hence that $\ell_p$ is strictly concave. The key inequality, for $A(u)=\log(1+e^{-u})$ and $u>0$, is $(\log(A(u)/u))''\ge u^{-2}-e^{-u}>0$. Positive log arguments follow by adding the constant one, and sums preserve log-convexity. Coercivity at both ends proves existence and uniqueness for nonconstant samples containing an observation below $s$.

Constant samples at or below $s$ have unbounded likelihood. When every observation is at least $s$ and some exceeds it, the supremum is on the endpoint-$s$ Pareto path. With $S=\sum z_i$ and $Z=\#\{x_i=s\}$, its value is $n\log(n/S)-n-\sum\log x_i-Z\log2$. Stop details now retain this endpoint penalty. The endpoint stays held through reciprocal and threshold mappings. A proper Weibull face cannot compete with scale held; the profiled endpoint analysis exhausts this subset.

The solver normalizes logs by their maximum absolute value, proposes a root by doubling and bisection, and accepts only after practical bounds give strictly positive and negative endpoint scores. Strict concavity then certifies the unique global root. Both coefficient images must have relative width at most $10^{-6}$. Concave tangents bound the maximum likelihood from above; a finite witness bounds it below. Budget exhaustion returns `CertificateBudget`, unresolved signs or coefficient accuracy return `CertificateTie`, and invalid numerical bounds or nonfinite arithmetic return `NumericalFailure`. Free coefficients are not returned on these stops. Finite witnesses remain available in budget and tie diagnostics.

The arithmetic contract is certification in exact arithmetic conditional on the computed comparisons. It uses the existing Burr practical allowance of $64\epsilon(1+\max|a|)$ for elementary operations and transcendental values, and $64\epsilon(m+1)(1+\sum|a_i|)$ for an $m$-term sum. This is the package's practical floating-point margin. Extreme or ill-conditioned inputs can therefore stop without an estimate. Reciprocal collisions with held scale and inexact threshold subtraction onto held scale stop numerically; subtraction uses an error-free TwoSum residual. These checks prevent a rounded equality from changing a boundary decision or its endpoint penalty.

The initial fully refined root differed from the historical held-scale Burr coefficients by $5.63\times10^{-10}$ in $k$ and $2.28\times10^{-9}$ in $c$, exceeding the existing absolute regression tolerance of $10^{-10}$. The historical Newton result is therefore retained solely as a compatibility proposal when it lies within $10^{-8}$ relatively of the refined root. It must pass the independent signed bracket and coefficient-image checks. Failed or distant proposals are discarded. This preserves the goldens while adding a global certificate. The original failure evidence is retained in [initial-findings.txt](2026-11-06-burr-scale-data/initial-findings.txt).

An independent 60-digit mpmath calculation bisected the profiled score 150 times on six finite workloads, including the 500-observation historical fixture, data entirely below scale, mixed data, near-scale observations and values spanning $10^{-100}$ to $10^{100}$. The largest coefficient discrepancy from the high-precision optimum was $7.01\times10^{-9}$ relatively, and the largest absolute log-likelihood discrepancy was $9.08\times10^{-14}$. Every oracle optimum lay inside the reported likelihood bounds. These are numerical accuracy checks; globality follows from the concavity proof and signed comparisons. Inputs, results and reproduction commands are retained in [probe.json](2026-11-06-burr-scale-data/probe.json), [checks.json](2026-11-06-burr-scale-data/checks.json), [probe.cpp](2026-11-06-burr-scale-data/probe.cpp) and [check.py](2026-11-06-burr-scale-data/check.py). The standalone probe needs the Boost include directory, supplied through `BOOST_INCLUDE_DIR` in its documented build command; the workstation used the installed BH headers.

Single-call timings from the optimized standalone probe were 6.24 ms for the 500-observation historical fixture and 0.057–0.123 ms for the smaller finite workloads. These include the compatibility proposal and certification, under a one-slot reservation and `nice -n 10`; they are illustrative timings rather than repeated benchmark estimates.

All final runs below used the workstation, GCC 15.2.0 and R 4.6.1 where applicable, through one `/tmp/umvl-cpu/run4` reservation with BLAS/OpenMP pinned to one thread. Durations include build/generation work and exclude the preceding slot wait. Both native full tiers ran after development, JavaScript and site validation.

| Command | Result | Wall time |
|---|---|---:|
| `just cpp-test` | 106/106 dev shards passed | 120.0 s |
| `just cpp-test-no-long-double` | 127/127 tests passed | 180.5 s |
| `just test` | FAIL 0, WARN 0, SKIP 154, PASS 20689 | 290.0 s |
| `just js-build` | passed | 68.3 s |
| `just js-test` | 421/421 passed, no skips | 116.1 s |
| `just site-check` | passed; 99 family pages and C++ snippets, MathML and links checked | 136.0 s |
| `UMVL_TEST_TIER=full just cpp-test` | 127/127 full shards passed | 114.3 s |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43504 | 326.0 s |

The final tests cover historical goldens, independent held-subset likelihood comparisons, reciprocal and translated delegation, finite likelihood bounds and scores, endpoint penalties, constant samples, overflowing ratios, numerical refusals, budget witnesses and the sign-uncertain held-scale tie case `exp(c(-1.1,-1))`. Development skips are recorded separately from passes; the R full tier had no skips. `git diff --check` passed.

A preliminary R development run on simbox failed an unrelated `mlst` scale-equivariance assertion near df 1272, as well as five mistakes in newly added assertions comparing model attributes or incorrect Pareto coefficient names. The new assertions were corrected. The existing `mlst` assertion passed unchanged in both final workstation tiers. The simbox run is preliminary failure evidence, not a final passing run; its diagnostic is retained in `initial-findings.txt`.

[run_checks.py](2026-11-06-burr-scale-data/run_checks.py), [runs.jsonl](2026-11-06-burr-scale-data/runs.jsonl) and [final-summaries.txt](2026-11-06-burr-scale-data/final-summaries.txt) preserve commands, machines, timings and results. [source-manifest.json](2026-11-06-burr-scale-data/source-manifest.json) records the base revision and source hashes, including the sequence-start snapshot and the final tie-test additions. Fitter sources were unchanged throughout the final sequence; the final tie assertions passed C++ no-long-double/native full and R dev/full. No implementation or validation item remains open. The JavaScript browser checks regenerated an older screenshot; that unrelated generated change was restored to its original bytes after validation.

Open delivery issue: the required artifact-pane `show` command returned HTTP 403 because this worktree is outside the viewer server's readable roots. The report remains at the requested path. Display requires the viewer configuration to include `/home/jonas/Files/programming/univariateML.wt/burr-scale`; no server configuration or other checkout was changed.
