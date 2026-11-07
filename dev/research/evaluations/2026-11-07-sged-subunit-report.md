# SGED asymmetric subunit scale certificate — TASK-116.31

Completed J9a: scale alone free, with public `mean`, positive `nu < 1` and finite positive `xi != 1` held. The C++ entry point now reaches a global certificate through `sged/certificate.hpp` and the new `subunit_certificate.hpp`. The existing binding-neutral `fit_sged` uses this entry point for both R and JS; both public bindings have regression cases for the new fits and structured stops.

Tested source is base `7c52f905` on `codex/sged-subunit`, plus the uncommitted files identified in the [source manifest](2026-11-07-sged-subunit-data/source-manifest.sha256). Those hashes passed on simbox before its final runs and on the workstation after testing. No commits were made. Default fitting code and frozen fixtures were retained; the golden regressions pass at their existing tolerances, including the R SGED tolerance `3e-5`.

The family-wide `fit.certified` remains `partial`. Its note now includes every positive held-shape scale-only fit. Subsets with a free shape or skew still use their existing numerical searches and are not globally certified. The obsolete scale-only numerical-tail-guard limit declaration was removed.

The canonical proof, arithmetic margins and computational costs are in the [held-shape certificate section of the dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sged/README.md#held-shape-certificate). In brief, let `u=max(abs(x-mean))`, `d=mu_u`, `r=u*sigma_u/(abs(d)*sd)`, and `b_i=sign(d)*(x_i-mean)/u`. With fixed positive side weights, the cost is

```
F(r) = -n log(r) + P(r),  r > 0,
P(r) = sum k * xi^(-nu*sign(d*(1+b_i*r))) * abs(1+b_i*r)^nu,
k = abs(d/lambda)^nu / 2.
```

If `u=0`, scale collapse is unbounded and stops as `nomle`. Otherwise, `F` diverges at both ends. An incumbent `B` excludes `log(r)<-B/n-1`, since `P>=0`. For `r>=2`, a term with `abs(b_i)=1` gives the lower envelope `C*r^nu-n*log(r)`, where `C=k*exp(-abs(nu*log(xi)))/2^nu>0`. That envelope increases after `C*nu*r^nu>n` and eventually exceeds `B`. These inequalities close both tails without a fixed scale cutoff and prove existence of a finite maximum.

Known positive knots `r=-1/b_i` partition the remaining compact domain. On each sign cell, `P` is concave and `P'` decreases. The chord of `P`, plus `-n*log(r)`, supplies an analytically minimized lower bound. Bounds `P'(h)-n/l` and `P'(l)-n/h` can also exclude a monotone cell after its endpoints have been evaluated. Remaining cells are bisected. Thus every global minimizer is retained, rather than inferred from a scan or a small score. Terminal cells have log width at most `1e-10`; acceptance requires all surviving winners to fit within log-scale width `1e-7` and public scale width `1e-7*(1+abs(sd))`.

Uniqueness fails even for a constant sample away from the held mean. Take repeated observations `-1`, held mean zero and `nu=1/2`. Let `R` be the root in `(4,6)` of `log(R)=2*(1-1/R)`, set `K=2*sqrt(R-1)/R` and `xi=sqrt(1+K^2/6)`. Numerically, `R=4.921553634567505` and `xi=1.0525849322732358`. Beyond the cusp, cost relative to `r=1` is `n*(-log(r)+K*sqrt(r-1))`. Its score equation is quadratic in `sqrt(r-1)`: the first stationary point is a maximum and the second is a minimum at `R`, tied with the cusp. Cost decreases towards the cusp on `(0,1)`. Hence these are two separated global scale maxima. C++, R and WASM retain competing witnesses and stop as `tie`.

The certificate is exact-arithmetic conditional on computed comparisons, with practical floating-point margins, not outward-rounded interval arithmetic. Cost comparisons use `4096*epsilon*(1+n+abs(F))*(1+abs(nu*log(xi))+1/nu)`; score comparisons use the corresponding conditioning factor and absolute term sum. Algebraic knot residuals are zeroed. The stored public coefficients must reproduce the ideal likelihood within `1e-7*(n+abs(logLik))`.

A translated `nu=.3, xi=.3` development fixture initially failed that last check. Recovery now checks three equivalent scale expressions at a cusp and their adjacent representable values, retaining the original margin. A non-cusp initial witness at `r=1/2` is available for diagnostic stops: every factor `1+b_i*r` then has magnitude at least `1/2`. Nonfinite arithmetic or lost coordinate resolution stops numerically. Exhaustion stops as `budget`, with finite witnesses when representable; separated unresolved winners stop as `tie`. Neither stop returns a successful fit.

Tail, point and box work shares `128*min(iterlim,100000)` certificate evaluations, including reserved public-coordinate checks. A partition whose endpoint evaluations cannot fit the remaining budget is refused before evaluating all knots. The 100,000-point regression case stops with a finite budget witness in fewer than 32 evaluations; its isolated workstation run took **0.05 seconds** ([record](2026-11-07-sged-subunit-data/large-budget-timing.txt)). A funded partition still costs `O(n*k)` for `k` distinct knots, followed by adaptive cell work. Large funded partitions can therefore be expensive; the budget stop is part of the supported result, not a claim of fast completion for every sample.

Independent numerical checks use gamma-moment formulas, SciPy score bracketing with all cusp candidates, and a 70-digit mpmath calculation of the analytic tie. The deterministic nine-fit workload is `x=(-3,-1,-.2,0,.3,.4,2)`, held mean `.1`, `nu` in `(.3,.5,.9)` and `xi` in `(.3,1.3,3)`. On both compilers, maximum relative scale disagreement was **9.91e-9**, and maximum total log-likelihood disagreement was **2.71e-8**, against limits `2e-7` and `2e-6`. These finite numerical comparisons supplement the proof; they do not establish globality themselves. See the [oracle code](2026-11-07-sged-subunit-data/oracle.py), [workstation results](2026-11-07-sged-subunit-data/oracle.json) and [simbox results](2026-11-07-sged-subunit-data/oracle-simbox.json).

The paired benchmark compiles baseline SGED headers from `7c52f905` and the revised headers with `-O2 -std=c++17`, using the existing BH headers. It runs twenty repetitions per fixture, `gradtol=1e-8`, `iterlim=200`, one thread and `nice -n 10`; no RNG is involved. Final timings are microseconds per fit. Speedup is the median of nine paired ratios, not the ratio of the two median times.

| Machine | Before median | After median | Paired median speedup | Range of paired speedups |
|---|---:|---:|---:|---:|
| Workstation, GCC 15 | 182.14 | 17.75 | 11.8x | 2.49–17.72x |
| simbox, GCC 13 | 159.14 | 6.74 | 23.9x | 5.47–29.60x |

These are small-workload measurements on shared machines, not general scaling guarantees. Raw timings, immutable baseline header snapshots, the [benchmark script](2026-11-07-sged-subunit-data/benchmark.sh) and [metrics](2026-11-07-sged-subunit-data/metrics.json) are retained. Conditional coefficients differed from the old scan by at most `1.02e-8` relatively, while cusp likelihoods improved by up to `0.001410`: tiny scale displacement can cause appreciable subunit cusp loss. Default golden fixtures were not regenerated.

All final builds and test runs completed with exit zero:

| Check | Machine | Result |
|---|---|---|
| `just cpp-test` — dev | simbox, GCC 13 | 106/106 shards passed |
| `UMVL_TEST_TIER=full just cpp-test` | simbox, GCC 13 | 129/129 passed |
| `just cpp-test-no-long-double` | simbox, GCC 13 | 129/129 passed |
| `just cpp-test-no-long-double` | Workstation, GCC 15 | 129/129 passed |
| Targeted R `sged-subunit` file | simbox, final source | 38 expectations passed; zero warnings/skips |
| `just test` — dev | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21279 |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 44097 |
| `just js-build`; `just js-test` | Workstation | Build passed; 471 tests passed, zero failures/skips |
| `just site-check` | Workstation | Passed, including MathML, browser checks and 99 C++17 snippets |

The 154 R dev skips were explicitly full-tier gates; the full run had none. No permitted failures were used. Compiler warnings from unchanged headers remain visible in the archived logs. Initial harness compilation needed the existing BH include path and C++17 aggregate span construction; those corrections did not install dependencies or change library tolerances.

The remote sequence is retained in [final-simbox.sh](2026-11-07-sged-subunit-data/final-simbox.sh). It was launched through `/tmp/umvl-cpu/simbox-run`, with one CPU of affinity while the workstation build used three workers. After all workstation computations finished, affinity for the remaining R process tree was expanded to four CPUs. No simbox drop or local fallback occurred. On the workstation, the final sequence was:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
/tmp/umvl-cpu/run4 bash -c '
  set -e
  export CMAKE_BUILD_PARALLEL_LEVEL=3 CTEST_PARALLEL_LEVEL=3 EMCC_CORES=3
  /usr/bin/time -p just cpp-test-no-long-double
  /usr/bin/time -p just js-build
  /usr/bin/time -p just js-test
  /usr/bin/time -p just site-check
'
```

An earlier workstation development run also passed all 38 new R expectations, before the final large-partition preflight was added. Final GCC 15 coverage is supplied by the complete no-long-double suite and the WASM tests. Targeted R checks used `testthat::test_local` with `filter="^sged-subunit$"`, `UMVL_TEST_TIER=dev` and one worker after `just regen`. Benchmarks and the isolated large-sample check used `/tmp/umvl-cpu/run`. All temporary files were under `build/`; unnecessary native object files and the temporary baseline tree were removed. A browser-check screenshot generated outside this job's evidence directory was restored to its original bytes.

The workstation used Ubuntu 26.04.1, Linux 7.0.0-42, GCC 15.2.0, Node 22.22.1 and Emscripten 6.0.13. Simbox used WSL2 Linux 6.18.54.1 and GCC 13.3.0. Both used R 4.6.1, BH 1.90.0.1 and Rcpp 1.1.2. The workstation oracle used Python 3.14.5 / SciPy 1.18.0 / mpmath 1.3.0 / NumPy 2.4.6; simbox used Python 3.12.3 / SciPy 1.11.4 / mpmath 1.2.1 / NumPy 1.26.4. See the [environment record](2026-11-07-sged-subunit-data/environment-local.txt) and compressed complete [workstation log](2026-11-07-sged-subunit-data/final-local.log.gz) and [simbox log](2026-11-07-sged-subunit-data/final-simbox.log.gz).

No J9a mathematical case remains open under the package's certificate convention. Outside this job, free-shape/free-skew profiles and their moment-constrained limiting-face comparisons still lack a global certificate; the family-wide flag correctly remains `partial`. This work supplies no uniform floating-point error proof or new guarantee for those existing numerical searches.

The finished report was submitted to `bin/show`. The viewer returned HTTP 403 because this worktree is outside its configured readable roots. Pane delivery requires registering this worktree or adding it to `CODEX_MATH_WEB_VIEW_ROOTS`; no viewer configuration or other checkout was changed.
