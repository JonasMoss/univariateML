# TASK-116.3 — nbinom and Kumaraswamy profile certificates

Both fitters now certify a global maximizer through a signed root enclosure, or stop without returning an unchecked estimate. Both dossiers declare `fit.certified: 'yes'` under the package's conditional exact-arithmetic contract. Kumaraswamy joint uniqueness is proved below and in its canonical dossier. Goldens were not modified; all final required tests and both full tiers passed. No commits, board edits, package installations, other-family edits or simbox runs were made.

The tested base is `fe4573e9b02a950a1440039a8395c30ff2f25926`, branch `codex/scalar-cert`, with the uncommitted changes identified by [source-sha256.txt](2026-11-05-scalar-cert-data/source-sha256.txt). The manifest's SHA-256 is `d29ad03f0accaf16ecd22190bc197e6a8a2c8d94e5e378d357c106e2b9254518`. It covers both families' fitters, certificate headers, dossiers, C++/R tests, common binding adapters and the three numerical-check sources. [validation.json](2026-11-05-scalar-cert-data/validation.json) records the environment, commands, counts, timings and numerical errors.

For nbinom, the existing existence/uniqueness theorem supplies the global argument: with positive mean and both coefficients free, overdispersion gives one finite maximum; otherwise the supremum is Poisson. Holding probability below one gives a strictly decreasing size score. The new [certificate](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/certificate.hpp) requires padded positive and negative endpoint scores and relative size width at most `5e-7`. The profiled probability is increasing in size with relative sensitivity at most one. Newton and its old fallback only nominate a point. Historical coefficients survive only inside the certified enclosure.

The score uses integer harmonic recurrences, a bounded digamma expansion for large counts, and compensated sums. Near the Poisson face, it cancels the leading mean algebraically and evaluates `log1p(u)-u` by a degree-20 series at `u < .01`. This avoids subtracting two large, nearly equal score terms. The expansion remainder uses the positive-real digamma result in [NIST DLMF 5.11(ii)](https://dlmf.nist.gov/5.11.ii): the two remainders have the same sign, so their difference is bounded by the larger first-neglected-term bound. After multiplying by size, that bound is `691/(32760*r^11)`. The [dossier](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md) gives the formulas, comparison margins and dispersion guard.

The Poisson stop now carries its fitted mean and log likelihood in `core/stop_detail.hpp`'s existing `StopDetail`. R exposes `univariateML_limit`, `model = "pois"`, named `estimates = c(lambda = mean(x))`, and `limit_loglik`. Numerical uncertainty never diagnoses a Poisson escape. All-zero data retain the attained size-zero representative and its documented lack of identification.

For Kumaraswamy, put $t_i=-\log x_i$, $T=\sum_i t_i$, and $G(a)=\sum_i-\log(1-e^{-at_i})$. Profiling gives $b=n/G(a)$ and

$$h(a)=n\log a-n\log G(a)-aT+G(a)+C.$$

The positive expansion $G(a)=\sum_i\sum_{k\ge1}e^{-kat_i}/k$ makes $G$ a Laplace transform of a positive measure. Cauchy–Schwarz gives $(\log G)''\ge0$. Also

$$G''(a)=\sum_i\frac{t_i^2e^{at_i}}{(e^{at_i}-1)^2}<n/a^2,$$

because $2\sinh(u/2)>u$ for $u>0$. Consequently

$$h''(a)=-n/a^2-n(\log G)''(a)+G''(a)<0.$$

Combined with the already established coercivity for nonconstant interior samples, this proves a unique finite joint maximum. This is an analytic proof, not a conclusion from the seeded searches. Constant joint samples remain unbounded; either held shape gives its previously proved unique conditional fit. See the [canonical proof and implementation contract](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md).

The Kumar [certificate](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/certificate.hpp) brackets the unique zero of `a*h'(a)/n`. Its relative `a` width is at most `5e-7`, tightened according to `a*T/n`; the endpoint `b` values must have log ratio below `1e-6`. Since `b(a)` increases, these bound both coordinates. A `1e-12` relative rounding pad permits the historical `b` computation inside that enclosure. Survival terms use stable logarithms and a scaled derivative formula at large arguments; sums precede division by sample size.

Both certificates are exact in arithmetic conditional on the computed comparisons, with stated practical floating-point allowances. Nbinom adds a `128*epsilon` term-scale allowance and its analytic expansion remainder; Kumar uses `256*epsilon` times its normalized absolute score terms, with summation allowances. These are practical margins, not outward-rounded intervals or universal libm error proofs. Nonfinite arithmetic, unresolved signs, unrepresentable results and exhausted budgets stop. The certificate budget is `16 * min(maxit, 100000)` evaluations; `reltol` controls the proposer. Core failure details distinguish arithmetic from budget exhaustion; the common bindings preserve budget stops as `univariateML_budget`. The Kumar binding also rejects every non-`Ok` status. No failed last iterate is returned.

Three implementation decisions preserve material evidence. An initial Kumar root width of `5e-9` moved the historical golden by approximately `1.02e-8` in `a` and `1.90e-8` in `b`. The final, still signed, accuracy enclosure preserves the existing golden without relaxing its tests. A fixed wider enclosure initially stopped one sensitive-`b` oracle case; sensitivity-dependent refinement resolved it. Recomputing nbinom likelihood through the generic density path lost about `0.0076` on the held-probability regression, so the fitter reuses its existing stable fixed-coefficient likelihood evaluation. [Initial C++ failures](2026-11-05-scalar-cert-data/initial-regressions.txt) and [the initial oracle run](2026-11-05-scalar-cert-data/oracle-initial.json) retain those observations. The first broad R dev run also caught a bad newly added expectation that a fully held subnormal model should error; the corrected test requires finite likelihood evaluation and preservation of the held size. [That initial failure](2026-11-05-scalar-cert-data/r-initial-failure.txt) is retained; no existing test or tolerance was weakened.

All final runs used the local x86_64 workstation, GCC 15.2.0, R 4.6.1, Rcpp 1.1.2 and BH 1.90.0.1. Heavy commands ran under `/tmp/umvl-cpu/run4`; independent checks and timings used `run`. The launchers enforce nice scheduling and one BLAS/OpenMP thread per worker. Temporary files stayed in `build/tmp`. Times below include each command's build/regeneration work and exclude reservation waits.

| Final command | Result | Elapsed |
|---|---|---:|
| `just cpp-test` | 106/106 shards | 109.56 s |
| `just cpp-test-no-long-double` | 127/127 shards | 177.06 s |
| Targeted R dev, `dist-(nbinom\|kumar)` | 132 pass, 0 fail/warn, 2 tier skips | 22.10 s |
| `just test` | 20,628 pass, 0 fail/warn, 154 tier skips | 188.78 s |
| `UMVL_TEST_TIER=full just cpp-test` | 127/127 shards | 113.73 s |
| `just test-full` | 43,443 pass, 0 fail/warn, 0 skips | 335.59 s |

The no-long-double command uses `BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS`. The normal C++ and R full tiers ran after the dev checks. There were no permitted failures in the final runs. The dev skips are full-tier gates. Existing golden tests passed, including Kumar's absolute `1e-12` preservation check. Added cases cover Poisson stop details, exhausted budgets, large-count recurrence comparison, near-Poisson cancellation, start independence and R diagnostics.

The independent [checker](2026-11-05-scalar-cert-data/check.py) and [C++ probe](2026-11-05-scalar-cert-data/probe.cpp) use mpmath 1.3.0 at 70 digits, Python 3.14.5, seed 1163, fitter tolerance `1e-10` and `maxit = 200`. They checked 54 samples: both 500-observation goldens, 24 seeded 20-observation samples per family, large-count and sparse-count nbinom cases, a near-Poisson sample and a clipped-endpoint Kumar sample. The final [oracle results](2026-11-05-scalar-cert-data/oracle.json) contain 51 interior fits and three nbinom attained-zero/Poisson cases, with no numerical stops.

| Family | Maximum relative first-coefficient error | Maximum relative second-coefficient error | Maximum absolute log-likelihood error per observation |
|---|---:|---:|---:|
| nbinom | `2.03e-7` | `1.22e-10` | `5.78e-11` |
| Kumar | `2.39e-7` | `4.06e-8` | `1.13e-13` |

Acceptance thresholds were `1e-6` relative coefficient error and `1e-7` absolute log-likelihood error per observation. This is independent numerical evidence on the stated workloads, separate from the uniqueness proofs and golden regression evidence.

For reproduction from the repository root, use the usual temporary-directory environment and one CPU reservation:

```sh
export TMPDIR="$PWD/build/tmp" JUST_TEMPDIR="$PWD/build/tmp" XDG_RUNTIME_DIR="$PWD/build/tmp"
mkdir -p "$TMPDIR" build/scratch
export CCACHE_DIR="${UMVL_CCACHE_DIR:-$HOME/.cache/ccache/univariateML}"
export CCACHE_BASEDIR="$PWD" CCACHE_NOHASHDIR=1
/tmp/umvl-cpu/run bash -c '
  set -euo pipefail
  scalar_bh=$(Rscript -e '\''cat(system.file("include", package="BH"))'\'')
  ccache c++ -std=c++17 -O2 -IunivariateML-cpp/include -I"$scalar_bh" \
    dev/research/evaluations/2026-11-05-scalar-cert-data/probe.cpp \
    -o build/scratch/scalar-probe
  python3 dev/research/evaluations/2026-11-05-scalar-cert-data/check.py \
    build/scratch/scalar-probe \
    dev/research/evaluations/2026-11-05-scalar-cert-data/oracle.json
'
```

[Timing source](2026-11-05-scalar-cert-data/timing.cpp) and [measurements](2026-11-05-scalar-cert-data/timing.txt) compare 100 sequential default-control fits on each golden sample, using C++17 `-O2` and one CPU slot. The certificate costs about 10 times the proposer on these small workloads; absolute times remain below one millisecond. These are single-workstation batches, not a broad performance study.

| Family, n = 500 | Historical proposer | Certified fitter | Ratio |
|---|---:|---:|---:|
| nbinom | 29.11 us/fit | 293.00 us/fit | 10.1x |
| Kumar | 47.31 us/fit | 506.99 us/fit | 10.7x |

Repeated score evaluations explain the added cost. Nbinom additionally reuses the stable rising-factorial likelihood route for counts below its existing threshold; this can cost more on large counts. Such workloads were not systematically benchmarked here.

Transfer is limited to actual parent calls. `zinb` obtains a certified ordinary-NB candidate, but its outer mixture/profile search is not thereby certified. `bnbinom` obtains a certified free NB face on `Ok`; its held-size face remains analytic. At this revision, `ztnbinom` has its own truncated profile, `zmnbinom` delegates to it, and `bbinom` has separate analytic face calculations: none inherits this free-size root certificate automatically. Their code and certification tags were not changed.

Two concrete items remain open outside the permitted file scope:

- `bnbinom/mle.hpp:136–145` does not explicitly propagate an NB `PrecisionFailure`: it can omit the unresolved NB face and continue with the Poisson comparison. Certifying that caller requires failure/stop-detail transport or an independent bound for the omitted face. A successful certified parent call alone does not close this gap.
- The generic C++ NB density seam needs a separate extreme-parameter check/fix. For `x = c(0,1,5)`, held `p = 1-1e-12` and the fitted size near `2e12`, summing `special::dnbinom(..., true)` gave about `-6.63625`, against about `-6.62861` from the stable fitter path and its Poisson-limit regression. `nbinom/dpqr.hpp` normally delegates this case to that seam. An independent extreme-parameter density fixture and a protected-seam fix remain to be made; `special/`, `dpqr.hpp`, truth files and generated d/p/q/r were left untouched as instructed. This does not affect the fitted likelihood path implemented here.

