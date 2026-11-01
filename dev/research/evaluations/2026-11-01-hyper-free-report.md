# Fully free hypergeometric search

2026-11-01. Tested source: `65da739f` with the uncommitted changes in this worktree. Goldens and generated d/p/q/r implementations were not changed.

## Result and search

The fully free fit now uses the exact-arithmetic tail inequalities below to certify a finite maximum when it finds a finite candidate strictly above all binomial and Poisson faces. It explores 16 successive smaller sizes starting at the sample maximum and 128 feasible populations for each, profiling the other size with the existing unimodal search. Constant data have an attained likelihood of one. The remaining cases either pass the face and tail comparisons or stop with a named unresolved face, a numerical face tie, or numerical failure.

For a candidate above the Poisson face, the implementation chooses bounds `S,N0` by splitting the candidate/Poisson gap between the two envelope corrections. It compares each binomial face of size below `S`, bounds its population using `-c_N(s) <= s(s-1)/(2(N-s+1))`, and visits every population below that cutoff. It also enumerates feasible sizes at least `S` below `N0`. The one-coordinate likelihood profile supplies the maximum at each visited pair. A preflight counts the required populations; the runtime budget counts profile search and face evaluations, including exploration, with a limit of 10^7. No partial maximum is returned on exhaustion. Additional guards limit `X` and `S` to 100,000, enforce count representation, and reject nonfinite results. These bounds are conservative and can make an otherwise valid fit stop on the work limit.

The comparisons use `1e-9*(1+abs(face))`, as in the held-size procedure. This is a practical floating-point margin, not an outward-rounded error enclosure; profile signs, face evaluation, and cutoff arithmetic are likewise numerical. The exact-arithmetic argument proves the tail exclusion conditional on those comparisons. A face stop does not prove that the face is the global supremum, and a tie is only a numerical match. Validated rounding, broader adversarial near-face coverage, and efficiency improvements remain open. The fully free R and JavaScript conformance checks accept the documented numerical-failure or unresolved-face diagnostics for this subset; goldens are unchanged.

## Faces

Write `h` for sample size, `X=max(x)`, `T=sum(x)`, and `lambda=mk/N`. The hypergeometric law is symmetric in `m,k`. Along `N -> infinity`, a bounded smaller size has a subsequence equal to a fixed integer `s` and `max(m,k)/N -> p`, giving `Binomial(s,p)`, including its point-mass endpoints. If both sizes diverge and `lambda -> lambda0 < infinity`, factorial moments converge to those of `Poisson(lambda0)`; zero gives the point mass at zero. If `lambda -> infinity`, `Var(X) <= lambda` and Chebyshev's inequality implies escape to infinity, so no tight probability limit on the nonnegative integers is missed. Point masses at every nonnegative integer are already attained at finite parameters. A constant sample therefore has maximum log likelihood zero; below, assume a nonconstant sample, hence `X,T > 0`.

## Uniform Poisson envelope

For a supported observation `x < N`, let `(a)_b` denote a falling factorial. The pmf factors as

```
P(x) = (m)_x (k)_x / (x! (N)_x)
       * (N-m)_(k-x) / (N-x)_(k-x).
```

The first fraction is at most `lambda^x / (x! (1-x/N)^x)`. In the second, each factor is `1 - (m-x)/(N-x-j)` and is at most `exp(-(m-x)/N)`; the conclusion also holds when a factor vanishes. Thus

```
log P(x) <= x log(lambda) - log(x!)
            - lambda (1-x/m)(1-x/k) - x log(1-x/N).
```

Let `Q` be the maximized Poisson sample log likelihood, attained at `lambda=T/h`. If `m,k >= S > X` and `N >= N0 > X`, put `a=(1-X/S)^2`. Summing and maximizing the right-hand side over all positive `lambda` gives

```
log L(N,m,k) <= Q + T[-2 log(1-X/S) - log(1-X/N0)].
```

This is uniform over both sizes and tends to `Q` as `S,N0` grow. It is a bound in exact arithmetic, not a directed-rounding floating-point certificate. The separate fixed-size bound in the [hyper dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hyper/README.md) covers `s=min(m,k)<S`: `log L <= B_s - h c_N(s)`, where `B_s` is the maximized binomial log likelihood at size `s`.

## Comparison of all binomial sizes

For `s>X`, the binomial pmf is at most `lambda^x exp(-lambda(1-x/s))/x!` with `lambda=sp`. Extending the maximization to every positive `lambda` yields

```
B_s <= Q - T log(1-X/s).
```

Thus, for any candidate `C>Q`, sufficiently large sizes have `B_s<C`, leaving finitely many sizes `s>=X` for direct comparison. If each of those faces is strictly below `C`, choose `S` and `N0` so the uniform Poisson envelope is below `C`; then increase `N0` until each of the finitely many fixed-size corrections also lies below `C`. This proves an eventual cutoff for **all** `(m,k)` at each larger `N`. It does not prove existence when a candidate meets or falls below a face, nor identify whether such a face is the global supremum. An exploratory search in that situation can only report unresolved or a numerical face tie.

## Reproducible check

`/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-01-hyper-free-data/check_envelope.py` used Python's standard-library double precision, seed 121, 100,000 random trials with `N<=2000`, and tolerance `2e-10`. It checked 99,979 supported cases with `x<N`; the largest `log P - bound` was `-5.60527999682e-06`. The pmf check uses log-gamma arithmetic; it is a numerical algebra check, not a proof, face-comparison test, or rounding-error bound. Cases excluded at `x=N` are outside the stated envelope and do not affect the `N0>X` tail.

The C++ and R tests exercise a certified fit for three 2s and three 3s, a binomial-face tie for `{0,1}`, a Poisson-face stop for `{0,0,0,5,5,5}`, and failure after the 10^7 work budget for `{8,8,9,9}`. Three seeded samples in each language from `Hyper(N=4,m=2,k=3)` agree with independent full population grids through `N=200` at relative tolerance `1e-9`. The C++ reference uses log-gamma combinations; the R reference uses `stats::dhyper`. A finite reference grid alone does not prove the tail. The C++ targeted tests passed 29 assertions, and the R hyper tests passed 295 assertions. The separate R conformance check passed 2,555 assertions.

## Final runs

Simbox was unreachable (exit 75) for the four C++ and R runs, so every final command ran locally, sequentially under `/tmp/umvl-cpu/run4`, with BLAS/OpenMP pinned to one thread and temporary files under `build/tmp`. The authorized hard-link dependency copy failed with `EXDEV` across filesystem boundaries; an ordinary local copy of the existing dependencies succeeded, with no package install. The first R regeneration identified an indentation error in the dossier; it was fixed before testing. The first JavaScript suite failed its old all-free hyper conformance expectation; the expectation was updated and both the targeted hyper cases and the full suite passed on rerun.

| Command | Result |
|---|---|
| `just cpp-test` | Pass, 106/106 development shards. |
| `just cpp-test-no-long-double` | Pass, 127/127 tests. |
| `just test` | Pass: 20,102 passes, 0 failures, 0 warnings, 151 tier skips. |
| `just test-full` | Pass: 41,705 passes, 0 failures, 0 warnings, 0 skips. |
| `just js-build` | Pass. |
| `just js-test` | Pass, 413/413. |
| `just site-check` | Pass. |
