# Lomax (Pareto Type II)

## Definition

Continuous on `x > 0` with rate `lambda > 0` and shape `kappa > 0`:

```
f(x; lambda, kappa)  = lambda * kappa * (1 + lambda * x)^(-(kappa+1))
F(x; lambda, kappa)  = 1 - (1 + lambda * x)^(-kappa)
F^{-1}(p)            = ((1 - p)^(-1/kappa) - 1) / lambda
```

This is the `extraDistr::Lomax` parameterisation. Some references use
the equivalent `(scale = 1/lambda, shape = kappa)` form; the inverse
relationship between the two `lambda` conventions is the most common
source of confusion when comparing references.

## Parameters

| name   | role  | support  | default | estimable |
|--------|-------|----------|---------|-----------|
| lambda | rate  | (0, ∞)   | 1       | yes       |
| kappa  | shape | (0, ∞)   | 2       | yes       |

`estimable` is forward-looking: today every parameter is fitted. The
field is the metadata seam for the future variable-parameter-selection
feature (see `roadmap/2026-05-08-variable-parameter-selection.md`).

## MLE — existence

The two-parameter Lomax MLE exists only when

```
mean(x^2) >= 2 * mean(x)^2     (equivalently, sample CV >= 1).
```

Below that threshold the likelihood has no interior maximum: as
`kappa -> Inf` with `lambda * kappa` held constant, the Lomax density
converges to an Exponential, and the likelihood approaches that limit
without ever attaining it. The R-side wrapper `mllomax()` detects the
condition at entry and stops with a message recommending `mlexp()`.

## MLE — score and Hessian

Profiling out `kappa` at fixed `lambda` gives the closed-form
maximiser

```
kappa(lambda) = n / sum(log(1 + lambda * x_i))
```

Substituting back yields a 1D problem in `lambda` whose score is

```
f(lambda) = 1 / lambda - psi - S1
```

with

```
S   = (1/n) * sum(log(1 + lambda * x_i))
S1  = (1/n) * sum(x_i / (1 + lambda * x_i))
S2  = (1/n) * sum((x_i / (1 + lambda * x_i))^2)
psi = S1 / S
```

Newton derivative:

```
f'(lambda) = psi^2 + S2 / S + S2 - 1 / lambda^2
```

Full 2-parameter analytic Hessian (in `mle.hpp`'s `LomaxCost::hessian`):

```
H_ll/obs = ((1 + lx)^2 - lambda^2 * x^2 * (kappa + 1)) /
           (lambda^2 * (1 + lx)^2)              where lx = lambda * x
H_kk/obs = 1 / kappa^2                          (data-independent)
H_lk/obs = x / (1 + lambda * x)
```

## Algorithm

1. **Method-of-moments init** for `lambda`. With `m = mean(x)`,
   `s = mean(x^2)`:
   ```
   alpha0  = 2 * (s - m^2) / (s - 2 m^2)
   lambda0 = 1 / (m * (alpha0 - 1))
   ```
   Reject if `lambda0 <= 0` or non-finite (`InvalidStart`).
2. **1D Newton on `lambda`** using the score above. Hybrid relative +
   absolute step convergence (kept verbatim from the original R
   implementation):
   ```
   |step| < |lambda| * tol + tol
   ```
   Reject any step that lands at `lambda <= 0` (`IterationFailure`).
3. **Closed-form `kappa`** at the converged `lambda`.

Defaults: `tol = 1.220703e-4`, `maxit = 100`.

## Status outcomes

`LomaxStatus` enum:

- `Ok` — fit converged.
- `MeanVarianceCondition` — `s < 2 m^2`; the MLE doesn't exist. The
  R-side wrapper translates this into `Rcpp::stop("...recommend
  mlexp()")`.
- `InvalidStart` — MOM produced a non-positive or non-finite seed for
  `lambda`. R-side wrapper returns NA.
- `IterationFailure` — Newton stepped to non-positive `lambda` before
  convergence. R-side wrapper returns NA.

## Implementation provenance

Algorithm matches the legacy R-only implementation `mllomax.R` from
the pre-C++ tree. Ported in 2025 during the de-Armadillo sweep; the
C++ port reproduces the original goldens at 1e-12. Formal provenance
IDs are deferred — see
`roadmap/2026-05-08-implementation-provenance.md`.

## Quality status

`ported` — the implementation reproduces an earlier accepted
behaviour (the R-only version) and the goldens. Promotion to
`validated` requires the broader multi-layer validation work (see
`roadmap/2026-05-08-quality-ladder.md`).

## Tests + goldens

- R-side: `r/tests/testthat/test_mllomax.R` (32 assertions covering
  the moment-condition error path, NA propagation, and the moments
  → estimates round-trip).
- Golden regression: `quality/golden/lomax.json`. Locked at 1e-12.
- C++ doctest: `cppunivariate/tests/test_estimators.cpp` includes a
  Lomax block driven from the same JSON.
- Hessian check: `cppunivariate/tests/test_hessians.cpp` exercises
  the analytic Hessian against finite differences at the recovered
  MLE.

## References

- Kleiber, C. and Kotz, S. (2003). *Statistical Size Distributions in
  Economics and Actuarial Sciences*. Wiley.
- Giles, D. E., Feng, H. and Godwin, R. T. (2013). On the bias of the
  maximum likelihood estimator for the two-parameter Lomax
  distribution. *Communications in Statistics — Theory and Methods*,
  42(11), 1934–1950.
