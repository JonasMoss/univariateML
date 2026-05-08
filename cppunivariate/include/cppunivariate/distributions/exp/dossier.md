# Exponential

The simple-distribution pilot for the dossier convention. Closed
form, no iteration, no special cases — the whole MLE is one line.

## Definition

Continuous on `x >= 0` with rate `lambda > 0`:

```
f(x; lambda)  = lambda * exp(-lambda * x)
F(x; lambda)  = 1 - exp(-lambda * x)
F^{-1}(p)     = -log(1 - p) / lambda
```

`stats::dexp` / `stats::rexp` from base R cover the dpqr surface;
`cppunivariate` only ships the estimator (no `dpqr.hpp` in this
folder).

## Parameters

| name | role | support | default | estimable |
|------|------|---------|---------|-----------|
| rate | rate | (0, ∞)  | 1       | yes       |

`estimable` is forward-looking — see the variable-parameter-selection
roadmap entry.

## MLE

The score is `n / lambda - sum(x)`, zero at

```
lambda_hat = n / sum(x)         (= 1 / mean(x))
```

The Hessian is `n / lambda^2`, strictly positive everywhere on the
support, so the MLE is unique and always exists for any sample with
`n >= 1` and `sum(x) > 0`.

The Fisher information is `1 / lambda^2` per observation, recovering
the same Hessian (Exponential is a one-parameter exponential family
with `lambda` as the canonical rate parameter).

## Algorithm

`exp_mle()` computes the closed form in a single pass. No optimiser,
no tolerance, no iteration limit, no failure modes other than empty
input (which returns `{NaN, NaN}`).

## Implementation provenance

The C++ port reproduces the R-only `mlexp.R` exactly — there's only
one expression to reproduce. Formal provenance IDs are deferred (see
`roadmap/2026-05-08-implementation-provenance.md`).

## Quality status

`ported`. No second-implementation cross-check exists yet (would be
`validated`); the closed form is its own correctness proof.

## Tests + goldens

- R-side: `r/tests/testthat/test_mlexp.R`.
- Golden regression: `quality/golden/exp.json` (locked at 1e-12).
- C++ doctest: included in `cppunivariate/tests/test_estimators.cpp`'s
  golden-driven block.

## References

- Casella, G. and Berger, R. L. (2002). *Statistical Inference*, 2nd
  ed. Section 6.2 — Exponential as the canonical one-parameter
  exponential family.
