# C++ style guide for mlcore

mlcore is the C++ artifact under `src/ml/`. Header-only, C++17, depends
only on `<cmath>` / `<algorithm>` / `<vector>` / `<cstddef>` and
Boost.Math (via the `BH` package or system Boost).

## Layout

```
src/ml/
  core/           # span, optimisers (newton, bfgs), reductions, finite_diff, reparam
  estimators/     # one .hpp per distribution; *_mle(span x, ...) -> *Result
  distributions/  # one .hpp per distribution; d/p/q/r as templates
  special/        # special.hpp (Boost.Math seam), skewed_densities.hpp
  ml.hpp          # aggregator
```

R-shell adapters live outside `src/ml/`:

- `src/ml_exports.cpp` — Rcpp wrappers calling mlcore estimators.
- `src/distribution_exports.cpp` + `src/distributions/<dist>.cpp` —
  Rcpp wrappers calling mlcore distributions.

## Boundary types

- Public input: `mlcore::span` (a C++17 substitute for
  `std::span<const double>`); see `core/span.hpp`. `mlcore::ispan`
  for integer data.
- Public output: per-estimator `*Result` struct. Always includes
  `logLik`, often `iterations` and a `*Status` enum.
- R-shell adapters wrap with `Rcpp::NumericVector` /
  `Rcpp::IntegerVector`. Never `arma::vec`.

## Estimator template

```cpp
namespace mlcore {

enum class FooStatus { Ok, IterationFailure, BadInit };

struct FooResult {
  double param1, param2;
  double logLik;
  int iterations;
  FooStatus status;
};

inline FooResult foo_mle(span x, double tol, int maxit) {
  // mlcore::sum / mean / sum_log etc. from reductions.hpp;
  // mlcore::special::digamma / lgamma / etc. from special/special.hpp;
  // mlcore::newton_raphson_1d for 1D Newton or
  // mlcore::bfgs_minimize for multi-D.
}

}  // namespace mlcore
```

Match the existing files in `estimators/` for naming and convergence-rule
choices. The 1D Newton helpers in `core/newton.hpp` come in `_rel` and
`_abs` step variants — pick what the original R algorithm used so
goldens stay reproducible.

## Multi-D optimiser

Hand-rolled BFGS in `src/ml/core/bfgs.hpp`: header-only quasi-Newton
with backtracking Armijo line search and the standard two-rank update
on the inverse-Hessian approximation.

Cost concept (compile-time):

```cpp
int    dim() const;
double operator()(const double *theta) const;
void   gradient(const double *theta, double *g) const;
```

If you only have an objective, wrap in `mlcore::finite_diff_cost<Cost>`
from `core/finite_diff.hpp`; that adapter supplies a central-difference
gradient. The six fGarch-family estimators (paralogis, std, ged, snorm,
sstd, sged) use this path.

For unconstrained reparametrisation: `core/reparam.hpp` has `from_log`
/ `to_log` and `from_log_shift` / `to_log_shift` (e.g. `nu > 2` for
Student-t / sstd).

### Tolerances

`BfgsControl` exposes three convergence rules; **any one** fires:

- `||g||_2 < grad_tol` (default `1e-6`)
- `||theta_new − theta||_inf < step_tol * (||theta||_inf + step_tol)`
  (default `1e-8`)
- `(f_old − f_new) < reltol * (|f_old| + reltol)` (default `1e-12`)

The reltol default is intentionally tight: at the standard
`kFitTol = 1.22e-4` the f-relative criterion fires on the asymptotic
flat approach to an MLE long before the gradient is actually small.
Estimators that need a score-residual gate map their user-facing
`tol` onto `grad_tol`, not `reltol`.

### Score-residual gate

Each multi-D estimator's doctest pairs the golden check with a
`finite_diff_gradient` evaluation at the recovered MLE:

```cpp
CHECK( ||g||_2 < 1e-3 )
```

This catches the "fake-fit" class of bug: a parameter implicitly
fixed during fitting will leave a non-zero score component there,
regardless of what optimiser produced the result. Independent of
optimiser trajectory; survives golden regeneration.

### Wrapper failure handling

Match `nlminb`'s legacy behaviour: only `Rcpp::stop` on truly invalid
output (NaN in any returned parameter) or `InvalidStart` (data isn't
fittable). On plain BFGS iteration-limit hits, return whatever
`theta_hat` the optimiser landed on — downstream consumers running on
the legacy fGarch path expect a usable result back, not an error.

## Style rules

- No `using` declarations. Always be explicit.
- No raw pointers in mlcore. R-shell adapters can use Rcpp types
  freely.
- No printing or logging in mlcore; surface diagnostics via
  `Result::status` and the `*Status` enum.
- C++17 features OK; avoid C++20-only syntax (the project doesn't
  bump the standard yet).
- clang-format the file before committing.
- `tol` for tolerance, `maxit` for iteration limits — uniform across
  all estimators.

## Adding a new distribution

1. Implement `mlcore::*_mle` in `src/ml/estimators/<name>.hpp`. If the
   distribution is also new on the dpqr side, add the templates in
   `src/ml/distributions/<name>.hpp` too.
2. Add an include line to `src/ml/ml.hpp`.
3. Wire an Rcpp wrapper in `src/ml_exports.cpp` (and
   `src/distribution_exports.cpp` if dpqr).
4. Update `metadata/models.json` (canonical param names + sampler).
5. Regenerate goldens via `Rscript scripts/gen_goldens.R`.
6. Add a doctest case in `tests/cpp/test_estimators.cpp`. For multi-D
   estimators add the score-residual `||g||_2 < 1e-3` check too.
7. `just test` and `just cpp-test` must both pass.
