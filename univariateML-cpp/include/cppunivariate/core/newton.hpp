// Contract, retained choices, evidence and known numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
// 1D Newton-Raphson helpers.
//
// `newton_raphson_1d` mirrors the R-side convention from R/code_misc.R:64-86
// (`newton_raphson_1d(f_over_df, param0, ...)`) — relative-step convergence,
// so C++ and R estimators converge on identical iteration counts when both
// use this rule.
//
// `newton_raphson_1d_abs` is the absolute-step variant some pre-existing
// estimators rely on (e.g. gamma_mle's `|step| < tol` rule). Goldens
// reproduce bit-for-bit only if the convergence test is preserved.
//
// The relative variant rejects a NaN next iterate; the absolute variant
// rejects a NaN step. Callers validate starts and the returned score/domain.

#pragma once

#include <cmath>

namespace cppunivariate {

struct NewtonResult {
  double x;
  int iterations;
  bool converged;
};

// Relative-step convergence: |dx / x_old| < reltol.
template <typename FOverDf>
inline NewtonResult newton_raphson_1d(double param0, FOverDf f_over_df,
                                      double reltol, int maxit) {
  double param = param0;
  int i = 1;
  for (; i <= maxit; ++i) {
    const double next = param - f_over_df(param);
    if (next != next) return {param, i, false};
    if (std::abs((param - next) / param) < reltol) {
      param = next;
      break;
    }
    param = next;
  }
  return {param, i, i <= maxit};
}

// Absolute-step convergence: |dx| < tol.
template <typename FOverDf>
inline NewtonResult newton_raphson_1d_abs(double param0, FOverDf f_over_df,
                                          double tol, int maxit) {
  double param = param0;
  int i = 1;
  for (; i <= maxit; ++i) {
    const double step = f_over_df(param);
    if (step != step) return {param, i, false};
    param -= step;
    if (std::abs(step) < tol) break;
  }
  return {param, i, i <= maxit};
}

}  // namespace cppunivariate
