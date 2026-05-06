// 1D Newton-Raphson helper. Mirrors the R-side convention from
// R/code_misc.R:64-86 (`newton_raphson_1d(f_over_df, param0, ...)`) so
// C++ and R estimators converge on identical iteration counts.
//
// Caller provides f_over_df(x) returning the Newton step f(x)/f'(x).
// Loop: x_{k+1} = x_k - f_over_df(x_k).
// Convergence: |dx / x| < reltol.
// Bails on NaN with converged=false.

#pragma once

#include <cmath>

namespace mlcore {

struct NewtonResult {
  double x;
  int iterations;
  bool converged;
};

template <typename FOverDf>
inline NewtonResult newton_raphson_1d(double param0, FOverDf f_over_df,
                                      double reltol, int maxit) {
  double param = param0;
  int i = 1;
  for (; i <= maxit; ++i) {
    const double next = param - f_over_df(param);
    if (next != next) return {param, i, false};  // NaN bail
    if (std::abs((param - next) / param) < reltol) {
      param = next;
      break;
    }
    param = next;
  }
  return {param, i, i <= maxit};
}

}  // namespace mlcore
