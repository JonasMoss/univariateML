// Inverse Gamma MLE: y = 1/x, then Gamma MLE on y. Result fields
// (shape, scale) follow the standard shape-scale parameterisation:
//   f_X(x; shape, scale) = scale^shape / Gamma(shape) * x^(-shape-1)
//                        * exp(-scale / x).
// Implementation reuses GammaResult{shape, rate} on Y = 1/X; scale is
// numerically equal to GammaResult.rate via the inverse transform.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct InvGammaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
};

inline InvGammaResult invgamma_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> invx(n);
  double sum_log = 0.0;
  double sum_inv = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    sum_log += std::log(x[i]);
    sum_inv += invx[i];
  }
  const double dn = static_cast<double>(n);
  GammaResult g = gamma_mle(span{invx.data(), n}, tol, maxit);

  const double L = sum_log / dn;
  const double M = sum_inv / dn;
  const double logLik = dn * (g.shape * std::log(g.rate) -
                              std::lgamma(g.shape) -
                              (g.shape + 1.0) * L - g.rate * M);
  return {g.shape, g.rate, logLik, g.iterations};
}

}  // namespace cppunivariate
