// Nakagami MLE: y = x^2, then Gamma MLE on y. Shape parameter is
// preserved; scale is rederived from the gamma rate as scale = shape/rate.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct NakaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  GammaStatus status = GammaStatus::Ok;
};

inline NakaResult naka_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> x2(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    x2[i] = x[i] * x[i];
    sum_log_x += std::log(x[i]);
  }
  const double dn = static_cast<double>(n);

  GammaResult g = gamma_mle(span{x2.data(), n}, tol, maxit);
  const double shape = g.shape;
  const double scale = shape / g.rate;
  // X^2 is gamma: log f_naka(x) = log f_gamma(x^2) + log 2 + log x.
  const double logLik = g.logLik + dn * std::log(2.0) + sum_log_x;
  return {shape, scale, logLik, g.iterations, g.status};
}

}  // namespace cppunivariate
