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
};

inline NakaResult naka_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> x2(n);
  double sum_log_x = 0.0;
  double sum_x2 = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    x2[i] = x[i] * x[i];
    sum_log_x += std::log(x[i]);
    sum_x2 += x2[i];
  }
  const double dn = static_cast<double>(n);

  GammaResult g = gamma_mle(span{x2.data(), n}, tol, maxit);
  const double shape = g.shape;
  const double scale = shape / g.rate;
  const double logLik =
      dn * (shape * std::log(shape) + std::log(2.0) - std::lgamma(shape) -
            shape * std::log(scale)) +
      (2.0 * shape - 1.0) * sum_log_x - (shape / scale) * sum_x2;
  return {shape, scale, logLik, g.iterations};
}

}  // namespace cppunivariate
