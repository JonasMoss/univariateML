// Power distribution MLE: closed form using log(x) statistics.
//
// alpha = max(x) + epsilon, beta = 1 / (log(alpha) - mean(log x)).
// Returns NaN params if min(x) <= 0; the caller turns that into an error.

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct PowerResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
};

inline PowerResult power_mle(span x, double epsilon) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  double xmin = x[0];
  double xmax = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    xmin = std::min(xmin, x[i]);
    xmax = std::max(xmax, x[i]);
  }
  if (xmin <= 0.0) return {kNaN, kNaN, kNaN, 0};

  double sum_log = 0.0;
  for (auto v : x) sum_log += std::log(v);
  const double m = sum_log / static_cast<double>(n);
  const double alpha = xmax + epsilon;
  const double beta = 1.0 / (std::log(alpha) - m);
  const double logLik = static_cast<double>(n) *
      (std::log(beta) - beta * std::log(alpha) + (beta - 1.0) * m);
  return {alpha, beta, logLik, 0};
}

}  // namespace cppunivariate
