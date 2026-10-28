// Lognormal MLE: y = log(x), then Normal MLE on y. Adjusted log-likelihood
// includes the change-of-variable Jacobian -sum(log(x)).

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct LognormalResult {
  double meanlog;
  double sdlog;
  double logLik;
};

inline LognormalResult lognormal_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  double sum_log = 0.0;
  for (auto v : x) sum_log += std::log(v);
  const double dn = static_cast<double>(n);
  const double meanlog = sum_log / dn;

  double sum_sq_dev = 0.0;
  for (auto v : x) {
    const double d = std::log(v) - meanlog;
    sum_sq_dev += d * d;
  }
  const double sdlog = std::sqrt(sum_sq_dev / dn);
  const double logLik =
      -0.5 * dn * (std::log(2.0 * M_PI * sdlog * sdlog) + 1.0) - sum_log;
  return {meanlog, sdlog, logLik};
}

}  // namespace cppunivariate
