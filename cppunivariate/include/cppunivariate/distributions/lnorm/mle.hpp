// Lognormal MLE: y = log(x), then Normal MLE on y. Adjusted log-likelihood
// includes the change-of-variable Jacobian -sum(log(x)).

#pragma once

#include "../../core/span.hpp"
#include "../norm/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

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

  std::vector<double> y(n);
  double sum_log = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    y[i] = std::log(x[i]);
    sum_log += y[i];
  }
  const auto r = norm_mle(span{y.data(), n});
  return {r.mean, r.sd, r.logLik - sum_log};
}

}  // namespace cppunivariate
