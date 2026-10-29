// Logit-normal MLE: y = logit(x), then Normal MLE on y. Adjusted log-
// likelihood includes the change-of-variable Jacobian -sum(log(x*(1-x))).

#pragma once

#include "../../core/span.hpp"
#include "../norm/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct LogitnormResult {
  double mu;
  double sigma;
  double logLik;
};

inline LogitnormResult logitnorm_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  std::vector<double> y(n);
  double sum_log_jac = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double v = x[i];
    y[i] = std::log(v / (1.0 - v));
    sum_log_jac += std::log(v * (1.0 - v));
  }
  const auto r = norm_mle(span{y.data(), n});
  return {r.mean, r.sd, r.logLik - sum_log_jac};
}

}  // namespace cppunivariate
