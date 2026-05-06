// Logit-normal MLE: y = logit(x), then Normal MLE on y. Adjusted log-
// likelihood includes the change-of-variable Jacobian -sum(log(x*(1-x))).

#pragma once

#include "../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

struct LogitnormResult {
  double mu;
  double sigma;
  double logLik;
};

inline LogitnormResult logitnorm_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  double sum_logit = 0.0;
  double sum_log_jac = 0.0;
  for (auto v : x) {
    sum_logit += std::log(v / (1.0 - v));
    sum_log_jac += std::log(v * (1.0 - v));
  }
  const double dn = static_cast<double>(n);
  const double mu = sum_logit / dn;

  double sum_sq_dev = 0.0;
  for (auto v : x) {
    const double d = std::log(v / (1.0 - v)) - mu;
    sum_sq_dev += d * d;
  }
  const double sigma = std::sqrt(sum_sq_dev / dn);
  const double logLik =
      -0.5 * dn * (std::log(2.0 * M_PI * sigma * sigma) + 1.0) - sum_log_jac;
  return {mu, sigma, logLik};
}

}  // namespace mlcore
