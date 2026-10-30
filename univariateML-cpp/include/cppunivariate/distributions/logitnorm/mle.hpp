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

inline LogitnormResult logitnorm_mle(span x,
    double mu = std::numeric_limits<double>::quiet_NaN(),
    double sigma = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  std::vector<double> y(n);
  double sum_log_jac = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double v = x[i];
    if (!(v > 0 && v < 1)) return {kNaN,kNaN,kNaN};
    y[i] = std::log(v / (1.0 - v));
    sum_log_jac += std::log(v * (1.0 - v));
  }
  const auto r = norm_mle(span{y.data(), n});
  if (std::isnan(mu) && std::isnan(sigma))
    return {r.mean, r.sd, r.logLik - sum_log_jac};
  if (std::isnan(mu)) mu = r.mean;
  long double ss = 0;
  for (double z : y) {
    const long double d = static_cast<long double>(z) - mu;
    ss += d*d;
  }
  if (std::isnan(sigma)) sigma = std::sqrt(ss / n);
  if (!std::isfinite(mu) || !(sigma > 0) || !std::isfinite(sigma))
    return {kNaN,kNaN,kNaN};
  const double ll = -static_cast<double>(n) * (std::log(sigma) + .5*std::log(2*M_PI)) -
                    .5L*ss/(static_cast<long double>(sigma)*sigma) - sum_log_jac;
  return {mu,sigma,ll};
}

}  // namespace cppunivariate
