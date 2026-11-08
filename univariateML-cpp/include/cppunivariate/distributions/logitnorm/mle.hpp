// Logit-normal MLE: y = logit(x), then Normal MLE on y. Adjusted log-
// likelihood includes the change-of-variable Jacobian -sum(log(x*(1-x))).

#pragma once
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../norm/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LogitnormStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct LogitnormResult {
  double mu;
  double sigma;
  double logLik;
  LogitnormStatus status = LogitnormStatus::Ok;
};

inline LogitnormResult logitnorm_mle(span x,
    double mu = std::numeric_limits<double>::quiet_NaN(),
    double sigma = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0 || (!std::isnan(mu) && !std::isfinite(mu)) ||
      (!std::isnan(sigma) && !(sigma > 0 && std::isfinite(sigma))))
    return {kNaN,kNaN,kNaN,LogitnormStatus::InvalidInput};

  std::vector<double> y(n);
  double sum_log_jac = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double v = x[i];
    if (!(v > 0 && v < 1)) return {kNaN,kNaN,kNaN,LogitnormStatus::InvalidInput};
    y[i] = std::log(v / (1.0 - v));
    sum_log_jac += std::log(v * (1.0 - v));
  }
  const auto r = norm_mle(span{y.data(), n},mu,sigma);
  if(r.status==NormStatus::Unbounded && (!std::isnan(mu) || !std::isnan(sigma)))
    return {kNaN,kNaN,kNaN,LogitnormStatus::Unbounded};
  return {r.mean, r.sd, r.logLik - sum_log_jac,
    r.status == NormStatus::Ok ? LogitnormStatus::Ok :
    r.status == NormStatus::Unbounded ? LogitnormStatus::Unbounded :
    r.status == NormStatus::InvalidInput ? LogitnormStatus::InvalidInput : LogitnormStatus::IterationFailure};
}

}  // namespace cppunivariate
