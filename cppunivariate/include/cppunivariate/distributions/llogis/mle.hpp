// Log-logistic MLE: y = log(x), then Logistic MLE on y. Parameters are
// recovered analytically: shape = 1/scale_logistic, rate = exp(-mu_logistic).
// The log-likelihood is rederived in the log-logistic parameterisation.

#pragma once

#include "../../core/span.hpp"
#include "../logis/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct LogLogisticResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

inline LogLogisticResult loglogistic_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> y(n);
  for (std::size_t i = 0; i < n; ++i) y[i] = std::log(x[i]);
  LogisticResult base = logistic_mle(span{y.data(), n}, tol, maxit);

  const double shape = 1.0 / base.scale;
  const double rate = std::exp(-base.mu);
  // Change of variables avoids powers of extreme observations.
  double sum_log_x = 0.0;
  for (auto v : y) sum_log_x += v;
  const double logLik = base.logLik - sum_log_x;
  return {shape, rate, logLik, base.iterations};
}

}  // namespace cppunivariate
