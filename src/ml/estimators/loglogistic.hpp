// Log-logistic MLE: y = log(x), then Logistic MLE on y. Parameters are
// recovered analytically: shape = 1/scale_logistic, rate = exp(-mu_logistic).
// The log-likelihood is rederived in the log-logistic parameterisation.

#pragma once

#include "../core/span.hpp"
#include "logistic.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

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
  const double scale_ll = 1.0 / rate;
  const double log_scale_ll = std::log(scale_ll);

  double sum_log_x = 0.0, sum_log1p_z_pow = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum_log_x += y[i];  // = log(x[i])
    sum_log1p_z_pow += std::log1p(std::pow(x[i] / scale_ll, shape));
  }
  const double logLik =
      static_cast<double>(n) * (std::log(shape) - log_scale_ll) +
      (shape - 1.0) * (sum_log_x - static_cast<double>(n) * log_scale_ll) -
      2.0 * sum_log1p_z_pow;
  return {shape, rate, logLik, base.iterations};
}

}  // namespace mlcore
