// Inverse Weibull MLE via Gumbel transform on log(x). Parameters recovered
// as shape = 1/sigma, rate = exp(-mu + s) where s = -mean(log x).

#pragma once

#include "../core/span.hpp"
#include "gumbel.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct InvWeibullResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

inline InvWeibullResult invweibull_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> log_x(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = std::log(x[i]);
    sum_log_x += log_x[i];
  }
  const double dn = static_cast<double>(n);
  const double s = -sum_log_x / dn;
  for (auto &v : log_x) v += s;

  GumbelResult g = fit_gumbel_newton(span{log_x.data(), n}, tol, maxit);
  const double rate_log = -g.mu + s;
  const double rate = std::exp(rate_log);
  const double shape = 1.0 / g.sigma;
  const double logLik =
      dn * (std::log(shape) + shape * (-rate_log + s) - 1.0 + s);
  return {shape, rate, logLik, g.iterations};
}

}  // namespace mlcore
