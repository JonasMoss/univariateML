// Weibull MLE via Gumbel transform on -log(x). Parameters recovered as
// shape = 1/sigma, scale = exp(-mu + s) where s = -mean(log_x).

#pragma once

#include "../../core/span.hpp"
#include "../gumbel/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct WeibullResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
};

// shape0 is an optional positive starting shape; NaN keeps the automatic start.
inline WeibullResult weibull_mle(span x, double tol, int maxit,
                                  double shape0 = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> log_x(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = -std::log(x[i]);
    sum_log_x += log_x[i];
  }
  const double dn = static_cast<double>(n);
  const double s = -sum_log_x / dn;
  for (auto &v : log_x) v += s;

  GumbelResult g = fit_gumbel_newton(span{log_x.data(), n}, tol, maxit,
      std::isnan(shape0) ? shape0 : -std::log(shape0));
  const double shape = 1.0 / g.sigma;
  const double scale = std::exp(-g.mu + s);
  const double logLik =
      dn * (std::log(shape / scale) +
            (shape - 1.0) * (s - std::log(scale)) - 1.0);
  return {shape, scale, logLik, g.iterations};
}

}  // namespace cppunivariate
