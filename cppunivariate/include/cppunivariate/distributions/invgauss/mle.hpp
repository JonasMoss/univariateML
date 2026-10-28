// Inverse Gaussian MLE: closed form.
//   mu = mean(x)
//   1/lambda = mean(1/x) - 1/mu

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct InvGaussResult {
  double mu;
  double lambda;
  double logLik;
};

inline InvGaussResult invgauss_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN};

  const double dn = static_cast<double>(n);
  double sum = 0.0;
  double sum_inv = 0.0;
  double sum_log = 0.0;
  for (auto v : x) {
    sum += v;
    sum_inv += 1.0 / v;
    sum_log += std::log(v);
  }
  const double mu = sum / dn;
  const double inv_x_bar = sum_inv / dn;
  const double lambda = 1.0 / (inv_x_bar - 1.0 / mu);
  const double L = sum_log / dn;

  double sum_S = 0.0;
  for (auto v : x) {
    const double d = v - mu;
    sum_S += d * d / v;
  }
  const double S = sum_S / dn;
  const double logLik =
      -0.5 * dn *
      (3.0 * L - std::log(lambda) + std::log(2.0 * M_PI) +
       lambda / (mu * mu) * S);
  return {mu, lambda, logLik};
}

}  // namespace cppunivariate
