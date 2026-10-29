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
  double sum_log = 0.0;
  for (auto v : x) {
    sum_log += std::log(v);
  }
  long double total = 0.0L;
  for (auto v : x) total += v;
  const double mu = static_cast<double>(total / dn);
  double lambda;
  long double relative_ss = 0.0L;
  for (auto v : x) {
    const long double y = static_cast<long double>(v) / mu;
    relative_ss += (y - 1.0L) * (y - 1.0L) / y;
  }
  lambda = static_cast<double>(static_cast<long double>(mu) * dn / relative_ss);
  const double L = sum_log / dn;

  // At the profiled lambda, the quadratic term equals n exactly.
  const double logLik = -0.5 * dn *
      (3.0 * L - std::log(lambda) + std::log(2.0 * M_PI) + 1.0);
  return {mu, lambda, logLik};
}

}  // namespace cppunivariate
