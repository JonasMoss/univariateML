// Rayleigh MLE: closed form. sigma = sqrt(mean(x^2) / 2).

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct RayleighResult {
  double sigma;
  double logLik;
};

inline RayleighResult rayleigh_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  double sum_sq = 0.0;
  for (auto v : x) sum_sq += v * v;
  const double dn = static_cast<double>(n);
  const double sigma2 = sum_sq / (2.0 * dn);
  if (sigma2 <= 0.0) return {kNaN, kNaN};

  const double sigma = std::sqrt(sigma2);
  double sum_log = 0.0;
  for (auto v : x) sum_log += std::log(v);
  const double logLik =
      sum_log - 2.0 * dn * std::log(sigma) - sum_sq / (2.0 * sigma2);
  return {sigma, logLik};
}

}  // namespace cppunivariate
