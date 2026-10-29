// Exponential MLE: rate = 1 / mean(x).

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct ExpResult {
  double rate;
  double logLik;
};

inline ExpResult exp_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  double sum = 0.0;
  for (auto v : x) sum += v;
  const double dn = static_cast<double>(n);
  double rate = dn / sum;
  if (!std::isfinite(sum)) {
    long double total = 0.0L;
    for (auto v : x) total += v;
    rate = static_cast<double>(static_cast<long double>(n) / total);
  }
  const double logLik = dn * (std::log(rate) - 1.0);
  return {rate, logLik};
}

}  // namespace cppunivariate
