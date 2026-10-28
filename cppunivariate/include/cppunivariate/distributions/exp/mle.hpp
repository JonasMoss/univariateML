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
  const double rate = dn / sum;
  const double logLik = dn * std::log(rate) - rate * sum;
  return {rate, logLik};
}

}  // namespace cppunivariate
