// Uniform MLE: a = min(x), b = max(x).

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct UnifResult {
  double min;
  double max;
  double logLik;
};

inline UnifResult unif_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  double a = x[0];
  double b = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    a = std::min(a, x[i]);
    b = std::max(b, x[i]);
  }
  if (b <= a) return {kNaN, kNaN, kNaN};
  const double logLik = -static_cast<double>(n) * std::log(b - a);
  return {a, b, logLik};
}

}  // namespace cppunivariate
