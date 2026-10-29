// Discrete Uniform MLE: a = min(x), b = max(x), pmf = 1 / (b - a + 1).

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct DiscreteUnifResult {
  double min;
  double max;
  double logLik;
};

inline DiscreteUnifResult dunif_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  int a = x[0];
  int b = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    a = std::min(a, x[i]);
    b = std::max(b, x[i]);
  }
  if (b < a) return {kNaN, kNaN, kNaN};
  // b - a + 1 in int overflows for ranges wider than INT_MAX.
  const double logLik =
      -static_cast<double>(n) *
      std::log(static_cast<double>(b) - static_cast<double>(a) + 1.0);
  return {static_cast<double>(a), static_cast<double>(b), logLik};
}

}  // namespace cppunivariate
