// Discrete Uniform MLE: a = min(x), b = max(x), pmf = 1 / (b - a + 1).

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class DiscreteUnifStatus { Ok, InvalidInput };

struct DiscreteUnifResult {
  double min;
  double max;
  double logLik;
  DiscreteUnifStatus status=DiscreteUnifStatus::Ok;
};

inline DiscreteUnifResult dunif_mle(ispan x, double held_min=std::numeric_limits<double>::quiet_NaN(),
    double held_max=std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN,DiscreteUnifStatus::InvalidInput};

  int a = x[0];
  int b = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    a = std::min(a, x[i]);
    b = std::max(b, x[i]);
  }
  if (b < a) return {kNaN, kNaN, kNaN,DiscreteUnifStatus::InvalidInput};
  const double lower=std::isnan(held_min)?static_cast<double>(a):held_min;
  const double upper=std::isnan(held_max)?static_cast<double>(b):held_max;
  if (!std::isfinite(lower) || !std::isfinite(upper) || lower!=std::floor(lower) ||
      upper!=std::floor(upper) || lower>a || upper<b || lower>upper)
    return {kNaN,kNaN,kNaN,DiscreteUnifStatus::InvalidInput};
  // b - a + 1 in int overflows for ranges wider than INT_MAX.
  const double logLik =
      -static_cast<double>(n) *
      std::log(upper - lower + 1.0);
  return {lower, upper, logLik};
}

}  // namespace cppunivariate
