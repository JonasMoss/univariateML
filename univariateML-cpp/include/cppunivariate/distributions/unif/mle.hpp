// Uniform MLE: a = min(x), b = max(x).

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class UnifStatus { Ok, InvalidInput, Incompatible, Unbounded };

struct UnifResult {
  double min;
  double max;
  double logLik;
  UnifStatus status = UnifStatus::Ok;
};

inline UnifResult unif_mle(span x,
    double min = std::numeric_limits<double>::quiet_NaN(),
    double max = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  for (double v : x) if (!std::isfinite(v))
    return {kNaN,kNaN,kNaN,UnifStatus::InvalidInput};
  double a = x[0];
  double b = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    a = std::min(a, x[i]);
    b = std::max(b, x[i]);
  }
  if ((!std::isnan(min) && !std::isfinite(min)) ||
      (!std::isnan(max) && !std::isfinite(max)) ||
      (!std::isnan(min) && !std::isnan(max) && min >= max))
    return {kNaN,kNaN,kNaN,UnifStatus::InvalidInput};
  if ((!std::isnan(min) && min > a) || (!std::isnan(max) && max < b))
    return {kNaN,kNaN,kNaN,UnifStatus::Incompatible};
  if (!std::isnan(min)) a = min;
  if (!std::isnan(max)) b = max;
  if (b <= a) return {kNaN, kNaN, kNaN,UnifStatus::Unbounded};
  const double log_width = std::isnan(min) && std::isnan(max) ? std::log(b-a) :
    static_cast<double>(std::log(static_cast<long double>(b)-a));
  const double logLik = -static_cast<double>(n) * log_width;
  return {a, b, logLik};
}

}  // namespace cppunivariate
