// Pareto MLE: closed form. xmin = min(x); shape = n / sum(log(x/xmin)).

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct ParetoResult {
  double shape;
  double min;
  double logLik;
};

inline ParetoResult pareto_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  double xmin = x[0];
  for (std::size_t i = 1; i < n; ++i) xmin = std::min(xmin, x[i]);
  if (xmin <= 0.0) return {kNaN, kNaN, kNaN};

  double sum_log_ratio = 0.0;
  double sum_log_x = 0.0;
  for (auto v : x) {
    sum_log_ratio += std::log(v / xmin);
    sum_log_x += std::log(v);
  }
  const double dn = static_cast<double>(n);
  const double alpha = dn / sum_log_ratio;
  const double logLik = dn * std::log(alpha) + dn * alpha * std::log(xmin) -
                        (alpha + 1.0) * sum_log_x;
  return {alpha, xmin, logLik};
}

}  // namespace cppunivariate
