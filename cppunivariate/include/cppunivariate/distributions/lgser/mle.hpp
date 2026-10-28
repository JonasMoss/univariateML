// Logarithmic-series MLE. Closed-form via Lambert W. The all-ones case
// is degenerate (theta is unidentified); flagged via the result struct.

#pragma once

#include "../../core/lambert_w.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct LgserResult {
  double theta;
  double logLik;
  bool all_ones;
};

inline LgserResult lgser_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, kNaN, false};

  double sumx = 0.0;
  for (auto v : x) sumx += static_cast<double>(v);
  const double dn = static_cast<double>(n);
  const double x_bar = sumx / dn;
  if (sumx == dn) return {kNaN, kNaN, true};

  const double z = -std::exp(-1.0 / x_bar) / x_bar;
  const double xi = -x_bar * lambertwm1(z) - 1.0;
  const double theta = xi / (xi + 1.0);

  double sum_log_x = 0.0;
  for (auto v : x) sum_log_x += std::log(static_cast<double>(v));
  const double logLik = -dn * std::log(-std::log1p(-theta)) +
                        sumx * std::log(theta) - sum_log_x;
  return {theta, logLik, false};
}

}  // namespace cppunivariate
