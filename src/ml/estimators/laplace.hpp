// Laplace MLE: median for location, mean absolute deviation for scale.

#pragma once

#include "../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct LaplaceResult {
  double mu;
  double sigma;
  double logLik;
};

inline LaplaceResult laplace_mle(span x) {
  const std::size_t n = x.size();
  if (n == 0) {
    constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
    return {kNaN, kNaN, kNaN};
  }
  std::vector<double> sorted(x.begin(), x.end());
  std::sort(sorted.begin(), sorted.end());
  const double mu = sorted[n / 2];
  double abs_sum = 0.0;
  for (auto v : x) abs_sum += std::abs(v - mu);
  const double sigma = abs_sum / static_cast<double>(n);
  const double logLik =
      -static_cast<double>(n) * (1.0 + std::log(2.0 * sigma));
  return {mu, sigma, logLik};
}

}  // namespace mlcore
