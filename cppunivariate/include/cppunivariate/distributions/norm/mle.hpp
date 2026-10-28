// Normal MLE: closed form.
//
// Standalone C++ implementation, free of R / Armadillo dependencies.
//
#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct NormResult {
  double mean;
  double sd;
  double logLik;
};

inline NormResult norm_mle(span x) {
  const std::size_t n = x.size();
  if (n == 0) {
    constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
    return {kNaN, kNaN, kNaN};
  }
  double sum = 0.0;
  for (auto v : x) sum += v;
  const double mu = sum / static_cast<double>(n);
  double ss = 0.0;
  for (auto v : x) {
    const double d = v - mu;
    ss += d * d;
  }
  const double var = ss / static_cast<double>(n);
  const double sigma = std::sqrt(var);
  const double logLik =
      -0.5 * static_cast<double>(n) * (std::log(2.0 * M_PI * var) + 1.0);
  return {mu, sigma, logLik};
}

}  // namespace cppunivariate
