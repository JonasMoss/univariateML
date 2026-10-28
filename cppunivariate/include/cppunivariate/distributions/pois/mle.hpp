// Poisson MLE: lambda = mean(x). Log-likelihood via Rmath dpois for
// bit-identical reproduction of pre-port behaviour.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cstddef>
#include <limits>

namespace cppunivariate {

struct PoisResult {
  double lambda;
  double logLik;
};

inline PoisResult pois_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  double sum = 0.0;
  for (auto v : x) sum += static_cast<double>(v);
  const double lambda = sum / static_cast<double>(n);

  double logLik = 0.0;
  for (auto v : x) {
    logLik += ::cppunivariate::special::dpois(static_cast<double>(v), lambda, 1);
  }
  return {lambda, logLik};
}

}  // namespace cppunivariate
