// Zero-Inflated Poisson MLE. Closed-form via Lambert W: when the empirical
// zero-rate exceeds exp(-mean), the principal branch W_0 yields the MLE.

#pragma once

#include "../../core/lambert_w.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class ZipStatus { Ok = 0, InvalidInput, NumericalFailure };

struct ZipResult {
  double lambda;
  double pi;
  double logLik;
  ZipStatus status;
};


inline ZipResult zip_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN, ZipStatus::InvalidInput};

  double sum_zero = 0.0, sum_x = 0.0;
  for (auto v : x) {
    if (v < 0) return {kNaN, kNaN, kNaN, ZipStatus::InvalidInput};
    if (v == 0) sum_zero += 1.0;
    sum_x += static_cast<double>(v);
  }
  const double dn = static_cast<double>(n);
  const double r0 = sum_zero / dn;
  const double x_bar = sum_x / dn;

  double lambda = 0.0;
  double pi = 0.0;
  if (r0 == 1.0) {
    // Non-identifiable: choose a finite representative of the point mass.
    return {1.0, 1.0, 0.0, ZipStatus::Ok};
  } else if (r0 > std::exp(-x_bar)) {
    const double gamma = x_bar / (1.0 - r0);
    const double z = -gamma * std::exp(-gamma);
    const double w = lambertw0(z);
    lambda = w + gamma;
    pi = (r0 - std::exp(-lambda)) / (1.0 - std::exp(-lambda));
  } else {
    pi = 0.0;
    lambda = x_bar;
  }

  if (!std::isfinite(lambda) || !std::isfinite(pi) ||
      lambda <= 0.0 || pi < 0.0 || pi >= 1.0)
    return {kNaN, kNaN, kNaN, ZipStatus::NumericalFailure};

  double logLik = 0.0;
  for (auto v : x) {
    if (v == 0) {
      logLik += std::log(pi + (1.0 - pi) * std::exp(-lambda));
    } else {
      logLik += std::log(1.0 - pi) +
                static_cast<double>(v) * std::log(lambda) - lambda -
                cppunivariate::special::lgamma(static_cast<double>(v) + 1.0);
    }
  }
  if (!std::isfinite(logLik))
    return {kNaN, kNaN, kNaN, ZipStatus::NumericalFailure};
  return {lambda, pi, logLik, ZipStatus::Ok};
}

}  // namespace cppunivariate
