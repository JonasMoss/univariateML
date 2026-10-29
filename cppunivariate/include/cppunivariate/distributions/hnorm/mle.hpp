#pragma once

#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {

enum class HnormStatus { Ok = 0, DegenerateData, InvalidData };
struct HnormResult {
  double mu, sigma, logLik;
  int iterations;
  HnormStatus status;
};

inline HnormResult hnorm_mle(span x, double held_mu = std::numeric_limits<double>::quiet_NaN()) {
  const bool fixed = !std::isnan(held_mu);
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (x.size() < (fixed ? 1u : 2u)) return {nan, nan, nan, 0, HnormStatus::DegenerateData};
  for (double v : x)
    if (!std::isfinite(v)) return {nan, nan, nan, 0, HnormStatus::InvalidData};
  const double mu = fixed ? held_mu : *std::min_element(x.begin(), x.end());
  if (!std::isfinite(mu) || *std::min_element(x.begin(), x.end()) < mu)
    return {mu, nan, nan, 0, HnormStatus::InvalidData};
  const double largest = *std::max_element(x.begin(), x.end()) - mu;
  if (largest == 0.0) return {mu, nan, nan, 0, HnormStatus::DegenerateData};
  if (!std::isfinite(largest)) return {mu, nan, nan, 0, HnormStatus::InvalidData};
  // Scale the sum of squares to avoid overflow/underflow for finite ranges.
  double ss = 0.0;
  for (double v : x) { const double z = (v - mu) / largest; ss += z*z; }
  const double sigma = largest * std::sqrt(ss / static_cast<double>(x.size()));
  if (!(sigma > 0.0) || !std::isfinite(sigma))
    return {mu, sigma, nan, 0, HnormStatus::InvalidData};
  const double ll = static_cast<double>(x.size()) *
      (0.5 * std::log(2.0 / std::acos(-1.0)) - std::log(sigma) - 0.5);
  return {mu, sigma, ll, 0, HnormStatus::Ok};
}
} // namespace cppunivariate
