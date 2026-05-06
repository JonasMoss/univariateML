// Centralized special functions used by the ML core.
//
// The polygamma family (digamma, trigamma, tetragamma, psigamma) routes
// through src/ml/special/polygamma.cpp — the in-tree dpsifn port — so the
// standalone test harness can build without linking Rmath. dnorm/dpois/dgeom
// inline their formulas directly. lgamma/lbeta use std::lgamma.

#pragma once

#include "constants.hpp"
#include "polygamma.hpp"
#include <cmath>

namespace mlcore {
namespace special {

inline double digamma(double x) { return psigamma_deriv(x, 0); }

inline double trigamma(double x) { return psigamma_deriv(x, 1); }

inline double tetragamma(double x) { return psigamma_deriv(x, 2); }

inline double psigamma(double x, int deriv) { return psigamma_deriv(x, deriv); }

inline double lgamma(double x) { return std::lgamma(x); }

inline double lbeta(double a, double b) {
  if (!std::isfinite(a) || !std::isfinite(b)) {
    return kNaN;
  }
  return lgamma(a) + lgamma(b) - lgamma(a + b);
}

inline double dnorm(double x, double mean, double sd, bool log) {
  if (!std::isfinite(sd) || sd <= 0.0) {
    return kNaN;
  }
  const double z = (x - mean) / sd;
  const double log_pref = -std::log(sd) - 0.5 * std::log(2.0 * M_PI);
  const double log_density = log_pref - 0.5 * z * z;
  return log ? log_density : std::exp(log_density);
}

inline double dpois(double x, double lambda, bool log) {
  if (!std::isfinite(lambda) || lambda < 0.0) {
    return kNaN;
  }
  if (lambda == 0.0) {
    if (x == 0.0) {
      return log ? 0.0 : 1.0;
    }
    return log ? kNegInf : 0.0;
  }
  if (!std::isfinite(x) || x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double k = std::floor(x + 0.5);
  // Treat non-integers as zero probability.
  if (std::fabs(k - x) > 1e-7) {
    return log ? kNegInf : 0.0;
  }
  const double log_density =
      k * std::log(lambda) - lambda - std::lgamma(k + 1.0);
  return log ? log_density : std::exp(log_density);
}

inline double dgeom(double x, double prob, bool log) {
  if (prob <= 0.0 || prob > 1.0 || x < 0.0 || !std::isfinite(x)) {
    return kNaN;
  }
  const double k = std::floor(x + 0.5); // R's geometric is on 0,1,2,...
  const double log_density = k * std::log1p(-prob) + std::log(prob);
  return log ? log_density : std::exp(log_density);
}

} // namespace special
} // namespace mlcore
