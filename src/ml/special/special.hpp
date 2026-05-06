// Centralized special functions used by the ML core.
// Currently thin wrappers around Rmath; replace implementations here to
// decouple from R.

#pragma once

#include "constants.hpp"
#include <Rmath.h>
#include <cmath>

// Rmath.h aliases dozens of common identifiers (beta, gamma, dnorm, lbeta,
// digamma, ...) to their Rf_* equivalents via #define, polluting the global
// namespace. Undef the ones we either call here directly or that collide
// with field/variable names elsewhere in the project. New estimator
// headers downstream can then use names like `beta` freely.
#undef digamma
#undef trigamma
#undef tetragamma
#undef psigamma
#undef lgamma
#undef lbeta
#undef dnorm
#undef dpois
#undef dgeom
#undef beta
#undef gamma

namespace mlcore {
namespace special {

inline double digamma(double x) { return Rf_digamma(x); }

inline double trigamma(double x) { return Rf_psigamma(x, 1); }

inline double tetragamma(double x) { return Rf_psigamma(x, 2); }

inline double psigamma(double x, int deriv) { return Rf_psigamma(x, deriv); }

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
