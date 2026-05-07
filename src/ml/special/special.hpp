// Centralized special functions used by the ML core.
//
// Polygamma family routes through Boost.Math (boost::math::digamma /
// trigamma / polygamma); shipped via the BH package on CRAN. dnorm /
// dpois / dgeom inline their formulas; lgamma uses std::lgamma. This is
// the single seam between mlcore and the math backend — swap this file
// to retarget another backend.

#pragma once

#include "constants.hpp"
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/polygamma.hpp>
#include <boost/math/special_functions/trigamma.hpp>
#include <cmath>

namespace mlcore {
namespace special {

inline double digamma(double x) { return boost::math::digamma(x); }

inline double trigamma(double x) { return boost::math::trigamma(x); }

inline double tetragamma(double x) { return boost::math::polygamma(2, x); }

inline double psigamma(double x, int deriv) {
  if (deriv == 0) return boost::math::digamma(x);
  if (deriv == 1) return boost::math::trigamma(x);
  return boost::math::polygamma(deriv, x);
}

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
