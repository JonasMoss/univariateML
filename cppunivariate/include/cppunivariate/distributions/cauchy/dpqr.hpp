// Cauchy distribution: scalar d/p/q/r.
// Parameters: location, scale > 0; support x in R.
//   f(x)    = 1 / (pi * scale * (1 + ((x-location)/scale)^2))
//   F(x)    = 0.5 + atan((x-location)/scale) / pi
//   F^-1(p) = location + scale * tan(pi * (p - 0.5))

#pragma once

#include "../../special/constants.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dcauchy(double x, double location, double scale, bool log) {
  if (!(scale > 0.0) || !std::isfinite(x) || !std::isfinite(location)) {
    return kNaN;
  }
  const double z = (x - location) / scale;
  // log f = -log(pi) - log(scale) - log1p(z^2)
  const double log_density = -std::log(M_PI) - std::log(scale) -
                             std::log1p(z * z);
  return log ? log_density : std::exp(log_density);
}

inline double pcauchy(double q, double location, double scale, bool lower_tail,
                      bool log_p) {
  if (!(scale > 0.0) || !std::isfinite(location)) {
    return kNaN;
  }
  // 0.5 + atan(z)/pi handles +/-inf cleanly via atan saturating at +/- pi/2.
  const double z = (q - location) / scale;
  double prob = 0.5 + std::atan(z) / M_PI;
  if (!lower_tail) prob = 1.0 - prob;
  return log_p ? std::log(prob) : prob;
}

inline double qcauchy(double p, double location, double scale, bool lower_tail,
                      bool log_p) {
  if (!(scale > 0.0) || !std::isfinite(location)) {
    return kNaN;
  }
  if (log_p) p = std::exp(p);
  if (!lower_tail) p = 1.0 - p;
  if (p == 0.0) return -std::numeric_limits<double>::infinity();
  if (p == 1.0) return std::numeric_limits<double>::infinity();
  return location + scale * std::tan(M_PI * (p - 0.5));
}

template <typename Rng>
inline double rcauchy(Rng &rng, double location, double scale) {
  // Inverse-CDF; matches qcauchy at U ~ U(0, 1).
  const double u = rng.runif(0.0, 1.0);
  return location + scale * std::tan(M_PI * (u - 0.5));
}

}  // namespace distributions
}  // namespace cppunivariate
