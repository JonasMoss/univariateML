// Gamma distribution: scalar d/p/q/r.
// Parameters: shape > 0, rate > 0 (R's stats::dgamma rate parameterisation).
// Support: x >= 0.
//
// d/p/q delegate to cppunivariate::special::* (Boost.Math-backed).
// special::dgamma takes the (shape, scale) form, so rate is converted to
// scale = 1/rate at the seam.

#pragma once

#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dgamma(double x, double shape, double rate, bool log) {
  return cppunivariate::special::dgamma(x, shape, 1.0 / rate, log);
}

inline double pgamma(double q, double shape, double rate, bool lower_tail,
                     bool log_p) {
  return cppunivariate::special::pgamma(q, shape, 1.0 / rate, lower_tail, log_p);
}

inline double qgamma(double p, double shape, double rate, bool lower_tail,
                     bool log_p) {
  return cppunivariate::special::qgamma(p, shape, 1.0 / rate, lower_tail, log_p);
}

template <typename Rng>
inline double rgamma(Rng &rng, double shape, double rate) {
  // Rng concept's rgamma takes (shape, scale); convert from rate.
  return rng.rgamma(shape, 1.0 / rate);
}

}  // namespace distributions
}  // namespace cppunivariate
