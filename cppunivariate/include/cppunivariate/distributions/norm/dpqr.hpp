// Normal distribution: scalar d/p/q/r.
// Parameters: mean, sd > 0; support x in R.
//
// d/p/q delegate to cppunivariate::special::* (Boost.Math-backed).
// Sampling uses the Rng concept's rnorm() primitive.

#pragma once

#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dnorm(double x, double mean, double sd, bool log) {
  return cppunivariate::special::dnorm(x, mean, sd, log);
}

inline double pnorm(double q, double mean, double sd, bool lower_tail,
                    bool log_p) {
  return cppunivariate::special::pnorm(q, mean, sd, lower_tail, log_p);
}

inline double qnorm(double p, double mean, double sd, bool lower_tail,
                    bool log_p) {
  return cppunivariate::special::qnorm(p, mean, sd, lower_tail, log_p);
}

template <typename Rng>
inline double rnorm(Rng &rng, double mean, double sd) {
  return mean + sd * rng.rnorm();
}

}  // namespace distributions
}  // namespace cppunivariate
