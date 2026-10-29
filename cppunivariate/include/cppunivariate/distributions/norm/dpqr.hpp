// Normal distribution: scalar d/p/q/r.
// Parameters: mean, sd > 0; support x in R.
//
// Compute the smaller normal tail in log space and invert that log tail.
// Sampling uses the Rng concept's rnorm() primitive.

#pragma once

#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dnorm(double x, double mean, double sd, bool log) {
  if (!(sd > 0.0 && std::isfinite(sd) && std::isfinite(mean))) return special::kNaN;
  const double z = special::scaled_difference(x,mean,sd);
  const double ld = -(0.5*z)*z - std::log(sd) - 0.91893853320467274178;
  return log ? ld : std::exp(ld);
}

inline double pnorm(double q, double mean, double sd, bool lower_tail,
                    bool log_p) {
  if (!(sd > 0.0 && std::isfinite(sd) && std::isfinite(mean))) return special::kNaN;
  const double z = special::scaled_difference(q,mean,sd);
  return special::tail_probability(special::tails_from_log(special::normal_log_upper(std::abs(z)),z <= 0.0),lower_tail,log_p);
}

inline double qnorm(double p, double mean, double sd, bool lower_tail,
                    bool log_p) {
  if (!(sd > 0.0 && std::isfinite(sd) && std::isfinite(mean))) return special::kNaN;
  return std::fma(sd,special::normal_quantile(p,lower_tail,log_p),mean);
}

template <typename Rng>
inline double rnorm(Rng &rng, double mean, double sd) {
  return mean + sd * rng.rnorm();
}

}  // namespace distributions
}  // namespace cppunivariate
