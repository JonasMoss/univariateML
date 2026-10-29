// Gamma distribution: scalar d/p/q/r.
// Parameters: shape > 0, rate > 0 (R's stats::dgamma rate parameterisation).
// Support: x >= 0.
//
// Regularized incomplete gamma tails, with logarithmic continuations
// and inversion in log coordinates; retain the rate without reciprocating.

#pragma once

#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dgamma(double x, double shape, double rate, bool log) {
  if (!(shape > 0.0 && rate > 0.0 && std::isfinite(shape) && std::isfinite(rate)) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    if (shape < 1.0) return special::kPosInf;
    if (shape > 1.0) return log ? special::kNegInf : 0.0;
    return log ? std::log(rate) : rate;
  }
  const double lx=std::log(x), lr=std::log(rate);
  const double ld=shape*lr+(shape-1.0)*lx-x*rate-std::lgamma(shape);
  return log ? ld : std::exp(ld);
}

inline double pgamma(double q, double shape, double rate, bool lower_tail,
                     bool log_p) {
  if (!(rate > 0.0 && std::isfinite(rate)) || std::isnan(q)) return special::kNaN;
  const double lx=q<=0.0 ? special::kNegInf : std::log(q)+std::log(rate);
  return special::tail_probability(special::gamma_log_tails(lx,shape),lower_tail,log_p);
}

inline double qgamma(double p, double shape, double rate, bool lower_tail,
                     bool log_p) {
  if (!(rate > 0.0 && std::isfinite(rate))) return special::kNaN;
  return std::exp(special::gamma_log_quantile(p,shape,lower_tail,log_p)-std::log(rate));
}

template <typename Rng>
inline double rgamma(Rng &rng, double shape, double rate) {
  // Rng concept's rgamma takes (shape, scale); convert from rate.
  return rng.rgamma(shape, 1.0 / rate);
}

}  // namespace distributions
}  // namespace cppunivariate
