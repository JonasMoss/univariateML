// Gamma distribution: scalar d/p/q/r.
// Parameters: shape > 0, scale > 0 (README order).
// Support: x >= 0.
//
// Regularized incomplete gamma tails, with logarithmic continuations
// and inversion in log coordinates.

#pragma once

#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dgamma(double x, double shape, double scale, bool log) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (!(shape > 0.0 && std::isfinite(shape)) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    if (shape < 1.0) return special::kPosInf;
    if (shape > 1.0) return log ? special::kNegInf : 0.0;
    return log ? log_rate : rate;
  }
  const double lx=std::log(x), lr=log_rate;
  const double ld=shape*lr+(shape-1.0)*lx-(std::isfinite(rate) ? x*rate : x/scale)-std::lgamma(shape);
  return log ? ld : std::exp(ld);
}

inline double pgamma(double q, double shape, double scale, bool lower_tail,
                     bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (std::isnan(q)) return special::kNaN;
  const double lx=q<=0.0 ? special::kNegInf : std::log(q)+log_rate;
  return special::tail_probability(special::gamma_log_tails(lx,shape),lower_tail,log_p);
}

inline double qgamma(double p, double shape, double scale, bool lower_tail,
                     bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  return std::exp(special::gamma_log_quantile(p,shape,lower_tail,log_p)-log_rate);
}

template <typename Rng>
inline double rgamma(Rng &rng, double shape, double scale) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape > 0.0 && std::isfinite(shape))) return special::kNaN;
  return rng.rgamma(shape, scale);
}

}  // namespace distributions
}  // namespace cppunivariate
