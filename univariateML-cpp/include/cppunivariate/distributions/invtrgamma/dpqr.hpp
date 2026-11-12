// Inverse transformed gamma d/p/q/r, actuar parameterisation:
//   f(x) = shape2 * u^shape1 * exp(-u) / (x * Gamma(shape1)), u = (scale/x)^shape2.
#pragma once
#include "../invgamma/dpqr.hpp"

#include "../trgamma/dpqr.hpp"

#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions {

inline double dinvtrgamma(double x, double shape1, double shape2, double scale, bool log) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(x)) return x;
  if (!(x > 0.0) || !std::isfinite(x)) return log ? -std::numeric_limits<double>::infinity() : 0.0;
  const double ld=trgamma_power_logdensity(x,special::log_positive_ratio(scale,x),shape1,shape2);
  return log ? ld : std::exp(ld);
}

inline double pinvtrgamma(double q, double shape1, double shape2, double scale,
                          bool lower_tail, bool log_p) {
  if (!trgamma_valid(shape1,shape2,scale) || std::isnan(q)) return special::kNaN;
  if (shape2==1) return pinvgamma(q,shape1,scale,lower_tail,log_p);
  const auto tails=q<=0?special::tails_from_log(special::kNegInf,false):
    trgamma_power_tails(special::log_positive_ratio(scale,q),shape1,shape2);
  return special::tail_probability(tails,!lower_tail,log_p);
}

inline double qinvtrgamma(double p, double shape1, double shape2, double scale,
                          bool lower_tail, bool log_p) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  const double displacement=trgamma_power_logquantile(p,shape1,shape2,!lower_tail,log_p);
  return std::exp(std::log(scale)-displacement);
}

template <typename Rng>
inline double rinvtrgamma(Rng &rng, double shape1, double shape2, double scale) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (shape1 < 0.01) return qinvtrgamma(rng.runif(0.0, 1.0), shape1, shape2, scale, true, false);
  return std::exp(std::log(scale) - std::log(rng.rgamma(shape1, 1.0)) / shape2);
}

}  // namespace distributions
}  // namespace cppunivariate
