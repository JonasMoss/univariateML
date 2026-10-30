// Inverse transformed gamma d/p/q/r, actuar parameterisation:
//   f(x) = shape2 * u^shape1 * exp(-u) / (x * Gamma(shape1)), u = (scale/x)^shape2.
#pragma once

#include "../trgamma/dpqr.hpp"

#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions {

inline double dinvtrgamma(double x, double shape1, double shape2, double scale, bool log) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(x)) return x;
  if (!(x > 0.0) || !std::isfinite(x)) return log ? -std::numeric_limits<double>::infinity() : 0.0;
  const double logu = shape2 * (std::log(scale) - std::log(x));
  const double z = logu - std::log(shape1);
  const double ld = std::log(shape2) + 0.5 * std::log(shape1) -
      trgamma_detail::kHalfLog2Pi - trgamma_detail::stirling_remainder(shape1) -
      shape1 * trgamma_detail::expm1_minus(z) - std::log(x);
  return log ? ld : std::exp(ld);
}

inline double pinvtrgamma(double q, double shape1, double shape2, double scale,
                          bool lower_tail, bool log_p) {
  if (!trgamma_valid(shape1,shape2,scale) || std::isnan(q)) return special::kNaN;
  const double lu=q<=0.0 ? special::kPosInf : shape2*(std::log(scale)-std::log(q));
  return special::tail_probability(special::gamma_log_tails(lu,shape1),!lower_tail,log_p);
}

inline double qinvtrgamma(double p, double shape1, double shape2, double scale,
                          bool lower_tail, bool log_p) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  const double logg = trgamma_gamma_logquantile(p, shape1, !lower_tail, log_p);
  return std::exp(std::log(scale) - logg / shape2);
}

template <typename Rng>
inline double rinvtrgamma(Rng &rng, double shape1, double shape2, double scale) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (shape1 < 0.01) return qinvtrgamma(rng.runif(0.0, 1.0), shape1, shape2, scale, true, false);
  return std::exp(std::log(scale) - std::log(rng.rgamma(shape1, 1.0)) / shape2);
}

}  // namespace distributions
}  // namespace cppunivariate
