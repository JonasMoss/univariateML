// Transformed gamma d/p/q/r, actuar parameterisation:
//   f(x) = shape2 * u^shape1 * exp(-u) / (x * Gamma(shape1)), u = (x/scale)^shape2.
// Quantiles and draws are formed on the log scale, so scale * G^(1/shape2)
// does not overflow when G^(1/shape2) alone would.
#pragma once

#include "mle.hpp"

#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions {

inline bool trgamma_valid(double shape1, double shape2, double scale) {
  return shape1 > 0.0 && shape2 > 0.0 && scale > 0.0 && std::isfinite(shape1) &&
         std::isfinite(shape2) && std::isfinite(scale);
}

// Shared unit-gamma helpers retain logarithmic arguments below underflow.
inline double trgamma_gamma_logcdf(double logu, double a) {
  return special::gamma_log_tails(logu,a).lower;
}

inline double trgamma_gamma_logquantile(double p, double a, bool lower_tail, bool log_p) {
  return special::gamma_log_quantile(p,a,lower_tail,log_p);
}

inline double dtrgamma(double x, double shape1, double shape2, double scale, bool log) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(x)) return x;
  if (!(x >= 0.0) || !std::isfinite(x)) return log ? -std::numeric_limits<double>::infinity() : 0.0;
  if (x == 0.0) {
    const double power = shape1 * shape2;
    if (power < 1.0) return std::numeric_limits<double>::infinity();
    if (power > 1.0) return log ? -std::numeric_limits<double>::infinity() : 0.0;
    const double ld = std::log(shape2) - std::log(scale) - std::lgamma(shape1);
    return log ? ld : std::exp(ld);
  }
  const double logu = shape2 * (std::log(x) - std::log(scale));
  const double z = logu - std::log(shape1);
  const double ld = std::log(shape2) + 0.5 * std::log(shape1) -
      trgamma_detail::kHalfLog2Pi - trgamma_detail::stirling_remainder(shape1) -
      shape1 * trgamma_detail::expm1_minus(z) - std::log(x);
  return log ? ld : std::exp(ld);
}

inline double ptrgamma(double q, double shape1, double shape2, double scale, bool lower_tail,
                       bool log_p) {
  if (!trgamma_valid(shape1,shape2,scale) || std::isnan(q)) return special::kNaN;
  const double lu=q<=0.0 ? special::kNegInf : shape2*(std::log(q)-std::log(scale));
  return special::tail_probability(special::gamma_log_tails(lu,shape1),lower_tail,log_p);
}

inline double qtrgamma(double p, double shape1, double shape2, double scale, bool lower_tail,
                       bool log_p) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  const double logg = trgamma_gamma_logquantile(p, shape1, lower_tail, log_p);
  return std::exp(std::log(scale) + logg / shape2);
}

template <typename Rng>
inline double rtrgamma(Rng &rng, double shape1, double shape2, double scale) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (shape1 < 0.01) return qtrgamma(rng.runif(0.0, 1.0), shape1, shape2, scale, true, false);
  return std::exp(std::log(scale) + std::log(rng.rgamma(shape1, 1.0)) / shape2);
}

}  // namespace distributions
}  // namespace cppunivariate
