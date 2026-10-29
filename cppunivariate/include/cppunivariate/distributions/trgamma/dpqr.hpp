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

// Below this anchor, omitted gamma-CDF correction terms are < 1e-304.
inline double trgamma_gamma_logcdf(double logu, double a) {
  if (logu < -700.0)
    return special::pgamma(std::exp(-700.0), a, 1.0, true, true) + a * (logu + 700.0);
  return special::pgamma(std::exp(logu), a, 1.0, true, true);
}

inline double trgamma_gamma_logquantile(double p, double a, bool lower_tail, bool log_p) {
  const double g = special::qgamma(p, a, 1.0, lower_tail, log_p);
  if (!(g < std::exp(-700.0))) return std::log(g);
  const double lp = log_p ? (lower_tail ? p : std::log(-std::expm1(p)))
                         : (lower_tail ? std::log(p) : std::log1p(-p));
  return -700.0 + (lp - trgamma_gamma_logcdf(-700.0, a)) / a;
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
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (q <= 0.0) {
    const double p = lower_tail ? 0.0 : 1.0;
    return log_p ? std::log(p) : p;
  }
  const double logu = shape2 * (std::log(q) - std::log(scale));
  if (!(logu < -700.0)) return special::pgamma(std::exp(logu), shape1, 1.0, lower_tail, log_p);
  const double lp = trgamma_gamma_logcdf(logu, shape1);
  if (lower_tail) return log_p ? lp : std::exp(lp);
  return log_p ? std::log(-std::expm1(lp)) : -std::expm1(lp);
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
