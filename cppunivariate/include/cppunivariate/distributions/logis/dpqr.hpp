// Logistic distribution: scalar d/p/q/r.
// Parameters: location, scale > 0; support x in R.
//   z       = (x - location) / scale
//   f(x)    = exp(-z) / (scale * (1 + exp(-z))^2)
//   F(x)    = 1 / (1 + exp(-z))
//   F^-1(p) = location + scale * log(p / (1-p))

#pragma once

#include "../../special/tails.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dlogis(double x, double location, double scale, bool log) {
  if (!(scale>0 && std::isfinite(scale) && std::isfinite(location))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  const double a=std::fabs(special::scaled_difference(x,location,scale));
  const double ld=-a-std::log(scale)-2*std::log1p(std::exp(-a));
  return log ? ld : std::exp(ld);
}

inline double plogis(double q, double location, double scale, bool lower_tail,
                     bool log_p) {
  if (!(scale>0 && std::isfinite(scale) && std::isfinite(location))) return special::kNaN;
  const double z=special::scaled_difference(q,location,scale);
  const double lp=-std::fabs(z)-std::log1p(std::exp(-std::fabs(z)));
  return special::tail_probability(special::tails_from_log(lp,z<=0),lower_tail,log_p);
}

inline double qlogis(double p, double location, double scale, bool lower_tail,
                     bool log_p) {
  if (!(scale>0 && std::isfinite(scale) && std::isfinite(location))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double offset=special::probability_minus_half(p,lower_tail,log_p);
  if (std::fabs(offset)<1e-8) return std::fma(scale,std::log1p(2*offset)-std::log1p(-2*offset),location);

  const double z=t.lower-t.upper;
  return std::fma(scale,z,location);
}

template <typename Rng>
inline double rlogis(Rng &rng, double location, double scale) {
  // Inverse-CDF: location + scale * log(u / (1 - u)), u ~ U(0, 1).
  const double u = rng.runif(0.0, 1.0);
  return location + scale * (std::log(u) - std::log1p(-u));
}

}  // namespace distributions
}  // namespace cppunivariate
