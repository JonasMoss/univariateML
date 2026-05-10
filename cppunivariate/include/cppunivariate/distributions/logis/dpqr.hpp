// Logistic distribution: scalar d/p/q/r.
// Parameters: location, scale > 0; support x in R.
//   z       = (x - location) / scale
//   f(x)    = exp(-z) / (scale * (1 + exp(-z))^2)
//   F(x)    = 1 / (1 + exp(-z))
//   F^-1(p) = location + scale * log(p / (1-p))

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;

inline double dlogis(double x, double location, double scale, bool log) {
  if (!(scale > 0.0) || !std::isfinite(x) || !std::isfinite(location)) {
    return kNaN;
  }
  const double z = (x - location) / scale;
  // log f = -z - log(scale) - 2 * log(1 + exp(-z))
  //       = -z - log(scale) - 2 * log1pexp(-z)
  // softplus log1pexp(-z) avoids overflow for large |z|.
  const double abs_z = std::fabs(z);
  const double log1pe = abs_z + std::log1p(std::exp(-abs_z));
  // log1p(exp(-z)) = log1p(exp(|z|)) - |z| when z<0; for z>=0 it's
  // log1p(exp(-z)). The formula above (|z| + log1p(exp(-|z|))) gives
  // log(1+exp(|z|)); we need log(1+exp(-z)). Recompute properly:
  const double log1p_minus_z = (z >= 0.0)
      ? std::log1p(std::exp(-z))
      : -z + std::log1p(std::exp(z));
  (void)log1pe;
  const double log_density = -z - std::log(scale) - 2.0 * log1p_minus_z;
  return log ? log_density : std::exp(log_density);
}

inline double plogis(double q, double location, double scale, bool lower_tail,
                     bool log_p) {
  return cppunivariate::special::plogis(q, location, scale, lower_tail, log_p);
}

inline double qlogis(double p, double location, double scale, bool lower_tail,
                     bool log_p) {
  return cppunivariate::special::qlogis(p, location, scale, lower_tail, log_p);
}

template <typename Rng>
inline double rlogis(Rng &rng, double location, double scale) {
  // Inverse-CDF: location + scale * log(u / (1 - u)), u ~ U(0, 1).
  const double u = rng.runif(0.0, 1.0);
  return location + scale * (std::log(u) - std::log1p(-u));
}

}  // namespace distributions
}  // namespace cppunivariate
