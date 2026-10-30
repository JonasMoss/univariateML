// Inverse-Gamma distribution (extraDistr R-level parameterisation).
// Parameters: alpha > 0 (shape), beta > 0 (scale).
//   f(x)   = beta^alpha / Gamma(alpha) * x^(-alpha-1) * exp(-beta/x)
//   F(x)   = upper-tail of unit-scale Gamma(alpha) at beta/x
// Sample: 1/Y for Y ~ Gamma(shape=alpha, scale=1/beta).

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dinvgamma(double x, double alpha, double beta, bool log) {
  if (!(alpha > 0.0 && beta > 0.0) || std::isnan(x)) {
    return kNaN;
  }
  if (x <= 0.0 || x == special::kPosInf) {
    return log ? kNegInf : 0.0;
  }
  const double log_density = alpha * std::log(beta) - std::lgamma(alpha) +
                             (-alpha - 1.0) * std::log(x) - beta / x;
  return log ? log_density : std::exp(log_density);
}

inline double pinvgamma(double q, double alpha, double beta, bool lower_tail,
                        bool log_p) {
  if (!(beta > 0.0 && std::isfinite(beta)) || std::isnan(q)) return special::kNaN;
  const double lx=q<=0.0 ? special::kPosInf : std::log(beta)-std::log(q);
  return special::tail_probability(special::gamma_log_tails(lx,alpha),!lower_tail,log_p);
}

inline double qinvgamma(double p, double alpha, double beta, bool lower_tail,
                        bool log_p) {
  if (!(beta > 0.0 && std::isfinite(beta))) return special::kNaN;
  return std::exp(std::log(beta)-special::gamma_log_quantile(p,alpha,!lower_tail,log_p));
}

template <typename Rng>
inline double rinvgamma(Rng &rng, double alpha, double beta) {
  // X ~ InvGamma(shape=alpha, scale=beta) <=> 1/X ~ Gamma(shape=alpha,
  // scale=1/beta).
  return 1.0 / rng.rgamma(alpha, 1.0 / beta);
}

} // namespace distributions
} // namespace cppunivariate
