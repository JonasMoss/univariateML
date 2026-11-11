// Inverse-Gamma distribution (extraDistr R-level parameterisation).
// Parameters: alpha > 0 (shape), beta > 0 (scale).
//   f(x)   = beta^alpha / Gamma(alpha) * x^(-alpha-1) * exp(-beta/x)
//   F(x)   = upper-tail of unit-scale Gamma(alpha) at beta/x
// Sample: 1/Y for Y ~ Gamma(shape=alpha, scale=1/beta).

#pragma once

#include "../../special/constants.hpp"
#include "../gamma/dpqr.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {


inline double dinvgamma(double x, double alpha, double beta, bool log) {
  if (!(alpha > 0.0 && beta > 0.0 && std::isfinite(alpha) && std::isfinite(beta)) || std::isnan(x)) {
    return special::kNaN;
  }
  if (x <= 0.0 || x == special::kPosInf) {
    return log ? special::kNegInf : 0.0;
  }
  const double log_density = special::gamma_log_density_positive(beta,alpha,x)
      +std::log(beta)-std::log(x);
  return log ? log_density : std::exp(log_density);
}

inline double pinvgamma(double q, double alpha, double beta, bool lower_tail,
                        bool log_p) {
  if (!(alpha>0 && std::isfinite(alpha) && beta > 0.0 && std::isfinite(beta)) || std::isnan(q)) return special::kNaN;
  if(q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if(q==special::kPosInf) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  return pgamma(beta,alpha,q,!lower_tail,log_p);
}

inline double qinvgamma(double p, double alpha, double beta, bool lower_tail,
                        bool log_p) {
  if (!(alpha>0 && std::isfinite(alpha) && beta > 0.0 && std::isfinite(beta))) return special::kNaN;
  const auto tails=special::quantile_log_tails(p,lower_tail,log_p);
  if(!tails.valid) return special::kNaN;
  if(special::gamma_mean_quantile_enclosed(alpha,tails)) return beta/alpha;
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
