// Laplace (double-exponential) distribution: scalar d/p/q/r.
// Parameters: location mu, scale sigma > 0.
//   f(x)   = (1/(2*sigma)) * exp(-|x-mu|/sigma)
//   F(x)   = (x < mu) ? 0.5*exp((x-mu)/sigma) : 1 - 0.5*exp(-(x-mu)/sigma)
//   F^-1(p)= mu + sigma * log(2p)         if p <= 0.5
//          = mu - sigma * log(2(1-p))     otherwise
// Sampler: inverse-CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dlaplace(double x, double mu, double sigma, bool log) {
  if (!(sigma>0 && std::isfinite(sigma) && std::isfinite(mu))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  const double a=std::fabs(special::scaled_difference(x,mu,sigma));
  const double ld=-a-std::log(sigma)-std::log(2.0);
  return log ? ld : std::exp(ld);
}

inline double plaplace(double q, double mu, double sigma, bool lower_tail,
                       bool log_p) {
  if (!(sigma>0 && std::isfinite(sigma) && std::isfinite(mu))) return special::kNaN;
  const double z=special::scaled_difference(q,mu,sigma);
  const double lp=-std::fabs(z)-std::log(2.0);
  return special::tail_probability(special::tails_from_log(lp,z<=0),lower_tail,log_p);
}

inline double qlaplace(double p, double mu, double sigma, bool lower_tail,
                       bool log_p) {
  if (!(sigma>0 && std::isfinite(sigma) && std::isfinite(mu))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double offset=special::probability_minus_half(p,lower_tail,log_p);
  if (std::fabs(offset)<1e-8) return std::fma(sigma,std::copysign(-std::log1p(-2*std::fabs(offset)),offset),mu);

  const double z=(t.lower<t.upper ? t.lower+std::log(2.0) : -t.upper-std::log(2.0));
  return std::fma(sigma,z,mu);
}

template <typename Rng>
inline double rlaplace(Rng &rng, double mu, double sigma) {
  const double u = rng.runif(0.0, 1.0);
  if (u < 0.5) {
    return mu + sigma * std::log(2.0 * u);
  }
  return mu - sigma * std::log(2.0 * (1.0 - u));
}

} // namespace distributions
} // namespace cppunivariate
