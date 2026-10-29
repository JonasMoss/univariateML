// Rayleigh distribution: scalar d/p/q/r.
// Parameter: scale sigma > 0; support x >= 0.
//   f(x)   = (x/sigma^2) * exp(-x^2/(2*sigma^2))
//   F(x)   = 1 - exp(-x^2/(2*sigma^2))
//   F^-1(p)= sigma * sqrt(-2 * log(1-p))
// (Equivalent to Weibull(shape=2, scale=sigma*sqrt(2)).)

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double drayleigh(double x, double sigma, bool log) {
  if (!(sigma>0 && std::isfinite(sigma))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  const double ld=std::log(x)-2*std::log(sigma)-std::exp(2*(std::log(x)-std::log(sigma))-std::log(2.0));
  return log ? ld : std::exp(ld);
}

inline double prayleigh(double q, double sigma, bool lower_tail, bool log_p) {
  if (!(sigma>0 && std::isfinite(sigma))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double lh = 2*(std::log(q)-std::log(sigma))-std::log(2.0);
  const auto t = lh < -36 ? special::tails_from_log(lh,true) : special::tails_from_log(-std::exp(lh),false);
  return special::tail_probability(t,lower_tail,log_p);
}

inline double qrayleigh(double p, double sigma, bool lower_tail, bool log_p) {
  if (!(sigma>0 && std::isfinite(sigma))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double lh=t.lower < -0.6931471805599453 ? special::log_neg_log1mexp(t.lower) : std::log(-t.upper);
  return std::exp(std::log(sigma)+(lh+std::log(2.0))/2);
}

template <typename Rng>
inline double rrayleigh(Rng &rng, double sigma) {
  const double u = rng.runif(0.0, 1.0);
  return sigma * std::sqrt(-2.0 * std::log(1.0 - u));
}

} // namespace distributions
} // namespace cppunivariate
