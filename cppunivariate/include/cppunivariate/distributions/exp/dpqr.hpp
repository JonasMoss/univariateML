// Exponential distribution (rate parameterisation).
//
// Single parameter: rate > 0. Support: x >= 0.
//   f(x; rate)      = rate * exp(-rate * x)
//   F(x; rate)      = 1 - exp(-rate * x)
//   F^{-1}(p; rate) = -log1p(-p) / rate
//
// Sampler: inverse CDF on U ~ Uniform(0, 1). Matches the same
// inverse-CDF pattern as the other lightweight distributions in
// cppunivariate.

#pragma once

#include "../../special/tails.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dexp(double x, double rate, bool log) {
  if (!(rate>0 && std::isfinite(rate))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  const double ld=std::log(rate)-rate*x;
  return log ? ld : std::exp(ld);
}

inline double pexp(double q, double rate, bool lower_tail, bool log_p) {
  if (!(rate>0 && std::isfinite(rate))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double lh = std::log(rate)+std::log(q);
  const auto t = lh < -36 ? special::tails_from_log(lh,true) : special::tails_from_log(-std::exp(lh),false);
  return special::tail_probability(t,lower_tail,log_p);
}

inline double qexp(double p, double rate, bool lower_tail, bool log_p) {
  if (!(rate>0 && std::isfinite(rate))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double lh=t.lower < -0.6931471805599453 ? special::log_neg_log1mexp(t.lower) : std::log(-t.upper);
  return std::exp(lh-std::log(rate));
}

template <typename Rng>
inline double rexp(Rng &rng, double rate) {
  const double u = rng.runif(0.0, 1.0);
  return -std::log1p(-u) / rate;
}

}  // namespace distributions
}  // namespace cppunivariate
