// Uniform distribution on [min, max].
//   f(x)    = 1/(max-min)              for x in [min, max]
//   F(x)    = (x - min)/(max - min)    for x in [min, max]
//   F^-1(p) = min + p * (max - min)
// R semantics: density at endpoints is 1/(max-min) (closed interval).

#pragma once

#include "../../special/tails.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double unif_log_distance(double hi,double lo) {
  const double d=hi-lo;
  return std::isfinite(d) ? std::log(d) : std::log(hi/2-lo/2)+std::log(2.0);
}
inline double dunif(double x, double min, double max, bool log) {
  if (!(std::isfinite(min) && std::isfinite(max) && max>min)) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  const double ld=(x<min || x>max) ? special::kNegInf : -unif_log_distance(max,min);
  return log ? ld : std::exp(ld);
}

inline double punif(double q, double min, double max, bool lower_tail,
                    bool log_p) {
  if (!(std::isfinite(min) && std::isfinite(max) && max>min)) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=min) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if (q>=max) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  const double ld=unif_log_distance(max,min);
  return special::tail_probability({unif_log_distance(q,min)-ld,unif_log_distance(max,q)-ld,true},lower_tail,log_p);
}

inline double qunif(double p, double min, double max, bool lower_tail,
                    bool log_p) {
  if (!(std::isfinite(min) && std::isfinite(max) && max>min)) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (!std::isfinite(max-min))
    return min*std::exp(t.upper)+max*std::exp(t.lower);
  const double lw=unif_log_distance(max,min);
  return t.lower<t.upper ? min+std::exp(t.lower+lw) : max-std::exp(t.upper+lw);
}

template <typename Rng>
inline double runif(Rng &rng, double min, double max) {
  return rng.runif(min, max);
}

}  // namespace distributions
}  // namespace cppunivariate
