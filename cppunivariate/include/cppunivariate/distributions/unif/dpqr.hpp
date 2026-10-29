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
inline double dunif(double x, double min_, double max_, bool log) {
  if (!(std::isfinite(min_) && std::isfinite(max_) && max_>min_)) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  const double ld=(x<min_ || x>max_) ? special::kNegInf : -unif_log_distance(max_,min_);
  return log ? ld : std::exp(ld);
}

inline double punif(double q, double min_, double max_, bool lower_tail,
                    bool log_p) {
  if (!(std::isfinite(min_) && std::isfinite(max_) && max_>min_)) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=min_) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if (q>=max_) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  const double ld=unif_log_distance(max_,min_);
  return special::tail_probability({unif_log_distance(q,min_)-ld,unif_log_distance(max_,q)-ld,true},lower_tail,log_p);
}

inline double qunif(double p, double min_, double max_, bool lower_tail,
                    bool log_p) {
  if (!(std::isfinite(min_) && std::isfinite(max_) && max_>min_)) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (!std::isfinite(max_-min_))
    return min_*std::exp(t.upper)+max_*std::exp(t.lower);
  const double lw=unif_log_distance(max_,min_);
  return t.lower<t.upper ? min_+std::exp(t.lower+lw) : max_-std::exp(t.upper+lw);
}

template <typename Rng>
inline double runif_(Rng &rng, double min_, double max_) {
  return rng.runif(min_, max_);
}

}  // namespace distributions
}  // namespace cppunivariate
