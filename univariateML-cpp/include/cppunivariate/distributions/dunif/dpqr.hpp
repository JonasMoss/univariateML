// Discrete uniform distribution on integers {min, min+1, ..., max}.
// Parameters: min, max integers with min <= max.
//   p(x)   = 1/(max-min+1) for integer x in [min, max], 0 otherwise.
//   F(x)   = (floor(x) - min + 1) / (max - min + 1) on [min, max].
//   F^-1(p)= ceil(p*(max-min+1) + min - 1) for p in (0, 1].
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once
#include "../../special/tails.hpp"

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

namespace detail {
inline bool dunif_valid_params(double min, double max) {
  return std::isfinite(min) && std::isfinite(max) && min <= max &&
         min == std::floor(min) && max == std::floor(max);
}
inline double dunif_log_count(double min,double max) {
  const double n=max-min+1;
  return std::isfinite(n)?std::log(n):std::log(max*0.5-min*0.5+0.5)+0.69314718055994530942;
}
} // namespace detail

inline double ddunif(double x, double min, double max, bool log) {
  if (!detail::dunif_valid_params(min, max) || std::isnan(x)) {
    return kNaN;
  }
  if (x < min || x > max || x != std::floor(x)) {
    return log ? kNegInf : 0.0;
  }
  const double lp=-detail::dunif_log_count(min,max);
  return log ? lp : std::exp(lp);
}

inline double pdunif(double q,double min,double max,bool lower_tail,bool log_p) {
  if(!detail::dunif_valid_params(min,max)||std::isnan(q))return special::kNaN;
  if(q<min)return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if(q>=max)return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  const double k=std::floor(q);
  const double lo=detail::dunif_log_count(min,k);
  const double up=detail::dunif_log_count(k+1,max);
  const bool lower=lo<up;
  const double lp=(lower?lo:up)-detail::dunif_log_count(min,max);
  return special::tail_probability(special::tails_from_log(lp,lower),lower_tail,log_p);
}

inline double qdunif(double p,double min,double max,bool lower_tail,bool log_p) {
  if(!detail::dunif_valid_params(min,max))return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),min,max,
      [=](double x){return special::LogTails{pdunif(x,min,max,true,true),pdunif(x,min,max,false,true),true};});
}

template <typename Rng>
inline double rdunif(Rng &rng, double min, double max) {
  if (!detail::dunif_valid_params(min,max)) return special::kNaN;
  if (min==max) return min;
  const double u = rng.runif(0.0, 1.0);
  return std::ceil(u * (max - min + 1.0) + min - 1.0);
}

} // namespace distributions
} // namespace cppunivariate
