#pragma once
#include "../../special/tails.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline bool triang_valid(double a,double b,double c) {
  return std::isfinite(a)&&std::isfinite(b)&&std::isfinite(c)&&a<b&&a<=c&&c<=b;
}
inline double dtriang(double x,double a,double b,double c,bool log=false) {
  if(!triang_valid(a,b,c)||std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double v=-INFINITY;
  if(x>=a&&x<=b) {
    v=std::log(2.0)-std::log(b-a);
    if(x<c) v+=std::log(x-a)-std::log(c-a);
    if(x>c) v+=std::log(b-x)-std::log(b-c);
  }
  return log?v:std::exp(v);
}
inline double ptriang(double x,double a,double b,double c,bool lower_tail=true,bool log_p=false) {
  if (!(triang_valid(a,b,c))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<=a) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if (x>=b) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  const double lw=std::log(b-a);
  double ll,lu;
  if (x<=c) {
    ll=2*std::log(x-a)-lw-std::log(c-a);
    lu=special::log_sum_exp(std::log(c-x)-lw+std::log1p((x-a)/(c-a)),std::log(b-c)-lw);
  } else {
    lu=2*std::log(b-x)-lw-std::log(b-c);
    ll=special::log_sum_exp(std::log(x-c)-lw+std::log1p((b-x)/(b-c)),std::log(c-a)-lw);
  }
  return special::tail_probability(ll<lu ? special::tails_from_log(ll,true) : special::tails_from_log(lu,false),lower_tail,log_p);
}
inline double qtriang(double p,double a,double b,double c,bool lower_tail=true,bool log_p=false) {
  if (!(triang_valid(a,b,c))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower==special::kNegInf) return a;
  if (t.upper==special::kNegInf) return b;
  const double lw=std::log(b-a);
  return t.lower<=std::log(c-a)-lw ? a+std::exp((t.lower+lw+std::log(c-a))/2) : b-std::exp((t.upper+lw+std::log(b-c))/2);
}
// Preserve the original inverse transform for seeded sampler draws.
inline double triang_sampling_quantile(double p,double a,double b,double c,bool lower_tail=true,bool log_p=false) {
  if(log_p) p=std::exp(p);
  if(!triang_valid(a,b,c)||!(p>=0&&p<=1)) return std::numeric_limits<double>::quiet_NaN();
  if(!lower_tail) p=1-p;
  const double t=(c-a)/(b-a);
  return p<=t?a+(b-a)*std::sqrt(p*t):b-(b-a)*std::sqrt((1-p)*(1-t));
}
template<class Rng> inline double rtriang(Rng& rng,double a,double b,double c) {
  return triang_sampling_quantile(rng.runif(0,1),a,b,c);
}
} // namespace distributions
} // namespace cppunivariate
