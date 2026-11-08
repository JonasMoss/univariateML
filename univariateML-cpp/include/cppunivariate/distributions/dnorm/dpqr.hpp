#pragma once
#include "../../special/special.hpp"
#include "../../special/tails.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
// Log probability of a standard normal interval. Subtract on the small-tail side.
inline double dnorm_interval(double a, double b) {
  if (!(a < b)) return -std::numeric_limits<double>::infinity();
  const double mid = a + (b-a)/2, w = b-a;
  if (w*(1+std::abs(mid)) < 1e-5)
    return -0.5*mid*mid - 0.91893853320467274178 + std::log(w)
           + std::log1p(((mid*w)*(mid*w)-w*w)/24);
  const bool lower = a+b < 0;
  const double hi = special::fs_normal_p(lower ? b : a, lower, true);
  const double lo = special::fs_normal_p(lower ? a : b, lower, true);
  return hi + special::fs_log1mexp(lo-hi);
}
inline double ddnorm(double x, double mean, double sd, bool log = false) {
  if (!std::isfinite(mean) || !std::isfinite(sd) || sd <= 0 || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  double lp = -std::numeric_limits<double>::infinity();
  if (std::isfinite(x) && x == std::floor(x)) {
    const double mid=(x+0.5-mean)/sd, w=1/sd;
    if (w*(1+std::abs(mid))<1e-5)
      lp=-0.5*mid*mid-0.91893853320467274178-std::log(sd)+std::log1p(((mid*w)*(mid*w)-w*w)/24);
    else if (std::abs(mid)>1e16) {
      const double near=std::abs(mid)-0.5*w;
      lp=special::fs_normal_p(near,false,true)+special::log1mexp(-near*w-0.5*w*w);
    } else lp=dnorm_interval((x-mean)/sd,(x+1-mean)/sd);
  }
  return log ? lp : std::exp(lp);
}
inline double pdnorm(double q, double mean, double sd, bool lower_tail = true, bool log_p = false) {
  if (!std::isfinite(mean) || !std::isfinite(sd) || sd <= 0 || std::isnan(q))
    return std::numeric_limits<double>::quiet_NaN();
  return special::fs_normal_p((std::floor(q)+1-mean)/sd,lower_tail,log_p);
}
inline double qdnorm(double p, double mean, double sd, bool lower_tail = true, bool log_p = false) {
  const double lp = log_p && lower_tail && p==-0.69314718055994530942 ? std::nextafter(p,0.0) : (log_p ? p : std::log(p));
  if (lp > 0 || (!log_p && p < 0) || !std::isfinite(mean) || !(sd > 0) || !std::isfinite(sd))
    return std::numeric_limits<double>::quiet_NaN();
  const double z = mean + sd*special::fs_normal_q(lp,lower_tail);
  if (!std::isfinite(z)) return z;
  double q=std::ceil(z)-1;
  for (int i=0;i<2;++i) {
    const double at=pdnorm(q,mean,sd,lower_tail,true);
    if (lower_tail ? at<lp : at>lp) q+=1;
    else break;
  }
  for (int i=0;i<2;++i) {
    const double at=pdnorm(q-1,mean,sd,lower_tail,true);
    if (lower_tail ? at>=lp : at<=lp) q-=1;
    else break;
  }
  const double at=pdnorm(q,mean,sd,lower_tail,true);
  const double before=pdnorm(q-1,mean,sd,lower_tail,true);
  if (q-1==q || std::isnan(at) || std::isnan(before) ||
      !(lower_tail ? at>=lp && before<lp : at<=lp && before>lp))
    return std::numeric_limits<double>::quiet_NaN();
  return q;
}
template <typename Rng> inline double rdnorm(Rng &rng, double mean, double sd) {
  if (!std::isfinite(mean) || !std::isfinite(sd) || !(sd>0)) return std::numeric_limits<double>::quiet_NaN();
  return std::floor(mean+sd*rng.rnorm());
}
}}
