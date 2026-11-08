#pragma once
#include "../../special/special.hpp"
#include "../../special/tails.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
// Boost's log-CDF seam may underflow in the lower tail before taking logs.
// The positive incomplete-gamma series restores that logarithm directly.
inline double dgamma_logtail(double t,double shape,bool lower) {
  const double value=special::fs_gamma_p(t,shape,lower,true);
  if (!lower || std::isfinite(value) || !(t>0) || !std::isfinite(t)) return value;
  double sum=1,term=1;
  for (int j=1;j<=100000;++j) {
    term*=t/(shape+j); sum+=term;
    if (term<2e-16*sum) return shape*std::log(t)-t-special::lgamma(shape+1)+std::log(sum);
  }
  return value;
}
inline double ddgamma(double x, double shape, double rate, bool log = false) {
  if (!(shape > 0 && rate > 0) || !std::isfinite(shape) || !std::isfinite(rate) || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  double lp = -std::numeric_limits<double>::infinity();
  if (std::isfinite(x) && x >= 0 && x == std::floor(x)) {
    const double a=x*rate, b=(x+1)*rate;
    if (x>1e16*std::max(1.0,shape))
      lp=(shape-1)*(std::log(x)+std::log(rate))-x*rate-special::lgamma(shape)+special::log1mexp(-rate);
    else if (x == 0) lp=dgamma_logtail(b,shape,true);
    else {
      const bool lower = special::fs_gamma_p(a,shape,true,false) < 0.5;
      const double hi=dgamma_logtail(lower ? b : a,shape,lower);
      const double lo=dgamma_logtail(lower ? a : b,shape,lower);
      lp=hi+special::fs_log1mexp(lo-hi);
      // Narrow intervals can lose their endpoint difference. Midpoint expansion.
      if (rate*(1+std::abs((shape-1)/a-1)) < 1e-6)
        lp=special::dgamma((x+0.5)*rate,shape,1,true)+std::log(rate);
    }
  }
  return log ? lp : std::exp(lp);
}
inline double pdgamma(double q,double shape,double rate,bool lower_tail=true,bool log_p=false) {
  if (!(shape>0 && rate>0) || !std::isfinite(shape) || !std::isfinite(rate) || std::isnan(q))
    return std::numeric_limits<double>::quiet_NaN();
  if (q<0) return lower_tail ? (log_p ? -std::numeric_limits<double>::infinity() : 0) : (log_p ? 0 : 1);
  const double lp=dgamma_logtail((std::floor(q)+1)*rate,shape,lower_tail);
  return log_p ? lp:std::exp(lp);
}
inline double qdgamma(double p,double shape,double rate,bool lower_tail=true,bool log_p=false) {
  const double lp=log_p ? p : std::log(p);
  if (std::isnan(lp) || lp>0 || (!log_p && p<0) || !(shape>0 && rate>0) || !std::isfinite(shape) || !std::isfinite(rate))
    return std::numeric_limits<double>::quiet_NaN();
  if (lp==-INFINITY) return lower_tail ? 0 : INFINITY;
  if (lp==0) return lower_tail ? INFINITY : 0;
  const double z=special::fs_gamma_q(lp,shape,lower_tail)/rate;
  if (!std::isfinite(z)) return z;
  double q=std::max(0.0,std::ceil(z)-1);
  for (int i=0;i<2;++i) {
    const double at=pdgamma(q,shape,rate,lower_tail,true);
    if (lower_tail ? at<lp : at>lp) q+=1;
    else break;
  }
  for (int i=0;i<2 && q>0;++i) {
    const double at=pdgamma(q-1,shape,rate,lower_tail,true);
    if (lower_tail ? at>=lp : at<=lp) q-=1;
    else break;
  }
  // The seed correction is bounded, so verify both inverse inequalities.
  // A failed CDF or an unrepresentable predecessor cannot certify a jump.
  const double at=pdgamma(q,shape,rate,lower_tail,true);
  const double before=q>0 ? pdgamma(q-1,shape,rate,lower_tail,true) : (lower_tail ? -INFINITY : 0);
  if (std::isnan(at) || std::isnan(before) || (q>0 && q-1==q) ||
      !(lower_tail ? at>=lp && before<lp : at<=lp && before>lp))
    return std::numeric_limits<double>::quiet_NaN();
  return q;
}
template <typename Rng> inline double rdgamma(Rng &rng,double shape,double rate) {
  if (!(shape>0 && rate>0) || !std::isfinite(shape) || !std::isfinite(rate)) return std::numeric_limits<double>::quiet_NaN();
  return std::floor(rng.rgamma(shape,1/rate));
}
}}
