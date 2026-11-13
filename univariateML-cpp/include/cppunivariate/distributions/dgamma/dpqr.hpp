#pragma once
#include "../gamma/dpqr.hpp"
#include "../../special/special.hpp"
#include "../../special/tails.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
// Boost's log-CDF seam may underflow in the lower tail before taking logs.
// The positive incomplete-gamma series restores that logarithm directly.
inline double dgamma_logtail(double t,double shape,bool lower) {
  const double value=pgamma(t,shape,1,lower,true);
  if (!lower || std::isfinite(value) || !(t>0) || !std::isfinite(t)) return value;
  double sum=1,term=1;
  for (int j=1;j<=100000;++j) {
    term*=t/(shape+j); sum+=term;
    if (term<2e-16*sum) return shape*std::log(t)-t-special::lgamma(shape+1)+std::log(sum);
  }
  return value;
}
// Integrate a centered Taylor series over one physical cell. On |z|=2,
// |f(mid+z)/f(mid)| <= exp(2|s|+(shape-1)*(2/mid)^2/(2*(1-2/mid))).
// Cauchy's coefficient bound controls the omitted terms after degree 32.
inline bool dgamma_centered_cell(double x,double shape,double rate,double &lp) {
  if(!(shape>=16 && x>4 && x<4503599627370496.0)) return false;
  const double mid=x+.5, displacement=std::fma(mid,rate,-shape);
  const double u=displacement/shape;
  if(std::abs(u)>.5) return false;
  const double slope=-(displacement+1)/mid, r=2/mid;
  const double log_bound=2*std::abs(slope)+(shape-1)*r*r/(2*(1-r));
  if(log_bound>10) return false;
  double previous=1,current=.5*slope,sum=1;
  for(int n=1;n<32;++n) {
    const double next=((.5*slope-.5*n/mid)*current-(.25*rate/mid)*previous)/(n+1);
    if((n+1)%2==0) sum+=next/(n+2);
    previous=current;current=next;
  }
  const double remainder=std::exp(log_bound)*std::pow(.25,33)/.75;
  if(!(sum>0 && remainder<=1e-13*sum)) return false;
  lp=.5*std::log(shape)-std::log(mid)-.91893853320467274178
    -special::density_stirling_remainder(shape)-special::scaled_positive_deviance(shape,u)+std::log(sum);
  return true;
}
inline double ddgamma(double x, double shape, double rate, bool log = false) {
  if (!(shape > 0 && rate > 0) || !std::isfinite(shape) || !std::isfinite(rate) || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  double lp = -std::numeric_limits<double>::infinity();
  if (std::isfinite(x) && x >= 0 && x == std::floor(x)) {
    const double a=x*rate, b=(x+1)*rate;
    if (dgamma_centered_cell(x,shape,rate,lp)) {}
    else if (x>1e16*std::max(1.0,shape))
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
  const auto search=[&]() {
    return special::discrete_search(0,INFINITY,[&](double k) {
      const double f=pdgamma(k,shape,rate,lower_tail,log_p);
      return special::discrete_reached(f,p,lower_tail,log_p);
    });
  };
  if (!std::isfinite(z) || std::abs(z)>=9007199254740992.0) return search();
  double q=std::max(0.0,std::ceil(z)-1);
  const auto hit=[&](double x) {
    return special::discrete_reached(pdgamma(x,shape,rate,lower_tail,log_p),p,lower_tail,log_p);
  };
  for(int i=0;i<2 && hit(q)==0;++i) q+=1;
  for(int i=0;i<2 && q>0 && hit(q-1)==1;++i) q-=1;
  if(hit(q)<0 || hit(q-1)<0) return std::numeric_limits<double>::quiet_NaN();
  if(q-1==q || hit(q)!=1 || hit(q-1)!=0) return search();
  return q;
}
template <typename Rng> inline double rdgamma(Rng &rng,double shape,double rate) {
  if (!(shape>0 && rate>0) || !std::isfinite(shape) || !std::isfinite(rate)) return std::numeric_limits<double>::quiet_NaN();
  return std::floor(rng.rgamma(shape,1/rate));
}
}}
