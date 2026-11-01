#pragma once
#include "../../special/special.hpp"
#include <cmath>
#include <algorithm>
namespace cppunivariate {
namespace distributions {
// Preserve a standardized distance above binary64 range for the power tail.
inline double slash_log_distance_cached(double x, double mu, double log_sigma) {
  const double distance=std::abs(x-mu);
  const double ld=std::isfinite(distance) || !std::isfinite(x) ? std::log(distance)
      : special::log_sum_exp(std::log(std::abs(x)),std::log(std::abs(mu)));
  return ld-log_sigma;
}
inline double slash_log_distance(double x, double mu, double sigma) {
  return slash_log_distance_cached(x,mu,std::log(sigma));
}
// Internal finite-parameter path: reuse log(sigma) across a likelihood pass.
inline double slash_log_density_cached(double x,double mu,double sigma,double log_sigma) {
  const double z=special::scaled_difference(x,mu,sigma), t=(0.5*z)*z;
  const double kernel=t<1e-8 ? -std::log(2.0)-t/2+t*t/24 : std::log(-std::expm1(-t))-2*slash_log_distance_cached(x,mu,log_sigma);
  return kernel-0.91893853320467274178-log_sigma;
}
inline double dslash(double x,double mu=0,double sigma=1,bool log=false) {
  if (!std::isfinite(mu)||!std::isfinite(sigma)||sigma<=0||std::isnan(x)) return special::kNaN;
  const double ld=slash_log_density_cached(x,mu,sigma,std::log(sigma));
  return log ? ld : std::exp(ld);
}
inline double pslash(double q,double mu=0,double sigma=1,bool lower_tail=true,bool log_p=false) {
  if (!std::isfinite(mu)||!std::isfinite(sigma)||sigma<=0||std::isnan(q)) return special::kNaN;
  const double z=special::scaled_difference(q,mu,sigma), w=std::abs(z);
  const double lp=w==0 ? -std::log(2.0) : special::log_sum_exp(special::normal_log_upper(w),std::log(-std::expm1(-(0.5*w)*w))-0.91893853320467274178-slash_log_distance(q,mu,sigma));
  return special::tail_probability(special::tails_from_log(lp,z<=0),lower_tail,log_p);
}
inline double qslash(double p,double mu=0,double sigma=1,bool lower_tail=true,bool log_p=false) {
  if(!(std::isfinite(mu)&&std::isfinite(sigma)&&sigma>0)||std::isnan(p)||(log_p?p>0:p<0||p>1))return special::kNaN;
  const double offset=special::probability_offset(p,log_p)*(lower_tail?1.0:-1.0);
  if (std::abs(offset)<1e-4) { const double y=offset*5.0132565492620010048,y2=y*y; return std::fma(sigma,y*(1+y2/12+y2*y2/80),mu); }
  double lp=log_p?p:std::log(p);
  if(lp==special::kNegInf)return lower_tail?special::kNegInf:special::kPosInf;
  if(lp==0)return lower_tail?special::kPosInf:special::kNegInf;
  if(lp>-.6931471805599453){lp=std::log(-std::expm1(lp));lower_tail=!lower_tail;}
  const double edge=lower_tail?-std::numeric_limits<double>::max():std::numeric_limits<double>::max();
  if(pslash(edge,mu,sigma,lower_tail,true)>lp)return lower_tail?special::kNegInf:special::kPosInf;
  double lo=-1,hi=1;const double scale=sigma;
  auto coordinate=[&](double z){return special::expm1_affine(z,mu,scale);};
  auto at=[&](double z){return pslash(coordinate(z),mu,sigma,lower_tail,true);};
  for(int k=0;k<1100&&(lower_tail?at(lo)>lp:at(lo)<lp);++k){lo*=2;if(!std::isfinite(lo))return special::kNegInf;}
  for(int k=0;k<1100&&(lower_tail?at(hi)<lp:at(hi)>lp);++k){hi*=2;if(!std::isfinite(hi))return special::kPosInf;}
  for(int k=0;k<1100;++k){const double m=lo/2+hi/2;if(lower_tail?at(m)<lp:at(m)>lp)lo=m;else hi=m;if(hi-lo<4e-14*std::max(1.0,std::abs(m)))break;}
  return coordinate(lo/2+hi/2);
}
template <typename Rng> inline double rslash(Rng& rng,double mu=0,double sigma=1) {if(!std::isfinite(mu)||!std::isfinite(sigma)||sigma<=0)return special::kNaN;return mu+sigma*rng.rnorm()/rng.runif(0,1);}
}
}
