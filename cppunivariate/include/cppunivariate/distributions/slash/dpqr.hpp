#pragma once
#include "../../special/special.hpp"
#include <cmath>
#include <algorithm>
namespace cppunivariate {
namespace distributions {
inline double dslash(double x,double mu=0,double sigma=1,bool log=false) {
  if(!std::isfinite(mu)||!std::isfinite(sigma)||sigma<=0||std::isnan(x))return special::kNaN;
  const long double z=(static_cast<long double>(x)-mu)/sigma,t=z*z/2;
  const long double kernel=t<1e-8L ? -std::log(2.0L)-t/2+t*t/24 : std::log(-std::expm1(-t))-2*std::log(std::abs(z));
  const double l=static_cast<double>(kernel-.91893853320467274178L-std::log(static_cast<long double>(sigma)));
  return log?l:std::exp(l);
}
inline double pslash(double x,double mu=0,double sigma=1,bool lower=true,bool log=false) {
  if(!std::isfinite(mu)||!std::isfinite(sigma)||sigma<=0||std::isnan(x))return special::kNaN;
  const long double z=(static_cast<long double>(x)-mu)/sigma,w=std::abs(z);
  long double small=w==0 ? .5L : std::erfc(w/std::sqrt(2.0L))/2 + (-std::expm1(-w*w/2))/(std::sqrt(2*std::acos(-1.0L))*w);
  const long double lp=std::log(small);
  const bool own=lower?z<=0:z>=0;
  const double ans=static_cast<double>(own?lp:std::log1p(-small));
  return log?ans:std::exp(ans);
}
inline double qslash(double p,double mu=0,double sigma=1,bool lower=true,bool log=false) {
  if(!(std::isfinite(mu)&&std::isfinite(sigma)&&sigma>0)||std::isnan(p)||(log?p>0:p<0||p>1))return special::kNaN;
  double lp=log?p:std::log(p);
  if(lp==special::kNegInf)return lower?special::kNegInf:special::kPosInf;
  if(lp==0)return lower?special::kPosInf:special::kNegInf;
  if(lp>-.6931471805599453){lp=std::log(-std::expm1(lp));lower=!lower;}
  const double edge=lower?-std::numeric_limits<double>::max():std::numeric_limits<double>::max();
  if(pslash(edge,mu,sigma,lower,true)>lp)return lower?special::kNegInf:special::kPosInf;
  double lo=-1,hi=1;const double scale=sigma;
  auto coordinate=[&](double z){
    const long double t=std::abs(static_cast<long double>(z));
    const long double distance=t<.1L?scale*std::expm1(t):std::exp(std::log(static_cast<long double>(scale))+t)*(-std::expm1(-t));
    return static_cast<double>(static_cast<long double>(mu)+(z<0?-1:1)*distance);
  };
  auto at=[&](double z){return pslash(coordinate(z),mu,sigma,lower,true);};
  for(int k=0;k<1100&&(lower?at(lo)>lp:at(lo)<lp);++k){lo*=2;if(!std::isfinite(lo))return special::kNegInf;}
  for(int k=0;k<1100&&(lower?at(hi)<lp:at(hi)>lp);++k){hi*=2;if(!std::isfinite(hi))return special::kPosInf;}
  for(int k=0;k<1100;++k){const double m=lo/2+hi/2;if(lower?at(m)<lp:at(m)>lp)lo=m;else hi=m;if(hi-lo<4e-14*std::max(1.0,std::abs(m)))break;}
  return coordinate(lo/2+hi/2);
}
template <typename Rng> inline double rslash(Rng& rng,double mu=0,double sigma=1) {if(!std::isfinite(mu)||!std::isfinite(sigma)||sigma<=0)return special::kNaN;return mu+sigma*rng.rnorm()/rng.runif(0,1);}
}
}
