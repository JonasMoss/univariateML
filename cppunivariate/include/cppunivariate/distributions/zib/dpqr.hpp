#pragma once
#include "../ztpois/dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline bool zib_valid(double size,double prob) {
  return std::isfinite(size) && size>=0 && size==std::floor(size) && prob>=0 && prob<=1;
}
inline double dzib(double x,double size,double prob,double pi,bool log=false) {
  if(!zib_valid(size,prob)||!(pi>=0 && pi<=1)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  double l=-std::numeric_limits<double>::infinity();
  if(size==0)l=x==0?0:-std::numeric_limits<double>::infinity();
  else if(x==0)l=std::log1p((1-pi)*std::expm1(size*std::log1p(-prob)));
  else if(x>0 && x<=size && x==std::floor(x))l=std::log1p(-pi)+zero_detail::bin_logpmf(static_cast<int>(x),size,prob);
  return log?l:std::exp(l);
}
inline double pzib(double q,double size,double prob,double pi,bool lower_tail=true,bool log_p=false) {
  if(!zib_valid(size,prob)||!(pi>=0 && pi<=1)||std::isnan(q))return std::numeric_limits<double>::quiet_NaN();
  // Keep the probability of a positive count in log space: reconstructing
  // it as 1-P(0) would erase small, representable binomial probabilities.
  if(size==0) {const double v=q<0?0:1;return log_p?std::log(lower_tail?v:1-v):(lower_tail?v:1-v);}
  const double s=q<0 ? 0 : std::log1p(-pi)+std::log(-std::expm1(size*std::log1p(-prob)))+
      zero_detail::logsurvival(3,q,size,prob);
  if(!lower_tail)return log_p?s:std::exp(s);
  return log_p?std::log(-std::expm1(s)):-std::expm1(s);
}
inline double qzib(double p,double size,double prob,double pi,bool lower_tail=true,bool log_p=false) {
  if(!zib_valid(size,prob)||!(pi>=0 && pi<=1)||std::isnan(p)||
      (log_p?p>0:(p<0||p>1)))return std::numeric_limits<double>::quiet_NaN();
  const double ls=lower_tail ? (log_p?std::log(-std::expm1(p)):std::log1p(-p)) : (log_p?p:std::log(p));
  if(size==0||prob==0||pi==1||ls==0)return 0;
  if(std::isinf(ls))return size;
  double lo=-1,hi=size;
  while(hi-lo>1) {const double mid=std::floor((lo+hi)/2);if(pzib(mid,size,prob,pi,false,true)<=ls)hi=mid;else lo=mid;}
  return hi;
}
template <typename Rng> inline double rzib(Rng &rng,double size,double prob,double pi) {
  return qzib(rng.runif(0,1),size,prob,pi);
}
} }
