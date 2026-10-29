#pragma once
#include "profile.hpp"
namespace cppunivariate {
enum class ZtpoisStatus { Ok, InvalidInput };
struct ZtpoisResult { double lambda, logLik; int iterations; ZtpoisStatus status; };
inline ZtpoisResult ztpois_mle(ispan x) {
  if(!zero_detail::positive(x)) return {0,0,0,ZtpoisStatus::InvalidInput};
  const double m=zero_detail::mean(x);
  if(m==1) return {0,0,0,ZtpoisStatus::Ok};
  double lo=0, hi=m;
  for(int i=0;i<100;++i) {
    const double l=(lo+hi)/2;
    if(l/(-std::expm1(-l))<m) lo=l; else hi=l;
  }
  const double l=(lo+hi)/2;
  double ll=0;
  for(int v:x) ll+=-l+v*std::log(l)-special::lgamma(v+1.0)-std::log(-std::expm1(-l));
  return {l,ll,100,ZtpoisStatus::Ok};
}
}
