#pragma once
#include "ztpois_profile.hpp"
namespace cppunivariate {
enum class ZtpoisStatus { Ok, InvalidInput };
struct ZtpoisResult { double lambda, logLik; int iterations; ZtpoisStatus status; };
inline ZtpoisResult ztpois_mle(ispan x, double fixed=std::numeric_limits<double>::quiet_NaN()) {
  if(!zero_detail::positive(x)) return {0,0,0,ZtpoisStatus::InvalidInput};
  if(!std::isnan(fixed)) {
    if(!(fixed>=0 && std::isfinite(fixed)))return {0,0,0,ZtpoisStatus::InvalidInput};
    double ll=0;for(int v:x)ll+=fixed==0?(v==1?0:-std::numeric_limits<double>::infinity()):-fixed+v*std::log(fixed)-special::lgamma(v+1.0)-std::log(-std::expm1(-fixed));
    if(!std::isfinite(ll))return {fixed,ll,0,ZtpoisStatus::InvalidInput};
    return {fixed,ll,0,ZtpoisStatus::Ok};
  }
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
