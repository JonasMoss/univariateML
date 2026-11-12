#pragma once
#include "profile.hpp"
#include "dpqr.hpp"
namespace cppunivariate {
inline truncated_detail::Result tpois_mle(ispan x, double a, double b, double held=std::numeric_limits<double>::quiet_NaN()) {
  auto result=[&]() -> truncated_detail::Result {
  if (x.size()==0) return {0,0,0,truncated_detail::Status::InvalidInput};
  int xmin=x[0],xmax=x[0];
  for (int v:x) {if(v<0)return {0,0,0,truncated_detail::Status::InvalidInput}; xmin=std::min(xmin,v);xmax=std::max(xmax,v);}
  if(std::isnan(a))a=static_cast<double>(xmin)-1;
  if(std::isnan(b))b=xmax;
  // A singleton conditioning event has likelihood one at every positive rate.
  const double lower=std::max(0.,std::floor(a)+1),upper=std::floor(b);
  if(a<b && lower==upper && lower==xmin && upper==xmax && std::isfinite(lower)) {
    const double rate=std::isnan(held)?1:held;
    if(!std::isfinite(rate)||rate<0||(rate==0&&lower!=0))
      return {0,0,0,truncated_detail::Status::InvalidInput};
    return {rate,0,0,truncated_detail::Status::Ok};
  }
  if(!std::isnan(held)) {
    auto r=truncated_detail::fit(x,a,b,-1);
    if(r.status==truncated_detail::Status::InvalidInput)return r;
    if(held<0 || !std::isfinite(held))return {0,0,0,truncated_detail::Status::InvalidInput};
    const double lo=std::max(0.,std::floor(a)+1),hi=std::floor(b);
    if(held==0) {
      const double point=held==0?0:0;
      if(point<lo||point>hi)return {0,0,0,truncated_detail::Status::InvalidInput};
      for(int v:x)if(v!=point)return {0,0,0,truncated_detail::Status::InvalidInput};
      return {held,0,0,truncated_detail::Status::Ok};
    }
    const double t=std::log(held);
    auto z=truncated_detail::mass(t,lo,hi,-1);
    if(!z.ok)return {0,0,0,truncated_detail::Status::NumericalFailure};
    double ll=0;for(int v:x)ll+=truncated_detail::kernel(v,t,-1)-z.logz;
    return {held,ll,0,truncated_detail::Status::Ok};
  }
  return truncated_detail::fit(x,a,b,-1);
  }();
  if(result.status==truncated_detail::Status::Ok) {
    double ll=0,correction=0;
    for(int v:x) {
      const double value=distributions::dtpois(v,result.parameter,a,b,true)-correction;
      const double next=ll+value;correction=(next-ll)-value;ll=next;
    }
    result.logLik=ll;
    if(!std::isfinite(ll))result.status=truncated_detail::Status::NumericalFailure;
  }
  return result;
}
}
