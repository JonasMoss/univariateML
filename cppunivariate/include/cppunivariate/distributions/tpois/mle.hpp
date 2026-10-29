#pragma once
#include "profile.hpp"
namespace cppunivariate {
inline truncated_detail::Result tpois_mle(ispan x, double a, double b, double held=std::numeric_limits<double>::quiet_NaN()) {
  
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
}
}
