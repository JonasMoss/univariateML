#pragma once
#include "../tpois/profile.hpp"
namespace cppunivariate {
inline truncated_detail::Result tbinom_mle(ispan x, double a, double b, double size, double held=std::numeric_limits<double>::quiet_NaN()) {
  if(!std::isfinite(size)||size<0||size!=std::floor(size)) return {0,0,0,truncated_detail::Status::InvalidInput};
  if(!std::isnan(held)) {
    auto r=truncated_detail::fit(x,a,b,size);
    if(r.status==truncated_detail::Status::InvalidInput)return r;
    if(held<0 || !std::isfinite(held) || held>1)return {0,0,0,truncated_detail::Status::InvalidInput};
    const double lo=std::max(0.,std::floor(a)+1),hi=std::min(std::floor(b),size);
    if(held==0 || held==1) {
      const double point=held==0?0:size;
      if(point<lo||point>hi)return {0,0,0,truncated_detail::Status::InvalidInput};
      for(int v:x)if(v!=point)return {0,0,0,truncated_detail::Status::InvalidInput};
      return {held,0,0,truncated_detail::Status::Ok};
    }
    const double t=std::log(held)-std::log1p(-held);
    auto z=truncated_detail::mass(t,lo,hi,size);
    if(!z.ok)return {0,0,0,truncated_detail::Status::NumericalFailure};
    double ll=0;for(int v:x)ll+=truncated_detail::kernel(v,t,size)-z.logz;
    return {held,ll,0,truncated_detail::Status::Ok};
  }
  return truncated_detail::fit(x,a,b,size);
}
}
