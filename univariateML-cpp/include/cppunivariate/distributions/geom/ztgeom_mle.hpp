#pragma once
#include "../pois/ztpois_profile.hpp"
namespace cppunivariate {
enum class ZtgeomStatus { Ok, InvalidInput };
struct ZtgeomResult { double prob, logLik; int iterations; ZtgeomStatus status; };
inline ZtgeomResult ztgeom_mle(ispan x, double fixed=std::numeric_limits<double>::quiet_NaN()) {
  if(!zero_detail::positive(x)) return {0,0,0,ZtgeomStatus::InvalidInput};
  if(!std::isnan(fixed)) {
    if(!(fixed>0 && fixed<=1 && std::isfinite(fixed)))return {0,0,0,ZtgeomStatus::InvalidInput};
    double ll=0;for(int v:x)ll+=fixed==1?(v==1?0:-std::numeric_limits<double>::infinity()):std::log(fixed)+(v-1)*std::log1p(-fixed);
    if(!std::isfinite(ll))return {fixed,ll,0,ZtgeomStatus::InvalidInput};
    return {fixed,ll,0,ZtgeomStatus::Ok};
  }
  const double p=1/zero_detail::mean(x);
  double ll=0;
  if(p<1) for(int v:x) ll+=std::log(p)+(v-1)*std::log1p(-p);
  return {p,ll,0,ZtgeomStatus::Ok};
}
}
