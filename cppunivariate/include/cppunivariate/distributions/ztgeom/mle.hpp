#pragma once
#include "../ztpois/profile.hpp"
namespace cppunivariate {
enum class ZtgeomStatus { Ok, InvalidInput };
struct ZtgeomResult { double prob, logLik; int iterations; ZtgeomStatus status; };
inline ZtgeomResult ztgeom_mle(ispan x) {
  if(!zero_detail::positive(x)) return {0,0,0,ZtgeomStatus::InvalidInput};
  const double p=1/zero_detail::mean(x);
  double ll=0;
  if(p<1) for(int v:x) ll+=std::log(p)+(v-1)*std::log1p(-p);
  return {p,ll,0,ZtgeomStatus::Ok};
}
}
