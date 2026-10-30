#pragma once
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class DlaplaceStatus { Ok, InvalidInput, PointMass };
struct DlaplaceResult { double location, scale, logLik; bool not_unique; DlaplaceStatus status; };
inline DlaplaceResult dlaplace_mle(ispan x, double location=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  DlaplaceResult r{location,scale,0,false,DlaplaceStatus::InvalidInput};
  const bool free_location=std::isnan(location), free_scale=std::isnan(scale);
  if(x.size()==0 || (!free_location && (!std::isfinite(location)||location!=std::floor(location))) || (!free_scale && !(scale>0 && scale<1))) return r;
  if(free_location) {
    std::vector<int> y(x.begin(),x.end());std::sort(y.begin(),y.end());
    r.location=y[(y.size()-1)/2];r.not_unique=y[(y.size()-1)/2]!=y[y.size()/2];
  }
  double s=0;for(int v:x)s+=std::abs(double(v)-r.location);
  if(free_scale) {
    if(s==0){r.status=DlaplaceStatus::PointMass;return r;}
    const double d=s/x.size();r.scale=d/(std::hypot(1.0,d)+1);
    if(!(r.scale>0&&r.scale<1))return r;
  }
  r.logLik=x.size()*(std::log1p(-r.scale)-std::log1p(r.scale))+s*std::log(r.scale);
  r.status=DlaplaceStatus::Ok;return r;
}
}
