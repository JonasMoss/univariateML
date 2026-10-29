#pragma once
#include "mle.hpp"
namespace cppunivariate {
namespace distributions {
inline double dgb2(double x,double shape1,double shape2,double shape3,
                   bool log=false,double scale=1) {
  if(!(shape1>0&&shape2>0&&shape3>0&&scale>0)||
     !std::isfinite(shape1+shape2+shape3)||!std::isfinite(scale)||std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if(x<=0||!std::isfinite(x))return log ? -std::numeric_limits<double>::infinity():0;
  const double t=shape2*(std::log(x)-std::log(scale));
  const double ll=std::log(shape2)-std::log(x)-gb2_detail::logbeta(shape3,shape1)
    -shape3*gb2_detail::softplus(-t)-shape1*gb2_detail::softplus(t);
  return log?ll:std::exp(ll);
}
} // namespace distributions
} // namespace cppunivariate
