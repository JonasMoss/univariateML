#pragma once
#include "mle.hpp"
namespace cppunivariate {
namespace distributions {
inline double dgenbeta(double x,double a,double b,double t,bool log=false,double scale=1) {
  const double inf=std::numeric_limits<double>::infinity();
  if(!(a>0&&b>0&&t>0&&scale>0)||!std::isfinite(a+b+t+scale)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  double ll=-inf;
  if(x==0) {
    if(a*t<1)ll=inf;
    else if(a*t==1)ll=std::log(t)-std::log(scale)-gb2_detail::logbeta(a,b);
  } else if(x==scale) {
    if(b<1)ll=inf;
    else if(b==1)ll=std::log(a*t)-std::log(scale);
  } else if(x>0&&x<scale) {
    const double z=t*(x>scale*.5?std::log1p((x-scale)/scale):std::log(x)-std::log(scale));
    const double r=z>-.6931471805599453?std::log(-std::expm1(z)):std::log1p(-std::exp(z));
    ll=std::log(t)-std::log(x)+a*z+(b-1)*r-gb2_detail::logbeta(a,b);
  }
  return log?ll:std::exp(ll);
}
} // namespace distributions
} // namespace cppunivariate
