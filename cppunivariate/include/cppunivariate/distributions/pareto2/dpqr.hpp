#pragma once
#include "../../special/special.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dpareto2(double x,double min,double shape,double scale,bool log=false) {
 const double inf=std::numeric_limits<double>::infinity();
 if(!(shape>0 && scale>0) || !std::isfinite(min) || !std::isfinite(scale) || !std::isfinite(1+1+shape) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
 if(x<min || x==inf) return log ? -inf : 0;
 const double c=std::log(shape)-std::log(scale);
 const double power=1*1-1;
 if(x==min) {const double ll=power<0 ? inf : (power>0 ? -inf : c); return log ? ll : std::exp(ll);}
 const double z=std::log(x-min)-std::log(scale),t=1*z;
 const double sp=std::fmax(t,0)+std::log1p(std::exp(-std::abs(t)));
 const double ll=c+power*z-(1+shape)*sp;
 return log ? ll : std::exp(ll);
}
} }
