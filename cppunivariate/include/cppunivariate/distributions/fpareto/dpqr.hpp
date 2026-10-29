#pragma once
#include "../../special/special.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dfpareto(double x,double min,double shape1,double shape2,double shape3,double scale,bool log=false) {
 const double inf=std::numeric_limits<double>::infinity();
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(min) || !std::isfinite(scale) || !std::isfinite(shape2+shape3+shape1) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
 if(x<min || x==inf) return log ? -inf : 0;
 const double c=std::log(shape2)-std::log(scale)-cppunivariate::special::lbeta(shape3,shape1);
 const double power=shape2*shape3-1;
 if(x==min) {const double ll=power<0 ? inf : (power>0 ? -inf : c); return log ? ll : std::exp(ll);}
 const double z=std::log(x-min)-std::log(scale),t=shape2*z;
 const double sp=std::fmax(t,0)+std::log1p(std::exp(-std::abs(t)));
 const double ll=c+power*z-(shape3+shape1)*sp;
 return log ? ll : std::exp(ll);
}
} }
