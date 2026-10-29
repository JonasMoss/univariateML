#pragma once
#include "../beta/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double dnsbeta(double x, double shape1, double shape2, double min, double max, bool log=false) {
 
 if(!(max>min) || !std::isfinite(min) || !std::isfinite(max) || !(shape1>0) || !(shape2>0)) return std::numeric_limits<double>::quiet_NaN();
 if(x==min && shape1==1) return log ? std::log(shape2/(max-min)) : shape2/(max-min);
 if(x==max && shape2==1) return log ? std::log(shape1/(max-min)) : shape1/(max-min);
 if(std::isinf(x)) return log ? -std::numeric_limits<double>::infinity() : 0;
 const double ll=dbeta(((x-min)/(max-min)),shape1,shape2,true)-std::log(max-min);
 return log ? ll : std::exp(ll);
}
} }
