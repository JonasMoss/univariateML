#pragma once
#include "../pareto3/dpqr.hpp"
#include "../../special/special.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dpareto4(double x,double shape1,double shape2,double scale,double min,bool log) {
 const double inf=std::numeric_limits<double>::infinity();
 if(!(shape1>0 && shape2>0 && scale>0) || !std::isfinite(min) || !std::isfinite(scale) || !std::isfinite(shape2+1+shape1) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
 if(x<min || x==inf) return log ? -inf : 0;
 const double c=std::log(shape1)+std::log(shape2)-std::log(scale);
 const double power=shape2*1-1;
 if(x==min) {const double ll=power<0 ? inf : (power>0 ? -inf : c); return log ? ll : std::exp(ll);}
 const double z=std::log(x-min)-std::log(scale),t=shape2*z;
 const double sp=std::fmax(t,0)+std::log1p(std::exp(-std::abs(t)));
 const double ll=c+power*z-(1+shape1)*sp;
 return log ? ll : std::exp(ll);
}
} }

#include "../burr/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double ppareto4(double q,double shape1,double shape2,double scale,double min,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 if(q<=min) return log_p ? (lower_tail ? -std::numeric_limits<double>::infinity() : 0) : (lower_tail ? 0 : 1);
 if(q>=std::numeric_limits<double>::infinity()) return log_p ? (lower_tail ? 0 : -std::numeric_limits<double>::infinity()) : (lower_tail ? 1 : 0);
 return pburr(q-min,shape1,shape2,scale,lower_tail,log_p);
}
inline double qpareto4(double p,double shape1,double shape2,double scale,double min,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 if(p==(log_p ? -std::numeric_limits<double>::infinity() : 0)) return lower_tail ? min : std::numeric_limits<double>::infinity();
 if(p==(log_p ? 0 : 1)) return lower_tail ? std::numeric_limits<double>::infinity() : min;
 if(shape1==1) return qpareto3(p,shape2,scale,min,lower_tail,log_p);
 return min+qburr(p,shape1,shape2,scale,lower_tail,log_p);
}
template <typename Rng> inline double rpareto4(Rng &rng,double shape1,double shape2,double scale,double min) {
 if(!(std::isfinite(min) && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 return min+rburr(rng,shape1,shape2,scale);
}
} }
