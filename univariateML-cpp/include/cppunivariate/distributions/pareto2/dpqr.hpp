#pragma once
#include "../pareto3/dpqr.hpp"
#include "../../special/special.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dpareto2(double x,double shape,double scale,double min,bool log) {
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

#include "../lomax/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double ppareto2(double q,double shape,double scale,double min,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 if(q<=min) return log_p ? (lower_tail ? -std::numeric_limits<double>::infinity() : 0) : (lower_tail ? 0 : 1);
 if(q>=std::numeric_limits<double>::infinity()) return log_p ? (lower_tail ? 0 : -std::numeric_limits<double>::infinity()) : (lower_tail ? 1 : 0);
 return plomax(q-min,1/scale,shape,lower_tail,log_p);
}
inline double qpareto2(double p,double shape,double scale,double min,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 if(p==(log_p ? -std::numeric_limits<double>::infinity() : 0)) return lower_tail ? min : std::numeric_limits<double>::infinity();
 if(p==(log_p ? 0 : 1)) return lower_tail ? std::numeric_limits<double>::infinity() : min;
 if(shape==1) return qpareto3(p,1,scale,min,lower_tail,log_p);
 return min+qlomax(p,1/scale,shape,lower_tail,log_p);
}
template <typename Rng> inline double rpareto2(Rng &rng,double shape,double scale,double min) {
 if(!(std::isfinite(min) && shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 return min+rlomax(rng,1/scale,shape);
}
} }
