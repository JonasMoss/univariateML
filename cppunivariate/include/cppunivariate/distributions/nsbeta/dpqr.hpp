#pragma once
#include "../beta/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double dnsbeta(double x, double shape1, double shape2, double min, double max, bool log) {
 
 if(!(max>min) || !std::isfinite(min) || !std::isfinite(max) || !(shape1>0) || !(shape2>0)) return std::numeric_limits<double>::quiet_NaN();
 if(x==min && shape1==1) return log ? std::log(shape2/(max-min)) : shape2/(max-min);
 if(x==max && shape2==1) return log ? std::log(shape1/(max-min)) : shape1/(max-min);
 if(std::isinf(x)) return log ? -std::numeric_limits<double>::infinity() : 0;
 const double ll=dbeta(((x-min)/(max-min)),shape1,shape2,true)-std::log(max-min);
 return log ? ll : std::exp(ll);
}
} }

#include "../beta/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double pnsbeta(double q,double shape1,double shape2,double min,double max,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 if(q<=min) return log_p ? (lower_tail ? -std::numeric_limits<double>::infinity() : 0) : (lower_tail ? 0 : 1);
 if(q>=max) return log_p ? (lower_tail ? 0 : -std::numeric_limits<double>::infinity()) : (lower_tail ? 1 : 0);
 return pbeta((q-min)/(max-min),shape1,shape2,lower_tail,log_p);
}
inline double qnsbeta(double p,double shape1,double shape2,double min,double max,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 if(p==(log_p ? -std::numeric_limits<double>::infinity() : 0)) return lower_tail ? min : max;
 if(p==(log_p ? 0 : 1)) return lower_tail ? max : min;
 return min+(max-min)*qbeta(p,shape1,shape2,lower_tail,log_p);
}
template <typename Rng> inline double rnsbeta(Rng &rng,double shape1,double shape2,double min,double max) {
 if(!(std::isfinite(min) && std::isfinite(max) && max>min && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2))) return std::numeric_limits<double>::quiet_NaN();
 return min+(max-min)*rbeta(rng,shape1,shape2);
}
} }
