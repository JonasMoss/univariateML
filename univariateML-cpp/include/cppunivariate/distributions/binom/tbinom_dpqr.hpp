#pragma once
#include "../pois/tpois_dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dtbinom(double x,double prob, double a, double b, double size,bool log=false){if(!std::isfinite(size)||size<0||size!=std::floor(size))return std::numeric_limits<double>::quiet_NaN();return truncated_detail::evaluate(false,x,prob,a,b,size,true,log);}
inline double ptbinom(double q,double prob, double a, double b, double size,bool lower_tail=true,bool log_p=false){if(!std::isfinite(size)||size<0||size!=std::floor(size))return std::numeric_limits<double>::quiet_NaN();return truncated_detail::evaluate(true,q,prob,a,b,size,lower_tail,log_p);}
inline double qtbinom(double p,double prob, double a, double b, double size,bool lower_tail=true,bool log_p=false){if(!std::isfinite(size)||size<0||size!=std::floor(size))return std::numeric_limits<double>::quiet_NaN();return truncated_detail::quantile(p,prob,a,b,size,lower_tail,log_p);}
template <typename Rng> inline double rtbinom(Rng &rng,double prob, double a, double b, double size){if (prob==0 || prob==1) return qtbinom(.5,prob,a,b,size); return qtbinom(rng.runif(0,1), prob, a, b, size);}
} }
