#pragma once
#include "../tpois/dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dtbinom(double x,double size, double prob, double a, double b,bool log=false){if(!std::isfinite(size)||size<0||size!=std::floor(size))return std::numeric_limits<double>::quiet_NaN();return truncated_detail::evaluate(false,x,prob,a,b,size,true,log);}
inline double ptbinom(double q,double size, double prob, double a, double b,bool lower_tail=true,bool log_p=false){if(!std::isfinite(size)||size<0||size!=std::floor(size))return std::numeric_limits<double>::quiet_NaN();return truncated_detail::evaluate(true,q,prob,a,b,size,lower_tail,log_p);}
inline double qtbinom(double p,double size, double prob, double a, double b,bool lower_tail=true,bool log_p=false){if(!std::isfinite(size)||size<0||size!=std::floor(size))return std::numeric_limits<double>::quiet_NaN();return truncated_detail::quantile(p,prob,a,b,size,lower_tail,log_p);}
template <typename Rng> inline double rtbinom(Rng &rng,double size, double prob, double a, double b){return qtbinom(rng.runif(0,1),size,prob,a,b);}
} }
