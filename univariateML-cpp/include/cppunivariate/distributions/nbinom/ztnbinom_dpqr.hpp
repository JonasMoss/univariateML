#pragma once
#include "../pois/ztpois_dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dztnbinom(double x,double size, double prob,bool log=false) {return zero_detail::density(2,x,size,prob,0,log);}
inline double pztnbinom(double q,double size, double prob,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(2,q,size,prob,0,lower_tail,log_p);}
inline double qztnbinom(double p,double size, double prob,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(2,p,size,prob,0,lower_tail,log_p);}
template <typename Rng> inline double rztnbinom(Rng &rng,double size, double prob) {return zero_detail::quantile(2,rng.runif(0,1),size,prob,0,true,false,-1,false);}
} }
