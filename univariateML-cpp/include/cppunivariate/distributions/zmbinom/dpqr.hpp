#pragma once
#include "../ztpois/dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dzmbinom(double x,double size, double prob, double p0,bool log=false) {return zero_detail::density(3,x,size,prob,p0,log);}
inline double pzmbinom(double q,double size, double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(3,q,size,prob,p0,lower_tail,log_p);}
inline double qzmbinom(double p,double size, double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(3,p,size,prob,p0,lower_tail,log_p,0);}
template <typename Rng> inline double rzmbinom(Rng &rng,double size, double prob, double p0) {if (prob==1 && p0==0) return qzmbinom(.5,size, prob, p0); return qzmbinom(rng.runif(0,1),size, prob, p0);}
} }
