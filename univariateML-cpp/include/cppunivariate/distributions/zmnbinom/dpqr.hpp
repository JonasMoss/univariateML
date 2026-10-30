#pragma once
#include "../ztpois/dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dzmnbinom(double x,double size, double prob, double p0,bool log=false) {return zero_detail::density(2,x,size,prob,p0,log);}
inline double pzmnbinom(double q,double size, double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(2,q,size,prob,p0,lower_tail,log_p);}
inline double qzmnbinom(double p,double size, double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(2,p,size,prob,p0,lower_tail,log_p);}
template <typename Rng> inline double rzmnbinom(Rng &rng,double size, double prob, double p0) {return qzmnbinom(rng.runif(0,1),size, prob, p0);}
} }
