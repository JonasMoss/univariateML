#pragma once
#include "ztpois_dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dzmpois(double x,double lambda, double p0,bool log=false) {return zero_detail::density(0,x,lambda,0,p0,log);}
inline double pzmpois(double q,double lambda, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(0,q,lambda,0,p0,lower_tail,log_p);}
inline double qzmpois(double p,double lambda, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(0,p,lambda,0,p0,lower_tail,log_p,0);}
template <typename Rng> inline double rzmpois(Rng &rng,double lambda, double p0) {return zero_detail::quantile(0,rng.runif(0,1),lambda,0,p0,true,false,0,false);}
} }
