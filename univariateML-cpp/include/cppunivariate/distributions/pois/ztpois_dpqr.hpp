#pragma once
#include "ztpois_dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dztpois(double x,double lambda,bool log=false) {return zero_detail::density(0,x,lambda,0,0,log);}
inline double pztpois(double q,double lambda,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(0,q,lambda,0,0,lower_tail,log_p);}
inline double qztpois(double p,double lambda,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(0,p,lambda,0,0,lower_tail,log_p);}
template <typename Rng> inline double rztpois(Rng &rng,double lambda) {return zero_detail::quantile(0,rng.runif(0,1),lambda,0,0,true,false,-1,false);}
} }
