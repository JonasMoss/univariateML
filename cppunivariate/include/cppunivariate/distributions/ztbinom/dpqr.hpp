#pragma once
#include "../ztpois/dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dztbinom(double x,double size, double prob,bool log=false) {return zero_detail::density(3,x,size,prob,0,log);}
inline double pztbinom(double q,double size, double prob,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(3,q,size,prob,0,lower_tail,log_p);}
inline double qztbinom(double p,double size, double prob,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(3,p,size,prob,0,lower_tail,log_p);}
template <typename Rng> inline double rztbinom(Rng &rng,double size, double prob) {if (prob==1) return qztbinom(.5,size, prob); return qztbinom(rng.runif(0,1),size, prob);}
} }
