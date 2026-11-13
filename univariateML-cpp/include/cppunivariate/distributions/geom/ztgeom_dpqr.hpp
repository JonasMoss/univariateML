#pragma once
#include "../pois/ztpois_dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dztgeom(double x,double prob,bool log=false) {return zero_detail::density(1,x,prob,0,0,log);}
inline double pztgeom(double q,double prob,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(1,q,prob,0,0,lower_tail,log_p);}
inline double qztgeom(double p,double prob,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(1,p,prob,0,0,lower_tail,log_p);}
template <typename Rng> inline double rztgeom(Rng &rng,double prob) {return zero_detail::quantile(1,rng.runif(0,1),prob,0,0,true,false,-1,false);}
} }
