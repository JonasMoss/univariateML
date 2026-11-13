#pragma once
#include "../pois/ztpois_dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dzmgeom(double x,double prob, double p0,bool log=false) {return zero_detail::density(1,x,prob,0,p0,log);}
inline double pzmgeom(double q,double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(1,q,prob,0,p0,lower_tail,log_p);}
inline double qzmgeom(double p,double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(1,p,prob,0,p0,lower_tail,log_p,0);}
template <typename Rng> inline double rzmgeom(Rng &rng,double prob, double p0) {return qzmgeom(rng.runif(0,1),prob, p0);}
} }
