#pragma once
#include "../ztpois/dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dzmlogarithmic(double x,double prob, double p0,bool log=false) {return zero_detail::density(4,x,prob,0,p0,log);}
inline double pzmlogarithmic(double q,double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::cdf(4,q,prob,0,p0,lower_tail,log_p);}
inline double qzmlogarithmic(double p,double prob, double p0,bool lower_tail=true,bool log_p=false) {return zero_detail::quantile(4,p,prob,0,p0,lower_tail,log_p);}
template <typename Rng> inline double rzmlogarithmic(Rng &rng,double prob, double p0) {return qzmlogarithmic(rng.runif(0,1),prob, p0);}
} }
