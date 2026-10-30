#pragma once
#include "dpqr_core.hpp"
namespace cppunivariate { namespace distributions {
inline double dtpois(double x,double lambda, double a, double b,bool log=false){return truncated_detail::evaluate(false,x,lambda,a,b,-1,true,log);}
inline double ptpois(double q,double lambda, double a, double b,bool lower_tail=true,bool log_p=false){return truncated_detail::evaluate(true,q,lambda,a,b,-1,lower_tail,log_p);}
inline double qtpois(double p,double lambda, double a, double b,bool lower_tail=true,bool log_p=false){return truncated_detail::quantile(p,lambda,a,b,-1,lower_tail,log_p);}
template <typename Rng> inline double rtpois(Rng &rng,double lambda, double a, double b){return qtpois(rng.runif(0,1),lambda,a,b);}
} }
