#pragma once
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dfrechet(double x,double lambda,double mu,double sigma,bool log) {
 if(!(lambda>0) || !(sigma>0) || !std::isfinite(lambda) || !std::isfinite(mu) || !std::isfinite(sigma) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
 if(x<=mu || std::isinf(x)) return log ? -std::numeric_limits<double>::infinity() : 0;
 const double z=std::log(x-mu)-std::log(sigma);
 const double ll=std::log(lambda)-std::log(sigma)-(lambda+1)*z-std::exp(-lambda*z);
 return log ? ll : std::exp(ll);
}
} }

#include "../invweibull/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double pfrechet(double q,double lambda,double mu,double sigma,bool lower_tail,bool log_p) {
 if(!(std::isfinite(mu) && lambda>0 && std::isfinite(lambda) && sigma>0 && std::isfinite(sigma))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 if(q<=mu) return log_p ? (lower_tail ? -std::numeric_limits<double>::infinity() : 0) : (lower_tail ? 0 : 1);
 if(q>=std::numeric_limits<double>::infinity()) return log_p ? (lower_tail ? 0 : -std::numeric_limits<double>::infinity()) : (lower_tail ? 1 : 0);
 return pinvweibull(q-mu,lambda,sigma,lower_tail,log_p);
}
inline double qfrechet(double p,double lambda,double mu,double sigma,bool lower_tail,bool log_p) {
 if(!(std::isfinite(mu) && lambda>0 && std::isfinite(lambda) && sigma>0 && std::isfinite(sigma))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 if(p==(log_p ? -std::numeric_limits<double>::infinity() : 0)) return lower_tail ? mu : std::numeric_limits<double>::infinity();
 if(p==(log_p ? 0 : 1)) return lower_tail ? std::numeric_limits<double>::infinity() : mu;
 return mu+qinvweibull(p,lambda,sigma,lower_tail,log_p);
}
template <typename Rng> inline double rfrechet(Rng &rng,double lambda,double mu,double sigma) {
 if(!(std::isfinite(mu) && lambda>0 && std::isfinite(lambda) && sigma>0 && std::isfinite(sigma))) return std::numeric_limits<double>::quiet_NaN();
 return mu+rinvweibull(rng,lambda,sigma);
}
} }
