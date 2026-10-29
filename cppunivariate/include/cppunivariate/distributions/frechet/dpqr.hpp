#pragma once
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dfrechet(double x,double lambda,double mu,double sigma,bool log=false) {
 if(!(lambda>0) || !(sigma>0) || !std::isfinite(lambda) || !std::isfinite(mu) || !std::isfinite(sigma) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
 if(x<=mu || std::isinf(x)) return log ? -std::numeric_limits<double>::infinity() : 0;
 const double z=std::log(x-mu)-std::log(sigma);
 const double ll=std::log(lambda)-std::log(sigma)-(lambda+1)*z-std::exp(-lambda*z);
 return log ? ll : std::exp(ll);
}
} }
