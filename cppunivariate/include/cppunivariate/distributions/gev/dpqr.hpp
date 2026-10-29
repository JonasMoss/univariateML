#pragma once
#include "mle.hpp"
namespace cppunivariate { namespace distributions {
inline bool gev_valid(double mu,double sigma,double xi) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0 && std::isfinite(xi);
}
inline double dgev(double x,double mu,double sigma,double xi,bool log) {
  if(!gev_valid(mu,sigma,xi) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double ll=-std::numeric_limits<double>::infinity();
  if(std::isfinite(x) && 1+xi*((x-mu)/sigma)>0) {
    const double L=gev_log_ratio((x-mu)/sigma,xi);
    ll=-std::log(sigma)-(1+xi)*L-std::exp(-L);
  }
  return log ? ll : std::exp(ll);
}
inline double pgev(double x,double mu,double sigma,double xi,bool lower_tail,bool log_p) {
  if(!gev_valid(mu,sigma,xi) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  const double z=(x-mu)/sigma;
  double L;
  if(!std::isfinite(x) || 1+xi*z<=0) L=x<mu ? -std::numeric_limits<double>::infinity() : std::numeric_limits<double>::infinity();
  else L=gev_log_ratio(z,xi);
  const double a=std::exp(-L);
  if(lower_tail) return log_p ? -a : std::exp(-a);
  if(log_p && L>36) return -L;
  return log_p ? std::log(-std::expm1(-a)) : -std::expm1(-a);
}
inline double qgev(double p,double mu,double sigma,double xi,bool lower_tail,bool log_p) {
  if(!gev_valid(mu,sigma,xi) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
  const double lp=lower_tail ? (log_p ? p : std::log(p)) : (log_p ? std::log(-std::expm1(p)) : std::log1p(-p));
  if(lp==0) return xi<0 ? mu-sigma/xi : std::numeric_limits<double>::infinity();
  if(std::isinf(lp)) return xi>0 ? mu-sigma/xi : -std::numeric_limits<double>::infinity();
  const double y=-std::log(-lp);
  return mu+sigma*(xi==0 ? y : std::expm1(xi*y)/xi);
}
template <typename Rng> inline double rgev(Rng &rng,double mu,double sigma,double xi) {
  return qgev(rng.runif(0,1),mu,sigma,xi,true,false);
}
} }
