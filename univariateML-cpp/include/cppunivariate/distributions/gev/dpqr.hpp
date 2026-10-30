#pragma once
#include "mle.hpp"
#include "../../special/tails.hpp"
namespace cppunivariate { namespace distributions {
inline bool gev_valid(double mu,double sigma,double xi) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0 && std::isfinite(xi);
}
inline double gev_dpqr_ratio(double x,double mu,double sigma,double xi) {
  const double z=special::scaled_difference(x,mu,sigma), v=xi*z;
  if(xi==0) return z;
  if(std::abs(v)<1e-8) return z*(1-v/2+v*v/3);
  return std::abs(v)<0.5 ? std::log1p(v)/xi
      : std::log(special::support_residual(x,mu,sigma,xi))/xi;
}
inline double dgev(double x,double mu,double sigma,double xi,bool log) {
  if(!gev_valid(mu,sigma,xi) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double ll=-std::numeric_limits<double>::infinity();
  // R7a: at the finite upper endpoint, f -> 1/sigma for xi=-1,
  // zero for -1<xi<0, and infinity for xi<-1.
  if (std::isfinite(x) && ((xi==-1 && x==mu+sigma) ||
      (xi<0 && special::support_residual(x,mu,sigma,xi)==0))) {
    const double endpoint=xi==-1 ? -std::log(sigma) :
        (xi < -1 ? special::kPosInf : special::kNegInf);
    return log ? endpoint : std::exp(endpoint);
  }
  if(std::isfinite(x) && special::support_residual(x,mu,sigma,xi)>0) {
    const double L=gev_dpqr_ratio(x,mu,sigma,xi);
    ll=-std::log(sigma)-(1+xi)*L-std::exp(-L);
  }
  return log ? ll : std::exp(ll);
}
inline double pgev(double q,double mu,double sigma,double xi,bool lower_tail,bool log_p) {
  if(!gev_valid(mu,sigma,xi) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
  // Match the finite-density member's rounded quantile endpoint in both tails.
  if (xi==-1 && q==mu+sigma)
    return lower_tail ? (log_p ? 0 : 1) : (log_p ? special::kNegInf : 0);
  double L;
  if(!std::isfinite(q) || special::support_residual(q,mu,sigma,xi)<=0) L=q<mu ? -std::numeric_limits<double>::infinity() : std::numeric_limits<double>::infinity();
  else L=gev_dpqr_ratio(q,mu,sigma,xi);
  const double a=std::exp(-L);
  if(lower_tail) return log_p ? -a : std::exp(-a);
  if(log_p && L>36) return -L;
  return log_p ? std::log(-std::expm1(-a)) : -std::expm1(-a);
}
inline double qgev(double p,double mu,double sigma,double xi,bool lower_tail,bool log_p) {
  if(!gev_valid(mu,sigma,xi) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
  const auto tails=special::quantile_log_tails(p,lower_tail,log_p);
  if(tails.lower==special::kNegInf) return xi>0 ? mu-sigma/xi : special::kNegInf;
  if((lower_tail && p==(log_p?0:1)) || (!lower_tail && p==(log_p?special::kNegInf:0)))
    return xi<0 ? mu-sigma/xi : special::kPosInf;
  const double y=lower_tail ? -std::log(-(log_p?p:std::log(p)))
      : -special::log_neg_log1mexp(log_p?p:std::log(p));
  return mu+sigma*(xi==0 ? y : std::expm1(xi*y)/xi);
}
template <typename Rng> inline double rgev(Rng &rng,double mu,double sigma,double xi) {
  return qgev(rng.runif(0,1),mu,sigma,xi,true,false);
}
} }
