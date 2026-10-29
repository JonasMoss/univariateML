#pragma once
#include "mle.hpp"
#include "../../special/tails.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline bool gpd_valid(double mu, double sigma, double xi) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0 && std::isfinite(xi);
}
inline double gpd_dpqr_ratio(double x,double mu,double sigma,double xi) {
  const double z=special::scaled_difference(x,mu,sigma), v=xi*z;
  if(xi==0) return z;
  if(std::abs(v)<1e-8) return z*(1-v/2+v*v/3);
  return std::abs(v)<0.5 ? std::log1p(v)/xi
      : std::log(special::support_residual(x,mu,sigma,xi))/xi;
}
inline double dgpd(double x, double mu, double sigma, double xi, bool log) {
  if (!gpd_valid(mu,sigma,xi) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double ll=-std::numeric_limits<double>::infinity();
  if (x>=mu && std::isfinite(x) && special::support_residual(x,mu,sigma,xi)>0)
    ll=-std::log(sigma)-(1+xi)*gpd_dpqr_ratio(x,mu,sigma,xi);
  return log ? ll : std::exp(ll);
}
inline double pgpd(double q, double mu, double sigma, double xi, bool lower_tail, bool log_p) {
  if (!gpd_valid(mu,sigma,xi) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
  double ls=0;
  if (q>mu) ls=(!std::isfinite(q) || special::support_residual(q,mu,sigma,xi)<=0) ?
      -std::numeric_limits<double>::infinity() : -gpd_dpqr_ratio(q,mu,sigma,xi);
  if (!lower_tail) return log_p ? ls : std::exp(ls);
  if (q>mu && -ls<std::numeric_limits<double>::min() && std::isfinite(q)) {
    const double lp=std::log(q-mu)-std::log(sigma);
    return log_p ? lp : std::exp(lp);
  }
  return log_p ? special::tails_from_log(ls,false).lower : -std::expm1(ls);
}
inline double qgpd(double p, double mu, double sigma, double xi, bool lower_tail, bool log_p) {
  if (!gpd_valid(mu,sigma,xi) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1)))
    return std::numeric_limits<double>::quiet_NaN();
  const auto tails=special::quantile_log_tails(p,lower_tail,log_p);
  const double ls=tails.upper;
  if (lower_tail && log_p && p < -36 && p+std::log(std::abs(xi)) < -36) return mu+std::exp(std::log(sigma)+p);
  if (ls==0) return mu;
  if (std::isinf(ls)) return xi<0 ? mu-sigma/xi : std::numeric_limits<double>::infinity();
  return mu+sigma*(xi==0 ? -ls : std::expm1(-xi*ls)/xi);
}
template <typename Rng>
inline double rgpd(Rng &rng, double mu, double sigma, double xi) {
  return qgpd(rng.runif(0,1),mu,sigma,xi,true,false);
}
} // namespace distributions
} // namespace cppunivariate
