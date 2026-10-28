#pragma once
#include "mle.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline bool gpd_valid(double mu, double sigma, double xi) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0 && std::isfinite(xi);
}
inline double dgpd(double x, double mu, double sigma, double xi, bool log) {
  if (!gpd_valid(mu,sigma,xi) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double ll=-std::numeric_limits<double>::infinity();
  if (x>mu && std::isfinite(x) && 1+xi*((x-mu)/sigma)>0)
    ll=-std::log(sigma)-(1+xi)*gpd_log_ratio((x-mu)/sigma,xi);
  return log ? ll : std::exp(ll);
}
inline double pgpd(double x, double mu, double sigma, double xi, bool lower_tail, bool log_p) {
  if (!gpd_valid(mu,sigma,xi) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double ls=0;
  if (x>mu) ls=(!std::isfinite(x) || 1+xi*((x-mu)/sigma)<=0) ?
      -std::numeric_limits<double>::infinity() : -gpd_log_ratio((x-mu)/sigma,xi);
  if (!lower_tail) return log_p ? ls : std::exp(ls);
  return log_p ? (ls < -0.6931471805599453 ? std::log1p(-std::exp(ls)) : std::log(-std::expm1(ls))) : -std::expm1(ls);
}
inline double qgpd(double p, double mu, double sigma, double xi, bool lower_tail, bool log_p) {
  if (!gpd_valid(mu,sigma,xi) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1)))
    return std::numeric_limits<double>::quiet_NaN();
  const double ls=lower_tail ? (log_p ? std::log(-std::expm1(p)) : std::log1p(-p)) : (log_p ? p : std::log(p));
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
