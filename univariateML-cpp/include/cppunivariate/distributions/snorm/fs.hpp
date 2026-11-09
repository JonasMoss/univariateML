// Shared Fernandez--Steel probability algebra; symmetric callbacks use log tails.
#pragma once
#include "../../special/constants.hpp"
#include "../../special/skewed_densities.hpp"
#include <cmath>
#include <algorithm>
#include <limits>
namespace cppunivariate {
namespace distributions {
namespace fs {
inline bool valid(double mean, double sd, double nu, double xi) {
  return std::isfinite(mean) && std::isfinite(sd) && sd > 0.0 &&
         std::isfinite(nu) && nu > 0.0 && std::isfinite(xi) && xi > 0.0;
}
inline double log1mexp(double x) {
  return x < -0.6931471805599453 ? std::log1p(-std::exp(x)) : std::log(-std::expm1(x));
}
inline double logprob(double p, bool log_p) {
  if (std::isnan(p) || (log_p ? p > 0.0 : (p < 0.0 || p > 1.0)))
    return cppunivariate::special::kNaN;
  return log_p ? p : std::log(p);
}
// Log gamma ratios extend GED evaluation below the fitter's nu >= 1 domain.
inline double ged_logscale(double nu) {
  const double result=cppunivariate::dens::ged_logscale(nu);
  return std::isfinite(result) ? result : cppunivariate::special::kNaN;
}
// log(|z|/sqrt(3)) retains the side of the limiting uniform edge even
// when log(|z|) and log(sqrt(3)) round to the same double. The gamma
// recurrence separates the small finite-shape correction from log(3)/2.
inline double ged_log_radius(double z,double nu,double logscale) {
  const double u=std::abs(z);
  if(nu>=1e6 && u>1 && u<2)
    return .5*std::log1p(std::fma(u,u,-3.0)/3.0)
        -.5*(special::lgamma1p(1/nu)-special::lgamma1p(3/nu));
  return std::log(u)-logscale;
}
// Practical forward-error gate for location/scale and skew transforms.
// Inside the edge, Q is of order max(1-u/lambda,1/nu). Outside it,
// accept an uncertain radius only when even its lower bound has an
// unrepresentable log tail. Exact unit inputs need no transform gate.
inline bool ged_radius_precise(double u,double w,double nu,double error) {
  if(nu<1e6 || error==0) return true;
  if(!std::isfinite(error) || std::isnan(w) || std::isnan(u)) return false;
  if(w<0) return error<=1e-11*std::max(-std::expm1(w),1/nu);
  if(!(u>error)) return false;
  const double log_error=std::log1p(error/(u-error));
  return nu*(w-log_error)>std::log(std::numeric_limits<double>::max())+2;
}
inline double ged_input_error(double x,double mean,double sd,double z) {
  if((mean==0 && sd==1) || x==mean || !std::isfinite(x)) return 0;
  return 32*std::numeric_limits<double>::epsilon()
      *(1+std::abs(z)+(std::abs(x)+std::abs(mean))/sd);
}
inline double ged_logdensity(double z, double nu) {
  const double ls=ged_logscale(nu);
  const double power=std::exp(nu*ged_log_radius(z,nu,ls));
  return (nu >= 1.0 ? -std::log(2.0)-ls-cppunivariate::special::lgamma1p(1.0/nu)
      : std::log(nu)-std::log(2.0)-ls-std::lgamma(1.0/nu))-power;
}
inline double std_logdensity(double z, double nu) {
  const double square=z*z/(nu-2.0);
  double term=std::log1p(square);
  if(std::isinf(square) && std::isfinite(z)) {
    const double scaled=std::abs(z)/std::sqrt(nu-2.0);
    const double scaled_square=scaled*scaled;
    if(std::isfinite(scaled_square)) term=std::log1p(scaled_square);
    else {
      const double t=2*std::log(std::abs(z))-std::log(nu-2.0);
      term=t>0 ? t+std::log1p(std::exp(-t)) : std::log1p(std::exp(t));
    }
  }
  return cppunivariate::dens::log_dstd_unit_const(nu)-0.5*(nu+1.0)*term;
}
inline double skew_sigma(double moment, double xi, double &mu) {
  mu=moment*(xi-1.0/xi);
  const double sigma=std::sqrt((1.0-moment*moment)*(xi*xi+1.0/(xi*xi))+2.0*moment*moment-1.0);
  return std::isfinite(sigma) ? sigma : special::kNaN;
}
inline double sstd_sigma(double nu, double xi, double &mu) {
  const double moment=cppunivariate::dens::std_abs_moment(nu);
  return skew_sigma(moment,xi,mu);
}
inline double sged_sigma(double nu, double xi, double &mu) {
  const double moment=cppunivariate::dens::ged_abs_moment(nu);
  return skew_sigma(moment,xi,mu);
}
template <typename SymmetricP>
inline double skew_p(double z, double xi, bool lower, bool log_p, SymmetricP base) {
  const bool left = z < 0.0;
  const double scale = left ? 1.0 / xi : xi;
  const double lp = std::log(2.0) - std::log(xi + 1.0/xi) + std::log(scale)
      + base(-std::fabs(z)/scale);
  // The small side is evaluated directly; only its large complement is subtracted.
  const double result = (left == lower) ? lp : log1mexp(lp);
  return log_p ? result : std::exp(result);
}
template <typename SymmetricQ>
inline double skew_q(double p, double xi, bool lower, bool log_p, SymmetricQ base) {
  const double lp = logprob(p, log_p);
  if (std::isnan(lp)) return lp;
  const double lc = log1mexp(lp);
  const double logg = std::log(2.0) - std::log(xi + 1.0/xi);
  const double lleft = logg - std::log(xi) - std::log(2.0);
  const double lright = logg + std::log(xi) - std::log(2.0);
  const bool left = lower ? lp <= lleft : lp >= lright;
  const double scale = left ? 1.0/xi : xi;
  const double side = (left == lower) ? lp : lc;
  const double q = base(side - logg - std::log(scale));
  return left ? scale*q : -scale*q;
}
} // namespace fs
} // namespace distributions
} // namespace cppunivariate
