// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include <limits>
#include "tails.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline double dged(double x, double mean, double sd, double nu, bool log) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(x)) return cppunivariate::special::kNaN;
  const double z = (x - mean)/sd;
  const double ls=fs::ged_logscale(nu);
  if(nu>=1e6) {
    const double w=fs::ged_log_radius(z,nu,ls);
    if(!fs::ged_radius_precise(std::abs(z),w,nu,fs::ged_input_error(x,mean,sd,z)))
      return special::kNaN;
    const double ld=-std::log(2.0)-ls-special::lgamma1p(1/nu)-std::log(sd)-std::exp(nu*w);
    return log?ld:std::exp(ld);
  }
  const double ld = (std::isinf(z) || (x != mean && std::fabs(z) < std::numeric_limits<double>::min())) && std::isfinite(x)
      ? -std::log(2.0)-ls-std::lgamma(1+1/nu)-std::log(sd)
          -std::exp(nu*(special::log_abs_difference(x,mean)-std::log(sd)-ls))
      : fs::ged_logdensity(z, nu) - std::log(sd);
  return log ? ld : std::exp(ld);
}
inline double pged(double q, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(q)) return cppunivariate::special::kNaN;
  double la;
  if(nu>=1e6) {
    const double z=(q-mean)/sd;
    la=fs::ged_log_radius(z,nu,fs::ged_logscale(nu));
    if(!fs::ged_radius_precise(std::abs(z),la,nu,fs::ged_input_error(q,mean,sd,z)))
      return special::kNaN;
  } else la=special::log_abs_difference(q,mean)-std::log(sd)-fs::ged_logscale(nu);
  const double small = -std::log(2.0) + ged_detail::gamma_power_log(la,nu,false);
  const double result = ((q < mean) == lower_tail) ? small : fs::log1mexp(small);
  return log_p ? result : std::exp(result);
}
inline double qged(double p, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  const double lp = fs::logprob(p, log_p);
  if (std::isnan(lp)) return lp;
  const double h=log_p ? (p+std::log(2.0))+2.3190468138462995584e-17 : 0;
  if ((!log_p && std::abs(p-0.5)<1e-8) || (log_p && std::abs(h)<1e-8)) {
    if(!log_p && p==0.5) return mean;
    const double mass=log_p ? std::expm1(h) : 2*(p-0.5);
    const double la=ged_detail::gamma_power_logq(std::log(std::abs(mass)),nu,true);
    const double magnitude=std::exp(std::log(sd)+fs::ged_logscale(nu)+la);
    return mean+((mass>0)==lower_tail?magnitude:-magnitude);
  }
  const bool small = lp <= -std::log(2.0);
  const double tail = (small ? lp : fs::log1mexp(lp)) + std::log(2.0);
  const double la = ged_detail::gamma_power_logq(tail,nu,false);
  const double magnitude = std::exp(std::log(sd)+fs::ged_logscale(nu)+la);
  return mean + ((small == lower_tail) ? -magnitude : magnitude);
}
template <typename Rng>
inline double rged(Rng &rng, double mean, double sd, double nu) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  // Gamma augmentation: G_a = G_(a+1) U^(1/a). Apply the
  // GED power before exponentiation so U survives even for a near zero.
  const double a=1/nu;
  double logamplitude;
  if(nu>=16) {
    const double g=rng.rgamma(1+a,1);
    if(!(g>0) || !std::isnormal(g)) return special::kNaN;
    const double u=rng.runif(0,1);
    logamplitude=std::log(g)*a+std::log(u);
  } else {
    // Preserve the ordinary-shape primitive stream. A lost gamma draw is
    // a numerical failure, rather than a spurious atom at the center.
    const double r=rng.rgamma(a,1);
    if(!(r>0) || !std::isnormal(r)) return special::kNaN;
    logamplitude=std::log(r)/nu;
  }
  const double sign = cppunivariate::dens::sign1(rng.runif(0.0, 1.0) - 0.5);
  const double z = std::exp(fs::ged_logscale(nu) + logamplitude) * sign;
  return z * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
