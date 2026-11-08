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
  const double ld = (std::isinf(z) || (x != mean && std::fabs(z) < std::numeric_limits<double>::min())) && std::isfinite(x)
      ? -std::log(2.0)-ls-std::lgamma(1+1/nu)-std::log(sd)
          -std::exp(nu*(special::log_abs_difference(x,mean)-std::log(sd)-ls))
      : fs::ged_logdensity(z, nu) - std::log(sd);
  return log ? ld : std::exp(ld);
}
inline double pged(double q, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(q)) return cppunivariate::special::kNaN;
  const double la = nu*(special::log_abs_difference(q,mean)-std::log(sd)-fs::ged_logscale(nu));
  const double small = -std::log(2.0) + ged_detail::gamma_log(la,1.0/nu,false);
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
    const double la=ged_detail::gamma_logq(std::log(std::abs(mass)),1.0/nu,true);
    const double magnitude=std::exp(std::log(sd)+fs::ged_logscale(nu)+la/nu);
    return mean+((mass>0)==lower_tail?magnitude:-magnitude);
  }
  const bool small = lp <= -std::log(2.0);
  const double tail = (small ? lp : fs::log1mexp(lp)) + std::log(2.0);
  const double la = ged_detail::gamma_logq(tail,1.0/nu,false);
  const double magnitude = std::exp(std::log(sd)+fs::ged_logscale(nu)+la/nu);
  return mean + ((small == lower_tail) ? -magnitude : magnitude);
}
template <typename Rng>
inline double rged(Rng &rng, double mean, double sd, double nu) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  const double r = rng.rgamma(1.0 / nu, 1.0);
  const double sign = cppunivariate::dens::sign1(rng.runif(0.0, 1.0) - 0.5);
  const double z = std::exp(fs::ged_logscale(nu) + std::log(r)/nu) * sign;
  return z * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
