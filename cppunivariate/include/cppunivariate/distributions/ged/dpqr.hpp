// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline double dged(double x, double mean, double sd, double nu, bool log) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(x)) return cppunivariate::special::kNaN;
  const double z = (x - mean)/sd;
  const double ld = fs::ged_logdensity(z, nu) - std::log(sd);
  return log ? ld : std::exp(ld);
}
inline double pged(double x, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0)) || std::isnan(x)) return cppunivariate::special::kNaN;
  const double z = (x-mean)/sd;
  const double a = std::exp(nu*(std::log(std::fabs(z))-fs::ged_logscale(nu)));
  const double small = -std::log(2.0) + cppunivariate::special::fs_gamma_p(a, 1.0/nu, false, true);
  const double result = ((z < 0.0) == lower_tail) ? small : fs::log1mexp(small);
  return log_p ? result : std::exp(result);
}
inline double qged(double p, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0))) return cppunivariate::special::kNaN;
  const double lp = fs::logprob(p, log_p);
  if (std::isnan(lp)) return lp;
  const bool small = lp <= -std::log(2.0);
  const double tail = (small ? lp : fs::log1mexp(lp)) + std::log(2.0);
  const double a = cppunivariate::special::fs_gamma_q(tail, 1.0/nu, false);
  const double magnitude = std::exp(fs::ged_logscale(nu)+std::log(a)/nu);
  return mean + sd*((small == lower_tail) ? -magnitude : magnitude);
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
