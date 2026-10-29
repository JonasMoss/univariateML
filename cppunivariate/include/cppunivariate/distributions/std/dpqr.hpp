// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline double dstd(double x, double mean, double sd, double nu, bool log) {
  if (!(fs::valid(mean, sd, nu, 1.0) && nu > 2.0) || std::isnan(x)) return cppunivariate::special::kNaN;
  const double z = (x - mean)/sd;
  const double ld = fs::std_logdensity(z, nu) - std::log(sd);
  return log ? ld : std::exp(ld);
}
inline double pstd(double x, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0) && nu > 2.0) || std::isnan(x)) return cppunivariate::special::kNaN;
  return cppunivariate::special::fs_t_p((x-mean)/sd*std::sqrt(nu/(nu-2.0)), nu, lower_tail, log_p);
}
inline double qstd(double p, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0) && nu > 2.0)) return cppunivariate::special::kNaN;
  const double lp = fs::logprob(p, log_p);
  if (std::isnan(lp)) return lp;
  return mean + sd/std::sqrt(nu/(nu-2.0))*cppunivariate::special::fs_t_q(lp, nu, lower_tail);
}
template <typename Rng>
inline double rstd(Rng &rng, double mean, double sd, double nu) {
  if (!(fs::valid(mean, sd, nu, 1.0) && nu > 2.0)) return cppunivariate::special::kNaN;
  const double z = rng.rt(nu);
  const double scale = std::sqrt(nu / (nu - 2.0));
  return z / scale * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
