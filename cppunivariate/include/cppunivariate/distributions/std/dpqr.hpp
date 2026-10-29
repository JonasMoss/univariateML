// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../lst/tails.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline double dstd(double x, double mean, double sd, double nu, bool log) {
  if (!(fs::valid(mean, sd, nu, 1.0) && nu > 2.0) || std::isnan(x)) return cppunivariate::special::kNaN;
  const double z = (x - mean)/sd;
  const double logscale=std::log(sd)+0.5*(std::log(nu-2.0)-std::log(nu));
  const double ld = std::isinf(z) && std::isfinite(x)
      ? student_detail::logdensity_log(special::log_abs_difference(x,mean)-logscale,nu)-logscale
      : fs::std_logdensity(z, nu) - std::log(sd);
  return log ? ld : std::exp(ld);
}
inline double pstd(double x, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0) && nu > 2.0) || std::isnan(x)) return cppunivariate::special::kNaN;
  return student_detail::probability_log(special::log_abs_difference(x,mean)-std::log(sd)+0.5*(std::log(nu)-std::log(nu-2.0)),x<mean,nu,lower_tail,log_p);
}
inline double qstd(double p, double mean, double sd, double nu, bool lower_tail, bool log_p) {
  if (!(fs::valid(mean, sd, nu, 1.0) && nu > 2.0)) return cppunivariate::special::kNaN;
  return mean + student_detail::quantile(p,nu,lower_tail,log_p,std::log(sd)+0.5*(std::log(nu-2.0)-std::log(nu)));
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
