// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline double dsnorm(double x, double mean, double sd, double xi, bool log) {
  if (!(fs::valid(mean, sd, 1.0, xi)) || std::isnan(x)) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double z = (x - mean)/sd*sigma + mu;
  const double ld = cppunivariate::dens::log_dsnorm_core(z, xi) - std::log(sd) + std::log(sigma);
  return log ? ld : std::exp(ld);
}
inline double psnorm(double q, double mean, double sd, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, 1.0, xi)) || std::isnan(q)) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double z = (q - mean)/sd*sigma + mu;
  return fs::skew_p(z, xi, lower_tail, log_p, [&](double v) { return cppunivariate::special::fs_normal_p(v, true, true); });
}
inline double qsnorm(double p, double mean, double sd, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, 1.0, xi))) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double z = fs::skew_q(p, xi, lower_tail, log_p, [&](double v) { return cppunivariate::special::fs_normal_q(v, true); });
  return mean + sd*(z-mu)/sigma;
}
template <typename Rng>
inline double rsnorm(Rng &rng, double mean, double sd, double xi) {
  if (!(fs::valid(mean, sd, 1.0, xi))) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  const double z = rng.runif(-weight, 1.0 - weight);
  const double Xi = std::pow(xi, cppunivariate::dens::sign1(z));
  const double base =
      -std::fabs(rng.rnorm()) / Xi * cppunivariate::dens::sign1(z);
  const double scaled = (base - mu) / sigma;
  return scaled * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
