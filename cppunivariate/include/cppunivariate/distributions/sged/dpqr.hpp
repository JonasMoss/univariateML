// Package-defined, mean/standard-deviation parameterisation.
#pragma once
#include "../snorm/fs.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"
#include "../ged/dpqr.hpp"
namespace cppunivariate {
namespace distributions {
inline double dsged(double x, double mean, double sd, double nu, double xi, bool log) {
  if (!(fs::valid(mean, sd, nu, xi)) || std::isnan(x)) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double z = (x - mean)/sd*sigma + mu;
  const double ld = (std::log(2.0/(xi+1.0/xi)) + fs::ged_logdensity(z/(z<0.0 ? 1.0/xi : xi), nu)) - std::log(sd) + std::log(sigma);
  return log ? ld : std::exp(ld);
}
inline double psged(double q, double mean, double sd, double nu, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, nu, xi)) || std::isnan(q)) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double z = (q - mean)/sd*sigma + mu;
  return fs::skew_p(z, xi, lower_tail, log_p, [&](double v) { return pged(v, 0.0, 1.0, nu, true, true); });
}
inline double qsged(double p, double mean, double sd, double nu, double xi, bool lower_tail=true, bool log_p=false) {
  if (!(fs::valid(mean, sd, nu, xi))) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double z = fs::skew_q(p, xi, lower_tail, log_p, [&](double v) { return qged(v, 0.0, 1.0, nu, true, true); });
  return mean + sd*(z-mu)/sigma;
}
template <typename Rng>
inline double rsged(Rng &rng, double mean, double sd, double nu, double xi) {
  if (!(fs::valid(mean, sd, nu, xi))) return cppunivariate::special::kNaN;
  double mu = 0.0;
  const double sigma = fs::sged_sigma(nu, xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  const double z = rng.runif(-weight, 1.0 - weight);
  const double Xi = std::pow(xi, cppunivariate::dens::sign1(z));
  const double base = -std::fabs(cppunivariate::distributions::rged(rng, 0.0, 1.0,
                                                             nu)) /
                      Xi * cppunivariate::dens::sign1(z);
  const double scaled = (base - mu) / sigma;
  return scaled * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
