// Generalised Error Distribution (GED), parameterised by (mean, sd, nu).
// nu is the shape parameter; nu=2 recovers the normal, nu=1 the Laplace.
//
// Density (centred, unit-sd):
//   f(z) = g * exp(-0.5 * |z/lambda|^nu)
// where lambda, g are functions of nu (cppunivariate::dens::ged_{lambda,g}).
//
// CDF / quantile derived via the relation Y = lambda * (2 * Gamma(1/nu, 1))^{1/nu}
// with sign uniform, where Gamma is the gamma distribution.

#pragma once

#include "../special/constants.hpp"
#include "../special/skewed_densities.hpp"
#include "../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;

inline double dged(double x, double mean, double sd, double nu, bool log) {
  if (!(sd > 0.0 && nu > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  const double z = (x - mean) / sd;
  const double lambda = cppunivariate::dens::ged_lambda(nu);
  const double g = cppunivariate::dens::ged_g(nu, lambda);
  const double expo = -0.5 * std::pow(std::fabs(z / lambda), nu);
  if (log) {
    return std::log(g) + expo - std::log(sd);
  }
  return g * std::exp(expo) / sd;
}

inline double pged(double q, double mean, double sd, double nu,
                   bool lower_tail, bool log_p) {
  if (!(sd > 0.0 && nu > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  const double z = (q - mean) / sd;
  const double lambda = cppunivariate::dens::ged_lambda(nu);
  const double g = cppunivariate::dens::ged_g(nu, lambda);
  const double h =
      std::pow(2.0, 1.0 / nu) * lambda * g * std::tgamma(1.0 / nu) / nu;
  const double s_term = 0.5 * std::pow(std::fabs(z) / lambda, nu);
  double res = 0.5 + cppunivariate::dens::sign1(z) * h *
                         cppunivariate::special::pgamma(s_term, 1.0 / nu, 1.0,
                                                 /*lower_tail=*/true,
                                                 /*log_p=*/false);
  if (!lower_tail) {
    res = 1.0 - res;
  }
  return log_p ? std::log(res) : res;
}

inline double qged(double p, double mean, double sd, double nu,
                   bool lower_tail, bool log_p) {
  if (!(sd > 0.0 && nu > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  const double lambda = cppunivariate::dens::ged_lambda(nu);
  const double qg = cppunivariate::special::qgamma(std::fabs(2.0 * p - 1.0),
                                            1.0 / nu, 1.0,
                                            /*lower_tail=*/true,
                                            /*log_p=*/false);
  const double q = lambda * std::pow(2.0 * qg, 1.0 / nu) *
                   cppunivariate::dens::sign1(2.0 * p - 1.0);
  return q * sd + mean;
}

template <typename Rng>
inline double rged(Rng &rng, double mean, double sd, double nu) {
  const double lambda = cppunivariate::dens::ged_lambda(nu);
  const double r = rng.rgamma(1.0 / nu, 1.0);
  const double sign = cppunivariate::dens::sign1(rng.runif(0.0, 1.0) - 0.5);
  const double z = lambda * std::pow(2.0 * r, 1.0 / nu) * sign;
  return z * sd + mean;
}

} // namespace distributions
} // namespace cppunivariate
