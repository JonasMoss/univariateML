// Skew normal (Fernandez-Steel form), parameterised by (mean, sd, xi).
// xi > 0 is the skew parameter; xi == 1 recovers the symmetric normal.

#pragma once

#include "../special/constants.hpp"
#include "../special/skewed_densities.hpp"
#include "../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;

namespace detail {
inline double dsnorm_core(double z, double xi) {
  const double Xi = std::pow(xi, cppunivariate::dens::sign1(z));
  const double g = 2.0 / (xi + 1.0 / xi);
  return g * cppunivariate::special::dnorm(z / Xi, 0.0, 1.0, /*log=*/false);
}
} // namespace detail

inline double dsnorm(double x, double mean, double sd, double xi, bool log) {
  if (!(sd > 0.0 && xi > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double z = (x - mean) / sd * sigma + mu;
  const double dens = detail::dsnorm_core(z, xi) * sigma / sd;
  return log ? std::log(dens) : dens;
}

inline double psnorm(double q, double mean, double sd, double xi) {
  if (!(sd > 0.0 && xi > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double z = (q - mean) / sd * sigma + mu;
  const double sig = (z >= 0.0) ? 1.0 : -1.0;
  const double Xi = std::pow(xi, sig);
  const double g = 2.0 / (xi + 1.0 / xi);
  const double base = cppunivariate::special::pnorm(-std::fabs(z) / Xi, 0.0, 1.0,
                                             /*lower_tail=*/true,
                                             /*log_p=*/false);
  return (z >= 0.0 ? 1.0 : 0.0) - sig * g * Xi * base;
}

inline double qsnorm(double p, double mean, double sd, double xi) {
  if (!(sd > 0.0 && xi > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  double mu = 0.0;
  const double sigma = cppunivariate::dens::snorm_sigma(xi, mu);
  const double g = 2.0 / (xi + 1.0 / xi);
  const double pxi = p - (1.0 / (1.0 + xi * xi));
  const double sig = cppunivariate::dens::sign1(pxi);
  const double Xi = std::pow(xi, sig);
  const double adj_p = (cppunivariate::dens::heaviside(pxi) - sig * p) / (g * Xi);
  const double q = cppunivariate::special::qnorm(adj_p, 0.0, Xi, /*lower_tail=*/true,
                                          /*log_p=*/false);
  return (-sig * q - mu) * sd / sigma + mean;
}

template <typename Rng>
inline double rsnorm(Rng &rng, double mean, double sd, double xi) {
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
