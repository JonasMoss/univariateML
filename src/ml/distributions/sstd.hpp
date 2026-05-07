// Skew Student-t (Fernandez-Steel skew on the standardised t), parameterised
// by (mean, sd, nu, xi). nu > 2 (finite variance), xi > 0 (skew).

#pragma once

#include "../special/constants.hpp"
#include "../special/skewed_densities.hpp"
#include "std.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;

namespace detail {
inline double dsstd_core(double z, double nu, double xi) {
  const double Xi = std::pow(xi, mlcore::dens::sign1(z));
  const double g = 2.0 / (xi + 1.0 / xi);
  return g * mlcore::distributions::dstd(z / Xi, 0.0, 1.0, nu, /*log=*/false);
}
} // namespace detail

inline double dsstd(double x, double mean, double sd, double nu, double xi,
                    bool log) {
  if (!(sd > 0.0 && nu > 2.0 && xi > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  double mu = 0.0;
  const double sigma = mlcore::dens::sstd_sigma(nu, xi, mu);
  const double z = (x - mean) / sd * sigma + mu;
  const double dens = detail::dsstd_core(z, nu, xi) * sigma / sd;
  return log ? std::log(dens) : dens;
}

inline double psstd(double q, double mean, double sd, double nu, double xi) {
  if (!(sd > 0.0 && nu > 2.0 && xi > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double mu = 0.0;
  const double sigma = mlcore::dens::sstd_sigma(nu, xi, mu);
  const double z = (q - mean) / sd * sigma + mu;
  const double sig = (z >= 0.0) ? 1.0 : -1.0;
  const double Xi = std::pow(xi, sig);
  const double g = 2.0 / (xi + 1.0 / xi);
  const double base = mlcore::distributions::pstd(-std::fabs(z) / Xi, 0.0,
                                                  1.0, nu,
                                                  /*lower_tail=*/true,
                                                  /*log_p=*/false);
  return (z >= 0.0 ? 1.0 : 0.0) - sig * g * Xi * base;
}

inline double qsstd(double p, double mean, double sd, double nu, double xi) {
  if (!(sd > 0.0 && nu > 2.0 && xi > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  double mu = 0.0;
  const double sigma = mlcore::dens::sstd_sigma(nu, xi, mu);
  const double g = 2.0 / (xi + 1.0 / xi);
  const double pxi = p - (1.0 / (1.0 + xi * xi));
  const double sig = mlcore::dens::sign1(pxi);
  const double Xi = std::pow(xi, sig);
  const double adj_p = (mlcore::dens::heaviside(pxi) - sig * p) / (g * Xi);
  const double q = mlcore::distributions::qstd(adj_p, 0.0, 1.0, nu,
                                               /*lower_tail=*/true,
                                               /*log_p=*/false);
  return (-sig * q - mu) * sd / sigma + mean;
}

template <typename Rng>
inline double rsstd(Rng &rng, double mean, double sd, double nu, double xi) {
  double mu = 0.0;
  const double sigma = mlcore::dens::sstd_sigma(nu, xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  const double z = rng.runif(-weight, 1.0 - weight);
  const double Xi = std::pow(xi, mlcore::dens::sign1(z));
  const double base = -std::fabs(mlcore::distributions::rstd(rng, 0.0, 1.0,
                                                             nu)) /
                      Xi * mlcore::dens::sign1(z);
  const double scaled = (base - mu) / sigma;
  return scaled * sd + mean;
}

} // namespace distributions
} // namespace mlcore
