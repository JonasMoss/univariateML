// Contract, retained choices, evidence and known numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
// Scalar density helpers for the skew-symmetric family
// (snorm, std, sstd, ged, sged) plus the small bag of utility helpers
// (sign1, heaviside, ged_lambda, ged_g, *_sigma) shared by the estimator
// path and the per-distribution dpqr.hpp templates.
//
// `log_d*_unit` / `log_d*_core` are closed-form log-densities used by the
// BFGS estimator path — using the shared no-throw special-function seam.

#pragma once

#include "cppunivariate/core/constants.hpp"
#include "special.hpp"
#include <cmath>

namespace cppunivariate {
namespace dens {

// --- shared helpers --------------------------------------------------------

inline double sign1(double x) { return (x > 0) - (x < 0); }

inline double heaviside(double x) {
  return x > 0 ? 1.0 : (x < 0 ? 0.0 : 0.5);
}

inline double ged_logscale(double nu) {
  return -0.5*special::ged_log_gamma_ratio(nu, 3.0);
}
inline double ged_abs_moment(double nu) {
  if (nu >= 1.0)
    return (std::sqrt(3.0)/2.0)*std::exp(special::lgamma1p(2.0/nu)
        -0.5*(special::lgamma1p(1.0/nu)+special::lgamma1p(3.0/nu)));
  return std::exp(special::ged_log_gamma_ratio(nu, 2.0)+ged_logscale(nu));
}
inline double ged_lambda(double nu) {
  return std::exp(ged_logscale(nu)-std::log(2.0)/nu);
}
inline double ged_g(double nu, double lambda) {
  if (nu >= 1.0)
    return std::exp(-std::log(2.0)-std::log(lambda)-std::log(2.0)/nu
                   -special::lgamma1p(1.0/nu));
  return nu / (lambda * std::pow(2.0, 1.0 + 1.0 / nu) * std::tgamma(1.0 / nu));
}

// E|Z| for the variance-one Student t. At small nu use the backend ratio;
// at large nu combine the square roots analytically before evaluation.
inline double std_abs_moment_from_excess(double m) {
  if (m < 127.0)
    return special::gamma_delta_ratio(0.5*(m+1.0), 0.5)
        *std::sqrt(m/cppunivariate::pi);
  const double v=m+1.0;
  return std::sqrt(2.0/cppunivariate::pi)*std::sqrt(1.0-1.0/v)
      *std::exp(-special::log_half_gamma_correction(v));
}

inline double std_abs_moment(double nu) {
  return std_abs_moment_from_excess(nu-2.0);
}

// Skew-normal scale/shift: returns sigma; writes mu via out param.
inline double snorm_sigma(double xi, double &mu_out) {
  const double m1 = 2.0 / std::sqrt(2.0 * cppunivariate::pi);
  mu_out = m1 * (xi - 1.0 / xi);
  return std::sqrt((1.0 - m1 * m1) * (xi * xi + 1.0 / (xi * xi)) +
                   2.0 * m1 * m1 - 1.0);
}

inline double sstd_sigma(double nu, double xi, double &mu_out) {
  const double m1 = std_abs_moment(nu);
  mu_out = m1 * (xi - 1.0 / xi);
  return std::sqrt((1.0 - m1 * m1) * (xi * xi + 1.0 / (xi * xi)) +
                   2.0 * m1 * m1 - 1.0);
}

inline double sged_sigma(double nu, double xi, double &mu_out) {
  const double m1 = ged_abs_moment(nu);
  mu_out = m1 * (xi - 1.0 / xi);
  return std::sqrt((1.0 - m1 * m1) * (xi * xi + 1.0 / (xi * xi)) +
                   2.0 * m1 * m1 - 1.0);
}

// --- unit log-densities (mean=0, sd=1) ---------------------------

inline double log_dnorm_unit(double z) {
  static const double kLog2Pi = std::log(2.0 * cppunivariate::pi);
  return -0.5 * kLog2Pi - 0.5 * z * z;
}

inline double log_dged_unit(double z, double nu) {
  const double lambda = ged_lambda(nu);
  const double g = ged_g(nu, lambda);
  return std::log(g) - 0.5 * std::pow(std::fabs(z / lambda), nu);
}

// log of the unit-variance t density at 0: the z-free part of log_dstd_unit.
inline double log_dstd_unit_const(double nu) {
  if (nu >= 128.0)
    return special::log_t_mode(nu)-0.5*std::log1p(-2.0/nu);
  return -std::log(special::gamma_delta_ratio(0.5*nu,0.5))
      -0.5*(std::log(nu-2.0)+std::log(cppunivariate::pi));
}

inline double log_dstd_unit(double z, double nu) {
  return log_dstd_unit_const(nu) -
         0.5 * (nu + 1.0) * std::log1p(z * z / (nu - 2.0));
}

// --- skew log-density cores ---------------------------------------
// Each takes z already standardised to (mean=0, sd=1) frame, returns log f.

inline double log_dsnorm_core(double z, double xi) {
  const double Xi = std::pow(xi, sign1(z));
  const double log_g = std::log(2.0 / (xi + 1.0 / xi));
  return log_g + log_dnorm_unit(z / Xi);
}

inline double log_dsstd_core(double z, double nu, double xi) {
  const double Xi = std::pow(xi, sign1(z));
  const double log_g = std::log(2.0 / (xi + 1.0 / xi));
  return log_g + log_dstd_unit(z / Xi, nu);
}

inline double log_dsged_core(double z, double nu, double xi) {
  const double Xi = std::pow(xi, sign1(z));
  const double log_g = std::log(2.0 / (xi + 1.0 / xi));
  return log_g + log_dged_unit(z / Xi, nu);
}

}  // namespace dens
}  // namespace cppunivariate
