// Pure-libm scalar density helpers for the skew-symmetric family
// (snorm, std, sstd, ged, sged) plus the small bag of utility helpers
// (sign1, heaviside, ged_lambda, ged_g, *_sigma) shared by the estimator
// path and the dpqr Rcpp wrappers in src/distributions/.
//
// Two layers:
//   * `*_sigma` and the pure helpers — pure libm. No Rmath dependency.
//     The dpqr cpps delegate to these (no behavior change vs the previous
//     static-inline copies).
//   * `log_d*_unit` / `log_d*_core` — pure-libm log-densities for the
//     standalone estimator path. Rederived via lgamma rather than Rf_dt
//     so the BFGS path doesn't pull in Rmath. NOT bit-equivalent to the
//     dpqr cpps (which keep using Rf_dt) — those goldens stay reproducible
//     because the cpp dpqr math is unchanged.

#pragma once

#include <cmath>

namespace mlcore {
namespace dens {

// --- shared helpers --------------------------------------------------------

inline double sign1(double x) { return (x > 0) - (x < 0); }

inline double heaviside(double x) {
  return x > 0 ? 1.0 : (x < 0 ? 0.0 : 0.5);
}

inline double ged_lambda(double nu) {
  return std::sqrt(std::pow(2.0, -2.0 / nu) * std::tgamma(1.0 / nu) /
                   std::tgamma(3.0 / nu));
}

inline double ged_g(double nu, double lambda) {
  return nu / (lambda * std::pow(2.0, 1.0 + 1.0 / nu) * std::tgamma(1.0 / nu));
}

// Skew-normal scale/shift: returns sigma; writes mu via out param.
inline double snorm_sigma(double xi, double &mu_out) {
  const double m1 = 2.0 / std::sqrt(2.0 * M_PI);
  mu_out = m1 * (xi - 1.0 / xi);
  return std::sqrt((1.0 - m1 * m1) * (xi * xi + 1.0 / (xi * xi)) +
                   2.0 * m1 * m1 - 1.0);
}

inline double sstd_sigma(double nu, double xi, double &mu_out) {
  const double m1 = std::tgamma(0.5 * (nu - 1.0)) / std::tgamma(0.5 * nu) *
                    std::sqrt(nu / M_PI);
  mu_out = m1 * (xi - 1.0 / xi);
  return std::sqrt((1.0 - m1 * m1) * (xi * xi + 1.0 / (xi * xi)) +
                   2.0 * m1 * m1 - 1.0);
}

inline double sged_sigma(double nu, double xi, double &mu_out) {
  const double lambda = ged_lambda(nu);
  const double m1 = std::pow(2.0, 1.0 / nu) * lambda *
                    std::tgamma(2.0 / nu) / std::tgamma(1.0 / nu);
  mu_out = m1 * (xi - 1.0 / xi);
  return std::sqrt((1.0 - m1 * m1) * (xi * xi + 1.0 / (xi * xi)) +
                   2.0 * m1 * m1 - 1.0);
}

// --- pure-libm unit log-densities (mean=0, sd=1) ---------------------------

inline double log_dnorm_unit(double z) {
  static const double kLog2Pi = std::log(2.0 * M_PI);
  return -0.5 * kLog2Pi - 0.5 * z * z;
}

inline double log_dged_unit(double z, double nu) {
  const double lambda = ged_lambda(nu);
  const double g = ged_g(nu, lambda);
  return std::log(g) - 0.5 * std::pow(std::fabs(z / lambda), nu);
}

inline double log_dstd_unit(double z, double nu) {
  // Rederived from Rf_dt(z*scale, nu, log=true) + log(scale), scale = sqrt(nu/(nu-2)).
  const double scale = std::sqrt(nu / (nu - 2.0));
  const double w = z * scale;
  const double log_dt = std::lgamma(0.5 * (nu + 1.0)) -
                        std::lgamma(0.5 * nu) -
                        0.5 * std::log(nu * M_PI) -
                        0.5 * (nu + 1.0) * std::log1p(w * w / nu);
  return log_dt + std::log(scale);
}

// --- pure-libm skew log-density cores ---------------------------------------
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
}  // namespace mlcore
