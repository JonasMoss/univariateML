// Contract, retained choices, evidence and known numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
// Pure-libm scalar density helpers for the skew-symmetric family
// (snorm, std, sstd, ged, sged) plus the small bag of utility helpers
// (sign1, heaviside, ged_lambda, ged_g, *_sigma) shared by the estimator
// path and the per-distribution dpqr.hpp templates.
//
// `log_d*_unit` / `log_d*_core` are closed-form log-densities used by the
// BFGS estimator path — independent of the math backend so the standalone
// fit only depends on <cmath>.

#pragma once

#include <cmath>

namespace cppunivariate {
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
  // m1 = E|X| where X is the variance-standardised Student-t with df=nu
  // (var(X) = 1). The skew construction needs the |.|-mean of the
  // *standardised* base, not the raw t. Pre-2026-05 this used
  // sqrt(nu/pi) which is E|T| of raw t — gave wrong dsstd / psstd /
  // qsstd values (and a self-consistent but biased mlsstd golden).
  // lgamma, not a tgamma ratio: tgamma overflows for nu > 343.
  const double m1 = std::exp(std::lgamma(0.5 * (nu - 1.0)) - std::lgamma(0.5 * nu)) *
                    std::sqrt((nu - 2.0) / M_PI);
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

// log of the unit-variance t density at 0: the z-free part of log_dstd_unit.
inline double log_dstd_unit_const(double nu) {
  // log Gamma(z+1/2)/Gamma(z) = log(z)/2 - 1/(8z)
  // + 1/(192z^3) - 1/(640z^5) + O(z^-7). Use at nu >= 128
  // to avoid lgamma cancellation; see the contract note for accuracy limits.
  const double inv = 1.0 / nu;
  const double ratio = nu >= 128.0
      ? 0.5 * std::log(0.5 * nu) - 0.25 * inv +
            inv * inv * inv / 24.0 - std::pow(inv, 5) / 20.0
      : std::lgamma(0.5 * (nu + 1.0)) - std::lgamma(0.5 * nu);
  return ratio - 0.5 * std::log((nu - 2.0) * M_PI);
}

inline double log_dstd_unit(double z, double nu) {
  return log_dstd_unit_const(nu) -
         0.5 * (nu + 1.0) * std::log1p(z * z / (nu - 2.0));
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
}  // namespace cppunivariate
