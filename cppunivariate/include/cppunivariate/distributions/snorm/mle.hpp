// Skew-normal MLE on (mu, sigma, xi).
//
// Reparam phi = (mu, log sigma, log xi). Pure-libm log-density; mirrors the
// snorm_sigma + log_dsnorm_core decomposition used by dsnorm() so that the
// fitted-parameter likelihood matches sum(dsnorm(..., log = TRUE)) up to
// the lgamma-vs-Rmath drift documented in skewed_densities.hpp.
//
// f(x; mu, sigma, xi) = (sigma_u / sigma) * f_core(z; xi),
//   where (sigma_u, mu_u) = snorm_sigma(xi),
//   z = (x - mu)/sigma * sigma_u + mu_u.

#pragma once

#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/reductions.hpp"
#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../../special/skewed_densities.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class SnormStatus { Ok = 0, BfgsFailure, InvalidStart };

struct SnormResult {
  double mu;
  double sigma;
  double xi;
  double logLik;
  int iterations;
  SnormStatus status;
};

// Per-obs nll in phi-space (mu, log sigma, log xi):
//   nll_per = phi[1] - log sigma_u(xi) + log(xi + 1/xi) - log 2
//             + (1/2) log(2 pi) + (1/2) z^2 / chi^2
// where z = (x - mu)/sigma * sigma_u(xi) + mu_u(xi),
//       chi = xi^sign(z), and the (sigma_u, mu_u, log_g) functions
//       come from the Fernandez-Steel standardisation.
//
// The Hessian factors as:
//   H_total = sum_i 0.5 T_i (p_i p_i^T + h_i)  +  n * d^2 D / d lx^2 e_22
// where T_i = z_i^2 / chi_i^2, p_i = grad (log T_i), h_i is the
// Hessian of log T_i wrt phi, and D(xi) = -log sigma_u + log_g
// gathers the xi-only constant factor (only its lx,lx entry is
// non-zero). Sympy-derived; numerically verified against FD.
struct SnormCost {
  span x;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double xi = cppunivariate::from_log(phi[2]);
    if (!std::isfinite(sigma) || !std::isfinite(xi) || sigma <= 0.0 ||
        xi <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    double mu_u = 0.0;
    const double sigma_u = cppunivariate::dens::snorm_sigma(xi, mu_u);
    const double log_factor = std::log(sigma_u) - std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma * sigma_u + mu_u;
      neg -= log_factor + cppunivariate::dens::log_dsnorm_core(z, xi);
    }
    return neg;
  }
  void hessian(const double *phi, double *H) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double xi = cppunivariate::from_log(phi[2]);
    if (!std::isfinite(sigma) || !std::isfinite(xi) || sigma <= 0.0 ||
        xi <= 0.0) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      for (int i = 0; i < 9; ++i) H[i] = nan;
      return;
    }
    const double m1 = 2.0 / std::sqrt(2.0 * M_PI);
    const double xi2 = xi * xi;
    const double inv_xi2 = 1.0 / xi2;
    const double mu_u = m1 * (xi - 1.0 / xi);
    const double sigma_u_sq =
        (1.0 - m1 * m1) * (xi2 + inv_xi2) + 2.0 * m1 * m1 - 1.0;
    const double sigma_u = std::sqrt(sigma_u_sq);
    const double m_prime = m1 * (xi + 1.0 / xi);
    const double m_pprime = mu_u;  // = m1*(xi - 1/xi)
    const double s_prime =
        (1.0 - m1 * m1) * (xi2 - inv_xi2) / sigma_u;
    const double s_pprime =
        (2.0 * (1.0 - m1 * m1) * (xi2 + inv_xi2) - s_prime * s_prime) /
        sigma_u;
    const double e_s = s_prime / sigma_u;
    const double r_xi = xi + 1.0 / xi;
    const double g_pprime = -4.0 / (r_xi * r_xi);
    const double Q_const = s_pprime / sigma_u - e_s * e_s;
    const double B_const = m_pprime - (s_pprime / sigma_u) * mu_u +
                           2.0 * e_s * e_s * mu_u -
                           2.0 * e_s * m_prime;
    const double Cval = m_prime - e_s * mu_u;
    const double Cval2 = Cval * Cval;
    for (int i = 0; i < 9; ++i) H[i] = 0.0;
    for (auto v : x) {
      const double a = v - mu;
      const double z = a * sigma_u / sigma + mu_u;
      if (z == 0.0) continue;
      const double alpha = (z > 0.0) ? -2.0 : 2.0;
      const double chi2_inv = (z > 0.0) ? inv_xi2 : xi2;
      const double T = z * z * chi2_inv;
      const double inv_z = 1.0 / z;
      const double inv_z2 = inv_z * inv_z;
      const double p_mu = -2.0 * sigma_u / (sigma * z);
      const double p_ls = -2.0 + 2.0 * mu_u * inv_z;
      const double p_lx = 2.0 * e_s + 2.0 * Cval * inv_z + alpha;
      const double h_mu_mu =
          -2.0 * sigma_u * sigma_u / (sigma * sigma) * inv_z2;
      const double h_mu_ls = 2.0 * sigma_u * mu_u / sigma * inv_z2;
      const double h_mu_lx =
          -2.0 * (s_prime * mu_u - sigma_u * m_prime) / sigma * inv_z2;
      const double h_ls_ls = 2.0 * mu_u * (z - mu_u) * inv_z2;
      const double h_ls_lx =
          2.0 * (z - mu_u) * (m_prime - mu_u * e_s) * inv_z2;
      const double h_lx_lx =
          2.0 * Q_const + 2.0 * B_const * inv_z - 2.0 * Cval2 * inv_z2;
      H[0] += 0.5 * T * (p_mu * p_mu + h_mu_mu);
      const double off_01 = 0.5 * T * (p_mu * p_ls + h_mu_ls);
      H[1] += off_01;
      H[3] += off_01;
      const double off_02 = 0.5 * T * (p_mu * p_lx + h_mu_lx);
      H[2] += off_02;
      H[6] += off_02;
      H[4] += 0.5 * T * (p_ls * p_ls + h_ls_ls);
      const double off_12 = 0.5 * T * (p_ls * p_lx + h_ls_lx);
      H[5] += off_12;
      H[7] += off_12;
      H[8] += 0.5 * T * (p_lx * p_lx + h_lx_lx);
    }
    const double dn = static_cast<double>(x.size());
    // d^2 D / d lx^2 = -Q_const - g_pprime
    H[8] += dn * (-Q_const - g_pprime);
  }
};

inline SnormResult snorm_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, kNaN, 0, SnormStatus::InvalidStart};

  const double mean_x = cppunivariate::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, 0, SnormStatus::InvalidStart};
  }

  SnormCost cost{x};
  finite_diff_cost<SnormCost> fd{cost};
  const double phi0[3] = {mean_x, cppunivariate::to_log(sd_x), cppunivariate::to_log(1.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = cppunivariate::from_log(r.theta[1]);
  const double xi = cppunivariate::from_log(r.theta[2]);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, xi, logLik, r.iterations, SnormStatus::BfgsFailure};
  }
  return {mu, sigma, xi, logLik, r.iterations, SnormStatus::Ok};
}

}  // namespace cppunivariate
