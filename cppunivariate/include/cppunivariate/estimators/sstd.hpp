// Skew Student-t MLE on (mu, sigma, nu, xi).
//
// Reparam phi = (mu, log sigma, log(nu - 2), log xi). Pure-libm log-density
// via sstd_sigma + log_dsstd_core; sstd's standardised core is the
// lgamma-based log_dstd_unit, so the standalone fit doesn't pull in Rmath.
//
// f(x; mu, sigma, nu, xi) = (sigma_u / sigma) * f_core(z; nu, xi),
//   where (sigma_u, mu_u) = sstd_sigma(nu, xi),
//   z = (x - mu)/sigma * sigma_u + mu_u.

#pragma once

#include "../core/bfgs.hpp"
#include "../core/finite_diff.hpp"
#include "../core/reductions.hpp"
#include "../core/reparam.hpp"
#include "../core/span.hpp"
#include "../special/skewed_densities.hpp"
#include "../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class SstdStatus { Ok = 0, BfgsFailure, InvalidStart };

struct SstdResult {
  double mu;
  double sigma;
  double nu;
  double xi;
  double logLik;
  int iterations;
  SstdStatus status;
};

// Per-obs nll = ls + F_s(lm, lx) + F_t(lm) + 0.5 (nu+1) log R, where:
//   F_s(lm, lx) = -log sigma_u(m, xi) + log(xi + 1/xi) - log 2
//   F_t(lm)     = -lgamma((nu+1)/2) + lgamma(nu/2) + 0.5 log(pi m)
//   z = (x - mu) sigma_u / sigma + mu_u(m, xi)
//   chi = xi^sign(z), w = z / chi, R = 1 + w^2 / m, m = nu - 2
//
// The Hessian is decomposed via chain rule:
//   H = sum_i H_K(phi; x_i)  +  n * H_F_skew(lm, lx)  +  n * H_F_t(lm)
// where H_F_skew is non-zero only on the (lm, lx) block and H_F_t only
// on (lm, lm). H_K(phi; x) is computed per-observation by lifting log R's
// derivatives through w(phi; x) and chi(lx). Sympy-validated against the
// fully-substituted symbolic Hessian to machine precision.
struct SstdCost {
  span x;
  int dim() const { return 4; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double nu = cppunivariate::from_log_shift(phi[2], 2.0);
    const double xi = cppunivariate::from_log(phi[3]);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || !std::isfinite(xi) ||
        sigma <= 0.0 || nu <= 2.0 || xi <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    double mu_u = 0.0;
    const double sigma_u = cppunivariate::dens::sstd_sigma(nu, xi, mu_u);
    const double log_factor = std::log(sigma_u) - std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma * sigma_u + mu_u;
      neg -= log_factor + cppunivariate::dens::log_dsstd_core(z, nu, xi);
    }
    return neg;
  }
  void hessian(const double *phi, double *H) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double m = cppunivariate::from_log(phi[2]);  // m = nu - 2
    const double xi = cppunivariate::from_log(phi[3]);
    if (!std::isfinite(sigma) || !std::isfinite(m) || !std::isfinite(xi) ||
        sigma <= 0.0 || m <= 0.0 || xi <= 0.0) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      for (int i = 0; i < 16; ++i) H[i] = nan;
      return;
    }
    // m1(m) = Gamma((m+1)/2) / Gamma((m+2)/2) * sqrt(m / pi).
    const double log_m1 =
        std::lgamma(0.5 * (m + 1.0)) - std::lgamma(0.5 * (m + 2.0)) +
        0.5 * std::log(m / M_PI);
    const double m1 = std::exp(log_m1);
    const double dlogm1_dm =
        0.5 * (cppunivariate::special::digamma(0.5 * (m + 1.0)) -
               cppunivariate::special::digamma(0.5 * (m + 2.0))) +
        0.5 / m;
    const double d2logm1_dm2 =
        0.25 * (cppunivariate::special::trigamma(0.5 * (m + 1.0)) -
                cppunivariate::special::trigamma(0.5 * (m + 2.0))) -
        0.5 / (m * m);
    const double a1 = m * dlogm1_dm;        // = m1_lm / m1
    const double m1_lm = m1 * a1;
    const double d2logm1_dlm2 = m * (dlogm1_dm + m * d2logm1_dm2);
    const double m1_lmlm = m1 * (a1 * a1 + d2logm1_dlm2);
    // mu_u, sigma_u and their first/second derivatives wrt (lm, lx).
    const double xi2 = xi * xi;
    const double inv_xi2 = 1.0 / xi2;
    const double xi_p = xi + 1.0 / xi;
    const double xi_m = xi - 1.0 / xi;
    const double xi_psq = xi2 + inv_xi2;
    const double xi_msq = xi2 - inv_xi2;
    const double mu_u = m1 * xi_m;
    const double mu_u_lm = m1_lm * xi_m;
    const double mu_u_lx = m1 * xi_p;
    const double mu_u_lmlm = m1_lmlm * xi_m;
    const double mu_u_lxlx = mu_u;
    const double mu_u_lmlx = m1_lm * xi_p;
    const double su2 = (1.0 - m1 * m1) * xi_psq + 2.0 * m1 * m1 - 1.0;
    const double sigma_u = std::sqrt(su2);
    const double su2_lm = 2.0 * m1 * m1_lm * (2.0 - xi_psq);
    const double su2_lx = (1.0 - m1 * m1) * 2.0 * xi_msq;
    const double su2_lmlm =
        2.0 * (m1_lm * m1_lm + m1 * m1_lmlm) * (2.0 - xi_psq);
    const double su2_lxlx = (1.0 - m1 * m1) * 4.0 * xi_psq;
    const double su2_lmlx = -4.0 * m1 * m1_lm * xi_msq;
    const double su_lm = su2_lm / (2.0 * sigma_u);
    const double su_lx = su2_lx / (2.0 * sigma_u);
    const double su_lmlm =
        (su2_lmlm - 2.0 * su_lm * su_lm) / (2.0 * sigma_u);
    const double su_lxlx =
        (su2_lxlx - 2.0 * su_lx * su_lx) / (2.0 * sigma_u);
    const double su_lmlx =
        (su2_lmlx - 2.0 * su_lm * su_lx) / (2.0 * sigma_u);
    // F_skew = -log sigma_u + log(xi + 1/xi) - log 2.
    const double F_s_lmlm =
        -(su_lmlm / sigma_u - (su_lm / sigma_u) * (su_lm / sigma_u));
    const double F_s_lxlx_part1 =
        -(su_lxlx / sigma_u - (su_lx / sigma_u) * (su_lx / sigma_u));
    const double F_s_lxlx = F_s_lxlx_part1 + 4.0 / (xi_p * xi_p);
    const double F_s_lmlx =
        -(su_lmlx / sigma_u - (su_lm / sigma_u) * (su_lx / sigma_u));
    // F_t(lm) = -lgamma((m+3)/2) + lgamma((m+2)/2) + 0.5 log(pi m).
    const double F_t_lmlm =
        m * (-0.5 * cppunivariate::special::digamma(0.5 * (m + 3.0)) +
             0.5 * cppunivariate::special::digamma(0.5 * (m + 2.0))) +
        m * m *
            (-0.25 * cppunivariate::special::trigamma(0.5 * (m + 3.0)) +
             0.25 * cppunivariate::special::trigamma(0.5 * (m + 2.0)));
    // Initialise.
    for (int i = 0; i < 16; ++i) H[i] = 0.0;
    for (auto v : x) {
      const double a_val = v - mu;
      const double z = a_val * sigma_u / sigma + mu_u;
      if (z == 0.0) continue;
      const double eps = (z > 0.0) ? 1.0 : -1.0;
      const double chi = (z > 0.0) ? xi : 1.0 / xi;
      const double w = z / chi;
      const double R = 1.0 + w * w / m;
      // First derivatives of z.
      const double z_mu = -sigma_u / sigma;
      const double z_ls = -a_val * sigma_u / sigma;  // = -(z - mu_u)
      const double z_lm = a_val / sigma * su_lm + mu_u_lm;
      const double z_lx = a_val / sigma * su_lx + mu_u_lx;
      // Second derivatives of z (mu/ls don't enter sigma_u/mu_u).
      const double z_muls = sigma_u / sigma;
      const double z_mulm = -su_lm / sigma;
      const double z_mulx = -su_lx / sigma;
      const double z_lsls = a_val * sigma_u / sigma;  // = (z - mu_u)
      const double z_lslm = -a_val / sigma * su_lm;
      const double z_lslx = -a_val / sigma * su_lx;
      const double z_lmlm = a_val / sigma * su_lmlm + mu_u_lmlm;
      const double z_lmlx = a_val / sigma * su_lmlx + mu_u_lmlx;
      const double z_lxlx = a_val / sigma * su_lxlx + mu_u_lxlx;
      // First derivatives of w (chain through chi for the lx component).
      const double w_mu = z_mu / chi;
      const double w_ls = z_ls / chi;
      const double w_lm = z_lm / chi;
      const double w_lx = z_lx / chi - z * eps / chi;
      // Second derivatives of w. chi depends only on lx; chi_lx = eps*chi,
      // chi_lxlx = chi.
      const double w_mumu = 0.0;
      const double w_muls = z_muls / chi;
      const double w_mulm = z_mulm / chi;
      const double w_mulx = z_mulx / chi - z_mu * eps / chi;
      const double w_lsls = z_lsls / chi;
      const double w_lslm = z_lslm / chi;
      const double w_lslx = z_lslx / chi - z_ls * eps / chi;
      const double w_lmlm = z_lmlm / chi;
      const double w_lmlx = z_lmlx / chi - z_lm * eps / chi;
      const double w_lxlx =
          z_lxlx / chi - 2.0 * z_lx * eps / chi + z / chi;
      // R derivatives. R = 1 + w^2/m.
      // R_a = 2 w w_a / m  for a in {mu, ls, lx};
      // R_lm = 2 w w_lm/m - w^2/m.
      const double R_mu = 2.0 * w * w_mu / m;
      const double R_ls = 2.0 * w * w_ls / m;
      const double R_lm = 2.0 * w * w_lm / m - w * w / m;
      const double R_lx = 2.0 * w * w_lx / m;
      // R_ab. For neither index = lm: R_ab = 2(w_a w_b + w w_ab)/m.
      // For one index = lm (say a): R_ab = 2(w_a w_b + w w_ab - w w_b)/m.
      // For both = lm: R_lmlm = (2 w_lm^2 + 2 w w_lmlm - 4 w w_lm + w^2)/m.
      const double R_mumu = 2.0 * (w_mu * w_mu + w * w_mumu) / m;
      const double R_muls = 2.0 * (w_mu * w_ls + w * w_muls) / m;
      const double R_mulm =
          2.0 * (w_mu * w_lm + w * w_mulm - w * w_mu) / m;
      const double R_mulx = 2.0 * (w_mu * w_lx + w * w_mulx) / m;
      const double R_lsls = 2.0 * (w_ls * w_ls + w * w_lsls) / m;
      const double R_lslm =
          2.0 * (w_ls * w_lm + w * w_lslm - w * w_ls) / m;
      const double R_lslx = 2.0 * (w_ls * w_lx + w * w_lslx) / m;
      const double R_lmlm =
          (2.0 * w_lm * w_lm + 2.0 * w * w_lmlm - 4.0 * w * w_lm +
           w * w) / m;
      const double R_lmlx =
          2.0 * (w_lm * w_lx + w * w_lmlx - w * w_lx) / m;
      const double R_lxlx = 2.0 * (w_lx * w_lx + w * w_lxlx) / m;
      // f = log R. f_a = R_a/R; f_ab = R_ab/R - R_a R_b / R^2.
      const double inv_R = 1.0 / R;
      const double inv_R2 = inv_R * inv_R;
      const double f_mu = R_mu * inv_R;
      const double f_ls = R_ls * inv_R;
      const double f_lm = R_lm * inv_R;
      const double f_lx = R_lx * inv_R;
      const double f_mumu = R_mumu * inv_R - R_mu * R_mu * inv_R2;
      const double f_muls = R_muls * inv_R - R_mu * R_ls * inv_R2;
      const double f_mulm = R_mulm * inv_R - R_mu * R_lm * inv_R2;
      const double f_mulx = R_mulx * inv_R - R_mu * R_lx * inv_R2;
      const double f_lsls = R_lsls * inv_R - R_ls * R_ls * inv_R2;
      const double f_lslm = R_lslm * inv_R - R_ls * R_lm * inv_R2;
      const double f_lslx = R_lslx * inv_R - R_ls * R_lx * inv_R2;
      const double f_lmlm = R_lmlm * inv_R - R_lm * R_lm * inv_R2;
      const double f_lmlx = R_lmlx * inv_R - R_lm * R_lx * inv_R2;
      const double f_lxlx = R_lxlx * inv_R - R_lx * R_lx * inv_R2;
      // K = 0.5 (m+3) f. K_ab = 0.5 [(m+3)_ab f + (m+3)_a f_b
      //                              + (m+3)_b f_a + (m+3) f_ab]
      // (m+3) depends only on lm: (m+3)_lm = m, (m+3)_lmlm = m.
      const double np3 = m + 3.0;
      const double f = std::log(R);
      const double K_mumu = 0.5 * np3 * f_mumu;
      const double K_muls = 0.5 * np3 * f_muls;
      const double K_mulm = 0.5 * (m * f_mu + np3 * f_mulm);
      const double K_mulx = 0.5 * np3 * f_mulx;
      const double K_lsls = 0.5 * np3 * f_lsls;
      const double K_lslm = 0.5 * (m * f_ls + np3 * f_lslm);
      const double K_lslx = 0.5 * np3 * f_lslx;
      const double K_lmlm =
          0.5 * (m * f + 2.0 * m * f_lm + np3 * f_lmlm);
      const double K_lmlx = 0.5 * (m * f_lx + np3 * f_lmlx);
      const double K_lxlx = 0.5 * np3 * f_lxlx;
      H[0] += K_mumu;
      H[1] += K_muls;
      H[4] += K_muls;
      H[2] += K_mulm;
      H[8] += K_mulm;
      H[3] += K_mulx;
      H[12] += K_mulx;
      H[5] += K_lsls;
      H[6] += K_lslm;
      H[9] += K_lslm;
      H[7] += K_lslx;
      H[13] += K_lslx;
      H[10] += K_lmlm;
      H[11] += K_lmlx;
      H[14] += K_lmlx;
      H[15] += K_lxlx;
    }
    const double dn = static_cast<double>(x.size());
    H[10] += dn * (F_t_lmlm + F_s_lmlm);
    const double f_lmlx_off = dn * F_s_lmlx;
    H[11] += f_lmlx_off;
    H[14] += f_lmlx_off;
    H[15] += dn * F_s_lxlx;
  }
};

inline SstdResult sstd_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2)
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::InvalidStart};

  const double mean_x = cppunivariate::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::InvalidStart};
  }

  SstdCost cost{x};
  finite_diff_cost<SstdCost> fd{cost};
  const double phi0[4] = {mean_x, cppunivariate::to_log(sd_x),
                          cppunivariate::to_log_shift(6.0, 2.0), cppunivariate::to_log(1.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = cppunivariate::from_log(r.theta[1]);
  const double nu = cppunivariate::from_log_shift(r.theta[2], 2.0);
  const double xi = cppunivariate::from_log(r.theta[3]);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, nu, xi, logLik, r.iterations, SstdStatus::BfgsFailure};
  }
  return {mu, sigma, nu, xi, logLik, r.iterations, SstdStatus::Ok};
}

}  // namespace cppunivariate
