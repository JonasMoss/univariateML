// Skew GED MLE on (mu, sigma, nu, xi).
//
// Reparam phi = (mu, log sigma, log nu, log xi). Pure-libm log-density via
// sged_sigma + log_dsged_core; the standardised log_dged_unit core is
// already lgamma-/tgamma-based, so the fit doesn't pull in Rmath.
//
// f(x; mu, sigma, nu, xi) = (sigma_u / sigma) * f_core(z; nu, xi),
//   where (sigma_u, mu_u) = sged_sigma(nu, xi),
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

enum class SgedStatus { Ok = 0, BfgsFailure, InvalidStart };

struct SgedResult {
  double mu;
  double sigma;
  double nu;
  double xi;
  double logLik;
  int iterations;
  SgedStatus status;
};

// Per-obs nll = ls + F_s(ln, lx) + F_g(ln) + 0.5 |w/lambda(nu)|^nu, with:
//   F_s(ln, lx) = -log sigma_u(nu, xi) + log(xi + 1/xi) - log 2
//   F_g(ln) = -log nu + 1.5 lgamma(1/nu) - 0.5 lgamma(3/nu) + log 2
//   z = (x - mu) sigma_u / sigma + mu_u(nu, xi)
//   chi = xi^sign(z), w = z / chi, q = |w| / lambda(nu)
// Hessian via chain rule: K = 0.5 q^nu so K_ab = K (LK_a LK_b + LK_ab)
// where LK = log K = log 0.5 + nu log q. The phi-only constants F_s, F_g
// contribute n * H_const to the (ln, lx) block. Sympy-validated against
// fully substituted symbolic Hessian to machine precision.
struct SgedCost {
  span x;
  int dim() const { return 4; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double nu = cppunivariate::from_log(phi[2]);
    const double xi = cppunivariate::from_log(phi[3]);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || !std::isfinite(xi) ||
        sigma <= 0.0 || nu <= 0.0 || xi <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    double mu_u = 0.0;
    const double sigma_u = cppunivariate::dens::sged_sigma(nu, xi, mu_u);
    const double log_factor = std::log(sigma_u) - std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma * sigma_u + mu_u;
      neg -= log_factor + cppunivariate::dens::log_dsged_core(z, nu, xi);
    }
    return neg;
  }
  void hessian(const double *phi, double *H) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double nu = cppunivariate::from_log(phi[2]);
    const double xi = cppunivariate::from_log(phi[3]);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || !std::isfinite(xi) ||
        sigma <= 0.0 || nu <= 0.0 || xi <= 0.0) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      for (int i = 0; i < 16; ++i) H[i] = nan;
      return;
    }
    // lambda(nu) = sqrt(2^(-2/nu) Gamma(1/nu) / Gamma(3/nu)).
    const double lam = cppunivariate::dens::ged_lambda(nu);
    const double log_lam = std::log(lam);
    // m1(nu) = 2^(1/nu) * lambda * Gamma(2/nu) / Gamma(1/nu).
    // log m1 = -0.5 lgamma(1/nu) + lgamma(2/nu) - 0.5 lgamma(3/nu).
    const double psi_1 = cppunivariate::special::digamma(1.0 / nu);
    const double psi_2 = cppunivariate::special::digamma(2.0 / nu);
    const double psi_3 = cppunivariate::special::digamma(3.0 / nu);
    const double tri_1 = cppunivariate::special::trigamma(1.0 / nu);
    const double tri_2 = cppunivariate::special::trigamma(2.0 / nu);
    const double tri_3 = cppunivariate::special::trigamma(3.0 / nu);
    const double log_m1 = -0.5 * std::lgamma(1.0 / nu) +
                          std::lgamma(2.0 / nu) -
                          0.5 * std::lgamma(3.0 / nu);
    const double m1 = std::exp(log_m1);
    const double f_val = 0.5 * psi_1 - 2.0 * psi_2 + 1.5 * psi_3;
    const double dlogm1_dln = f_val / nu;
    const double df_dln = (-0.5 * tri_1 + 4.0 * tri_2 - 4.5 * tri_3) / nu;
    const double d2logm1_dln2 = (df_dln - f_val) / nu;
    const double m1_ln = m1 * dlogm1_dln;
    const double m1_lnln =
        m1 * (dlogm1_dln * dlogm1_dln + d2logm1_dln2);
    // sigma_u, mu_u and their lm/lx derivatives (same form as sstd
    // but with sged's m1).
    const double xi2 = xi * xi;
    const double inv_xi2 = 1.0 / xi2;
    const double xi_p = xi + 1.0 / xi;
    const double xi_m = xi - 1.0 / xi;
    const double xi_psq = xi2 + inv_xi2;
    const double xi_msq = xi2 - inv_xi2;
    const double mu_u = m1 * xi_m;
    const double mu_u_ln = m1_ln * xi_m;
    const double mu_u_lx = m1 * xi_p;
    const double mu_u_lnln = m1_lnln * xi_m;
    const double mu_u_lxlx = mu_u;
    const double mu_u_lnlx = m1_ln * xi_p;
    const double su2 =
        (1.0 - m1 * m1) * xi_psq + 2.0 * m1 * m1 - 1.0;
    const double sigma_u = std::sqrt(su2);
    const double su2_ln = 2.0 * m1 * m1_ln * (2.0 - xi_psq);
    const double su2_lx = (1.0 - m1 * m1) * 2.0 * xi_msq;
    const double su2_lnln =
        2.0 * (m1_ln * m1_ln + m1 * m1_lnln) * (2.0 - xi_psq);
    const double su2_lxlx = (1.0 - m1 * m1) * 4.0 * xi_psq;
    const double su2_lnlx = -4.0 * m1 * m1_ln * xi_msq;
    const double su_ln = su2_ln / (2.0 * sigma_u);
    const double su_lx = su2_lx / (2.0 * sigma_u);
    const double su_lnln =
        (su2_lnln - 2.0 * su_ln * su_ln) / (2.0 * sigma_u);
    const double su_lxlx =
        (su2_lxlx - 2.0 * su_lx * su_lx) / (2.0 * sigma_u);
    const double su_lnlx =
        (su2_lnlx - 2.0 * su_ln * su_lx) / (2.0 * sigma_u);
    const double F_s_lnln =
        -(su_lnln / sigma_u - (su_ln / sigma_u) * (su_ln / sigma_u));
    const double F_s_lxlx =
        -(su_lxlx / sigma_u - (su_lx / sigma_u) * (su_lx / sigma_u)) +
        4.0 / (xi_p * xi_p);
    const double F_s_lnlx =
        -(su_lnlx / sigma_u - (su_ln / sigma_u) * (su_lx / sigma_u));
    // F_g(ln) lnln contribution.
    const double F_g_lnln = (3.0 / (2.0 * nu)) * (psi_1 - psi_3) +
                            (3.0 / (2.0 * nu * nu)) *
                                (tri_1 - 3.0 * tri_3);
    // K_const, Kc_over_nu_ln.
    const double K_const =
        std::log(2.0) - 0.5 * psi_1 + 1.5 * psi_3;
    const double Kc_ln = (tri_1 - 9.0 * tri_3) / (2.0 * nu);
    const double Kc_over_nu_ln = -K_const / nu + Kc_ln / nu;
    for (int i = 0; i < 16; ++i) H[i] = 0.0;
    for (auto v : x) {
      const double a_val = v - mu;
      const double z = a_val * sigma_u / sigma + mu_u;
      if (z == 0.0) continue;
      const double eps = (z > 0.0) ? 1.0 : -1.0;
      const double chi = (z > 0.0) ? xi : 1.0 / xi;
      const double w = z / chi;
      const double az_w = std::fabs(w);
      const double log_w = std::log(az_w);
      const double log_q = log_w - log_lam;
      const double W = 0.5 * std::exp(nu * log_q);
      // First derivatives of z.
      const double z_mu = -sigma_u / sigma;
      const double z_ls = -a_val * sigma_u / sigma;
      const double z_ln = a_val / sigma * su_ln + mu_u_ln;
      const double z_lx = a_val / sigma * su_lx + mu_u_lx;
      const double z_muls = sigma_u / sigma;
      const double z_muln = -su_ln / sigma;
      const double z_mulx = -su_lx / sigma;
      const double z_lsls = a_val * sigma_u / sigma;
      const double z_lsln = -a_val / sigma * su_ln;
      const double z_lslx = -a_val / sigma * su_lx;
      const double z_lnln = a_val / sigma * su_lnln + mu_u_lnln;
      const double z_lnlx = a_val / sigma * su_lnlx + mu_u_lnlx;
      const double z_lxlx = a_val / sigma * su_lxlx + mu_u_lxlx;
      // First/second derivatives of w (chain through chi for lx).
      const double w_mu = z_mu / chi;
      const double w_ls = z_ls / chi;
      const double w_ln = z_ln / chi;
      const double w_lx = z_lx / chi - z * eps / chi;
      const double w_mumu = 0.0;
      const double w_muls = z_muls / chi;
      const double w_muln = z_muln / chi;
      const double w_mulx = z_mulx / chi - z_mu * eps / chi;
      const double w_lsls = z_lsls / chi;
      const double w_lsln = z_lsln / chi;
      const double w_lslx = z_lslx / chi - z_ls * eps / chi;
      const double w_lnln = z_lnln / chi;
      const double w_lnlx = z_lnlx / chi - z_ln * eps / chi;
      const double w_lxlx =
          z_lxlx / chi - 2.0 * z_lx * eps / chi + z / chi;
      // log q derivatives. (log q)_a = w_a/w (for non-ln); for ln there's
      // an extra -K_const/nu factor in the first deriv but it cancels
      // structurally in the L expansion below; we account for it via
      // lq_a_ln below and the lq_lnln correction Kc_over_nu_ln.
      const double inv_w = 1.0 / w;
      const double inv_w2 = inv_w * inv_w;
      auto lq_partial = [&](double w_a, double w_b, double w_ab,
                            bool both_ln) {
        double v = w_ab * inv_w - w_a * w_b * inv_w2;
        if (both_ln) v -= Kc_over_nu_ln;
        return v;
      };
      const double lq_mumu = lq_partial(w_mu, w_mu, w_mumu, false);
      const double lq_muls = lq_partial(w_mu, w_ls, w_muls, false);
      const double lq_muln = lq_partial(w_mu, w_ln, w_muln, false);
      const double lq_mulx = lq_partial(w_mu, w_lx, w_mulx, false);
      const double lq_lsls = lq_partial(w_ls, w_ls, w_lsls, false);
      const double lq_lsln = lq_partial(w_ls, w_ln, w_lsln, false);
      const double lq_lslx = lq_partial(w_ls, w_lx, w_lslx, false);
      const double lq_lnln = lq_partial(w_ln, w_ln, w_lnln, true);
      const double lq_lnlx = lq_partial(w_ln, w_lx, w_lnlx, false);
      const double lq_lxlx = lq_partial(w_lx, w_lx, w_lxlx, false);
      const double lq_a_mu = w_mu * inv_w;
      const double lq_a_ls = w_ls * inv_w;
      const double lq_a_ln = w_ln * inv_w - K_const / nu;
      const double lq_a_lx = w_lx * inv_w;
      // L = log K - log 0.5 = nu log q. L_a = nu_a log q + nu (log q)_a.
      // L_ab = nu_ab log q + nu_a (log q)_b + nu_b (log q)_a + nu (log q)_ab.
      // nu depends only on ln: nu_ln = nu, nu_lnln = nu.
      const double L_mu = nu * lq_a_mu;
      const double L_ls = nu * lq_a_ls;
      // L_ln = nu * log q + nu * (log q)_ln; the -K_const offset is
      // already absorbed in lq_a_ln = w_ln/w - K_const/nu.
      const double L_ln = nu * log_q + nu * lq_a_ln;
      const double L_lx = nu * lq_a_lx;
      const double L_mumu = nu * lq_mumu;
      const double L_muls = nu * lq_muls;
      const double L_muln = nu * lq_a_mu + nu * lq_muln;
      const double L_mulx = nu * lq_mulx;
      const double L_lsls = nu * lq_lsls;
      const double L_lsln = nu * lq_a_ls + nu * lq_lsln;
      const double L_lslx = nu * lq_lslx;
      const double L_lnln =
          nu * log_q + 2.0 * nu * lq_a_ln + nu * lq_lnln;
      const double L_lnlx = nu * lq_a_lx + nu * lq_lnlx;
      const double L_lxlx = nu * lq_lxlx;
      // K = 0.5 q^nu. K_ab = K (L_a L_b + L_ab).
      H[0] += W * (L_mu * L_mu + L_mumu);
      const double off_01 = W * (L_mu * L_ls + L_muls);
      H[1] += off_01;
      H[4] += off_01;
      const double off_02 = W * (L_mu * L_ln + L_muln);
      H[2] += off_02;
      H[8] += off_02;
      const double off_03 = W * (L_mu * L_lx + L_mulx);
      H[3] += off_03;
      H[12] += off_03;
      H[5] += W * (L_ls * L_ls + L_lsls);
      const double off_12 = W * (L_ls * L_ln + L_lsln);
      H[6] += off_12;
      H[9] += off_12;
      const double off_13 = W * (L_ls * L_lx + L_lslx);
      H[7] += off_13;
      H[13] += off_13;
      H[10] += W * (L_ln * L_ln + L_lnln);
      const double off_23 = W * (L_ln * L_lx + L_lnlx);
      H[11] += off_23;
      H[14] += off_23;
      H[15] += W * (L_lx * L_lx + L_lxlx);
    }
    const double dn = static_cast<double>(x.size());
    H[10] += dn * (F_g_lnln + F_s_lnln);
    const double off_23_const = dn * F_s_lnlx;
    H[11] += off_23_const;
    H[14] += off_23_const;
    H[15] += dn * F_s_lxlx;
  }
};

inline SgedResult sged_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2)
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SgedStatus::InvalidStart};

  const double mean_x = cppunivariate::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SgedStatus::InvalidStart};
  }

  SgedCost cost{x};
  finite_diff_cost<SgedCost> fd{cost};
  const double phi0[4] = {mean_x, cppunivariate::to_log(sd_x), cppunivariate::to_log(2.0),
                          cppunivariate::to_log(1.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = cppunivariate::from_log(r.theta[1]);
  const double nu = cppunivariate::from_log(r.theta[2]);
  const double xi = cppunivariate::from_log(r.theta[3]);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, nu, xi, logLik, r.iterations, SgedStatus::BfgsFailure};
  }
  return {mu, sigma, nu, xi, logLik, r.iterations, SgedStatus::Ok};
}

}  // namespace cppunivariate
