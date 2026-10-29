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

#include "../snorm/mle.hpp"
#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/reductions.hpp"
#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../../core/tie_collapse.hpp"
#include "../norm/mle.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class SstdStatus {
  Ok = 0, BfgsFailure, InvalidStart, Unbounded, ScaleBoundary,
  SnormBoundary,                                    // nu -> inf: use mlsnorm
  RightHalfNormalBoundary, LeftHalfNormalBoundary,  // and xi -> inf / 0
  RightHalfTBoundary, LeftHalfTBoundary,            // xi -> inf / 0
  DfTwoBoundary                                     // nu -> 2
};

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
  double divisor = 1.0;
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
    // log_dsstd_core with its z-free terms hoisted out of the loop.
    const double log_const = std::log(sigma_u) - std::log(sigma) +
                             std::log(2.0 / (xi + 1.0 / xi)) +
                             cppunivariate::dens::log_dstd_unit_const(nu);
    const double half_power = 0.5 * (nu + 1.0);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma * sigma_u + mu_u;
      const double w = z > 0.0 ? z / xi : z * xi;
      neg -= log_const - half_power * std::log1p(w * w / (nu - 2.0));
    }
    return neg/divisor;
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
    for (int i=0;i<16;++i) H[i] /= divisor;
  }
};

namespace sstd_detail {

constexpr double kPi = 3.14159265358979323846;
constexpr double kInf = std::numeric_limits<double>::infinity();

// log t_nu(0) for the ordinary (unit-scale) t; nu = inf is the normal.
// For large nu the lgamma difference cancels; use
// ln Gamma(z + 1/2) - ln Gamma(z) - ln(z)/2 = -1/(8z) + 1/(192 z^3) + O(z^-5)
// with z = nu/2.
inline double log_t_mode(double nu) {
  if (!(nu < kInf)) return -0.5 * std::log(2.0 * kPi);
  if (nu > 1e4) {
    return -0.5 * std::log(2.0 * kPi) - 0.25 / nu + 1.0 / (24.0 * nu * nu * nu);
  }
  return std::lgamma(0.5 * (nu + 1.0)) - std::lgamma(0.5 * nu) -
         0.5 * std::log(kPi * nu);
}

// log t_nu(w) - log t_nu(0).
inline double log_t_kernel(double w, double nu) {
  if (!(nu < kInf)) return -0.5 * w * w;
  return -0.5 * (nu + 1.0) * std::log1p(w * w / nu);
}

// Two-piece t with fixed df in mode/scale coordinates,
//   f(x) = 2 / (a + b) * t_nu((x - c) / s),  s = a left of c, b right of c,
// theta = (c, log a, log b). With nu = 2 this is the df-2 face of sstd.
struct TwoPieceTCost {
  span z;
  double nu;
  int dim() const { return 3; }
  double operator()(const double *theta) const {
    const double a = std::exp(theta[1]);
    const double b = std::exp(theta[2]);
    if (!(a > 0.0 && b > 0.0) || !std::isfinite(a + b)) return kInf;
    const double dn = static_cast<double>(z.size());
    double neg = dn * (std::log(a + b) - std::log(2.0) - log_t_mode(nu));
    for (double v : z) {
      const double d = v - theta[0];
      neg -= log_t_kernel(d / (d < 0.0 ? a : b), nu);
    }
    return neg;
  }
};

// Half-t face (xi -> inf or 0): f(y) = 2 / s * t_nu(y / s) for y = |x - e|
// >= 0, endpoint e at the sample minimum or maximum. Returns the
// log-likelihood maximised over the scale s for eta = 1/nu in [0, 1/2]
// (eta = 0 is the half-normal) and the maximising log s. The scale score
// h(u) = sum (nu + 1) w^2 / (nu + w^2) - n, w = y exp(-u), decreases in u,
// so its root is unique when it exists. It does not exist when at most
// n / (nu + 1) observations sit off the endpoint (heavy ties): the face
// likelihood is unbounded in the strict case; equality has a finite
// unattained scale-zero supremum. Neither case has a positive scale root.
inline double half_t_profile(const std::vector<double> &y, double eta,
                             double &log_scale) {
  const double dn = static_cast<double>(y.size());
  double second = 0.0;
  double positive = 0.0;
  for (double v : y) {
    second += v * v;
    if (v > 0.0) positive += 1.0;
  }
  second /= dn;
  if (!(second > 0.0)) return kInf;
  const double nu = eta > 0.0 ? 1.0 / eta : kInf;
  double u = 0.5 * std::log(second);
  if (eta > 0.0) {
    if (nu * positive < dn-positive) return kInf;
    if (nu * positive == dn-positive) {
      log_scale = -kInf;
      double ll = dn*(std::log(2.0)+log_t_mode(nu))
          + 0.5*(nu+1.0)*positive*std::log(nu);
      for (double v : y) if (v>0.0) ll -= (nu+1.0)*std::log(v);
      return ll;
    }
    auto score = [&](double t, double &slope) {
      const double inv_scale = std::exp(-t);
      double h = -dn;
      slope = 0.0;
      for (double v : y) {
        const double w = v * inv_scale;
        const double q = nu + w * w;
        h += (nu + 1.0) * w * w / q;
        slope -= 2.0 * (nu + 1.0) * nu * w * w / (q * q);
      }
      return h;
    };
    double slope = 0.0;
    // sum (nu+1) w^2/(nu+w^2) <= (nu+1)/nu sum w^2, so h(hi) <= 0.
    double hi = u + 0.5 * std::log1p(eta);
    double lo = u - 1.0;
    for (int k = 0; k < 200 && score(lo, slope) <= 0.0; ++k) lo -= 1.0;
    for (int k = 0; k < 200; ++k) {
      const double h = score(u, slope);
      if (h > 0.0) lo = u; else hi = u;
      double next = u - h / slope;
      if (!(next > lo && next < hi)) next = 0.5 * (lo + hi);
      const bool done = std::abs(next - u) <= 1e-14 * (1.0 + std::abs(u));
      u = next;
      if (done || hi - lo <= 1e-14 * (1.0 + std::abs(u))) break;
    }
  }
  log_scale = u;
  const double inv_scale = std::exp(-u);
  double ll = dn * (std::log(2.0) - u + log_t_mode(nu));
  for (double v : y) ll += log_t_kernel(v * inv_scale, nu);
  return ll;
}

struct HalfTFit {
  double logLik;
  double eta;
  double log_scale;
};

// Global search of the half-t profile over eta = 1/nu in [0, 1/2]: a grid,
// then golden-section refinement around every grid-local maximum.
inline HalfTFit best_half_t(const std::vector<double> &y) {
  constexpr int kGrid = 200;
  const double step = 0.5 / kGrid;
  std::vector<double> grid(kGrid + 1);
  double log_scale = 0.0;
  HalfTFit best{-kInf, 0.0, 0.0};
  for (int j = 0; j <= kGrid; ++j) {
    grid[j] = half_t_profile(y, j * step, log_scale);
    if (grid[j] > best.logLik) best = {grid[j], j * step, log_scale};
  }
  const double ratio = 0.5 * (std::sqrt(5.0) - 1.0);
  for (int j = 0; j <= kGrid; ++j) {
    const bool left_ok = j == 0 || grid[j] >= grid[j - 1];
    const bool right_ok = j == kGrid || grid[j] >= grid[j + 1];
    if (!(left_ok && right_ok) || !std::isfinite(grid[j])) continue;
    double lo = std::max(0.0, (j - 1) * step);
    double hi = std::min(0.5, (j + 1) * step);
    double l = hi - ratio * (hi - lo), r = lo + ratio * (hi - lo);
    double fl = half_t_profile(y, l, log_scale);
    double fr = half_t_profile(y, r, log_scale);
    for (int k = 0; k < 60; ++k) {
      if (fl > fr) {
        hi = r; r = l; fr = fl;
        l = hi - ratio * (hi - lo);
        fl = half_t_profile(y, l, log_scale);
      } else {
        lo = l; l = r; fl = fr;
        r = lo + ratio * (hi - lo);
        fr = half_t_profile(y, r, log_scale);
      }
    }
    const double eta = 0.5 * (lo + hi);
    const double ll = half_t_profile(y, eta, log_scale);
    if (ll > best.logLik) best = {ll, eta, log_scale};
  }
  return best;
}

}  // namespace sstd_detail

// The skew-t likelihood can be maximised on the boundary of the parameter
// space (notes/ml-behaviour.tex). In mode/scale coordinates (c, a, b, nu),
// xi = sqrt(b / a), the closure has three faces:
//   nu -> inf        the two-piece normal (snorm, exact fit), whose own
//                    xi -> inf / 0 corners are translated half-normals;
//   xi -> inf / 0    a translated half-t at the sample minimum / maximum,
//                    df in [2, inf] (scale profiled, df searched);
//   nu -> 2          a two-piece t with df 2 (infinite variance).
// The fitter compares the best of several interior BFGS fits with each
// face. Faces win ties within 1e-8 log-likelihood units per observation,
// as in std and sged: an interior iterate running off towards a face
// approaches the face value from below. This is a multistart comparison,
// not a certificate of the global maximum.
//
// On a boundary status, mu is the mode or endpoint c, sigma the scale of
// the face (snorm: its sd; df-2 face: inf), nu the face df and xi
// sqrt(b / a) (inf or 0 on the half faces).
inline SstdResult sstd_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  constexpr double kInf = sstd_detail::kInf;
  const std::size_t n = x.size();
  if (n < 2)
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::InvalidStart};

  // For fixed skewness the two side scales are s*xi and s/xi;
  // constants change, but the collapse exponent remains nu*(n-k)-k.
  const std::size_t k = collapse_multiplicity(x);
  if (k > 2*(n-k))
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::Unbounded};
  if (k == 2*(n-k))
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::ScaleBoundary};

  const auto moments = norm_detail::moments(x);
  if (moments.sd > 0.0 && std::isfinite(moments.sd) &&
      (moments.sd < 1e-3 || moments.sd > 1e3 || std::abs(moments.mean) > 1e6 * moments.sd)) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i)
      z[i] = static_cast<double>((static_cast<long double>(x[i]) - moments.mean) / moments.sd);
    auto r = sstd_mle(span{z.data(), n}, tol, maxit);
    r.mu = moments.mean + moments.sd*r.mu;
    r.sigma *= moments.sd;
    r.logLik -= static_cast<double>(n)*std::log(moments.sd);
    return r;
  }

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
  const double dn = static_cast<double>(n);
  const double tie = 1e-8 * dn;

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;

  // Interior: the historical start first (it fixes the golden), then
  // restarts; a restart replaces it only when better by more than `tie`.
  const double objective_divisor = n>=100000 ? dn : 1.0;
  SstdCost cost{x,objective_divisor};
  finite_diff_cost<SstdCost> fd{cost};
  const auto sn = snorm_mle(x, tol, maxit);
  std::vector<std::vector<double>> starts;
  starts.push_back({mean_x, std::log(sd_x), std::log(6.0 - 2.0), 0.0});
  for (double xi0 : {0.5, 2.0}) {
    starts.push_back({mean_x, std::log(sd_x), std::log(6.0 - 2.0), std::log(xi0)});
  }
  for (double nu0 : {2.5, 30.0}) {
    starts.push_back({mean_x, std::log(sd_x), std::log(nu0 - 2.0), 0.0});
  }
  if (sn.status == SnormStatus::Ok) {
    // Near the snorm face; finds interior maxima at large df.
    for (double nu0 : {20.0, 200.0}) {
      starts.push_back({sn.mu, std::log(sn.sigma), std::log(nu0 - 2.0), std::log(sn.xi)});
    }
  }
  BfgsResult r;
  r.f = kInf;
  std::vector<BfgsResult> interior;
  for (const auto &start : starts) {
    auto candidate = bfgs_minimize(fd, start.data(), ctl);
    candidate.f *= objective_divisor;
    if (!std::isfinite(candidate.f)) continue;
    if (!std::isfinite(r.f) || candidate.f < r.f - tie) r = candidate;
    interior.push_back(candidate);
  }
  const double interior_ll = std::isfinite(r.f) ? -r.f : -kInf;

  // Faces, fitted on standardised data z = (x - mean) / sd.
  std::vector<double> z;
  for (double v : x) z.push_back((v - mean_x) / sd_x);
  std::sort(z.begin(), z.end());
  const double log_sd = std::log(sd_x);
  const double x_min = *std::min_element(x.begin(), x.end());
  const double x_max = *std::max_element(x.begin(), x.end());

  // nu -> inf: the two-piece normal, exact.
  SstdResult face{sn.mu, sn.sigma, kInf, sn.xi, sn.logLik, 0, SstdStatus::SnormBoundary};
  if (sn.status == SnormStatus::RightHalfNormalBoundary ||
      sn.status == SnormStatus::LeftHalfNormalBoundary) {
    const bool right = sn.status == SnormStatus::RightHalfNormalBoundary;
    const double endpoint = right ? z.front() : z.back();
    double second = 0.0;
    for (double v : z) second += (v - endpoint) * (v - endpoint) / dn;
    face = {right ? x_min : x_max, sd_x * std::sqrt(second), kInf,
            right ? kInf : 0.0, sn.logLik, 0,
            right ? SstdStatus::RightHalfNormalBoundary
                  : SstdStatus::LeftHalfNormalBoundary};
  } else if (sn.status != SnormStatus::Ok) {
    face.logLik = -kInf;
  }

  // xi -> inf / 0: translated half-t at the sample minimum / maximum.
  for (int side = 0; side < 2; ++side) {
    const double endpoint = side == 0 ? z.front() : z.back();
    std::vector<double> y;
    for (double v : z) y.push_back(std::abs(v - endpoint));
    const auto half = sstd_detail::best_half_t(y);
    const double ll = half.logLik - dn * log_sd;
    if (half.eta > 0.0 && ll > face.logLik + tie) {
      face = {side == 0 ? x_min : x_max, sd_x * std::exp(half.log_scale),
              1.0 / half.eta, side == 0 ? kInf : 0.0, ll, 0,
              side == 0 ? SstdStatus::RightHalfTBoundary
                        : SstdStatus::LeftHalfTBoundary};
    }
  }

  // nu -> 2: two-piece t2, from quantile starts and from every interior
  // candidate mapped to mode/scale coordinates.
  sstd_detail::TwoPieceTCost two{span{z.data(), z.size()}, 2.0};
  finite_diff_cost<sstd_detail::TwoPieceTCost> two_fd{two};
  std::vector<std::vector<double>> two_starts;
  const double iqr = z[3 * n / 4] - z[n / 4];
  const double log_s0 = std::log(iqr > 0.0 ? iqr / 1.6 : 1.0);
  for (double location : {z[n / 4], z[n / 2], z[3 * n / 4]}) {
    for (double skew : {-1.0, 0.0, 1.0}) {
      two_starts.push_back({location, log_s0 - skew, log_s0 + skew});
    }
  }
  for (const auto &c : interior) {
    const double nu = cppunivariate::from_log_shift(c.theta[2], 2.0);
    const double xi = std::exp(c.theta[3]);
    double mu_u = 0.0;
    const double sigma_u = cppunivariate::dens::sstd_sigma(nu, xi, mu_u);
    // f = (sigma_u/sd) 2/(xi+1/xi) g(z / xi^sign), g the unit-variance t:
    // mode c = mean - sd mu_u / sigma_u, scales sd / (xi k sigma_u) and
    // sd xi / (k sigma_u) with k = sqrt(nu / (nu - 2)).
    const double sd = std::exp(c.theta[1]);
    const double base = sd / (std::sqrt(nu / (nu - 2.0)) * sigma_u) / sd_x;
    const double mode = (c.theta[0] - sd * mu_u / sigma_u - mean_x) / sd_x;
    if (std::isfinite(base) && base > 0.0 && std::isfinite(mode)) {
      two_starts.push_back({mode, std::log(base / xi), std::log(base * xi)});
    }
  }
  for (const auto &start : two_starts) {
    const auto t2 = bfgs_minimize(two_fd, start.data(), ctl);
    const double ll = -t2.f - dn * log_sd;
    if (std::isfinite(t2.f) && ll > face.logLik + tie) {
      face = {mean_x + sd_x * t2.theta[0], kInf, 2.0,
              std::exp(0.5 * (t2.theta[2] - t2.theta[1])), ll, 0,
              SstdStatus::DfTwoBoundary};
    }
  }

  if (!std::isfinite(face.logLik) && !std::isfinite(interior_ll)) {
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::BfgsFailure};
  }
  if (face.logLik >= interior_ll - tie) {
    face.iterations = r.iterations;
    return face;
  }

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
