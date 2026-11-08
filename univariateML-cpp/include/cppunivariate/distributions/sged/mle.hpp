// Skew GED MLE on (mu, sigma, nu, xi).
//
// Fitting reparam phi = (mu, log sigma, log(nu - 1), log xi). Pure-libm log-density via
// sged_sigma + log_dsged_core; the standardised log_dged_unit core is
// already lgamma-/tgamma-based, so the fit doesn't pull in Rmath.
//
// f(x; mu, sigma, nu, xi) = (sigma_u / sigma) * f_core(z; nu, xi),
//   where (sigma_u, mu_u) = sged_sigma(nu, xi),
//   z = (x - mu)/sigma * sigma_u + mu_u.

#pragma once
#include "../../core/work_budget.hpp"
#include "../sstd/hold_search.hpp"

#include "../../core/bfgs.hpp"
#include "../../core/multistart.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/reductions.hpp"
#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../norm/mle.hpp"
#include "../ged/mle.hpp"
#include "../snorm/mle.hpp"
#include "../snorm/fs.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <vector>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {
inline constexpr double kSgedNuLower = 1.0;


enum class SgedStatus { Ok = 0, BfgsFailure, InvalidStart, Unbounded, ScaleBoundary, UnsupportedHold, UniformBoundary, RightHalfGedBoundary, LeftHalfGedBoundary, CertificateFailure };

struct SgedResult {
  double mu;
  double sigma;
  double nu;
  double xi;
  double logLik;
  int iterations;
  SgedStatus status; unsigned restricted = 0;
  StopDetail stop_detail{};
  bool certified = false;
};

#include "certificate.hpp"

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
    const double m1 = cppunivariate::dens::ged_abs_moment(nu);
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

#include "skew_certificate.hpp"

// Keep the unconstrained Cost/Hessian coordinates for density diagnostics.
// Fitting uses log(nu - 1), with the attained face compared separately.
struct SgedFitCost {
  span x;
  double divisor = 1.0;
  int dim() const { return 4; }
  double operator()(const double *phi) const {
    double q[4];
    for (int i = 0; i < 4; ++i) q[i] = phi[i];
    q[2] = std::log1p(std::exp(phi[2]));
    return SgedCost{x}(q)/divisor;
  }
};

inline SgedResult sged_mle_free(span x, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  double sd_x = std::sqrt(var_x);
  if (!(sd_x > 0.0) || !std::isfinite(sd_x))
    sd_x = norm_detail::moments(x).sd * std::sqrt(static_cast<double>(n) / (n-1.0));
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SgedStatus::InvalidStart};
  }

  // Standardise before optimisation to make tolerances independent of units.
  std::vector<double> z;
  for (double v : x) z.push_back((v - mean_x) / sd_x);
  std::sort(z.begin(), z.end());
  const double dn = static_cast<double>(n);
  const double tie = 1e-8 * dn; // total log-likelihood tie; attained wins.
  SgedResult best{0.0, 1.0, 1.0, 1.0,
                  -std::numeric_limits<double>::infinity(), 0, SgedStatus::Ok};
  // f = exp(-|x-c|/side)/(a+b). Profiling gives
  // a = sqrt(A)(sqrt(A)+sqrt(B))/n, b analogously.
  // Between observations A,B are affine; sqrt(A B) is concave,
  // so (sqrt(A)+sqrt(B))^2 attains a minimum at an observation.
  double total = 0.0, prefix = 0.0;
  for (double v : z) total += v;
  for (std::size_t i = 0; i < n; ++i) {
    const double c = z[i];
    const double A = std::max(0.0, c * i - prefix);
    const double B = std::max(0.0, total - prefix - c * (n - i));
    const double root = std::sqrt(A) + std::sqrt(B);
    const double a = std::sqrt(A) * root / dn;
    const double b = std::sqrt(B) * root / dn;
    const double ll = -dn * (std::log(root * root / dn) + 1.0);
    if (a > 0.0 && b > 0.0 && ll > best.logLik) {
      best = {c + b - a, std::sqrt(a*a + b*b), 1.0,
              std::sqrt(b/a), ll, 0, SgedStatus::Ok};
    }
    prefix += c;
  }
  double attempted=best.logLik;
  bool accepted=false;
  const double objective_divisor = n>=100000 ? dn : 1.0;
  SgedFitCost cost{span{z.data(), z.size()},objective_divisor};
  finite_diff_cost<SgedFitCost> fd{cost};
  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  for (double nu0 : {1.5, 2.0, 4.0, 8.0}) {
  for (double xi0 : {0.5, 1.0, 2.0}) {
    const double phi0[4] = {0.0, 0.0, std::log(nu0 - 1.0), std::log(xi0)};
    auto r = bfgs_minimize(fd, phi0, ctl);
    if (std::isfinite(r.f)) attempted=std::max(attempted,-r.f*objective_divisor);
    if (!bfgs_candidate_acceptable(r,objective_divisor)) continue;
    accepted=true;
    r.status=BfgsStatus::Ok;
    r.f *= objective_divisor;
    if (std::isfinite(r.f) && -r.f > best.logLik + tie) {
      best = {r.theta[0], std::exp(r.theta[1]), 1.0 + std::exp(r.theta[2]),
              std::exp(r.theta[3]),
              -r.f, r.iterations,
              r.status == BfgsStatus::Ok ? SgedStatus::Ok : SgedStatus::BfgsFailure};
    }
  }
  }
  if (!accepted && attempted>best.logLik+tie) best.status=SgedStatus::BfgsFailure;
  // Half-GED profile: log a = (log nu + log mean(y^nu))/nu.
  // Scale y to [0,1] and use log sums to avoid power overflow.
  for (int side : {1, -1}) {
    const double endpoint = side == 1 ? z.front() : z.back();
    const double range = z.back() - z.front();
    auto profile = [&](double t) {
      const double nu = std::exp(t);
      double moment = 0.0;
      for (double v : z) moment += std::pow(std::abs(v-endpoint)/range, nu) / dn;
      const double loga = std::log(range) + (std::log(nu) + std::log(moment))/nu;
      return dn * (std::log(nu) - std::lgamma(1.0/nu) - loga - 1.0/nu);
    };
    double ll = profile(0.0), shape = 1.0;
    // Scan log nu, refining every bracket (no unimodality assumption).
    for (int k = 0; k < maxit; ++k) {
      double lo = k * 0.2, hi = (k+1) * 0.2;
      for (int j = 0; j < maxit; ++j) {
        const double l = lo + (hi-lo)/3, r = hi - (hi-lo)/3;
        if (profile(l) < profile(r)) lo = l; else hi = r;
      }
      const double t = (lo+hi)/2, candidate = profile(t);
      if (candidate > ll) { ll = candidate; shape = std::exp(t); }
    }
    if (ll > std::max(best.logLik,attempted) + tie) {
      best.logLik = ll; best.nu = shape;
      double moment=0;for(double v:z)moment+=std::pow(std::abs(v-endpoint)/range,shape)/dn;
      const double a=sd_x*range*std::exp((std::log(shape)+std::log(moment))/shape);
      best.stop_detail=limit_detail({mean_x+sd_x*endpoint,a,shape},ll-dn*std::log(sd_x),side==1?StopModel::HalfGed:StopModel::ReflectedHalfGed);
      best.status = side == 1 ? SgedStatus::RightHalfGedBoundary
                              : SgedStatus::LeftHalfGedBoundary;
    }
  }
  const double uniform = -dn * std::log(z.back() - z.front());
  if (uniform > std::max(best.logLik,attempted) + tie ||
      ((best.status == SgedStatus::RightHalfGedBoundary ||
        best.status == SgedStatus::LeftHalfGedBoundary) &&
       uniform >= best.logLik - tie)) {
    best.logLik = uniform;
    best.status = SgedStatus::UniformBoundary;
    best.stop_detail=limit_detail({mean_x+sd_x*z.front(),mean_x+sd_x*z.back()},uniform-dn*std::log(sd_x),StopModel::Unif);
  }
  best.mu = mean_x + sd_x * best.mu;
  best.sigma *= sd_x;
  best.logLik -= dn * std::log(sd_x);
  return best;
}

#include "face_payload.hpp"
#include "free_shape_certificate.hpp"

// Classified holds: fixed shape/skew and either location or scale (or both).
inline SgedResult sged_mle_impl(span x, double tol, int maxit,
    double fixed_mean=std::numeric_limits<double>::quiet_NaN(),
    double fixed_sd=std::numeric_limits<double>::quiet_NaN(),
    double fixed_nu=std::numeric_limits<double>::quiet_NaN(),
    double fixed_xi=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double inf=std::numeric_limits<double>::infinity();
  const bool hm=!std::isnan(fixed_mean), hs=!std::isnan(fixed_sd);
  const bool hn=!std::isnan(fixed_nu), hx=!std::isnan(fixed_xi);
  SgedResult out{nan,nan,nan,nan,nan,0,SgedStatus::InvalidStart};
  if((hm && !std::isfinite(fixed_mean)) || (hs && !(fixed_sd>0 && std::isfinite(fixed_sd))) ||
     (hn && !(fixed_nu>0 && std::isfinite(fixed_nu))) ||
     (hx && !(fixed_xi>0 && std::isfinite(fixed_xi))) || x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1) return out;
  for(double v:x) if(!std::isfinite(v)) return out;
  if(!hn && hx && fixed_xi==1)
    return sged_free_shape_certificate::symmetric(x,tol,maxit,fixed_mean,fixed_sd);
  if(!hn && !hm && !hs && sged_free_shape_certificate::balanced_uniform(x,out))return out;
  if(!hn && hx && !hm && !hs) return sged_weighted_shape::fit(x,tol,maxit,fixed_xi);
  if(!hm && !hs && !hn && !hx) return sged_mle_free(x,tol,maxit);
  if(hn && !hx && fixed_nu==2)
    return sged_skew_certificate::normal(x,maxit,fixed_mean,fixed_sd);
  if(hn && !hx && !hm && !hs && fixed_nu<=1)
    return sged_skew_certificate::subunit(x,maxit,fixed_nu);
  if(hn && !hx && !hm && !hs && fixed_nu>1)
    return sged_mode_skew::fit(x,tol,maxit,fixed_nu);
  if(hn && hx && (!hm || !hs))
    return sged_cert_detail::fit(x,tol,maxit,fixed_mean,fixed_sd,fixed_nu,fixed_xi);
  if(!(hn && hx && (hm || hs))) {
    const auto r=skew_hold_detail::fit(x,false,fixed_mean,fixed_sd,fixed_nu,fixed_xi,tol,maxit);
    const std::vector<SgedStatus> statuses{SgedStatus::Ok,SgedStatus::BfgsFailure,SgedStatus::InvalidStart,SgedStatus::Unbounded,SgedStatus::ScaleBoundary,SgedStatus::UniformBoundary,SgedStatus::UniformBoundary,SgedStatus::RightHalfGedBoundary,SgedStatus::LeftHalfGedBoundary,SgedStatus::UniformBoundary};
    auto status=statuses[r.status];
    SgedResult out={r.mean,r.sd,r.nu,r.xi,-r.f,r.iterations,status};
    sged_face_payload(x,fixed_mean,fixed_sd,out,maxit);return out;
  }
  double lo=inf, hi=-inf;
  for(double v:x) { lo=std::min(lo,v); hi=std::max(hi,v); }
  if(!hs && lo==hi && lo==fixed_mean) { out.status=SgedStatus::Unbounded; return out; }

  const double origin=hm ? fixed_mean : lo+(hi-lo)/2;
  const double unit=hs ? fixed_sd : std::max(hi-lo,std::max(std::abs(lo-origin),std::abs(hi-origin)));
  if(!(unit>0) || !std::isfinite(unit)) return out;
  std::vector<double> y;
  for(double v:x) y.push_back((v-origin)/unit);
  double shift=0;
  const double scale=dens::sged_sigma(fixed_nu,fixed_xi,shift);
  const double log_g=std::log(2.0/(fixed_xi+1.0/fixed_xi));
  const double kernel_scale=dens::ged_lambda(fixed_nu);
  const double normalizer=std::log(dens::ged_g(fixed_nu,kernel_scale));
  auto cost=[&](double mean, double sd) {
    double neg=x.size()*(std::log(sd)-std::log(scale));
    for(double v:y) {
      const double z=(v-mean)*scale/sd+shift;
      const double Xi=std::pow(fixed_xi,dens::sign1(z));
      neg-=log_g+(normalizer-0.5*std::pow(std::fabs((z/Xi)/kernel_scale),fixed_nu));
    }
    return neg;
  };
  double mean=0, sd=hs ? fixed_sd/unit : 1;
  if(!hm) {
    // Every location mode lies between the smallest/largest translated observations.
    const double left=(lo-origin)/unit+sd*shift/scale, right=(hi-origin)/unit+sd*shift/scale;
    auto objective=[&](double m) { return cost(m,sd); };
    mean=snorm_detail::scan(objective,left,right,256);
    // Include each density kink / possible narrow t mode explicitly.
    for(double v:y) {
      const double c=v+sd*shift/scale;
      const double t=snorm_detail::minimize(objective,std::max(left,c-sd/scale),std::min(right,c+sd/scale));
      if(objective(t)<objective(mean)) mean=t;
      if(objective(c)<objective(mean)) mean=c;
    }
  } else if(!hs) {
    auto objective=[&](double ls) { return cost(0,std::exp(ls)); };
    const double ls=snorm_detail::scan(objective,-32,32,512);
    if(std::abs(ls)>31.9) { out.status=SgedStatus::ScaleBoundary; return out; }
    sd=std::exp(ls);
  }
  out={hm ? fixed_mean : origin+unit*mean,hs ? fixed_sd : unit*sd,
       fixed_nu,fixed_xi,-cost(mean,sd)-x.size()*std::log(unit),0,SgedStatus::Ok};
  if(!std::isfinite(out.logLik)) out.status=SgedStatus::BfgsFailure;
  return out;
}


inline SgedResult sged_mle(span x,double tol,int maxit,double mean= std::numeric_limits<double>::quiet_NaN(),double sd= std::numeric_limits<double>::quiet_NaN(),double nu= std::numeric_limits<double>::quiet_NaN(),double xi= std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=sged_mle_impl(x,tol,maxit,mean,sd,nu,xi);
  if(r.status==SgedStatus::Ok && (std::isnan(nu)) && r.nu==kSgedNuLower) r.restricted|=16u;
  return r;
}
}  // namespace cppunivariate
