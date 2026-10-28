// Exact one-dimensional two-piece normal profile, returned in fGarch's
// standardised (mean, sd, xi) coordinates. SnormCost remains in phi-space
// for the independent density/Hessian validation harness.

#pragma once

#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../../special/skewed_densities.hpp"

#include <algorithm>
#include <vector>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class SnormStatus { Ok = 0, BfgsFailure, InvalidStart,
                         RightHalfNormalBoundary, LeftHalfNormalBoundary };

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

namespace snorm_detail {
// Recursive derivative isolation: between successive derivative roots a
// polynomial is monotone. Bisection therefore finds every sign-changing root;
// derivative roots themselves are retained to include all multiple roots.
inline long double evaluate(const std::vector<long double>& p, long double t) {
  long double y = 0;
  for (std::size_t i = p.size(); i-- > 0;) y = y * t + p[i];
  return y;
}
inline std::vector<long double> roots(std::vector<long double> p) {
  while (p.size() > 1 && p.back() == 0) p.pop_back();
  if (p.size() <= 1) return {};
  std::vector<long double> d(p.size() - 1);
  for (std::size_t i = 1; i < p.size(); ++i) d[i-1] = i * p[i];
  auto cuts = roots(d);
  cuts.push_back(0); cuts.push_back(1);
  std::sort(cuts.begin(), cuts.end());
  std::vector<long double> out = cuts;
  for (std::size_t i = 1; i < cuts.size(); ++i) {
    long double l = cuts[i-1], r = cuts[i], fl = evaluate(p, l);
    const long double fr = evaluate(p, r);
    if ((fl < 0 && fr > 0) || (fl > 0 && fr < 0)) {
      for (int j = 0; j < 80; ++j) {
        const long double m = (l+r)/2, fm = evaluate(p, m);
        if (m == l || m == r) break;
        if ((fl < 0) == (fm < 0)) { l = m; fl = fm; }
        else r = m;
      }
      out.push_back((l+r)/2);
    }
  }
  std::sort(out.begin(), out.end());
  out.erase(std::unique(out.begin(), out.end()), out.end());
  return out;
}
inline std::vector<long double> multiply(const std::vector<long double>& a,
                                         const std::vector<long double>& b) {
  std::vector<long double> p(a.size()+b.size()-1, 0);
  for (std::size_t i=0; i<a.size(); ++i)
    for (std::size_t j=0; j<b.size(); ++j) p[i+j] += a[i]*b[j];
  return p;
}
} // namespace snorm_detail

inline SnormResult snorm_mle(span x, double /*tol*/, int /*maxit*/) {
  constexpr double nan = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
  std::vector<long double> y;
  for (double v : x) {
    if (!std::isfinite(v)) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
    y.push_back(v);
  }
  std::sort(y.begin(), y.end());
  const long double origin = y.front(), scale = y.back()-origin;
  if (!(scale > 0)) return {nan,nan,nan,nan,0,SnormStatus::InvalidStart};
  // Affine normalisation keeps the degree-seven coefficients bounded.
  for (auto& v : y) v = (v-origin)/scale;
  // Left moments about 0 and right moments about 1 avoid cancellation
  // at the half-normal endpoints; never subtract prefix sums from totals.
  std::vector<long double> s(n+1,0), ss(n+1,0), z(n+1,0), zz(n+1,0);
  for (std::size_t i=0; i<n; ++i) { s[i+1]=s[i]+y[i]; ss[i+1]=ss[i]+y[i]*y[i]; }
  for (std::size_t i=n; i-- > 0;) { z[i]=z[i+1]+(1-y[i]); zz[i]=zz[i+1]+(1-y[i])*(1-y[i]); }
  long double best = std::numeric_limits<long double>::infinity();
  long double mode=0, ca=0, cb=0;
  auto consider = [&](long double c, std::size_t k) {
    const long double A=std::max(0.0L, ss[k]-2*c*s[k]+k*c*c);
    const long double B=std::max(0.0L, zz[k]-2*(1-c)*z[k]+(n-k)*(1-c)*(1-c));
    // Minimising the cube root of Q is equivalent and avoids cubing.
    const long double a=std::cbrt(A), b=std::cbrt(B), q=a+b;
    if (q < best || (q == best && c>0 && c<1)) {
      best=q; mode=c; ca=a; cb=b;
    }
  };
  consider(0,0); consider(1,n);
  for (std::size_t k=1; k<n; ++k) {
    const long double l=y[k-1], h=y[k]-l;
    if (h == 0) continue;
    const std::vector<long double> A={ss[k]-2*l*s[k]+k*l*l,
                                      2*h*(k*l-s[k]), k*h*h};
    const std::vector<long double> B={zz[k]-2*(1-l)*z[k]+(n-k)*(1-l)*(1-l),
                                      -2*h*((n-k)*(1-l)-z[k]), (n-k)*h*h};
    const std::vector<long double> u={k*l-s[k],k*h}, v={(n-k)*(1-l)-z[k],-static_cast<long double>(n-k)*h};
    auto p=snorm_detail::multiply(snorm_detail::multiply(snorm_detail::multiply(u,u),u),snorm_detail::multiply(B,B));
    const auto q=snorm_detail::multiply(snorm_detail::multiply(snorm_detail::multiply(v,v),v),snorm_detail::multiply(A,A));
    for (std::size_t j=0; j<p.size(); ++j) p[j]-=q[j];
    for (long double t : snorm_detail::roots(p)) consider(l+h*t,k);
  }
  const double ll=static_cast<double>(n*(0.5L*std::log(2.0L/M_PI)-std::log(scale)
                     -1.5L*std::log(best)+0.5L*std::log(static_cast<long double>(n))-0.5L));
  if (mode == 0) return {nan,nan,nan,ll,0,SnormStatus::RightHalfNormalBoundary};
  if (mode == 1) return {nan,nan,nan,ll,0,SnormStatus::LeftHalfNormalBoundary};
  // For the two-piece normal: E[X]=c+m1*(b-a),
  // Var[X]=a*a-a*b+b*b-m1*m1*(b-a)^2, xi=sqrt(b/a).
  const long double factor=scale*std::sqrt(best/n), a=ca*factor, b=cb*factor;
  const long double m1=std::sqrt(2.0L/M_PI);
  const double mu=static_cast<double>(origin+scale*mode+m1*(b-a));
  const double sigma=static_cast<double>(std::sqrt(a*a-a*b+b*b-m1*m1*(b-a)*(b-a)));
  return {mu,sigma,static_cast<double>(std::sqrt(b/a)),ll,0,SnormStatus::Ok};
}

}  // namespace cppunivariate
