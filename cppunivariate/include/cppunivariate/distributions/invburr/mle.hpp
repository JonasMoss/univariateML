// Inverse Burr MLE: Burr MLE on 1/x. Log-likelihood rederived in the
// inverse Burr parameterisation.

#pragma once

#include "../../core/span.hpp"
#include "../burr/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class InvBurrStatus { Ok = 0, AllLeOne, InvalidInit, IterationFailure };

struct InvBurrResult {
  double shape1;
  double shape2;
  double logLik;
  int iterations;
  InvBurrStatus status;
};

// Negative log-likelihood of Inverse Burr (shape1=k, shape2=c) with scale=1:
//   log f = log(k) + log(c) + (k*c - 1)*log(x) - (k+1)*log(1 + x^c)
// Per-obs Hessian (sympy, with y := x^c, lx := log(x)):
//   H_kk/obs = 1/k^2                                       (data-indep diag)
//   H_cc/obs = 1/c^2 + (k+1)*y*lx^2 / (1+y)^2              (same as Burr)
//   H_kc/obs = -lx / (1+y)
struct InvBurrCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double k = theta[0];
    const double c = theta[1];
    if (!(k > 0.0 && c > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1py = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double lx = std::log(v);
      S_lx += lx;
      S_l1py += std::log1p(std::exp(c * lx));
    }
    return -n * (std::log(k) + std::log(c)) - (k * c - 1.0) * S_lx +
           (k + 1.0) * S_l1py;
  }
  void gradient(const double *theta, double *g) const {
    const double k = theta[0];
    const double c = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1py = 0.0, S_y_lx_over_1py = 0.0;
    for (auto v : x) {
      const double lx = std::log(v);
      const double y = std::exp(c * lx);
      S_lx += lx;
      S_l1py += std::log1p(y);
      S_y_lx_over_1py += y * lx / (1.0 + y);
    }
    g[0] = -n / k - c * S_lx + S_l1py;
    g[1] = -n / c - k * S_lx + (k + 1.0) * S_y_lx_over_1py;
  }
  void hessian(const double *theta, double *H) const {
    const double k = theta[0];
    const double c = theta[1];
    const double n = static_cast<double>(x.size());
    double H_cc_acc = 0.0, H_kc_acc = 0.0;
    for (auto v : x) {
      const double lx = std::log(v);
      const double y = std::exp(c * lx);
      const double yp1 = 1.0 + y;
      H_cc_acc += (k + 1.0) * y * lx * lx / (yp1 * yp1);
      H_kc_acc += -lx / yp1;
    }
    H[0] = n / (k * k);
    H[1] = H_kc_acc;
    H[2] = H_kc_acc;
    H[3] = n / (c * c) + H_cc_acc;
  }
};

inline InvBurrResult invburr_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, InvBurrStatus::InvalidInit};

  bool all_le_one = true;
  for (auto v : x) {
    if (!(v > 0.0) || !std::isfinite(v) || !std::isfinite(1.0 / v))
      return {kNaN, kNaN, kNaN, 0, InvBurrStatus::InvalidInit};
    if (v > 1.0) all_le_one = false;
  }
  if (all_le_one)
    return {kNaN, kNaN, kNaN, 0, InvBurrStatus::AllLeOne};

  std::vector<double> invx(n);
  double sum_log_invx = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    sum_log_invx += std::log(invx[i]);
  }
  const double dn = static_cast<double>(n);
  const double m1 = sum_log_invx / dn;
  double sq_sum = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double d = std::log(invx[i]) - m1;
    sq_sum += d * d;
  }
  const double m2 = sq_sum / dn;  // population variance, matches arma::var(.,1)

  BurrLmomResult init = burr_lmom_core(m1, m2, true);
  if (!std::isfinite(init.c) || init.c <= 0.0) {
    return {kNaN, kNaN, kNaN, 0, InvBurrStatus::InvalidInit};
  }
  BurrMLE res = burr_mle_core(span{invx.data(), n}, init.c, tol, maxit);
  if (res.status != BurrStatus::Ok) {
    return {kNaN, kNaN, kNaN, res.iterations, InvBurrStatus::IterationFailure};
  }

  const double shape1 = res.shape1;
  const double shape2 = res.shape2;
  // Change-of-variables Jacobian; reuse the checked Burr likelihood.
  const double logLik = res.logLik + 2.0 * sum_log_invx;
  if (!std::isfinite(logLik))
    return {kNaN, kNaN, kNaN, res.iterations, InvBurrStatus::IterationFailure};
  return {shape1, shape2, logLik, res.iterations, InvBurrStatus::Ok};
}

}  // namespace cppunivariate
