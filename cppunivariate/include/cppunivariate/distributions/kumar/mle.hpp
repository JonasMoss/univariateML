// Kumaraswamy MLE: two-stage. Stage 1 uses an L-moment-based Newton on
// `a` to seed; stage 2 refines `a` by Newton on the score. `b` is then
// the closed-form maximiser. Convergence is hybrid (relative + absolute):
//   |step| < |a| * tol + tol
// kept verbatim for golden parity.

#pragma once

#include "../../core/inverse_digamma.hpp"
#include "../../core/reductions.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct KumarResult {
  double a;
  double b;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Kumaraswamy(a, b):
//   log f = log(a) + log(b) + (a-1)*log(x) + (b-1)*log(1 - x^a)
// Per-obs Hessian (sympy, with y = x^a):
//   H_aa/obs = (b-1)*y*log(x)^2 / (1 - y)^2 + 1/a^2
//   H_bb/obs = 1 / b^2                                 (data-indep diagonal)
//   H_ab/obs = y*log(x) / (1 - y)
struct KumarCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1my = 0.0;
    for (auto v : x) {
      if (!(v > 0.0 && v < 1.0)) return std::numeric_limits<double>::infinity();
      S_lx += std::log(v);
      S_l1my += std::log1p(-std::pow(v, a));
    }
    return -n * (std::log(a) + std::log(b)) - (a - 1.0) * S_lx -
           (b - 1.0) * S_l1my;
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1my = 0.0, S_y_lx_over_1my = 0.0;
    for (auto v : x) {
      const double y = std::pow(v, a);
      const double lx = std::log(v);
      S_lx += lx;
      S_l1my += std::log1p(-y);
      S_y_lx_over_1my += y * lx / (1.0 - y);
    }
    // d log f / da = 1/a + log(x) + (b-1) * (-x^a * log(x)) / (1 - x^a)
    //              = 1/a + log(x) - (b-1) * y*lx / (1-y)
    g[0] = -n / a - S_lx + (b - 1.0) * S_y_lx_over_1my;
    g[1] = -n / b - S_l1my;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double H_aa_acc = 0.0, H_ab_acc = 0.0;
    for (auto v : x) {
      const double y = std::pow(v, a);
      const double lx = std::log(v);
      const double one_my = 1.0 - y;
      H_aa_acc += (b - 1.0) * y * lx * lx / (one_my * one_my);
      H_ab_acc += y * lx / one_my;
    }
    H[0] = H_aa_acc + n / (a * a);
    H[1] = H_ab_acc;
    H[2] = H_ab_acc;
    H[3] = n / (b * b);
  }
};

inline KumarResult kumar_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  const double mu = cppunivariate::mean_log(x);
  // sigma2 = mean(log(x)^2) - mu^2
  double sum_log_sq = 0.0;
  for (auto v : x) {
    const double l = std::log(v);
    sum_log_sq += l * l;
  }
  const double sigma2 = sum_log_sq / static_cast<double>(n) - mu * mu;

  // Stage 1: Newton on `a` from L-moment estimating equation.
  auto f_over_df_lmom = [&](double a0) {
    const double b = inverse_digamma_scalar(-0.5772156649015328606 - a0 * mu);
    const double tri = cppunivariate::special::trigamma(b);
    const double tetra = cppunivariate::special::tetragamma(b);
    const double basel = M_PI * M_PI / 6.0;
    const double f = a0 * tri * (basel - tri - a0 * a0 * sigma2);
    const double df = a0 * tetra * mu - 2.0 * tri * (basel + tri);
    return f / df;
  };
  double a0 = std::sqrt((M_PI * M_PI / 6.0) / sigma2);
  for (int i = 0; i < maxit; ++i) {
    const double step = f_over_df_lmom(a0);
    a0 -= step;
    if (std::abs(step) < (std::abs(a0) * tol + tol)) break;
  }
  if (a0 <= 0.0) a0 = std::sqrt((M_PI * M_PI / 6.0) / sigma2);

  // Stage 2: full ML Newton on `a`.
  auto f_over_df = [&](double a) {
    double sum_log1p_neg_xa = 0.0;
    double sum_xa_z = 0.0;
    double sum_xa_z2 = 0.0;
    for (auto v : x) {
      const double log_v = std::log(v);
      const double xa = std::exp(a * log_v);
      const double z = log_v / (1.0 - xa);
      sum_log1p_neg_xa += std::log1p(-xa);
      sum_xa_z += xa * z;
      sum_xa_z2 += xa * z * z;
    }
    const double dn = static_cast<double>(n);
    const double alpha = sum_log1p_neg_xa / dn;
    const double beta_v = sum_xa_z / dn;
    const double gamma_v = sum_xa_z2 / dn;

    const double f = 1.0 / a + mu + (1.0 / alpha + 1.0) * beta_v;
    const double df = -1.0 / (a * a) +
                      (gamma_v * alpha + beta_v * beta_v) / (alpha * alpha) +
                      gamma_v;
    return f / df;
  };

  double a = a0;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double step = f_over_df(a);
    a -= step;
    if (a <= 0.0 || !std::isfinite(a)) {
      return {kNaN, kNaN, kNaN, iter};
    }
    if (std::abs(step) < (std::abs(a) * tol + tol)) break;
  }

  double sum_log1p_neg_xa = 0.0, sum_log_x = 0.0;
  for (auto v : x) {
    sum_log1p_neg_xa += std::log1p(-std::pow(v, a));
    sum_log_x += std::log(v);
  }
  const double dn = static_cast<double>(n);
  const double b = -dn / sum_log1p_neg_xa;
  const double logLik = dn * (std::log(a) + std::log(b)) +
                        (a - 1.0) * sum_log_x +
                        (b - 1.0) * sum_log1p_neg_xa;
  return {a, b, logLik, iter};
}

}  // namespace cppunivariate
