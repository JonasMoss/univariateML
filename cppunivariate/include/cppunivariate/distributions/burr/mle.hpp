// Burr type-XII MLE.
//
// Two-stage: L-moment-based root-finding seeds shape `c`, then Newton on
// log(c) with the score's analytical derivative refines. After convergence,
// shape `k` is the closed-form maximiser.
//
// The internal helpers (logit/logistic/logistic_pdf, three invert_*
// solvers for the L-moment equation, burr_lmom_core, burr_mle_core) are
// kept in this header rather than core/ because Burr is the only consumer.
// If a second user emerges later, lift them out then.
//
// Status enum: AllGeOne fires when every observation is >= 1, in which
// case the Burr MLE doesn't exist (caller should fall back to Pareto).

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct RootResult {
  double k;
  int iterations;
};

struct BurrLmomResult {
  double k;
  double c;
  int iterations;
};

enum class BurrStatus { Ok = 0, AllGeOne, InvalidInit, IterationFailure };

struct BurrMLE {
  double shape1;
  double shape2;
  double logLik;
  int iterations;
  BurrStatus status;
};

// Negative log-likelihood of Burr type-XII (shape1=k, shape2=c) with scale=1:
//   log f = log(k) + log(c) + (c-1)*log(x) - (k+1)*log(1 + x^c)
// Per-obs Hessian (sympy, with y := x^c, lx := log(x)):
//   H_kk/obs = 1/k^2                                       (data-indep diag)
//   H_cc/obs = 1/c^2 + (k+1)*y*lx^2 / (1+y)^2
//   H_kc/obs = y*lx / (1+y)
struct BurrCost {
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
    return -n * (std::log(k) + std::log(c)) - (c - 1.0) * S_lx +
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
    g[0] = -n / k + S_l1py;
    g[1] = -n / c - S_lx + (k + 1.0) * S_y_lx_over_1py;
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
      H_kc_acc += y * lx / yp1;
    }
    H[0] = n / (k * k);
    H[1] = H_kc_acc;
    H[2] = H_kc_acc;
    H[3] = n / (c * c) + H_cc_acc;
  }
};

namespace detail {

inline double logit(double p) { return std::log(p / (1.0 - p)); }

inline double logistic(double x) {
  const double e = std::exp(-x);
  return 1.0 / (1.0 + e);
}

inline double logistic_pdf(double x) {
  const double L = logistic(x);
  return L * (1.0 - L);
}

inline RootResult invert_lower_logit(double cv, double tol, int maxit) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  const double loc = -0.5495872;
  const double scl = 0.6103216;
  const double cv_lo = 0.1;
  const double cv_hi = 0.95;
  const double eps = 1e-15;

  double k0;
  if (cv < cv_lo) {
    k0 = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k0 = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    const double y = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    const double xlog = loc + scl * logit(y);
    k0 = logistic((xlog - loc) / scl);
  }
  k0 = std::min(1.0 - eps, std::max(eps, k0));

  double x = logit(k0);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double k = logistic(x);
    const double di = cppunivariate::special::digamma(k);
    const double tri = cppunivariate::special::trigamma(k);
    const double tetra = cppunivariate::special::tetragamma(k);
    const double pd = di - NEULER_CST;
    const double bpt = BASEL_CST + tri;
    const double f = (pd * pd) / bpt - cv;
    const double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    const double df = fpk * logistic_pdf(x);

    const double step = f / df;
    const double x1 = x - step;
    if (std::abs(x1 - x) < (std::abs(x) * tol + 1e-3 * tol)) {
      x = x1;
      break;
    }
    x = x1;
  }
  return {logistic(x), iter + 1};
}

inline RootResult invert_lower_log(double cv, double tol, int maxit) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  const double loc = -0.5495872;
  const double scl = 0.6103216;
  const double cv_lo = 0.1;
  const double cv_hi = 0.95;
  const double eps = 1e-15;

  double k0;
  if (cv < cv_lo) {
    k0 = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k0 = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    const double y = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    const double xlog = loc + scl * logit(y);
    k0 = logistic((xlog - loc) / scl);
  }
  k0 = std::max(eps, std::min(1.0 - eps, k0));

  double p = std::log(k0);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double k = std::exp(p);
    const double di = cppunivariate::special::digamma(k);
    const double tri = cppunivariate::special::trigamma(k);
    const double tetra = cppunivariate::special::tetragamma(k);
    const double pd = di - NEULER_CST;
    const double bpt = BASEL_CST + tri;
    const double f = (pd * pd) / bpt - cv;
    const double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    const double dfp = fpk * k;

    const double step = f / dfp;
    const double p1 = p - step;
    if (std::abs(p1 - p) < (std::abs(p) * tol + 1e-3 * tol)) {
      p = p1;
      break;
    }
    p = p1;
  }
  return {std::exp(p), iter + 1};
}

inline RootResult invert_upper(double cv, double tol, int maxit) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  const double thr = 0.2;
  const double eps = 1e-9;

  double k0;
  if (cv < thr) {
    k0 = 1.0 + std::sqrt(2.0 * cv / BASEL_CST);
  } else {
    k0 = std::exp(std::sqrt(cv * BASEL_CST));
  }
  k0 = std::max(1.0 + eps, k0);

  double x = std::log(k0 - 1.0);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double kk = std::exp(x) + 1.0;
    const double di = cppunivariate::special::digamma(kk);
    const double tri = cppunivariate::special::trigamma(kk);
    const double tetra = cppunivariate::special::tetragamma(kk);
    const double pd = di - NEULER_CST;
    const double bpt = BASEL_CST + tri;
    const double f = (pd * pd) / bpt - cv;
    const double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    const double df = fpk * std::exp(x);

    const double step = f / df;
    const double x1 = x - step;
    if (std::abs(x1 - x) < (std::abs(x) * tol + 1e-3 * tol)) {
      x = x1;
      break;
    }
    x = x1;
  }
  return {std::exp(x) + 1.0, iter + 1};
}

}  // namespace detail

inline BurrLmomResult burr_lmom_core(double mu, double sigma2,
                                     bool use_logit_lower) {
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  constexpr double kEps = std::numeric_limits<double>::epsilon();
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();

  double k_hat = kNaN;
  double c_hat = kNaN;
  int iter = 0;

  // Match the original tol/maxit defaults from kTolDefault/kMaxIterDefault.
  constexpr double tol_default = 1.22e-4;
  constexpr int maxit_default = 100;

  if (sigma2 > 0.0 && std::abs(mu) >= kEps) {
    const double cv = (mu * mu) / sigma2;
    RootResult root = (mu > 0.0)
        ? (use_logit_lower
               ? detail::invert_lower_logit(cv, tol_default, maxit_default)
               : detail::invert_lower_log(cv, tol_default, maxit_default))
        : detail::invert_upper(cv, tol_default, maxit_default);
    k_hat = root.k;
    iter = root.iterations;

    const double t1 = cppunivariate::special::trigamma(k_hat);
    const double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0.0) c_hat = std::sqrt(csq);
  } else if (std::abs(mu) < kEps) {
    k_hat = 1.0;
    const double t1 = cppunivariate::special::trigamma(1.0);
    const double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0.0) c_hat = std::sqrt(csq);
  }
  return {k_hat, c_hat, iter};
}

inline BurrMLE burr_mle_core(span x, double c_start, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  const double nd = static_cast<double>(n);
  if (n == 0) return {0.0, 0.0, kNaN, 0, BurrStatus::InvalidInit};

  bool all_ge_one = true;
  for (auto v : x) {
    if (v < 1.0) { all_ge_one = false; break; }
  }
  if (all_ge_one) return {kNaN, kNaN, kNaN, 0, BurrStatus::AllGeOne};

  std::vector<double> log_x(n);
  double sum_l = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = std::log(x[i]);
    sum_l += log_x[i];
  }

  double logc = std::log(c_start);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double c = std::exp(logc);
    double g = 0.0, dg = 0.0, d2g1 = 0.0, d2g2 = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double lx = log_x[i];
      const double xc = std::exp(c * lx);
      const double t = xc / (xc + 1.0);
      g += std::log1p(xc);
      dg += lx * t;
      const double lx_t = lx * t;
      d2g1 += lx * lx * t;
      d2g2 += lx_t * lx_t;
    }
    const double d2g = d2g1 - d2g2;
    const double g2 = g * g;
    const double f_c = nd / c - nd * dg / g + sum_l - dg;
    const double df_c = -nd / (c * c) - nd * (g * d2g - dg * dg) / g2 - d2g;
    const double f_p = c * f_c;
    const double d2_p = c * (f_c + c * df_c);
    const double step_p = f_p / d2_p;
    logc -= step_p;
    if (std::fabs(step_p) < tol) {
      ++iter;
      break;
    }
  }

  const double c = std::exp(logc);
  double sum_log1p = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum_log1p += std::log1p(std::exp(c * log_x[i]));
  }
  const double k = nd / sum_log1p;
  const double logLik = nd * (std::log(k) + std::log(c)) +
                        (c - 1.0) * sum_l - (k + 1.0) * sum_log1p;
  return {k, c, logLik, iter == maxit ? maxit : iter, BurrStatus::Ok};
}

}  // namespace cppunivariate
