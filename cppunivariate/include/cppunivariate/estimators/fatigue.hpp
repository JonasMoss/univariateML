// Birnbaum-Saunders (Fatigue Life) MLE.
//
// 1D Newton on beta with absolute-step convergence; alpha is the
// closed-form maximiser given beta. Log-likelihood evaluated via
// Rmath dnorm in standard form.

#pragma once

#include "../core/newton.hpp"
#include "../core/span.hpp"
#include "../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct FatigueResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Birnbaum-Saunders Fatigue(alpha, beta) with
// mu = 0 (the wrapper's convention):
//   inner(x; beta) = x/beta + beta/x   (>= 2)
//   log f = -log(2*alpha) - log(x) + 0.5*log(inner) - 0.5*log(2*pi)
//         - (inner - 2) / (2*alpha^2)
// Per-obs Hessian (sympy):
//   H_aa/obs = -1/alpha^2 + (3/alpha^4)*(inner - 2)
//   H_ab/obs = (x^2 - beta^2) / (alpha^3 * beta^2 * x)
//   H_bb/obs = (-alpha^2*beta*x^2*(beta^2 + x^2)
//              + alpha^2*beta*(beta^2 - x^2)^2 / 2
//              + x*(beta^2 + x^2)^2)
//              / (alpha^2*beta^3*(beta^2 + x^2)^2)
// At MLE the alpha-direction simplifies: sum (inner_i - 2) = n*alpha^2,
// so sum H_aa = n*(-1/alpha^2 + 3/alpha^2) = 2n/alpha^2.
struct FatigueCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double inner = v / b + b / v;
      sum += std::log(2.0 * a) + std::log(v) - 0.5 * std::log(inner) +
             0.5 * std::log(2.0 * M_PI) + (inner - 2.0) / (2.0 * a * a);
    }
    return sum;
    (void)n;
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_inner_m2 = 0.0;
    double dnll_db = 0.0;
    for (auto v : x) {
      const double inner = v / b + b / v;
      S_inner_m2 += inner - 2.0;
      // d log f / db = (1/(2 inner)) * (-x/b^2 + 1/x) - (1/(2 alpha^2)) * (-x/b^2 + 1/x)
      const double dinner_db = -v / (b * b) + 1.0 / v;
      dnll_db += -(dinner_db / (2.0 * inner) - dinner_db / (2.0 * a * a));
    }
    g[0] = n / a - S_inner_m2 / (a * a * a);
    g[1] = dnll_db;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_inner_m2 = 0.0;
    double H_ab_acc = 0.0, H_bb_acc = 0.0;
    const double a2 = a * a;
    const double a3 = a2 * a;
    const double a4 = a2 * a2;
    const double b2 = b * b;
    const double b3 = b2 * b;
    for (auto v : x) {
      const double inner = v / b + b / v;
      const double v2 = v * v;
      S_inner_m2 += inner - 2.0;
      H_ab_acc += (v2 - b2) / (a3 * b2 * v);
      const double bsumsq = b2 + v2;
      const double bdiffsq = b2 - v2;
      const double num =
          -a2 * b * v2 * bsumsq + 0.5 * a2 * b * bdiffsq * bdiffsq +
          v * bsumsq * bsumsq;
      H_bb_acc += num / (a2 * b3 * bsumsq * bsumsq);
    }
    H[0] = -n / a2 + 3.0 * S_inner_m2 / a4;
    H[1] = H_ab_acc;
    H[2] = H_ab_acc;
    H[3] = H_bb_acc;
  }
};

inline FatigueResult fatigue_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  const double dn = static_cast<double>(n);
  double sum_x = 0.0, sum_inv_x = 0.0, sum_sqrt_x = 0.0, sum_inv_sqrt_x = 0.0;
  for (auto v : x) {
    sum_x += v;
    sum_inv_x += 1.0 / v;
    const double sx = std::sqrt(v);
    sum_sqrt_x += sx;
    sum_inv_sqrt_x += 1.0 / sx;
  }
  const double x_bar = sum_x / dn;
  const double x_inv_bar = dn / sum_inv_x;

  auto f_over_df = [&](double beta) {
    double sum_xb = 0.0, sum_xb_sq = 0.0;
    for (auto v : x) {
      const double xb = 1.0 / (v + beta);
      sum_xb += xb;
      sum_xb_sq += xb * xb;
    }
    const double xi = dn / sum_xb;
    const double dxi = sum_xb_sq * xi * xi / dn;
    const double f =
        beta * beta - beta * (2.0 * x_inv_bar + xi) + x_inv_bar * (x_bar + xi);
    const double df =
        2.0 * beta - (2.0 * x_inv_bar + xi) - beta * dxi + x_inv_bar * dxi;
    return f / df;
  };

  // Method-of-moments start.
  const double beta0 = sum_sqrt_x / sum_inv_sqrt_x;
  auto nr = cppunivariate::newton_raphson_1d_abs(beta0, f_over_df, tol, maxit);
  if (nr.x <= 0.0) return {kNaN, kNaN, kNaN, nr.iterations};

  const double beta = nr.x;
  const double alpha = std::sqrt(x_bar / beta + beta / x_inv_bar);

  double logLik = 0.0;
  for (auto v : x) {
    const double sx = std::sqrt(v / beta);
    const double sy = std::sqrt(beta / v);
    const double z = (sx - sy) / alpha;
    const double log_pref = std::log(sx + sy) - std::log(2.0 * alpha) - std::log(v);
    const double log_phi = cppunivariate::special::dnorm(z, 0.0, 1.0, 1);
    logLik += log_pref + log_phi;
  }
  return {alpha, beta, logLik, nr.iterations};
}

}  // namespace cppunivariate
