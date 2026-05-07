// Beta MLE: Newton-Raphson on log(beta), with alpha solved via a small
// digamma-inversion inner loop each iteration.
//
// Convergence is relative on log(beta): |db/b| < tol. Bit-identical to
// the prior arma'd implementation.

#pragma once

#include "../core/reductions.hpp"
#include "../core/span.hpp"
#include "../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

struct BetaResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Beta(a, b):
//   nll(a, b) = n*[lgamma(a)+lgamma(b)-lgamma(a+b)] - (a-1)*sum log(x)
//             - (b-1)*sum log(1-x).
// Hessian (data-independent closed form):
//   H_aa = n*(trigamma(a) - trigamma(a+b))
//   H_bb = n*(trigamma(b) - trigamma(a+b))
//   H_ab = -n*trigamma(a+b)
struct BetaCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1mx = 0.0;
    for (auto v : x) {
      if (!(v > 0.0 && v < 1.0)) return std::numeric_limits<double>::infinity();
      S_lx += std::log(v);
      S_l1mx += std::log(1.0 - v);
    }
    return n * (mlcore::special::lgamma(a) + mlcore::special::lgamma(b) -
                mlcore::special::lgamma(a + b)) -
           (a - 1.0) * S_lx - (b - 1.0) * S_l1mx;
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1mx = 0.0;
    for (auto v : x) { S_lx += std::log(v); S_l1mx += std::log(1.0 - v); }
    const double psi_ab = mlcore::special::digamma(a + b);
    g[0] = n * (mlcore::special::digamma(a) - psi_ab) - S_lx;
    g[1] = n * (mlcore::special::digamma(b) - psi_ab) - S_l1mx;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    const double psi1_ab = mlcore::special::trigamma(a + b);
    H[0] = n * (mlcore::special::trigamma(a) - psi1_ab);
    H[1] = -n * psi1_ab;
    H[2] = H[1];
    H[3] = n * (mlcore::special::trigamma(b) - psi1_ab);
  }
};

inline BetaResult beta_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  const double dn = static_cast<double>(n);
  double sum_log_x = 0.0, sum_log_1mx = 0.0, sum_x = 0.0, sum_xsq = 0.0;
  for (auto v : x) {
    sum_log_x += std::log(v);
    sum_log_1mx += std::log(1.0 - v);
    sum_x += v;
    sum_xsq += v * v;
  }
  const double s = sum_log_x / dn;
  const double r = sum_log_1mx / dn;
  const double m = sum_x / dn;
  const double v_var = sum_xsq / dn - m * m;

  const double nu = m * (1.0 - m) / v_var - 1.0;
  double b = std::log((1.0 - m) * nu);
  double alpha = kNaN;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double exp_b = std::exp(b);
    const double digamma_b = mlcore::special::digamma(exp_b);
    const double trigamma_b = mlcore::special::trigamma(exp_b);
    const double y = s + digamma_b - r;

    alpha = 1.0 / std::log1p(std::exp(-y));
    for (int k = 0; k < 3; ++k) {
      const double ra = mlcore::special::digamma(alpha) - y;
      alpha -= ra / mlcore::special::trigamma(alpha);
    }

    const double f = exp_b *
        (digamma_b - r - mlcore::special::digamma(alpha + exp_b));
    const double df = f + exp_b * exp_b *
        (trigamma_b - mlcore::special::trigamma(alpha + exp_b) *
                          (trigamma_b / mlcore::special::trigamma(alpha) + 1.0));
    const double b0 = b - f / df;
    if (std::abs((b0 - b) / b0) < tol) {
      b = b0;
      break;
    }
    b = b0;
  }

  const double beta = std::exp(b);
  const double logLik = dn *
      ((alpha - 1.0) * s + (beta - 1.0) * r - std::lgamma(alpha) -
       std::lgamma(beta) + std::lgamma(alpha + beta));
  return {alpha, beta, logLik, iter};
}

}  // namespace mlcore
