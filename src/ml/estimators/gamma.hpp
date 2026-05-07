// Gamma MLE: Newton-Raphson on shape via the digamma equation
//   log(shape) - psi(shape) = log(mean(x)) - mean(log x)
// then rate = shape / mean(x). Initialization uses Minka's closed-form
// approximation (with a small empirical scaling that matches the R impl
// 1:1 — preserved verbatim so goldens reproduce).
//
// Convergence is absolute on the Newton step (not relative): the R-side
// implementations of dependent estimators (lgamma, naka, invgamma) were
// written against this rule, so newton_raphson_1d_abs is the right helper.

#pragma once

#include "../core/newton.hpp"
#include "../core/reductions.hpp"
#include "../core/span.hpp"
#include "../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

struct GammaResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Gamma(shape, rate):
//   nll(a, r) = -n*a*log(r) + n*lgamma(a) - (a-1)*sum log(x) + r*sum(x).
// Hessian (data-independent — these are exact closed forms; equal Fisher
// info × n at the MLE):
//   H_aa = n*trigamma(a)
//   H_rr = n*a / r^2
//   H_ar = -n / r
struct GammaCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double r = theta[1];
    if (!(a > 0.0 && r > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_x = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      S_lx += std::log(v);
      S_x += v;
    }
    return -n * a * std::log(r) + n * mlcore::special::lgamma(a) -
           (a - 1.0) * S_lx + r * S_x;
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double r = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_x = 0.0;
    for (auto v : x) { S_lx += std::log(v); S_x += v; }
    g[0] = -n * std::log(r) + n * mlcore::special::digamma(a) - S_lx;
    g[1] = -n * a / r + S_x;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double r = theta[1];
    const double n = static_cast<double>(x.size());
    H[0] = n * mlcore::special::trigamma(a);
    H[1] = -n / r;
    H[2] = H[1];
    H[3] = n * a / (r * r);
  }
};

inline GammaResult gamma_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  const double dn = static_cast<double>(n);
  const double mean_hat = mlcore::mean(x);
  const double lx_bar = mlcore::mean_log(x);
  const double s = std::log(mean_hat) - lx_bar;

  auto f_over_df = [s](double shape) {
    return (std::log(shape) - mlcore::special::digamma(shape) - s) /
           (1.0 / shape - mlcore::special::trigamma(shape));
  };
  const double init = 1.0 / (12.0 * s) *
                          (3.0 - s + std::sqrt((s - 3.0) * (s - 3.0) + 24.0 * s)) *
                          0.9989 +
                      0.0102;
  auto nr = mlcore::newton_raphson_1d_abs(init, f_over_df, tol, maxit);

  const double rate = nr.x / mean_hat;
  const double logLik = dn * (nr.x * std::log(rate) - std::lgamma(nr.x) +
                              (nr.x - 1.0) * lx_bar - rate * mean_hat);
  return {nr.x, rate, logLik, nr.iterations};
}

}  // namespace mlcore
