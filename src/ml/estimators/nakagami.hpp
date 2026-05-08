// Nakagami MLE: y = x^2, then Gamma MLE on y. Shape parameter is
// preserved; scale is rederived from the gamma rate as scale = shape/rate.

#pragma once

#include "../core/span.hpp"
#include "../special/special.hpp"
#include "gamma.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct NakaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Nakagami(shape=m, scale=Omega):
//   log f = log(2) + m*log(m) - lgamma(m) - m*log(W) + (2m-1)*log(x) - m x^2/W
// Hessian (sympy-derived):
//   H_mm/obs = trigamma(m) - 1/m                                  (data-indep!)
//   H_WW/obs = m * (2 x^2 - W) / W^3
//   H_mW/obs = (W - x^2) / W^2
// At MLE the score for W is zero ⇒ sum(x^2) = n*W ⇒ H_WW(MLE) = n*m/W^2
// and H_mW(MLE) = 0; observed info at MLE is data-independent diagonal.
struct NakaCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double m = theta[0];
    const double W = theta[1];
    if (!(m > 0.0 && W > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum_log_x = 0.0, sum_x2 = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      sum_log_x += std::log(v);
      sum_x2 += v * v;
    }
    return -n * std::log(2.0) - n * m * std::log(m) +
           n * cppunivariate::special::lgamma(m) + n * m * std::log(W) -
           (2.0 * m - 1.0) * sum_log_x + m * sum_x2 / W;
  }
  void gradient(const double *theta, double *g) const {
    const double m = theta[0];
    const double W = theta[1];
    const double n = static_cast<double>(x.size());
    double sum_log_x = 0.0, sum_x2 = 0.0;
    for (auto v : x) { sum_log_x += std::log(v); sum_x2 += v * v; }
    g[0] = -n * (std::log(m) + 1.0) + n * cppunivariate::special::digamma(m) +
           n * std::log(W) - 2.0 * sum_log_x + sum_x2 / W;
    g[1] = n * m / W - m * sum_x2 / (W * W);
  }
  void hessian(const double *theta, double *H) const {
    const double m = theta[0];
    const double W = theta[1];
    const double n = static_cast<double>(x.size());
    double sum_x2 = 0.0;
    for (auto v : x) sum_x2 += v * v;
    H[0] = n * (cppunivariate::special::trigamma(m) - 1.0 / m);
    H[3] = m * (2.0 * sum_x2 - n * W) / (W * W * W);
    H[1] = (n * W - sum_x2) / (W * W);
    H[2] = H[1];
  }
};

inline NakaResult naka_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> x2(n);
  double sum_log_x = 0.0;
  double sum_x2 = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    x2[i] = x[i] * x[i];
    sum_log_x += std::log(x[i]);
    sum_x2 += x2[i];
  }
  const double dn = static_cast<double>(n);

  GammaResult g = gamma_mle(span{x2.data(), n}, tol, maxit);
  const double shape = g.shape;
  const double scale = shape / g.rate;
  const double logLik =
      dn * (shape * std::log(shape) + std::log(2.0) - std::lgamma(shape) -
            shape * std::log(scale)) +
      (2.0 * shape - 1.0) * sum_log_x - (shape / scale) * sum_x2;
  return {shape, scale, logLik, g.iterations};
}

}  // namespace cppunivariate
