// Power distribution MLE: closed form using log(x) statistics.
//
// alpha = max(x) + epsilon, beta = 1 / (log(alpha) - mean(log x)).
// Returns NaN params if min(x) <= 0; the caller turns that into an error.

#pragma once

#include "../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct PowerResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Power(alpha=upper-bound, beta=shape):
//   f(x; alpha, beta) = beta * x^(beta-1) / alpha^beta  for 0 < x < alpha
//   log f = log(beta) + (beta-1)*log(x) - beta*log(alpha)
// Per-obs Hessian (data-independent — alpha doesn't appear inside any sum):
//   H_aa/obs = -beta / alpha^2   (NEGATIVE — alpha is upper-support boundary)
//   H_bb/obs = 1 / beta^2
//   H_ab/obs = 1 / alpha
// regular_params() = {0, 1}: alpha is non-regular (boundary MLE
// alpha_hat = max(x) + eps), beta is regular.
struct PowerCost {
  span x;
  int dim() const { return 2; }
  std::vector<int> regular_params() const { return {0, 1}; }
  double operator()(const double *theta) const {
    const double alpha = theta[0];
    const double beta = theta[1];
    if (!(alpha > 0.0 && beta > 0.0))
      return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0;
    for (auto v : x) {
      if (!(v > 0.0 && v < alpha))
        return std::numeric_limits<double>::infinity();
      S_lx += std::log(v);
    }
    return -n * std::log(beta) - (beta - 1.0) * S_lx + n * beta * std::log(alpha);
  }
  void gradient(const double *theta, double *g) const {
    const double alpha = theta[0];
    const double beta = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0;
    for (auto v : x) S_lx += std::log(v);
    g[0] = n * beta / alpha;
    g[1] = -n / beta - S_lx + n * std::log(alpha);
  }
  void hessian(const double *theta, double *H) const {
    const double alpha = theta[0];
    const double beta = theta[1];
    const double n = static_cast<double>(x.size());
    H[0] = -n * beta / (alpha * alpha);
    H[1] = n / alpha;
    H[2] = n / alpha;
    H[3] = n / (beta * beta);
  }
};

inline PowerResult power_mle(span x, double epsilon) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  double xmin = x[0];
  double xmax = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    xmin = std::min(xmin, x[i]);
    xmax = std::max(xmax, x[i]);
  }
  if (xmin <= 0.0) return {kNaN, kNaN, kNaN, 0};

  double sum_log = 0.0;
  for (auto v : x) sum_log += std::log(v);
  const double m = sum_log / static_cast<double>(n);
  const double alpha = xmax + epsilon;
  const double beta = 1.0 / (std::log(alpha) - m);
  const double logLik = static_cast<double>(n) *
      (std::log(beta) - beta * std::log(alpha) + (beta - 1.0) * m);
  return {alpha, beta, logLik, 0};
}

}  // namespace cppunivariate
