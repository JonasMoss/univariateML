// Rayleigh MLE: closed form. sigma = sqrt(mean(x^2) / 2).

#pragma once

#include "../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

struct RayleighResult {
  double sigma;
  double logLik;
};

// Negative log-likelihood of Rayleigh(sigma):
//   nll(sigma) = -sum log(x_i) + 2n log(sigma) + sum(x_i^2) / (2 sigma^2).
// Hessian: -2n/sigma^2 + 3 sum(x^2)/sigma^4. At MLE this reduces to
// 4n/sigma^2 (Fisher information is exactly 4/sigma^2 per obs).
struct RayleighCost {
  span x;
  int dim() const { return 1; }
  double operator()(const double *theta) const {
    const double sigma = theta[0];
    if (!(sigma > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum_log = 0.0, sum_sq = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      sum_log += std::log(v);
      sum_sq += v * v;
    }
    return -sum_log + 2.0 * n * std::log(sigma) + sum_sq / (2.0 * sigma * sigma);
  }
  void gradient(const double *theta, double *g) const {
    const double sigma = theta[0];
    const double n = static_cast<double>(x.size());
    double sum_sq = 0.0;
    for (auto v : x) sum_sq += v * v;
    g[0] = 2.0 * n / sigma - sum_sq / (sigma * sigma * sigma);
  }
  void hessian(const double *theta, double *H) const {
    const double sigma = theta[0];
    const double n = static_cast<double>(x.size());
    double sum_sq = 0.0;
    for (auto v : x) sum_sq += v * v;
    const double s2 = sigma * sigma;
    const double s4 = s2 * s2;
    H[0] = -2.0 * n / s2 + 3.0 * sum_sq / s4;
  }
};

inline RayleighResult rayleigh_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  double sum_sq = 0.0;
  for (auto v : x) sum_sq += v * v;
  const double dn = static_cast<double>(n);
  const double sigma2 = sum_sq / (2.0 * dn);
  if (sigma2 <= 0.0) return {kNaN, kNaN};

  const double sigma = std::sqrt(sigma2);
  double sum_log = 0.0;
  for (auto v : x) sum_log += std::log(v);
  const double logLik =
      sum_log - 2.0 * dn * std::log(sigma) - sum_sq / (2.0 * sigma2);
  return {sigma, logLik};
}

}  // namespace mlcore
