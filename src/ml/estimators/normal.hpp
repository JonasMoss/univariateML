// Normal MLE: closed form.
//
// Standalone C++ implementation, free of R / Armadillo dependencies.
// Migrated from src/ml/ml.hpp during the WS1 de-Armadillo sweep.
//
// The negative-log-likelihood Cost (`NormCost`) carries the data and exposes
// `operator()`, `gradient`, and `hessian` — the latter is the analytical
// observed-information matrix used by the asymptotic-inference pass. At the
// MLE the Hessian reduces to diag(n/sigma^2, 2n/sigma^2).

#pragma once

#include "../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

struct NormResult {
  double mean;
  double sd;
  double logLik;
};

// Negative log-likelihood of N(mu, sigma^2):
//   nll(mu, sigma) = (n/2) log(2 pi sigma^2) + sum (x_i - mu)^2 / (2 sigma^2)
struct NormCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    if (!(sigma > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double ss = 0.0;
    for (auto v : x) { const double d = v - mu; ss += d * d; }
    return 0.5 * n * std::log(2.0 * M_PI * sigma * sigma) +
           ss / (2.0 * sigma * sigma);
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    double sum_d = 0.0, sum_d2 = 0.0;
    for (auto v : x) {
      const double d = v - mu;
      sum_d += d;
      sum_d2 += d * d;
    }
    const double n = static_cast<double>(x.size());
    const double s2 = sigma * sigma;
    g[0] = -sum_d / s2;
    g[1] = n / sigma - sum_d2 / (s2 * sigma);
  }
  void hessian(const double *theta, double *H) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    double sum_d = 0.0, sum_d2 = 0.0;
    for (auto v : x) {
      const double d = v - mu;
      sum_d += d;
      sum_d2 += d * d;
    }
    const double n = static_cast<double>(x.size());
    const double s2 = sigma * sigma;
    const double s3 = s2 * sigma;
    const double s4 = s2 * s2;
    H[0] = n / s2;
    H[1] = 2.0 * sum_d / s3;
    H[2] = H[1];
    H[3] = -n / s2 + 3.0 * sum_d2 / s4;
  }
};

inline NormResult norm_mle(span x) {
  const std::size_t n = x.size();
  if (n == 0) {
    constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
    return {kNaN, kNaN, kNaN};
  }
  double sum = 0.0;
  for (auto v : x) sum += v;
  const double mu = sum / static_cast<double>(n);
  double ss = 0.0;
  for (auto v : x) {
    const double d = v - mu;
    ss += d * d;
  }
  const double var = ss / static_cast<double>(n);
  const double sigma = std::sqrt(var);
  const double logLik =
      -0.5 * static_cast<double>(n) * (std::log(2.0 * M_PI * var) + 1.0);
  return {mu, sigma, logLik};
}

}  // namespace mlcore
