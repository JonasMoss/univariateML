// Poisson MLE: lambda = mean(x). Log-likelihood via Rmath dpois for
// bit-identical reproduction of pre-port behaviour.

#pragma once

#include "../core/span.hpp"
#include "../special/special.hpp"

#include <cstddef>
#include <limits>

namespace cppunivariate {

struct PoisResult {
  double lambda;
  double logLik;
};

// Negative log-likelihood of Poisson(lambda):
//   nll(lambda) = -sum(x) * log(lambda) + n * lambda + const.
// Hessian = sum(x) / lambda^2; at MLE this equals n / lambda_hat
// (since lambda_hat = mean(x), sum(x) = n * lambda_hat).
struct PoisCost {
  ispan x;
  int dim() const { return 1; }
  double operator()(const double *theta) const {
    const double lam = theta[0];
    if (!(lam > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    return -S_x * std::log(lam) + n * lam;
  }
  void gradient(const double *theta, double *g) const {
    const double lam = theta[0];
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    g[0] = -S_x / lam + n;
  }
  void hessian(const double *theta, double *H) const {
    const double lam = theta[0];
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    H[0] = S_x / (lam * lam);
  }
};

inline PoisResult pois_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  double sum = 0.0;
  for (auto v : x) sum += static_cast<double>(v);
  const double lambda = sum / static_cast<double>(n);

  double logLik = 0.0;
  for (auto v : x) {
    logLik += ::cppunivariate::special::dpois(static_cast<double>(v), lambda, 1);
  }
  return {lambda, logLik};
}

}  // namespace cppunivariate
