// Exponential MLE: rate = 1 / mean(x).

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct ExpResult {
  double rate;
  double logLik;
};

// Negative log-likelihood of Exp(rate=lambda):
//   nll(lambda) = -n log(lambda) + lambda * sum(x).
// Hessian = n / lambda^2 (data-independent at fixed lambda).
struct ExpCost {
  span x;
  int dim() const { return 1; }
  double operator()(const double *theta) const {
    const double lambda = theta[0];
    if (!(lambda > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum_x = 0.0;
    for (auto v : x) sum_x += v;
    return -n * std::log(lambda) + lambda * sum_x;
  }
  void gradient(const double *theta, double *g) const {
    const double lambda = theta[0];
    const double n = static_cast<double>(x.size());
    double sum_x = 0.0;
    for (auto v : x) sum_x += v;
    g[0] = -n / lambda + sum_x;
  }
  void hessian(const double *theta, double *H) const {
    const double lambda = theta[0];
    const double n = static_cast<double>(x.size());
    H[0] = n / (lambda * lambda);
  }
};

inline ExpResult exp_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  double sum = 0.0;
  for (auto v : x) sum += v;
  const double dn = static_cast<double>(n);
  const double rate = dn / sum;
  const double logLik = dn * std::log(rate) - rate * sum;
  return {rate, logLik};
}

}  // namespace cppunivariate
