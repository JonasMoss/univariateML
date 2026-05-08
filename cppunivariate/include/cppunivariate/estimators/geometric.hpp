// Geometric MLE: closed-form prob = 1 / (mean(x) + 1).
//
// Depends on the Rmath-backed special::dgeom for the log-likelihood
// summation (matches existing Rmath behaviour bit-for-bit).

#pragma once

#include "../core/span.hpp"
#include "../special/special.hpp"

#include <cstddef>
#include <limits>

namespace cppunivariate {

struct GeomResult {
  double prob;
  double logLik;
};

// Negative log-likelihood of Geometric(prob=p), R convention (x in 0,1,2,...):
//   nll(p) = -n * log(p) - sum(x) * log(1 - p).
// Hessian = n/p^2 + sum(x)/(1-p)^2. At MLE this equals n / (p^2 (1-p))
// (closed-form Fisher info * n).
struct GeomCost {
  ispan x;
  int dim() const { return 1; }
  double operator()(const double *theta) const {
    const double p = theta[0];
    if (!(p > 0.0 && p < 1.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    return -n * std::log(p) - S_x * std::log(1.0 - p);
  }
  void gradient(const double *theta, double *g) const {
    const double p = theta[0];
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    g[0] = -n / p + S_x / (1.0 - p);
  }
  void hessian(const double *theta, double *H) const {
    const double p = theta[0];
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    const double q = 1.0 - p;
    H[0] = n / (p * p) + S_x / (q * q);
  }
};

inline GeomResult geom_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN};

  double sum = 0.0;
  for (auto v : x) sum += static_cast<double>(v);
  const double mean = sum / static_cast<double>(n);
  const double prob = 1.0 / (mean + 1.0);

  double logLik = 0.0;
  for (auto v : x) {
    logLik += ::cppunivariate::special::dgeom(static_cast<double>(v), prob, 1);
  }
  return {prob, logLik};
}

}  // namespace cppunivariate
