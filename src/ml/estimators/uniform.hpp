// Uniform MLE: a = min(x), b = max(x).

#pragma once

#include "../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct UnifResult {
  double min;
  double max;
  double logLik;
};

// Negative log-likelihood of Uniform(a, b):
//   nll = n * log(b - a)  for all x in [a, b]
// Smooth-interior Hessian:
//   H_aa = -n / (b-a)^2
//   H_bb = -n / (b-a)^2
//   H_ab = +n / (b-a)^2
// Diagonal is NEGATIVE — the smooth log-likelihood is convex in (a, b),
// not concave. This is the structural signature of a boundary-MLE
// estimator: the gradient never vanishes in the interior, and asymptotic
// theory uses scaled-extreme-value distributions instead of Wald.
// regular_params() = {0, 0}; the Phase 4 inference attribute will mark
// the entire vcov as NA. Hessian provided for completeness/diagnostics.
struct UnifCost {
  span x;
  int dim() const { return 2; }
  std::vector<int> regular_params() const { return {0, 0}; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(b > a)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    for (auto v : x) {
      if (v < a || v > b) return std::numeric_limits<double>::infinity();
    }
    return n * std::log(b - a);
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    g[0] = -n / (b - a);
    g[1] = n / (b - a);
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    const double w = b - a;
    const double inv_w2 = 1.0 / (w * w);
    H[0] = -n * inv_w2;
    H[1] = n * inv_w2;
    H[2] = n * inv_w2;
    H[3] = -n * inv_w2;
  }
};

inline UnifResult unif_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  double a = x[0];
  double b = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    a = std::min(a, x[i]);
    b = std::max(b, x[i]);
  }
  if (b <= a) return {kNaN, kNaN, kNaN};
  const double logLik = -static_cast<double>(n) * std::log(b - a);
  return {a, b, logLik};
}

}  // namespace mlcore
