// Discrete Uniform MLE: a = min(x), b = max(x), pmf = 1 / (b - a + 1).

#pragma once

#include "../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct DiscreteUnifResult {
  double min;
  double max;
  double logLik;
};

// Negative log-likelihood of DiscreteUniform(a, b):
//   nll = n * log(b - a + 1)
// Smooth-continuous Hessian (treating a, b as real-valued):
//   H_aa = -n / (b - a + 1)^2
//   H_bb = -n / (b - a + 1)^2
//   H_ab = +n / (b - a + 1)^2
// Boundary MLE in both directions; regular_params() = {0, 0}.
// (Same shape as Uniform; the +1 shifts the denominator.)
struct DiscreteUnifCost {
  ispan x;
  int dim() const { return 2; }
  std::vector<int> regular_params() const { return {0, 0}; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(b >= a)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    for (auto v : x) {
      const double vd = static_cast<double>(v);
      if (vd < a || vd > b) return std::numeric_limits<double>::infinity();
    }
    return n * std::log(b - a + 1.0);
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    const double w = b - a + 1.0;
    g[0] = -n / w;
    g[1] = n / w;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    const double w = b - a + 1.0;
    const double inv_w2 = 1.0 / (w * w);
    H[0] = -n * inv_w2;
    H[1] = n * inv_w2;
    H[2] = n * inv_w2;
    H[3] = -n * inv_w2;
  }
};

inline DiscreteUnifResult dunif_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  int a = x[0];
  int b = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    a = std::min(a, x[i]);
    b = std::max(b, x[i]);
  }
  if (b < a) return {kNaN, kNaN, kNaN};
  const double logLik =
      -static_cast<double>(n) * std::log(static_cast<double>(b - a + 1));
  return {static_cast<double>(a), static_cast<double>(b), logLik};
}

}  // namespace mlcore
