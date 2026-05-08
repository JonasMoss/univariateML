// Pareto MLE: closed form. xmin = min(x); shape = n / sum(log(x/xmin)).

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct ParetoResult {
  double shape;
  double min;
  double logLik;
};

// Negative log-likelihood of Pareto(shape=a, min=b):
//   log f = log(a) + a*log(b) - (a+1)*log(x)  for x >= b
// Hessian (smooth interior, b < min(x)):
//   H_aa = n / a^2,  H_bb = n*a / b^2,  H_ab = -n / b
// The b parameter is the lower-support boundary; its MLE b_hat = min(x)
// sits on the boundary so it's NOT asymptotically normal — its Wald
// vcov entry is meaningless. regular_params() = {1, 0} flags this for
// Phase 4's vcov masking. The Hessian formula is still well-defined in
// the smooth interior; tests evaluate at b = b_hat * 0.99 to stay
// strictly inside the support.
struct ParetoCost {
  span x;
  int dim() const { return 2; }
  std::vector<int> regular_params() const { return {1, 0}; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0;
    for (auto v : x) {
      if (v < b) return std::numeric_limits<double>::infinity();
      S_lx += std::log(v);
    }
    return -n * std::log(a) - n * a * std::log(b) + (a + 1.0) * S_lx;
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0;
    for (auto v : x) S_lx += std::log(v);
    g[0] = -n / a - n * std::log(b) + S_lx;
    g[1] = -n * a / b;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    H[0] = n / (a * a);
    H[1] = -n / b;
    H[2] = -n / b;
    H[3] = n * a / (b * b);
  }
};

inline ParetoResult pareto_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  double xmin = x[0];
  for (std::size_t i = 1; i < n; ++i) xmin = std::min(xmin, x[i]);
  if (xmin <= 0.0) return {kNaN, kNaN, kNaN};

  double sum_log_ratio = 0.0;
  double sum_log_x = 0.0;
  for (auto v : x) {
    sum_log_ratio += std::log(v / xmin);
    sum_log_x += std::log(v);
  }
  const double dn = static_cast<double>(n);
  const double alpha = dn / sum_log_ratio;
  const double logLik = dn * std::log(alpha) + dn * alpha * std::log(xmin) -
                        (alpha + 1.0) * sum_log_x;
  return {alpha, xmin, logLik};
}

}  // namespace cppunivariate
