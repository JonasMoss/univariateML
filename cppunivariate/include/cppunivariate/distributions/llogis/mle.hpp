// Log-logistic MLE: y = log(x), then Logistic MLE on y. Parameters are
// recovered analytically: shape = 1/scale_logistic, rate = exp(-mu_logistic).
// The log-likelihood is rederived in the log-logistic parameterisation.

#pragma once

#include "../../core/span.hpp"
#include "../logis/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct LogLogisticResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

// Negative log-likelihood of LogLogistic(shape=a, rate=r):
//   log f = log(a) + log(r) + (a-1)*(log(r) + log(x)) - 2*log(1 + (rx)^a)
// Per-obs Hessian (sympy + simplification, y := (rx)^a, lrx := log(rx)):
//   H_aa/obs = 1/a^2 + 2*y*lrx^2 / (1+y)^2
//   H_rr/obs = a*(2*a*y - y^2 + 1) / (r^2 (1+y)^2)
//   H_ar/obs = (2*a*lrx*y + y^2 - 1) / (r*(1+y)^2)
struct LogLogisticCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double r = theta[1];
    if (!(a > 0.0 && r > 0.0)) return std::numeric_limits<double>::infinity();
    double sum = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double lrx = std::log(r * v);
      // log(1 + (rx)^a) = log(1 + exp(a*lrx)). Use log1p for stability.
      const double log1py = std::log1p(std::exp(a * lrx));
      sum += -std::log(a) - std::log(r) - (a - 1.0) * lrx + 2.0 * log1py;
    }
    return sum;
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double r = theta[1];
    double dnll_da = 0.0, dnll_dr = 0.0;
    for (auto v : x) {
      const double lrx = std::log(r * v);
      const double y = std::exp(a * lrx);
      const double w = y / (1.0 + y);
      // d log f / d a = 1/a + lrx - 2*y*lrx/(1+y) = 1/a + lrx*(1 - 2w)
      // d log f / d r = 1/r + (a-1)/r - 2*y*a/(r*(1+y))
      //               = a/r - 2*a*w/r = a*(1 - 2w)/r
      dnll_da += -(1.0 / a + lrx * (1.0 - 2.0 * w));
      dnll_dr += -(a * (1.0 - 2.0 * w) / r);
    }
    g[0] = dnll_da;
    g[1] = dnll_dr;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double r = theta[1];
    double H_aa_acc = 0.0, H_rr_acc = 0.0, H_ar_acc = 0.0;
    const double r2 = r * r;
    for (auto v : x) {
      const double lrx = std::log(r * v);
      const double y = std::exp(a * lrx);
      const double yp1 = 1.0 + y;
      const double yp1_sq = yp1 * yp1;
      H_aa_acc += 1.0 / (a * a) + 2.0 * y * lrx * lrx / yp1_sq;
      H_rr_acc += a * (2.0 * a * y - y * y + 1.0) / (r2 * yp1_sq);
      H_ar_acc += (2.0 * a * lrx * y + y * y - 1.0) / (r * yp1_sq);
    }
    H[0] = H_aa_acc;
    H[1] = H_ar_acc;
    H[2] = H_ar_acc;
    H[3] = H_rr_acc;
  }
};

inline LogLogisticResult loglogistic_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> y(n);
  for (std::size_t i = 0; i < n; ++i) y[i] = std::log(x[i]);
  LogisticResult base = logistic_mle(span{y.data(), n}, tol, maxit);

  const double shape = 1.0 / base.scale;
  const double rate = std::exp(-base.mu);
  // Change of variables avoids powers of extreme observations.
  double sum_log_x = 0.0;
  for (auto v : y) sum_log_x += v;
  const double logLik = base.logLik - sum_log_x;
  return {shape, rate, logLik, base.iterations};
}

}  // namespace cppunivariate
