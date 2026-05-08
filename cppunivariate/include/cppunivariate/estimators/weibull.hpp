// Weibull MLE via Gumbel transform on -log(x). Parameters recovered as
// shape = 1/sigma, scale = exp(-mu + s) where s = -mean(log_x).

#pragma once

#include "../core/span.hpp"
#include "gumbel.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct WeibullResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Weibull(shape=s, scale=c):
//   log f = log(s) - log(c) + (s-1)*(log(x) - log(c)) - (x/c)^s
//   nll = -sum log f.
// Per-obs Hessian via sympy (z = log(x/c), w = (x/c)^s):
//   H_ss/obs = z^2 * w + 1/s^2
//   H_cc/obs = s/c^2 * ((s+1)*w - 1)
//   H_sc/obs = (1 - w*(s*z + 1)) / c
struct WeibullCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double s = theta[0];
    const double c = theta[1];
    if (!(s > 0.0 && c > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum_lx = 0.0, sum_w = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double lx = std::log(v);
      sum_lx += lx;
      sum_w += std::exp(s * (lx - std::log(c)));
    }
    return -n * std::log(s) + n * std::log(c) - (s - 1.0) * sum_lx +
           n * (s - 1.0) * std::log(c) + sum_w;
  }
  void gradient(const double *theta, double *g) const {
    const double s = theta[0];
    const double c = theta[1];
    const double log_c = std::log(c);
    const double n = static_cast<double>(x.size());
    double dnll_ds = 0.0, dnll_dc = 0.0;
    for (auto v : x) {
      const double z = std::log(v) - log_c;        // log(x/c)
      const double w = std::exp(s * z);            // (x/c)^s
      // d log f / ds = 1/s + z - w * z
      // d log f / dc = -1/c + (s-1)*(-1/c) - w*(-s/c)
      //              = -s/c + s*w/c
      dnll_ds += -(1.0 / s + z - w * z);
      dnll_dc += -(-s / c + s * w / c);
    }
    g[0] = dnll_ds;
    g[1] = dnll_dc;
    (void)n;
  }
  void hessian(const double *theta, double *H) const {
    const double s = theta[0];
    const double c = theta[1];
    const double log_c = std::log(c);
    double sum_zz_w = 0.0;        // sum z^2 * w
    double sum_w = 0.0;
    double sum_z_w = 0.0;
    const double n = static_cast<double>(x.size());
    for (auto v : x) {
      const double z = std::log(v) - log_c;
      const double w = std::exp(s * z);
      sum_zz_w += z * z * w;
      sum_w += w;
      sum_z_w += z * w;
    }
    const double s2 = s * s;
    const double c2 = c * c;
    H[0] = sum_zz_w + n / s2;
    H[3] = (s / c2) * ((s + 1.0) * sum_w - n);
    // sum (1 - w*(s*z + 1)) / c = (n - sum w - s * sum z*w) / c
    H[1] = (n - sum_w - s * sum_z_w) / c;
    H[2] = H[1];
  }
};

inline WeibullResult weibull_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> log_x(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = -std::log(x[i]);
    sum_log_x += log_x[i];
  }
  const double dn = static_cast<double>(n);
  const double s = -sum_log_x / dn;
  for (auto &v : log_x) v += s;

  GumbelResult g = fit_gumbel_newton(span{log_x.data(), n}, tol, maxit);
  const double shape = 1.0 / g.sigma;
  const double scale = std::exp(-g.mu + s);
  const double logLik =
      dn * (std::log(shape / scale) +
            (shape - 1.0) * (s - std::log(scale)) - 1.0);
  return {shape, scale, logLik, g.iterations};
}

}  // namespace cppunivariate
