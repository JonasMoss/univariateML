// Inverse Weibull MLE via Gumbel transform on log(x). Parameters recovered
// as shape = 1/sigma, rate = exp(-mu + s) where s = -mean(log x).

#pragma once

#include "../../core/span.hpp"
#include "../gumbel/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct InvWeibullResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

// Negative log-likelihood of InverseWeibull(shape=s, rate=r) [actuar
// parameterisation, rate = 1/scale]:
//   log f = log(s) - s*log(r) - (s+1)*log(x) - (r*x)^(-s)
// Per-obs Hessian (sympy, with u = (r*x)^(-s), lrx = log(r*x)):
//   H_ss/obs = u*lrx^2 + 1/s^2
//   H_rr/obs = s*(u*(s+1) - 1) / r^2
//   H_sr/obs = (1 + u*s*lrx - u) / r
struct InvWeibullCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double s = theta[0];
    const double r = theta[1];
    if (!(s > 0.0 && r > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double u = std::exp(-s * std::log(r * v));
      sum += -std::log(s) + s * std::log(r) + (s + 1.0) * std::log(v) + u;
    }
    return sum;
    (void)n;
  }
  void gradient(const double *theta, double *g) const {
    const double s = theta[0];
    const double r = theta[1];
    const double log_r = std::log(r);
    const double n = static_cast<double>(x.size());
    double dnll_ds = 0.0, dnll_dr = 0.0;
    for (auto v : x) {
      const double lrx = log_r + std::log(v);
      const double u = std::exp(-s * lrx);
      // d log f / ds = 1/s - log(r) - log(x) + u * lrx = 1/s - lrx + u*lrx
      // d log f / dr = -s/r + u * s/r
      dnll_ds += -(1.0 / s - lrx + u * lrx);
      dnll_dr += -(-s / r + u * s / r);
    }
    g[0] = dnll_ds;
    g[1] = dnll_dr;
    (void)n;
  }
  void hessian(const double *theta, double *H) const {
    const double s = theta[0];
    const double r = theta[1];
    const double log_r = std::log(r);
    double H_ss = 0.0, H_rr = 0.0, H_sr = 0.0;
    const double n = static_cast<double>(x.size());
    const double s2 = s * s;
    const double r2 = r * r;
    for (auto v : x) {
      const double lrx = log_r + std::log(v);
      const double u = std::exp(-s * lrx);
      H_ss += u * lrx * lrx + 1.0 / s2;
      H_rr += s * (u * (s + 1.0) - 1.0) / r2;
      H_sr += (1.0 + u * s * lrx - u) / r;
    }
    H[0] = H_ss;
    H[1] = H_sr;
    H[2] = H_sr;
    H[3] = H_rr;
    (void)n;
  }
};

inline InvWeibullResult invweibull_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> log_x(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = std::log(x[i]);
    sum_log_x += log_x[i];
  }
  const double dn = static_cast<double>(n);
  const double s = -sum_log_x / dn;
  for (auto &v : log_x) v += s;

  GumbelResult g = fit_gumbel_newton(span{log_x.data(), n}, tol, maxit);
  const double rate_log = -g.mu + s;
  const double rate = std::exp(rate_log);
  const double shape = 1.0 / g.sigma;
  const double logLik =
      dn * (std::log(shape) + shape * (-rate_log + s) - 1.0 + s);
  return {shape, rate, logLik, g.iterations};
}

}  // namespace cppunivariate
