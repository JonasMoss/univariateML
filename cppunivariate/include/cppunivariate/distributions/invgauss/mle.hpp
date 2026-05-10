// Inverse Gaussian MLE: closed form.
//   mu = mean(x)
//   1/lambda = mean(1/x) - 1/mu

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct InvGaussResult {
  double mu;
  double lambda;
  double logLik;
};

// Negative log-likelihood of InverseGaussian(mu, lambda):
//   nll = sum (1/2) log(2 pi x_i^3) - (n/2) log(lambda)
//       + (lambda / (2 mu^2)) * sum (x_i - mu)^2 / x_i.
// Per-obs Hessian (sympy-derived):
//   H_mm/obs = lambda * (3 x - 2 mu) / mu^4
//   H_ll/obs = 1 / (2 lambda^2)
//   H_ml/obs = (mu - x) / mu^3
struct InvGaussCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double lam = theta[1];
    if (!(mu > 0.0 && lam > 0.0))
      return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum_S = 0.0, sum_log_x = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double d = v - mu;
      sum_S += d * d / v;
      sum_log_x += std::log(v);
    }
    return 0.5 * (3.0 * sum_log_x + n * std::log(2.0 * M_PI)) -
           0.5 * n * std::log(lam) + (lam / (2.0 * mu * mu)) * sum_S;
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double lam = theta[1];
    const double n = static_cast<double>(x.size());
    double sum_S = 0.0, sum_d = 0.0;
    for (auto v : x) {
      const double d = v - mu;
      sum_S += d * d / v;
      sum_d += d;
    }
    // d nll/dmu = -(lam/mu^3) * sum_S + (lam/mu^2) * (-sum_d/.. ). Easier:
    // d log f/dmu = -lam (mu - x)/(mu^3 x) summed → -lam (n*mu - sum) / mu^3?
    // Use sympy result directly: per-obs d log f/dmu = lam*(x-mu)/(mu^2 * x) ?
    // Actually the per-obs H_ml = (mu - x)/mu^3 (negated log f). Per-obs grad
    // of nll w.r.t. mu: derive from log f = -lam (x-mu)^2 / (2 mu^2 x), so
    // d/dmu log f = -lam*(x-mu)*(-1)/(mu^2 x) - lam*(x-mu)^2*(-2)/(2 mu^3 x)
    //             = lam(x-mu)/(mu^2 x) + lam(x-mu)^2/(mu^3 x)
    // d nll / dmu = -[ above ] summed.
    double dnll_dmu = 0.0;
    for (auto v : x) {
      const double d = v - mu;
      dnll_dmu += -(lam * d / (mu * mu * v) + lam * d * d / (mu * mu * mu * v));
    }
    g[0] = dnll_dmu;
    g[1] = -0.5 * n / lam + sum_S / (2.0 * mu * mu);
  }
  void hessian(const double *theta, double *H) const {
    const double mu = theta[0];
    const double lam = theta[1];
    const double n = static_cast<double>(x.size());
    double H_mm = 0.0, H_ml = 0.0;
    for (auto v : x) {
      H_mm += lam * (3.0 * v - 2.0 * mu) / (mu * mu * mu * mu);
      H_ml += (mu - v) / (mu * mu * mu);
    }
    H[0] = H_mm;
    H[1] = H_ml;
    H[2] = H_ml;
    H[3] = 0.5 * n / (lam * lam);
  }
};

inline InvGaussResult invgauss_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN};

  const double dn = static_cast<double>(n);
  double sum = 0.0;
  double sum_inv = 0.0;
  double sum_log = 0.0;
  for (auto v : x) {
    sum += v;
    sum_inv += 1.0 / v;
    sum_log += std::log(v);
  }
  const double mu = sum / dn;
  const double inv_x_bar = sum_inv / dn;
  const double lambda = 1.0 / (inv_x_bar - 1.0 / mu);
  const double L = sum_log / dn;

  double sum_S = 0.0;
  for (auto v : x) {
    const double d = v - mu;
    sum_S += d * d / v;
  }
  const double S = sum_S / dn;
  const double logLik =
      -0.5 * dn *
      (3.0 * L - std::log(lambda) + std::log(2.0 * M_PI) +
       lambda / (mu * mu) * S);
  return {mu, lambda, logLik};
}

}  // namespace cppunivariate
