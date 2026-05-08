// Gumbel MLE: 1D Newton on log-scale; mu computed analytically given sigma.
//
// Used directly by Weibull and Inverse Weibull (via log-of-log transforms),
// so the helper fit_gumbel_newton is part of the public cppunivariate API for
// downstream estimators.

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct GumbelResult {
  double mu;
  double sigma;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Gumbel(mu, sigma):
//   log f = -log(sigma) - z - exp(-z),    z = (x-mu)/sigma
// Per-obs Hessian (sympy-simplified, e := exp(-z)):
//   H_mm/obs = e / sigma^2
//   H_ms/obs = (1 + e*(z - 1)) / sigma^2
//   H_ss/obs = (-1 + 2*z*(1 - e) + z^2 * e) / sigma^2
struct GumbelCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    if (!(sigma > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      sum += z + std::exp(-z);
    }
    return n * std::log(sigma) + sum;
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    const double n = static_cast<double>(x.size());
    double dnll_dmu = 0.0, dnll_ds = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      const double e = std::exp(-z);
      // d log f / d mu = (1 - e) / sigma; nll grad = -that
      // d log f / d sigma = -1/sigma + z(1 - e)/sigma; nll grad = -that
      dnll_dmu += -(1.0 - e) / sigma;
      dnll_ds += 1.0 / sigma - z * (1.0 - e) / sigma;
    }
    g[0] = dnll_dmu;
    g[1] = dnll_ds;
    (void)n;
  }
  void hessian(const double *theta, double *H) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    const double s2 = sigma * sigma;
    double H_mm = 0.0, H_ms = 0.0, H_ss = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      const double e = std::exp(-z);
      H_mm += e;
      H_ms += 1.0 + e * (z - 1.0);
      H_ss += -1.0 + 2.0 * z * (1.0 - e) + z * z * e;
    }
    H[0] = H_mm / s2;
    H[1] = H_ms / s2;
    H[2] = H[1];
    H[3] = H_ss / s2;
  }
};

inline GumbelResult fit_gumbel_newton(span x, double tol, int maxit) {
  const std::size_t n = x.size();
  const double dn = static_cast<double>(n);

  double sum_x = 0.0;
  double sum_x2 = 0.0;
  for (auto v : x) {
    sum_x += v;
    sum_x2 += v * v;
  }
  const double xbar = sum_x / dn;
  const double var0 = sum_x2 / dn - xbar * xbar;

  double theta = std::log(std::sqrt(var0) * 2.45 / M_PI);
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double sigma = std::exp(theta);
    const double invs = 1.0 / sigma;
    const double negs = -invs;

    double psi0 = 0.0, psi1 = 0.0, psi2 = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double v = x[i] - xbar;
      const double e = std::exp(v * negs);
      psi0 += e;
      psi1 += v * e;
      psi2 += v * v * e;
    }
    psi1 /= psi0;
    psi2 = psi2 / psi0 - psi1 * psi1;

    const double f = sigma + psi1;
    const double df = 1.0 + psi2 * (invs * invs);
    const double dth = f / (df * sigma);

    theta -= dth;
    if (std::abs(dth) < std::abs(theta) * tol) break;
  }

  const double sigma = std::exp(theta);
  const double invsF = -1.0 / sigma;
  double sum_e = 0.0;
  double sum_z = 0.0;
  double sum_exp_neg_z = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum_e += std::exp((x[i] - xbar) * invsF);
  }
  const double mu = -sigma * std::log(sum_e / dn) + xbar;
  for (std::size_t i = 0; i < n; ++i) {
    const double z = (x[i] - mu) / sigma;
    sum_z += z;
    sum_exp_neg_z += std::exp(-z);
  }
  const double logLik =
      -dn * std::log(sigma) - sum_z - sum_exp_neg_z;
  return {mu, sigma, logLik, iter};
}

}  // namespace cppunivariate
