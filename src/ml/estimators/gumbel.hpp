// Gumbel MLE: 1D Newton on log-scale; mu computed analytically given sigma.
//
// Used directly by Weibull and Inverse Weibull (via log-of-log transforms),
// so the helper fit_gumbel_newton is part of the public mlcore API for
// downstream estimators.

#pragma once

#include "../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

struct GumbelResult {
  double mu;
  double sigma;
  double logLik;
  int iterations;
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

}  // namespace mlcore
