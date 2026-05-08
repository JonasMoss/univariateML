// Inverse Gamma MLE: y = 1/x, then Gamma MLE on y. Parameters are
// preserved (shape, rate); log-likelihood is rederived in the inverse
// gamma parameterisation.

#pragma once

#include "../core/span.hpp"
#include "../special/special.hpp"
#include "gamma.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct InvGammaResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

// Negative log-likelihood of InvGamma(shape=a, rate=b):
//   log f = a*log(b) - lgamma(a) - (a+1)*log(x) - b/x.
// Hessian closed-form, data-independent (analogous to Gamma):
//   H_aa = n*trigamma(a)
//   H_bb = n*a/b^2
//   H_ab = -n/b
struct InvGammaCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_log_x = 0.0, S_inv_x = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      S_log_x += std::log(v);
      S_inv_x += 1.0 / v;
    }
    return -n * a * std::log(b) + n * cppunivariate::special::lgamma(a) +
           (a + 1.0) * S_log_x + b * S_inv_x;
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_log_x = 0.0, S_inv_x = 0.0;
    for (auto v : x) { S_log_x += std::log(v); S_inv_x += 1.0 / v; }
    g[0] = -n * std::log(b) + n * cppunivariate::special::digamma(a) + S_log_x;
    g[1] = -n * a / b + S_inv_x;
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    H[0] = n * cppunivariate::special::trigamma(a);
    H[1] = -n / b;
    H[2] = -n / b;
    H[3] = n * a / (b * b);
  }
};

inline InvGammaResult invgamma_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> invx(n);
  double sum_log = 0.0;
  double sum_inv = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    sum_log += std::log(x[i]);
    sum_inv += invx[i];
  }
  const double dn = static_cast<double>(n);
  GammaResult g = gamma_mle(span{invx.data(), n}, tol, maxit);

  const double L = sum_log / dn;
  const double M = sum_inv / dn;
  const double logLik = dn * (g.shape * std::log(g.rate) -
                              std::lgamma(g.shape) -
                              (g.shape + 1.0) * L - g.rate * M);
  return {g.shape, g.rate, logLik, g.iterations};
}

}  // namespace cppunivariate
