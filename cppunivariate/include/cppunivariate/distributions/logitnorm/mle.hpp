// Logit-normal MLE: y = logit(x), then Normal MLE on y. Adjusted log-
// likelihood includes the change-of-variable Jacobian -sum(log(x*(1-x))).

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct LogitnormResult {
  double mu;
  double sigma;
  double logLik;
};

// Negative log-likelihood of LogitNormal(mu, sigma):
//   nll = sum log(x*(1-x)) + (n/2) log(2*pi*sigma^2)
//       + sum (logit(x) - mu)^2 / (2 sigma^2).
// The Jacobian sum log(x(1-x)) is constant in (mu, sigma) so the Hessian
// is identical to NormCost evaluated on logit(x). Same shape as Lognormal.
struct LogitnormCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    if (!(sigma > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lj = 0.0, ss = 0.0;
    for (auto v : x) {
      if (!(v > 0.0 && v < 1.0)) return std::numeric_limits<double>::infinity();
      S_lj += std::log(v * (1.0 - v));
      const double d = std::log(v / (1.0 - v)) - mu;
      ss += d * d;
    }
    return S_lj + 0.5 * n * std::log(2.0 * M_PI * sigma * sigma) +
           ss / (2.0 * sigma * sigma);
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    double sum_d = 0.0, sum_d2 = 0.0;
    for (auto v : x) {
      const double d = std::log(v / (1.0 - v)) - mu;
      sum_d += d;
      sum_d2 += d * d;
    }
    const double n = static_cast<double>(x.size());
    const double s2 = sigma * sigma;
    g[0] = -sum_d / s2;
    g[1] = n / sigma - sum_d2 / (s2 * sigma);
  }
  void hessian(const double *theta, double *H) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    double sum_d = 0.0, sum_d2 = 0.0;
    for (auto v : x) {
      const double d = std::log(v / (1.0 - v)) - mu;
      sum_d += d;
      sum_d2 += d * d;
    }
    const double n = static_cast<double>(x.size());
    const double s2 = sigma * sigma;
    const double s3 = s2 * sigma;
    const double s4 = s2 * s2;
    H[0] = n / s2;
    H[1] = 2.0 * sum_d / s3;
    H[2] = H[1];
    H[3] = -n / s2 + 3.0 * sum_d2 / s4;
  }
};

inline LogitnormResult logitnorm_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  double sum_logit = 0.0;
  double sum_log_jac = 0.0;
  for (auto v : x) {
    sum_logit += std::log(v / (1.0 - v));
    sum_log_jac += std::log(v * (1.0 - v));
  }
  const double dn = static_cast<double>(n);
  const double mu = sum_logit / dn;

  double sum_sq_dev = 0.0;
  for (auto v : x) {
    const double d = std::log(v / (1.0 - v)) - mu;
    sum_sq_dev += d * d;
  }
  const double sigma = std::sqrt(sum_sq_dev / dn);
  const double logLik =
      -0.5 * dn * (std::log(2.0 * M_PI * sigma * sigma) + 1.0) - sum_log_jac;
  return {mu, sigma, logLik};
}

}  // namespace cppunivariate
