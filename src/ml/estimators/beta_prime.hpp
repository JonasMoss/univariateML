// Beta Prime MLE: y = x / (1 + x), then Beta MLE on y. Log-likelihood
// recomputed in the beta-prime parameterisation using the original x.

#pragma once

#include "../core/span.hpp"
#include "../special/special.hpp"
#include "beta.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct BetaPrimeResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
};

// Negative log-likelihood of BetaPrime(alpha, beta):
//   log f = (alpha-1)*log(x) - (alpha+beta)*log(1+x)
//         - lgamma(alpha) - lgamma(beta) + lgamma(alpha+beta)
// Hessian (closed form, data-independent — same shape as Beta on the
// transformed variable):
//   H_aa = n*(trigamma(a) - trigamma(a+b))
//   H_bb = n*(trigamma(b) - trigamma(a+b))
//   H_ab = -n*trigamma(a+b)
struct BetaPrimeCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1px = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      S_lx += std::log(v);
      S_l1px += std::log1p(v);
    }
    return -(a - 1.0) * S_lx + (a + b) * S_l1px +
           n * (mlcore::special::lgamma(a) + mlcore::special::lgamma(b) -
                mlcore::special::lgamma(a + b));
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1px = 0.0;
    for (auto v : x) { S_lx += std::log(v); S_l1px += std::log1p(v); }
    const double psi_ab = mlcore::special::digamma(a + b);
    g[0] = -S_lx + S_l1px + n * (mlcore::special::digamma(a) - psi_ab);
    g[1] = S_l1px + n * (mlcore::special::digamma(b) - psi_ab);
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    const double psi1_ab = mlcore::special::trigamma(a + b);
    H[0] = n * (mlcore::special::trigamma(a) - psi1_ab);
    H[1] = -n * psi1_ab;
    H[2] = -n * psi1_ab;
    H[3] = n * (mlcore::special::trigamma(b) - psi1_ab);
  }
};

inline BetaPrimeResult betapr_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> y(n);
  double sum_log_x = 0.0, sum_log1p_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    y[i] = x[i] / (1.0 + x[i]);
    sum_log_x += std::log(x[i]);
    sum_log1p_x += std::log1p(x[i]);
  }
  BetaResult br = beta_mle(span{y.data(), n}, tol, maxit);

  const double dn = static_cast<double>(n);
  const double logLik =
      (br.alpha - 1.0) * sum_log_x - (br.alpha + br.beta) * sum_log1p_x -
      dn * mlcore::special::lbeta(br.alpha, br.beta);
  return {br.alpha, br.beta, logLik, br.iterations};
}

}  // namespace mlcore
