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
