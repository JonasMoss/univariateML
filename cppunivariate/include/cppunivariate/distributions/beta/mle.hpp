// Beta MLE: Newton-Raphson on log(beta), with alpha solved via a small
// digamma-inversion inner loop each iteration.
//
// Convergence is relative on log(beta): |db/b| < tol. Bit-identical to
// the prior arma'd implementation.

#pragma once

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct BetaResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
};

inline BetaResult beta_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0)
    return {0.0, 0.0, kNaN, 0};

  const double dn = static_cast<double>(n);
  double sum_log_x = 0.0, sum_log_1mx = 0.0, sum_x = 0.0, sum_xsq = 0.0;
  for (auto v : x) {
    sum_log_x += std::log(v);
    sum_log_1mx += std::log(1.0 - v);
    sum_x += v;
    sum_xsq += v * v;
  }
  const double s = sum_log_x / dn;
  const double r = sum_log_1mx / dn;
  const double m = sum_x / dn;
  const double v_var = sum_xsq / dn - m * m;

  const double nu = m * (1.0 - m) / v_var - 1.0;
  double b = std::log((1.0 - m) * nu);
  double alpha = kNaN;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double exp_b = std::exp(b);
    const double digamma_b = cppunivariate::special::digamma(exp_b);
    const double trigamma_b = cppunivariate::special::trigamma(exp_b);
    const double y = s + digamma_b - r;

    alpha = 1.0 / std::log1p(std::exp(-y));
    for (int k = 0; k < 3; ++k) {
      const double ra = cppunivariate::special::digamma(alpha) - y;
      alpha -= ra / cppunivariate::special::trigamma(alpha);
    }

    const double f =
        exp_b * (digamma_b - r - cppunivariate::special::digamma(alpha + exp_b));
    const double df =
        f + exp_b * exp_b *
                (trigamma_b -
                 cppunivariate::special::trigamma(alpha + exp_b) *
                     (trigamma_b / cppunivariate::special::trigamma(alpha) + 1.0));
    const double b0 = b - f / df;
    if (std::abs((b0 - b) / b0) < tol) {
      b = b0;
      break;
    }
    b = b0;
  }

  const double beta = std::exp(b);
  const double logLik =
      dn * ((alpha - 1.0) * s + (beta - 1.0) * r - std::lgamma(alpha) -
            std::lgamma(beta) + std::lgamma(alpha + beta));
  return {alpha, beta, logLik, iter};
}

} // namespace cppunivariate
