// Inverse Burr MLE: Burr MLE on 1/x. Log-likelihood rederived in the
// inverse Burr parameterisation.

#pragma once

#include "../core/span.hpp"
#include "burr.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct InvBurrResult {
  double shape1;
  double shape2;
  double logLik;
  int iterations;
};

inline InvBurrResult invburr_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> invx(n);
  double sum_log_invx = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    sum_log_invx += std::log(invx[i]);
  }
  const double dn = static_cast<double>(n);
  const double m1 = sum_log_invx / dn;
  double sq_sum = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double d = std::log(invx[i]) - m1;
    sq_sum += d * d;
  }
  const double m2 = sq_sum / dn;  // population variance, matches arma::var(.,1)

  BurrLmomResult init = burr_lmom_core(m1, m2, true);
  if (!std::isfinite(init.c) || init.c <= 0.0) {
    return {kNaN, kNaN, kNaN, 0};
  }
  BurrMLE res = burr_mle_core(span{invx.data(), n}, init.c, tol, maxit);
  if (res.status != BurrStatus::Ok) {
    return {kNaN, kNaN, kNaN, res.iterations};
  }

  const double shape1 = res.shape1;
  const double shape2 = res.shape2;
  double sum_log_x = 0.0;
  double sum_log1p = 0.0;
  for (auto v : x) {
    sum_log_x += std::log(v);
    sum_log1p += std::log1p(std::pow(v, -shape2));
  }
  const double logLik = dn * (std::log(shape1) + std::log(shape2)) -
                        (shape2 + 1.0) * sum_log_x -
                        (shape1 + 1.0) * sum_log1p;
  return {shape1, shape2, logLik, res.iterations};
}

}  // namespace mlcore
