// Inverse Burr MLE: Burr MLE on 1/x. Log-likelihood rederived in the
// inverse Burr parameterisation.

#pragma once

#include "../../core/span.hpp"
#include "../burr/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class InvBurrStatus { Ok = 0, AllLeOne, InvalidInit, IterationFailure };

struct InvBurrResult {
  double shape1;
  double shape2;
  double logLik;
  int iterations;
  InvBurrStatus status;
};

inline InvBurrResult invburr_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, InvBurrStatus::InvalidInit};

  bool all_le_one = true;
  for (auto v : x) {
    if (!(v > 0.0) || !std::isfinite(v) || !std::isfinite(1.0 / v))
      return {kNaN, kNaN, kNaN, 0, InvBurrStatus::InvalidInit};
    if (v > 1.0) all_le_one = false;
  }
  if (all_le_one)
    return {kNaN, kNaN, kNaN, 0, InvBurrStatus::AllLeOne};

  std::vector<double> invx(n);
  double sum_log_invx = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    sum_log_invx += std::log(invx[i]);
  }
  const double c0 = burr_mle_start(span{invx.data(), n}, true);
  BurrMLE res = burr_mle_core(span{invx.data(), n}, c0, tol, maxit);
  if (res.status != BurrStatus::Ok) {
    return {kNaN, kNaN, kNaN, res.iterations, InvBurrStatus::IterationFailure};
  }

  const double shape1 = res.shape1;
  const double shape2 = res.shape2;
  // Change-of-variables Jacobian; reuse the checked Burr likelihood.
  const double logLik = res.logLik + 2.0 * sum_log_invx;
  if (!std::isfinite(logLik))
    return {kNaN, kNaN, kNaN, res.iterations, InvBurrStatus::IterationFailure};
  return {shape1, shape2, logLik, res.iterations, InvBurrStatus::Ok};
}

}  // namespace cppunivariate
