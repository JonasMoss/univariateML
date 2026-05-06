// Lomax MLE.
//
// Initialised from a method-of-moments lambda; refined by 1D Newton
// with hybrid (relative + absolute) convergence kept verbatim from the
// original implementation. kappa is the closed-form maximiser given
// lambda.
//
// Status enum surfaces three special outcomes:
//   - MeanVarianceCondition: mean(x^2) < 2 * mean(x)^2 — the Lomax
//     MLE doesn't exist; data is consistent with an exponential.
//   - InvalidStart: lambda init was non-positive or non-finite.
//   - IterationFailure: Newton stepped to non-positive lambda.
// (Lomax → Exponential degeneracy will get a docs note in WS6.)

#pragma once

#include "../core/reductions.hpp"
#include "../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

enum class LomaxStatus {
  Ok = 0,
  MeanVarianceCondition,
  InvalidStart,
  IterationFailure
};

struct LomaxResult {
  double lambda;
  double kappa;
  double logLik;
  int iterations;
  LomaxStatus status;
};

inline LomaxResult lomax_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, LomaxStatus::InvalidStart};

  const double m = mlcore::mean(x);
  double sum_xsq = 0.0;
  for (auto v : x) sum_xsq += v * v;
  const double s = sum_xsq / static_cast<double>(n);
  const double m2 = m * m;
  if (s < 2.0 * m2) {
    return {kNaN, kNaN, kNaN, 0, LomaxStatus::MeanVarianceCondition};
  }

  const double alpha0 = 2.0 * (s - m2) / (s - 2.0 * m2);
  double lambda = 1.0 / (m * (alpha0 - 1.0));
  if (lambda <= 0.0 || !std::isfinite(lambda)) {
    return {kNaN, kNaN, kNaN, 0, LomaxStatus::InvalidStart};
  }

  int iter = 1;
  for (; iter <= maxit; ++iter) {
    double S = 0.0, S1 = 0.0, S2 = 0.0;
    for (auto v : x) {
      const double xb = 1.0 + lambda * v;
      S += std::log(xb);
      const double xd = v / xb;
      S1 += xd;
      S2 += xd * xd;
    }
    const double dn = static_cast<double>(n);
    S /= dn;
    S1 /= dn;
    S2 /= dn;

    const double psi = S1 / S;
    const double f = 1.0 / lambda - psi - S1;
    const double df = psi * psi + S2 / S + S2 - 1.0 / (lambda * lambda);

    const double step = f / df;
    const double lambda_new = lambda - step;
    if (lambda_new <= 0.0 || !std::isfinite(lambda_new)) {
      return {kNaN, kNaN, kNaN, iter, LomaxStatus::IterationFailure};
    }
    if (std::abs(step) < (std::abs(lambda) * tol + tol)) {
      lambda = lambda_new;
      break;
    }
    lambda = lambda_new;
  }

  double sum_log1p = 0.0;
  for (auto v : x) sum_log1p += std::log1p(lambda * v);
  const double dn = static_cast<double>(n);
  const double kappa = dn / sum_log1p;
  const double logLik =
      dn * (std::log(kappa) + std::log(lambda) - 1.0 - 1.0 / kappa);
  return {lambda, kappa, logLik, iter, LomaxStatus::Ok};
}

}  // namespace mlcore
