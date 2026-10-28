// Lomax MLE. The parameterisation, the existence condition
// (mean(x^2) < 2*mean(x)^2, surfaced as LomaxStatus::MeanVarianceCondition)
// and the Lomax -> exponential limit are in README.md; this file documents
// only the implementation.

#pragma once

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

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

// Negative log-likelihood of Lomax(lambda, kappa):
//   log f = log(lambda) + log(kappa) - (kappa+1)*log(1 + lambda*x)
// Per-obs Hessian (sympy):
//   H_ll/obs = ((1+lx)^2 - lambda^2 x^2 (kappa+1)) / (lambda^2 (1+lx)^2)
//             where lx = lambda*x
//   H_kk/obs = 1 / kappa^2  (data-independent diagonal)
//   H_lk/obs = x / (1 + lambda*x)
struct LomaxCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double lam = theta[0];
    const double kap = theta[1];
    if (!(lam > 0.0 && kap > 0.0))
      return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum_log_1px = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      sum_log_1px += std::log1p(lam * v);
    }
    return -n * (std::log(lam) + std::log(kap)) + (kap + 1.0) * sum_log_1px;
  }
  void gradient(const double *theta, double *g) const {
    const double lam = theta[0];
    const double kap = theta[1];
    const double n = static_cast<double>(x.size());
    double sum_log_1px = 0.0, sum_x_over_1px = 0.0;
    for (auto v : x) {
      sum_log_1px += std::log1p(lam * v);
      sum_x_over_1px += v / (1.0 + lam * v);
    }
    g[0] = -n / lam + (kap + 1.0) * sum_x_over_1px;
    g[1] = -n / kap + sum_log_1px;
  }
  void hessian(const double *theta, double *H) const {
    const double lam = theta[0];
    const double kap = theta[1];
    const double n = static_cast<double>(x.size());
    double H_ll = 0.0, H_lk = 0.0;
    for (auto v : x) {
      const double lx = lam * v;
      const double one_plus = 1.0 + lx;
      H_ll +=
          ((one_plus * one_plus) - lam * lam * v * v * (kap + 1.0)) /
          (lam * lam * one_plus * one_plus);
      H_lk += v / one_plus;
    }
    H[0] = H_ll;
    H[1] = H_lk;
    H[2] = H_lk;
    H[3] = n / (kap * kap);
  }
};

namespace lomax_detail {
// Profile log-likelihood in lambda (kappa at its closed-form maximiser),
// up to a constant: n log(lambda) - n log(S) - S, S = sum log1p(lambda x).
inline double profile_loglik(span x, double lambda) {
  double S = 0.0;
  for (auto v : x) S += std::log1p(lambda * v);
  const double n = static_cast<double>(x.size());
  return n * std::log(lambda) - n * std::log(S) - S;
}
}  // namespace lomax_detail

// lambda0: optional start for lambda; NaN (the default) or a non-positive
// value means the method-of-moments start.
inline LomaxResult lomax_mle(
    span x, double tol, int maxit,
    double lambda0 = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, LomaxStatus::InvalidStart};

  const double m = cppunivariate::mean(x);
  double sum_xsq = 0.0;
  for (auto v : x) sum_xsq += v * v;
  const double s = sum_xsq / static_cast<double>(n);
  const double m2 = m * m;
  if (s < 2.0 * m2) {
    return {kNaN, kNaN, kNaN, 0, LomaxStatus::MeanVarianceCondition};
  }

  const double alpha0 = 2.0 * (s - m2) / (s - 2.0 * m2);
  double lambda = (std::isfinite(lambda0) && lambda0 > 0.0)
                      ? lambda0
                      : 1.0 / (m * (alpha0 - 1.0));
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

    // f is the profile score (per observation) and df its derivative.
    // Newton is an ascent step only where the profile is concave (df < 0);
    // elsewhere (possible from a user-supplied lambda0 far from the MLE)
    // move geometrically uphill. Every step is halved until the profile
    // log-likelihood does not decrease, and convergence is only declared
    // on a full Newton step, so a shrunken step cannot fake convergence.
    // From the moment start the plain Newton step is normally accepted.
    const bool newton = df < 0.0;
    double step = newton ? f / df : (f > 0.0 ? -lambda : 0.5 * lambda);
    if (!std::isfinite(step)) {
      return {kNaN, kNaN, kNaN, iter, LomaxStatus::IterationFailure};
    }
    if (lambda - step <= 0.0) step = 0.5 * lambda;
    const double lp_old = lomax_detail::profile_loglik(x, lambda);
    int halvings = 0;
    for (; halvings < 60; ++halvings) {
      const double lp_new = lomax_detail::profile_loglik(x, lambda - step);
      if (lp_new >= lp_old - 1e-12 * (std::abs(lp_old) + 1.0)) break;
      step *= 0.5;
    }
    const double lambda_new = lambda - step;
    if (newton && halvings == 0 &&
        std::abs(step) < (std::abs(lambda) * tol + tol)) {
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

}  // namespace cppunivariate
