// Gamma MLE: Newton-Raphson on shape via the digamma equation
//   log(shape) - psi(shape) = log(mean(x)) - mean(log x)
// then rate = shape / mean(x). Initialization uses Minka's closed-form
// approximation (with a small empirical scaling that matches the R impl
// 1:1 — preserved verbatim so goldens reproduce).
//
// Convergence is absolute on the Newton step (not relative): the R-side
// implementations of dependent estimators (lgamma, naka, invgamma) were
// written against this rule, so newton_raphson_1d_abs is the right helper.
//
// Robust path. For nearly tied data s = log(mean) - mean(log x) is a
// difference of nearly equal numbers and shape is huge, where
// log(shape) - psi(shape) and the lgamma-based log-likelihood cancel
// catastrophically (shape went negative at data 1e8 + N(0, 1)). When s is
// small, or Newton leaves the domain or does not converge, s is recomputed
// from relative deviations, the root of log(k) - psi(k) = s is bisected in
// log(k) inside (1/(2s), 1/s) (Alzer 1997: 1/(2k) < log k - psi(k) < 1/k),
// with asymptotic series for large k, and the log-likelihood uses the
// Stirling form. Constant data have s = 0: no MLE (point-mass limit).

#pragma once

#include "../../core/newton.hpp"
#include "../../core/reductions.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class GammaStatus { Ok = 0, Degenerate, IterationFailure };

struct GammaResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
  GammaStatus status = GammaStatus::Ok;
};

namespace gamma_detail {

// t - log1p(t) for t = x/c - 1 > -1, accurate near 0; ratio = x/c.
inline double phi(double t, double ratio) {
  if (std::abs(t) < 0.1) {
    double term = t * t, sum = 0.0;
    for (int j = 2; j < 60; ++j) {
      const double add = term / static_cast<double>(j);
      sum += (j % 2 == 0) ? add : -add;
      if (std::abs(add) <= 1e-17 * std::abs(sum)) break;
      term *= t;
    }
    return sum;
  }
  return t - std::log(ratio);
}

// log(k) - psi(k); asymptotic series for large k, where the direct
// difference cancels.
inline double log_minus_digamma(double k) {
  if (k >= 100.0) {
    const double r = 1.0 / k, r2 = r * r;
    return r * (0.5 + r * (1.0 / 12.0 - r2 * (1.0 / 120.0 - r2 * (1.0 / 252.0 - r2 / 240.0))));
  }
  return std::log(k) - cppunivariate::special::digamma(k);
}

// lgamma(k) - ((k - 1/2) log k - k + log(2 pi)/2) for k >= 100.
inline double stirlerr(double k) {
  const double r = 1.0 / k, r2 = r * r;
  return r * (1.0 / 12.0 - r2 * (1.0 / 360.0 - r2 * (1.0 / 1260.0 - r2 / 1680.0)));
}

// s = log(mean) - mean(log x) from deviations relative to c = mean.
inline double robust_s(span x, double mean_hat) {
  const double dn = static_cast<double>(x.size());
  double sum_d = 0.0, sum_phi = 0.0;
  for (auto v : x) {
    const double ratio = v / mean_hat;
    const double t = (v - mean_hat) / mean_hat;
    sum_d += t;
    sum_phi += phi(t, ratio);
  }
  const double m_d = sum_d / dn;
  return sum_phi / dn - phi(m_d, 1.0 + m_d);
}

}  // namespace gamma_detail

inline GammaResult gamma_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  const double dn = static_cast<double>(n);
  double mean_hat = cppunivariate::mean(x);
  if (!std::isfinite(mean_hat)) {
    long double total = 0.0L;
    for (auto v : x) total += v;
    mean_hat = static_cast<double>(total / dn);
  }
  const double lx_bar = cppunivariate::mean_log(x);
  double s = std::log(mean_hat) - lx_bar;

  bool all_equal = true;
  for (auto v : x) all_equal = all_equal && v == x[0];
  if (all_equal) return {kNaN, kNaN, kNaN, 0, GammaStatus::Degenerate};

  int iterations = 0;
  if (s >= 1e-3 && std::isfinite(s)) {
    auto f_over_df = [s](double shape) {
      return (std::log(shape) - cppunivariate::special::digamma(shape) - s) /
             (1.0 / shape - cppunivariate::special::trigamma(shape));
    };
    const double init = 1.0 / (12.0 * s) *
                            (3.0 - s + std::sqrt((s - 3.0) * (s - 3.0) + 24.0 * s)) *
                            0.9989 +
                        0.0102;
    auto nr = cppunivariate::newton_raphson_1d_abs(init, f_over_df, tol, maxit);
    if (nr.converged && nr.x > 0.0 && std::isfinite(nr.x)) {
      const double rate = nr.x / mean_hat;
      const double logLik = dn * (nr.x * std::log(rate) - std::lgamma(nr.x) +
                                  (nr.x - 1.0) * lx_bar - rate * mean_hat);
      return {nr.x, rate, logLik, nr.iterations};
    }
    iterations = nr.iterations;
  } else {
    s = gamma_detail::robust_s(x, mean_hat);
  }
  if (!(s > 0.0) || !std::isfinite(s)) {
    return {kNaN, kNaN, kNaN, iterations, GammaStatus::IterationFailure};
  }

  double lo = 0.25 / s, hi = 2.0 / s;
  for (int i = 0; i < 200 && hi / lo - 1.0 > 4.0 * std::numeric_limits<double>::epsilon(); ++i) {
    const double mid = std::sqrt(lo) * std::sqrt(hi);
    if (gamma_detail::log_minus_digamma(mid) > s) lo = mid; else hi = mid;
    ++iterations;
  }
  const double shape = std::sqrt(lo) * std::sqrt(hi);
  const double rate = shape / mean_hat;
  // Per observation: k log k - k - lgamma(k) - k s - mean(log x).
  const double per_obs =
      shape >= 100.0
          ? 0.5 * std::log(shape) - 0.5 * std::log(2.0 * M_PI) -
                gamma_detail::stirlerr(shape) - shape * s - lx_bar
          : shape * std::log(shape) - shape - std::lgamma(shape) - shape * s - lx_bar;
  const double logLik = dn * per_obs;
  if (!std::isfinite(shape) || !std::isfinite(rate) || !std::isfinite(logLik)) {
    return {kNaN, kNaN, kNaN, iterations, GammaStatus::IterationFailure};
  }
  return {shape, rate, logLik, iterations};
}

}  // namespace cppunivariate
