// Gompertz MLE.
//
// Up-front overdispersion check: mean(x) < sd(x) ⇒ data is consistent
// with an exponential and the Gompertz b MLE doesn't exist. Otherwise
// 1D Newton on b with hybrid convergence; a is the closed-form
// maximiser given b.

#pragma once

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class GompertzStatus { Ok = 0, Overdispersed, IterationFailure };

// Negative log-likelihood of Gompertz(a, b) [actuar: shape=a, rate=b]:
//   log f = log(a) + b*x - (a/b) * (exp(b*x) - 1)
// Per-obs Hessian (sympy):
//   H_aa/obs = 1/a^2                                   (data-indep diagonal)
//   H_bb/obs = a*(b^2 x^2 e - 2 b x e + 2 e - 2) / b^3,  e = exp(b*x)
//   H_ab/obs = (b*x*e - e + 1) / b^2
struct GompertzResult {
  double a;
  double b;
  double logLik;
  int iterations;
  GompertzStatus status;
};

inline GompertzResult gompertz_mle_impl(span x, double tol, int maxit, bool standardized) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, GompertzStatus::IterationFailure};

  long double total = 0.0L;
  for (auto v : x) total += v;
  const double unit = static_cast<double>(total / static_cast<long double>(n));
  if (unit > 0.0 && std::isfinite(unit) && !standardized) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = x[i] / unit;
    const auto r = gompertz_mle_impl(span{z.data(), n}, tol, maxit, true);
    return {r.a/unit, r.b/unit, r.logLik-static_cast<double>(n)*std::log(unit), r.iterations, r.status};
  }

  const double dn = static_cast<double>(n);
  double sum_x = 0.0, sum_xsq = 0.0;
  for (auto v : x) {
    sum_x += v;
    sum_xsq += v * v;
  }
  const double mean_x = sum_x / dn;
  // Profile likelihood is linear minus log of a moment generating
  // function: strictly concave. Its right derivative at zero is
  // n*(mean - mean(x^2)/(2*mean)), so equality also selects exponential.
  if (sum_xsq / dn >= 2.0 * mean_x * mean_x) {
    return {kNaN, kNaN, kNaN, 0, GompertzStatus::Overdispersed};
  }

  auto f_over_df = [&](double b) {
    double sum_bx = 0.0, sum_bx_x = 0.0, sum_bx_x2 = 0.0;
    for (auto v : x) {
      const double e = std::exp(b * v);
      sum_bx += e;
      sum_bx_x += v * e;
      sum_bx_x2 += v * v * e;
    }
    const double s = sum_bx - dn;
    const double r = b * sum_bx_x - s;
    const double a_local = b * dn / s;
    const double da = -dn * r / (s * s);
    const double d2a =
        dn / (s * s) *
        (b * (2.0 * sum_bx_x * sum_bx_x / s - sum_bx_x2) - 2.0 * sum_bx_x);
    const double f = dn * da / a_local + sum_x;
    const double df = dn * (a_local * d2a - da * da) / (a_local * a_local);
    return f / df;
  };

  double b = 1e-1;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double step = f_over_df(b);
    b -= step;
    if (b <= 0.0 || !std::isfinite(b)) {
      return {kNaN, kNaN, kNaN, iter, GompertzStatus::IterationFailure};
    }
    if (std::abs(step) < (std::abs(b) * tol + tol)) break;
  }

  double sum_exp_bx = 0.0;
  for (auto v : x) sum_exp_bx += std::exp(b * v);
  const double a = b * dn / (sum_exp_bx - dn);
  const double logLik =
      dn * std::log(a) + b * sum_x - (a / b) * (sum_exp_bx - dn);
  return {a, b, logLik, iter, GompertzStatus::Ok};
}

inline GompertzResult gompertz_mle(span x, double tol, int maxit) {
  return gompertz_mle_impl(x, tol, maxit, false);
}

}  // namespace cppunivariate
