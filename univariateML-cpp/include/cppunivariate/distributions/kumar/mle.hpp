// Kumaraswamy MLE: two-stage. Stage 1 uses an L-moment-based Newton on
// `a` to seed; stage 2 refines `a` by Newton on the score. `b` is then
// the closed-form maximiser. Convergence is hybrid (relative + absolute):
//   |step| < |a| * tol + tol
// kept verbatim for golden parity.

#pragma once

#include "../../core/inverse_digamma.hpp"
#include "../../core/reductions.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class KumarStatus { Ok, InvalidData, Unbounded, IterationFailure };

struct KumarResult {
  double a;
  double b;
  double logLik;
  int iterations;
  KumarStatus status = KumarStatus::Ok;
};

// Stable profile fallback: b = -n / sum log(1-x^a). Solve the
// profile score in log(a), preserving the legacy Newton path when it works.
inline KumarResult kumar_profile_mle(span x, double tol, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double n = static_cast<double>(x.size());
  const auto evaluate = [&](double p, double &b, double &ll) {
    const double a = std::exp(p);
    double S = 0.0, L = 0.0, D = 0.0;
    for (double v : x) {
      const double l = std::log(v), t = -a * l;
      S += std::log(-std::expm1(-t));
      L += l;
      D += t > 700.0 ? 0.0 : -t / std::expm1(t);
    }
    b = -n / S;
    ll = n * (p + std::log(b)) + (a - 1.0) * L + (b - 1.0) * S;
    return 1.0 + a * L / n - (b - 1.0) * D / n;
  };
  double b, ll;
  double lo = 0.0, hi = 0.0;
  for (int i = 0; i < 700 && evaluate(lo, b, ll) <= 0.0; ++i) lo -= 1.0;
  for (int i = 0; i < 700 && evaluate(hi, b, ll) >= 0.0; ++i) hi += 1.0;
  if (!(evaluate(lo, b, ll) > 0.0 && evaluate(hi, b, ll) < 0.0))
    return {nan, nan, nan, 0, KumarStatus::IterationFailure};
  for (int i = 1; i <= maxit; ++i) {
    const double p = (lo + hi) / 2.0;
    const double score = evaluate(p, b, ll);
    // Stop on a zero score or when the bracket is at rounding level; for
    // large n the summed score carries rounding noise above 1e-12.
    const bool narrow = hi - lo <= 1e-13 * (1.0 + std::abs(p));
    if ((std::abs(score) < std::min(tol, 1e-12) || narrow) && std::isfinite(ll))
      return {std::exp(p), b, ll, i};
    if (score > 0.0) lo = p; else hi = p;
  }
  return {nan, nan, nan, maxit, KumarStatus::IterationFailure};
}

inline KumarResult kumar_mle(span x, double tol, int maxit,
                             double start = std::numeric_limits<double>::quiet_NaN(),
                             double fixed_a = std::numeric_limits<double>::quiet_NaN(),
                             double fixed_b = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN, 0, KumarStatus::InvalidData};

  bool constant = true;
  for (double v : x) constant = constant && v == x[0];
  bool extreme = false;
  for (double v : x) {
    if (!(v > 0.0 && v < 1.0)) return {kNaN, kNaN, kNaN, 0, KumarStatus::InvalidData};
    extreme = extreme || v < 1e-8 || v > 1.0 - 1e-8;
  }
  if (!std::isnan(fixed_a) || !std::isnan(fixed_b)) {
    const bool fa = !std::isnan(fixed_a), fb = !std::isnan(fixed_b);
    if ((fa && (!(fixed_a > 0) || !std::isfinite(fixed_a))) ||
        (fb && (!(fixed_b > 0) || !std::isfinite(fixed_b))))
      return {kNaN,kNaN,kNaN,0,KumarStatus::InvalidData};
    const double dn = static_cast<double>(n);
    double L = 0;
    for (double v : x) L += std::log(v);
    double a = fixed_a, b = fixed_b;
    int it = 0;
    if (!fa) {
      if (maxit < 1) return {kNaN,kNaN,kNaN,0,KumarStatus::IterationFailure};
      // Score multiplied by a. Each t/expm1(t) decreases from 1 to 0;
      // the original score has negative derivative also when b < 1,
      // since t^2 exp(t)/expm1(t)^2 <= 1.
      const auto score = [&](double z) {
        double d = 0;
        for (double v : x) {
          const double t = -z * std::log(v);
          d += t > 700 ? 0 : t / std::expm1(t);
        }
        return dn + z * L + (b - 1) * d;
      };
      double lo = 0, hi = 1;
      while (score(hi) > 0 && hi < 1e300) hi *= 2;
      if (!(score(hi) <= 0))
        return {kNaN,kNaN,kNaN,0,KumarStatus::IterationFailure};
      for (; it < maxit; ++it) {
        a = lo + (hi - lo) / 2;
        if (score(a) > 0) lo = a; else hi = a;
        if (hi - lo <= 1e-13 * a) break;
      }
      if (it == maxit) return {kNaN,kNaN,kNaN,it,KumarStatus::IterationFailure};
    }
    double S = 0;
    for (double v : x) {
      const double q = a * std::log(v);
      S += q < -0.6931471805599453 ? std::log1p(-std::exp(q)) :
                                    std::log(-std::expm1(q));
    }
    if (!fb) b = -dn / S;
    const double ll = dn * (std::log(a) + std::log(b)) + (a-1)*L + (b-1)*S;
    if (!std::isfinite(ll) || !(b > 0) || !std::isfinite(b))
      return {kNaN,kNaN,kNaN,it,KumarStatus::IterationFailure};
    return {a,b,ll,it};
  }
  if (constant) return {kNaN, kNaN, kNaN, 0, KumarStatus::Unbounded};
  if (!std::isnan(start) && (!(start > 0.0) || !std::isfinite(start)))
    return {kNaN, kNaN, kNaN, 0, KumarStatus::InvalidData};
  if (extreme) return kumar_profile_mle(x, tol, maxit);
  const double mu = cppunivariate::mean_log(x);
  // sigma2 = mean(log(x)^2) - mu^2
  double sum_log_sq = 0.0;
  for (auto v : x) {
    const double l = std::log(v);
    sum_log_sq += l * l;
  }
  const double sigma2 = sum_log_sq / static_cast<double>(n) - mu * mu;

  if (!(sigma2 > 0.0) || !std::isfinite(sigma2))
    return kumar_profile_mle(x, tol, maxit);

  // Stage 1: Newton on `a` from L-moment estimating equation.
  auto f_over_df_lmom = [&](double a0) {
    const double b = inverse_digamma_scalar(-0.5772156649015328606 - a0 * mu);
    if (!(b > 0.0) || !std::isfinite(b)) return kNaN;
    const double tri = cppunivariate::special::trigamma(b);
    const double tetra = cppunivariate::special::tetragamma(b);
    const double basel = M_PI * M_PI / 6.0;
    const double f = a0 * tri * (basel - tri - a0 * a0 * sigma2);
    const double df = a0 * tetra * mu - 2.0 * tri * (basel + tri);
    return f / df;
  };
  double a0 = std::sqrt((M_PI * M_PI / 6.0) / sigma2);
  if (std::isnan(start)) {
    for (int i = 0; i < maxit; ++i) {
      if (!(a0 > 0.0) || !std::isfinite(a0) || !std::isfinite(a0 * mu))
        return kumar_profile_mle(x, tol, maxit);
      const double step = f_over_df_lmom(a0);
      a0 -= step;
      if (std::abs(step) < (std::abs(a0) * tol + tol)) break;
    }
    if (a0 <= 0.0) a0 = std::sqrt((M_PI * M_PI / 6.0) / sigma2);

  } else {
    if (!(start > 0.0) || !std::isfinite(start))
      return {kNaN, kNaN, kNaN, 0, KumarStatus::InvalidData};
    a0 = start;
  }

  // Stage 2: full ML Newton on `a`.
  auto f_over_df = [&](double a) {
    double sum_log1p_neg_xa = 0.0;
    double sum_xa_z = 0.0;
    double sum_xa_z2 = 0.0;
    for (auto v : x) {
      const double log_v = std::log(v);
      const double xa = std::exp(a * log_v);
      const double z = log_v / (1.0 - xa);
      sum_log1p_neg_xa += std::log1p(-xa);
      sum_xa_z += xa * z;
      sum_xa_z2 += xa * z * z;
    }
    const double dn = static_cast<double>(n);
    const double alpha = sum_log1p_neg_xa / dn;
    const double beta_v = sum_xa_z / dn;
    const double gamma_v = sum_xa_z2 / dn;

    const double f = 1.0 / a + mu + (1.0 / alpha + 1.0) * beta_v;
    const double df = -1.0 / (a * a) +
                      (gamma_v * alpha + beta_v * beta_v) / (alpha * alpha) +
                      gamma_v;
    return f / df;
  };

  double a = a0;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double step = f_over_df(a);
    a -= step;
    if (a <= 0.0 || !std::isfinite(a)) {
      return kumar_profile_mle(x, tol, maxit);
    }
    if (std::abs(step) < (std::abs(a) * tol + tol)) break;
  }

  if (iter > maxit) return kumar_profile_mle(x, tol, maxit);

  double sum_log1p_neg_xa = 0.0, sum_log_x = 0.0;
  for (auto v : x) {
    sum_log1p_neg_xa += std::log1p(-std::pow(v, a));
    sum_log_x += std::log(v);
  }
  const double dn = static_cast<double>(n);
  const double b = -dn / sum_log1p_neg_xa;
  const double logLik = dn * (std::log(a) + std::log(b)) +
                        (a - 1.0) * sum_log_x +
                        (b - 1.0) * sum_log1p_neg_xa;
  return {a, b, logLik, iter};
}

}  // namespace cppunivariate
