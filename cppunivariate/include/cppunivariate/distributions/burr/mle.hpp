// Burr type-XII MLE.
//
// Two-stage: log-moment root-finding (or profile search) seeds `c`, then Newton on
// log(c) with the score's analytical derivative refines. After convergence,
// shape `k` is the closed-form maximiser.
//
// The internal helpers (logit/logistic/logistic_pdf, three invert_*
// solvers for the L-moment equation, burr_lmom_core, burr_mle_core) are
// kept in this header rather than core/ because Burr is the only consumer.
// If a second user emerges later, lift them out then.
//
// Status enum: AllGeOne fires when every observation is >= 1, in which
// case the Burr MLE doesn't exist (caller should fall back to Pareto).

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct RootResult {
  double k;
  int iterations;
};

struct BurrLmomResult {
  double k;
  double c;
  int iterations;
};

enum class BurrStatus { Ok = 0, AllGeOne, InvalidInit, IterationFailure };

struct BurrMLE {
  double shape1;
  double shape2;
  double logLik;
  int iterations;
  BurrStatus status;
};

// Preserve ordinary-range arithmetic and avoid overflow in the tails.
inline double burr_softplus(double z) {
  return z > 350.0 ? z + std::log1p(std::exp(-z))
                   : std::log1p(std::exp(z));
}

namespace detail {

inline double logit(double p) { return std::log(p / (1.0 - p)); }

inline double logistic(double x) {
  const double e = std::exp(-x);
  return 1.0 / (1.0 + e);
}

inline double logistic_pdf(double x) {
  const double L = logistic(x);
  return L * (1.0 - L);
}

inline RootResult invert_lower_logit(double cv, double tol, int maxit) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  const double loc = -0.5495872;
  const double scl = 0.6103216;
  const double cv_lo = 0.1;
  const double cv_hi = 0.95;
  const double eps = 1e-15;

  double k0;
  if (cv < cv_lo) {
    k0 = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k0 = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    const double y = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    const double xlog = loc + scl * logit(y);
    k0 = logistic((xlog - loc) / scl);
  }
  k0 = std::min(1.0 - eps, std::max(eps, k0));

  double x = logit(k0);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double k = logistic(x);
    if (!std::isfinite(k) || k < 1e-100)
      return {std::numeric_limits<double>::quiet_NaN(), iter + 1};
    const double di = cppunivariate::special::digamma(k);
    const double tri = cppunivariate::special::trigamma(k);
    const double tetra = cppunivariate::special::tetragamma(k);
    const double pd = di - NEULER_CST;
    const double bpt = BASEL_CST + tri;
    const double f = (pd * pd) / bpt - cv;
    const double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    const double df = fpk * logistic_pdf(x);

    const double step = f / df;
    const double x1 = x - step;
    if (std::abs(x1 - x) < (std::abs(x) * tol + 1e-3 * tol)) {
      x = x1;
      break;
    }
    x = x1;
  }
  return {logistic(x), iter + 1};
}

inline RootResult invert_lower_log(double cv, double tol, int maxit) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  const double loc = -0.5495872;
  const double scl = 0.6103216;
  const double cv_lo = 0.1;
  const double cv_hi = 0.95;
  const double eps = 1e-15;

  double k0;
  if (cv < cv_lo) {
    k0 = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k0 = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    const double y = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    const double xlog = loc + scl * logit(y);
    k0 = logistic((xlog - loc) / scl);
  }
  k0 = std::max(eps, std::min(1.0 - eps, k0));

  double p = std::log(k0);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double k = std::exp(p);
    if (!std::isfinite(k) || k < 1e-100)
      return {std::numeric_limits<double>::quiet_NaN(), iter + 1};
    const double di = cppunivariate::special::digamma(k);
    const double tri = cppunivariate::special::trigamma(k);
    const double tetra = cppunivariate::special::tetragamma(k);
    const double pd = di - NEULER_CST;
    const double bpt = BASEL_CST + tri;
    const double f = (pd * pd) / bpt - cv;
    const double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    const double dfp = fpk * k;

    const double step = f / dfp;
    const double p1 = p - step;
    if (std::abs(p1 - p) < (std::abs(p) * tol + 1e-3 * tol)) {
      p = p1;
      break;
    }
    p = p1;
  }
  return {std::exp(p), iter + 1};
}

inline RootResult invert_upper(double cv, double tol, int maxit) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  const double thr = 0.2;
  const double eps = 1e-9;

  double k0;
  if (cv < thr) {
    k0 = 1.0 + std::sqrt(2.0 * cv / BASEL_CST);
  } else {
    k0 = std::exp(std::sqrt(cv * BASEL_CST));
  }
  k0 = std::max(1.0 + eps, k0);

  double x = std::log(k0 - 1.0);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double kk = std::exp(x) + 1.0;
    if (!std::isfinite(kk) || kk < 1e-100)
      return {std::numeric_limits<double>::quiet_NaN(), iter + 1};
    const double di = cppunivariate::special::digamma(kk);
    const double tri = cppunivariate::special::trigamma(kk);
    const double tetra = cppunivariate::special::tetragamma(kk);
    const double pd = di - NEULER_CST;
    const double bpt = BASEL_CST + tri;
    const double f = (pd * pd) / bpt - cv;
    const double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    const double df = fpk * std::exp(x);

    const double step = f / df;
    const double x1 = x - step;
    if (std::abs(x1 - x) < (std::abs(x) * tol + 1e-3 * tol)) {
      x = x1;
      break;
    }
    x = x1;
  }
  return {std::exp(x) + 1.0, iter + 1};
}

}  // namespace detail

inline BurrLmomResult burr_lmom_core(double mu, double sigma2,
                                     bool use_logit_lower) {
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  constexpr double kEps = std::numeric_limits<double>::epsilon();
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();

  double k_hat = kNaN;
  double c_hat = kNaN;
  int iter = 0;

  // Match the original tol/maxit defaults from kTolDefault/kMaxIterDefault.
  constexpr double tol_default = 1.22e-4;
  constexpr int maxit_default = 100;

  if (sigma2 > 0.0 && std::abs(mu) >= kEps) {
    const double cv = (mu * mu) / sigma2;
    // For k in (0,1), the moment ratio ranges strictly between 0 and 1.
    // Outside that range Newton has no root and can underflow k to zero.
    if (!std::isfinite(cv) || (mu > 0.0 && cv >= 1.0))
      return {kNaN, kNaN, 0};
    RootResult root = (mu > 0.0)
        ? (use_logit_lower
               ? detail::invert_lower_logit(cv, tol_default, maxit_default)
               : detail::invert_lower_log(cv, tol_default, maxit_default))
        : detail::invert_upper(cv, tol_default, maxit_default);
    k_hat = root.k;
    iter = root.iterations;

    if (!std::isfinite(k_hat) || k_hat < 1e-100)
      return {kNaN, kNaN, iter};
    const double t1 = cppunivariate::special::trigamma(k_hat);
    const double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0.0) c_hat = std::sqrt(csq);
  } else if (std::abs(mu) < kEps) {
    k_hat = 1.0;
    const double t1 = cppunivariate::special::trigamma(1.0);
    const double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0.0) c_hat = std::sqrt(csq);
  }
  return {k_hat, c_hat, iter};
}

// Shared start: retain the legacy moment seed whenever it is feasible.
// inverse_variance preserves the inverse wrapper's centered variance sum.
inline double burr_mle_start(span x, bool inverse_variance = false) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (x.size() == 0) return nan;
  std::vector<double> logs;
  double sum = 0.0, squares = 0.0, scale = 0.0;
  for (auto v : x) {
    if (!(v > 0.0) || !std::isfinite(v)) return nan;
    const double l = std::log(v);
    logs.push_back(l);
    sum += l;
    squares += l * l;
    scale = std::max(scale, std::abs(l));
  }
  const double n = static_cast<double>(x.size());
  const double mu = sum / n;
  double variance = squares / n - mu * mu;
  if (inverse_variance) {
    squares = 0.0;
    for (auto l : logs) squares += (l - mu) * (l - mu);
    variance = squares / n;
  }
  const double moment = burr_lmom_core(mu, variance, true).c;
  if (moment > 0.0 && std::isfinite(moment)) return moment;
  if (!(scale > 0.0)) return nan;

  // Profile out k = n/g(c). This expression avoids subtracting c*sum(l)
  // and g(c), which can both be large on mostly-Pareto samples.
  const auto profile = [&](double logc) {
    const double c = std::exp(logc);
    if (!(c > 0.0) || !std::isfinite(c))
      return -std::numeric_limits<double>::infinity();
    double g = 0.0, remainder = 0.0;
    for (auto l : logs) {
      const double z = c * l;
      if (!std::isfinite(z)) return -std::numeric_limits<double>::infinity();
      g += burr_softplus(z);
      remainder -= burr_softplus(-z);
    }
    if (!(g > 0.0) || !std::isfinite(g))
      return -std::numeric_limits<double>::infinity();
    return n * (std::log(n) - std::log(g) + logc) - n - sum + remainder;
  };
  // Unit log-c grid, expanded in the improving direction until bracketed.
  double center = -std::log(scale);
  const double middle = profile(center);
  const double left = profile(center - 1.0);
  const double right = profile(center + 1.0);
  if (left > middle || right > middle) {
    const double direction = left > right ? -1.0 : 1.0;
    double best = middle;
    bool bracketed = false;
    for (int i = 0; i < 1500; ++i) {
      const double next = center + direction;
      const double value = profile(next);
      if (value <= best) { bracketed = true; break; }
      center = next;
      best = value;
    }
    if (!bracketed) return nan;
  }
  double a = center - 1.0, b = center + 1.0;
  constexpr double ratio = 0.6180339887498948482;
  double u = b - ratio * (b - a), v = a + ratio * (b - a);
  double fu = profile(u), fv = profile(v);
  for (int i = 0; i < 100 && b - a > 1e-9; ++i) {
    if (fu > fv) {
      b = v; v = u; fv = fu;
      u = b - ratio * (b - a); fu = profile(u);
    } else {
      a = u; u = v; fu = fv;
      v = a + ratio * (b - a); fv = profile(v);
    }
  }
  return std::exp((a + b) / 2.0);
}

inline BurrMLE burr_mle_core(span x, double c_start, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  const double nd = static_cast<double>(n);
  if (n == 0) return {0.0, 0.0, kNaN, 0, BurrStatus::InvalidInit};

  for (auto v : x) {
    if (!(v > 0.0) || !std::isfinite(v))
      return {kNaN, kNaN, kNaN, 0, BurrStatus::InvalidInit};
  }
  bool all_ge_one = true;
  for (auto v : x) {
    if (v < 1.0) { all_ge_one = false; break; }
  }
  if (all_ge_one) return {kNaN, kNaN, kNaN, 0, BurrStatus::AllGeOne};

  std::vector<double> log_x(n);
  double sum_l = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = std::log(x[i]);
    sum_l += log_x[i];
  }

  if (!(c_start > 0.0) || !std::isfinite(c_start))
    return {kNaN, kNaN, kNaN, 0, BurrStatus::InvalidInit};
  double logc = std::log(c_start);
  int iter = 0;
  for (; iter < maxit; ++iter) {
    const double c = std::exp(logc);
    if (!(c > 0.0) || !std::isfinite(c))
      return {kNaN, kNaN, kNaN, iter, BurrStatus::IterationFailure};
    double g = 0.0, dg = 0.0, d2g1 = 0.0, d2g2 = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double lx = log_x[i];
      if (!std::isfinite(c * lx))
        return {kNaN, kNaN, kNaN, iter, BurrStatus::IterationFailure};
      const double z = c * lx;
      if (z > 350.0) {
        const double e = std::exp(-z);
        const double t = 1.0 / (1.0 + e);
        g += burr_softplus(z);
        dg += lx * t;
        d2g1 += lx * lx * e / ((1.0 + e) * (1.0 + e));
        continue;
      }
      const double xc = std::exp(z);
      const double t = xc / (xc + 1.0);
      g += std::log1p(xc);
      dg += lx * t;
      const double lx_t = lx * t;
      d2g1 += lx * lx * t;
      d2g2 += lx_t * lx_t;
    }
    const double d2g = d2g1 - d2g2;
    const double g2 = g * g;
    const double f_c = nd / c - nd * dg / g + sum_l - dg;
    const double df_c = -nd / (c * c) - nd * (g * d2g - dg * dg) / g2 - d2g;
    const double f_p = c * f_c;
    const double d2_p = c * (f_c + c * df_c);
    const double step_p = f_p / d2_p;
    if (!std::isfinite(step_p))
      return {kNaN, kNaN, kNaN, iter, BurrStatus::IterationFailure};
    logc -= step_p;
    if (std::fabs(step_p) < tol) {
      ++iter;
      break;
    }
  }

  if (iter >= maxit)
    return {kNaN, kNaN, kNaN, iter, BurrStatus::IterationFailure};
  const double c = std::exp(logc);
  double sum_log1p = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    sum_log1p += burr_softplus(c * log_x[i]);
  }
  const double k = nd / sum_log1p;
  const double logLik = nd * (std::log(k) + std::log(c)) +
                        (c - 1.0) * sum_l - (k + 1.0) * sum_log1p;
  if (!(k > 0.0 && c > 0.0) || !std::isfinite(k) ||
      !std::isfinite(c) || !std::isfinite(logLik))
    return {kNaN, kNaN, kNaN, iter, BurrStatus::IterationFailure};
  return {k, c, logLik, iter == maxit ? maxit : iter, BurrStatus::Ok};
}

inline BurrMLE burr_mle(span x, double tol, int maxit) {
  return burr_mle_core(x, burr_mle_start(x), tol, maxit);
}

}  // namespace cppunivariate
