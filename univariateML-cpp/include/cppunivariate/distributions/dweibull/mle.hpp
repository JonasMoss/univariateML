// Discrete Weibull (type I, Nakagawa--Osaki) MLE.
//
// P(X >= k) = q^(k^beta) = exp(-exp(alpha + beta*log k)) with
// alpha = log(-log q): X is the integer part of a Weibull variable, so
// every count is an interval-censored observation of the log-Weibull
// location-scale family. Each log p_k is strictly concave in
// (alpha, beta) (README "Details"), hence so is the log-likelihood once
// one count is positive. The fitter profiles beta: for each beta the
// unique alpha root is found by safeguarded Newton, and the profile
// derivative, which decreases strictly in beta, is solved by Brent's
// method in log beta. Counts are tabulated once, so the cost is
// O(n log n) for sorting plus O(K) per evaluation for K distinct counts.
//
// Escapes are exact data conditions (sample range; see README limits).

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class DweibullStatus {
  Ok = 0,
  InvalidData,         // empty, or not finite nonnegative integers
  InvalidControl,      // tol, maxit or a held value outside its domain
  PointMass,           // free: constant sample; shape2 held: all zeros
  BernoulliLimit,      // free: values {0, 1}, both present (binomial size 1)
  TwoPointLimit,       // free: values {k, k+1}, k >= 1, both present
  HeldBernoulliLimit,  // shape1 held: values in {0, 1}, at least one 1
  ShapeUnidentified,   // shape1 held, all zeros: flat in shape2; shape2 = 1
  Unrepresentable,     // fitted shape1 = exp(-t) rounds to an excluded endpoint
  IterationFailure
};

struct DweibullResult {
  double shape1;  // q
  double shape2;  // beta
  double logLik;
  int iterations;
  DweibullStatus status;
  StopDetail stop_detail{};
};

inline FitOutcome outcome(const DweibullResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case DweibullStatus::Ok: return FitOutcome::estimate(true);
    case DweibullStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case DweibullStatus::InvalidControl: return FitOutcome::failure(OutcomeReason::InvalidArgument);
    case DweibullStatus::PointMass: return FitOutcome::limit(true);
    case DweibullStatus::BernoulliLimit: return FitOutcome::limit(true);
    case DweibullStatus::TwoPointLimit: return FitOutcome::limit(true);
    case DweibullStatus::HeldBernoulliLimit: return FitOutcome::limit(true);
    case DweibullStatus::ShapeUnidentified: return FitOutcome::estimate(true, false);
    case DweibullStatus::Unrepresentable: return FitOutcome::failure(OutcomeReason::Evaluation);
    case DweibullStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}

namespace dweibull_detail {

// Distinct counts with multiplicities; zeros kept apart. For a positive
// count k: l = log(k) - logc (centred log count), d = log1p(1/k).
struct Problem {
  double n = 0, n0 = 0, logc = 0;
  double min = 0, max = 0;
  std::vector<double> value, w, l, d;
  // Cache for the current beta.
  double beta = std::numeric_limits<double>::quiet_NaN();
  double bl0 = 0;  // beta * (log 1 - logc), exponent offset of the zero cell
  std::vector<double> bl, logE, dinv;

  bool tabulate(span x) {
    if (x.size() == 0) return false;
    std::vector<double> v(x.begin(), x.end());
    for (double z : v) {
      if (!std::isfinite(z) || z < 0 || z != std::floor(z)) return false;
    }
    std::sort(v.begin(), v.end());
    n = static_cast<double>(v.size());
    min = v.front();
    max = v.back();
    for (std::size_t i = 0; i < v.size();) {
      std::size_t j = i;
      while (j < v.size() && v[j] == v[i]) ++j;
      const double m = static_cast<double>(j - i);
      if (v[i] == 0) {
        n0 = m;
      } else {
        value.push_back(v[i]);
        w.push_back(m);
      }
      i = j;
    }
    return true;
  }

  // Centre at c: alpha' = alpha + beta*log(c) keeps the inner root near
  // log(log 2) for every beta when c is the median of the latent Weibull.
  void centre(double log_c) {
    logc = log_c;
    l.resize(value.size());
    d.resize(value.size());
    for (std::size_t j = 0; j < value.size(); ++j) {
      l[j] = std::log(value[j]) - logc;
      d[j] = std::log1p(1 / value[j]);
    }
  }

  double median_plus_half() const {
    double cum = n0;
    if (cum >= n / 2) return 0.5;
    for (std::size_t j = 0; j < value.size(); ++j) {
      cum += w[j];
      if (cum >= n / 2) return value[j] + 0.5;
    }
    return max + 0.5;
  }

  void set_beta(double b) {
    beta = b;
    bl0 = -b * logc;
    bl.resize(value.size());
    logE.resize(value.size());
    dinv.resize(value.size());
    for (std::size_t j = 0; j < value.size(); ++j) {
      const double s = b * d[j];
      bl[j] = b * l[j];
      // log(expm1(s)) and d/(1 - exp(-s)) without overflow or underflow.
      logE[j] = s < 1e-10 ? std::log(b) + std::log(d[j]) + s / 2
              : s > 40     ? s + std::log1p(-std::exp(-s))
                           : std::log(std::expm1(s));
      dinv[j] = s < 1e-10 ? (1 + s / 2) / b : d[j] / (-std::expm1(-s));
    }
  }
};

// For D = t*((k+1)^beta - k^beta): r = D/expm1(D) is the derivative of
// log(1 - exp(-D)) in log D, and c = D*r'(D) its derivative.
inline void ratio(double D, double &r, double &c) {
  if (D == 0) {
    r = 1;
    c = 0;
  } else if (D < 1e-5) {
    r = 1 - D / 2 + D * D / 12;
    c = -D / 2 + D * D / 6;
  } else if (D <= 700) {
    r = D / std::expm1(D);
    c = r * (1 - r - D);
  } else if (std::isinf(D)) {
    r = 0;
    c = 0;
  } else {
    r = std::exp(std::log(D) - D);
    c = r * (1 - r - D);
  }
}

// log(1 - exp(-D)) from log D.
inline double log1mexp_log(double logD) {
  if (logD < -30) return logD - std::exp(logD) / 2;
  const double D = std::exp(logD);
  if (D < 0.6931471805599453) return std::log(-std::expm1(-D));
  return std::log1p(-std::exp(-D));
}

// Score and second derivative in a = alpha' at the cached beta. The score
// decreases strictly from n (a -> -inf) to -inf (a -> inf, a positive count).
inline void inner_eval(const Problem &P, double a, double &f, double &df) {
  f = 0;
  df = 0;
  double r, c;
  if (P.n0 > 0) {
    ratio(std::exp(a + P.bl0), r, c);
    f += P.n0 * r;
    df += P.n0 * c;
  }
  for (std::size_t j = 0; j < P.w.size(); ++j) {
    const double A = std::exp(a + P.bl[j]);
    ratio(std::exp(a + P.bl[j] + P.logE[j]), r, c);
    f += P.w[j] * (r - A);
    df += P.w[j] * (c - A);
  }
}

// Derivative in beta at fixed a (the profile derivative at the inner root).
inline double beta_score(const Problem &P, double a) {
  double g = 0, r, c;
  if (P.n0 > 0) {
    ratio(std::exp(a + P.bl0), r, c);
    g -= P.n0 * r * P.logc;
  }
  for (std::size_t j = 0; j < P.w.size(); ++j) {
    const double A = std::exp(a + P.bl[j]);
    ratio(std::exp(a + P.bl[j] + P.logE[j]), r, c);
    g += P.w[j] * (P.l[j] * (r - A) + r * P.dinv[j]);
  }
  return g;
}

inline double loglik(const Problem &P, double a) {
  double ll = 0;
  if (P.n0 > 0) ll += P.n0 * log1mexp_log(a + P.bl0);
  for (std::size_t j = 0; j < P.w.size(); ++j) {
    ll += P.w[j] * (-std::exp(a + P.bl[j]) + log1mexp_log(a + P.bl[j] + P.logE[j]));
  }
  return ll;
}

// Unique root of the inner score at the cached beta, from warm start a.
// Requires a positive count. Bracket by doubling steps, then Newton with
// bisection safeguard.
inline bool inner_root(const Problem &P, double &a, int maxit, int &evals) {
  maxit = static_cast<int>(solver_budget(maxit));
  double f, df;
  inner_eval(P, a, f, df);
  ++evals;
  if (f == 0) return true;
  // Move from a in the direction of the root until the sign changes:
  // [lo, hi] then has f(lo) >= 0 >= f(hi) with one strict inequality.
  const bool up = f > 0;
  double lo = a, hi = a, flo = f, fhi = f, dlo = df, dhi = df, step = 1;
  for (int i = 0;; ++i, step *= 2) {
    if (i >= maxit) return false;
    const double y = up ? hi + step : lo - step;
    double fy, dy;
    inner_eval(P, y, fy, dy);
    ++evals;
    if (up && fy > 0) { lo = hi = y; flo = fhi = fy; dlo = dhi = dy; continue; }
    if (!up && fy < 0) { lo = hi = y; flo = fhi = fy; dlo = dhi = dy; continue; }
    if (up) { hi = y; fhi = fy; dhi = dy; } else { lo = y; flo = fy; dlo = dy; }
    break;
  }
  const bool at_lo = std::abs(flo) < std::abs(fhi);
  double x = at_lo ? lo : hi, fx = at_lo ? flo : fhi, dx = at_lo ? dlo : dhi;
  double step_old = hi - lo, step_now = step_old;
  for (int it = 0; it < maxit; ++it) {
    if (fx == 0) { a = x; return true; }
    // Newton unless it leaves the bracket or fails to halve the step
    // before last (on the exponential side Newton moves by about 1).
    const double newton = x - fx / dx;
    const bool use_newton = newton > lo && newton < hi &&
                            std::abs(2 * fx) <= std::abs(step_old * dx);
    const double nx = use_newton ? newton : lo + (hi - lo) / 2;
    step_old = step_now;
    step_now = nx - x;
    if (std::abs(step_now) <= 1e-13 * (1 + std::abs(x))) { a = nx; return true; }
    x = nx;
    inner_eval(P, x, fx, dx);
    ++evals;
    if (fx > 0) lo = x; else hi = x;
    if (hi - lo <= 4 * std::numeric_limits<double>::epsilon() * (1 + std::abs(x))) {
      a = x;
      return true;
    }
  }
  return false;
}

// Brent's root finder for a strictly decreasing g on [a, b] with
// g(a) > 0 >= g(b); bisection whenever a value is not finite.
template <typename G>
inline bool brent_root(G &&g, double a, double b, double fa, double fb,
                       double tol, int maxit, double &root, int &iter) {
  maxit = static_cast<int>(solver_budget(maxit));
  double c = a, fc = fa, d = b - a, e = d;
  constexpr double eps = std::numeric_limits<double>::epsilon();
  for (iter = 0; iter < maxit; ++iter) {
    if ((fb > 0) == (fc > 0) && fb != 0) { c = a; fc = fa; d = e = b - a; }
    if (std::abs(fc) < std::abs(fb)) {
      a = b; b = c; c = a; fa = fb; fb = fc; fc = fa;
    }
    const double tol1 = 2 * eps * std::abs(b) + tol / 2, xm = (c - b) / 2;
    if (std::abs(xm) <= tol1 || fb == 0) { root = b; return true; }
    const bool finite = std::isfinite(fa) && std::isfinite(fb) && std::isfinite(fc);
    if (finite && std::abs(e) >= tol1 && std::abs(fa) > std::abs(fb)) {
      double p, q, r, s = fb / fa;
      if (a == c) { p = 2 * xm * s; q = 1 - s; }
      else {
        q = fa / fc; r = fb / fc;
        p = s * (2 * xm * q * (q - r) - (b - a) * (r - 1));
        q = (q - 1) * (r - 1) * (s - 1);
      }
      if (p > 0) q = -q;
      p = std::abs(p);
      if (2 * p < std::min(3 * xm * q - std::abs(tol1 * q), std::abs(e * q))) {
        e = d; d = p / q;
      } else { d = xm; e = d; }
    } else { d = xm; e = d; }
    a = b; fa = fb;
    b += std::abs(d) > tol1 ? d : (xm > 0 ? tol1 : -tol1);
    fb = g(b);
    if (std::isnan(fb)) return false;
  }
  return false;
}

// Bracket the sign change of a strictly decreasing g in u from u = 0
// outwards (steps 1, 2, 4, ...), then solve it by Brent's method.
template <typename G>
inline bool decreasing_root(G &&g, double tol, int maxit, double &root,
                            int &iter) {
  maxit = static_cast<int>(solver_budget(maxit));
  double u = 0, gu = g(u);
  if (std::isnan(gu)) return false;
  if (gu == 0) { root = 0; iter = 0; return true; }
  const double sign = gu > 0 ? 1 : -1;
  double prev = u, gprev = gu, step = 1;
  for (;;) {
    u = prev + sign * step;
    if (std::abs(u) > 64) return false;
    gu = g(u);
    if (std::isnan(gu)) return false;
    if ((gu > 0) != (gprev > 0) || gu == 0) break;
    prev = u; gprev = gu; step *= 2;
  }
  if (gu == 0) { root = u; iter = 0; return true; }
  if (sign > 0) return brent_root(g, prev, u, gprev, gu, tol, maxit, root, iter);
  return brent_root(g, u, prev, gu, gprev, tol, maxit, root, iter);
}

// Fit beta with alpha = log(-log q) held: strictly concave in beta, and
// the score changes sign when max(x) >= 2. Uncentred (logc = 0).
inline DweibullResult held_q_fit(Problem &P, double q, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double alpha = std::log(-std::log(q));
  P.centre(0);
  int evals = 0;
  auto g = [&](double u) {
    P.set_beta(std::exp(u));
    ++evals;
    return beta_score(P, alpha);
  };
  double u = 0;
  int iter = 0;
  if (!decreasing_root(g, tol, maxit, u, iter)) {
    return {q, nan, nan, evals, DweibullStatus::IterationFailure};
  }
  const double beta = std::exp(u);
  P.set_beta(beta);
  return {q, beta, loglik(P, alpha), evals, DweibullStatus::Ok};
}

// shape1 = exp(-t) from (a, beta) at the current centring. The log-likelihood
// is recomputed at the returned (rounded) shape1. Doubles near 1 resolve t
// only to about 1.1e-16/t relative; below t = 1e-6 beta is refitted with
// that representable shape1 held, the best fit the parametrization can
// return. shape1 == 1 cannot be returned at all.
inline DweibullResult finish(Problem &P, double a, double beta, int iterations,
                             bool refit, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double t = std::exp(a - beta * P.logc);
  const double q = std::exp(-t);
  if (!(q > 0 && q < 1)) return {q, beta, nan, iterations, DweibullStatus::Unrepresentable};
  if (refit && t < 1e-6) {
    DweibullResult out = held_q_fit(P, q, tol, maxit);
    out.iterations += iterations;
    return out;
  }
  const double alpha = std::log(-std::log(q));
  return {q, beta, loglik(P, alpha + beta * P.logc), iterations, DweibullStatus::Ok};
}

}  // namespace dweibull_detail

// shape1 / shape2 NaN: estimate; finite: hold fixed at that value.
// tol: absolute tolerance on log(shape2) in the shape2 root search.
inline DweibullResult dweibull_mle(
    span x, double tol = 1e-12, int maxit = 200,
    double shape1 = std::numeric_limits<double>::quiet_NaN(),
    double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> DweibullResult {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  DweibullResult bad{nan, nan, nan, 0, DweibullStatus::InvalidData};
  const bool hold_q = !std::isnan(shape1), hold_b = !std::isnan(shape2);
  if (!(tol > 0 && std::isfinite(tol)) || maxit < 1 || (hold_q && !(shape1 > 0 && shape1 < 1)) ||
      (hold_b && !(shape2 > 0 && std::isfinite(shape2)))) {
    bad.status = DweibullStatus::InvalidControl;
    return bad;
  }
  dweibull_detail::Problem P;
  if (!P.tabulate(x)) return bad;

  if (hold_q) {
    if (hold_b) {
      P.centre(0);
      P.set_beta(shape2);
      return {shape1, shape2, dweibull_detail::loglik(P, std::log(-std::log(shape1))),
              0, DweibullStatus::Ok};
    }
    if (P.max == 0) {
      // p_0 = 1 - shape1 for every shape2: shape2 is not identified.
      P.centre(0);
      P.set_beta(1);
      return {shape1, 1, dweibull_detail::loglik(P, std::log(-std::log(shape1))), 0,
              DweibullStatus::ShapeUnidentified};
    }
    if (P.max == 1) return {shape1, nan, nan, 0, DweibullStatus::HeldBernoulliLimit};
    return dweibull_detail::held_q_fit(P, shape1, tol, maxit);
  }

  if (P.max == 0) return {nan, hold_b ? shape2 : nan, nan, 0, DweibullStatus::PointMass};
  // Centre at the latent median: the inner root is then near log(log 2).
  P.centre(std::log(P.median_plus_half()));
  double a = std::log(std::log(2.0));
  int evals = 0;

  if (hold_b) {
    P.set_beta(shape2);
    if (!dweibull_detail::inner_root(P, a, maxit, evals)) {
      return {nan, shape2, nan, evals, DweibullStatus::IterationFailure};
    }
    return dweibull_detail::finish(P, a, shape2, evals, false, tol, maxit);
  }

  const double range = P.max - P.min;
  if (range == 0) return {nan, nan, nan, 0, DweibullStatus::PointMass};
  if (range == 1) {
    return {nan, nan, nan, 0, P.min == 0 ? DweibullStatus::BernoulliLimit
                                         : DweibullStatus::TwoPointLimit};
  }
  // Range >= 2: a unique finite MLE exists. The profile derivative in
  // beta decreases strictly; solve its sign change in u = log(beta),
  // warm-starting each inner root from the nearest u evaluated so far.
  std::vector<double> seen_u, seen_a;
  bool inner_ok = true;
  auto g = [&](double u) {
    if (!seen_u.empty()) {
      std::size_t best = 0;
      for (std::size_t i = 1; i < seen_u.size(); ++i) {
        if (std::abs(seen_u[i] - u) < std::abs(seen_u[best] - u)) best = i;
      }
      a = seen_a[best];
    }
    P.set_beta(std::exp(u));
    if (!dweibull_detail::inner_root(P, a, maxit, evals)) {
      inner_ok = false;
      return nan;
    }
    seen_u.push_back(u);
    seen_a.push_back(a);
    return dweibull_detail::beta_score(P, a);
  };
  double u = 0;
  int iter = 0;
  if (!dweibull_detail::decreasing_root(g, tol, maxit, u, iter) || !inner_ok) {
    return {nan, nan, nan, static_cast<int>(seen_u.size()),
            DweibullStatus::IterationFailure};
  }
  g(u);  // inner root at the returned beta
  if (!inner_ok) return {nan, nan, nan, 0, DweibullStatus::IterationFailure};
  return dweibull_detail::finish(P, a, std::exp(u), static_cast<int>(seen_u.size()),
                                 true, tol, maxit);

  }();
  if(stopped.status==DweibullStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  if(stopped.status==DweibullStatus::TwoPointLimit) {
    const double lo=*std::min_element(x.begin(),x.end());
    stopped.stop_detail=discrete_two_point(x,lo,lo+1);
  }
  if(stopped.status==DweibullStatus::BernoulliLimit || stopped.status==DweibullStatus::HeldBernoulliLimit)
    stopped.stop_detail=discrete_two_point(x,0,1,stopped.status==DweibullStatus::HeldBernoulliLimit?shape1:NAN);
  return stopped;
}

}  // namespace cppunivariate
